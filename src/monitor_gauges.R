
# control parameters ------------------------------------------------------

# once service account is set up we will load all data from gdrive using the `{googledrive}` package
# so that files will be acessible from runner
service_account_set <- c(T,F)[1]

# not sure which basin level we are using yet so you can toggle between level 4 & 5
basin_level <-  c(4,5)[1]

# I wanted to have a version of the email that is different if there is a trigger - 
# therefore made fake data if below is set to TRUE
insert_fake_breach_data <- c(T,F)[1]

# two types of graphs for discharge time series - currently opting for % change
line_plot_type <- c("pct_change","discharge","discharge_norm")[3]

# the flood extent kml will presumably be updated soon, but now we just have place holder
kml_place_holder <- c("fake","example","realtime")[2]

map_coverage <-  c("local","widespread")[2]

# scenario 1 - no alert, no threshold breach
# scenario 2 - no alert some gauge breach (1 basin)
# scenario 3 - no alert some gauge breach (>1 basin)
# scenario 4 - alert (1 basin)
# scenario 5 - alert (>1 basin)

scenario <- 5


# libs --------------------------------------------------------------------
library(tidyverse)
library(sf)
library(googlesheets4)
library(targets)
library(janitor)
library(tmap)
library(here)
library(blastula)
library(gt)
library(googledrive)
library(terra)
library(grid) #viewport
library(tmap)
library(ggtext) # colored title
library(glue)
library(gghdx)
# load data ---------------------------------------------------------------

# for quick access we are loading target objects.
# run `targets::tar_make()` to get them all.
# eventually we will move them to gdrive and access w/ `{googledrive}` package from GH action runner.

tar_source()
tar_load(basins_clipped)
tar_load(nga_adm)
tar_load(nga_riv)

# authenticate `{googlesheets4}` and `{googledrive}`
walk(list(drive_auth, gs4_auth),\(f){
  f(
    path = Sys.getenv("GFF_JSON")
  )
})
drive_dribble <- drive_ls(
  corpus="user"
)
# need access to googlesheet w/ gauge data - add link to your environment vars
gauge_df_list <- read_gauge_googlesheets(url = Sys.getenv("GFH_GAUGE_URL"))


if(kml_place_holder=="fake"){
  tar_load(gfh_now)
  poly_fextent<- gfh_now
}
if(kml_place_holder=="example"){
  # these are just examples
  fext_gdrive_id <- list(
    `f_20230510` = "1jcnbkPK34b4eTRGA9cyiD-MKcQqAmTQ7",
    `f_20230509` = "1vN6HumxVGez_nwXUdylaWNc2XnntBCsL"
  )
  fext <- fext_gdrive_id %>% 
    map(
      \(.x){
        drive_download_kml(.x,
                           path = fp <- tempfile(fileext = ".kml")
        )
        st_read(fp,layer = "Layer #0" ) %>% 
          # fix invalid geoms
          # st_make_valid doesnt work here.
          st_buffer(dist = 0)
      }
    )
}

#^ need to write code when we are using realtime kml extent data

if(basin_level==4){
  basin_sp <-  basins_clipped$hybas_af_lev04_v1c %>% 
    clean_names()

  basin_sp <- basin_sp %>% 
    mutate(
      basin_name = case_when(
        hybas_id == 1040909900 ~ "Benue",
        hybas_id == 1040909890 ~ "Lower Niger",
        hybas_id == 1040022420 ~ "Niger Delta",
        hybas_id == 1040760290 ~ "Upper Niger",
        .default= NA
      )
    ) %>% 
    filter(!is.na(basin_name))
}
if(basin_level ==5){
  basin_sp <-  basins_clipped$hybas_af_lev05_v1c %>% 
    clean_names()
}


# make gauge locations spatial and join basins
gauge_locations_sp <- gauge_df_list$metadata %>% 
  st_as_sf(coords = c("longitude","latitude"), crs= 4326) %>% 
  st_join(basin_sp %>% 
            select(hybas_id,basin_name)) %>% 
  filter(!is.na(hybas_id))

# extract gauged basins
basins_gauged <- basin_sp %>% 
  filter(hybas_id %in% gauge_locations_sp$hybas_id) 


# compile gauge forecast data - add basins
gauge_forecast <- gauge_df_list %>% 
  keep_at(at = ~str_detect(.x,"hybas_")) %>% 
  bind_rows() %>% 
  mutate(date= dmy(date)) %>% 
  left_join(
    gauge_locations_sp %>% 
      st_drop_geometry()
  )


# near realtime (latest available) forecast data
gauge_nrt <- gauge_forecast %>% 
  group_by(gauge_id) %>% 
  filter(
    date==max(date)
  ) %>% 
  select(date, gauge_id,hybas_id, basin_name, contains("discharge"),contains("return")) %>% 
  ungroup() %>% 
  filter(gauge_id %in% gauge_locations_sp$gauge_id) 


# if we want to see what email looks like when threshold is breached we need to fake some data
# if(insert_fake_breach_data){
#   basin_fake_data <- gauge_nrt %>% 
#     count(hybas_id) %>% 
#     filter(n==max(n)) %>% 
#     pull(hybas_id)
#   gauge_nrt <-   gauge_nrt %>% 
#     rowwise() %>% 
#     mutate(
#       fake_above_2 = sample(x2_years_return_period:x5_years_return_period,1),
#       fake_above_5 = sample(x5_years_return_period:x20_years_return_period,1)
#     ) %>% 
#     ungroup() %>% 
#     mutate(
#       forecast_discharge_5day = ifelse(hybas_id==basin_fake_data,fake_above_2,forecast_discharge_5day)
#     ) %>% 
#     select(-starts_with("fake_")) 
# }

# used for subject in email
date_prediction_made <- gauge_nrt$date %>% unique()
dt_made_chr <- gsub(" 0", " ", format(as_date(date_prediction_made), " - %d %B %Y"))


# predicted discharge by date
gauge_long <- gauge_nrt %>% 
  pivot_longer(
    cols = matches("discharge")
  ) %>% 
  mutate(
    leadtime = replace_na( parse_number(name),0),
    date_predict = date + leadtime,
  )


#######################
sample_gauges <- function(df,samp_pct){
  df_distinct <- df %>% 
    distinct(hybas_id, gauge_id)  
  df_l <-  split(df_distinct,df_distinct$hybas_id)
  df_l %>% 
    map(
      \(dft){
        num_gauge <- nrow(dft)
        samp_n = floor(num_gauge * samp_pct)
        
        sample(dft$gauge_id, samp_n)
      }
    )
}
###############

# scenario 1 - no alert, no threshold breach
# scenario 2 - no alert some gauge breach (1 basin)
# scenario 3 - no alert- some gauge breach (>1 basin)
# scenario 4 - alert (1 basin)
# scenario 5 - alert (>1 basin)

samp_25_percent_cross <- sample_gauges(df = gauge_long,samp_pct = 0.25)
samp_65_percent_cross <- sample_gauges(df = gauge_long,samp_pct = 0.65)
samp_40_percent_cross <- sample_gauges(df = gauge_long,samp_pct = 0.65)
if(scenario==1){
  Q_df <-  gauge_long
}
if(scenario==2){
  Q_df <-  gauge_long %>% 
    rowwise() %>% 
    mutate(
      value = case_when(
        gauge_id %in% samp_25_percent_cross$`1040909900`  & leadtime==5~ sample(x2_years_return_period:x5_years_return_period,1),
        .default= value
    )
    ) %>% ungroup()
}
if(scenario==3){
  Q_df <-  gauge_long %>% 
    rowwise() %>% 
    mutate(
      value = case_when(
        gauge_id %in% samp_25_percent_cross$`1040909900`  & leadtime==5~ sample(x2_years_return_period:x5_years_return_period,1),
        gauge_id %in% samp_25_percent_cross$`1040760290`  & leadtime==5~ sample(x2_years_return_period:x5_years_return_period,1),
        .default= value
      )
    ) %>% ungroup()
}
if(scenario==4){
  Q_df <-  gauge_long %>% 
    rowwise() %>% 
    mutate(
      value = case_when(
        # gauge_id %in% samp_65_percent_cross$`1040909900`[1:(length(samp_65_percent_cross$`1040909900`)-4)] & leadtime==4~ sample(x2_years_return_period:x5_years_return_period,1),
        gauge_id %in% samp_65_percent_cross$`1040909900`  & leadtime==5~ sample(x2_years_return_period:x5_years_return_period,1),
        
        .default= value
      )
    ) %>% ungroup()
}
if(scenario==5){
  Q_df <-  gauge_long %>% 
    rowwise() %>% 
    mutate(
      value = case_when(
        gauge_id %in% samp_65_percent_cross$`1040909900`  & leadtime==5~ sample(x2_years_return_period:x5_years_return_period,1),
        gauge_id %in% samp_65_percent_cross$`1040760290`  & leadtime==5~ sample(x2_years_return_period:x5_years_return_period,1),
        .default= value
      )
    ) %>% ungroup()
}


Q_pred <- Q_df %>% 
  mutate(
    Q_norm_rp2 = value/x2_years_return_period,
    gte_2_rp = value >= x2_years_return_period,
    gte_5_rp = value >= x5_years_return_period,
    gte_20_rp = value >= x20_years_return_period
  ) 


# flags - extract the trigger flag if any exists
gauge_flagged <- Q_pred %>% 
  group_by(hybas_id,gauge_id) %>% 
  summarise(
    gte_2_rp_any= any(gte_2_rp),
    gte_5_rp_any= any(gte_5_rp),
    gte_20_rp_any= any(gte_20_rp),
    rp_class= case_when(
      gte_20_rp_any ~ ">= 20 year RP",
      gte_5_rp_any ~ ">= 5 year RP",
      gte_2_rp_any ~ ">= 2 year RP",
      .default = "No threshold exceeded"
    ),.groups="drop"
  ) %>% 
  mutate(rp_class= fct_expand(rp_class,c("No threshold exceeded",
                                         ">= 2 year RP",
                                         ">= 5 year RP",
                                         ">= 20 year RP"))
  )
  

Q_class <- Q_pred %>% 
  left_join(gauge_flagged, by=c("hybas_id","gauge_id")) 

# Line Plot Forecast Discharge ------------------------------------------

p_discharge_norm <- plot_gauge_discharge(df = Q_class,
                     date = "date_predict",
                     facet_var = NULL)

p_pct_gauges <- plot_gauge_cum_pct(df = Q_class,
                                   lgl_var = "gte_2_rp",
                                   threshold = 0.5,
                                   basin_palette = basin_pal()
)



p_gauge_all_norm <- plot_gauge_discharge_normalized(
  df = Q_class,
  date = "date_predict",
  threshold = 0.5,
  basin_palette = basin_pal(),
  annotation = F
)
library(patchwork)
p_1x2 <- p_pct_gauges+
  labs(
    title = gen_titles(df = Q_class,date = "date_predict",threshold = 0.5)$title,
    # subtitle  = gen_titles(df = Q_class,date = "date_predict",threshold = 0.5)$subtitle,
    subtitle= "Status July 31 2023: Flooding Predicted by 05 August",
       y= "Basin Status: % Gauges Breaching 2 Year RP")+

  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 15),
    plot.title =  element_markdown()
  )+
  p_gauge_all_norm+
  # labs(y= "% 2 year RP")+
  theme(
    plot.title  = element_blank(),
    plot.subtitle = element_blank(),
    axis.title.y = element_text(size = 15)
    
  )+
  plot_layout(ncol = 1,
              nrow = 2)




# plot discharge by basin
p_discharge_lines <- Q_pred %>% 
  left_join(gauge_flagged, by=c("hybas_id","gauge_id")) %>%
  ggplot(aes(x=date_predict,y=value,
             group=gauge_id,
             color=rp_class))+
  geom_line(alpha=0.6)+
  scale_x_date(breaks = "1 day",date_labels = "%m-%d")+
  scale_color_manual(drop=FALSE,
                     values=c(`No threshold exceeded`="#43a2ca",
                              `>= 2 year RP` = "#f03b20",
                              `>= 5 year RP` = "#bd0026",
                              `>= 20 year RP` ="#7a0177"
                     )
  )+
  facet_wrap(~basin_name,ncol=1,scales = "free_y")+
  labs(y= "Discharge (m3/s)",
       title ="Predicted discharge at gauge"
  )+
  theme(
    axis.text.x = element_text(angle=90),
    axis.title.x = element_blank(),
    legend.title = element_blank()
  )

# plot discharge % change by basin
p_discharge_pct_change <- Q_pred %>% 
  left_join(gauge_flagged, by=c("hybas_id","gauge_id")) %>% 
  group_by(gauge_id) %>% 
  mutate(
    init_value = value[date_predict==min(date_predict)],
    pct_chg = value/init_value
    ) %>% ggplot(aes(x=date_predict,y=pct_chg,
             group=gauge_id,
             color=rp_class))+
  geom_line(alpha=0.6)+
  scale_x_date(breaks = "1 day",date_labels = "%m-%d")+
  scale_y_continuous(labels = scales::percent)+
  scale_color_manual(drop=FALSE,
                     values=c(`No threshold exceeded`="#43a2ca",
                              `>= 2 year RP` = "#f03b20",
                              `>= 5 year RP` = "#bd0026",
                              `>= 20 year RP` ="#7a0177"
                     )
  )+
  facet_wrap(~hybas_id,ncol=1,scales = "free_y")+
  labs(y= "% Change",
       title ="Predicted increase/decrease in discharge",
       subtitle="Forecast range: 7 days"
  )+
  theme(
    axis.text.x = element_text(angle=90),
    axis.title.x = element_blank(),
    legend.title = element_blank()
  )

if(line_plot_type=="pct_change"){
  p_lineplot <- p_discharge_pct_change
}

if(line_plot_type=="discharge"){
  p_lineplot <- p_discharge_lines
}
if(line_plot_type=="discharge_norm"){
  p_lineplot <- p_1x2
}
  

# Maps --------------------------------------------------------------------

## Alert Map Wrangle ####
# evaluate if >= 50 % of gauges have passed 2,5, 20 year RP values
basin_pct_flagged <- gauge_flagged %>%
      group_by(hybas_id) %>% 
      summarise(
        num_gauges=n(),
        across(
          matches("^gte_"), ~sum(.x)/num_gauges,.names = "pct_{.col}"
        ),.groups="drop"
      ) %>% 
      mutate(
        pct_class = case_when(
          pct_gte_20_rp_any>=0.5~ ">= 50 % gauges exceeding 20 year RP",
          pct_gte_5_rp_any>=0.5~ ">= 50 % gauges exceeding 5 year RP",
          pct_gte_2_rp_any>=0.5~ ">= 50 % gauges exceeding 2 year RP",
          pct_gte_2_rp_any<0.5~ "< 50 % gauges exceeding 2 year RP"
        ),
        # for some reason I need to go overkill w/ `{forcats}` here... 
        # the levels were getting dropped and out of order w/ out both steps in `{tmap}`
        pct_class = fct_expand(pct_class,c("< 50 % gauges exceeding 2 year RP",
                                           ">= 50 % gauges exceeding 2 year RP",
                                           ">= 50 % gauges exceeding 5 year RP",
                                           ">= 50 % gauges exceeding 20 year RP")
                               ) ,
        pct_class = fct_relevel(pct_class,c("< 50 % gauges exceeding 2 year RP",
                                             ">= 50 % gauges exceeding 2 year RP",
                                             ">= 50 % gauges exceeding 5 year RP",
                                            ">= 50 % gauges exceeding 20 year RP")
        ),
        pct_class2 = case_when(
          pct_gte_20_rp_any>=0.5~ "Very High Alert",
          pct_gte_5_rp_any>=0.5~ "High Alert",
          pct_gte_2_rp_any>=0.5~ "Medium Alert",
          pct_gte_2_rp_any<0.5~ "No Warning"
        ) ,
        pct_class2= fct_expand(pct_class2,c("Very High Alert","High Alert","Medium Alert","No Warning")) %>% 
          fct_relevel(c("Very High Alert","High Alert","Medium Alert","No Warning") %>% rev())
        
      )

# make flags long
flags_long <- basin_pct_flagged %>% 
  select(-pct_class,-pct_class2) %>% 
  pivot_longer(cols = matches("^pct")) %>% 
  mutate(
    rp_num = parse_number(name),
    flag_label = case_when(
      name =="pct_gte_2_rp"~">= 50 % gauges exceeded 2 year RP",
      name== "pct_gte_5_rp"~">= 50 % gauges exceeded 5 year RP",
      name== "pct_gte_20_rp"~">= 50 % gauges exceeded 20 year RP"
    )) %>% 
  filter(value>=0.5) 

ipc_pal <-  c(
  "#f1e118ff",
  "#c86010ff",
  "#a01410ff",
  "#400c0aff"
)

# join basin poly w/ trigger statistics for choropleth map
basin_sp_w_stats <- basin_sp %>%
           left_join(basin_pct_flagged, by ="hybas_id") %>% 
           filter(!is.na(pct_class))

## Alert Map -Viz ####
# create choropleth map showing trigger status

gauge_locations_sp %>% 
  filter(hybas_id %in% gauge_flagged$hybas_id) %>% 
  left_join(gauge_flagged)
tar_load(wca_adm0)
nga_adm0 <- wca_adm0 %>% 
  filter(admin0Pcod=="NG")
adm0_excl <- wca_adm0 %>% 
  filter(admin0Pcod!="NG")
basin_sp_w_stats_cl <- st_intersection(basin_sp_w_stats,nga_adm0)


basin_map_poly <- basin_sp_w_stats_cl %>% 
  mutate(
    alert_status = ifelse(pct_class2!="No Warning","Warning","No Warning")
  )
basin_map_lines <- st_cast(basin_map_poly,"MULTILINESTRING")
gauge_data_map %>% count(dot_alpha) 
gauge_data_map <- gauge_locations_sp %>%
  filter(hybas_id %in% gauge_flagged$hybas_id) %>%
  left_join(gauge_flagged %>% 
              mutate(
                `Gauge Status` =ifelse(gte_2_rp_any,"Threshold Exceeded","Below Threshold"),
                dot_size =ifelse(gte_2_rp_any,
                                 0.01,#.00002,# 0.005,
                                 .005),#0.000015),
                dot_alpha =ifelse(gte_2_rp_any,
                                 1,
                                 0.5),
                dot_halo_size =dot_size+0.005,
                
              )
            
  )

m_basin_alerts <- nga_base_map(surrounding_fill = hdx_colors()["gray-dark"])+
  tm_shape(
    basin_sp_w_stats_cl %>% 
      mutate(
        alert_status = ifelse(pct_class2!="No Warning","Warning","No Warning")
      ),
    
  )+
  tm_polygons(col= "alert_status",
              palette=c(hdx_colors()["mint-ultra-light"], 
                        hdx_colors()["tomato-hdx"]) ,
              alpha = 0.5,
              title="Basin Alert Status",
              border.col = NULL # should play w/ color
  )+
  tm_shape(
    basin_map_lines,
    legend.show=F
  )+
  tm_lines(col = "alert_status",legend.col.show = F,
             palette=c(hdx_colors()["mint-hdx"],
                       hdx_colors()["tomato-dark"]),
           lwd=4,alpha = 0.3
           )+
  tm_shape(nga_adm$adm1)+
  tm_borders(
    col=hdx_colors()["gray-dark"],
    alpha = 0.2
  )+
  # tm_shape(nga_adm$adm0)+
  # tm_borders(col="black")+
  tm_shape(nga_riv)+
  tm_lines(
    # col = "lightblue",
    col=hdx_colors()["sapphire-hdx" ],
    lwd=3)+
  # gauge locations
  tm_shape(gauge_data_map,
           legend.show=F
  )+
  tm_dots(
             col="Gauge Status",
             size= 0.2,
             # contrast = c(0.1,1),
             # size=0.2,
             # alpha = c(0.5),
             # alpha = "dot_alpha",
             # size = "dot_size",
             # border.col = "white",
             # border.col=NULL,
             # border.lwd = 0.5,
             palette=c("#25252533",hdx_colors()["tomato-dark"]),
             # palette=c(hdx_colors()["gray-dark"],"tomato"),
             # palette=c("blue","red"),
             # legend.show=F,
             legend.size.show=F
             )+
  tm_shape(basin_sp_w_stats_cl)+
  tm_text(text = "basin_name",shadow = T)+
  tm_shape(adm0_excl)+
  tm_text(text= "admin0Name",col = "white")+
  tm_shape(nga_adm0)+
  tm_borders(col = "#414141",lwd = 5,alpha=1)+
  tm_shape(nga_adm0,legend.show=F)+
  tm_borders(
    col="white",
    # lty = 3, # linetype long dash
    lwd=1,
    alpha=0.7)+

  tm_layout(main.title.size = 0.5,
            title.size = 0.5,
            legend.text.size = 0.5,
            legend.title.size = 0.7,
            bg.color = "lightblue")
  
# if trigger occurs
if(nrow(flags_long)>0){
  
  # extract just basin(s) where alert is
  basin_w_alert <- basin_sp_w_stats %>% 
    filter(pct_class!="< 50 % gauges exceeding 2 year RP")


  
  if(kml_place_holder=="fake"){
    # generate FAKE pop data -- need to adjust w/ real kml extent over raster
    pop_affected_df <- basin_w_alert %>% 
      st_join(poly_fextent) %>% 
      st_join(nga_adm$adm2) %>% 
      st_drop_geometry() %>% 
      group_by(ADM1_EN) %>% 
      summarise(
        `Affected Pop` = n()*100
      ) %>% 
      rename(State="ADM1_EN")
    
    ## Flood Map Viz ####
    #### Fake Placeholder Data ####
    m_flood_extent <- 
      tm_shape(nga_adm$adm0)+
      tm_polygons(col = "white",border.col = "black")+
      tm_shape(nga_adm$adm1)+
      tm_borders(col="darkgrey",lwd = 1,alpha=0.7)+
      tm_shape(nga_adm$adm2)+
      tm_borders(col="lightgrey",alpha = 0.3)+
      tm_shape(nga_adm$adm0)+
      tm_borders(col="black")+
      tm_shape(nga_riv)+
      tm_lines(col = "lightblue",lwd=3)+
      tm_shape(poly_fextent)+
      tm_polygons(col = "darkblue",
                  border.col = "darkblue",
                  alpha = 0.75)
  }
  if(kml_place_holder=="example"){

    ### Example Placeholder Data ####
    library(rnaturalearth)
    
    if(map_coverage=="widespread"){
      poly_fextent <- fext %>% 
        bind_rows() %>% 
        st_buffer(dist = 0) %>% 
        summarise() %>% 
        st_make_valid()
      
    }
    pop_affected_df <- basin_w_alert %>% 
      st_join(poly_fextent) %>% 
      st_join(nga_adm$adm2) %>% 
      st_drop_geometry() %>% 
      group_by(ADM1_EN) %>% 
      summarise(
        `Affected Pop` = n()*100
      ) %>% 
      rename(State="ADM1_EN")

    # can I split poly by basin
    fext_by_basin <- poly_fextent %>% 
      st_make_valid() %>% 
      st_cast("POLYGON") %>% 
      st_join(
        basin_sp
      ) %>% 
      group_by(basin_name) %>% 
      summarise()
    
    fext_centroid_basin <- fext_by_basin %>% 
      mutate(
        `Area flooded` = as.numeric(units::set_units(st_area(.),km^2))
      ) %>% 
      st_centroid() 
    
    m_flood_extent <- inset_admin_base()+
      tm_shape(nga_riv)+
      tm_lines(col = "lightblue",lwd=3)+
      tm_shape(fext_centroid_basin)+
      tm_bubbles(
        size = "Area flooded",
        col = rgb(238,88,89,max=255),
        scale = 2,
        title.size= "Area Flooded\n(km2)",
        legend.size.is.portrait=TRUE,
        # legend.max.symbol.size = 4
      )+
      tm_layout(
        
        legend.frame = T,
        legend.bg.color = "white",
        legend.bg.alpha = 0.5,
        legend.width = 1,
        panel.label.size = 1,
        panel.label.height = 1  
      )
  }
  # using kml place holder right now to map flood extent

  
  
  
}



# Email set up ------------------------------------------------------------


# Generate conditional email subject
subj_email <-  paste0("Nigeria Gauge Monitoring: Basin Level ",
                     basin_level,
                     dt_made_chr)
if(insert_fake_breach_data){
  subj_email <- paste0("Nigeria Gauge Monitoring: Basin Level ",
                       basin_level,
                       " (FAKE DATA) ",
                       dt_made_chr)
}

# will need to write code w/ `{googledrive}` package to access from service account/GH Action runner
if(!service_account_set){
  email_receps_fp <- file.path(
    Sys.getenv("GFH_DATA_DIR"), 
    "gha_inputs",
    "email_recepients.csv"
  ) 
}

if(service_account_set){
  drive_download(as_id("1A1WPSWBPJKFDBqZb1OXYHipsYxEErR-7"),email_receps_fp <- tempfile(fileext = ".csv"))
  
  
}
email_receps_df <- read_csv(email_receps_fp)
email_to <- email_receps_df %>% 
  filter(to) %>% 
  pull(email_address)
  

# Load in e-mail credentials
email_creds <- creds_envvar(
  user = Sys.getenv("CHD_DS_EMAIL_USERNAME"),
  pass_envvar = "CHD_DS_EMAIL_PASSWORD",
  host = Sys.getenv("CHD_DS_HOST"),
  port = Sys.getenv("CHD_DS_PORT"),
  use_ssl = TRUE
)

# email_rmd_fp <- file.path(
#   "src",
#   "email",
#   "email_mockup.Rmd"
# )
# email_rmd_fp <- "Users/zackarno/Documents/CHD/repos/pa-flood-pilot-validation/src/email/email_mockup.Rmd"
email_rmd_fp <- "email_mockup.Rmd"
render_email(
  input = email_rmd_fp,
  envir = parent.frame()
) %>%
  smtp_send(
    to = email_to,
    # bcc = filter(df_recipients, !to)$email,
    from = "data.science@humdata.org",
    subject = subj_email,
    credentials = email_creds
  )
  