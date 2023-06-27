
# control parameters ------------------------------------------------------

# once service account is set up we will load all data from gdrive using the `{googledrive}` package
# so that files will be acessible from runner
service_account_set <- c(T,F)[2]

# not sure which basin level we are using yet so you can toggle between level 4 & 5
basin_level <-  c(4,5)[2]

# I wanted to have a version of the email that is different if there is a trigger - 
# therefore made fake data if below is set to TRUE
insert_fake_breach_data <- c(T,F)[2]

# two types of graphs for discharge time series - currently opting for % change
line_plot_type <- c("pct_change","discharge")[1]

# the flood extent kml will presumably be updated soon, but now we just have place holder
kml_place_holder <- c(T,F)[1]


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

# load data ---------------------------------------------------------------

# for quick access we are loading target objects.
# run `targets::tar_make()` to get them all.
# eventually we will move them to gdrive and access w/ `{googledrive}` package from GH action runner.

tar_source()
tar_load(basins_clipped)
tar_load(nga_adm)
tar_load(nga_riv)

# need access to googlesheet w/ gauge data - add link to your environment vars
gauge_df_list <- read_gauge_googlesheets(url = Sys.getenv("GFH_GAUGE_URL"))


if(kml_place_holder){
  tar_load(gfh_now)
  poly_fextent<- gfh_now
}
#^ need to write code when we are using realtime kml extent data

if(basin_level==4){
  basin_sp <-  basins_clipped$hybas_af_lev04_v1c %>% 
    clean_names()
}
if(basin_level ==5){
  basin_sp <-  basins_clipped$hybas_af_lev05_v1c %>% 
    clean_names()
}


# make gauge locations spatial and join basins
gauge_locations_sp <- gauge_df_list$metadata %>% 
  st_as_sf(coords = c("longitude","latitude"), crs= 4326) %>% 
  st_join(basin_sp %>% 
            select(hybas_id))

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
  select(date,hybas_id, gauge_id, contains("discharge"),contains("return")) %>% 
  ungroup()


# if we want to see what email looks like when threshold is breached we need to fake some data
if(insert_fake_breach_data){
  basin_fake_data <- gauge_nrt %>% 
    count(hybas_id) %>% 
    filter(n==max(n)) %>% 
    pull(hybas_id)
  gauge_nrt <-   gauge_nrt %>% 
    rowwise() %>% 
    mutate(
      fake_above_2 = sample(x2_years_return_period:x5_years_return_period,1),
      fake_above_5 = sample(x5_years_return_period:x20_years_return_period,1)
    ) %>% 
    ungroup() %>% 
    mutate(
      forecast_discharge_5day = ifelse(hybas_id==basin_fake_data,fake_above_2,forecast_discharge_5day)
    ) %>% 
    select(-starts_with("fake_")) 
}

# used for subject in email
date_prediction_made <- gauge_nrt$date %>% unique()
dt_made_chr <- gsub(" 0", " ", format(as_date(date_prediction_made), " - %d %B %Y"))


# predicted discharge by date
Q_pred <- gauge_nrt %>% 
  pivot_longer(
    cols = matches("discharge")
  ) %>% 
  mutate(
    leadtime = replace_na( parse_number(name),0),
    date_predict = date + leadtime,
    gte_2_rp = value >= x2_years_return_period,
    gte_5_rp = value >= x5_years_return_period,
    gte_20_rp = value >= x20_years_return_period
  ) 

# flags - extract the trigger flag if any exists
gauge_flagged <- Q_pred %>% 
  group_by(hybas_id,gauge_id) %>% 
  summarise(
    gte_2_rp= any(gte_2_rp),
    gte_5_rp= any(gte_5_rp),
    gte_20_rp= any(gte_20_rp),
    rp_class= case_when(
      gte_20_rp ~ ">= 20 year RP",
      gte_5_rp ~ ">= 5 year RP",
      gte_2_rp ~ ">= 2 year RP",
      .default = "No threshold exceeded"
    ),.groups="drop"
  ) %>% 
  mutate(rp_class= fct_expand(rp_class,c("No threshold exceeded",
                                         ">= 2 year RP",
                                         ">= 5 year RP",
                                         ">= 20 year RP"))
  )
  

# Line Plot Forecast Discharge ------------------------------------------

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
  facet_wrap(~hybas_id,ncol=1,scales = "free_y")+
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
  

# Maps --------------------------------------------------------------------

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
          pct_gte_20_rp>=0.5~ ">= 50 % gauges exceeding 20 year RP",
          pct_gte_5_rp>=0.5~ ">= 50 % gauges exceeding 5 year RP",
          pct_gte_2_rp>=0.5~ ">= 50 % gauges exceeding 2 year RP",
          pct_gte_2_rp<0.5~ "< 50 % gauges exceeding 2 year RP"
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
        )
      )

# make flags long
flags_long <- basin_pct_flagged %>% 
  select(-pct_class) %>% 
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

# create choropleth map showing trigger status
m_basin_alerts <-
  tm_shape(nga_adm$adm0)+
  tm_polygons(col = "white",border.col = "black")+
  tm_shape(basin_sp_w_stats)+
  tm_polygons(col= "pct_class",
              palette=ipc_pal ,
              title="classification",
              border.col = "darkblue" # should play w/ color
              )+
  tm_shape(nga_adm$adm1)+
  tm_borders(col="darkgrey")+
  tm_shape(nga_adm$adm0)+
  tm_borders(col="black")+
  tm_shape(nga_riv)+
  tm_lines(col = "lightblue",lwd=3)+
  tm_shape(gauge_locations_sp)+
  tm_dots(size = 0.1)+
  tm_layout(main.title.size = 0.5,
            title.size = 0.5)


# if trigger occurs
if(nrow(flags_long)>0){
  
  # extract just basin(s) where alert is
  basin_w_alert <- basin_sp_w_stats %>% 
    filter(pct_class!="< 50 % gauges exceeding 2 year RP")

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
  
  # using kml place holder right now to map flood extent
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
    "email_recepients.csv"
  ) 
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
