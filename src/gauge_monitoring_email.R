

# libs --------------------------------------------------------------------
# in dedicated monitoring repo should consider setting up w/ {renv}
library(tidyverse)
library(sf)
library(googlesheets4)
library(janitor)
library(tmap)
library(here)
library(blastula)
library(gt)
library(googledrive)
# library(terra) # maybe remove
# library(grid) #viewport
library(ggtext) # colored title
library(glue)
library(gghdx)
library(rhdx)
gghdx()

source("R/email_funcs.R")
# source("R/utils.R")



# Get Data ----------------------------------------------------------------

## Authenticate google APIs ####
walk(list(drive_auth, gs4_auth),\(f){
  f(
    path = Sys.getenv("GFF_JSON")
  )
})
drive_dribble <- drive_ls(
  corpus="user"
)

## Static Layers ####
# load layers for mapping from hdx
L <- hdx_map_viz_layers()


# Basins
drive_download(
  as_id("1AsB_Cf9QQb3vkp9ebIFvTPZ5gBYqYRJR"),
  path = basin_fp <- tempfile(fileext = ".rds")
  )

gdf_basins_poly <- read_rds(basin_fp) %>% 
  filter_basins() %>% 
  # clip to country
  st_intersection(
    L$west_central_africa %>% 
      filter(admin0Pcod=="NG")
  )


## Live Data ####
### Google forecast Workbook ####
gauge_df_list <- read_gauge_googlesheets(
  url = Sys.getenv("GFF_GAUGE_URL")
  )

### Gauge Locations ####
gdf_gauge <- st_as_sf(gauge_df_list$metadata, 
                      coords = c("longitude", "latitude"),
                      crs = 4326) %>% 
  filter_gauges() %>% 
  st_join(
    gdf_basins_poly %>% 
      select(hybas_id,basin_name)
  )

### Discharge Forecasts ####
df_forecast_long <- gauge_df_list %>% 
  keep_at(at = ~str_detect(.x,"hybas_")) %>% 
  bind_rows() %>% 
  # remove potential duplication issue where two discharge values may get appended to googlesheed w/ same date
  mutate(
    date= dmy(date),
    update_time_utc = ymd_hms(update_time_utc)
    ) %>% 
  filter(date==max(date)) %>% 
  group_by(gauge_id,date) %>%
  filter(
    update_time_utc == max(update_time_utc)
  ) %>%
  ungroup() %>% 
  filter_gauges() %>% 
  # add basin informatino to gauge/forecast data
  left_join(
    gdf_gauge %>% 
      st_drop_geometry(),
    by ="gauge_id"
  ) %>% 
  pivot_longer(
    cols = matches("discharge"),
    names_to = "forecast_cat",
    values_to = "Q"
  ) %>% 
  mutate(
    leadtime = replace_na(parse_number(forecast_cat),0) %>% 
      suppressWarnings(), # don't need the parse_number warning
    date_predict = date + leadtime,
    Q_pct_rp2 = Q/x2_years_return_period,
    gte_2_rp = Q >= x2_years_return_period,
    gte_5_rp = Q >= x5_years_return_period,
    gte_20_rp = Q >= x20_years_return_period
  )


gauge_ids_breaching <- df_forecast_long %>% 
  filter(gte_2_rp) %>% 
  distinct(gauge_id) %>% 
  pull(gauge_id)

gdf_gauge_pts <- gdf_gauge %>% 
  mutate(
    lgl_gauge_status =gauge_id %in% gauge_ids_breaching,
    `Gauge Status` =if_else(lgl_gauge_status,"Threshold Exceeded","Below Threshold"),
    aes_dot_size =if_else(lgl_gauge_status,0.01,.005),
    aes_dot_alpha = if_else(lgl_gauge_status,1,0.5)
  )

# Aggregate Alert Status to basin level.
df_basin_alert_status <- accum_pct_gauges_breached(
  df = df_forecast_long,
  date="date_predict",
  lgl_var = "gte_2_rp"
) %>% 
  group_by(basin_name) %>% 
  slice_max(
    order_by = cum_pct,
    with_ties = F
  ) %>% 
  ungroup() %>% 
  mutate(
    `Basin Alert Status` = if_else(cum_pct >= 0.8 ,"Warning","No Warning")
  )

gdf_basin_alert_poly <- gdf_basins_poly %>% 
  left_join(
    df_basin_alert_status,by="basin_name"
  )

gdf_basin_alert_lines <- st_cast(gdf_basin_alert_poly,"MULTILINESTRING")


# Alert Map ---------------------------------------------------------------

m_basin_alerts <- nga_base_map(
  west_africa_adm0 = L$west_central_africa,
  country_fill = "white",
  surrounding_fill = hdx_colors()["gray-dark"]
)+
  tm_shape(
    gdf_basin_alert_poly
    
  )+
  tm_polygons(col= "Basin Alert Status",
              palette=c(
                `No Warning`= hdx_colors()["mint-ultra-light"],
                `Warning` = hdx_colors()["tomato-hdx"]
              ) ,
              alpha = 0.5,
              border.col = NULL 
  )+
  tm_shape(
    gdf_basin_alert_lines,
    legend.show=F
  )+
  tm_lines(col = "Basin Alert Status",
           legend.col.show = F,
           palette=c(hdx_colors()["mint-hdx"],
                     hdx_colors()["tomato-dark"]),
           lwd=4,alpha = 0.3
  )+
  tm_shape(L$admin_1)+
  tm_borders(
    col=hdx_colors()["gray-dark"],
    alpha = 0.2
  )+
  tm_shape(L$river)+
  tm_lines(
    col=hdx_colors()["sapphire-hdx" ],
    lwd=3)+
  # gauge locations
  tm_shape(gdf_gauge_pts,
           legend.show=F
  )+
  tm_dots(
    col="Gauge Status",
    size= 0.2,
    palette=c("#25252533","black"),
    legend.size.show=F
  )+
  tm_shape(gdf_basin_alert_poly)+
  tm_text(text = "basin_name",shadow = T)+
  tm_layout(main.title.size = 0.5,
            title.size = 0.5,
            legend.text.size = 0.5,
            legend.title.size = 0.7,
            bg.color = "lightblue")




# Plot --------------------------------------------------------------------
p_discharge <- plot_average_discharge_normalized(
  df = df_forecast_long,
  date = "date_predict",
  threshold = 0.8,
  basin_palette = basin_pal()
    )

txt_warning_status <- ifelse(
  any(df_basin_alert_status$`Basin Alert Status`=="Warning"),"Warning","No Warning"
  )


# config email ------------------------------------------------------------

date_prediction_made <- df_forecast_long$date %>% unique()
dt_made_chr <- gsub("^0", "", format(as_date(date_prediction_made), "%d %B %Y"))

# Generate conditional email subject
subj_email <-  paste0("Nigeria Riverine Flood Monitoring: ",
                      dt_made_chr)

# will need to write code w/ `{googledrive}` package to access from service account/GH Action runner


drive_download(
  as_id("1A1WPSWBPJKFDBqZb1OXYHipsYxEErR-7"),
  email_receps_fp <- tempfile(fileext = ".csv")
)


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

email_rmd_fp <- "email_flood_monitoring.Rmd"
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



