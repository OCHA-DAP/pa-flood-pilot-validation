library(tidyverse)
library(sf)
library(ncdf4)
library(terra)

input_dir <- file.path(
  Sys.getenv("GFH_DATA_DIR"),
  "inputs"
)

output_dir <- file.path(
  Sys.getenv("GFH_DATA_DIR"),
  "outputs"
)

#################
#### ADMIN 0 ####
#################

nga_adm <- read_sf(
  file.path(
    Sys.getenv("AA_DATA_DIR"),
    "public",
    "raw",
    "nga",
    "cod_ab",
    "nga_admbnda_adm0_osgof_20190417.shp"
  )
)

###################
#### FLOODSCAN ####
###################

fs_nc <- nc_open(
  file.path(
    Sys.getenv("AA_DATA_DIR"),
    "private",
    "raw",
    "glb",
    "floodscan",
    "floodscan_flooded_fraction_africa_19980112-20221231_p00",
    "aer_sfed_area_300s_19980112_20221231_v05r01.nc"
  )
)

# set date start
fs_dates <- as.Date(ncvar_get(fs_nc, "time"), origin = "1998-01-12")
dates_start <- 1
dates_count <- length(fs_dates)

nga_bbox <- st_bbox(nga_adm)

# set lat start
fs_lat <- ncvar_get(fs_nc, "lat")
lat_idx <- which(fs_lat >= nga_bbox[2] & fs_lat <= nga_bbox[4])
lat_start <- min(lat_idx)
lat_count <- length(lat_idx)
lat_vals <- fs_lat[lat_start:(lat_start + lat_count - 1)]

# set lon start
fs_lon <- ncvar_get(fs_nc, "lon")
lon_idx <- which(fs_lon >= nga_bbox[1] & fs_lon <= nga_bbox[3])
lon_start <- min(lon_idx)
lon_count <- length(lon_idx)
lon_vals <- fs_lon[lon_start:(lon_start + lon_count - 1)]

fs_array <- ncvar_get(
  nc = fs_nc, 
  varid = "SFED_AREA",
  start = c(dates_start, lon_start, lat_start),
  count = c(dates_count, lon_count, lat_count)
)

####################################
#### GOOGLE FLOOD FORECAST DATA ####
####################################

df_google <- map(
  .x = list.files(
    file.path(
      input_dir,
      "historic_forecasts"
    ),
    full.names = TRUE
  ),
  .f = ~read_csv(.x) %>% mutate(hybas_station = str_extract(.x, "([0-9]+)(?=.csv$)"))
) %>%
  list_rbind()

write_csv(
  df_google,
  file.path(
    output_dir,
    "google_predictions.csv"
  )
)

#####################
#### HYDROBASINS ####
#####################

sf_hybas <- read_sf(
  file.path(
    input_dir,
    "hybas_af_lev01-12_v1c",
    "hybas_af_lev12_v1c.shp"
  )
) %>%
  filter(
    HYBAS_ID %in% as.numeric(df_google$hybas_station)
  ) %>%
  st_make_valid()
  
################################
#### GOOGLE FINAL WRANGLING ####
################################

# we will use centroids of the basins to buffer for floodscan data

sf_hybas_centroids <- sf_hybas %>%
  st_centroid()

write_sf(
  sf_hybas_centroids,
  file.path(
    output_dir,
    "google_centroids.gpkg"
  )
)

st_hybas_buffer <- st_hybas_centroids %>%
  st_transform("+proj=aeqd +lat_0=7.974 +lon_0=5.692") %>%
  st_buffer(
    dist = 30000
  ) %>%
  st_transform(
    "EPSG:4326"
  )

# for all dates of floodscan data, loop through and create raster,
# and then generate weighted average for each polygon (which will be % area flooded)

df_hybas_fs <- map(
  1:9120,
  .f = \(n) {
    fs_rast <- rast(
      fs_array[n,,],
      extent = ext(min(lat_vals), max(lat_vals), min(lon_vals), max(lon_vals)),
      crs = "EPSG:4326"
    ) %>%
      t()
    
    extract(
      x = fs_rast,
      y = sf_hybas_buffer,
      fun = mean,
      weights = TRUE
    ) %>%
      mutate(
        time = !!n
      )
  }
) %>%
  list_rbind() %>%
  mutate(
    time = as.Date(time, origin = "1998-01-12")
  )

# save out the data

df_hybas_fs %>%
  left_join(
    data.frame(
      ID = 1:20,
      HYBAS_ID = sf_hybas$HYBAS_ID
    ),
    by = "ID"
  ) %>%
  rename(
    sfed = lyr.1
  ) %>%
  write_csv(
    file.path(
      output_dir,
      "google_hybas_fs_mean_30k.csv"
    )
  )

#####################
#### GloFAS DATA ####
#####################

glofas_nc_files <- list.files(
  file.path(
    Sys.getenv(
      "OAP_DATA_DIR"
    ),
    "public",
    "processed",
    "nga",
    "glofas"
  ),
  full.names = TRUE
)

# get station data from specific GloFAS NC
get_station_data <- function(nc, station) {
  station_data <- ncvar_get(
    nc = nc,
    varid = station,
    start = c(7, 1, 1), # 7 day ahead forecast
    count = c(1, -1, -1)
  )
  
  ensemble <- apply( # take median of ensembles
    X = station_data,
    MARGIN = 1,
    FUN = median
  )
  
  data.frame(
    station = station,
    glofas_forecast = ensemble
  )
}

# get glofas data from a specific NetCDF file
get_glofas_data <- function(nc) {
  dates <- as.character(as.PCICt(nc$dim$time$vals, cal = "proleptic_gregorian", origin = "1970-01-01"))
  stations <- c(
    "Benue A Makurdi",
    "Benue A Umaisha",
    "Benue A Wuro Boki",
    "Ibi",
    "Kaduna A Wuya",
    "Katsina Ala A Katsina Ala",
    "Lokoja",
    "Na",
    "Niger A  Jebba Downstream Dam",
    "Niger A Baro",
    "Niger A Onitsha",
    "Sokoto A Kende",
    "Yidere Bode"
  )
  
  map(
    .x = stations,
    .f = \(x) get_station_data(nc = nc, station = x)
  ) %>%
    list_rbind() %>%
    mutate(
      date = rep(as.Date(dates), length(stations))
    )
}

# read in and process GloFAS data 
read_glofas_data <- function(fp) {
  nc <- nc_open(filename = fp)
  get_glofas_data(nc)
}

df_glofas <- map(
  .x = glofas_nc_files,
  .f = read_glofas_data
) %>%
  list_rbind() %>%
  complete(
    station,
    date = seq(as.Date("1999-01-03"), as.Date("2018-12-30"), by = 1)
  ) %>%
  group_by(
    station
  ) %>%
  mutate(
    interpolated = is.na(glofas_forecast),
    glofas_forecast = spline(date, glofas_forecast, n = n())$y
  ) %>%
  ungroup()

write_csv(
  x = df_glofas,
  file = file.path(
    output_dir,
    "glofas_forecasts_until_2018.csv"
  )
)

###############################
#### GloFAS Floodscan data ####
###############################

glofas_yaml <- yaml::read_yaml(
  file.path(
    input_dir,
    "nga_niger_benue.yaml"
  )
)

sf_glofas_pts <- glofas_yaml$glofas %>%
  as_tibble() %>%
  mutate(
    reporting_points = map(reporting_points, as_tibble)
  ) %>%
  unnest(
    reporting_points
  ) %>%
  st_as_sf(
    coords = c("lon", "lat"),
    crs = "EPSG:4326"
  )

# get buffer around points for getting floodscan data like we did with Google
sf_glofas_buffer <- sf_glofas_pts %>%
  st_transform("+proj=aeqd +lat_0=7.974 +lon_0=5.692") %>%
  st_buffer(
    dist = 30000
  ) %>%
  st_transform(
    "EPSG:4326"
  )

df_glofas_fs <- map(
  1:9120,
  .f = \(n) {
    fs_rast <- rast(
      fs_array[n,,],
      extent = ext(min(lat_vals), max(lat_vals), min(lon_vals), max(lon_vals)),
      crs = "EPSG:4326"
    ) %>%
      t()
    
    extract(
      x = fs_rast,
      y = sf_glofas_buffer,
      fun = mean,
      weights = TRUE
    ) %>%
      mutate(
        time = !!n
      )
  }
) %>%
  list_rbind() %>%
  mutate(
    time = as.Date(time, origin = "1998-01-12")
  )

df_glofas_fs %>%
  rename(
    sfed = lyr.1
  ) %>%
  write_csv(
    file.path(
      output_dir,
      "glofas_fs_mean_30k.csv"
    )
  )
