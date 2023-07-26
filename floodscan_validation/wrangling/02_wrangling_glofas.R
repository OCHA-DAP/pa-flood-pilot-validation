library(tidyverse)
library(sf)
library(ncdf4)
library(terra)
library(PCICt)

#####################
#### GloFAS DATA ####
#####################

glofas_files <- list.files(
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

# only nc files
glofas_nc_files <- glofas_files[str_detect(glofas_files, ".nc$")]

# get station data from specific GloFAS NC
get_station_data <- function(nc, station) {
  # get start and end variable based on dims
  # since sometimes has 2 sometimes 3 dimes
  station_data <- ncvar_get(
    nc = nc,
    varid = station,
    start = c(7, rep( 1, length(nc$dim) - 1)), # 7 day ahead forecast
    count = c(1, rep(-1, length(nc$dim) - 1))
  )
  
  # take median of vector or the general array
  if (length(dim(station_data)) > 1) {
    ensemble <- apply( # take median of ensembles
      X = station_data,
      MARGIN = 1,
      FUN = median
    )
  } else {
    ensemble <- median(station_data)
  }
  
  data.frame(
    station = station,
    glofas_forecast = ensemble
  )
}

# get glofas data from a specific NetCDF file
get_glofas_data <- function(nc, fp = NULL) {
  if ("time" %in% names(nc$dim)) {
    dates <- as.character(as.PCICt(nc$dim$time$vals, cal = "proleptic_gregorian", origin = "1970-01-01"))
  } else {
    dates <- str_extract(fp, "[\\d]{4}-[\\d]{2}-[\\d]{2}")
  }
  
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
  get_glofas_data(nc, fp = fp)
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
