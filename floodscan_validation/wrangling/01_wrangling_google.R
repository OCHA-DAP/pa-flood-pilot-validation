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
      "historic_nowcasts"
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
    "google_reanalysis.csv"
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
  
#############################
#### FLOODSCAN WRANGLING ####
#############################

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

sf_hybas_buffer <- sf_hybas_centroids %>%
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
      ID = 1:60,
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

############################################
#### REPEAT THIS FOR ALL BASINS LEVEL 4 ####
############################################

sf_hybas_04 <- read_sf(
  file.path(
    input_dir,
    "hybas_af_lev01-12_v1c",
    "hybas_af_lev04_v1c.shp"
  )
) %>%
  st_make_valid() %>%
  st_filter(
    sf_hybas_centroids,
    .predicate = st_contains
  )

df_hybas_fs_04 <- map(
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
      y = sf_hybas_04,
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


df_hybas_fs_04 %>%
  left_join(
    data.frame(
      ID = 1:7,
      HYBAS_ID = sf_hybas_04$HYBAS_ID
    ),
    by = "ID"
  ) %>%
  rename(
    sfed = lyr.1
  ) %>%
  write_csv(
    file.path(
      output_dir,
      "google_hybas_04_fs_mean.csv"
    )
  )
