library(sf)
library(tidyverse)
library(rhdx)
library(terra)
library(ncdf4)

input_dir <- file.path(
  Sys.getenv("GFH_DATA_DIR"),
  "inputs"
)

output_dir <- file.path(
  Sys.getenv("GFH_DATA_DIR"),
  "outputs"
)

################################################################################
########################## READING IN DATA #####################################
################################################################################

###########################
#### GOOGLE FLOOD DATA ####
###########################

gfh_coverage <- map(
  .x = c("added_in_dec", "aois_for_launch"),
  .f = \(layer) {
    read_sf(
      dsn = file.path(
        input_dir,
        "Nigeria Cover areas with google FF.kml"
      ),
      layer = layer
    )
  } %>%
    mutate(
      coverage = !!layer
    )
) %>%
  list_rbind() %>%
  st_as_sf()


gfh_now <- gfh_coverage %>%
  filter(
    coverage == "added_in_dec"
  )

gfh_future <- gfh_coverage %>%
  filter(
    coverage == "aois_for_launch"
  )

#################################
#### GRIDDED POPULATION DATA ####
#################################

# download data from HDX
pull_dataset(
  "62ec6c48-2f23-476b-8c1e-e924ad79908d"
) %>%
  get_resource(2) %>%
  download_resource(
    folder = input_dir,
    filename = "nga_pop_geotiff.zip"
  )

# unzip geojson file 
unzip(
  zipfile = file.path(
    input_dir,
    "nga_pop_geotiff.zip"
  ),
  exdir = file.path(
    input_dir,
    "nga_pop"
  )
)

# read in unzipped folder

nga_pop <- rast(
  file.path(
    input_dir,
    "nga_pop",
    "nga_general_2020.tif"
  )
)

# aggregate smaller

nga_pop_agg <- aggregate(
  x = nga_pop, 
  fact = 256,
  fun = "sum",
  na.rm = TRUE
)

######################
#### RIVER COURSE ####
######################

nga_rivers <- pull_dataset("741c6f20-6956-420d-aae4-37015cdd1ad4") %>%
  get_resource(2) %>%
  read_resource()

# reproject rivers for meter buffering
nga_niger <- nga_rivers %>%
  filter(
    NAME == "Niger"
  ) %>%
  st_transform("+proj=aeqd +lat_0=7.974 +lon_0=5.692") %>%
  st_buffer(
    dist = 15000
  ) %>%
  st_transform(
    "EPSG:4326"
  )

#######################
#### FLOOD EXTENTS ####
#######################

nga_extents <- pull_dataset("7d978dcf-afa9-454e-8eaa-d9e54458f44e") %>%
  get_resource(1) %>%
  read_resource()

###########################
#### HISTORICAL FLOODS ####
###########################

nga_hist <- read_sf(
  "/Users/caldwellst/Desktop/FloodArchive_region.shp"
) %>%
  filter(
    COUNTRY == "Nigeria"
  ) %>%
  st_set_crs("WGS84")

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
    "aer_mfed_area_300s_19980112_20221231_v05r01.nc"
  )
)

#### FS MASK ####

# set date start
fs_dates <- as.Date(ncvar_get(fs_nc, "time"), origin = "1998-01-12")
dates_idx <- which(fs_dates >= "2022-10-01" & fs_dates <= "2022-10-25")
dates_start <- min(dates_idx)
dates_count <- length(dates_idx)

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
  varid = "MFED_AREA",
  start = c(dates_start, lon_start, lat_start),
  count = c(dates_count, lon_count, lat_count)
)

# get maximum across all dates
fs_max <- apply(
  X = fs_array,
  MARGIN = 2:3,
  FUN = max,
  na.rm = TRUE
)

# create raster of Floodscan data
fs_rast <- rast(
  fs_max,
  extent = ext(min(lat_vals), max(lat_vals), min(lon_vals), max(lon_vals)),
  crs = "EPSG:4326"
) %>%
  t()

# get floodscan mask
# get mask

fs_mask <- ncvar_get(
  nc = fs_nc, 
  varid = "LWMASK_AREA",
  start = c(lon_start, lat_start),
  count = c(lon_count, lat_count)
) %>%
  rast(
    extent = ext(min(lat_vals), max(lat_vals), min(lon_vals), max(lon_vals)),
    crs = "EPSG:4326"
  ) %>%
  t() 
