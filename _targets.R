library(targets)
library(sf)
library(tidyverse)
library(rhdx)
library(terra)
library(ncdf4)
# library(tarchetypes)
tar_source()

# Set target options:
tar_option_set(
  packages = c("tibble"), # packages that your targets need to run
  format = "rds" # default storage format
  # Set other options as needed.
)
# tar_make_clustermq() configuration (okay to leave alone):
options(clustermq.scheduler = "multicore")

input_dir <- file.path(
  Sys.getenv("GFH_DATA_DIR"),
  "inputs"
)
gfh_dsn <- file.path(
  input_dir,
  "Nigeria AOIs.kml"
)

list(
  # Track Files -------------------------------------------------------------
  tar_target(
    name = nga_aoi_kml_fp,
    command = file.path(
      input_dir,
      "Nigeria AOIs.kml"
    ),
    format = "file"
  ),
  tar_target(
    name = nga_pop_r_fp,
    command = file.path(
      input_dir,
      "nga_pop",
      "nga_general_2020.tif"
    ),
    format = "file"
  ),
  tar_target(
    name = nga_adm_fp,
    command = file.path(
      Sys.getenv("AA_DATA_DIR"),
      "public",
      "raw",
      "nga",
      "cod_ab",
      "nga_admbnda_adm0_osgof_20190417.shp"
    ),
    format = "file"
  ),

  # Load Data ---------------------------------------------------------------
  ## ADM 0 ####
  tar_target(
    name = nga_adm,
    command = read_sf(nga_adm_fp)
  ),
  tar_target(
    name = gfh_coverage,
    command = c("Nigeria_AOIs", "Nigeria_launched") %>%
      map_dfr(\(lyr_name){
        st_read(dsn = gfh_dsn, layer = lyr_name) %>%
          mutate(
            coverage = lyr_name
          )
      })
  ),
  tar_target(
    name = gfh_now,
    command = gfh_coverage %>%
      filter(coverage == "Nigeria_launched")
  ),
  ## River MultiLines ####
  tar_target(
    name = nga_riv,
    command = pull_dataset("741c6f20-6956-420d-aae4-37015cdd1ad4") %>%
      get_resource(2) %>%
      read_resource()
  ),
  tar_target(
    name = nig_riv_b15k,
    command = nga_riv %>%
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
  )
)
