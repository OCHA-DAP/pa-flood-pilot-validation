library(targets)
library(sf)
library(tidyverse)
library(rhdx)
library(terra)
library(ncdf4)
library(readxl)
library(janitor)
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
hydrobasin_dsn <- file.path(
  input_dir,
  "hybas_af_lev01-12_v1c"
) 
google_nowcast_fp <- file.path(
  input_dir,
  "historic_nowcasts"
) 
nga_cod_gdb <- file.path(
  Sys.getenv("AA_DATA_DIR"),
  "public",
  "raw",
  "nga",
  "cod_ab",
  "nga_adm.shp.zip" )

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
    name = gauge_google_fp,
    command = file.path(
      input_dir,
      "nga_google_gauges.xlsx"
    ),
    format = "file"
  ),

  # Load Data ---------------------------------------------------------------
  ## ADM 0 ####
  tar_target(
    name = nga_adm,
    command = read_shape_zip(path = nga_cod_gdb,layer = c("nga_admbnda_adm0_osgof_20190417",
                                                          "nga_admbnda_adm1_osgof_20190417",
                                                          "nga_admbnda_adm2_osgof_20190417")) %>% 
      set_names(c("adm0","adm1","adm2"))
  ),
  tar_target(
    name = gauge_google_wb,
    command = read_all_tabs(gauge_google_fp,clean_names = T,skip = 0)
  ),
  tar_target(
    name= gauge_google_sp,
    command = st_as_sf(gauge_google_wb$metadata,coords=c("longitude","latitude"),crs=4326)
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
    name = basins_clipped,
    command = load_clipped_hydrobasins(
      lyr_names = c("hybas_af_lev04_v1c","hybas_af_lev05_v1c"),
      mask = nga_adm$adm0
      )
  ),
  tar_target(
    name= gauges_basin_google,
    command= basins_clipped %>% 
      map(\(lyr){
        st_join(gauge_google_sp,lyr)
      }
  )),
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
  ),
  tar_target(
    name = google_nowcast,
    command = map(
      .x = list.files(google_nowcast_fp,
        full.names = TRUE
      ),
      .f = ~read_csv(.x) %>%
        mutate(hybas_station = str_extract(.x, "([0-9]+)(?=.csv$)"))
    ) %>%
      list_rbind()
  ),
  tar_target(
    name = google_nowcast_class,
    command = classify_google_nowcast_data(nowcast = google_nowcast,rp_df = gauge_google_wb$return_period)
  ),
  tar_target(
    name = google_nowcast_basin,
    command= gauges_basin_google %>% 
      map(
        \(basin_level_df){
          basin_level_df %>% 
            st_drop_geometry() %>% 
            left_join(
              google_nowcast_class, by="gauge_id"
            )
        }
      )
  )
)
