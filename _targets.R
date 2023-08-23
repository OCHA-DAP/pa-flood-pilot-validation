library(targets)
library(sf)
library(tidyverse)
library(rhdx)
library(terra)
library(ncdf4)
library(readxl)
library(janitor)
library(googlesheets4)
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

gs4_auth(
  path = Sys.getenv("GFF_JSON")
)

input_dir <- file.path(
  Sys.getenv("GFH_DATA_DIR"),
  "inputs"
)

nga_cod_gdb <- file.path(
  Sys.getenv("AA_DATA_DIR"),
  "public",
  "raw",
  "nga",
  "cod_ab",
  "nga_adm.shp.zip"
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
  # Load Data ---------------------------------------------------------------
  ## Base Data ####
  # base data includes admin zones, rivers, basins,
  ### Admin 0-2 ####
  tar_target(
    name = nga_adm,
    command = read_shape_zip(path = nga_cod_gdb, layer = c(
      "nga_admbnda_adm0_osgof_20190417",
      "nga_admbnda_adm1_osgof_20190417",
      "nga_admbnda_adm2_osgof_20190417"
    )) %>%
      set_names(c("adm0", "adm1", "adm2"))
  ),
  ### River MultiLines ####
  tar_target(
    name = nga_riv,
    command = pull_dataset("741c6f20-6956-420d-aae4-37015cdd1ad4") %>%
      get_resource(2) %>%
      read_resource()
  ),
  # buffer rivers
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
  ### Hydrobasins ####
  # clipping basin levels 4 & 5 to NGA
  tar_target(
    name = basins_clipped,
    command = load_clipped_hydrobasins(
      gdb = file.path(input_dir, "hybas_af_lev01-12_v1c"),
      lyr_names = c("hybas_af_lev04_v1c", "hybas_af_lev05_v1c"),
      mask = nga_adm$adm0
    )
  ),
  ## Google Inputs ####

  ### Flood Extent Kmls ####
  # note: not yet using this in the _targets pipeline - just adding as a simple target since it is
  # in other wrangling/analysis scripts
  tar_target(
    name = gfh_coverage,
    command = c("Nigeria_AOIs", "Nigeria_launched") %>%
      map_dfr(\(lyr_name){
        st_read(
          dsn = nga_aoi_kml_fp,
          layer = lyr_name
        ) %>%
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
  ### Google real-time nowcast/forecast ####
  
  # read in full wb from google.
  tar_target(
    name = gauge_google_wb_full,
    command = read_gauge_googlesheets(url = Sys.getenv("GFH_GAUGE_URL"))
  ),

  # remove any potential duplicate records by taking latest update_time_utc per date
  
  
    
  tar_target(
    name = gauge_data_google,
    command = gauge_google_wb_full %>%
      keep_at(
        at = ~ str_detect(.x, "hybas_") # gauge data prefix
        ) %>%
      bind_rows() %>%
      mutate(
        date = dmy(date),
        update_time_utc = ymd_hms(update_time_utc)
      )  %>% 
      # rm any potential duplicate date (take latest update)
      group_by(gauge_id,date) %>% 
      filter(update_time_utc==max(update_time_utc)) %>% 
      ungroup()
  ),
  tar_target(
    name = g_realtime_gauge,
    command = gauge_data_google %>%
      group_by(gauge_id) %>%
      filter(
        date == max(date), # get max date per gauge
      ) %>%
      ungroup()
  ),
  ### Google historical-reanalysis (nowcast) ####
  # compile all historical data into one data.frame
  tar_target(
    name = google_historical,
    command = map(
      .x = list.files(
        file.path(input_dir, "historic_nowcasts"),
        full.names = TRUE
      ),
      .f = ~ read_csv(.x) %>%
        mutate(hybas_station = str_extract(.x, "([0-9]+)(?=.csv$)"))
    ) %>%
      list_rbind()
  ),
  # Wrangling ####

  ## Realtime (nowcast & forecast) from google ####

  # create spatial object of gauge locations
  tar_target(
    name = gauge_google_sp,
    command = st_as_sf(gauge_google_wb_full$metadata,
                       coords = c("longitude", "latitude"), crs = 4326)
  ),
  # assign each gauge to a basin (for both basin levels 4 & 5)
  tar_target(
    name = gauges_basin_google,
    command = basins_clipped %>%
      map(\(lyr){
        st_join(gauge_google_sp, lyr)
      })
  ),

  ## Historical nowcast data ####
  # based on calculated RPs (from google) classify each prediction (daily 1981-current) for each gauge
  # we classify w/ logicals based on whether value is greater than or equal to RPs: 2,5,20
  # also flag
  tar_target(
    name = google_historical_class,
    command = classify_google_historical_data(
      historical = google_historical,
      rp_df = gauge_google_wb_full$return_period
    )
  ),
  # take the classified now class data and just add the basin id's
  tar_target(
    name = google_historical_basin,
    command = gauges_basin_google %>%
      map(
        \(basin_level_df){
          basin_level_df %>%
            st_drop_geometry() %>%
            left_join(
              google_historical_class,
              by = "gauge_id"
            )
        }
      )
  ),
  # might want to turn this into a function so that we can easily change the +/- days parameter
  # Overview: 
    # - find all distinct gauge+basin+date+discharge records where threshold (RP2) was exceeded
    # - find filter to those basin + dates in full historical and scan +/- n (5) days in each basin
    # - count the number of other gauges in the basin that crossed in that period (n)
  
  tar_target(
    name = ghistorical_rp2_breach_pct_basin_gauges,
    command = google_historical_basin %>%
      map(\(dft){
        # a little wrangling - should do in earlier step an rm from here later
        dft_c <- dft %>%
          rename(
            hybas_id = HYBAS_ID
          ) %>%
          mutate(
            hybas_id = as.character(hybas_id)
          )
        # find all distinct flagging events (when RP 2 was exceeded)
        distinct_flags <- dft_c %>%
          filter(rp_2_flag) %>%
          distinct(
            hybas_id,
            gauge_id,
            time
          )

        distinct_flags %>%
          pmap_dfr(function(...) {
            current <- tibble(...)

            # filter ncst to +/-x days
            ncst_filtered <- dft_c %>%
              filter(
                time >= current$time - 5,
                time <= current$time + 5,
                hybas_id == current$hybas_id
              )
            # get gauge count per basin - could grab this externally as well
            basin_gauge_count <- dft_c %>%
              group_by(hybas_id,
                yr = year(time),
                time
              ) %>%
              summarise(
                num_gauge = n(), .groups = "drop"
              ) %>%
              distinct(hybas_id, num_gauge)

            ncst_filtered %>%
              mutate(
                date = current$time
              ) %>%
              group_by(
                hybas_id,
                date,
                gauge_id,
                .drop = F,
              ) %>%
              summarise(
                discharge_crossed = any(gte_rp2), .groups = "drop_last"
              ) %>%
              summarise(
                gauges_crossed = sum(discharge_crossed, na.rm = T),
                .groups = "drop"
              ) %>%
              left_join(basin_gauge_count, by = "hybas_id") %>%
              mutate(
                pct_crossed = gauges_crossed / num_gauge
              )
          }) # close pmap
      }) # close first map
  )
)
