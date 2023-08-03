load_clipped_hydrobasins <- function(gdb, lyr_names, mask) {
  basins <- c(lyr_names) %>%
    map(\(lyr_name){
      basin <- st_read(dsn = gdb, layer = lyr_name) %>%
        mutate(
          level = lyr_name
        )
      basin_valid <- st_make_valid(basin)
      basin_valid[mask, ]
    }) %>%
    set_names(lyr_names)
}

# alert helper function
# only generate alert when boundary crossed from below
lim_alert <- function(x, lim) {
  x >= lim & lag(x) < lim
}

classify_google_historical_data <- function(historical = google_historical, rp_df = gauge_google_wb$return_period) {
  # since each gauge has unique RPs we need to split the nowcast data by gauge id before classifyng
  historical_split <- historical %>%
    mutate(
      gauge_id = paste0("hybas_", hybas_station)
    ) %>%
    split(.$gauge_id)

  rp_df_long <- rp_df %>%
    pivot_longer(
      -gauge_id
    ) %>%
    mutate(
      rp_yr = parse_number(name)
    )

  historical_classified <- historical_split %>%
    imap_dfr(
      \(dft, gid){
        rp_df_temp <- rp_df_long %>%
          filter(gauge_id == gid)
        dft %>%
          mutate(
            rp_2_flag = lim_alert(x = prediction, lim = rp_df_temp[rp_df_temp$rp_yr == 2, ]$value),
            rp_5_flag = lim_alert(x = prediction, lim = rp_df_temp[rp_df_temp$rp_yr == 5, ]$value),
            rp_20_flag = lim_alert(x = prediction, lim = rp_df_temp[rp_df_temp$rp_yr == 20, ]$value),
            lt_rp2 = prediction < rp_df_temp[rp_df_temp$rp_yr == 2, ]$value,
            gte_rp2 = prediction >= rp_df_temp[rp_df_temp$rp_yr == 2, ]$value,
            gte_rp5 = prediction >= rp_df_temp[rp_df_temp$rp_yr == 5, ]$value,
            gte_rp20 = prediction >= rp_df_temp[rp_df_temp$rp_yr == 20, ]$value
          )
      }
    )
  return(historical_classified)
}
