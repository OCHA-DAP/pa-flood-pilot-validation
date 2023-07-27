library(tidyverse)
library(sf)
library(gghdx)
library(googlesheets4)
library(rhdx)
library(patchwork)

gghdx()

input_dir <- file.path(
  Sys.getenv("GFH_DATA_DIR"),
  "inputs"
)

output_dir <- file.path(
  Sys.getenv("GFH_DATA_DIR"),
  "outputs"
)

###################################
#### READ IN THE WRANGLED DATA ####
###################################

# geopoints of gauges

sf_gauges <- read_sheet(
  ss = as_sheets_id("1FN8TvfmKejE6vv-wbjrcIvt8tpl3_p9EfCfq-bXpEtw"),
  sheet = "metadata"
) %>%
  st_as_sf(
    coords = c("longitude", "latitude"),
    crs = "EPSG:4326"
  )

# return periods for gauges

google_rp <- read_sheet(
  ss = as_sheets_id("1FN8TvfmKejE6vv-wbjrcIvt8tpl3_p9EfCfq-bXpEtw"),
  sheet = "return_period"
)

# files for mapping

nga_rivers <- pull_dataset("741c6f20-6956-420d-aae4-37015cdd1ad4") %>%
  get_resource(2) %>%
  read_resource()

nga_adm0 <- read_sf(
  file.path(
    Sys.getenv("AA_DATA_DIR"),
    "public",
    "raw",
    "nga",
    "cod_ab",
    "nga_admbnda_adm0_osgof_20190417.shp"
  )
)

# river basins

basins_list <- map(
  .x = 3:5,
  .f = \(n) {
    read_sf(
      file.path(
        input_dir,
        "hybas_af_lev01-12_v1c",
        paste0("hybas_af_lev0", n, "_v1c.shp")
      )
    ) %>%
      st_make_valid() %>%
      st_intersection(nga_adm0) %>%
      mutate(
        HYBAS_LEVEL = !!n
      )
  }
)

sf_basins <- list_rbind(basins_list) %>%
  st_as_sf()


# floodscan validation data

fs_gauge <- read_csv(
  file.path(
    output_dir,
    "google_hybas_fs_mean_30k.csv"
  )
)

fs_basin <- read_csv(
  file.path(
    output_dir,
    "google_hybas_04_fs_mean_30k.csv"
  )
)

# google historical re-analysis

google_reanalysis <- read_csv(
  file.path(
    output_dir,
    "google_predictions.csv"
  )
)

sf_google <- read_sf(
  file.path(
    output_dir,
    "google_centroids.gpkg"
  )
)

#######################
#### BASIN CHOICES ####
#######################

sf_basins$num_gauges <- st_intersects(
  x = sf_basins,
  y = sf_gauges,
  sparse = FALSE
) %>%
  apply(
    MARGIN = 1,
    FUN = sum
  )

# look at the number of gauges per basin

basin_bar_chart <- sf_basins %>%
  filter(
    num_gauges > 0
  ) %>%
  group_by(
    HYBAS_LEVEL
  ) %>%
  arrange(
    num_gauges,
    .by_group = TRUE
  ) %>%
  mutate(
    plot_id = row_number()
  ) %>%
  ggplot(
    aes(
      x = plot_id,
      y = num_gauges
    )
  ) +
  geom_bar(
    stat = "identity"
  ) +
  facet_wrap(
    ~ HYBAS_LEVEL,
    nrow = 1,
    scales = "free",
    labeller = labeller(
      HYBAS_LEVEL = \(x) paste0("Basin level: ", x)
    )
  ) +
  labs(
    x = "Basin",
    y = "# of gauges",
    title = "Gauages per basin, based on basin level"
  )

gauges_map <- ggplot(
  sf_basins
) +
  geom_sf() +
  geom_sf(
    data = sf_gauges
  ) +
  facet_wrap(
    ~ HYBAS_LEVEL,
    labeller = labeller(
      HYBAS_LEVEL = \(x) paste0("Basin level: ", x)
    )
  ) +
  coord_sf(
    datum = NA
  ) +
  labs(
    title = "Map of hydrobasins and gauges"
  )

gauges_map +
  basin_bar_chart +
  plot_layout(
    ncol = 1
  )

########################################
#### GET FLOODSCAN THRESHOLD LEVELS ####
########################################

# simple approach, take max avlue of each year we have full data for
# and then take the median, which will ensure half the years would trigger
# and half would not for an approx 2 year RP thresholding
fs_gauge_thresholds <- fs_gauge %>%
  mutate(
    year = lubridate::year(time)
  ) %>%
  filter(
    year %in% 1999:2022
  ) %>%
  group_by(
    HYBAS_ID,
    year
  ) %>%
  summarize(
    sfed_max = max(sfed),
    .groups = "drop_last"
  ) %>%
  summarize(
    sfed_threshold = median(sfed_max),
    .groups = "drop"
  )

# now get the times that these gauges have crossed the threshold

fs_gauge_activations <- fs_gauge %>%
  left_join(
    fs_gauge_thresholds,
    by = "HYBAS_ID"
  ) %>%
  mutate(
    fs_above_threshold = sfed >= sfed_threshold
  )

# now find when the google data has passed their thresholds

google_gauge_activations <- google_reanalysis %>%
  mutate(
    gauge_id = paste0("hybas_", hybas_station)
  ) %>%
  left_join(
    google_rp,
    by = "gauge_id"
  ) %>%
  mutate(
    google_activation_2y_rp = prediction >= `2_year_return_period`,
    google_activation_5y_rp = prediction >= `5_year_return_period`,
    google_activation_20y_rp = prediction >= `20_year_return_period`,
  )

# now bring together for validation
# we will consider an activation valid if within 7 days of the google gauge
# reaching the 2 year RP, the floodscan around the gauge also did

gauge_validation <- google_gauge_activations %>%
  select(
    hybas_station,
    time,
    google_activation_2y_rp
  ) %>%
  inner_join(
    select(
      fs_gauge_activations,
      hybas_station = HYBAS_ID,
      time,
      fs_above_threshold
    ),
    by = c("hybas_station", "time")
  ) %>%
  group_by(
    hybas_station
  ) %>%
  mutate(
    fs_above_threshold_7_days = zoo::rollsum(
      x = fs_above_threshold,
      k = 7,
      fill = FALSE,
      align = "left"
    ) > 0,
    google_id = cumsum(google_activation_2y_rp != lag(google_activation_2y_rp, default = FALSE)),
    fs_id = cumsum(fs_above_threshold_7_days != lag(fs_above_threshold_7_days, default = FALSE)),
  )


# median flood duration

gauge_validation %>%
  group_by(hybas_station, fs_id) %>%
  filter(
    fs_above_threshold
  ) %>%
  summarize(
    n = n(),
    .groups = "drop_last"
  ) %>%
  summarize(
    median_flood_length = median(n),
    .groups = "drop"
  ) %>%
  mutate(
    gauge_id = paste0("hybas_", hybas_station)
  ) %>%
  right_join(
    sf_gauges,
    by = "gauge_id"
  ) %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(
    data = nga_adm0,
    fill = "white"
  ) +
  geom_sf(
    data = nga_rivers
  ) +
  geom_sf(
    aes(
      color = median_flood_length
    )
  ) +
  coord_sf(
    datum = NA
  ) +
  scale_color_gradient(
    limits = c(0, 28),
    low = "white",
    high = hdx_hex("sapphire-hdx")
  ) +
  labs(
    title = "Flood duration around gauges",
    subtitle = "Measured as days flood extents above 2 year RP",
    color = "Median flood duration (days)"
  )

# median time above 2 year rp

gauge_validation %>%
  group_by(hybas_station, google_id) %>%
  filter(
    google_activation_2y_rp
  ) %>%
  summarize(
    n = n(),
    .groups = "drop_last"
  ) %>%
  summarize(
    median_time_above_rp_length = median(n),
    .groups = "drop"
  ) %>%
  mutate(
    gauge_id = paste0("hybas_", hybas_station)
  ) %>%
  right_join(
    sf_gauges,
    by = "gauge_id"
  ) %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(
    data = nga_adm0,
    fill = "white"
  ) +
  geom_sf(
    data = nga_rivers
  ) +
  geom_sf(
    aes(
      color = median_time_above_rp_length
    )
  ) +
  coord_sf(
    datum = NA
  ) +
  scale_color_gradient(
    limits = c(0, 42),
    low = "white",
    high = hdx_hex("sapphire-hdx")
  ) +
  labs(
    title = "Median time above 2 year RP",
    subtitle = "Measured as days nowcast above 2 year RP",
    color = "Median duration above 2 year RP (days)"
  )

# metric generation
# true and false positives
# look across entire time of nowcast + 7 days

gauge_positives <- gauge_validation %>%
  filter(
    google_activation_2y_rp
  ) %>%
  group_by(
    hybas_station,
    google_id
  ) %>%
  summarize(
    positive = sum(fs_above_threshold_7_days) > 0,
    .groups = "drop_last"
  ) %>%
  summarize(
    tp = sum(positive),
    fp = sum(!positive)
  )

# false and true negatives
# although ignoring TN for validation, just using precision/recall

gauge_negatives <- gauge_validation %>%
  filter(
    !fs_above_threshold_7_days
  ) %>%
  group_by(
    hybas_station,
    fs_id
  ) %>%
  summarize(
    negative = sum(google_activation_2y_rp) == 0,
    .groups = "drop_last"
  ) %>%
  summarize(
    tn = sum(negative),
    fn = sum(!negative),
    .groups = "drop"
  )

# bring metrics together

gauge_metrics <- gauge_positives %>%
  left_join(
    gauge_negatives,
    by = "hybas_station"
  ) %>%
  mutate(
    Recall = tp / (tp + fp),
    Precision = tp / (tp + fn)
  )

# map the metrics

gauge_metrics %>%
  pivot_longer(
    cols = Recall:Precision
  ) %>%
  mutate(
    gauge_id = paste0("hybas_", hybas_station)
  ) %>%
  right_join(
    sf_gauges,
    by = "gauge_id"
  ) %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(
    data = nga_adm0,
    fill = "white"
  ) +
  geom_sf(
    data = nga_rivers
  ) +
  geom_sf(
    aes(
      color = value
    )
  ) +
  coord_sf(
    datum = NA
  ) +
  facet_wrap(
    ~ name
  ) +
  expand_limits(
    color = 0
  ) +
  scale_color_gradient(
    low = "white",
    high = hdx_hex("mint-hdx")
  ) +
  labs(
    color = "Metric",
    title = "Performance by gauges"
  )

# identify stations for removal

sf_gauges %>%
  ggplot() +
  geom_sf(
    data = nga_adm0
  ) +
  geom_sf(
    data = nga_rivers
  ) +
  geom_sf() +
  geom_sf_text(
    aes(
      label = gauge_id
    )
  )

# filter out stations and check final results

sf_gauges_filtered <- sf_gauges %>%
  filter(
    !(gauge_id %in% c("hybas_1120794570", "hybas_1120741070", "hybas_1120946640", "hybas_1120974450", "hybas_1120981190"))
  ) 

final_basins_map <- sf_gauges_filtered %>%
  st_join(
    sf_basins %>%
      filter(HYBAS_LEVEL == 4) %>%
      mutate(
        HYBAS_ID = as.character(HYBAS_ID)
      ),
    join = st_intersects
  ) %>%
  ggplot() +
  geom_sf(
    data = filter(sf_basins, HYBAS_LEVEL == 4),
    fill = "white"
  ) +
  geom_sf(
    data = nga_rivers
  ) +
  geom_sf(
    aes(
      color = HYBAS_ID
    )
  ) +
  coord_sf(
    datum = NA
  ) +
  scale_color_manual(
    guide = "none",
    values = unname(hdx_hex(
      c(
        "sapphire-hdx",
        "mint-hdx",
        "tomato-hdx",
        "gray-dark"
      )
    ))
  ) +
  guides(
    color = "none"
  ) +
  labs(
    title = "Final basins and gauges"
  )


sf_basins$num_gauges_filtered <- st_intersects(
  x = sf_basins,
  y = sf_gauges_filtered,
  sparse = FALSE
) %>%
  apply(
    MARGIN = 1,
    FUN = sum
  )

final_gauges_bar <- sf_basins %>%
  filter(
    HYBAS_LEVEL == 4,
    num_gauges_filtered > 0
  ) %>%
  arrange(
    num_gauges_filtered
  ) %>%
  mutate(
    plot_id = row_number()
  ) %>%
  ggplot(
    aes(
      x = plot_id,
      y = num_gauges_filtered
    )
  ) +
  geom_bar(
    stat = "identity"
  ) +
  labs(
    x = "Basin",
    y = "# of gauges",
    title = "Final gauges per basin"
  )

final_plot <- final_basins_map + final_gauges_bar

################################
#### BASIN LEVEL VALIDATION ####
################################

# get station activations and basins together
basin_activation <- sf_gauges_filtered %>%
  st_join(
    sf_basins %>%
      filter(
        HYBAS_LEVEL == 4
      ),
    join = st_intersects
  ) %>%
  select(
    gauge_id, basin_id = HYBAS_ID
  ) %>%
  inner_join(
    google_gauge_activations,
    by = "gauge_id"
  ) %>%
  as_tibble() %>%
  select(
    -geometry
  )

# simple approach, take max avlue of each year we have full data for
# and then take the median, which will ensure half the years would trigger
# and half would not for an approx 2 year RP thresholding
fs_basin_thresholds <- fs_basin %>%
  mutate(
    year = lubridate::year(time)
  ) %>%
  filter(
    year %in% 1999:2022
  ) %>%
  group_by(
    HYBAS_ID,
    year
  ) %>%
  summarize(
    sfed_max = max(sfed),
    .groups = "drop_last"
  ) %>%
  summarize(
    sfed_threshold = median(sfed_max),
    .groups = "drop"
  )

# now check when the basins cross these thresholds
fs_basin_activations <- fs_basin %>%
  left_join(
    fs_basin_thresholds,
    by = "HYBAS_ID"
  ) %>%
  mutate(
    fs_above_threshold = sfed >= sfed_threshold
  ) %>%
  group_by(
    basin_id = HYBAS_ID
  ) %>%
  mutate(
    fs_above_threshold_7_days = zoo::rollsum(
      x = fs_above_threshold,
      k = 7,
      align = "left",
      fill = FALSE
    ) > 0
  ) %>%
  ungroup()

# now do the validations across various metrics

basin_pcts <- basin_activation %>%
  group_by(
    basin_id,
    time
  ) %>%
  summarize(
    across(
      .cols = `google_activation_2y_rp`:`google_activation_20y_rp`,
      .fns = mean
    ),
    .groups = "drop"
  ) %>%
  rename_with(
    .cols = starts_with("google"),
    .fn = \(x) str_remove(x, "google_activation_")
  )

pcts <- seq(.10, .90, by = .1)
rps <- c(2, 5, 20)

rep_grid <- expand_grid(pcts, rps)

df_basin_metrics <- map2(
  .x = rep_grid$pcts,
  .y = rep_grid$rps,
  .f = \(pct, rp) {
    basin_valid <- basin_pcts %>%
      mutate(
        activation = .data[[paste0(rp, "y_rp")]] >= !!pct
      ) %>%
      inner_join(
        select(
          fs_basin_activations,
          basin_id,
          time,
          fs_above_threshold_7_days
        ),
        by = c("basin_id", "time")
      ) %>%
      group_by(
        basin_id
      ) %>%
      mutate(
        basin_group = cumsum(activation != lag(activation, default = FALSE)),
        fs_group = cumsum(fs_above_threshold_7_days != lag(fs_above_threshold_7_days, default = FALSE))
      )
    
    basin_positives <- basin_valid %>%
      filter(
        activation
      ) %>%
      group_by(
        basin_id,
        basin_group
      ) %>%
      summarize(
        positive = sum(fs_above_threshold_7_days) > 0,
        .groups = "drop_last"
      ) %>%
      summarize(
        tp = sum(positive),
        fp = sum(!positive),
        .groups = "drop"
      )
    
    basin_negatives <- basin_valid %>%
      filter(
        !activation
      ) %>%
      group_by(
        basin_id,
        basin_group
      ) %>%
      summarize(
        negative = sum(fs_above_threshold_7_days) == 0,
        .groups = "drop_last"
      ) %>%
      summarize(
        tn = sum(negative),
        fn = sum(!negative),
        .groups = "drop"
      )
    
    basin_metrics <- left_join(
      basin_positives,
      basin_negatives,
      by = "basin_id"
    ) %>%
      mutate(
        precision = tp / (tp + fp),
        recall = tp / (tp + fn),
        f1 = precision * recall / (precision + recall),
        threshold = !!pct,
        rp = !!rp
      )
  }
) %>%
  list_rbind()


# map out the metrics results
df_metrics_plot <- df_basin_metrics %>%
  filter(
    rp != 20
  ) %>%
  pivot_longer(
    precision:f1
  ) %>%
  group_by(
    basin_id,
    name,
    rp
  ) %>%
  mutate(
    rp = as.factor(rp),
    value = replace_na(value, 0),
    best_rp = value == max(value, na.rm = TRUE),
    basin_name = factor(
      case_when(
        basin_id == 1040760290 ~ "Upper Niger",
        basin_id == 1040909890 ~ "Lower Niger",
        basin_id == 1040022420 ~ "Niger Delta",
        basin_id == 1040909900 ~ "Benue"
      ),
      levels = c("Upper Niger", "Lower Niger", "Benue", "Niger Delta")
    )
  ) %>%
  group_by(
    basin_id,
    name,
  ) %>%
  mutate(
    best_overall = value == max(value, na.rm = TRUE)
  )


base_metrics_plot <- df_metrics_plot %>%
  ggplot(
    aes(
      x = threshold,
      y = rp,
      fill = value
    )
  ) +
  geom_tile(
    color = "white"
  ) +
  facet_grid(
    rows = vars(basin_name),
    cols = vars(name),
    labeller = labeller(.cols = str_to_title)
  ) +
  scale_x_continuous(
    labels = scales::label_percent()
  ) +
  scale_fill_gradient(
    low = "white",
    high = hdx_hex("mint-hdx")
  ) + 
  theme(
    panel.grid = element_blank()
  ) +
  labs(
    x = "Threshold",
    y = "Return period",
    fill = "Metric"
  )

base_metrics_plot

# look at overall best perofmring options
base_metrics_plot + geom_tile(
  data = filter(df_metrics_plot, best_overall),
  fill = NA,
  color = hdx_hex("gray-dark"),
  lwd = .5
)

# look at best performing by RP if we want to look at RPs

base_metrics_plot + geom_tile(
  data = filter(df_metrics_plot, best_rp),
  fill = NA,
  color = hdx_hex("gray-dark"),
  lwd = .5
)


# look at overall metrics across all basins

df_metrics_overall <- df_metrics_plot %>%
  group_by(
    threshold,
    rp
  ) %>%
  summarize(
    across(
      .cols = tp:fn,
      .fns = sum
    )
  ) %>%
  mutate(
    precision = tp / (tp + fp),
    recall = tp / (tp + fn),
    f1 = precision * recall / (precision + recall)
  ) %>%
  pivot_longer(
    precision:f1
  ) %>%
  group_by(
    rp,
    name
  ) %>%
  mutate(
    best = value == max(value)
  )

df_metrics_overall %>%
  ggplot(
    aes(
      x = threshold,
      y = rp,
      fill = value
    )
  ) +
  geom_tile() +
  facet_wrap(
    ~ name,
    labeller = labeller(.cols = str_to_title)
  ) +
  scale_x_continuous(
    labels = scales::label_percent()
  ) +
  scale_fill_gradient(
    low = "white",
    high = hdx_hex("mint-hdx")
  ) + 
  scale_color_manual(
    values = c("white", "black")
  ) +
  theme(
    panel.grid = element_blank()
  ) +
  labs(
    x = "Threshold",
    y = "Return period",
    fill = "Metric"
  )

#################################################################
#### CHECK FREQUENCY OF ACTIVATION BASED ON THESE THRESHOLDS ####
#################################################################

final_activations <- basin_pcts %>%
  mutate(
    activation_2y = `2y_rp` >= 1,
    activation_5y = `2y_rp` >= 0.5,
    activation_20y = `2y_rp` > 0.2,
    basin_name = factor(
      case_when(
        basin_id == 1040760290 ~ "Upper Niger",
        basin_id == 1040909890 ~ "Lower Niger",
        basin_id == 1040022420 ~ "Niger Delta",
        basin_id == 1040909900 ~ "Benue"
      ),
      levels = c("Upper Niger", "Lower Niger", "Benue", "Niger Delta")
    )
  ) %>%
  pivot_longer(
    starts_with("activation")
  ) %>%
  filter(
    value
  ) %>%
  group_by(
    basin_id, name
  ) %>%
  arrange(
    time,
    .by_group = TRUE
  ) %>%
  mutate(
    day_group = c(0, cumsum(diff(time) != 1))
  ) %>%
  group_by(
    basin_name,
    name,
    day_group
  ) %>%
  summarize(
    start_time = min(time),
    end_time = max(time),
    start_year = lubridate::year(start_time),
    end_year = lubridate::year(end_time),
    .groups = "drop"
  )

final_activations %>%
  group_by(
    basin_name,
    name,
    start_year
  ) %>%
  summarize(
    n = n(),
    .groups = "drop"
  ) %>%
  mutate(
    name = factor(name, levels = c("activation_2y", "activation_5y", "activation_20y"))
  ) %>%
  ggplot(
    aes(
      x = start_year,
      y = fct_rev(basin_name),
      fill = n
    )
  ) +
  geom_tile(
    color = "white"
  ) +
  facet_wrap(
    ~ name,
    ncol = 1,
    labeller = as_labeller(\(x, y) {
      lbls <- c(
        "activation_2yr" = "2 year RP (>= 80%)",
        "activation_5yr" = "5 year RP (>= 50%)",
        "activation_20yr" = "20 year RP (>= 20%)"
      )
      lbls[y] 
    }
    )
  ) +
  labs(
    y = "",
    x = "",
    title = "Activations per year in historical data",
    fill = "# of activations"
  )

# look at gauge level activations to see how frequent across a decade we are
# activating

google_gauge_activations %>%
  mutate(
    year = lubridate::year(time),
    decade = case_when(
      year <= 1992 ~ "1983 - 1992",
      year <= 2002 ~ "1993 - 2002",
      year <= 2012 ~ "2003 - 2012",
      year <= 2022 ~ "2013 - 2022"
    )
  ) %>%
  filter(
    between(year, 1983, 2022)
  ) %>%
  pivot_longer(
    starts_with("google")
  ) %>%
  filter(
    value
  ) %>%
  group_by(
    gauge_id,
    decade,
    name
  ) %>%
  mutate(
    day_group = c(0, cumsum(diff(time) != 1))
  ) %>%
  group_by(
    day_group,
    .add = TRUE
  ) %>%
  summarize(
    start_time = min(time),
    end_time = max(time),
    .groups = "drop_last"
  ) %>%
  summarize(
    num_activations = n(),
    .groups = "drop"
  ) %>%
  pivot_wider(
    values_from = num_activations,
    values_fill = 0
  ) %>%
  ggplot(
    aes(
      x = google_activation_2y_rp,
      y = google_activation_5y_rp
    )
  ) +
  geom_hline(
    yintercept = 2,
    color = hdx_hex('tomato-hdx')
  ) +
  geom_vline(
    xintercept = 5,
    color = hdx_hex('tomato-hdx')
  ) +
  geom_point() +
  facet_wrap(
    ~ decade,
    scales = "free",
    nrow = 1
  ) +
  geom_text(
    data = data.frame(decade = "1983 - 1992"),
    x = 17,
    y = 2.2,
    label = "Expected # of activations"
  ) +
  geom_text(
    data = data.frame(decade = "1983 - 1992"),
    x = 4.3,
    y = 4,
    label = "Expected # of activations",
    angle = 90
  ) +
  labs(
    y = "# of times gauge reached the 5 year RP",
    x = "# of times gauge reached the 2 year RP",
    title = "Number of times each gauge reaches the 2 year and 5 year RP, by decade"
  )
