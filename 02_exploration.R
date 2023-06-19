library(tidyverse)
library(sf)
library(gghdx)
library(rhdx)

gghdx()

output_dir <- file.path(
  Sys.getenv("GFH_DATA_DIR"),
  "outputs"
)

###################################
#### READ IN THE WRANGLED DATA ####
###################################

# shapefiles

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

sf_google <- read_sf(
  file.path(
    output_dir,
    "google_centroids.gpkg"
  )
)

nga_rivers <- pull_dataset("741c6f20-6956-420d-aae4-37015cdd1ad4") %>%
  get_resource(2) %>%
  read_resource()


# data frames

df_google_fs <- read_csv(
  file.path(
    output_dir,
    "google_hybas_fs_mean_30k.csv"
  )
)



df_google_preds <- read_csv(
  file.path(
    output_dir,
    "google_predictions.csv"
  )
)

# validation by using floodscan
# look at next day Floodscan relative to the nowcast
df_google_validation <- df_google_preds %>%
  mutate(
    time = time + lubridate::days(1)
  ) %>%
  inner_join(
    df_google_fs %>% select(-ID),
    by = c("hybas_station" = "HYBAS_ID", "time" = "time")
  ) %>%
  group_by(
    hybas_station
  ) %>%
  mutate(
    across(
      .cols = c(prediction, sfed),
      .fns = \(x) (x - mean(x)) / sd(x)
    )
  )

########################
#### GOOGLE MAPPING ####
########################

ggplot() +
  geom_sf(
    data = nga_adm
  ) +
  geom_sf(
    data = nga_rivers
  ) +
  geom_sf(
    data = sf_google,
    fill = hdx_hex("mint-hdx"),
    color = "white",
    size = 5,
    shape = 21
  ) +
  geom_sf_text(
    data = sf_google,
    mapping = aes(
      label = HYBAS_ID
    ),
    check_overlap = TRUE,
    nudge_x = .75
  ) +
  coord_sf(
    datum = NA
  ) +
  labs(
    x = "",
    y = ""
  )

#########################
#### GOOGLE PLOTTING ####
#########################

plot_station <- function(station) {
  df_google_preds %>%
    inner_join(
      df_google_fs %>% select(-ID),
      by = c("hybas_station" = "HYBAS_ID", "time" = "time")
    ) %>%
    group_by(
      hybas_station
    ) %>%
    mutate(
      across(
        .cols = c(prediction, sfed),
        .fns = \(x) (x - mean(x)) / sd(x)
      )
    ) %>%
    filter(
      hybas_station == station
    ) %>%
    pivot_longer(
      c(prediction, sfed)
    ) %>%
    ggplot(
      aes(
        x = time,
        y = value,
        group = name,
        color = name
      )
    ) +
    geom_line() +
    labs(
      x = "",
      y = "Z-score",
      color = "",
      title = "Google nowcasting vs Floodscan data",
      subtitle = paste0("Hybas station: ", station)
    ) +
    scale_color_manual(
      values = unname(hdx_hex(c("mint-hdx", "sapphire-hdx"))),
      labels = c("Google nowcast", "Flooded fraction")
    )
  
}

# direct plotting specific stations
plot_station(1120794570) # random point not attached to rivers
plot_station(1120898220) # confluence point
plot_station(1122033860) # point just above the delta

# general validation plot

df_google_validation %>%
  ggplot(
    aes(
      x = prediction,
      y = sfed
    )
  ) +
  geom_point(
    alpha = 0.05
  ) +
  scale_y_continuous_hdx() +
  labs(
    y = "Flooded fraction (z-score)",
    x = "Google nowcast (z-score)",
    title = "Google nowcast vs flooded fraction"
  )

# boxplot 

df_google_validation %>%
  ungroup() %>%
  mutate(
    bins = cut(
      x = prediction,
      breaks = seq(-2, 11),
      include.lowest = TRUE,
      labels = paste0(
        seq(-2, 10),
        " to ",
        seq(-1, 11)
      )
    )
  ) %>%
  ggplot(
    aes(
      x = bins,
      y = sfed
    )
  ) +
  geom_boxplot(
    alpha = 0.05
  ) +
  labs(
    y = "Flooded fraction (z-score)",
    x = "Google nowcast (z-score)",
    title = "Google nowcast vs flooded fraction"
  )

# look at performance across time as measured by distance between the 2 z scores
df_google_validation %>%
  mutate(
    error = prediction - sfed,
    square_error = error^2,
    missed_event = prediction < 1 & sfed > 1,
    rmse_100 = zoo::rollapplyr(
      data = square_error,
      width = 100,
      FUN = mean,
      fill = NA
    )
  ) %>%
  ggplot(
    aes(
      x = time,
      y = rmse_100,
      group = hybas_station
    )
  ) +
  geom_line()
