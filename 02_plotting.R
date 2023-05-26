source("01_wrangling.R")

library(tidyterra)
library(gghdx)
gghdx()

##################################################
#### GENERAL VIEW OF COVERAGE AND POPULATIONS ####
##################################################

# current coverage

p_coverage <- ggplot() +
  geom_sf(
    data = nga_rivers
  ) +
  geom_sf(
    data = nga_adm,
    fill = NA
  ) +
  geom_spatraster(
    data = nga_pop
  ) +
  geom_sf(
    data = gfh_now,
    color = "black",
    fill = NA
  ) +
  scale_fill_gradient(
    low = "lightgrey",
    high = hdx_hex("mint-dark"),
    na.value = NA,
    trans = "log10",
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::comma_format()
  ) +
  coord_sf(
    datum = NA
  ) +
  theme(
    legend.key.size = unit(1, "cm"),
    plot.title = element_text(size = 24),
    plot.subtitle = element_text(size = 18)
  ) +
  labs(
    title = "Google Flood Hub coverage relative to population",
    subtitle = "Current coverage",
    fill = "Population"
  )
  
p_coverage

# future coverage

p_coverage_future <- p_coverage +
  geom_sf(
    data = gfh_future,
    color = "black",
    fill = NA
  ) +
  labs(
    subtitle = "Future coverage"
  ) +
  coord_sf(
    datum = NA
  )

######################
#### RECENT FLOOD ####
######################

p_floods <- ggplot() +
  geom_sf(
    data = nga_adm,
    fill = NA
  ) +
  geom_sf(
    data = nga_extents,
    color = NA,
    fill = hdx_hex("tomato-hdx")
  ) +
  geom_sf(
    data = nga_rivers
  ) +
  coord_sf(
    datum = NA
  ) +
  theme(
    legend.key.size = unit(1, "cm"),
    plot.title = element_text(size = 24),
    plot.subtitle = element_text(size = 18)
  ) +
  labs(
    title = "2022 flood extents in Nigeria",
    subtitle = "Based on UNOSAT analysis"
  )

p_floods +
  geom_sf(
    data = gfh_coverage,
    color = "black",
    fill = hdx_hex("mint-light")
  ) +
  labs(
    title = "Google Flood Forecasting coverage of floods",
    subtitle = "Future coverage"
  ) +
  coord_sf(
    datum = NA
  )


##################################################
#### LOOK AT POPULATIONS WITHIN FLOODED AREAS ####
##################################################

flood_pop <- crop(nga_pop, nga_extents, mask = TRUE) # total population in flooded areas
gfh_flood_pop  <- crop(flood_pop, gfh_coverage, mask = TRUE) # population in flooded areas covered by GFH
flood_niger_pop <- crop(flood_pop, nga_niger, mask = TRUE) # population flooded within 15km of Niger
gfh_flood_niger_pop <- crop(gfh_flood_pop, nga_niger, mask = TRUE) # GFH population in flooded areas around Niger

pops <- map(
  .x = list(flood_pop, gfh_flood_pop, flood_niger_pop, gfh_flood_niger_pop),
  .f = \(x) global(x, "sum", na.rm = TRUE)
)

df_pop <- data.frame(
  coverage = c("all", "gfh", "all", "gfh"),
  area = c("All flooded areas", "All flooded areas", "Within 15km from the Niger river", "Within 15km from the Niger river"),
  pop = sapply(pops, \(x) x$sum)
)

df_pop %>%
  ggplot() +
  geom_bar(
    aes(
      x = pop,
      y = fct_rev(coverage)
    ),
    stat = "identity"
  ) +
  facet_wrap(
    ~ area,
    ncol = 1,
    scales = "free_x"
  ) +
  scale_y_discrete(
    labels = c("Areas covered by GFH", "All flooded areas")
  ) +
  scale_x_continuous(
    breaks = scales::pretty_breaks(),
    labels = scales::label_comma(),
  ) +
  theme(
    plot.margin = margin(10, 15, 10, 10)
  ) +
  labs(
    x = "Population",
    y = "",
    title = "Google Flood coverage of populations in flooded areas"
  )

###########################################
#### MAP THE COVERED FLOOD POPULATIONS ####
###########################################


# get in points format for plotting
# to enable deletion of NA values
flood_pop_pts <- as.points(flood_pop)
gfh_flood_pop_pts <- as.points(gfh_flood_pop)
flood_niger_pop_pts <- as.points(flood_niger_pop)
gfh_flood_niger_pop_pts <- as.points(gfh_flood_niger_pop)

ggplot() +
  geom_sf(
    data = nga_adm,
    fill = "white",
    color = hdx_hex("gray-dark")
  ) +
  geom_sf(
    data = nga_niger,
    color = NA,
    fill = hdx_hex("gray-light")
  ) +
  geom_sf(
    data = flood_pop_pts,
    mapping = aes(
      size = nga_general_2020
    ),
    color = hdx_hex("tomato-hdx")
  ) +
  geom_sf(
    data = gfh_flood_pop_pts,
    mapping = aes(
      size = nga_general_2020
    ),
    color = hdx_hex("mint-hdx")
  ) +
  geom_sf(
    data = nga_rivers,
    color = hdx_hex("sapphire-hdx")
  ) +
  scale_size(
    range = c(0.01, 0.3),
    guide = "none"
  ) +
  coord_sf(
    datum = NA
  ) +
  labs(
    title = "Flooded populations covered by Google Flood Forecasting"
  )


ggplot() +
  geom_sf(
    data = nga_adm,
    fill = "white",
    color = hdx_hex("gray-dark")
  ) +
  geom_sf(
    data = nga_niger,
    color = NA,
    fill = hdx_hex("gray-light")
  ) +
  geom_sf(
    data = flood_niger_pop_pts,
    mapping = aes(
      size = nga_general_2020
    ),
    color = hdx_hex("tomato-hdx")
  ) +
  geom_sf(
    data = gfh_flood_niger_pop_pts,
    mapping = aes(
      size = nga_general_2020
    ),
    color = hdx_hex("mint-hdx")
  ) +
  geom_sf(
    data = nga_rivers,
    color = hdx_hex("sapphire-hdx")
  ) +
  scale_size(
    range = c(0.01, 0.3),
    guide = "none"
  ) +
  coord_sf(
    datum = NA
  ) +
  labs(
    title = "Flooded populations covered by Google Flood Forecasting",
    subtitle = "Within 15km of the Niger river"
  )

############################
#### FLOODSCAN COVERAGE ####
############################
  

# looking more specifically at the extents
# get rasters in exact format necessary
extents_rast <- stars::st_rasterize(nga_extents) %>%
  rast()
extents_rast <- extents_rast$Water_Class_lyr.1

fs_rast_rs <- resample(fs_rast, extents_rast)
fs_mask_rs <- resample(fs_mask, extents_rast, "min")

# mask out wherever floodscan has flooding
fs_rast_mask <- ifel(fs_rast_rs > 0.01, fs_rast_rs, NA)

fs_mask_rs_masked <- ifel(mask(fs_mask_rs, extents_rast) > 0, fs_mask_rs, NA)
fs_mask_pts <- as.points(fs_mask_rs_masked)

extents_pts <- as.points(extents_rast)
fs_rast_pts <- as.points(fs_rast_mask)

# show the plot now

p_fs <- ggplot() +
  geom_sf(
    data = nga_adm,
    fill = NA
  ) +
  geom_sf(
    data = fs_rast_pts,
    mapping = aes(
      color = lyr.1
    ),
    size = 0.1
  ) +
  scale_color_gradient(
    na.value = "white",
    low = "white",
    high = hdx_hex("sapphire-hdx"),
    labels = scales::label_percent()
  ) +
  coord_sf(
    datum = NA
  ) +
  labs(
    title = "Floodscan flood extents data",
    subtitle = "Maximum extent observed October 1 - 25, 2022",
    color = "% of land flooded"
  )

p_fs +
  geom_sf(
    data = extents_pts,
    color = hdx_hex("tomato-hdx"),
    size = 0.1
  ) +
  geom_sf(
    data = fs_rast_pts,
    mapping = aes(
      color = lyr.1,
    ),
    size = 0.1
  ) +
  coord_sf(
    datum = NA
  ) +
  labs(
    subtitle = "UNOSAT flood extents underlaid in red"
  )

p_fs +
  geom_sf(
    data = extents_pts,
    color = hdx_hex("tomato-hdx"),
    size = 0.1
  ) +
  geom_sf(
    data = fs_rast_pts,
    mapping = aes(
      color = lyr.1,
      alpha = lyr.1
    ),
    size = 0.1
  ) +
  coord_sf(
    datum = NA
  ) +
  scale_alpha(
    guide = "none"
  ) +
  labs(
    subtitle = "UNOSAT flood extents underlaid in red, Floodscan transparency based on flood extents"
  )
