#' Title
#'
#' @param flood_extent
#' @param bbox
#'
#' @return
#' @export
#'
#' @examples
main_flood_map <- function(
    flood_extent = fext_big,
    bbox = "flood_extent") {
  ret <- list()
  flood_extent <- st_make_valid(flood_extent)
  r_crop <- terra::crop(x = wmask, flood_extent)
  r_crop[r_crop %in% c(3)] <- 1
  r_crop[r_crop != 3] <- NA
  if (bbox == "flood_extent") {
    ext_bbox <- st_bbox(flood_extent) %>%
      st_as_sfc()
  }
  if (bbox == "impacted_lga") {
    ext_bbox <- st_join(
      nga_adm$adm2,
      flood_extent %>%
        mutate(
          fext_id = row_number()
        )
    ) %>%
      filter(!is.na(fext_id)) %>%
      st_bbox() %>%
      st_as_sfc()
  }

  ret$bbox <- ext_bbox
  ret$map <- tm_shape(nga_adm$adm0, bbox = ext_bbox) +
    tm_polygons(col = "white", border.col = "black") +
    tm_shape(nga_adm$adm1) +
    tm_borders(col = "darkgrey", lwd = 2, alpha = 0.7) +
    tm_shape(nga_adm$adm2) +
    tm_borders(col = "lightgrey", alpha = 0.3) +
    # tm_text(text = "ADM2_EN",
    #         col = "black",
    #         shadow=T,
    #         auto.placement = T)+
    tm_shape(nga_adm$adm0) +
    tm_borders(col = "black") +
    tm_shape(nga_riv) +
    tm_lines(col = "lightblue", lwd = 3) +
    tm_shape(flood_extent) +
    tm_polygons(
      col = "darkblue",
      border.col = NULL,
      alpha = 0.75
    ) +
    tm_shape(r_crop) +
    tm_raster(
      style = "cat",
      # palette=c("aquamarine"),
      alpha = 0.7,
      legend.show = F
    ) +
    tm_add_legend(
      col = c("aquamarine", "darkblue"),
      labels = c("Water Mask", "Flood Water"),
      size = 100
    ) +
    tm_scale_bar(position = c("right", "bottom")) +
    tmap_options(check.and.fix = TRUE) +
    tm_layout(
      outer.margins = c(0, 0, 0, 0),
      inner.margins = c(0, 0, 0, 0)
    )
  return(ret)
}


# make admin base for inset
inset_admin_base <- function(size_scaler = 0.3) {
  region <- rnaturalearth::ne_countries(
    country = c("Nigeria", "Niger", "Benin", "Cameroon", "Chad"),
    scale = 10
  )
  region <- region %>%
    st_as_sf() %>%
    mutate(
      aoi = ifelse(adm0_a3 == "NGA", "aoi", "not_aoi")
    )
  region_l <- split(region, region$aoi)
  tm_shape(region, bbox = region_l$aoi) +
    tm_polygons(
      col = "aoi",
      palette = c("white", "lightgrey"),
      legend.show = F
    ) +
    # admin 2
    tm_shape(nga_adm$adm2) +
    tm_borders(col = "lightgrey", lwd = 1 * size_scaler, alpha = 0.4) +
    # admin 1
    tm_shape(nga_adm$adm1) +
    tm_borders(col = "#565656", lwd = 2 * size_scaler, alpha = 0.7) +

    # nga boundary
    tm_shape(region_l$aoi, legend.show = F) +
    tm_borders(col = "#414141", lwd = 5 * size_scaler, alpha = 0.7) +
    tm_shape(region_l$aoi, legend.show = F) +
    tm_borders(
      col = "lightgrey",
      lty = 3, # linetype long dash
      lwd = 2 * size_scaler
    ) +
    # country (not nga labels)
    tm_shape(region_l$not_aoi) +
    tm_text(
      "admin",
      col = "white",
      auto.placement = F,
      remove.overlap = T
    ) +
    tm_layout(bg.color = "lightblue")
}

#' Title
#'
#' @param flood_extent
#' @param bbox \code{character} use flood extent ("flood" default) or impacted LGA ("impacted_lga) to define bbox
#' @param tmap_mode
#' @param size_scaler
#'
#' @return
#' @export
#'
#' @examples
#' inset_map(
#'   flood_extent = fext_big,
#'   bbox = "flood",
#'   tmap_mode = "plot"
#' )
inset_map <- function(flood_extent,
                      bbox,
                      tmap_mode, size_scaler = 0.3) {
  current.mode <- tmap_mode(tmap_mode)
  if (bbox == "flood") {
    ext_bbox <- st_bbox(flood_extent) %>%
      st_as_sfc()
  }
  if (bbox == "impacted_lga") {
    ext_bbox <- st_join(
      nga_adm$adm2,
      flood_extent %>%
        mutate(
          fext_id = row_number()
        )
    ) %>%
      filter(!is.na(fext_id)) %>%
      st_bbox() %>%
      st_as_sfc()
  }

  admin_base <- inset_admin_base()
  admin_base +
    tm_shape(nga_riv) +
    tm_lines(col = "lightblue", lwd = 3 * size_scaler) +
    tm_shape(ext_bbox) +
    tm_polygons(
      border.col = "red",
      col = "red",
      alpha = 0.2
    ) +
    tm_layout(
      # inner.margins = c(0.04,0.04,0.04,0.04),
      outer.margins = c(0, 0, 0, 0), legend.show = F
    )
}

# tar_load(wca_adm0)
# tar_load(nga_adm)
# nga_base_map()

nga_base_map <- function(
    west_africa_adm0 = wca_adm0,
    country_fill = "white",
    surrounding_fill = "lightgrey") {
  # countries of interest
  coi <- west_africa_adm0 %>%
    mutate(
      aoi = ifelse(admin0Pcod == "NG", "aoi", "not_aoi")
    )
  coi_l <- split(coi, coi$aoi)


  tm_shape(coi, bbox = coi_l$aoi) +
    tm_polygons(
      col = "aoi",
      palette = c(country_fill, surrounding_fill),
      legend.show = F
    )
}
