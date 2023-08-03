# discharge class
# Q_class <- Q_pred %>%
#   left_join(gauge_flagged, by=c("hybas_id","gauge_id"))
#
#
#
# library(ggtext)
# library(glue)
# library(gghdx)
# theme_hdx()
# gghdx()
# install.packages("ggtext")


#' Title
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples \dontrun{
#' pct_crossed(Q_class)
#' }
#'
plot_gauge_cum_pct <- function(df = Q_class,
                               threshold = 0.5,
                               date = "date_predict",
                               lgl_var,
                               basin_palette = basin_pal()) {
  df_p <- cumulative_gauge_pct(df = df, lgl_var = lgl_var)

  df_thresh_breach_pts <- df_p %>%
    group_by(basin_name) %>%
    filter(cum_pct >= threshold) %>%
    slice(1) %>%
    ungroup()

  plot_subtitle <- gen_subtitle(
    df = df,
    basin_palette = basin_palette,
    date = date,
    threshold = threshold
  )
  # thresh_x_data <- get_thresh_crossing(df)
  df_p %>%
    ggplot(aes(
      x = date_predict,
      y = cum_pct,
      color = basin_name
    )) +
    geom_vline(
      data = df_thresh_breach_pts,
      aes(xintercept = date_predict),
      color = "tomato",
      alpha = 0.2,
      lwd = 7
    ) +
    geom_line(alpha = 0.5) +
    geom_point() +
    scale_color_manual(values = basin_palette) +
    scale_y_continuous(labels = scales::percent) +
    scale_x_date(breaks = "1 day", date_labels = "%m-%d") +
    labs(subtitle = plot_subtitle) +
    # labs(title = "% Gauges predicted to cross the discharge threshold")+
    geom_hline(
      yintercept = threshold,
      color = "tomato",
      lwd = 0.7,
      alpha = 0.5,
      linetype = "dashed"
    ) +
    theme(
      legend.position = "none",
      axis.text.x = element_blank(),
      # axis.title = element_blank(),
      legend.title = element_blank(),
      plot.subtitle = element_markdown()
    )
}



cumulative_gauge_pct <- function(df, lgl_var = "gte_2_rp") {
  df %>%
    group_by(basin_name, date_predict) %>%
    summarise(
      g_ids = list(unique(gauge_id)),
      g_ids_x = list(unique(gauge_id[!!sym(lgl_var)])), .groups = "drop_last"
    ) %>%
    mutate(
      g_ids_accum = accumulate(g_ids_x, `c`),
      g_ids_accum_unique = map(g_ids_accum, ~ unique(.x)),
      cum_pct = map_int(g_ids_accum_unique, length) / map_int(g_ids, length)
    ) %>%
    select(-starts_with("g_ids")) %>%
    ungroup()
}


get_thresh_crossing <- function(df) {
  forecast_gen <- min(df$date_predict)
  df %>%
    group_split(basin_name) %>%
    map_dfr(\(dft){
      n_gauge <- length(unique(dft$gauge_id))
      dft %>%
        filter(
          gte_2_rp
        ) %>%
        select(date_predict, basin_name, gauge_id, value, x2_years_return_period) %>%
        group_by(basin_name, date_predict) %>%
        summarise(
          gte_dist_gauge = n_distinct(gauge_id),
          .groups = "drop"
        ) %>%
        mutate(
          date_forecast_made = forecast_gen,
          n_gauge = n_gauge,
          pct_gte = gte_dist_gauge / n_gauge
        )
    })
}


#' Title
#'
#' @return
#' @export
#'
#' @examples
basin_pal <- function() {
  c(
    "tomato-dark",
    "gray-dark",
    "sapphire-hdx",
    "mint-dark"
  ) %>%
    map_chr(
      ~ hdx_colors()[.x]
    ) %>%
    set_names(
      "Benue",
      "Lower Niger",
      "Niger Delta",
      "Upper Niger"
    )
}


gen_subtitle <- function(df,
                         date = "date_predict",
                         threshold = 0.5,
                         basin_palette = basin_pal()) {
  date_forecast_gen <- min(df[[date]])

  df_cum_pct <- cumulative_gauge_pct(df = df, lgl_var = "gte_2_rp")
  df_thresh_breach_pts <- df_cum_pct %>%
    group_by(basin_name) %>%
    filter(cum_pct >= threshold) %>%
    slice(1) %>%
    ungroup()


  if (length(df_thresh_breach_pts) == 0) {
    res <- glue(
      "Status {date_forecast_gen}: No Warning issued"
    )
  } else {
    date_forecast_gen <- unique(df_thresh_breach_pts[[date]])
    basin_vec_syled <- df_thresh_breach_pts %>%
      mutate(
        col = basin_palette[basin_name],
        basin_name_styled = glue("<span style= 'color: {col}'> {basin_name} </span>")
        # uly: Warning issued for the <span style='color:{basin_pal[df_gte_pct_thresh$basin_name]}'>
      ) %>%
      pull(basin_name_styled)

    if (length(basin_vec_syled) > 1) {
      res <- glue(
        "Status {date_forecast_gen}: Warning issued for the {glue_collapse(basin_vec_syled,sep=',',last='&')} Basins"
      )
    }
    if (length(basin_vec_syled) == 1) {
      res <- glue(
        "Status {date_forecast_gen}: Warning issued for the {basin_vec_syled} Basin."
      )
    }
  }
  return(res)
}

gen_titles <- function(df,
                       date = "date_predict",
                       threshold = 0.5,
                       basin_palette = basin_pal()) {
  date_forecast_gen <- min(df[[date]])

  df_cum_pct <- cumulative_gauge_pct(df = df, lgl_var = "gte_2_rp")
  df_thresh_breach_pts <- df_cum_pct %>%
    group_by(basin_name) %>%
    filter(cum_pct >= threshold) %>%
    slice(1) %>%
    ungroup()

  res <- list()
  if (length(df_thresh_breach_pts) == 0) {
    res$title <- glue(
      "Status {date_forecast_gen}: No Warning issued"
    )
  } else {
    date_forecast_gen <- unique(df_thresh_breach_pts[[date]])
    basin_vec_syled <- df_thresh_breach_pts %>%
      mutate(
        col = basin_palette[basin_name],
        basin_name_styled = glue("<span style= 'color: {col}'> {basin_name} </span>")
        # uly: Warning issued for the <span style='color:{basin_pal[df_gte_pct_thresh$basin_name]}'>
      ) %>%
      pull(basin_name_styled)

    if (length(basin_vec_syled) > 1) {
      res$title <- glue(
        "Flood warning issued for the {glue_collapse(basin_vec_syled,sep=',',last='&')} Basins in Nigeria"
      )
    }
    if (length(basin_vec_syled) == 1) {
      res$title <- glue(
        "Flood warning: Warning issued for the {basin_vec_syled} Basin in Nigeria"
      )
    }
  }
  res$subtitle <- glue("Status: {date_forecast_gen}")
  return(res)
}


# combined plots ----------------------------------------------------------


plot_gauge_discharge <- function(
    df = Q_class,
    date = "date_predict",
    facet_var = NULL,
    basin_palette = c(
      `Benue` = "#66C2A5",
      `Lower Niger` = "#FC8D62",
      `Niger Delta` = "#8DA0CB",
      `Upper Niger` = "#E78AC3"
    )) {
  if (is.null(facet_var)) {
    p_res <- plot_gauge_discharge_normalized(
      df = df,
      date = date,
      basin_palette = basin_palette
    )
  }
  if (!is.null(facet_var)) {
    p_res <- plot_gauge_discharge_raw(
      df = df,
      date = date,
      facet_var = facet_var
    )
  }
  return(p_res)
}




# Normalized Plot ---------------------------------------------------------


plot_gauge_discharge_normalized <- function(df = Q_class,
                                            date = "date_predict",
                                            threshold = 0.5,
                                            basin_palette = c(
                                              `Benue` = "#66C2A5",
                                              `Lower Niger` = "#FC8D62",
                                              `Niger Delta` = "#8DA0CB",
                                              `Upper Niger` = "#E78AC3"
                                            ),
                                            annotation = F) {
  # bind together pct crossed w total gauge data
  gauge_crossing_id <- df %>%
    filter(gte_2_rp) %>%
    pull(gauge_id) %>%
    unique()
  df_thresh_breach_pts <- cumulative_gauge_pct(df = df, lgl_var = "gte_2_rp") %>%
    group_by(basin_name) %>%
    filter(cum_pct >= threshold) %>%
    slice(1) %>%
    ungroup()




  df <- df %>%
    mutate(alpha_val = ifelse(gauge_id %in% gauge_crossing_id, 1, .25))

  p_base <- df %>%
    ggplot(aes(
      x = !!sym(date),
      y = Q_norm_rp2,
      color = basin_name,
      group = gauge_id,
      alpha = alpha_val
    )) +
    geom_vline(
      data = df_thresh_breach_pts,
      aes(xintercept = date_predict),
      color = "tomato",
      alpha = 0.2,
      lwd = 7
    ) +
    geom_line() +
    scale_x_date(breaks = "1 day", date_labels = "%m-%d") +
    scale_y_continuous(
      breaks = seq(0, 4, .2),
      labels = scales::percent
    ) +
    scale_color_manual(values = basin_palette) +
    guides(alpha = "none") +
    labs(
      y = "Gauge Discharge (% 2 Year RP)",
      title = "NGA River Discharge: 7 Day Forecast"
    ) +
    theme(
      axis.text.x = element_text(angle = 90),
      axis.title.x = element_blank(),
      legend.title = element_blank(),
      plot.subtitle = element_markdown()
    )

  if (length(unique(df$gte_2_rp_any)) > 1) {
    # alpha_val = c(0.25,1)
    df_gte_pct_thresh <- get_thresh_crossing(df)
    if (nrow(df_gte_pct_thresh) > 0) {
      trig_alert <- T
      txt_subtitle <- gen_subtitle(
        df = df,
        date = date,
        threshold = threshold,
        basin_palette = basin_palette
      )


      p_res <- p_base +
        geom_hline(aes(yintercept = 1), color = "tomato", alpha = 0.5, lwd = 0.7, linetype = "dashed") +
        labs(
          subtitle = txt_subtitle
        )
      if (annotation) {
        txt_annotate <- glue("Over 50 % of gauges in the {df_gte_pct_thresh$basin_name} Basin crossed the warning threshold by {df_gte_pct_thresh$date_predict}")
        # txt_annotate <- ("'Large fish' >= 45 ~ 'cm'")
        x_end <- df_gte_pct_thresh$date_predict
        y_end <- 1
        x_start <- min(df$date_predict)
        y_start <- max(df$Q_norm_rp2)

        p_res <- p_res +
          geom_vline(aes(xintercept = df_gte_pct_thresh$date_predict),
            linetype = "dashed",
            color = hdx_colors()["tomato-hdx"],
            lwd = 0.5
          ) +
          geom_curve(
            aes(
              x = x_start + 1, y = y_start * .95,
              xend = x_end, yend = y_end
            ),
            arrow = grid::arrow(length = unit(0.5, "lines")),
            curvature = 0.3,
            angle = 100, ncp = 10, lwd = 0.5,
            color = hdx_colors()["gray-dark"]
          ) +
          annotate(
            geom = "text",
            label = txt_annotate,
            x = x_start,
            y = y_start,
            hjust = 0,
            # parse=TRUE
          )
      }
      return(p_res)
    }
  }
  if (length(unique(df$gte_2_rp_any)) == 1) {
    alpha_val <- c(1)
    p_res <- p_base +
      scale_alpha_manual(values = alpha_val)
  }
  return(p_res)
}

plot_gauge_discharge_raw <- function(df = Q_class,
                                     date = "date_predict",
                                     facet_var = "basin_name") {
  df %>%
    ggplot(aes(
      x = !!sym(date), y = value,
      group = gauge_id,
      color = rp_class
    )) +
    geom_line(alpha = 0.6) +
    scale_x_date(breaks = "1 day", date_labels = "%m-%d") +
    scale_color_manual(
      drop = FALSE,
      values = c(
        `No threshold exceeded` = "#43a2ca",
        `>= 2 year RP` = "#f03b20",
        `>= 5 year RP` = "#bd0026",
        `>= 20 year RP` = "#7a0177"
      )
    ) +
    facet_wrap(as.formula(paste("~", facet_var)), ncol = 1, scales = "free_y") +
    labs(
      y = "Discharge (m3/s)",
      title = "Predicted discharge at gauge"
    ) +
    theme(
      axis.text.x = element_text(angle = 90),
      axis.title.x = element_blank(),
      legend.title = element_blank()
    )
}
