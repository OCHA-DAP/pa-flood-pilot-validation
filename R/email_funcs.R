#' proj_map_viz_dataset
#'
#' @param dataset \code{character} name of data set to load 
#'  options are: 
#'    "wca" (West & Central Africa)
#'    "river"
#'    "adm1" (Nigeria Admin 1)
#'
#' @return spatial data frame containing layers downloaded from hdx
proj_map_viz_dataset <-  function(dataset){
  if(dataset=="wca"){
    ret <- pull_dataset("b20cd345-93fb-43bd-9c6e-7bc7d87b63eb") %>%
      get_resource(1) %>%
      read_resource()
  }
  if(dataset=="river"){
    ret <- pull_dataset("741c6f20-6956-420d-aae4-37015cdd1ad4") %>%
      get_resource(2) %>%
      # rhdx::get_resource_layers()
      read_resource(layer="NGA_rvrsl_1m_esri")
  }
  if(dataset=="adm1"){
    ret <- pull_dataset("81ac1d38-f603-4a98-804d-325c658599a3") %>% 
      get_resource(3) %>% 
      read_resource(layer = "nga_admbnda_adm1_osgof_20190417")
  }
  return(ret)
}

# let's put the funcs above together with all hdx layers w

#' hdx_map_viz_layers
#' @description
#' utility function to load hdx sourced spatial layers for mapping
#'
#' @return named list containing spatial data.frames to map
#' @examples \dontrun{
#' library(tidyverse)
#' library(rhdx)
#' hdx_map_viz_layers()
#' }
hdx_map_viz_layers <- function(){
  list(
    river= proj_map_viz_dataset(dataset="river"),
    west_central_africa = proj_map_viz_dataset(dataset="wca"),
    admin_1 = proj_map_viz_dataset(dataset="adm1")
  )
}



#' filter_basins
#'
#' @param df data.frame basin_id named "hybas_id"
#'
#' @return data.frame w/ specified hybas_ids removed
filter_basins <- function(df){
  df %>% 
    mutate(
      basin_name = case_when(
        hybas_id == 1040909900 ~ "Benue",
        hybas_id == 1040909890 ~ "Lower Niger",
        hybas_id == 1040022420 ~ "Niger Delta",
        hybas_id == 1040760290 ~ "Upper Niger",
        .default= NA
      ),.before=everything()
    ) %>% 
    filter(!is.na(basin_name))
}

#' filter_gauges
#' @desciption
#' convenience function to remove gauges that we are not using
#' @param df data.frame
#' @param gauge_id \code column name containing gauge_id (default = gauge_id)
#'
#' @return data.frame with gauges removed that we are not monitoring

filter_gauges <-  function(df, gauge_id=gauge_id){
  df %>% 
    filter(
      (!{{ gauge_id }} %in% c("hybas_1120794570",
                              "hybas_1120741070",
                              "hybas_1120946640",
                              "hybas_1120974450", 
                              "hybas_1120981190"))
    ) 
}


#' accum_pct_gauges_breached
#'
#' @param df 
#' @description 
#' Given all gauge data we need to look at each day in the forecast and per basin calculate the total
#' cumulative number % of gauges that have been breached. The complexity comes from needing to accumulate unique gauge
#' breachings over days. Created as modular function so that we can use in various places easily while keeping code simple and not needing
#' to track multiple data.frame objects as they evolve in a script.
#' @param date \code{character} name of variable containing date
#' @param lgl_var \code{character} name of variable containing logical value
#'
#' @return 
#' data.frame containing
#'  'basin_name'
#'  'date_predict'
#'  'cum_pct' : % of gauges that have been breached up to that point
#' @examples
accum_pct_gauges_breached <- function(df,
                                      date="date_predict", 
                                      lgl_var ="gte_2_rp"
){
  df %>%
    group_by(basin_name, !!sym(date)) %>%
    summarise(
      g_ids = list(unique(gauge_id)),
      g_ids_x = list(unique(gauge_id[!!sym(lgl_var)])),
      .groups = "drop_last"
    ) %>%
    mutate(
      g_ids_accum = accumulate(g_ids_x, `c`),
      g_ids_accum_unique = map(g_ids_accum, ~ unique(.x)),
      cum_pct = map_int(g_ids_accum_unique, length) / map_int(g_ids, length)
    ) %>%
    select(-starts_with("g_ids")) %>%
    ungroup()
}

read_gauge_googlesheets <- function(url = Sys.getenv("GFF_GAUGE_URL")) {
  sns <- sheet_names(ss = url)
  sn_filt <- sns[sns != "Sheet1"]
  sn_filt %>%
    map(
      \(sn){
        read_sheet(ss = url, sheet = sn) %>%
          clean_names()
      }
    ) %>%
    set_names(nm = sn_filt)
}




#' basin_pal
#' @description
#' Helper function to return basin-named palette. Useful to have as function since we can change values here 
#' and have them change across multiple plots and text elements.
#' 
#' @return named vector. Names are the basin names ('Benue', 'Lower Niger', 'Niger Delta', 'Upper Niger')
#' @examples \dontrunP
#' library(gghdx)
#' basin_pal()

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


#' gen_plot_title
#'
#' @param df 
#' @param date \code{character} column name containing predicted date
#' @param threshold \code{numeric} threshold
#' @param basin_palette named vector containing per basin 
#'
#' @return
#' @export
#'
#' @examples \dontrun{
#' gen_plot_title(gauge_nrt_long)
#' }
gen_plot_title <- function(df,
                           date = "date_predict",
                           threshold = 0.5,
                           basin_palette = basin_pal()) {
  date_forecast_gen <- min(df[[date]])
  
  basin_df_cum_pct <- accum_pct_gauges_breached(df = df, lgl_var = "gte_2_rp")
  
  basin_first_breach <- basin_df_cum_pct %>%
    group_by(basin_name) %>%
    filter(cum_pct >= threshold) %>%
    arrange(!!sym(date)) %>% 
    slice(1) %>%
    ungroup()
  
  
  if (nrow(basin_first_breach) == 0) {
    ret <- glue(
      "{date_forecast_gen}: No Warning issued"
    )
  } else {
    basin_vec_syled <- basin_first_breach %>%
      mutate(
        col = basin_palette[basin_name],
        basin_name_styled = glue("<span style= 'color: {col}'> {basin_name} </span>")
      ) %>%
      pull(basin_name_styled)
    
    # more than 1 basin we need plural
    if (length(basin_vec_syled) > 1) {
      ret <- glue(
        "{date_forecast_gen}: Flood warning issued for the {glue_collapse(basin_vec_syled,sep=',',last='&')} Basins in Nigeria"
      )
    }
    # 1 basin singular
    if (length(basin_vec_syled) == 1) {
      ret <- glue(
        "{date_forecast_gen}: Flood warning issued for the {basin_vec_syled} Basin in Nigeria"
      )
    }
  }
  return(ret)
}


plot_average_discharge_normalized <-  function(df,
                                               date="date_predict", 
                                               threshold=0.8,
                                               basin_palette=basin_pal()
){
  # average discharge predicted per day per basin.
  df_p <- df %>% 
    group_by(basin_name,!!sym(date)) %>% 
    summarise(
      Q_pct_rp2= mean(Q_pct_rp2,na.rm=T),
      .groups="drop"
    ) 
  
  # plot inputs
  plot_title <- gen_plot_title(df = df,
                               date = date,
                               threshold = threshold,
                               basin_palette = basin_palette)
  # for setting plot y-axis limits
  Q_pct_max <- max(df_p$Q_pct_rp2)
  ymax_lim <- ifelse(Q_pct_max<1,1,round(Q_pct_max+0.2,1))
  
  # plot
  df_p %>% 
    ggplot(
      aes(x= date_predict,
          y= Q_pct_rp2,
          color= basin_name,
          group=basin_name
      )
    )+
    geom_smooth(se=FALSE)+
    scale_color_manual(values = basin_palette)+
    scale_x_date(breaks = "1 day", date_labels = "%m-%d") +
    scale_y_continuous(
      breaks = seq(0, ymax_lim, .2),
      limits = c(0,ymax_lim),
      labels = scales::percent,
    ) +
    labs(
      title = plot_title,
      y = "Average gauge discharge (% 2 Year RP)",
    ) +
    theme(
      axis.text.x = element_text(angle = 90),
      axis.title.x = element_blank(),
      legend.title = element_blank(),
      plot.subtitle = element_markdown(),
      plot.title = element_markdown(size = 15)
    )
  
}



# Map ---------------------------------------------------------------------


#' nga_base_map
#' @description
#' convenience function to style the base map aesthetics - functinonalized just to simplify pipeline code
#' @param west_africa_adm0 
#' @param country_fill \code{character} background color of nga
#' @param surrounding_fill \code{character} background color for surrounding 
#'
#' @return map made with `{tmap}`

nga_base_map <- function(
    west_africa_adm0,
    country_fill = "white",
    surrounding_fill = "lightgrey") {
  
  # countries of interest
  coi <- west_africa_adm0 %>%
    mutate(
      aoi = ifelse(admin0Pcod == "NG", "aoi", "not_aoi")
    )
  
  # split
  coi_l <- split(coi, coi$aoi)
  
  # map
  tm_shape(coi, bbox = coi_l$aoi) +
    tm_polygons(
      col = "aoi",
      palette = c(country_fill, surrounding_fill),
      legend.show = F
    )+
    tm_shape(coi_l$not_aoi)+
    tm_text(text= "admin0Name",col = "white")+
    tm_shape(coi_l$aoi)+
    tm_borders(col = "#414141",lwd = 5,alpha=1)+
    tm_shape(coi_l$aoi,legend.show=F)+
    tm_borders(
      col="white",
      # lty = 3, # linetype long dash
      lwd=1,
      alpha=0.7)
}
