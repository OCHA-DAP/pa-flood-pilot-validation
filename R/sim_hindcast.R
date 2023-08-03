#' sim_hindcast
#' @description
#' given a historical record of strata dates, values, simulate a hindcast by strata
#' @param df data.frame
#' @param strata \code{character} name of grouping var
#' @param date \code{character} name of date var
#' @param val \code{character} name of var containing numeric value
#' @param leadtime  \code{integer} number of day leadtime to use to simulate hindcast
#'
#' @return
#' @export
#'
#' @examples
sim_hindcast <- function(df = gauge_historical %>%
                           filter(year(date) > 2020),
                         strata = c("gauge_id"),
                         date = "date",
                         val = "prediction",
                         leadtime = 7) {
  dt_use <- seq(min(df[[date]]), max(df[[date]]) - leadtime, 1)
  df %>%
    filter(!!sym(date) %in% dt_use) %>%
    pull(date) %>%
    unique() %>%
    map_dfr(
      \(dt){
        dt <- as_date(dt)
        print(dt)
        dt_lt <- dt + leadtime
        dt_seq <- seq(dt, dt_lt, 1)

        # forecast values temp
        fvt <- df %>%
          group_by(!!sym(strata)) %>%
          filter(
            !!sym(date) %in%
              dt_seq
          ) %>%
          arrange(!!sym(strata)) %>%
          mutate(
            date_forecast_made = dt,
            lt = 0:leadtime,
            date_predict = date_forecast_made + lt, .before = everything(),
            predict_sim = sample(prediction, 8, replace = T)
          ) %>%
          ungroup() %>%
          select(-!!sym(date))
        return(fvt)
      }
    )
}
