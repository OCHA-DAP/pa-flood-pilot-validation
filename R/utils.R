#' read_all_tabs from excel file
#'
#' @param fp \code{character} file path
#' @param skip \code{numeric} Minimum number of rows to skip before reading anything, be it column names or data. Leading empty rows are automatically skipped, so this is a lower bound. Ignored if range is given.
#'
#' @return list of data.frames
#'

read_all_tabs <- function(fp, 
                          clean_names = T,
                          skip = 1, 
                          col_names = T, 
                          sheet_names = NULL, 
                          .name_repair = "unique") {
  
  if (is.null(sheet_names)) {
    sheet_names <- readxl::excel_sheets(fp)
  }
  sheet_names %>%
    purrr::map(
      \(x){
        ret <- read_xlsx(path = fp,
                         sheet = x,
                         skip = skip,
                         col_names = col_names,
                         .name_repair = .name_repair)
        if (clean_names) {
          ret <- ret %>%
            clean_names()
        }
        return(ret)
      }
    ) %>%
    set_names(sheet_names)
}


read_shape_zip <- function(path,layer){
  tmp_dir <- tempfile()
  unzip(path, exdir = tmp_dir)
  ret <- layer %>% 
    map(
      ~st_read(tmp_dir,.x)
    )
  on.exit(unlink(tmp_dir), add = TRUE)
  return(ret)
}


read_gauge_googlesheets <- function(url=Sys.getenv("GFH_GAUGE_URL")){
  sns <- sheet_names(ss = url)
  sn_filt <- sns[sns!="Sheet1"]
  sn_filt %>% 
    map(
      \(sn){
        read_sheet(ss=url,sheet=sn) %>% 
          clean_names()
      }
    ) %>% 
    set_names(nm = sn_filt)
  
}


#' drive_download_kml
#' @description
#' A work around wrapper for \link[googledrive]{drive_download} to allow downloading  kmls files (See \url{https://github.com/tidyverse/googledrive/issues/441})
#' 
#' @param id \code{character} id of drive resource
#' @param path \code{character} path to download file
#' @return kml file downloaded to path


drive_download_kml <- function(id,path){
  # get individual file drive so that we can enfore mimeType
  doi <- drive_get(as_id(id))
  
  # make sure the original mimeType is the type we want to convert
  assertthat::assert_that(doi$drive_resource[[1]]$mimeType=="application/vnd.google-earth.kml+xml",
                          msg = "mimeType is not vnd.google-earth.kml+xml so you should use googledrive::drive_download()")
  
  # convert to text/xml
  doi$drive_resource[[1]]$mimeType <- "text/xml"
  
  # download
  drive_download(doi,path = path)
}

