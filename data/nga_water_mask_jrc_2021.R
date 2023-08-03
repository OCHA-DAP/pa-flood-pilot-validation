# nga_water_mask_jrc_2021.tif

library(rgee)
library(targets)
library(tidyrgee)
library(sf)
library(terra)
ee_Initialize()

tar_load(nga_adm)

# Load JRC yearly water classification IC
jrc_ic<- ee$ImageCollection("JRC/GSW1_4/YearlyHistory")

# last updated in 2021
jrc2021_img <- ee$Image(jrc_ic$
  filterDate("2021-01-01","2021-12-31")$first()
)

# create bbox of nga to clip to
bbox_ee <- nga_adm$adm0 %>% 
  st_bbox() %>% 
  st_as_sfc() %>% 
  sf_as_ee()

# clip to bbox
nga_water_mask <- jrc2021_img$
  clip(bbox_ee)

# sanity check
Map$addLayer(
  nga_water_mask$select("waterClass"),
  visParams = list(
    # band= "waterClass",
    min= 2,
    max=3,
    palette=c("lightblue","purple")
  )
)

# define out file path
out_r <- file.path(Sys.getenv("AA_DATA_DIR"),
          "public",
          "raw",
          "nga",
          "jrc_water",
          "nga_water_mask_jrc_2021.tif")

# convert ee$Image to raster object.
jrc2021_watermask <- rgee::ee_as_raster(image = nga_water_mask,
                   scale = 30,
                   region = bbox_ee,
                   maxPixels = 10e12
                   )             

dir(file.path(Sys.getenv("AA_DATA_DIR"),
              "public",
              "raw",
              "nga"))
writeRaster(x = jrc2021_watermask,filename = out_r)

