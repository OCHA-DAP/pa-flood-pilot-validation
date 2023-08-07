library(targets)
tar_load(basins_clipped)
out_name <- file.path(Sys.getenv("GFH_DATA_DIR"),'gha_inputs',"nga_hydrobasin_4.rds") 

basins_clipped$hybas_af_lev04_v1c %>% 
  clean_names() %>% 
  write_rds(out_name)


