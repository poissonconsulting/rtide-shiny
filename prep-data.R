# library(rtide)
# library(poisspatial)
# library(dplyr)
# 
# sites <- rtide::harmonics$Station %>%
#   ps_longlat_to_sfc() %>%
#   ps_activate_sfc() %>%
#   select(Station, TZ) %>%
#   ps_sfc_to_coords()
# 
# saveRDS(sites, 'input/sites.rds')