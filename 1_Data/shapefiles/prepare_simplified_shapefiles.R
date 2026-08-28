# Data-prep step (run once, offline) to turn the raw WFD/EA shapefiles
# into small, pre-simplified/reprojected .rds files for fast, responsive
# rendering in the Shiny app - mirrors how op_cat_simplified.rds was
# produced. Re-run this if any of the raw shapefiles are replaced/updated.

library(sf)
library(dplyr)

bng_crs <- 27700
wgs_crs <- 4326

# River Basin Districts
rbd_raw <- st_read(
  "1_Data/shapefiles/WFD_River_Basin_Districts_Cycle_2/WFD_River_Basin_Districts_Cycle_2.shp",
  quiet = TRUE
)
rbd <- rbd_raw |>
  select(rbd_id, rbd_name, geometry) |>
  st_simplify(dTolerance = 100) |>
  st_transform(wgs_crs)
saveRDS(rbd, "1_Data/shapefiles/WFD_River_Basin_Districts_Cycle_2/rbd_simplified.rds")

# Management Catchments
mng_cat_raw <- st_read(
  "1_Data/shapefiles/WFD_Surface_Water_Management_Catchments_Cycle_2/WFD_Surface_Water_Management_Catchments_Cycle_2.shp",
  quiet = TRUE
)
mng_cat <- mng_cat_raw |>
  select(mncat_id, mncat_name, rbd_id, rbd_name, geometry) |>
  st_simplify(dTolerance = 100) |>
  st_transform(wgs_crs)
saveRDS(mng_cat, "1_Data/shapefiles/WFD_Surface_Water_Management_Catchments_Cycle_2/mng_cat_simplified.rds")

# EA Areas
ea_area_raw <- st_read("1_Data/shapefiles/ea_area/ea_area.shp", quiet = TRUE)
ea_area <- ea_area_raw |>
  select(code, long_name, short_name, geometry) |>
  st_simplify(dTolerance = 100) |>
  st_transform(wgs_crs)
saveRDS(ea_area, "1_Data/shapefiles/ea_area/ea_area_simplified.rds")
