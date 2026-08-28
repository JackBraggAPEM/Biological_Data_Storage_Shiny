#
# Biological Data Storage Shiny App
#
# Three interaction modes, each gated behind its own checkbox so only one
# is shown/active at a time:
#   1. Add new site   - manual form entry for every Site sheet column.
#   2. Add sample      - record a macroinvertebrate sample (WHPT_NTAXA,
#                         WHPT_ASPT) against an existing site.
#   3. Download data   - filter and export saved Sites/Samples as CSV.
# A downloadable Excel template (Sites + Samples sheets) can also be
# filled in offline and re-uploaded to add many sites/samples at once.
#
# Saved sites/samples persist across sessions (stored as CSVs). Hovering
# over a site marker shows how many samples exist for it and the date
# of the most recent one.
#

library(shiny)
library(leaflet)
library(sf)
library(DT)
library(htmltools)
library(readxl)
library(writexl)

# ---- Paths -------------------------------------------------------------

op_cat_rds   <- "1_Data/shapefiles/op_cat/op_cat_simplified.rds"
rbd_rds      <- "1_Data/shapefiles/WFD_River_Basin_Districts_Cycle_2/rbd_simplified.rds"
mng_cat_rds  <- "1_Data/shapefiles/WFD_Surface_Water_Management_Catchments_Cycle_2/mng_cat_simplified.rds"
ea_area_rds  <- "1_Data/shapefiles/ea_area/ea_area_simplified.rds"

sites_csv_path  <- "1_Data/biological_data/sites.csv"
samples_csv_path <- "1_Data/biological_data/samples.csv"

bng_crs <- 27700  # British National Grid (Easting/Northing)
wgs_crs <- 4326   # lat/lon, required by leaflet

# ---- Data ---------------------------------------------------------------

# Pre-simplified/reprojected polygons for each shapefile level (see
# 1_Data/shapefiles/prepare_simplified_shapefiles.R for how these .rds
# files are produced from the raw WFD/EA shapefiles) for fast, responsive
# rendering.
op_cat  <- readRDS(op_cat_rds)
rbd     <- readRDS(rbd_rds)
mng_cat <- readRDS(mng_cat_rds)
ea_area <- readRDS(ea_area_rds)

# Levels the shapefile browser can show, each with its own polygon data
# and the column holding that level's display name. Operational catchment
# is the only level also recorded directly on saved sites
# (OPERATIONAL_CATCHMENT), so other levels are resolved down to the
# operational catchments they spatially overlap when filtering
# sites/samples (see selected_op_cats() below).
shapefile_levels <- list(
  river_basin_district = list(label = "River Basin District",  data = rbd,     name_col = "rbd_name"),
  management_catchment = list(label = "Management Catchment",  data = mng_cat, name_col = "mncat_name"),
  operational_catchment = list(label = "Operational Catchment", data = op_cat,  name_col = "operationa"),
  ea_area               = list(label = "EA Area",               data = ea_area, name_col = "long_name")
)
shapefile_level_choices <- setNames(
  names(shapefile_levels),
  vapply(shapefile_levels, function(x) x$label, character(1))
)

# Full Site sheet schema (matches the lab's existing site spreadsheet;
# column names are all-caps to match that convention). Only SITE_ID/
# EASTING/NORTHING are required for a site to be usable on the map - the
# rest are optional environmental/classification predictors, each exposed
# as its own input in the manual "Add new site" form.
site_field_defs <- list(
  list(id = "site_source",            col = "SITE_SOURCE",            type = "text"),
  list(id = "river_basin_district",   col = "RIVER_BASIN_DISTRICT",   type = "select", choices = sort(unique(rbd$rbd_name))),
  list(id = "management_catchment",   col = "MANAGEMENT_CATCHMENT",   type = "select", choices = sort(unique(mng_cat$mncat_name))),
  list(id = "operational_catchment",  col = "OPERATIONAL_CATCHMENT",  type = "select", choices = sort(unique(op_cat$operationa))),
  list(id = "ea_area",                col = "EA_AREA",                type = "select", choices = sort(unique(ea_area$long_name))),
  list(id = "altitude",               col = "ALTITUDE",               type = "numeric"),
  list(id = "slope",                  col = "SLOPE",                  type = "numeric"),
  list(id = "distance_from_source",   col = "DISTANCE_FROM_SOURCE",   type = "numeric"),
  list(id = "discharge_category",     col = "DISCHARGE_CATEGORY",     type = "numeric"),
  list(id = "width",                  col = "WIDTH",                  type = "numeric"),
  list(id = "depth",                  col = "DEPTH",                  type = "numeric"),
  list(id = "boulder_cobbles",        col = "BOULDER_COBBLES",        type = "numeric"),
  list(id = "pebbles_gravel",         col = "PEBBLES_GRAVEL",         type = "numeric"),
  list(id = "sand",                   col = "SAND",                   type = "numeric"),
  list(id = "silt_clay",              col = "SILT_CLAY",              type = "numeric"),
  list(id = "alkalinity",             col = "ALKALINITY",             type = "numeric"),
  list(id = "conductivity",           col = "CONDUCTIVITY",           type = "numeric"),
  list(id = "total_hardness",         col = "TOTAL_HARDNESS",         type = "numeric"),
  list(id = "calcium",                col = "CALCIUM",                type = "numeric")
)

# EASTING/NORTHING are stored as fixed-width 6-digit strings (not numbers)
# so a leading zero (e.g. "045123") is never lost. DATE_ADDED/DATE_CHANGED
# are filled in automatically by the app (never entered by the user).
site_text_cols <- c(
  "SITE_SOURCE", "SITE_ID", "EASTING", "NORTHING",
  "RIVER_BASIN_DISTRICT", "MANAGEMENT_CATCHMENT", "OPERATIONAL_CATCHMENT", "EA_AREA",
  "DATE_ADDED", "DATE_CHANGED"
)
site_numeric_cols <- c(
  "ALTITUDE", "SLOPE", "DISTANCE_FROM_SOURCE",
  "DISCHARGE_CATEGORY", "WIDTH", "DEPTH", "BOULDER_COBBLES", "PEBBLES_GRAVEL",
  "SAND", "SILT_CLAY", "ALKALINITY", "CONDUCTIVITY", "TOTAL_HARDNESS", "CALCIUM"
)

# Force a value to a 6-digit British National Grid reference string,
# padding with leading zeros where needed (e.g. 45123 -> "045123").
# Returns NA for anything that isn't a whole number in [0, 999999].
pad_grid_ref <- function(x) {
  chr <- trimws(as.character(x))
  already_valid <- grepl("^[0-9]{6}$", chr)
  num <- suppressWarnings(as.numeric(chr))
  can_pad <- !is.na(num) & num >= 0 & num <= 999999 & num == floor(num)
  ifelse(already_valid, chr, ifelse(can_pad, sprintf("%06d", as.integer(num)), NA_character_))
}
full_site_cols <- c(
  "SITE_SOURCE", "SITE_ID", "EASTING", "NORTHING", "RIVER_BASIN_DISTRICT",
  "MANAGEMENT_CATCHMENT", "OPERATIONAL_CATCHMENT", "EA_AREA", "ALTITUDE", "SLOPE", "DISTANCE_FROM_SOURCE",
  "DISCHARGE_CATEGORY", "WIDTH", "DEPTH", "BOULDER_COBBLES", "PEBBLES_GRAVEL",
  "SAND", "SILT_CLAY", "ALKALINITY", "CONDUCTIVITY", "TOTAL_HARDNESS", "CALCIUM",
  "DATE_ADDED", "DATE_CHANGED"
)
# Columns a user fills in via the template - DATE_ADDED/DATE_CHANGED are
# excluded since the app sets them automatically.
template_site_cols <- setdiff(full_site_cols, c("DATE_ADDED", "DATE_CHANGED"))

empty_sites <- setNames(
  data.frame(matrix(nrow = 0, ncol = length(full_site_cols))),
  full_site_cols
)
for (col in site_text_cols) empty_sites[[col]] <- character()
for (col in site_numeric_cols) empty_sites[[col]] <- double()
empty_sites <- empty_sites[, full_site_cols]

# Combine two data frames that may have different columns (e.g. an older
# sites.csv from before new predictor columns were added), filling
# missing columns with NA rather than erroring.
bind_fill <- function(a, b) {
  all_cols <- union(names(a), names(b))
  for (col in setdiff(all_cols, names(a))) a[[col]] <- NA
  for (col in setdiff(all_cols, names(b))) b[[col]] <- NA
  rbind(a[all_cols], b[all_cols])
}

# Backfill any columns missing from a data frame (e.g. a sites.csv/
# samples.csv saved before DATE_ADDED/DATE_CHANGED existed) with NA, and
# put columns in a fixed order.
ensure_cols <- function(df, cols) {
  for (col in setdiff(cols, names(df))) df[[col]] <- NA
  df[, cols]
}

# LIFE is an optional additional macroinvertebrate metric - unlike
# WHPT_NTAXA/WHPT_ASPT it isn't required for a sample to be valid.
full_sample_cols <- c("SITE_ID", "SAMPLE_DATE", "WHPT_NTAXA", "WHPT_ASPT", "LIFE", "DATE_ADDED")
template_sample_cols <- setdiff(full_sample_cols, "DATE_ADDED")

empty_samples <- data.frame(
  SITE_ID     = character(),
  SAMPLE_DATE = character(),
  WHPT_NTAXA  = double(),
  WHPT_ASPT   = double(),
  LIFE        = double(),
  DATE_ADDED  = character(),
  stringsAsFactors = FALSE
)

now_stamp <- function() format(Sys.time(), "%Y-%m-%d %H:%M:%S")

# Replace one or more existing sites (matched by SITE_ID) with new data.
# DATE_ADDED is carried over from the row being replaced so it always
# reflects when the site was first created; DATE_CHANGED is stamped now.
overwrite_sites <- function(current, replacement, change_type = "SITE_OVERWRITTEN") {
  old_rows <- current[current$SITE_ID %in% replacement$SITE_ID, ]
  match_idx <- match(replacement$SITE_ID, old_rows$SITE_ID)
  replacement$DATE_ADDED <- old_rows$DATE_ADDED[match_idx]
  replacement$DATE_CHANGED <- now_stamp()
  remaining <- current[!(current$SITE_ID %in% replacement$SITE_ID), ]
  bind_fill(remaining, replacement)
}

read_csv_or_default <- function(path, default, col_classes = NA) {
  # read.csv's own default for colClasses is NA (auto-detect every
  # column); passing NULL instead breaks it internally, so col_classes
  # must default to NA here too when the caller doesn't override it.
  if (file.exists(path)) {
    read.csv(path, stringsAsFactors = FALSE, colClasses = col_classes)
  } else {
    default
  }
}

# EASTING/NORTHING must stay character on reload or read.csv's automatic
# type conversion would strip any leading zero back out.
sites_col_classes <- c(EASTING = "character", NORTHING = "character")

save_csv <- function(df, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  write.csv(df, path, row.names = FALSE)
}

# Convert a data frame with EASTING/NORTHING (BNG) columns to an sf object
# in WGS84 for plotting with leaflet. Coordinates are built from numeric
# copies of EASTING/NORTHING so the original zero-padded string columns
# are left untouched (and still available for popups/tables).
sites_to_wgs84 <- function(sites) {
  if (nrow(sites) == 0) return(NULL)
  pts <- sites
  pts$.easting_num  <- as.numeric(sites$EASTING)
  pts$.northing_num <- as.numeric(sites$NORTHING)
  st_as_sf(pts, coords = c(".easting_num", ".northing_num"), crs = bng_crs) |>
    st_transform(wgs_crs)
}

# For each point in pts_sf (WGS84), return the matching value of name_col
# from polygons (also WGS84) - i.e. which polygon each point falls inside.
# Points that don't intersect any polygon (e.g. right on a simplified/
# generalised boundary) fall back to their nearest polygon, so every
# point with valid coordinates gets a value.
lookup_polygon_name <- function(pts_sf, polygons, name_col) {
  hits <- st_intersects(pts_sf, polygons)
  first_hit <- vapply(hits, function(h) if (length(h) > 0) h[1] else NA_integer_, integer(1))
  matched <- polygons[[name_col]][first_hit]
  missing <- is.na(first_hit)
  if (any(missing)) {
    nearest <- st_nearest_feature(pts_sf[missing, ], polygons)
    matched[missing] <- polygons[[name_col]][nearest]
  }
  matched
}

# Fill in RIVER_BASIN_DISTRICT/OPERATIONAL_CATCHMENT/EA_AREA for any site
# rows where they're blank, by looking up which polygon each site's
# EASTING/NORTHING falls inside. Rows that already have a value for a
# given field are left untouched; rows without usable coordinates are
# skipped (still blank afterwards).
infill_site_geography <- function(df) {
  geo_fields <- list(
    RIVER_BASIN_DISTRICT  = list(data = rbd,     name_col = "rbd_name"),
    MANAGEMENT_CATCHMENT  = list(data = mng_cat, name_col = "mncat_name"),
    OPERATIONAL_CATCHMENT = list(data = op_cat,  name_col = "operationa"),
    EA_AREA                = list(data = ea_area, name_col = "long_name")
  )
  for (col in names(geo_fields)) if (!col %in% names(df)) df[[col]] <- NA_character_
  if (nrow(df) == 0) return(df)

  is_blank <- function(x) is.na(x) | trimws(as.character(x)) == ""
  missing_any <- Reduce(`|`, lapply(names(geo_fields), function(col) is_blank(df[[col]])))

  has_coords <- !is_blank(df$EASTING) & !is_blank(df$NORTHING) &
    grepl("^[0-9]{6}$", df$EASTING) & grepl("^[0-9]{6}$", df$NORTHING)
  idx <- which(missing_any & has_coords)
  if (length(idx) == 0) return(df)

  pts <- df[idx, ]
  pts$.easting_num  <- as.numeric(pts$EASTING)
  pts$.northing_num <- as.numeric(pts$NORTHING)
  pts_sf <- st_as_sf(pts, coords = c(".easting_num", ".northing_num"), crs = bng_crs) |>
    st_transform(wgs_crs)

  for (col in names(geo_fields)) {
    need <- is_blank(df[[col]][idx])
    if (!any(need)) next
    field <- geo_fields[[col]]
    looked_up <- lookup_polygon_name(pts_sf[need, ], field$data, field$name_col)
    df[[col]][idx[need]] <- looked_up
  }
  df
}

# Filter a Sites data frame down to the given SITE_ID/OPERATIONAL_CATCHMENT/
# RIVER_BASIN_DISTRICT/SITE_SOURCE values. An empty selection for any
# field means "no filter on that field" (i.e. include every value).
filter_sites_df <- function(df, site_ids, catchments, rbds, sources) {
  if (length(site_ids) > 0) df <- df[df$SITE_ID %in% site_ids, ]
  if (length(catchments) > 0) df <- df[df$OPERATIONAL_CATCHMENT %in% catchments, ]
  if (length(rbds) > 0) df <- df[df$RIVER_BASIN_DISTRICT %in% rbds, ]
  if (length(sources) > 0) df <- df[df$SITE_SOURCE %in% sources, ]
  df
}

# Filter a Samples data frame to only the given sites and a SAMPLE_DATE range.
filter_samples_df <- function(samples_df, allowed_site_ids, date_start, date_end) {
  df <- samples_df[samples_df$SITE_ID %in% allowed_site_ids, ]
  d <- as.Date(df$SAMPLE_DATE)
  keep <- !is.na(d) &
    (is.na(date_start) | d >= date_start) &
    (is.na(date_end) | d <= date_end)
  df[keep, ]
}

# Per-site sample counts and most recent sample date.
summarise_samples <- function(samples) {
  if (nrow(samples) == 0) {
    return(data.frame(SITE_ID = character(), N_SAMPLES = integer(), LAST_DATE = as.Date(character())))
  }
  d <- as.Date(samples$SAMPLE_DATE)
  n_samples <- tapply(d, samples$SITE_ID, length)
  last_date <- tapply(d, samples$SITE_ID, max)
  data.frame(
    SITE_ID   = names(n_samples),
    N_SAMPLES = as.integer(n_samples),
    LAST_DATE = as.Date(last_date[names(n_samples)], origin = "1970-01-01"),
    row.names = NULL
  )
}

# Parse a date column that may come back from readxl as Date/POSIXct
# (when the Excel cell is date-formatted), a numeric Excel serial, or
# plain text (e.g. "2026-08-01" or "01/08/2026").
parse_dates <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXct")) return(as.Date(x))
  if (is.numeric(x)) return(as.Date(x, origin = "1899-12-30"))

  out <- suppressWarnings(as.Date(as.character(x)))
  still_na <- is.na(out) & !is.na(x) & as.character(x) != ""
  if (any(still_na)) {
    alt <- suppressWarnings(as.Date(as.character(x)[still_na], format = "%d/%m/%Y"))
    out[still_na] <- alt
  }
  out
}

instructions_df <- data.frame(
  Instructions = c(
    "This template has two data sheets: 'Sites' and 'Samples'.",
    "Do not rename the sheet tabs or column headers.",
    "",
    "Sites sheet required columns: SITE_ID, EASTING, NORTHING.",
    "EASTING/NORTHING must each be a 6-digit British National Grid reference",
    "(e.g. 045123). Values are auto-padded with leading zeros on import if",
    "Excel has stored them as plain numbers.",
    "Sites sheet optional columns: SITE_SOURCE, RIVER_BASIN_DISTRICT, MANAGEMENT_CATCHMENT,",
    "OPERATIONAL_CATCHMENT, EA_AREA, ALTITUDE, SLOPE, DISTANCE_FROM_SOURCE, DISCHARGE_CATEGORY,",
    "WIDTH, DEPTH, BOULDER_COBBLES, PEBBLES_GRAVEL, SAND, SILT_CLAY, ALKALINITY, CONDUCTIVITY,",
    "TOTAL_HARDNESS, CALCIUM.",
    "Leave optional columns blank if unknown - only SITE_ID/EASTING/NORTHING are required.",
    "If RIVER_BASIN_DISTRICT, MANAGEMENT_CATCHMENT, OPERATIONAL_CATCHMENT, and/or EA_AREA are",
    "left blank, the app automatically fills them in based on EASTING/NORTHING.",
    "Do not add DATE_ADDED/DATE_CHANGED columns - the app fills these in automatically.",
    "",
    "Samples sheet required columns: SITE_ID, SAMPLE_DATE (YYYY-MM-DD), WHPT_NTAXA, WHPT_ASPT.",
    "Samples sheet optional columns: LIFE.",
    "Every SITE_ID used in the Samples sheet must already exist - either",
    "already saved in the app, or included in the Sites sheet of this same file.",
    "",
    "A Sites row whose SITE_ID already exists in the app is skipped on import.",
    "",
    "This Instructions sheet is ignored on upload; no need to delete it."
  )
)

# Build the blank upload template (Instructions + empty Sites/Samples sheets).
write_template <- function(path) {
  write_xlsx(
    list(
      Instructions = instructions_df,
      Sites = empty_sites[, template_site_cols],
      Samples = empty_samples[, template_sample_cols]
    ),
    path = path
  )
}

# ---- UI -------------------------------------------------------------

# Build one input control for an optional site field, based on its type.
build_site_field_input <- function(field) {
  label <- field$col
  switch(
    field$type,
    text = textInput(field$id, label, value = ""),
    numeric = numericInput(field$id, label, value = NA),
    select = selectInput(
      field$id, label,
      choices = c("", field$choices),
      selected = ""
    )
  )
}

ui <- fluidPage(
  titlePanel("Biological Data Storage: Site Map"),
  sidebarLayout(
    sidebarPanel(
      tags$b("Bulk import from template"),
      p("Download the template, fill in the Sites and/or Samples sheets, then upload it here."),
      downloadButton("download_template", "Download template (.xlsx)"),
      hr(),
      fileInput("template_upload", "Upload completed template", accept = ".xlsx"),
      hr(),
      tags$b("Add data manually"),
      checkboxInput("mode_add_site", "Add new site", value = FALSE),
      conditionalPanel(
        condition = "input.mode_add_site == true",
        div(
          style = "max-height: 500px; overflow-y: auto;",
          wellPanel(
            h5("Required"),
            textInput("site_id", "SITE_ID", placeholder = "e.g. SITE001"),
            textInput("easting", "EASTING (BNG)", placeholder = "e.g. 045123"),
            textInput("northing", "NORTHING (BNG)", placeholder = "e.g. 312345"),
            helpText("Easting/Northing must each be exactly 6 digits (leading zeros allowed)."),
            h5("Optional"),
            lapply(site_field_defs, build_site_field_input),
            checkboxInput("overwrite_site", "Overwrite if Site ID already exists", value = FALSE),
            actionButton("add_site", "Add Site", class = "btn-primary")
          )
        )
      ),
      checkboxInput("mode_add_sample", "Add macroinvertebrate sample", value = FALSE),
      conditionalPanel(
        condition = "input.mode_add_sample == true",
        wellPanel(
          selectInput("sample_site_id", "SITE_ID", choices = character(0)),
          dateInput("sample_date", "SAMPLE_DATE", value = Sys.Date()),
          numericInput("whpt_ntaxa", "WHPT_NTAXA", value = NA, step = 1),
          numericInput("whpt_aspt", "WHPT_ASPT", value = NA, step = 0.01),
          numericInput("life", "LIFE", value = NA),
          actionButton("add_sample", "Add Sample", class = "btn-primary")
        )
      ),
      hr(),
      tags$b("Download data"),
      checkboxInput("mode_download", "Show download options", value = FALSE),
      conditionalPanel(
        condition = "input.mode_download == true",
        wellPanel(
          p("Leave a filter empty to include every value for that field. The date range only affects the Samples download."),
          selectizeInput("dl_site_id", "SITE_ID", choices = character(0), multiple = TRUE),
          selectizeInput("dl_catchment", "OPERATIONAL_CATCHMENT", choices = character(0), multiple = TRUE),
          selectizeInput("dl_rbd", "RIVER_BASIN_DISTRICT", choices = character(0), multiple = TRUE),
          selectizeInput("dl_site_source", "SITE_SOURCE", choices = character(0), multiple = TRUE),
          dateRangeInput("dl_date_range", "SAMPLE_DATE range"),
          downloadButton("download_sites", "Download Sites (.csv)"),
          downloadButton("download_samples", "Download Samples (.csv)"),
          hr(),
          p("Download every site and sample within the currently selected shapefile area (level + area dropdowns above the map), combined into a single Excel file (Sites + Samples sheets)."),
          downloadButton("download_catchment_combined", "Download selected area (Sites + Samples .xlsx)")
        )
      )
    ),
    mainPanel(
      fluidRow(
        column(
          12,
          div(
            style = "display: flex; justify-content: flex-end; gap: 15px; flex-wrap: wrap;",
            div(
              style = "min-width: 220px;",
              selectInput(
                "shapefile_level",
                "Shapefile level",
                choices = shapefile_level_choices,
                selected = "river_basin_district",
                width = "100%"
              )
            ),
            div(
              style = "min-width: 220px;",
              selectInput(
                "catchment_filter",
                "Select area",
                choices = c("All", sort(unique(rbd$rbd_name))),
                selected = "All",
                width = "100%"
              )
            )
          )
        )
      ),
      leafletOutput("map", height = 600),
      hr(),
      tabsetPanel(
        tabPanel("Sites", DTOutput("sites_table")),
        tabPanel("Samples", DTOutput("samples_table"))
      )
    )
  )
)

# ---- Server -------------------------------------------------------------

server <- function(input, output, session) {

  # Infill any missing RIVER_BASIN_DISTRICT/OPERATIONAL_CATCHMENT/EA_AREA
  # for sites saved before these fields existed (or left blank), and
  # persist the result so the fix only has to run once.
  initial_sites <- infill_site_geography(ensure_cols(
    read_csv_or_default(sites_csv_path, empty_sites, col_classes = sites_col_classes),
    full_site_cols
  ))
  save_csv(initial_sites, sites_csv_path)
  sites <- reactiveVal(initial_sites)
  samples <- reactiveVal(ensure_cols(
    read_csv_or_default(samples_csv_path, empty_samples),
    full_sample_cols
  ))

  # Only one interaction mode active at a time.
  observeEvent(input$mode_add_site, {
    if (isTRUE(input$mode_add_site)) {
      updateCheckboxInput(session, "mode_add_sample", value = FALSE)
      updateCheckboxInput(session, "mode_download", value = FALSE)
    }
  })
  observeEvent(input$mode_add_sample, {
    if (isTRUE(input$mode_add_sample)) {
      updateCheckboxInput(session, "mode_add_site", value = FALSE)
      updateCheckboxInput(session, "mode_download", value = FALSE)
    }
  })
  observeEvent(input$mode_download, {
    if (isTRUE(input$mode_download)) {
      updateCheckboxInput(session, "mode_add_site", value = FALSE)
      updateCheckboxInput(session, "mode_add_sample", value = FALSE)
    }
  })

  # Keep the sample form's site dropdown in sync with the current site list.
  observeEvent(sites(), {
    updateSelectInput(session, "sample_site_id", choices = sites()$SITE_ID)
  }, ignoreNULL = FALSE)

  # Keep the download filter choices in sync with the current site list.
  observeEvent(sites(), {
    s <- sites()
    updateSelectizeInput(session, "dl_site_id", choices = sort(unique(s$SITE_ID)))
    updateSelectizeInput(
      session, "dl_catchment",
      choices = sort(unique(s$OPERATIONAL_CATCHMENT[!is.na(s$OPERATIONAL_CATCHMENT) & s$OPERATIONAL_CATCHMENT != ""]))
    )
    updateSelectizeInput(
      session, "dl_rbd",
      choices = sort(unique(s$RIVER_BASIN_DISTRICT[!is.na(s$RIVER_BASIN_DISTRICT) & s$RIVER_BASIN_DISTRICT != ""]))
    )
    updateSelectizeInput(
      session, "dl_site_source",
      choices = sort(unique(s$SITE_SOURCE[!is.na(s$SITE_SOURCE) & s$SITE_SOURCE != ""]))
    )
  }, ignoreNULL = FALSE)

  # Keep the download date-range picker spanning the full range of saved
  # sample dates, so it includes everything by default.
  observeEvent(samples(), {
    d <- as.Date(samples()$SAMPLE_DATE)
    d <- d[!is.na(d)]
    rng <- if (length(d) > 0) range(d) else c(Sys.Date(), Sys.Date())
    updateDateRangeInput(session, "dl_date_range", start = rng[1], end = rng[2], min = rng[1], max = rng[2])
  }, ignoreNULL = FALSE)

  # Base map: tiles only. Catchment polygons are drawn/redrawn separately
  # below so the (filterable) polygon layer doesn't require a full
  # re-render of the map widget.
  output$map <- renderLeaflet({
    leaflet() |>
      addProviderTiles(providers$Esri.WorldGrayCanvas) |>
      fitBounds(
        st_bbox(op_cat)[["xmin"]],
        st_bbox(op_cat)[["ymin"]],
        st_bbox(op_cat)[["xmax"]],
        st_bbox(op_cat)[["ymax"]]
      )
  })
  

  # The sf data + display-name column for the currently selected level.
  current_level <- reactive({
    shapefile_levels[[input$shapefile_level]]
  })

  # When the shapefile level changes, repopulate the area dropdown with
  # "All" plus every distinct name at that level.
  observeEvent(input$shapefile_level, {
    lvl <- shapefile_levels[[input$shapefile_level]]
    # Prevent downstream reactives (selected_polygons/selected_op_cats)
    # from firing with the old area value still selected against the
    # new level's data, which could crash if that value doesn't exist
    # in the new list of choices.
    freezeReactiveValue(input, "catchment_filter")
    updateSelectInput(
      session, "catchment_filter",
      choices = c("All", sort(unique(lvl$data[[lvl$name_col]]))),
      selected = "All"
    )
  }, ignoreInit = TRUE)

  # Polygon(s) matching the current level + area selection ("All" matches
  # every polygon at that level).
  selected_polygons <- reactive({
    lvl <- current_level()
    val <- input$catchment_filter
    matched <- if (is.null(val) || identical(val, "All")) {
      lvl$data
    } else {
      lvl$data[lvl$data[[lvl$name_col]] == val, ]
    }
    # Fall back to showing every polygon at this level if the selected
    # area doesn't exist for it (e.g. transient state right after
    # switching levels), rather than crashing on an empty selection.
    if (nrow(matched) == 0) lvl$data else matched
  })

  # Operational catchments overlapping the current selection - the finest
  # level, and the only one recorded directly on saved sites
  # (OPERATIONAL_CATCHMENT). Coarser selections (RBD/management
  # catchment/EA area) are resolved down via a spatial intersection,
  # since their polygon boundaries/names don't line up exactly with
  # op_cat's own attribute columns.
  selected_op_cats <- reactive({
    if (identical(input$catchment_filter, "All")) return(op_cat)
    if (identical(input$shapefile_level, "operational_catchment")) return(selected_polygons())
    overlap <- lengths(st_intersects(op_cat, st_union(st_geometry(selected_polygons())))) > 0
    op_cat[overlap, ]
  })

  # Redraw the polygon layer when the level/area filter changes, and
  # zoom the map to fit whatever is shown.
  observeEvent(selected_polygons(), {
    filtered <- selected_polygons()
    lvl <- current_level()
    names <- filtered[[lvl$name_col]]

    proxy <- leafletProxy("map") |>
      clearGroup("catchments") |>
      addPolygons(
        data = filtered,
        group = "catchments",
        color = "#3182bd",
        weight = 1,
        fillOpacity = 0.05,
        label = names,
        popup = paste0("<b>", lvl$label, ":</b> ", names)
      )

    bbox <- st_bbox(filtered)
    proxy |> fitBounds(bbox[["xmin"]], bbox[["ymin"]], bbox[["xmax"]], bbox[["ymax"]])
  }, ignoreNULL = FALSE)

  output$download_template <- downloadHandler(
    filename = function() "site_sample_template.xlsx",
    content = function(file) write_template(file)
  )

  # Bulk import: read the Sites and Samples sheets from an uploaded copy
  # of the template and append any valid new rows.
  observeEvent(input$template_upload, {
    path <- input$template_upload$datapath

    sheet_names <- tryCatch(excel_sheets(path), error = function(e) NULL)
    if (is.null(sheet_names) || !all(c("Sites", "Samples") %in% sheet_names)) {
      showNotification(
        "Uploaded file must contain 'Sites' and 'Samples' sheets - please use the provided template.",
        type = "error"
      )
      return()
    }

    new_sites_raw   <- tryCatch(read_excel(path, sheet = "Sites"), error = function(e) NULL)
    new_samples_raw <- tryCatch(read_excel(path, sheet = "Samples"), error = function(e) NULL)
    if (is.null(new_sites_raw) || is.null(new_samples_raw)) {
      showNotification("Could not read the Sites/Samples sheets from the uploaded file.", type = "error")
      return()
    }

    required_site_cols <- c("SITE_ID", "EASTING", "NORTHING")
    if (!all(required_site_cols %in% names(new_sites_raw))) {
      showNotification(
        paste0("Sites sheet must contain columns: ", paste(required_site_cols, collapse = ", ")),
        type = "error"
      )
      return()
    }

    required_sample_cols <- c("SITE_ID", "SAMPLE_DATE", "WHPT_NTAXA", "WHPT_ASPT")
    if (!all(required_sample_cols %in% names(new_samples_raw))) {
      showNotification(
        paste0("Samples sheet must contain columns: ", paste(required_sample_cols, collapse = ", ")),
        type = "error"
      )
      return()
    }

    # ---- Sites: keep well-formed rows, drop duplicates (within-sheet and
    # against sites already saved). Optional predictor columns are kept
    # if present in the sheet and left NA otherwise.
    current_sites <- sites()
    candidate_sites <- as.data.frame(new_sites_raw)
    for (col in setdiff(full_site_cols, names(candidate_sites))) candidate_sites[[col]] <- NA
    candidate_sites <- candidate_sites[, full_site_cols]
    for (col in site_text_cols) candidate_sites[[col]] <- trimws(as.character(candidate_sites[[col]]))
    for (col in site_numeric_cols) candidate_sites[[col]] <- suppressWarnings(as.numeric(candidate_sites[[col]]))
    candidate_sites$EASTING  <- pad_grid_ref(candidate_sites$EASTING)
    candidate_sites$NORTHING <- pad_grid_ref(candidate_sites$NORTHING)
    candidate_sites <- infill_site_geography(candidate_sites)

    well_formed <- !is.na(candidate_sites$SITE_ID) & candidate_sites$SITE_ID != "" &
      !is.na(candidate_sites$EASTING) & !is.na(candidate_sites$NORTHING)
    n_malformed_sites <- sum(!well_formed)
    candidate_sites <- candidate_sites[well_formed, ]
    candidate_sites <- candidate_sites[!duplicated(candidate_sites$SITE_ID), ]

    matches_existing <- candidate_sites$SITE_ID %in% current_sites$SITE_ID
    sites_skipped <- n_malformed_sites + sum(matches_existing)
    candidate_sites <- candidate_sites[!matches_existing, ]
    candidate_sites$DATE_ADDED   <- now_stamp()
    candidate_sites$DATE_CHANGED <- NA_character_
    sites_added <- nrow(candidate_sites)
    updated_sites <- bind_fill(current_sites, candidate_sites)

    if (sites_added > 0) {
      sites(updated_sites)
      save_csv(updated_sites, sites_csv_path)
    }

    # ---- Samples: SITE_ID must exist among current + newly-added sites.
    known_site_ids <- updated_sites$SITE_ID
    candidate_samples <- data.frame(
      SITE_ID     = trimws(as.character(new_samples_raw$SITE_ID)),
      SAMPLE_DATE = parse_dates(new_samples_raw$SAMPLE_DATE),
      WHPT_NTAXA  = suppressWarnings(as.numeric(new_samples_raw$WHPT_NTAXA)),
      WHPT_ASPT   = suppressWarnings(as.numeric(new_samples_raw$WHPT_ASPT)),
      LIFE        = if ("LIFE" %in% names(new_samples_raw)) suppressWarnings(as.numeric(new_samples_raw$LIFE)) else NA_real_,
      DATE_ADDED  = now_stamp(),
      stringsAsFactors = FALSE
    )
    valid <- !is.na(candidate_samples$SITE_ID) & candidate_samples$SITE_ID != "" &
      !is.na(candidate_samples$SAMPLE_DATE) &
      !is.na(candidate_samples$WHPT_NTAXA) & !is.na(candidate_samples$WHPT_ASPT) &
      candidate_samples$SITE_ID %in% known_site_ids
    samples_skipped <- sum(!valid)
    candidate_samples <- candidate_samples[valid, ]
    candidate_samples$SAMPLE_DATE <- as.character(candidate_samples$SAMPLE_DATE)
    samples_added <- nrow(candidate_samples)

    if (samples_added > 0) {
      updated_samples <- rbind(samples(), candidate_samples)
      samples(updated_samples)
      save_csv(updated_samples, samples_csv_path)
    }

    showNotification(
      paste0(
        "Import complete. Sites added: ", sites_added,
        " (skipped: ", sites_skipped, "). ",
        "Samples added: ", samples_added, " (skipped: ", samples_skipped,
        " - unknown SITE_ID, missing/invalid date, or missing WHPT_NTAXA/WHPT_ASPT)."
      ),
      type = "message", duration = 10
    )
  })

  # Add a new site after validating inputs. Required fields are SITE_ID/
  # EASTING/NORTHING; every optional predictor field is read from its
  # own input (blank text -> NA, so unfilled fields stay NA).
  observeEvent(input$add_site, {
    site_id  <- trimws(input$site_id)
    easting  <- trimws(input$easting)
    northing <- trimws(input$northing)

    if (site_id == "") {
      showNotification("Please enter a Site ID.", type = "error")
      return()
    }
    if (!grepl("^[0-9]{6}$", easting) || !grepl("^[0-9]{6}$", northing)) {
      showNotification(
        "Easting and Northing must each be exactly 6 digits (e.g. 045123).",
        type = "error"
      )
      return()
    }

    current <- sites()
    is_existing <- site_id %in% current$SITE_ID
    if (is_existing && !isTRUE(input$overwrite_site)) {
      showNotification(
        paste0(
          "Site ID '", site_id, "' already exists. Tick 'Overwrite if Site ID already exists' ",
          "to replace it, or choose a unique ID."
        ),
        type = "error"
      )
      return()
    }

    new_row <- setNames(as.data.frame(matrix(nrow = 1, ncol = length(full_site_cols))), full_site_cols)
    for (col in site_text_cols) new_row[[col]] <- NA_character_
    for (col in site_numeric_cols) new_row[[col]] <- NA_real_
    new_row$SITE_ID  <- site_id
    new_row$EASTING  <- easting
    new_row$NORTHING <- northing
    for (field in site_field_defs) {
      val <- input[[field$id]]
      if (is.character(val) && identical(val, "")) val <- NA
      new_row[[field$col]] <- val
    }
    new_row <- infill_site_geography(new_row)

    if (is_existing) {
      updated <- overwrite_sites(current, new_row)
      showNotification(
        paste0("Overwrote site '", site_id, "'."),
        type = "message"
      )
    } else {
      new_row$DATE_ADDED <- now_stamp()
      updated <- bind_fill(current, new_row)
      showNotification(paste0("Saved site '", site_id, "'."), type = "message")
    }

    sites(updated)
    save_csv(updated, sites_csv_path)

    updateTextInput(session, "site_id", value = "")
    updateTextInput(session, "easting", value = "")
    updateTextInput(session, "northing", value = "")
    updateCheckboxInput(session, "overwrite_site", value = FALSE)
    for (field in site_field_defs) {
      if (field$type == "text") updateTextInput(session, field$id, value = "")
      if (field$type == "numeric") updateNumericInput(session, field$id, value = NA)
      if (field$type == "select") updateSelectInput(session, field$id, selected = "")
    }
  })

  # Add a macroinvertebrate sample for an existing site.
  observeEvent(input$add_sample, {
    site_id    <- input$sample_site_id
    date       <- input$sample_date
    whpt_ntaxa <- input$whpt_ntaxa
    whpt_aspt  <- input$whpt_aspt
    life       <- input$life

    if (is.null(site_id) || site_id == "") {
      showNotification("No sites available - add a site first.", type = "error")
      return()
    }
    if (!(site_id %in% sites()$SITE_ID)) {
      showNotification("Selected site is not in the site list.", type = "error")
      return()
    }
    if (is.null(date) || is.na(date)) {
      showNotification("Please provide a sample date.", type = "error")
      return()
    }
    if (is.na(whpt_ntaxa) || is.na(whpt_aspt)) {
      showNotification("Please provide both WHPT_NTAXA and WHPT_ASPT values.", type = "error")
      return()
    }

    updated <- rbind(
      samples(),
      data.frame(
        SITE_ID = site_id, SAMPLE_DATE = as.character(date),
        WHPT_NTAXA = whpt_ntaxa, WHPT_ASPT = whpt_aspt, LIFE = life,
        DATE_ADDED = now_stamp()
      )
    )
    samples(updated)
    save_csv(updated, samples_csv_path)

    updateNumericInput(session, "life", value = NA)
    updateNumericInput(session, "whpt_ntaxa", value = NA)
    updateNumericInput(session, "whpt_aspt", value = NA)

    showNotification(paste0("Saved sample for '", site_id, "'."), type = "message")
  })

  # Redraw site markers whenever sites or samples change, without
  # re-rendering the (expensive) catchment polygon layer.
  observe({
    proxy <- leafletProxy("map") |> clearMarkers()

    sites_sf <- sites_to_wgs84(sites())
    if (!is.null(sites_sf)) {
      summary_df <- summarise_samples(samples())
      merged <- merge(sites_sf, summary_df, by = "SITE_ID", all.x = TRUE)
      merged$N_SAMPLES[is.na(merged$N_SAMPLES)] <- 0L
      last_date_label <- ifelse(
        is.na(merged$LAST_DATE), "None", format(merged$LAST_DATE, "%Y-%m-%d")
      )

      coords <- st_coordinates(merged)
      hover_labels <- lapply(
        paste0(
          "<b>", merged$SITE_ID, "</b><br>",
          "Samples: ", merged$N_SAMPLES, "<br>",
          "Last sample: ", last_date_label
        ),
        HTML
      )

      proxy |>
        addCircleMarkers(
          lng = coords[, "X"],
          lat = coords[, "Y"],
          radius = 6,
          color = "#d95f0e",
          fillOpacity = 0.9,
          label = hover_labels,
          popup = paste0(
            "<b>", merged$SITE_ID, "</b><br>",
            "Easting: ", merged$EASTING, "<br>",
            "Northing: ", merged$NORTHING
          )
        )
    }
  })

  output$sites_table <- renderDT({
    sites()
  }, options = list(pageLength = 5, scrollX = TRUE), rownames = FALSE)

  output$samples_table <- renderDT({
    samples()
  }, options = list(pageLength = 5), rownames = FALSE)

  # Both downloads honour the same SITE_ID/OPERATIONAL_CATCHMENT/
  # RIVER_BASIN_DISTRICT/SITE_SOURCE filters; leaving them all empty
  # exports every site/sample. The date range additionally restricts the
  # Samples export.
  output$download_sites <- downloadHandler(
    filename = function() "sites.csv",
    content = function(file) {
      filtered <- filter_sites_df(sites(), input$dl_site_id, input$dl_catchment, input$dl_rbd, input$dl_site_source)
      write.csv(filtered, file, row.names = FALSE)
    }
  )

  output$download_samples <- downloadHandler(
    filename = function() "samples.csv",
    content = function(file) {
      allowed_sites <- filter_sites_df(sites(), input$dl_site_id, input$dl_catchment, input$dl_rbd, input$dl_site_source)$SITE_ID
      filtered <- filter_samples_df(samples(), allowed_sites, input$dl_date_range[1], input$dl_date_range[2])
      write.csv(filtered, file, row.names = FALSE)
    }
  )

  # Combined export: every site and sample within the shapefile area
  # currently selected on the map (any level - RBD, management catchment,
  # operational catchment, or EA area; "All" includes everything), written
  # into a single Excel file with Sites/Samples sheets. Sites are only
  # tagged with OPERATIONAL_CATCHMENT, so coarser/other-level selections
  # are resolved down to the operational catchments they spatially
  # overlap via selected_op_cats().
  output$download_catchment_combined <- downloadHandler(
    filename = function() {
      area_label <- if (identical(input$catchment_filter, "All")) "all_areas" else input$catchment_filter
      paste0("sites_and_samples_", gsub("[^A-Za-z0-9]+", "_", area_label), ".xlsx")
    },
    content = function(file) {
      catchment_sites <- if (identical(input$catchment_filter, "All")) {
        sites()
      } else {
        allowed_op_cats <- unique(selected_op_cats()$operationa)
        sites()[sites()$OPERATIONAL_CATCHMENT %in% allowed_op_cats, ]
      }
      catchment_samples <- samples()[samples()$SITE_ID %in% catchment_sites$SITE_ID, ]

      write_xlsx(list(Sites = catchment_sites, Samples = catchment_samples), path = file)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)
