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

catchment_rds   <- "1_Data/shapefiles/op_cat/op_cat_simplified.rds"
sites_csv_path  <- "1_Data/biological_data/sites.csv"
samples_csv_path <- "1_Data/biological_data/samples.csv"
# Every time a saved site's data is overwritten, its previous values are
# appended here (never deleted/overwritten themselves) so history is kept.
site_changelog_csv_path <- "1_Data/biological_data/sites_changelog.csv"

bng_crs <- 27700  # British National Grid (Easting/Northing)
wgs_crs <- 4326   # lat/lon, required by leaflet

# ---- Data ---------------------------------------------------------------

# Pre-simplified/reprojected catchment polygons (see data-prep step run
# once against the full WFD shapefile) for fast, responsive rendering.
op_cat <- readRDS(catchment_rds)

# Full Site sheet schema (matches the lab's existing site spreadsheet;
# column names are all-caps to match that convention). Only SITE_ID/
# EASTING/NORTHING are required for a site to be usable on the map - the
# rest are optional environmental/classification predictors, each exposed
# as its own input in the manual "Add new site" form.
site_field_defs <- list(
  list(id = "site_source",            col = "SITE_SOURCE",            type = "text"),
  list(id = "river_basin_district",   col = "RIVER_BASIN_DISTRICT",   type = "select", choices_from = "river_basi"),
  list(id = "operational_catchment",  col = "OPERATIONAL_CATCHMENT",  type = "select", choices_from = "operationa"),
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
  "RIVER_BASIN_DISTRICT", "OPERATIONAL_CATCHMENT",
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
  "OPERATIONAL_CATCHMENT", "ALTITUDE", "SLOPE", "DISTANCE_FROM_SOURCE",
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

full_sample_cols <- c("SITE_ID", "DATE", "WHPT_NTAXA", "WHPT_ASPT", "DATE_ADDED")
template_sample_cols <- setdiff(full_sample_cols, "DATE_ADDED")

empty_samples <- data.frame(
  SITE_ID    = character(),
  DATE       = character(),
  WHPT_NTAXA = double(),
  WHPT_ASPT  = double(),
  DATE_ADDED = character(),
  stringsAsFactors = FALSE
)

now_stamp <- function() format(Sys.time(), "%Y-%m-%d %H:%M:%S")

# Before a saved site's data is overwritten, append its current values to
# the (append-only) change log so nothing is ever silently lost.
log_site_changes <- function(old_rows, change_type) {
  if (nrow(old_rows) == 0) return(invisible())
  old_rows$CHANGE_TIMESTAMP <- now_stamp()
  old_rows$CHANGE_TYPE <- change_type
  dir.create(dirname(site_changelog_csv_path), recursive = TRUE, showWarnings = FALSE)
  write.table(
    old_rows, site_changelog_csv_path, sep = ",", row.names = FALSE,
    col.names = !file.exists(site_changelog_csv_path),
    append = file.exists(site_changelog_csv_path)
  )
}

# Replace one or more existing sites (matched by SITE_ID) with new data.
# The previous values are logged first via log_site_changes(), and
# DATE_ADDED is carried over from the row being replaced so it always
# reflects when the site was first created; DATE_CHANGED is stamped now.
overwrite_sites <- function(current, replacement, change_type = "SITE_OVERWRITTEN") {
  old_rows <- current[current$SITE_ID %in% replacement$SITE_ID, ]
  log_site_changes(old_rows, change_type)
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

# Filter a Sites data frame down to the given SITE_ID/OPERATIONAL_CATCHMENT/
# RIVER_BASIN_DISTRICT values. An empty selection for any field means "no
# filter on that field" (i.e. include every value).
filter_sites_df <- function(df, site_ids, catchments, rbds) {
  if (length(site_ids) > 0) df <- df[df$SITE_ID %in% site_ids, ]
  if (length(catchments) > 0) df <- df[df$OPERATIONAL_CATCHMENT %in% catchments, ]
  if (length(rbds) > 0) df <- df[df$RIVER_BASIN_DISTRICT %in% rbds, ]
  df
}

# Filter a Samples data frame to only the given sites and a DATE range.
filter_samples_df <- function(samples_df, allowed_site_ids, date_start, date_end) {
  df <- samples_df[samples_df$SITE_ID %in% allowed_site_ids, ]
  d <- as.Date(df$DATE)
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
  d <- as.Date(samples$DATE)
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
    "Sites sheet optional columns: SITE_SOURCE, RIVER_BASIN_DISTRICT, OPERATIONAL_CATCHMENT,",
    "ALTITUDE, SLOPE, DISTANCE_FROM_SOURCE, DISCHARGE_CATEGORY, WIDTH, DEPTH, BOULDER_COBBLES,",
    "PEBBLES_GRAVEL, SAND, SILT_CLAY, ALKALINITY, CONDUCTIVITY, TOTAL_HARDNESS, CALCIUM.",
    "Leave optional columns blank if unknown - only SITE_ID/EASTING/NORTHING are required.",
    "Do not add DATE_ADDED/DATE_CHANGED columns - the app fills these in automatically.",
    "",
    "Samples sheet columns: SITE_ID, DATE (YYYY-MM-DD), WHPT_NTAXA, WHPT_ASPT.",
    "Every SITE_ID used in the Samples sheet must already exist - either",
    "already saved in the app, or included in the Sites sheet of this same file.",
    "",
    "By default, a Sites row whose SITE_ID already exists is skipped. Tick",
    "'Overwrite existing sites found in the template' before uploading to replace it",
    "instead - the previous values are kept in a change log file, never deleted.",
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
      choices = c("", sort(unique(op_cat[[field$choices_from]]))),
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
      checkboxInput(
        "overwrite_sites_upload",
        "Overwrite existing sites found in the template",
        value = FALSE
      ),
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
          dateInput("sample_date", "DATE", value = Sys.Date()),
          numericInput("whpt_ntaxa", "WHPT_NTAXA", value = NA, step = 1),
          numericInput("whpt_aspt", "WHPT_ASPT", value = NA, step = 0.01),
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
          dateRangeInput("dl_date_range", "Sample DATE range"),
          downloadButton("download_sites", "Download Sites (.csv)"),
          downloadButton("download_samples", "Download Samples (.csv)"),
          downloadButton("download_site_changelog", "Download site change log (.csv)")
        )
      )
    ),
    mainPanel(
      selectInput(
        "catchment_filter",
        "Operational catchment",
        choices = c("All", sort(unique(op_cat$operationa))),
        selected = "All"
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

  sites <- reactiveVal(ensure_cols(
    read_csv_or_default(sites_csv_path, empty_sites, col_classes = sites_col_classes),
    full_site_cols
  ))
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
  }, ignoreNULL = FALSE)

  # Keep the download date-range picker spanning the full range of saved
  # sample dates, so it includes everything by default.
  observeEvent(samples(), {
    d <- as.Date(samples()$DATE)
    d <- d[!is.na(d)]
    rng <- if (length(d) > 0) range(d) else c(Sys.Date(), Sys.Date())
    updateDateRangeInput(session, "dl_date_range", start = rng[1], end = rng[2], min = rng[1], max = rng[2])
  }, ignoreNULL = FALSE)

  # Base map: tiles only. Catchment polygons are drawn/redrawn separately
  # below so the (filterable) polygon layer doesn't require a full
  # re-render of the map widget.
  output$map <- renderLeaflet({
    leaflet() |>
      addProviderTiles(providers$CartoDB.Positron) |>
      fitBounds(
        st_bbox(op_cat)[["xmin"]], st_bbox(op_cat)[["ymin"]],
        st_bbox(op_cat)[["xmax"]], st_bbox(op_cat)[["ymax"]]
      )
  })

  # Redraw the catchment polygon layer when the filter changes: "All"
  # shows every operational catchment, otherwise just the selected one,
  # and the map zooms to fit whatever is shown.
  observeEvent(input$catchment_filter, {
    filtered <- if (identical(input$catchment_filter, "All")) {
      op_cat
    } else {
      op_cat[op_cat$operationa == input$catchment_filter, ]
    }

    proxy <- leafletProxy("map") |>
      clearGroup("catchments") |>
      addPolygons(
        data = filtered,
        group = "catchments",
        color = "#3182bd",
        weight = 1,
        fillOpacity = 0.05,
        label = ~operationa,
        popup = ~paste0(
          "<b>", operationa, "</b><br>",
          "Management catchment: ", management, "<br>",
          "River basin district: ", river_basi
        )
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

    required_sample_cols <- c("SITE_ID", "DATE", "WHPT_NTAXA", "WHPT_ASPT")
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

    well_formed <- !is.na(candidate_sites$SITE_ID) & candidate_sites$SITE_ID != "" &
      !is.na(candidate_sites$EASTING) & !is.na(candidate_sites$NORTHING)
    n_malformed_sites <- sum(!well_formed)
    candidate_sites <- candidate_sites[well_formed, ]
    candidate_sites <- candidate_sites[!duplicated(candidate_sites$SITE_ID), ]

    matches_existing <- candidate_sites$SITE_ID %in% current_sites$SITE_ID
    overwrite_enabled <- isTRUE(input$overwrite_sites_upload)
    sites_overwritten <- 0L

    if (overwrite_enabled) {
      to_overwrite <- candidate_sites[matches_existing, ]
      to_add       <- candidate_sites[!matches_existing, ]
      sites_overwritten <- nrow(to_overwrite)
      sites_skipped <- n_malformed_sites
      to_add$DATE_ADDED   <- now_stamp()
      to_add$DATE_CHANGED <- NA_character_

      updated_sites <- current_sites
      if (sites_overwritten > 0) {
        updated_sites <- overwrite_sites(updated_sites, to_overwrite, "SITE_OVERWRITTEN_IMPORT")
      }
      updated_sites <- bind_fill(updated_sites, to_add)
      sites_added <- nrow(to_add)
    } else {
      sites_skipped <- n_malformed_sites + sum(matches_existing)
      candidate_sites <- candidate_sites[!matches_existing, ]
      candidate_sites$DATE_ADDED   <- now_stamp()
      candidate_sites$DATE_CHANGED <- NA_character_
      sites_added <- nrow(candidate_sites)
      updated_sites <- bind_fill(current_sites, candidate_sites)
    }

    if (sites_added > 0 || sites_overwritten > 0) {
      sites(updated_sites)
      save_csv(updated_sites, sites_csv_path)
    }

    # ---- Samples: SITE_ID must exist among current + newly-added sites.
    known_site_ids <- updated_sites$SITE_ID
    candidate_samples <- data.frame(
      SITE_ID    = trimws(as.character(new_samples_raw$SITE_ID)),
      DATE       = parse_dates(new_samples_raw$DATE),
      WHPT_NTAXA = suppressWarnings(as.numeric(new_samples_raw$WHPT_NTAXA)),
      WHPT_ASPT  = suppressWarnings(as.numeric(new_samples_raw$WHPT_ASPT)),
      DATE_ADDED = now_stamp(),
      stringsAsFactors = FALSE
    )
    valid <- !is.na(candidate_samples$SITE_ID) & candidate_samples$SITE_ID != "" &
      !is.na(candidate_samples$DATE) &
      !is.na(candidate_samples$WHPT_NTAXA) & !is.na(candidate_samples$WHPT_ASPT) &
      candidate_samples$SITE_ID %in% known_site_ids
    samples_skipped <- sum(!valid)
    candidate_samples <- candidate_samples[valid, ]
    candidate_samples$DATE <- as.character(candidate_samples$DATE)
    samples_added <- nrow(candidate_samples)

    if (samples_added > 0) {
      updated_samples <- rbind(samples(), candidate_samples)
      samples(updated_samples)
      save_csv(updated_samples, samples_csv_path)
    }

    showNotification(
      paste0(
        "Import complete. Sites added: ", sites_added,
        if (overwrite_enabled) paste0(", overwritten: ", sites_overwritten) else "",
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

    if (is_existing) {
      updated <- overwrite_sites(current, new_row)
      showNotification(
        paste0("Overwrote site '", site_id, "'. Previous values were saved to the change log."),
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
        SITE_ID = site_id, DATE = as.character(date),
        WHPT_NTAXA = whpt_ntaxa, WHPT_ASPT = whpt_aspt
      )
    )
    samples(updated)
    save_csv(updated, samples_csv_path)

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
  # RIVER_BASIN_DISTRICT filters; leaving them all empty exports every
  # site/sample. The date range additionally restricts the Samples export.
  output$download_sites <- downloadHandler(
    filename = function() "sites.csv",
    content = function(file) {
      filtered <- filter_sites_df(sites(), input$dl_site_id, input$dl_catchment, input$dl_rbd)
      write.csv(filtered, file, row.names = FALSE)
    }
  )

  output$download_samples <- downloadHandler(
    filename = function() "samples.csv",
    content = function(file) {
      allowed_sites <- filter_sites_df(sites(), input$dl_site_id, input$dl_catchment, input$dl_rbd)$SITE_ID
      filtered <- filter_samples_df(samples(), allowed_sites, input$dl_date_range[1], input$dl_date_range[2])
      write.csv(filtered, file, row.names = FALSE)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)
