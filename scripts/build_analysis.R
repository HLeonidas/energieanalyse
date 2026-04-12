#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(jsonlite))

get_script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) > 0) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg[1]), winslash = "/", mustWork = TRUE)))
  }
  normalizePath(".", winslash = "/", mustWork = TRUE)
}

de_num <- function(x) {
  x <- trimws(as.character(x))
  x[x == ""] <- NA_character_
  as.numeric(sub(",", ".", x, fixed = TRUE))
}

safe_sum <- function(x) {
  if (all(is.na(x))) {
    return(NA_real_)
  }
  sum(x, na.rm = TRUE)
}

safe_max <- function(x) {
  if (all(is.na(x))) {
    return(NA_real_)
  }
  max(x, na.rm = TRUE)
}

safe_quantile <- function(x, prob) {
  y <- x[!is.na(x)]
  if (!length(y)) {
    return(NA_real_)
  }
  unname(as.numeric(stats::quantile(y, probs = prob, names = FALSE, type = 7)))
}

safe_divide <- function(num, den) {
  ifelse(is.na(num) | is.na(den) | den == 0, NA_real_, num / den)
}

rolling_mean <- function(x, window = 7L) {
  out <- rep(NA_real_, length(x))
  for (i in seq_along(x)) {
    from_idx <- max(1L, i - window + 1L)
    window_values <- x[from_idx:i]
    window_values <- window_values[!is.na(window_values)]
    if (length(window_values)) {
      out[i] <- mean(window_values)
    }
  }
  out
}

cor_or_na <- function(x, y) {
  keep <- !is.na(x) & !is.na(y)
  if (sum(keep) < 3) {
    return(NA_real_)
  }
  unname(cor(x[keep], y[keep]))
}

to_point_pairs <- function(x, y) {
  keep <- !is.na(x) & !is.na(y)
  unname(Map(function(a, b) list(a, b), as.numeric(x[keep]), as.numeric(y[keep])))
}

parse_header_kv <- function(lines) {
  meta <- list()
  for (line in lines) {
    parts <- strsplit(line, ";", fixed = TRUE)[[1]]
    if (length(parts) >= 2) {
      key <- gsub("[^a-z0-9]+", "_", tolower(parts[1]))
      meta[[key]] <- parts[2]
    }
  }
  meta
}

load_meldewesen <- function(directory) {
  files <- sort(list.files(directory, pattern = "\\.json$", full.names = TRUE))
  frames <- lapply(files, function(path) {
    obj <- fromJSON(path)
    data.frame(
      date = as.Date(obj$result$m_GroupID, format = "%d.%m.%Y"),
      arrivals = as.numeric(obj$result$m_Arrivals),
      nights = as.numeric(obj$result$m_Nights),
      arrivals_compare = as.numeric(obj$result$c_Arrivals),
      nights_compare = as.numeric(obj$result$c_Nights),
      stringsAsFactors = FALSE
    )
  })
  df <- do.call(rbind, frames)
  df[order(df$date), ]
}

load_fronius <- function(directory) {
  files <- sort(list.files(directory, pattern = "\\.json$", full.names = TRUE))
  frames <- lapply(files, function(path) {
    obj <- fromJSON(path)
    if (is.data.frame(obj$settings$series)) {
      points <- obj$settings$series$data[[1]]
    } else {
      points <- obj$settings$series[[1]]$data
    }
    if (is.data.frame(points) || is.matrix(points)) {
      ts_ms <- as.numeric(points[, 1])
      values <- as.numeric(points[, 2])
    } else {
      ts_ms <- vapply(points, function(row) as.numeric(row[[1]]), numeric(1))
      values <- vapply(points, function(row) as.numeric(row[[2]]), numeric(1))
    }
    data.frame(
      date = as.Date(as.POSIXct(ts_ms / 1000, origin = "1970-01-01", tz = "UTC")),
      pv_kwh = values,
      stringsAsFactors = FALSE
    )
  })
  df <- do.call(rbind, frames)
  aggregate(pv_kwh ~ date, data = df, FUN = safe_sum)
}

load_fernwaerme <- function(directory) {
  path <- sort(list.files(directory, pattern = "\\.csv$", full.names = TRUE))[1]
  lines <- readLines(path, warn = FALSE, encoding = "CP1252")
  lines <- iconv(lines, from = "CP1252", to = "UTF-8")

  parse_monthly_row <- function(parts) {
    data.frame(
      month = parts[2],
      heat_export_kwh = de_num(parts[3]),
      heat_export_kwh_prev = de_num(parts[4]),
      heat_export_volume_m3 = de_num(parts[5]),
      heat_export_volume_per_mwh_m3 = de_num(parts[6]),
      stringsAsFactors = FALSE
    )
  }

  monthly_rows <- list()
  daily_rows <- list()
  in_monthly <- FALSE
  in_daily <- FALSE

  for (line in lines) {
    if (grepl("^Monatssummen;[0-9]{4}-[0-9]{2};", line)) {
      parts <- strsplit(line, ";", fixed = TRUE)[[1]]
      length(parts) <- max(length(parts), 8L)
      parts[is.na(parts)] <- ""
      monthly_rows[[length(monthly_rows) + 1L]] <- parse_monthly_row(parts)
      in_monthly <- TRUE
      in_daily <- FALSE
      next
    }

    if (startsWith(line, "Monatssummen;")) {
      in_monthly <- TRUE
      in_daily <- FALSE
      next
    }

    if (startsWith(line, "Zeitpunkt;")) {
      in_daily <- grepl("Z", line, fixed = TRUE)
      in_monthly <- FALSE
      next
    }

    parts <- strsplit(line, ";", fixed = TRUE)[[1]]
    length(parts) <- max(length(parts), 8L)
    parts[is.na(parts)] <- ""

    if (in_monthly && grepl("^;[0-9]{4}-[0-9]{2};", line)) {
      monthly_rows[[length(monthly_rows) + 1L]] <- parse_monthly_row(parts)
    }

    if (in_daily && grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2};", line)) {
      daily_rows[[length(daily_rows) + 1L]] <- data.frame(
        date = as.Date(parts[1]),
        heat_meter_kwh = de_num(parts[2]),
        heat_meter_m3 = de_num(parts[3]),
        heat_kwh = de_num(parts[4]),
        heat_kwh_prev = de_num(parts[5]),
        heat_volume_m3 = de_num(parts[6]),
        heat_volume_per_mwh_m3 = de_num(parts[7]),
        heat_approximated_flag = ifelse(toupper(trimws(parts[8])) == "JA", 1L, 0L),
        stringsAsFactors = FALSE
      )
    }
  }

  monthly <- do.call(rbind, monthly_rows)
  monthly <- monthly[grepl("^2025-", monthly$month), ]
  monthly <- monthly[order(monthly$month), ]

  daily <- do.call(rbind, daily_rows)
  daily <- daily[
    daily$date >= as.Date("2025-01-01") &
      daily$date <= as.Date("2025-12-31") &
      (!is.na(daily$heat_kwh) | !is.na(daily$heat_meter_kwh)),
  ]
  daily <- daily[order(daily$date), ]

  list(path = path, daily = daily, monthly = monthly)
}

load_strom <- function(directory) {
  path <- sort(list.files(directory, pattern = "\\.csv$", full.names = TRUE))[1]
  header_lines <- readLines(path, warn = FALSE, encoding = "UTF-8", n = 7)
  meta <- parse_header_kv(header_lines[1:6])

  raw <- read.csv(
    path,
    sep = ";",
    skip = 7,
    stringsAsFactors = FALSE,
    encoding = "UTF-8",
    na.strings = c("")
  )

  raw <- raw[nzchar(raw$Datum), , drop = FALSE]
  raw$timestamp <- as.POSIXct(
    paste(raw$Datum, raw$Zeit),
    format = "%d.%m.%Y %H:%M:%S",
    tz = "Europe/Vienna"
  )
  raw$date <- as.Date(raw$timestamp)
  raw$hour <- as.integer(format(raw$timestamp, "%H"))
  raw$kwh <- de_num(raw$kWh)
  raw$status_text <- raw$Status
  raw$status_class <- ifelse(grepl("Ersatzwert", raw$Status, fixed = TRUE), "replacement", "measured")
  raw$timestamp_ms <- as.numeric(raw$timestamp) * 1000
  raw$timestamp_iso <- format(raw$timestamp, "%Y-%m-%dT%H:%M:%S%z")

  groups <- split(raw, raw$date)
  daily <- do.call(rbind, lapply(groups, function(day_rows) {
    data.frame(
      date = unique(day_rows$date)[1],
      electricity_kwh = safe_sum(day_rows$kwh),
      electricity_peak_kw = safe_max(day_rows$kwh) * 4,
      electricity_baseload_kw_p10 = safe_quantile(day_rows$kwh, 0.10) * 4,
      electricity_intervals = nrow(day_rows),
      electricity_replacement_intervals = sum(day_rows$status_class == "replacement", na.rm = TRUE),
      electricity_replacement_share = safe_divide(
        sum(day_rows$status_class == "replacement", na.rm = TRUE),
        nrow(day_rows)
      ),
      electricity_night_kwh = safe_sum(day_rows$kwh[day_rows$hour < 6]),
      electricity_daytime_kwh = safe_sum(day_rows$kwh[day_rows$hour >= 8 & day_rows$hour < 20]),
      stringsAsFactors = FALSE
    )
  }))
  daily <- daily[order(daily$date), ]

  raw_out <- raw[, c("timestamp_iso", "timestamp_ms", "date", "Datum", "Zeit", "kwh", "status_text", "status_class")]
  names(raw_out) <- c("timestamp_iso", "timestamp_ms", "date", "date_local", "time_local", "kwh", "status", "status_class")

  list(path = path, meta = meta, raw = raw_out, daily = daily)
}

build_monthly_dataset <- function(raw_daily, heat_monthly_export) {
  month_keys <- sort(unique(format(raw_daily$date, "%Y-%m")))
  records <- lapply(month_keys, function(month_key) {
    month_rows <- raw_daily[format(raw_daily$date, "%Y-%m") == month_key, ]
    baseload_values <- month_rows$electricity_baseload_kw_p10[!is.na(month_rows$electricity_baseload_kw_p10)]
    data.frame(
      month = month_key,
      month_start = as.Date(paste0(month_key, "-01")),
      days_in_month = nrow(month_rows),
      electricity_kwh = safe_sum(month_rows$electricity_kwh),
      electricity_peak_kw = safe_max(month_rows$electricity_peak_kw),
      electricity_baseload_kw_p10 = ifelse(length(baseload_values), mean(baseload_values), NA_real_),
      electricity_intervals = safe_sum(month_rows$electricity_intervals),
      electricity_replacement_intervals = safe_sum(month_rows$electricity_replacement_intervals),
      electricity_replacement_share = safe_divide(
        safe_sum(month_rows$electricity_replacement_intervals),
        safe_sum(month_rows$electricity_intervals)
      ),
      electricity_night_kwh = safe_sum(month_rows$electricity_night_kwh),
      electricity_daytime_kwh = safe_sum(month_rows$electricity_daytime_kwh),
      heat_kwh = safe_sum(month_rows$heat_kwh),
      heat_volume_m3 = safe_sum(month_rows$heat_volume_m3),
      heat_approximated_days = safe_sum(month_rows$heat_approximated_flag),
      pv_kwh = safe_sum(month_rows$pv_kwh),
      pv_days_available = sum(!is.na(month_rows$pv_kwh)),
      arrivals = safe_sum(month_rows$arrivals),
      nights = safe_sum(month_rows$nights),
      days_with_guests = sum(!is.na(month_rows$nights) & month_rows$nights > 0),
      pv_missing_days = sum(is.na(month_rows$pv_kwh)),
      stringsAsFactors = FALSE
    )
  })

  monthly <- do.call(rbind, records)
  monthly <- merge(monthly, heat_monthly_export, by = "month", all.x = TRUE)
  monthly$month_start_ms <- as.numeric(as.POSIXct(monthly$month_start, tz = "Europe/Vienna")) * 1000
  monthly <- monthly[order(monthly$month_start), ]
  monthly
}

script_dir <- get_script_dir()
project_root <- normalizePath(file.path(script_dir, ".."), winslash = "/", mustWork = TRUE)
data_dir <- file.path(project_root, "data")
output_dir <- file.path(project_root, "output", "energy-analysis")
template_path <- file.path(project_root, "templates", "highcharts_dashboard.html")

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

meldewesen <- load_meldewesen(file.path(data_dir, "meldewesen"))
fronius <- load_fronius(file.path(data_dir, "fronius"))
fernwaerme <- load_fernwaerme(file.path(data_dir, "fernwaerme"))
strom <- load_strom(file.path(data_dir, "strom"))

date_spine <- data.frame(
  date = seq.Date(as.Date("2025-01-01"), as.Date("2025-12-31"), by = "day"),
  stringsAsFactors = FALSE
)

raw_daily <- Reduce(
  function(x, y) merge(x, y, by = "date", all.x = TRUE),
  list(date_spine, strom$daily, fernwaerme$daily, fronius, meldewesen)
)

raw_daily <- raw_daily[order(raw_daily$date), ]
raw_daily$timestamp_ms <- as.numeric(as.POSIXct(raw_daily$date, tz = "Europe/Vienna")) * 1000
raw_daily$has_guests_flag <- ifelse(!is.na(raw_daily$nights) & raw_daily$nights > 0, 1L, 0L)
raw_daily$electricity_data_available <- ifelse(is.na(raw_daily$electricity_kwh), 0L, 1L)
raw_daily$heat_data_available <- ifelse(is.na(raw_daily$heat_kwh), 0L, 1L)
raw_daily$pv_data_available <- ifelse(is.na(raw_daily$pv_kwh), 0L, 1L)
raw_daily$occupancy_data_available <- ifelse(is.na(raw_daily$nights), 0L, 1L)

analysis_daily <- raw_daily
analysis_daily$electricity_minus_pv_kwh <- ifelse(
  is.na(analysis_daily$electricity_kwh) | is.na(analysis_daily$pv_kwh),
  NA_real_,
  analysis_daily$electricity_kwh - analysis_daily$pv_kwh
)
analysis_daily$grid_need_after_pv_kwh <- ifelse(
  is.na(analysis_daily$electricity_minus_pv_kwh),
  NA_real_,
  pmax(analysis_daily$electricity_minus_pv_kwh, 0)
)
analysis_daily$pv_surplus_vs_load_kwh <- ifelse(
  is.na(analysis_daily$electricity_minus_pv_kwh),
  NA_real_,
  pmax(-analysis_daily$electricity_minus_pv_kwh, 0)
)
analysis_daily$pv_generation_to_load_ratio <- safe_divide(
  analysis_daily$pv_kwh,
  analysis_daily$electricity_kwh
)
analysis_daily$potential_pv_coverage_ratio <- safe_divide(
  pmin(analysis_daily$pv_kwh, analysis_daily$electricity_kwh),
  analysis_daily$electricity_kwh
)
analysis_daily$electricity_kwh_per_night <- safe_divide(
  analysis_daily$electricity_kwh,
  analysis_daily$nights
)
analysis_daily$heat_kwh_per_night <- safe_divide(
  analysis_daily$heat_kwh,
  analysis_daily$nights
)
analysis_daily$pv_kwh_per_night <- safe_divide(
  analysis_daily$pv_kwh,
  analysis_daily$nights
)
analysis_daily$electricity_kwh_per_arrival <- safe_divide(
  analysis_daily$electricity_kwh,
  analysis_daily$arrivals
)
analysis_daily$heat_kwh_per_arrival <- safe_divide(
  analysis_daily$heat_kwh,
  analysis_daily$arrivals
)
analysis_daily$heat_to_electricity_ratio <- safe_divide(
  analysis_daily$heat_kwh,
  analysis_daily$electricity_kwh
)
analysis_daily$electricity_7d_avg_kwh <- rolling_mean(analysis_daily$electricity_kwh, 7L)
analysis_daily$heat_7d_avg_kwh <- rolling_mean(analysis_daily$heat_kwh, 7L)
analysis_daily$pv_7d_avg_kwh <- rolling_mean(analysis_daily$pv_kwh, 7L)
analysis_daily$nights_7d_avg <- rolling_mean(analysis_daily$nights, 7L)

raw_monthly <- build_monthly_dataset(raw_daily, fernwaerme$monthly)
raw_monthly$heat_delta_vs_export_kwh <- raw_monthly$heat_kwh - raw_monthly$heat_export_kwh

analysis_monthly <- raw_monthly
analysis_monthly$electricity_minus_pv_kwh <- ifelse(
  is.na(analysis_monthly$electricity_kwh) | is.na(analysis_monthly$pv_kwh),
  NA_real_,
  analysis_monthly$electricity_kwh - analysis_monthly$pv_kwh
)
analysis_monthly$grid_need_after_pv_kwh <- ifelse(
  is.na(analysis_monthly$electricity_minus_pv_kwh),
  NA_real_,
  pmax(analysis_monthly$electricity_minus_pv_kwh, 0)
)
analysis_monthly$pv_surplus_vs_load_kwh <- ifelse(
  is.na(analysis_monthly$electricity_minus_pv_kwh),
  NA_real_,
  pmax(-analysis_monthly$electricity_minus_pv_kwh, 0)
)
analysis_monthly$pv_generation_to_load_ratio <- safe_divide(
  analysis_monthly$pv_kwh,
  analysis_monthly$electricity_kwh
)
analysis_monthly$potential_pv_coverage_ratio <- safe_divide(
  pmin(analysis_monthly$pv_kwh, analysis_monthly$electricity_kwh),
  analysis_monthly$electricity_kwh
)
analysis_monthly$electricity_kwh_per_night <- safe_divide(
  analysis_monthly$electricity_kwh,
  analysis_monthly$nights
)
analysis_monthly$heat_kwh_per_night <- safe_divide(
  analysis_monthly$heat_kwh,
  analysis_monthly$nights
)
analysis_monthly$pv_kwh_per_night <- safe_divide(
  analysis_monthly$pv_kwh,
  analysis_monthly$nights
)

pv_missing_days <- as.character(raw_daily$date[is.na(raw_daily$pv_kwh)])
heat_missing_days <- as.character(raw_daily$date[is.na(raw_daily$heat_kwh)])
occupancy_missing_days <- as.character(raw_daily$date[is.na(raw_daily$nights)])
electricity_missing_days <- as.character(raw_daily$date[is.na(raw_daily$electricity_kwh)])

metadata <- list(
  generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
  timezone = "Europe/Vienna",
  source_files = list(
    strom = normalizePath(strom$path, winslash = "/", mustWork = TRUE),
    fernwaerme = normalizePath(fernwaerme$path, winslash = "/", mustWork = TRUE),
    fronius = normalizePath(file.path(data_dir, "fronius"), winslash = "/", mustWork = TRUE),
    meldewesen = normalizePath(file.path(data_dir, "meldewesen"), winslash = "/", mustWork = TRUE)
  ),
  source_metadata = list(
    strom = strom$meta
  ),
  quality = list(
    coverage_start = "2025-01-01",
    coverage_end = "2025-12-31",
    row_counts = list(
      strom_quarter_hour = nrow(strom$raw),
      raw_daily = nrow(raw_daily),
      analysis_daily = nrow(analysis_daily),
      raw_monthly = nrow(raw_monthly),
      analysis_monthly = nrow(analysis_monthly)
    ),
    missing_days = list(
      electricity = electricity_missing_days,
      fernwaerme = heat_missing_days,
      pv = pv_missing_days,
      occupancy = occupancy_missing_days
    )
  ),
  summary = list(
    total_electricity_kwh = safe_sum(raw_daily$electricity_kwh),
    total_heat_kwh = safe_sum(raw_daily$heat_kwh),
    total_heat_volume_m3 = safe_sum(raw_daily$heat_volume_m3),
    total_pv_kwh_known_days = safe_sum(raw_daily$pv_kwh),
    pv_days_available = sum(!is.na(raw_daily$pv_kwh)),
    total_arrivals = safe_sum(raw_daily$arrivals),
    total_nights = safe_sum(raw_daily$nights),
    electricity_replacement_intervals = safe_sum(raw_daily$electricity_replacement_intervals),
    electricity_replacement_share = safe_divide(
      safe_sum(raw_daily$electricity_replacement_intervals),
      safe_sum(raw_daily$electricity_intervals)
    ),
    correlation_nights_vs_electricity = cor_or_na(analysis_daily$nights, analysis_daily$electricity_kwh),
    correlation_nights_vs_heat = cor_or_na(analysis_daily$nights, analysis_daily$heat_kwh),
    correlation_electricity_vs_pv = cor_or_na(analysis_daily$electricity_kwh, analysis_daily$pv_kwh),
    max_abs_monthly_heat_delta_kwh = safe_max(abs(raw_monthly$heat_delta_vs_export_kwh))
  ),
  notes = c(
    "Potential PV coverage is calculated on day and month level only. It is not a true self-consumption metric.",
    "Fronius data are missing for 2025-11-21, 2025-11-22, 2025-11-24 and 2025-12-25. Missing values stay NA and are not imputed.",
    "Fernwaerme export includes a closing meter row for 2026-01-01. This row is excluded from the 2025 day-level dataset.",
    "Strom timestamps are parsed in Europe/Vienna and include DST transitions with 92 and 100 quarter-hour intervals on switch days."
  )
)

write.csv(
  strom$raw,
  file.path(output_dir, "raw_strom_quarter_hour.csv"),
  row.names = FALSE
)
write.csv(
  raw_daily,
  file.path(output_dir, "raw_daily_sources.csv"),
  row.names = FALSE
)
write.csv(
  raw_monthly,
  file.path(output_dir, "raw_monthly_sources.csv"),
  row.names = FALSE
)
write.csv(
  analysis_daily,
  file.path(output_dir, "analysis_daily.csv"),
  row.names = FALSE
)
write.csv(
  analysis_monthly,
  file.path(output_dir, "analysis_monthly.csv"),
  row.names = FALSE
)

write_json(
  metadata,
  path = file.path(output_dir, "metadata.json"),
  pretty = TRUE,
  auto_unbox = TRUE,
  null = "null",
  na = "null"
)

dashboard_data <- list(
  metadata = metadata,
  raw = list(
    electricity_quarter_hour = list(
      points = to_point_pairs(strom$raw$timestamp_ms, strom$raw$kwh),
      replacement_points = to_point_pairs(
        strom$raw$timestamp_ms[strom$raw$status_class == "replacement"],
        strom$raw$kwh[strom$raw$status_class == "replacement"]
      )
    ),
    daily = raw_daily,
    monthly = raw_monthly
  ),
  analysis = list(
    daily = analysis_daily,
    monthly = analysis_monthly
  )
)

dashboard_js <- paste0(
  "window.energyAnalysisData = ",
  toJSON(dashboard_data, dataframe = "rows", auto_unbox = TRUE, na = "null", digits = 8),
  ";"
)
writeLines(dashboard_js, con = file.path(output_dir, "dashboard_data.js"), useBytes = TRUE)

invisible(file.copy(template_path, file.path(output_dir, "dashboard.html"), overwrite = TRUE))

cat("Analysis build complete\n")
cat("Output directory:", output_dir, "\n")
cat("Generated files:\n")
for (path in list.files(output_dir, full.names = TRUE)) {
  cat(" -", basename(path), "\n")
}
