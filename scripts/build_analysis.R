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

safe_mean <- function(x) {
  y <- x[!is.na(x)]
  if (!length(y)) {
    return(NA_real_)
  }
  mean(y)
}

safe_min <- function(x) {
  y <- x[!is.na(x)]
  if (!length(y)) {
    return(NA_real_)
  }
  min(y)
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

safe_coef <- function(model, term) {
  if (is.null(model)) {
    return(NA_real_)
  }
  coefficients <- coef(model)
  if (!(term %in% names(coefficients))) {
    return(NA_real_)
  }
  unname(as.numeric(coefficients[[term]]))
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

lag_vec <- function(x) {
  if (!length(x)) {
    return(x)
  }
  c(NA, head(x, -1))
}

season_cluster_from_date <- function(date) {
  month <- as.integer(format(date, "%m"))
  ifelse(
    month %in% c(11L, 12L, 1L, 2L, 3L),
    "winter",
    ifelse(month %in% c(6L, 7L, 8L), "summer", "transition")
  )
}

meteorological_season_from_date <- function(date) {
  month <- as.integer(format(date, "%m"))
  ifelse(
    month %in% c(12L, 1L, 2L),
    "winter",
    ifelse(
      month %in% c(3L, 4L, 5L),
      "spring",
      ifelse(month %in% c(6L, 7L, 8L), "summer", "autumn")
    )
  )
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

load_wetter <- function(directory) {
  candidates <- sort(list.files(directory, pattern = "Messstationen Tagesdaten.*\\.csv$", full.names = TRUE))
  if (!length(candidates)) {
    stop("No daily weather file found under data/wetter")
  }

  path <- candidates[1]
  raw <- read.csv(path, stringsAsFactors = FALSE, encoding = "UTF-8")

  raw$date <- as.Date(sub("T.*", "", raw$time))
  raw$weather_station_id <- as.character(raw$station)
  raw$temp_mean_c <- as.numeric(raw$tl_mittel)
  raw$temp_max_c <- as.numeric(raw$tlmax)
  raw$temp_min_c <- as.numeric(raw$tlmin)
  raw$precipitation_raw_mm <- as.numeric(raw$rr)
  raw$precipitation_negative_flag <- ifelse(!is.na(raw$precipitation_raw_mm) & raw$precipitation_raw_mm < 0, 1L, 0L)
  raw$precipitation_mm <- ifelse(
    is.na(raw$precipitation_raw_mm),
    NA_real_,
    pmax(raw$precipitation_raw_mm, 0)
  )
  raw$sunshine_hours <- as.numeric(raw$so_h)
  raw$humidity_mean_pct <- as.numeric(raw$rf_mittel)
  raw$wind_mean_ms <- as.numeric(raw$vv_mittel)
  raw$snow_depth_cm <- as.numeric(ifelse(trimws(raw$sh) == "", NA, raw$sh))
  raw$snow_depth_missing_flag <- ifelse(is.na(raw$snow_depth_cm), 1L, 0L)
  raw$hdd18 <- ifelse(is.na(raw$temp_mean_c), NA_real_, pmax(18 - raw$temp_mean_c, 0))
  raw$frost_day_flag <- ifelse(!is.na(raw$temp_min_c) & raw$temp_min_c < 0, 1L, 0L)
  raw$ice_day_flag <- ifelse(!is.na(raw$temp_max_c) & raw$temp_max_c < 0, 1L, 0L)
  raw$summer_day_flag <- ifelse(!is.na(raw$temp_max_c) & raw$temp_max_c >= 25, 1L, 0L)
  raw$weather_data_available <- ifelse(is.na(raw$temp_mean_c), 0L, 1L)

  weather_daily <- raw[, c(
    "date",
    "weather_station_id",
    "temp_mean_c",
    "temp_max_c",
    "temp_min_c",
    "precipitation_raw_mm",
    "precipitation_mm",
    "precipitation_negative_flag",
    "sunshine_hours",
    "humidity_mean_pct",
    "wind_mean_ms",
    "snow_depth_cm",
    "snow_depth_missing_flag",
    "hdd18",
    "frost_day_flag",
    "ice_day_flag",
    "summer_day_flag",
    "weather_data_available"
  )]
  weather_daily <- weather_daily[order(weather_daily$date), ]

  list(
    path = path,
    station_id = unique(weather_daily$weather_station_id)[1],
    daily = weather_daily
  )
}

fit_heat_weather_model <- function(daily_dataset) {
  fit_rows <- !is.na(daily_dataset$heat_kwh) & !is.na(daily_dataset$hdd18) & daily_dataset$hdd18 > 0
  if (sum(fit_rows) < 30) {
    return(list(prediction = rep(NA_real_, nrow(daily_dataset)), summary = NULL))
  }

  model <- stats::lm(heat_kwh ~ hdd18, data = daily_dataset[fit_rows, , drop = FALSE])
  prediction <- tryCatch(
    as.numeric(stats::predict(model, newdata = daily_dataset)),
    error = function(e) rep(NA_real_, nrow(daily_dataset))
  )
  prediction <- ifelse(is.na(prediction), NA_real_, pmax(prediction, 0))
  model_summary <- summary(model)

  list(
    prediction = prediction,
    summary = list(
      rows = sum(fit_rows),
      intercept = unname(coef(model)[1]),
      hdd18_slope = unname(coef(model)[2]),
      r_squared = unname(model_summary$r.squared)
    )
  )
}

build_monthly_dataset <- function(daily_dataset, heat_monthly_export) {
  month_keys <- sort(unique(format(daily_dataset$date, "%Y-%m")))
  records <- lapply(month_keys, function(month_key) {
    month_rows <- daily_dataset[format(daily_dataset$date, "%Y-%m") == month_key, ]
    baseload_values <- month_rows$electricity_baseload_kw_p10[!is.na(month_rows$electricity_baseload_kw_p10)]
    data.frame(
      month = month_key,
      month_start = as.Date(paste0(month_key, "-01")),
      season_cluster = unique(month_rows$season_cluster)[1],
      meteorological_season = unique(month_rows$meteorological_season)[1],
      days_in_month = nrow(month_rows),
      guest_days = safe_sum(month_rows$has_guests_flag),
      occupancy_increase_days = safe_sum(month_rows$occupancy_increase_flag),
      occupancy_drop_days = safe_sum(month_rows$occupancy_drop_flag),
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
      electricity_no_guest_baseline_kwh = safe_sum(month_rows$electricity_no_guest_baseline_kwh),
      electricity_guest_impact_kwh = safe_sum(month_rows$electricity_guest_impact_kwh),
      heat_kwh = safe_sum(month_rows$heat_kwh),
      heat_volume_m3 = safe_sum(month_rows$heat_volume_m3),
      heat_approximated_days = safe_sum(month_rows$heat_approximated_flag),
      heat_weather_baseline_kwh = safe_sum(month_rows$heat_weather_baseline_kwh),
      heat_weather_residual_kwh = safe_sum(month_rows$heat_weather_residual_kwh),
      heat_no_guest_baseline_kwh = safe_sum(month_rows$heat_no_guest_baseline_kwh),
      heat_guest_impact_kwh = safe_sum(month_rows$heat_guest_impact_kwh),
      pv_kwh = safe_sum(month_rows$pv_kwh),
      pv_days_available = sum(!is.na(month_rows$pv_kwh)),
      nights = safe_sum(month_rows$nights),
      days_with_guests = sum(!is.na(month_rows$nights) & month_rows$nights > 0),
      pv_missing_days = sum(is.na(month_rows$pv_kwh)),
      weather_days_available = safe_sum(month_rows$weather_data_available),
      temp_mean_c = safe_mean(month_rows$temp_mean_c),
      temp_min_c = safe_min(month_rows$temp_min_c),
      temp_max_c = safe_max(month_rows$temp_max_c),
      precipitation_mm = safe_sum(month_rows$precipitation_mm),
      precipitation_negative_days = safe_sum(month_rows$precipitation_negative_flag),
      sunshine_hours = safe_sum(month_rows$sunshine_hours),
      humidity_mean_pct = safe_mean(month_rows$humidity_mean_pct),
      wind_mean_ms = safe_mean(month_rows$wind_mean_ms),
      snow_depth_cm = safe_mean(month_rows$snow_depth_cm),
      hdd18 = safe_sum(month_rows$hdd18),
      frost_days = safe_sum(month_rows$frost_day_flag),
      ice_days = safe_sum(month_rows$ice_day_flag),
      summer_days = safe_sum(month_rows$summer_day_flag),
      stringsAsFactors = FALSE
    )
  })

  monthly <- do.call(rbind, records)
  monthly <- merge(monthly, heat_monthly_export, by = "month", all.x = TRUE)
  monthly$month_start_ms <- as.numeric(as.POSIXct(monthly$month_start, tz = "Europe/Vienna")) * 1000
  monthly$heat_delta_vs_export_kwh <- monthly$heat_kwh - monthly$heat_export_kwh
  monthly$electricity_minus_pv_kwh <- ifelse(
    is.na(monthly$electricity_kwh) | is.na(monthly$pv_kwh),
    NA_real_,
    monthly$electricity_kwh - monthly$pv_kwh
  )
  monthly$grid_need_after_pv_kwh <- ifelse(
    is.na(monthly$electricity_minus_pv_kwh),
    NA_real_,
    pmax(monthly$electricity_minus_pv_kwh, 0)
  )
  monthly$pv_surplus_vs_load_kwh <- ifelse(
    is.na(monthly$electricity_minus_pv_kwh),
    NA_real_,
    pmax(-monthly$electricity_minus_pv_kwh, 0)
  )
  monthly$potential_pv_to_heat_kwh <- ifelse(
    is.na(monthly$pv_surplus_vs_load_kwh) | is.na(monthly$heat_kwh),
    NA_real_,
    pmin(monthly$pv_surplus_vs_load_kwh, monthly$heat_kwh)
  )
  monthly$remaining_heat_after_pv_surplus_kwh <- ifelse(
    is.na(monthly$pv_surplus_vs_load_kwh) | is.na(monthly$heat_kwh),
    NA_real_,
    pmax(monthly$heat_kwh - monthly$pv_surplus_vs_load_kwh, 0)
  )
  monthly$pv_generation_to_load_ratio <- safe_divide(monthly$pv_kwh, monthly$electricity_kwh)
  monthly$potential_pv_coverage_ratio <- safe_divide(
    pmin(monthly$pv_kwh, monthly$electricity_kwh),
    monthly$electricity_kwh
  )
  monthly$potential_pv_to_heat_ratio <- safe_divide(
    monthly$potential_pv_to_heat_kwh,
    monthly$heat_kwh
  )
  monthly$electricity_kwh_per_night <- safe_divide(monthly$electricity_kwh, monthly$nights)
  monthly$heat_kwh_per_night <- safe_divide(monthly$heat_kwh, monthly$nights)
  monthly$pv_kwh_per_night <- safe_divide(monthly$pv_kwh, monthly$nights)
  monthly$electricity_guest_impact_share <- safe_divide(
    monthly$electricity_guest_impact_kwh,
    monthly$electricity_kwh
  )
  monthly$heat_guest_impact_share <- safe_divide(
    monthly$heat_guest_impact_kwh,
    monthly$heat_kwh
  )
  monthly$heat_kwh_per_hdd18 <- safe_divide(monthly$heat_kwh, monthly$hdd18)
  monthly$heat_kwh_per_hdd18_per_night <- safe_divide(monthly$heat_kwh, monthly$hdd18 * monthly$nights)
  monthly$heat_weather_adjusted_per_night_kwh <- safe_divide(
    monthly$heat_weather_residual_kwh,
    monthly$nights
  )
  monthly[order(monthly$month_start), ]
}

build_seasonal_dataset <- function(daily_dataset) {
  season_levels <- c("winter", "transition", "summer")
  seasonal_rows <- lapply(season_levels, function(season_name) {
    rows <- daily_dataset[daily_dataset$season_cluster == season_name, ]
    if (!nrow(rows)) {
      return(NULL)
    }
    data.frame(
      season_cluster = season_name,
      days = nrow(rows),
      guest_days = safe_sum(rows$has_guests_flag),
      occupancy_drop_days = safe_sum(rows$occupancy_drop_flag),
      electricity_kwh = safe_sum(rows$electricity_kwh),
      heat_kwh = safe_sum(rows$heat_kwh),
      pv_kwh = safe_sum(rows$pv_kwh),
      nights = safe_sum(rows$nights),
      temp_mean_c = safe_mean(rows$temp_mean_c),
      temp_min_c = safe_min(rows$temp_min_c),
      temp_max_c = safe_max(rows$temp_max_c),
      precipitation_mm = safe_sum(rows$precipitation_mm),
      sunshine_hours = safe_sum(rows$sunshine_hours),
      hdd18 = safe_sum(rows$hdd18),
      frost_days = safe_sum(rows$frost_day_flag),
      heat_weather_residual_kwh = safe_sum(rows$heat_weather_residual_kwh),
      electricity_guest_impact_kwh = safe_sum(rows$electricity_guest_impact_kwh),
      heat_guest_impact_kwh = safe_sum(rows$heat_guest_impact_kwh),
      potential_pv_to_heat_kwh = safe_sum(rows$potential_pv_to_heat_kwh),
      remaining_heat_after_pv_surplus_kwh = safe_sum(rows$remaining_heat_after_pv_surplus_kwh),
      electricity_kwh_per_night = safe_divide(safe_sum(rows$electricity_kwh), safe_sum(rows$nights)),
      heat_kwh_per_night = safe_divide(safe_sum(rows$heat_kwh), safe_sum(rows$nights)),
      heat_kwh_per_hdd18 = safe_divide(safe_sum(rows$heat_kwh), safe_sum(rows$hdd18)),
      electricity_guest_impact_share = safe_divide(
        safe_sum(rows$electricity_guest_impact_kwh),
        safe_sum(rows$electricity_kwh)
      ),
      heat_guest_impact_share = safe_divide(
        safe_sum(rows$heat_guest_impact_kwh),
        safe_sum(rows$heat_kwh)
      ),
      potential_pv_to_heat_ratio = safe_divide(
        safe_sum(rows$potential_pv_to_heat_kwh),
        safe_sum(rows$heat_kwh)
      ),
      heat_kwh_per_hdd18_per_night = safe_divide(
        safe_sum(rows$heat_kwh),
        safe_sum(rows$hdd18) * safe_sum(rows$nights)
      ),
      heat_weather_adjusted_per_night_kwh = safe_divide(
        safe_sum(rows$heat_weather_residual_kwh),
        safe_sum(rows$nights)
      ),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, seasonal_rows)
}

build_event_summary <- function(daily_dataset) {
  event_definitions <- list(
    all_days = rep(TRUE, nrow(daily_dataset)),
    guest_days = daily_dataset$has_guests_flag == 1,
    non_guest_days = daily_dataset$has_guests_flag == 0,
    occupancy_increase_days = daily_dataset$occupancy_increase_flag == 1,
    occupancy_drop_days = daily_dataset$occupancy_drop_flag == 1,
    winter_guest_days = daily_dataset$season_cluster == "winter" & daily_dataset$has_guests_flag == 1,
    winter_non_guest_days = daily_dataset$season_cluster == "winter" & daily_dataset$has_guests_flag == 0
  )

  records <- lapply(names(event_definitions), function(group_name) {
    rows <- daily_dataset[event_definitions[[group_name]], , drop = FALSE]
    data.frame(
      event_group = group_name,
      days = nrow(rows),
      avg_nights = safe_mean(rows$nights),
      avg_temp_mean_c = safe_mean(rows$temp_mean_c),
      avg_hdd18 = safe_mean(rows$hdd18),
      avg_heat_kwh = safe_mean(rows$heat_kwh),
      avg_heat_weather_residual_kwh = safe_mean(rows$heat_weather_residual_kwh),
      avg_heat_weather_adjusted_per_night_kwh = safe_mean(rows$heat_weather_adjusted_per_night_kwh),
      avg_electricity_kwh = safe_mean(rows$electricity_kwh),
      avg_pv_kwh = safe_mean(rows$pv_kwh),
      avg_heat_delta_prev_day_kwh = safe_mean(rows$heat_kwh_delta_prev_day),
      avg_heat_weather_residual_delta_prev_day_kwh = safe_mean(rows$heat_weather_residual_delta_prev_day_kwh),
      avg_electricity_delta_prev_day_kwh = safe_mean(rows$electricity_kwh_delta_prev_day),
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, records)
}

fit_guest_impact_model <- function(daily_dataset, target, formula, metric_key, metric_label, adjustment_label) {
  needed_columns <- unique(all.vars(formula))
  fit_rows <- stats::complete.cases(daily_dataset[, needed_columns, drop = FALSE])

  prediction_actual <- rep(NA_real_, nrow(daily_dataset))
  prediction_no_guests <- rep(NA_real_, nrow(daily_dataset))
  guest_impact <- rep(NA_real_, nrow(daily_dataset))

  if (sum(fit_rows) < 30) {
    return(list(
      summary = data.frame(
        metric_key = metric_key,
        metric_label = metric_label,
        adjustment_label = adjustment_label,
        formula = paste(deparse(formula), collapse = ""),
        model_rows = sum(fit_rows),
        r_squared = NA_real_,
        nights_coefficient_kwh = NA_real_,
        hdd18_coefficient_kwh = NA_real_,
        baseline_no_guest_avg_on_guest_days_kwh = NA_real_,
        raw_guest_day_avg_kwh = NA_real_,
        raw_no_guest_day_avg_kwh = NA_real_,
        raw_guest_day_delta_kwh = NA_real_,
        raw_guest_day_delta_pct = NA_real_,
        additional_per_guest_night_kwh = NA_real_,
        additional_per_guest_night_pct_vs_baseline = NA_real_,
        total_guest_attributed_kwh = NA_real_,
        total_guest_attributed_share = NA_real_,
        total_guest_nights = safe_sum(daily_dataset$nights),
        stringsAsFactors = FALSE
      ),
      prediction_actual = prediction_actual,
      prediction_no_guests = prediction_no_guests,
      guest_impact = guest_impact,
      model_summary = NULL
    ))
  }

  model <- stats::lm(formula, data = daily_dataset[fit_rows, , drop = FALSE])
  prediction_actual[fit_rows] <- as.numeric(stats::predict(model, newdata = daily_dataset[fit_rows, , drop = FALSE]))

  baseline_data <- daily_dataset[fit_rows, , drop = FALSE]
  baseline_data$nights <- 0
  prediction_no_guests[fit_rows] <- as.numeric(stats::predict(model, newdata = baseline_data))
  guest_impact[fit_rows] <- prediction_actual[fit_rows] - prediction_no_guests[fit_rows]

  guest_rows <- fit_rows & !is.na(daily_dataset$nights) & daily_dataset$nights > 0
  no_guest_rows <- fit_rows & !is.na(daily_dataset$nights) & daily_dataset$nights == 0
  model_summary <- summary(model)

  baseline_no_guest_avg_on_guest_days_kwh <- safe_mean(prediction_no_guests[guest_rows])
  raw_guest_day_avg_kwh <- safe_mean(daily_dataset[[target]][guest_rows])
  raw_no_guest_day_avg_kwh <- safe_mean(daily_dataset[[target]][no_guest_rows])
  raw_guest_day_delta_kwh <- ifelse(
    is.na(raw_guest_day_avg_kwh) | is.na(raw_no_guest_day_avg_kwh),
    NA_real_,
    raw_guest_day_avg_kwh - raw_no_guest_day_avg_kwh
  )
  raw_guest_day_delta_pct <- safe_divide(raw_guest_day_delta_kwh, raw_no_guest_day_avg_kwh)
  additional_per_guest_night_kwh <- safe_coef(model, "nights")
  additional_per_guest_night_pct_vs_baseline <- safe_divide(
    additional_per_guest_night_kwh,
    baseline_no_guest_avg_on_guest_days_kwh
  )
  total_guest_attributed_kwh <- safe_sum(guest_impact[fit_rows])
  total_guest_attributed_share <- safe_divide(
    total_guest_attributed_kwh,
    safe_sum(daily_dataset[[target]][fit_rows])
  )

  list(
    summary = data.frame(
      metric_key = metric_key,
      metric_label = metric_label,
      adjustment_label = adjustment_label,
      formula = paste(deparse(formula), collapse = ""),
      model_rows = sum(fit_rows),
      r_squared = unname(model_summary$r.squared),
      nights_coefficient_kwh = additional_per_guest_night_kwh,
      hdd18_coefficient_kwh = safe_coef(model, "hdd18"),
      baseline_no_guest_avg_on_guest_days_kwh = baseline_no_guest_avg_on_guest_days_kwh,
      raw_guest_day_avg_kwh = raw_guest_day_avg_kwh,
      raw_no_guest_day_avg_kwh = raw_no_guest_day_avg_kwh,
      raw_guest_day_delta_kwh = raw_guest_day_delta_kwh,
      raw_guest_day_delta_pct = raw_guest_day_delta_pct,
      additional_per_guest_night_kwh = additional_per_guest_night_kwh,
      additional_per_guest_night_pct_vs_baseline = additional_per_guest_night_pct_vs_baseline,
      total_guest_attributed_kwh = total_guest_attributed_kwh,
      total_guest_attributed_share = total_guest_attributed_share,
      total_guest_nights = safe_sum(daily_dataset$nights[fit_rows]),
      stringsAsFactors = FALSE
    ),
    prediction_actual = prediction_actual,
    prediction_no_guests = prediction_no_guests,
    guest_impact = guest_impact,
    model_summary = list(
      rows = sum(fit_rows),
      r_squared = unname(model_summary$r.squared),
      nights_coefficient_kwh = additional_per_guest_night_kwh,
      hdd18_coefficient_kwh = safe_coef(model, "hdd18")
    )
  )
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
wetter <- load_wetter(file.path(data_dir, "wetter"))

date_spine <- data.frame(
  date = seq.Date(as.Date("2025-01-01"), as.Date("2025-12-31"), by = "day"),
  stringsAsFactors = FALSE
)

raw_daily <- Reduce(
  function(x, y) merge(x, y, by = "date", all.x = TRUE),
  list(date_spine, strom$daily, fernwaerme$daily, fronius, meldewesen, wetter$daily)
)

raw_daily <- raw_daily[order(raw_daily$date), ]
raw_daily$timestamp_ms <- as.numeric(as.POSIXct(raw_daily$date, tz = "Europe/Vienna")) * 1000
raw_daily$has_guests_flag <- ifelse(!is.na(raw_daily$nights) & raw_daily$nights > 0, 1L, 0L)
raw_daily$electricity_data_available <- ifelse(is.na(raw_daily$electricity_kwh), 0L, 1L)
raw_daily$heat_data_available <- ifelse(is.na(raw_daily$heat_kwh), 0L, 1L)
raw_daily$pv_data_available <- ifelse(is.na(raw_daily$pv_kwh), 0L, 1L)
raw_daily$occupancy_data_available <- ifelse(is.na(raw_daily$nights), 0L, 1L)
raw_daily$season_cluster <- season_cluster_from_date(raw_daily$date)
raw_daily$meteorological_season <- meteorological_season_from_date(raw_daily$date)

analysis_daily <- raw_daily
analysis_daily$nights_prev_day <- lag_vec(analysis_daily$nights)
analysis_daily$occupancy_delta_nights <- ifelse(
  is.na(analysis_daily$nights) | is.na(analysis_daily$nights_prev_day),
  NA_real_,
  analysis_daily$nights - analysis_daily$nights_prev_day
)
analysis_daily$occupancy_increase_flag <- ifelse(!is.na(analysis_daily$occupancy_delta_nights) & analysis_daily$occupancy_delta_nights > 0, 1L, 0L)
analysis_daily$occupancy_drop_flag <- ifelse(!is.na(analysis_daily$occupancy_delta_nights) & analysis_daily$occupancy_delta_nights < 0, 1L, 0L)
analysis_daily$electricity_kwh_delta_prev_day <- ifelse(
  is.na(analysis_daily$electricity_kwh),
  NA_real_,
  analysis_daily$electricity_kwh - lag_vec(analysis_daily$electricity_kwh)
)
analysis_daily$heat_kwh_delta_prev_day <- ifelse(
  is.na(analysis_daily$heat_kwh),
  NA_real_,
  analysis_daily$heat_kwh - lag_vec(analysis_daily$heat_kwh)
)
analysis_daily$temp_mean_delta_prev_day <- ifelse(
  is.na(analysis_daily$temp_mean_c),
  NA_real_,
  analysis_daily$temp_mean_c - lag_vec(analysis_daily$temp_mean_c)
)
analysis_daily$hdd18_delta_prev_day <- ifelse(
  is.na(analysis_daily$hdd18),
  NA_real_,
  analysis_daily$hdd18 - lag_vec(analysis_daily$hdd18)
)

heat_weather_model <- fit_heat_weather_model(analysis_daily)
analysis_daily$heat_weather_baseline_kwh <- heat_weather_model$prediction
analysis_daily$heat_weather_residual_kwh <- ifelse(
  is.na(analysis_daily$heat_kwh) | is.na(analysis_daily$heat_weather_baseline_kwh),
  NA_real_,
  analysis_daily$heat_kwh - analysis_daily$heat_weather_baseline_kwh
)
analysis_daily$heat_weather_residual_delta_prev_day_kwh <- ifelse(
  is.na(analysis_daily$heat_weather_residual_kwh),
  NA_real_,
  analysis_daily$heat_weather_residual_kwh - lag_vec(analysis_daily$heat_weather_residual_kwh)
)

electricity_guest_impact_model <- fit_guest_impact_model(
  analysis_daily,
  "electricity_kwh",
  electricity_kwh ~ hdd18 + nights,
  "electricity",
  "Strom",
  "HDD18-kontrolliert"
)
heat_guest_impact_model <- fit_guest_impact_model(
  analysis_daily,
  "heat_kwh",
  heat_kwh ~ hdd18 + nights,
  "heat",
  "Fernwaerme",
  "HDD18-kontrolliert"
)
analysis_guest_impact <- rbind(
  electricity_guest_impact_model$summary,
  heat_guest_impact_model$summary
)

analysis_daily$electricity_modeled_kwh <- electricity_guest_impact_model$prediction_actual
analysis_daily$electricity_no_guest_baseline_kwh <- electricity_guest_impact_model$prediction_no_guests
analysis_daily$electricity_guest_impact_kwh <- electricity_guest_impact_model$guest_impact
analysis_daily$heat_modeled_kwh <- heat_guest_impact_model$prediction_actual
analysis_daily$heat_no_guest_baseline_kwh <- heat_guest_impact_model$prediction_no_guests
analysis_daily$heat_guest_impact_kwh <- heat_guest_impact_model$guest_impact

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
analysis_daily$potential_pv_to_heat_kwh <- ifelse(
  is.na(analysis_daily$pv_surplus_vs_load_kwh) | is.na(analysis_daily$heat_kwh),
  NA_real_,
  pmin(analysis_daily$pv_surplus_vs_load_kwh, analysis_daily$heat_kwh)
)
analysis_daily$remaining_heat_after_pv_surplus_kwh <- ifelse(
  is.na(analysis_daily$pv_surplus_vs_load_kwh) | is.na(analysis_daily$heat_kwh),
  NA_real_,
  pmax(analysis_daily$heat_kwh - analysis_daily$pv_surplus_vs_load_kwh, 0)
)
analysis_daily$pv_generation_to_load_ratio <- safe_divide(
  analysis_daily$pv_kwh,
  analysis_daily$electricity_kwh
)
analysis_daily$potential_pv_coverage_ratio <- safe_divide(
  pmin(analysis_daily$pv_kwh, analysis_daily$electricity_kwh),
  analysis_daily$electricity_kwh
)
analysis_daily$potential_pv_to_heat_ratio <- safe_divide(
  analysis_daily$potential_pv_to_heat_kwh,
  analysis_daily$heat_kwh
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
analysis_daily$heat_to_electricity_ratio <- safe_divide(
  analysis_daily$heat_kwh,
  analysis_daily$electricity_kwh
)
analysis_daily$heat_kwh_per_hdd18 <- safe_divide(
  analysis_daily$heat_kwh,
  analysis_daily$hdd18
)
analysis_daily$heat_kwh_per_hdd18_per_night <- safe_divide(
  analysis_daily$heat_kwh,
  analysis_daily$hdd18 * analysis_daily$nights
)
analysis_daily$heat_weather_adjusted_per_night_kwh <- safe_divide(
  analysis_daily$heat_weather_residual_kwh,
  analysis_daily$nights
)
analysis_daily$electricity_guest_impact_share <- safe_divide(
  analysis_daily$electricity_guest_impact_kwh,
  analysis_daily$electricity_kwh
)
analysis_daily$heat_guest_impact_share <- safe_divide(
  analysis_daily$heat_guest_impact_kwh,
  analysis_daily$heat_kwh
)
analysis_daily$electricity_7d_avg_kwh <- rolling_mean(analysis_daily$electricity_kwh, 7L)
analysis_daily$heat_7d_avg_kwh <- rolling_mean(analysis_daily$heat_kwh, 7L)
analysis_daily$pv_7d_avg_kwh <- rolling_mean(analysis_daily$pv_kwh, 7L)
analysis_daily$nights_7d_avg <- rolling_mean(analysis_daily$nights, 7L)
analysis_daily$temp_mean_7d_avg_c <- rolling_mean(analysis_daily$temp_mean_c, 7L)
analysis_daily$heat_weather_adjusted_7d_avg_kwh <- rolling_mean(analysis_daily$heat_weather_residual_kwh, 7L)
analysis_daily$heat_weather_adjusted_per_night_7d_avg_kwh <- rolling_mean(
  analysis_daily$heat_weather_adjusted_per_night_kwh,
  7L
)

analysis_monthly <- build_monthly_dataset(analysis_daily, fernwaerme$monthly)
raw_monthly <- analysis_monthly
analysis_seasonal <- build_seasonal_dataset(analysis_daily)
analysis_event_summary <- build_event_summary(analysis_daily)
electricity_guest_summary <- analysis_guest_impact[analysis_guest_impact$metric_key == "electricity", , drop = FALSE]
heat_guest_summary <- analysis_guest_impact[analysis_guest_impact$metric_key == "heat", , drop = FALSE]

pv_missing_days <- as.character(raw_daily$date[is.na(raw_daily$pv_kwh)])
heat_missing_days <- as.character(raw_daily$date[is.na(raw_daily$heat_kwh)])
occupancy_missing_days <- as.character(raw_daily$date[is.na(raw_daily$nights)])
electricity_missing_days <- as.character(raw_daily$date[is.na(raw_daily$electricity_kwh)])
weather_missing_days <- as.character(raw_daily$date[is.na(raw_daily$temp_mean_c)])
winter_rows <- analysis_daily$season_cluster == "winter"

metadata <- list(
  generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
  timezone = "Europe/Vienna",
  source_files = list(
    strom = normalizePath(strom$path, winslash = "/", mustWork = TRUE),
    fernwaerme = normalizePath(fernwaerme$path, winslash = "/", mustWork = TRUE),
    fronius = normalizePath(file.path(data_dir, "fronius"), winslash = "/", mustWork = TRUE),
    meldewesen = normalizePath(file.path(data_dir, "meldewesen"), winslash = "/", mustWork = TRUE),
    wetter = normalizePath(wetter$path, winslash = "/", mustWork = TRUE)
  ),
  source_metadata = list(
    strom = strom$meta,
    wetter = list(
      station_id = wetter$station_id,
      hdd_basis_c = 18
    )
  ),
  quality = list(
    coverage_start = "2025-01-01",
    coverage_end = "2025-12-31",
    row_counts = list(
      strom_quarter_hour = nrow(strom$raw),
      raw_daily = nrow(raw_daily),
      analysis_daily = nrow(analysis_daily),
      raw_monthly = nrow(raw_monthly),
      analysis_monthly = nrow(analysis_monthly),
      analysis_seasonal = nrow(analysis_seasonal),
      analysis_event_summary = nrow(analysis_event_summary),
      analysis_guest_impact = nrow(analysis_guest_impact),
      weather_daily = nrow(wetter$daily)
    ),
    missing_days = list(
      electricity = electricity_missing_days,
      fernwaerme = heat_missing_days,
      pv = pv_missing_days,
      occupancy = occupancy_missing_days,
      weather = weather_missing_days
    ),
    weather_flags = list(
      precipitation_negative_days = safe_sum(analysis_daily$precipitation_negative_flag),
      snow_depth_missing_days = safe_sum(analysis_daily$snow_depth_missing_flag)
    )
  ),
  summary = list(
    total_electricity_kwh = safe_sum(raw_daily$electricity_kwh),
    total_heat_kwh = safe_sum(raw_daily$heat_kwh),
    total_heat_volume_m3 = safe_sum(raw_daily$heat_volume_m3),
    total_pv_kwh_known_days = safe_sum(raw_daily$pv_kwh),
    pv_days_available = sum(!is.na(raw_daily$pv_kwh)),
    total_nights = safe_sum(raw_daily$nights),
    total_hdd18 = safe_sum(raw_daily$hdd18),
    winter_heat_kwh = safe_sum(analysis_daily$heat_kwh[winter_rows]),
    winter_heat_share = safe_divide(
      safe_sum(analysis_daily$heat_kwh[winter_rows]),
      safe_sum(analysis_daily$heat_kwh)
    ),
    total_potential_pv_to_heat_kwh = safe_sum(analysis_daily$potential_pv_to_heat_kwh),
    potential_pv_to_heat_share = safe_divide(
      safe_sum(analysis_daily$potential_pv_to_heat_kwh),
      safe_sum(analysis_daily$heat_kwh)
    ),
    guest_days = safe_sum(analysis_daily$has_guests_flag),
    occupancy_drop_days = safe_sum(analysis_daily$occupancy_drop_flag),
    electricity_additional_per_guest_night_kwh = electricity_guest_summary$additional_per_guest_night_kwh[1],
    electricity_additional_per_guest_night_pct = electricity_guest_summary$additional_per_guest_night_pct_vs_baseline[1],
    electricity_guest_attributed_kwh = electricity_guest_summary$total_guest_attributed_kwh[1],
    electricity_guest_attributed_share = electricity_guest_summary$total_guest_attributed_share[1],
    heat_additional_per_guest_night_kwh = heat_guest_summary$additional_per_guest_night_kwh[1],
    heat_additional_per_guest_night_pct = heat_guest_summary$additional_per_guest_night_pct_vs_baseline[1],
    heat_guest_attributed_kwh = heat_guest_summary$total_guest_attributed_kwh[1],
    heat_guest_attributed_share = heat_guest_summary$total_guest_attributed_share[1],
    electricity_replacement_intervals = safe_sum(raw_daily$electricity_replacement_intervals),
    electricity_replacement_share = safe_divide(
      safe_sum(raw_daily$electricity_replacement_intervals),
      safe_sum(raw_daily$electricity_intervals)
    ),
    precipitation_negative_days = safe_sum(analysis_daily$precipitation_negative_flag),
    correlation_nights_vs_electricity = cor_or_na(analysis_daily$nights, analysis_daily$electricity_kwh),
    correlation_nights_vs_heat = cor_or_na(analysis_daily$nights, analysis_daily$heat_kwh),
    correlation_heat_vs_hdd18 = cor_or_na(analysis_daily$heat_kwh, analysis_daily$hdd18),
    correlation_heat_vs_temp_mean = cor_or_na(analysis_daily$heat_kwh, analysis_daily$temp_mean_c),
    correlation_heat_vs_nights_winter = cor_or_na(
      analysis_daily$nights[winter_rows],
      analysis_daily$heat_kwh[winter_rows]
    ),
    correlation_electricity_vs_pv = cor_or_na(analysis_daily$electricity_kwh, analysis_daily$pv_kwh),
    max_abs_monthly_heat_delta_kwh = safe_max(abs(raw_monthly$heat_delta_vs_export_kwh))
  ),
  models = list(
    heat_weather_baseline = heat_weather_model$summary,
    guest_impact = list(
      electricity = electricity_guest_impact_model$model_summary,
      heat = heat_guest_impact_model$model_summary
    )
  ),
  notes = c(
    "Potential PV coverage and PV-to-heat metrics are calculated on day and month level only. They are not true self-consumption or feed-in metrics.",
    "Fronius data are missing for 2025-11-21, 2025-11-22, 2025-11-24 and 2025-12-25. Missing values stay NA and are not imputed.",
    "Fernwaerme export includes a closing meter row for 2026-01-01. This row is excluded from the 2025 day-level dataset.",
    "Strom timestamps are parsed in Europe/Vienna and include DST transitions with 92 and 100 quarter-hour intervals on switch days.",
    "Weather data are integrated on daily level from Geosphere station 19821. HDD uses a base temperature of 18C.",
    "Guest impact uses daily nights as occupancy measure. Strom and Fernwaerme are modeled against nights; Fernwaerme is additionally controlled by HDD18.",
    "Negative precipitation values are treated as 0 mm and flagged via precipitation_negative_flag.",
    "Blank snow depth values stay NA and are flagged via snow_depth_missing_flag."
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
write.csv(
  analysis_seasonal,
  file.path(output_dir, "analysis_seasonal.csv"),
  row.names = FALSE
)
write.csv(
  analysis_event_summary,
  file.path(output_dir, "analysis_event_summary.csv"),
  row.names = FALSE
)
write.csv(
  analysis_guest_impact,
  file.path(output_dir, "analysis_guest_impact.csv"),
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
    monthly = analysis_monthly,
    seasonal = analysis_seasonal,
    event_summary = analysis_event_summary,
    guest_impact = analysis_guest_impact
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
