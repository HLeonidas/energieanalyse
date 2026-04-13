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

clamp_between <- function(x, lower, upper) {
  ifelse(is.na(x) | is.na(lower) | is.na(upper), NA_real_, pmin(pmax(x, lower), upper))
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

weekday_label_from_iso <- function(weekday_iso) {
  labels <- c("Mo", "Di", "Mi", "Do", "Fr", "Sa", "So")
  out <- rep(NA_character_, length(weekday_iso))
  valid <- !is.na(weekday_iso) & weekday_iso >= 1 & weekday_iso <= 7
  out[valid] <- labels[weekday_iso[valid]]
  out
}

make_no_guest_scenario <- function(df) {
  guest_zero_columns <- c(
    "has_guests_flag",
    "nights",
    "log1p_nights",
    "nights_sq",
    "occupied_apartments_estimated",
    "arrivals",
    "occupancy_delta_nights",
    "occupancy_increase_flag",
    "occupancy_drop_flag"
  )
  for (column_name in guest_zero_columns) {
    if (column_name %in% names(df)) {
      df[[column_name]] <- 0
    }
  }
  df
}

build_model_comparison_dataset <- function(
  daily_dataset,
  target,
  metric_key,
  metric_label,
  model_specs
) {
  comparison_rows <- lapply(model_specs, function(spec) {
    formula <- spec$formula
    needed_columns <- unique(all.vars(formula))
    fit_rows <- stats::complete.cases(daily_dataset[, needed_columns, drop = FALSE])

    if (sum(fit_rows) < 30) {
      return(data.frame(
        metric_key = metric_key,
        metric_label = metric_label,
        model_id = spec$model_id,
        model_label = spec$model_label,
        formula = paste(deparse(formula), collapse = ""),
        model_rows = sum(fit_rows),
        r_squared = NA_real_,
        adjusted_r_squared = NA_real_,
        aic = NA_real_,
        hdd18_coefficient_kwh = NA_real_,
        has_guests_flag_coefficient_kwh = NA_real_,
        nights_coefficient_kwh = NA_real_,
        log1p_nights_coefficient_kwh = NA_real_,
        nights_sq_coefficient_kwh = NA_real_,
        occupied_apartments_estimated_coefficient_kwh = NA_real_,
        arrivals_coefficient_kwh = NA_real_,
        occupancy_delta_nights_coefficient_kwh = NA_real_,
        temp_mean_c_coefficient_kwh = NA_real_,
        sunshine_hours_coefficient_kwh = NA_real_,
        is_weekend_flag_coefficient_kwh = NA_real_,
        stringsAsFactors = FALSE
      ))
    }

    model <- stats::lm(formula, data = daily_dataset[fit_rows, , drop = FALSE])
    model_summary <- summary(model)

    data.frame(
      metric_key = metric_key,
      metric_label = metric_label,
      model_id = spec$model_id,
      model_label = spec$model_label,
      formula = paste(deparse(formula), collapse = ""),
      model_rows = sum(fit_rows),
      r_squared = unname(model_summary$r.squared),
      adjusted_r_squared = unname(model_summary$adj.r.squared),
      aic = stats::AIC(model),
      hdd18_coefficient_kwh = safe_coef(model, "hdd18"),
      has_guests_flag_coefficient_kwh = safe_coef(model, "has_guests_flag"),
      nights_coefficient_kwh = safe_coef(model, "nights"),
      log1p_nights_coefficient_kwh = safe_coef(model, "log1p_nights"),
      nights_sq_coefficient_kwh = safe_coef(model, "nights_sq"),
      occupied_apartments_estimated_coefficient_kwh = safe_coef(model, "occupied_apartments_estimated"),
      arrivals_coefficient_kwh = safe_coef(model, "arrivals"),
      occupancy_delta_nights_coefficient_kwh = safe_coef(model, "occupancy_delta_nights"),
      temp_mean_c_coefficient_kwh = safe_coef(model, "temp_mean_c"),
      sunshine_hours_coefficient_kwh = safe_coef(model, "sunshine_hours"),
      is_weekend_flag_coefficient_kwh = safe_coef(model, "is_weekend_flag"),
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, comparison_rows)
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

load_kelag_profile <- function(
  directory,
  value_col,
  interval_col,
  replacement_interval_col,
  replacement_share_col,
  peak_col = NULL,
  baseload_col = NULL,
  night_col = NULL,
  daytime_col = NULL
) {
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
    record <- list(date = unique(day_rows$date)[1])
    record[[value_col]] <- safe_sum(day_rows$kwh)
    record[[interval_col]] <- nrow(day_rows)
    record[[replacement_interval_col]] <- sum(day_rows$status_class == "replacement", na.rm = TRUE)
    record[[replacement_share_col]] <- safe_divide(
      sum(day_rows$status_class == "replacement", na.rm = TRUE),
      nrow(day_rows)
    )
    if (!is.null(peak_col)) {
      record[[peak_col]] <- safe_max(day_rows$kwh) * 4
    }
    if (!is.null(baseload_col)) {
      record[[baseload_col]] <- safe_quantile(day_rows$kwh, 0.10) * 4
    }
    if (!is.null(night_col)) {
      record[[night_col]] <- safe_sum(day_rows$kwh[day_rows$hour < 6])
    }
    if (!is.null(daytime_col)) {
      record[[daytime_col]] <- safe_sum(day_rows$kwh[day_rows$hour >= 8 & day_rows$hour < 20])
    }
    as.data.frame(record, stringsAsFactors = FALSE)
  }))
  daily <- daily[order(daily$date), ]

  raw_out <- raw[, c("timestamp_iso", "timestamp_ms", "date", "Datum", "Zeit", "kwh", "status_text", "status_class")]
  names(raw_out) <- c("timestamp_iso", "timestamp_ms", "date", "date_local", "time_local", "kwh", "status", "status_class")

  list(path = path, meta = meta, raw = raw_out, daily = daily)
}

load_strom <- function(directory) {
  load_kelag_profile(
    directory = directory,
    value_col = "electricity_kwh",
    interval_col = "electricity_intervals",
    replacement_interval_col = "electricity_replacement_intervals",
    replacement_share_col = "electricity_replacement_share",
    peak_col = "electricity_peak_kw",
    baseload_col = "electricity_baseload_kw_p10",
    night_col = "electricity_night_kwh",
    daytime_col = "electricity_daytime_kwh"
  )
}

load_feed_in <- function(directory) {
  load_kelag_profile(
    directory = directory,
    value_col = "electricity_feed_in_kwh",
    interval_col = "feed_in_intervals",
    replacement_interval_col = "feed_in_replacement_intervals",
    replacement_share_col = "feed_in_replacement_share"
  )
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
      electricity_feed_in_kwh = safe_sum(month_rows$electricity_feed_in_kwh),
      feed_in_intervals = safe_sum(month_rows$feed_in_intervals),
      feed_in_replacement_intervals = safe_sum(month_rows$feed_in_replacement_intervals),
      feed_in_replacement_share = safe_divide(
        safe_sum(month_rows$feed_in_replacement_intervals),
        safe_sum(month_rows$feed_in_intervals)
      ),
      electricity_self_consumption_raw_kwh = safe_sum(month_rows$electricity_self_consumption_raw_kwh),
      electricity_self_consumption_kwh = safe_sum(month_rows$electricity_self_consumption_kwh),
      real_electricity_kwh = safe_sum(month_rows$real_electricity_kwh),
      real_electricity_no_guest_baseline_kwh = safe_sum(month_rows$real_electricity_no_guest_baseline_kwh),
      real_electricity_guest_impact_kwh = safe_sum(month_rows$real_electricity_guest_impact_kwh),
      real_electricity_modeled_total_seasonal_kwh = safe_sum(month_rows$real_electricity_modeled_total_seasonal_kwh),
      real_electricity_modeled_seasonal_kwh = safe_sum(month_rows$real_electricity_modeled_seasonal_kwh),
      real_electricity_no_guest_baseline_seasonal_kwh = safe_sum(month_rows$real_electricity_no_guest_baseline_seasonal_kwh),
      real_electricity_guest_impact_seasonal_kwh = safe_sum(month_rows$real_electricity_guest_impact_seasonal_kwh),
      real_electricity_model_residual_seasonal_kwh = safe_sum(month_rows$real_electricity_model_residual_seasonal_kwh),
      real_electricity_guest_impact_fallback_days = safe_sum(month_rows$real_electricity_guest_impact_segment == "all_days_fallback"),
      heat_kwh = safe_sum(month_rows$heat_kwh),
      heat_volume_m3 = safe_sum(month_rows$heat_volume_m3),
      heat_approximated_days = safe_sum(month_rows$heat_approximated_flag),
      heat_weather_baseline_kwh = safe_sum(month_rows$heat_weather_baseline_kwh),
      heat_weather_residual_kwh = safe_sum(month_rows$heat_weather_residual_kwh),
      heat_no_guest_baseline_kwh = safe_sum(month_rows$heat_no_guest_baseline_kwh),
      heat_guest_impact_kwh = safe_sum(month_rows$heat_guest_impact_kwh),
      heat_modeled_total_seasonal_kwh = safe_sum(month_rows$heat_modeled_total_seasonal_kwh),
      heat_modeled_seasonal_kwh = safe_sum(month_rows$heat_modeled_seasonal_kwh),
      heat_no_guest_baseline_seasonal_kwh = safe_sum(month_rows$heat_no_guest_baseline_seasonal_kwh),
      heat_guest_impact_seasonal_kwh = safe_sum(month_rows$heat_guest_impact_seasonal_kwh),
      heat_model_residual_seasonal_kwh = safe_sum(month_rows$heat_model_residual_seasonal_kwh),
      heat_guest_impact_fallback_days = safe_sum(month_rows$heat_guest_impact_segment == "all_days_fallback"),
      pv_kwh = safe_sum(month_rows$pv_kwh),
      arrivals = safe_sum(month_rows$arrivals),
      pv_days_available = sum(!is.na(month_rows$pv_kwh)),
      nights = safe_sum(month_rows$nights),
      occupied_apartment_nights_estimated = safe_sum(month_rows$occupied_apartments_estimated),
      average_occupied_apartments_estimated_per_guest_day = safe_mean(
        month_rows$occupied_apartments_estimated[month_rows$has_guests_flag == 1]
      ),
      weekend_days = safe_sum(month_rows$is_weekend_flag),
      days_with_guests = sum(!is.na(month_rows$nights) & month_rows$nights > 0),
      pv_missing_days = sum(is.na(month_rows$pv_kwh)),
      feed_in_days_available = sum(!is.na(month_rows$electricity_feed_in_kwh)),
      feed_in_missing_days = sum(is.na(month_rows$electricity_feed_in_kwh)),
      real_electricity_days_available = sum(!is.na(month_rows$real_electricity_kwh)),
      feed_in_exceeds_pv_days = safe_sum(month_rows$feed_in_exceeds_pv_flag),
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
  monthly$potential_pv_to_heat_kwh <- ifelse(
    is.na(monthly$electricity_feed_in_kwh) | is.na(monthly$heat_kwh),
    NA_real_,
    pmin(monthly$electricity_feed_in_kwh, monthly$heat_kwh)
  )
  monthly$remaining_heat_after_pv_surplus_kwh <- ifelse(
    is.na(monthly$electricity_feed_in_kwh) | is.na(monthly$heat_kwh),
    NA_real_,
    pmax(monthly$heat_kwh - monthly$electricity_feed_in_kwh, 0)
  )
  monthly$pv_generation_to_load_ratio <- safe_divide(monthly$pv_kwh, monthly$real_electricity_kwh)
  monthly$electricity_autarky_ratio <- safe_divide(
    monthly$electricity_self_consumption_kwh,
    monthly$real_electricity_kwh
  )
  monthly$pv_self_consumption_ratio <- safe_divide(
    monthly$electricity_self_consumption_kwh,
    monthly$pv_kwh
  )
  monthly$pv_feed_in_ratio <- safe_divide(
    monthly$electricity_feed_in_kwh,
    monthly$pv_kwh
  )
  monthly$potential_pv_to_heat_ratio <- safe_divide(
    monthly$potential_pv_to_heat_kwh,
    monthly$heat_kwh
  )
  monthly$electricity_kwh_per_night <- safe_divide(monthly$electricity_kwh, monthly$nights)
  monthly$real_electricity_kwh_per_night <- safe_divide(monthly$real_electricity_kwh, monthly$nights)
  monthly$heat_kwh_per_night <- safe_divide(monthly$heat_kwh, monthly$nights)
  monthly$pv_kwh_per_night <- safe_divide(monthly$pv_kwh, monthly$nights)
  monthly$real_electricity_guest_impact_share <- safe_divide(
    monthly$real_electricity_guest_impact_kwh,
    monthly$real_electricity_kwh
  )
  monthly$heat_guest_impact_share <- safe_divide(
    monthly$heat_guest_impact_kwh,
    monthly$heat_kwh
  )
  monthly$heat_to_electricity_ratio <- safe_divide(
    monthly$heat_kwh,
    monthly$real_electricity_kwh
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
      electricity_feed_in_kwh = safe_sum(rows$electricity_feed_in_kwh),
      electricity_self_consumption_raw_kwh = safe_sum(rows$electricity_self_consumption_raw_kwh),
      electricity_self_consumption_kwh = safe_sum(rows$electricity_self_consumption_kwh),
      real_electricity_kwh = safe_sum(rows$real_electricity_kwh),
      heat_kwh = safe_sum(rows$heat_kwh),
      pv_kwh = safe_sum(rows$pv_kwh),
      arrivals = safe_sum(rows$arrivals),
      nights = safe_sum(rows$nights),
      occupied_apartment_nights_estimated = safe_sum(rows$occupied_apartments_estimated),
      average_occupied_apartments_estimated_per_guest_day = safe_mean(
        rows$occupied_apartments_estimated[rows$has_guests_flag == 1]
      ),
      weekend_days = safe_sum(rows$is_weekend_flag),
      temp_mean_c = safe_mean(rows$temp_mean_c),
      temp_min_c = safe_min(rows$temp_min_c),
      temp_max_c = safe_max(rows$temp_max_c),
      precipitation_mm = safe_sum(rows$precipitation_mm),
      sunshine_hours = safe_sum(rows$sunshine_hours),
      hdd18 = safe_sum(rows$hdd18),
      frost_days = safe_sum(rows$frost_day_flag),
      heat_weather_residual_kwh = safe_sum(rows$heat_weather_residual_kwh),
      real_electricity_guest_impact_kwh = safe_sum(rows$real_electricity_guest_impact_kwh),
      heat_guest_impact_kwh = safe_sum(rows$heat_guest_impact_kwh),
      potential_pv_to_heat_kwh = safe_sum(rows$potential_pv_to_heat_kwh),
      remaining_heat_after_pv_surplus_kwh = safe_sum(rows$remaining_heat_after_pv_surplus_kwh),
      feed_in_exceeds_pv_days = safe_sum(rows$feed_in_exceeds_pv_flag),
      electricity_kwh_per_night = safe_divide(safe_sum(rows$electricity_kwh), safe_sum(rows$nights)),
      real_electricity_kwh_per_night = safe_divide(safe_sum(rows$real_electricity_kwh), safe_sum(rows$nights)),
      heat_kwh_per_night = safe_divide(safe_sum(rows$heat_kwh), safe_sum(rows$nights)),
      heat_kwh_per_hdd18 = safe_divide(safe_sum(rows$heat_kwh), safe_sum(rows$hdd18)),
      electricity_autarky_ratio = safe_divide(
        safe_sum(rows$electricity_self_consumption_kwh),
        safe_sum(rows$real_electricity_kwh)
      ),
      pv_self_consumption_ratio = safe_divide(
        safe_sum(rows$electricity_self_consumption_kwh),
        safe_sum(rows$pv_kwh)
      ),
      pv_feed_in_ratio = safe_divide(
        safe_sum(rows$electricity_feed_in_kwh),
        safe_sum(rows$pv_kwh)
      ),
      real_electricity_guest_impact_share = safe_divide(
        safe_sum(rows$real_electricity_guest_impact_kwh),
        safe_sum(rows$real_electricity_kwh)
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
      avg_arrivals = safe_mean(rows$arrivals),
      avg_temp_mean_c = safe_mean(rows$temp_mean_c),
      avg_hdd18 = safe_mean(rows$hdd18),
      avg_occupied_apartments_estimated = safe_mean(rows$occupied_apartments_estimated),
      avg_is_weekend_flag = safe_mean(rows$is_weekend_flag),
      avg_heat_kwh = safe_mean(rows$heat_kwh),
      avg_heat_weather_residual_kwh = safe_mean(rows$heat_weather_residual_kwh),
      avg_heat_weather_adjusted_per_night_kwh = safe_mean(rows$heat_weather_adjusted_per_night_kwh),
      avg_electricity_kwh = safe_mean(rows$electricity_kwh),
      avg_real_electricity_kwh = safe_mean(rows$real_electricity_kwh),
      avg_electricity_feed_in_kwh = safe_mean(rows$electricity_feed_in_kwh),
      avg_electricity_self_consumption_kwh = safe_mean(rows$electricity_self_consumption_kwh),
      avg_pv_self_consumption_ratio = safe_mean(rows$pv_self_consumption_ratio),
      avg_pv_feed_in_ratio = safe_mean(rows$pv_feed_in_ratio),
      avg_pv_kwh = safe_mean(rows$pv_kwh),
      avg_heat_delta_prev_day_kwh = safe_mean(rows$heat_kwh_delta_prev_day),
      avg_heat_weather_residual_delta_prev_day_kwh = safe_mean(rows$heat_weather_residual_delta_prev_day_kwh),
      avg_electricity_delta_prev_day_kwh = safe_mean(rows$electricity_kwh_delta_prev_day),
      avg_real_electricity_delta_prev_day_kwh = safe_mean(rows$real_electricity_kwh_delta_prev_day),
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
        baseline_definition = "modellierte Referenz ohne Gaeste",
        formula = paste(deparse(formula), collapse = ""),
        model_rows = sum(fit_rows),
        r_squared = NA_real_,
        adjusted_r_squared = NA_real_,
        nights_coefficient_kwh = NA_real_,
        log1p_nights_coefficient_kwh = NA_real_,
        nights_sq_coefficient_kwh = NA_real_,
        has_guests_flag_coefficient_kwh = NA_real_,
        occupied_apartments_estimated_coefficient_kwh = NA_real_,
        arrivals_coefficient_kwh = NA_real_,
        occupancy_delta_nights_coefficient_kwh = NA_real_,
        hdd18_coefficient_kwh = NA_real_,
        hdd18_sq_coefficient_kwh = NA_real_,
        temp_mean_c_coefficient_kwh = NA_real_,
        sunshine_hours_coefficient_kwh = NA_real_,
        is_weekend_flag_coefficient_kwh = NA_real_,
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
        guest_days = safe_sum(daily_dataset$has_guests_flag),
        average_guest_nights_per_guest_day = safe_mean(
          daily_dataset$nights[!is.na(daily_dataset$has_guests_flag) & daily_dataset$has_guests_flag == 1]
        ),
        total_estimated_occupied_apartment_nights = safe_sum(daily_dataset$occupied_apartments_estimated),
        average_occupied_apartments_estimated_per_guest_day = safe_mean(
          daily_dataset$occupied_apartments_estimated[!is.na(daily_dataset$nights) & daily_dataset$nights > 0]
        ),
        negative_guest_impact_days = NA_real_,
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

  baseline_data <- make_no_guest_scenario(daily_dataset[fit_rows, , drop = FALSE])
  prediction_no_guests[fit_rows] <- as.numeric(stats::predict(model, newdata = baseline_data))
  guest_impact[fit_rows] <- prediction_actual[fit_rows] - prediction_no_guests[fit_rows]

  guest_rows <- fit_rows & !is.na(daily_dataset$has_guests_flag) & daily_dataset$has_guests_flag == 1
  no_guest_rows <- fit_rows & !is.na(daily_dataset$has_guests_flag) & daily_dataset$has_guests_flag == 0
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
  additional_per_guest_night_kwh <- safe_divide(
    safe_sum(guest_impact[guest_rows]),
    safe_sum(daily_dataset$nights[guest_rows])
  )
  additional_per_guest_night_pct_vs_baseline <- safe_divide(
    additional_per_guest_night_kwh,
    baseline_no_guest_avg_on_guest_days_kwh
  )
  total_guest_attributed_kwh <- safe_sum(guest_impact[guest_rows])
  total_guest_attributed_share <- safe_divide(
    total_guest_attributed_kwh,
    safe_sum(daily_dataset[[target]][fit_rows])
  )
  negative_guest_impact_days <- sum(guest_rows & !is.na(guest_impact) & guest_impact < -1e-8)

  list(
    summary = data.frame(
      metric_key = metric_key,
      metric_label = metric_label,
      adjustment_label = adjustment_label,
      baseline_definition = "modellierte Referenz ohne Gaeste",
      formula = paste(deparse(formula), collapse = ""),
      model_rows = sum(fit_rows),
      r_squared = unname(model_summary$r.squared),
      adjusted_r_squared = unname(model_summary$adj.r.squared),
      nights_coefficient_kwh = safe_coef(model, "nights"),
      log1p_nights_coefficient_kwh = safe_coef(model, "log1p_nights"),
      nights_sq_coefficient_kwh = safe_coef(model, "nights_sq"),
      has_guests_flag_coefficient_kwh = safe_coef(model, "has_guests_flag"),
      occupied_apartments_estimated_coefficient_kwh = safe_coef(model, "occupied_apartments_estimated"),
      arrivals_coefficient_kwh = safe_coef(model, "arrivals"),
      occupancy_delta_nights_coefficient_kwh = safe_coef(model, "occupancy_delta_nights"),
      hdd18_coefficient_kwh = safe_coef(model, "hdd18"),
      hdd18_sq_coefficient_kwh = safe_coef(model, "I(hdd18^2)"),
      temp_mean_c_coefficient_kwh = safe_coef(model, "temp_mean_c"),
      sunshine_hours_coefficient_kwh = safe_coef(model, "sunshine_hours"),
      is_weekend_flag_coefficient_kwh = safe_coef(model, "is_weekend_flag"),
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
      guest_days = safe_sum(daily_dataset$has_guests_flag[fit_rows]),
      average_guest_nights_per_guest_day = safe_mean(
        daily_dataset$nights[guest_rows]
      ),
      total_estimated_occupied_apartment_nights = safe_sum(daily_dataset$occupied_apartments_estimated[fit_rows]),
      average_occupied_apartments_estimated_per_guest_day = safe_mean(
        daily_dataset$occupied_apartments_estimated[guest_rows]
      ),
      negative_guest_impact_days = negative_guest_impact_days,
      stringsAsFactors = FALSE
    ),
    prediction_actual = prediction_actual,
    prediction_no_guests = prediction_no_guests,
    guest_impact = guest_impact,
    model_summary = list(
      rows = sum(fit_rows),
      r_squared = unname(model_summary$r.squared),
      adjusted_r_squared = unname(model_summary$adj.r.squared),
      baseline_definition = "modellierte Referenz ohne Gaeste",
      formula = paste(deparse(formula), collapse = ""),
      nights_coefficient_kwh = safe_coef(model, "nights"),
      log1p_nights_coefficient_kwh = safe_coef(model, "log1p_nights"),
      nights_sq_coefficient_kwh = safe_coef(model, "nights_sq"),
      has_guests_flag_coefficient_kwh = safe_coef(model, "has_guests_flag"),
      occupied_apartments_estimated_coefficient_kwh = safe_coef(model, "occupied_apartments_estimated"),
      arrivals_coefficient_kwh = safe_coef(model, "arrivals"),
      occupancy_delta_nights_coefficient_kwh = safe_coef(model, "occupancy_delta_nights"),
      hdd18_coefficient_kwh = safe_coef(model, "hdd18"),
      hdd18_sq_coefficient_kwh = safe_coef(model, "I(hdd18^2)"),
      temp_mean_c_coefficient_kwh = safe_coef(model, "temp_mean_c"),
      sunshine_hours_coefficient_kwh = safe_coef(model, "sunshine_hours"),
      is_weekend_flag_coefficient_kwh = safe_coef(model, "is_weekend_flag"),
      guest_days = safe_sum(daily_dataset$has_guests_flag[fit_rows]),
      average_guest_nights_per_guest_day = safe_mean(daily_dataset$nights[guest_rows]),
      additional_per_guest_night_kwh = additional_per_guest_night_kwh,
      negative_guest_impact_days = negative_guest_impact_days
    )
  )
}

build_guest_value_dataset <- function(
  seasonal_guest_impact,
  daily_dataset,
  seasonal_pricing,
  electricity_tariff_gross_eur_per_kwh,
  heat_tariff_gross_eur_per_kwh = NA_real_
) {
  seasonal_lookup <- function(metric_key, season_name) {
    seasonal_guest_impact[
      seasonal_guest_impact$metric_key == metric_key &
        seasonal_guest_impact$segment_key == season_name,
      ,
      drop = FALSE
    ]
  }

  rows <- lapply(seq_len(nrow(seasonal_pricing)), function(idx) {
    pricing_row <- seasonal_pricing[idx, , drop = FALSE]
    season_name <- pricing_row$season_cluster[1]
    apartment_rate_eur <- pricing_row$assumed_revenue_eur_per_night[1]
    electricity_summary <- seasonal_lookup("real_electricity", season_name)
    heat_summary <- seasonal_lookup("heat", season_name)
    season_rows <- daily_dataset[
      daily_dataset$season_cluster == season_name &
        !is.na(daily_dataset$nights) &
        daily_dataset$nights > 0,
      ,
      drop = FALSE
    ]

    electricity_additional_kwh <- electricity_summary$additional_per_guest_night_kwh[1]
    heat_additional_kwh <- heat_summary$additional_per_guest_night_kwh[1]
    total_guest_nights <- electricity_summary$total_guest_nights[1]
    total_estimated_occupied_apartment_nights <- safe_sum(season_rows$occupied_apartments_estimated)
    average_occupied_apartments_per_guest_day <- safe_mean(season_rows$occupied_apartments_estimated)
    estimated_revenue_total_eur <- total_estimated_occupied_apartment_nights * apartment_rate_eur
    estimated_revenue_per_guest_night_eur <- safe_divide(
      estimated_revenue_total_eur,
      total_guest_nights
    )
    estimated_revenue_per_guest_day_eur <- safe_divide(
      estimated_revenue_total_eur,
      electricity_summary$guest_days[1]
    )
    electricity_total_guest_attributed_kwh <- electricity_summary$total_guest_attributed_kwh[1]
    heat_total_guest_attributed_kwh <- heat_summary$total_guest_attributed_kwh[1]
    electricity_cost_gross_eur <- electricity_additional_kwh * electricity_tariff_gross_eur_per_kwh
    electricity_cost_total_gross_eur <- electricity_total_guest_attributed_kwh * electricity_tariff_gross_eur_per_kwh
    heat_cost_gross_eur <- ifelse(
      is.na(heat_tariff_gross_eur_per_kwh),
      NA_real_,
      heat_additional_kwh * heat_tariff_gross_eur_per_kwh
    )
    heat_cost_total_gross_eur <- ifelse(
      is.na(heat_tariff_gross_eur_per_kwh),
      NA_real_,
      heat_total_guest_attributed_kwh * heat_tariff_gross_eur_per_kwh
    )

    data.frame(
      season_cluster = season_name,
      season_label = pricing_row$season_label[1],
      model_days = electricity_summary$model_rows[1],
      guest_days = electricity_summary$guest_days[1],
      total_guest_nights = total_guest_nights,
      average_guest_nights_per_guest_day = electricity_summary$average_guest_nights_per_guest_day[1],
      occupancy_model_unit = "guest_nights",
      revenue_assumption_unit = "estimated_occupied_apartment_nights",
      assumed_revenue_basis = pricing_row$assumed_revenue_basis[1],
      assumed_revenue_eur_per_night = apartment_rate_eur,
      estimated_occupied_apartment_nights = total_estimated_occupied_apartment_nights,
      average_occupied_apartments_per_guest_day = average_occupied_apartments_per_guest_day,
      estimated_revenue_total_eur = estimated_revenue_total_eur,
      estimated_revenue_per_guest_day_eur = estimated_revenue_per_guest_day_eur,
      estimated_revenue_per_guest_night_eur = estimated_revenue_per_guest_night_eur,
      electricity_model_r_squared = electricity_summary$r_squared[1],
      heat_model_r_squared = heat_summary$r_squared[1],
      electricity_additional_per_guest_night_kwh = electricity_additional_kwh,
      heat_additional_per_guest_night_kwh = heat_additional_kwh,
      electricity_total_guest_attributed_kwh = electricity_total_guest_attributed_kwh,
      heat_total_guest_attributed_kwh = heat_total_guest_attributed_kwh,
      electricity_tariff_gross_eur_per_kwh = electricity_tariff_gross_eur_per_kwh,
      electricity_cost_per_additional_guest_night_gross_eur = electricity_cost_gross_eur,
      electricity_cost_total_guest_attributed_gross_eur = electricity_cost_total_gross_eur,
      heat_tariff_gross_eur_per_kwh = heat_tariff_gross_eur_per_kwh,
      heat_cost_per_additional_guest_night_gross_eur = heat_cost_gross_eur,
      heat_cost_total_guest_attributed_gross_eur = heat_cost_total_gross_eur,
      contribution_after_electricity_gross_eur = estimated_revenue_total_eur - electricity_cost_total_gross_eur,
      contribution_after_energy_gross_eur = ifelse(
        is.na(heat_cost_total_gross_eur),
        NA_real_,
        estimated_revenue_total_eur - electricity_cost_total_gross_eur - heat_cost_total_gross_eur
      ),
      heat_cost_formula_gross = ifelse(
        is.na(heat_total_guest_attributed_kwh),
        NA_character_,
        sprintf("%.2f * heat_tariff_gross_eur_per_kwh", heat_total_guest_attributed_kwh)
      ),
      net_contribution_formula_gross = ifelse(
        is.na(heat_total_guest_attributed_kwh),
        NA_character_,
        sprintf(
          "%.2f - %.2f - %.2f * heat_tariff_gross_eur_per_kwh",
          estimated_revenue_total_eur,
          electricity_cost_total_gross_eur,
          heat_total_guest_attributed_kwh
        )
      ),
      break_even_heat_tariff_gross_eur_per_kwh = safe_divide(
        estimated_revenue_total_eur - electricity_cost_total_gross_eur,
        heat_total_guest_attributed_kwh
      ),
      scenario_note = paste(
        "Revenue estimate uses occupied apartment nights = sum(min(3, ceiling(guest_nights / 2))).",
        "Energy costs are shown only as gross values and stay model-based, so they remain an approximation."
      ),
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, rows)
}

fit_segmented_guest_impact_model <- function(
  daily_dataset,
  target,
  formula,
  metric_key,
  metric_label,
  adjustment_label,
  segment_col = "season_cluster",
  segment_levels = NULL
) {
  if (is.null(segment_levels)) {
    segment_levels <- unique(as.character(daily_dataset[[segment_col]]))
  }

  needed_columns <- unique(all.vars(formula))
  prediction_actual <- rep(NA_real_, nrow(daily_dataset))
  prediction_no_guests <- rep(NA_real_, nrow(daily_dataset))
  guest_impact <- rep(NA_real_, nrow(daily_dataset))
  applied_segment <- rep(NA_character_, nrow(daily_dataset))
  summary_rows <- list()
  fallback_model <- fit_guest_impact_model(
    daily_dataset,
    target,
    formula,
    metric_key,
    metric_label,
    paste(adjustment_label, "fallback all days", sep = ", ")
  )

  for (segment_name in segment_levels) {
    if (is.na(segment_name) || !nzchar(segment_name)) {
      next
    }

    segment_mask <- !is.na(daily_dataset[[segment_col]]) & daily_dataset[[segment_col]] == segment_name
    if (!any(segment_mask)) {
      next
    }

    segment_dataset <- daily_dataset[segment_mask, , drop = FALSE]
    segment_model <- fit_guest_impact_model(
      segment_dataset,
      target,
      formula,
      metric_key,
      metric_label,
      paste(adjustment_label, "saisonal", sep = ", ")
    )

    segment_fit_rows <- stats::complete.cases(segment_dataset[, needed_columns, drop = FALSE])
    segment_additional_kwh <- segment_model$summary$additional_per_guest_night_kwh[1]
    segment_negative_guest_impact_days <- segment_model$summary$negative_guest_impact_days[1]
    use_segment_model <- !is.na(segment_additional_kwh) &&
      segment_additional_kwh >= 0 &&
      !is.na(segment_negative_guest_impact_days) &&
      segment_negative_guest_impact_days == 0

    chosen_prediction_actual <- if (use_segment_model) {
      segment_model$prediction_actual
    } else {
      fallback_model$prediction_actual[segment_mask]
    }
    chosen_prediction_no_guests <- if (use_segment_model) {
      segment_model$prediction_no_guests
    } else {
      fallback_model$prediction_no_guests[segment_mask]
    }
    chosen_guest_impact <- if (use_segment_model) {
      segment_model$guest_impact
    } else {
      fallback_model$guest_impact[segment_mask]
    }

    usable_rows <- !is.na(chosen_prediction_no_guests)
    prediction_actual[segment_mask] <- chosen_prediction_actual
    prediction_no_guests[segment_mask] <- chosen_prediction_no_guests
    guest_impact[segment_mask] <- chosen_guest_impact
    segment_indices <- which(segment_mask)
    applied_segment[segment_indices[usable_rows]] <- if (use_segment_model) {
      segment_name
    } else {
      "all_days_fallback"
    }

    summary_row <- if (use_segment_model) {
      segment_model$summary
    } else {
      summary_row_fallback <- segment_model$summary
      passthrough_columns <- c(
        "baseline_definition",
        "r_squared",
        "adjusted_r_squared",
        "nights_coefficient_kwh",
        "log1p_nights_coefficient_kwh",
        "nights_sq_coefficient_kwh",
        "has_guests_flag_coefficient_kwh",
        "occupied_apartments_estimated_coefficient_kwh",
        "arrivals_coefficient_kwh",
        "occupancy_delta_nights_coefficient_kwh",
        "hdd18_coefficient_kwh",
        "hdd18_sq_coefficient_kwh",
        "temp_mean_c_coefficient_kwh",
        "sunshine_hours_coefficient_kwh",
        "is_weekend_flag_coefficient_kwh",
        "negative_guest_impact_days"
      )
      for (column_name in passthrough_columns) {
        if (column_name %in% names(summary_row_fallback) && column_name %in% names(fallback_model$summary)) {
          summary_row_fallback[[column_name]] <- fallback_model$summary[[column_name]][1]
        }
      }
      summary_row_fallback$baseline_no_guest_avg_on_guest_days_kwh <- safe_mean(
        chosen_prediction_no_guests[segment_dataset$has_guests_flag == 1]
      )
      summary_row_fallback$additional_per_guest_night_kwh <- fallback_model$summary$additional_per_guest_night_kwh[1]
      summary_row_fallback$additional_per_guest_night_pct_vs_baseline <- safe_divide(
        summary_row_fallback$additional_per_guest_night_kwh,
        summary_row_fallback$baseline_no_guest_avg_on_guest_days_kwh
      )
      summary_row_fallback$total_guest_attributed_kwh <- safe_sum(chosen_guest_impact[segment_dataset$has_guests_flag == 1])
      summary_row_fallback$total_guest_attributed_share <- safe_divide(
        summary_row_fallback$total_guest_attributed_kwh,
        safe_sum(segment_dataset[[target]][segment_fit_rows])
      )
      summary_row_fallback
    }
    summary_row$segment_key <- segment_name
    summary_row$segment_col <- segment_col
    summary_row$segment_days <- sum(segment_mask)
    summary_row$guest_days <- safe_sum(segment_dataset$has_guests_flag)
    summary_row$average_guest_nights_per_guest_day <- safe_mean(
      segment_dataset$nights[segment_dataset$has_guests_flag == 1]
    )
    summary_row$total_estimated_occupied_apartment_nights <- safe_sum(
      segment_dataset$occupied_apartments_estimated[segment_fit_rows]
    )
    summary_row$average_occupied_apartments_estimated_per_guest_day <- safe_mean(
      segment_dataset$occupied_apartments_estimated[segment_dataset$has_guests_flag == 1]
    )
    summary_row$segment_source <- if (use_segment_model) {
      "segment"
    } else {
      "all_days_fallback_negative_or_sparse"
    }
    summary_rows[[length(summary_rows) + 1L]] <- summary_row
  }

  fallback_rows <- stats::complete.cases(daily_dataset[, needed_columns, drop = FALSE]) & is.na(prediction_no_guests)
  if (any(fallback_rows)) {
    prediction_actual[fallback_rows] <- fallback_model$prediction_actual[fallback_rows]
    prediction_no_guests[fallback_rows] <- fallback_model$prediction_no_guests[fallback_rows]
    guest_impact[fallback_rows] <- fallback_model$guest_impact[fallback_rows]
    applied_segment[fallback_rows] <- "all_days_fallback"
  }

  list(
    summary_segmented = do.call(rbind, summary_rows),
    fallback_summary = fallback_model$summary,
    prediction_actual = prediction_actual,
    prediction_no_guests = prediction_no_guests,
    guest_impact = guest_impact,
    applied_segment = applied_segment
  )
}

align_segmented_summary_to_applied_series <- function(
  segmented_summary,
  daily_dataset,
  annual_summary,
  metric_key,
  target_col,
  baseline_col,
  impact_col,
  segment_assignment_col
) {
  summary_rows <- segmented_summary$metric_key == metric_key
  season_rows <- which(summary_rows)

  for (row_idx in season_rows) {
    segment_name <- segmented_summary$segment_key[row_idx]
    segment_mask <- daily_dataset$season_cluster == segment_name
    applied_values <- unique(daily_dataset[[segment_assignment_col]][segment_mask & !is.na(daily_dataset[[segment_assignment_col]])])

    if (length(applied_values) == 1 && applied_values == "all_days_fallback") {
      fit_rows <- segment_mask &
        !is.na(daily_dataset[[target_col]]) &
        !is.na(daily_dataset[[baseline_col]]) &
        !is.na(daily_dataset[[impact_col]])
      guest_rows <- fit_rows & daily_dataset$has_guests_flag == 1
      passthrough_columns <- intersect(
        names(segmented_summary),
        c(
          "baseline_definition",
          "r_squared",
          "adjusted_r_squared",
          "nights_coefficient_kwh",
          "log1p_nights_coefficient_kwh",
          "nights_sq_coefficient_kwh",
          "has_guests_flag_coefficient_kwh",
          "occupied_apartments_estimated_coefficient_kwh",
          "arrivals_coefficient_kwh",
          "occupancy_delta_nights_coefficient_kwh",
          "hdd18_coefficient_kwh",
          "hdd18_sq_coefficient_kwh",
          "temp_mean_c_coefficient_kwh",
          "sunshine_hours_coefficient_kwh",
          "is_weekend_flag_coefficient_kwh",
          "negative_guest_impact_days"
        )
      )
      for (column_name in passthrough_columns) {
        if (column_name %in% names(annual_summary)) {
          segmented_summary[[column_name]][row_idx] <- annual_summary[[column_name]][1]
        }
      }
      segmented_summary$baseline_no_guest_avg_on_guest_days_kwh[row_idx] <- safe_mean(
        daily_dataset[[baseline_col]][guest_rows]
      )
      segmented_summary$additional_per_guest_night_kwh[row_idx] <- annual_summary$additional_per_guest_night_kwh[1]
      segmented_summary$additional_per_guest_night_pct_vs_baseline[row_idx] <- safe_divide(
        segmented_summary$additional_per_guest_night_kwh[row_idx],
        segmented_summary$baseline_no_guest_avg_on_guest_days_kwh[row_idx]
      )
      segmented_summary$total_guest_attributed_kwh[row_idx] <- safe_sum(daily_dataset[[impact_col]][fit_rows])
      segmented_summary$total_guest_attributed_share[row_idx] <- safe_divide(
        segmented_summary$total_guest_attributed_kwh[row_idx],
        safe_sum(daily_dataset[[target_col]][fit_rows])
      )
      segmented_summary$segment_source[row_idx] <- "all_days_fallback_negative_or_sparse"
    }
  }

  segmented_summary
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
feed_in <- load_feed_in(file.path(data_dir, "strom-einspeisung"))
wetter <- load_wetter(file.path(data_dir, "wetter"))

date_spine <- data.frame(
  date = seq.Date(as.Date("2025-01-01"), as.Date("2025-12-31"), by = "day"),
  stringsAsFactors = FALSE
)

raw_daily <- Reduce(
  function(x, y) merge(x, y, by = "date", all.x = TRUE),
  list(date_spine, strom$daily, feed_in$daily, fernwaerme$daily, fronius, meldewesen, wetter$daily)
)

raw_daily <- raw_daily[order(raw_daily$date), ]
raw_daily$timestamp_ms <- as.numeric(as.POSIXct(raw_daily$date, tz = "Europe/Vienna")) * 1000
raw_daily$has_guests_flag <- ifelse(!is.na(raw_daily$nights) & raw_daily$nights > 0, 1L, 0L)
  raw_daily$occupied_apartments_estimated <- ifelse(
    is.na(raw_daily$nights),
    NA_real_,
    ifelse(
      raw_daily$nights > 0,
      pmin(3, ceiling(raw_daily$nights / 2)),
      0
    )
  )
raw_daily$electricity_data_available <- ifelse(is.na(raw_daily$electricity_kwh), 0L, 1L)
raw_daily$feed_in_data_available <- ifelse(is.na(raw_daily$electricity_feed_in_kwh), 0L, 1L)
raw_daily$heat_data_available <- ifelse(is.na(raw_daily$heat_kwh), 0L, 1L)
raw_daily$pv_data_available <- ifelse(is.na(raw_daily$pv_kwh), 0L, 1L)
raw_daily$occupancy_data_available <- ifelse(is.na(raw_daily$nights), 0L, 1L)
raw_daily$season_cluster <- season_cluster_from_date(raw_daily$date)
raw_daily$meteorological_season <- meteorological_season_from_date(raw_daily$date)

analysis_daily <- raw_daily
analysis_daily$nights_prev_day <- lag_vec(analysis_daily$nights)
analysis_daily$log1p_nights <- ifelse(
  is.na(analysis_daily$nights),
  NA_real_,
  log1p(pmax(analysis_daily$nights, 0))
)
analysis_daily$nights_sq <- ifelse(
  is.na(analysis_daily$nights),
  NA_real_,
  analysis_daily$nights^2
)
analysis_daily$weekday_iso <- as.integer(format(analysis_daily$date, "%u"))
analysis_daily$weekday_label <- weekday_label_from_iso(analysis_daily$weekday_iso)
analysis_daily$month_num <- factor(format(analysis_daily$date, "%m"))
analysis_daily$is_weekend_flag <- ifelse(
  is.na(analysis_daily$weekday_iso),
  NA_integer_,
  ifelse(analysis_daily$weekday_iso %in% c(6L, 7L), 1L, 0L)
)
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
analysis_daily$electricity_feed_in_kwh_delta_prev_day <- ifelse(
  is.na(analysis_daily$electricity_feed_in_kwh),
  NA_real_,
  analysis_daily$electricity_feed_in_kwh - lag_vec(analysis_daily$electricity_feed_in_kwh)
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
analysis_daily$electricity_self_consumption_raw_kwh <- ifelse(
  is.na(analysis_daily$pv_kwh) | is.na(analysis_daily$electricity_feed_in_kwh),
  NA_real_,
  analysis_daily$pv_kwh - analysis_daily$electricity_feed_in_kwh
)
analysis_daily$feed_in_exceeds_pv_flag <- ifelse(
  !is.na(analysis_daily$electricity_feed_in_kwh) &
    !is.na(analysis_daily$pv_kwh) &
    analysis_daily$electricity_feed_in_kwh > analysis_daily$pv_kwh + 1e-8,
  1L,
  0L
)
analysis_daily$electricity_self_consumption_kwh <- clamp_between(
  analysis_daily$electricity_self_consumption_raw_kwh,
  0,
  analysis_daily$pv_kwh
)
analysis_daily$real_electricity_kwh <- ifelse(
  is.na(analysis_daily$electricity_kwh) | is.na(analysis_daily$electricity_self_consumption_kwh),
  NA_real_,
  analysis_daily$electricity_kwh + analysis_daily$electricity_self_consumption_kwh
)
analysis_daily$real_electricity_data_available <- ifelse(is.na(analysis_daily$real_electricity_kwh), 0L, 1L)
analysis_daily$real_electricity_kwh_delta_prev_day <- ifelse(
  is.na(analysis_daily$real_electricity_kwh),
  NA_real_,
  analysis_daily$real_electricity_kwh - lag_vec(analysis_daily$real_electricity_kwh)
)
analysis_daily$electricity_autarky_ratio <- safe_divide(
  analysis_daily$electricity_self_consumption_kwh,
  analysis_daily$real_electricity_kwh
)
analysis_daily$pv_self_consumption_ratio <- safe_divide(
  analysis_daily$electricity_self_consumption_kwh,
  analysis_daily$pv_kwh
)
analysis_daily$pv_feed_in_ratio <- safe_divide(
  analysis_daily$electricity_feed_in_kwh,
  analysis_daily$pv_kwh
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

real_electricity_formula_annual <- real_electricity_kwh ~
  splines::ns(hdd18, 3) +
  occupied_apartments_estimated +
  arrivals +
  occupancy_delta_nights +
  sunshine_hours +
  is_weekend_flag +
  factor(season_cluster)
real_electricity_formula_segmented <- real_electricity_kwh ~
  splines::ns(hdd18, 3) +
  occupied_apartments_estimated +
  arrivals +
  occupancy_delta_nights +
  sunshine_hours +
  is_weekend_flag
heat_formula_annual <- heat_kwh ~
  hdd18 +
  I(hdd18^2) +
  has_guests_flag +
  log1p_nights +
  occupied_apartments_estimated +
  arrivals +
  is_weekend_flag +
  factor(season_cluster)
heat_formula_segmented <- heat_kwh ~
  hdd18 +
  I(hdd18^2) +
  has_guests_flag +
  log1p_nights +
  occupied_apartments_estimated +
  arrivals +
  is_weekend_flag

real_electricity_model_comparison <- build_model_comparison_dataset(
  analysis_daily,
  "real_electricity_kwh",
  "real_electricity",
  "Realer Stromverbrauch",
  list(
    list(
      model_id = "model1_hdd18",
      model_label = "Modell 1: Verbrauch ~ hdd18",
      formula = real_electricity_kwh ~ hdd18
    ),
    list(
      model_id = "model2_hdd18_has_guests_flag",
      model_label = "Modell 2: Verbrauch ~ hdd18 + has_guests_flag",
      formula = real_electricity_kwh ~ hdd18 + has_guests_flag
    ),
    list(
      model_id = "model3_hdd18_has_guests_flag_nights",
      model_label = "Modell 3: Verbrauch ~ hdd18 + has_guests_flag + nights",
      formula = real_electricity_kwh ~ hdd18 + has_guests_flag + nights
    ),
    list(
      model_id = "model4_extended_operational",
      model_label = "Modell 4: Produktionsmodell mit HDD18-Spline, Sonne, Betrieb und Saison",
      formula = real_electricity_formula_annual
    ),
    list(
      model_id = "model5_extended_nonlinear",
      model_label = "Modell 5: komplexes Gastmodell mit Wetter und Betriebsmerkmalen",
      formula = real_electricity_kwh ~
        has_guests_flag +
        log1p_nights +
        nights_sq +
        occupied_apartments_estimated +
        arrivals +
        temp_mean_c +
        sunshine_hours +
        factor(weekday_iso) +
        factor(season_cluster)
    )
  )
)
heat_model_comparison <- build_model_comparison_dataset(
  analysis_daily,
  "heat_kwh",
  "heat",
  "Fernwaerme",
  list(
    list(
      model_id = "model1_hdd18",
      model_label = "Modell 1: Verbrauch ~ hdd18",
      formula = heat_kwh ~ hdd18
    ),
    list(
      model_id = "model2_hdd18_has_guests_flag",
      model_label = "Modell 2: Verbrauch ~ hdd18 + has_guests_flag",
      formula = heat_kwh ~ hdd18 + has_guests_flag
    ),
    list(
      model_id = "model3_hdd18_has_guests_flag_nights",
      model_label = "Modell 3: Verbrauch ~ hdd18 + has_guests_flag + nights",
      formula = heat_kwh ~ hdd18 + has_guests_flag + nights
    ),
    list(
      model_id = "model4_extended_operational",
      model_label = "Modell 4: erweitert mit HDD18^2, Betrieb und Saison",
      formula = heat_formula_annual
    ),
    list(
      model_id = "model5_extended_nonlinear",
      model_label = "Modell 5: erweitert mit nights^2 und weekday",
      formula = heat_kwh ~
        hdd18 +
        I(hdd18^2) +
        has_guests_flag +
        log1p_nights +
        nights_sq +
        occupied_apartments_estimated +
        arrivals +
        factor(weekday_iso) +
        factor(season_cluster)
    )
  )
)
analysis_model_comparison <- rbind(
  real_electricity_model_comparison,
  heat_model_comparison
)

real_electricity_guest_impact_model <- fit_guest_impact_model(
  analysis_daily,
  "real_electricity_kwh",
  real_electricity_formula_annual,
  "real_electricity",
  "Realer Stromverbrauch",
  "Nichtlinearer HDD18-, Sonnen-, Betriebs- und kalenderkontrolliert"
)
heat_guest_impact_model <- fit_guest_impact_model(
  analysis_daily,
  "heat_kwh",
  heat_formula_annual,
  "heat",
  "Fernwaerme",
  "HDD18-, Betriebs- und saisonkontrolliert"
)
season_levels <- c("winter", "transition", "summer")
real_electricity_guest_impact_model_seasonal <- fit_segmented_guest_impact_model(
  analysis_daily,
  "real_electricity_kwh",
  real_electricity_formula_segmented,
  "real_electricity",
  "Realer Stromverbrauch",
  "Nichtlinearer HDD18-, Sonnen-, Betriebs- und kalenderkontrolliert",
  segment_col = "season_cluster",
  segment_levels = season_levels
)
heat_guest_impact_model_seasonal <- fit_segmented_guest_impact_model(
  analysis_daily,
  "heat_kwh",
  heat_formula_segmented,
  "heat",
  "Fernwaerme",
  "HDD18- und betriebs-kontrolliert",
  segment_col = "season_cluster",
  segment_levels = season_levels
)
analysis_guest_impact <- rbind(
  real_electricity_guest_impact_model$summary,
  heat_guest_impact_model$summary
)
analysis_guest_impact_seasonal <- rbind(
  real_electricity_guest_impact_model_seasonal$summary_segmented,
  heat_guest_impact_model_seasonal$summary_segmented
)

analysis_daily$real_electricity_modeled_kwh <- real_electricity_guest_impact_model$prediction_actual
analysis_daily$real_electricity_no_guest_baseline_kwh <- real_electricity_guest_impact_model$prediction_no_guests
analysis_daily$real_electricity_guest_impact_kwh <- real_electricity_guest_impact_model$guest_impact
analysis_daily$real_electricity_modeled_total_seasonal_kwh <- real_electricity_guest_impact_model_seasonal$prediction_actual
analysis_daily$real_electricity_modeled_seasonal_kwh <- analysis_daily$real_electricity_modeled_total_seasonal_kwh
analysis_daily$real_electricity_no_guest_baseline_seasonal_kwh <- real_electricity_guest_impact_model_seasonal$prediction_no_guests
analysis_daily$real_electricity_guest_impact_seasonal_kwh <- real_electricity_guest_impact_model_seasonal$guest_impact
analysis_daily$real_electricity_model_residual_seasonal_kwh <- ifelse(
  is.na(analysis_daily$real_electricity_kwh) | is.na(analysis_daily$real_electricity_modeled_total_seasonal_kwh),
  NA_real_,
  analysis_daily$real_electricity_kwh - analysis_daily$real_electricity_modeled_total_seasonal_kwh
)
analysis_daily$real_electricity_guest_impact_segment <- real_electricity_guest_impact_model_seasonal$applied_segment
analysis_daily$heat_modeled_kwh <- heat_guest_impact_model$prediction_actual
analysis_daily$heat_no_guest_baseline_kwh <- heat_guest_impact_model$prediction_no_guests
analysis_daily$heat_guest_impact_kwh <- heat_guest_impact_model$guest_impact
analysis_daily$heat_modeled_total_seasonal_kwh <- heat_guest_impact_model_seasonal$prediction_actual
analysis_daily$heat_modeled_seasonal_kwh <- analysis_daily$heat_modeled_total_seasonal_kwh
analysis_daily$heat_no_guest_baseline_seasonal_kwh <- heat_guest_impact_model_seasonal$prediction_no_guests
analysis_daily$heat_guest_impact_seasonal_kwh <- heat_guest_impact_model_seasonal$guest_impact
analysis_daily$heat_model_residual_seasonal_kwh <- ifelse(
  is.na(analysis_daily$heat_kwh) | is.na(analysis_daily$heat_modeled_total_seasonal_kwh),
  NA_real_,
  analysis_daily$heat_kwh - analysis_daily$heat_modeled_total_seasonal_kwh
)
analysis_daily$heat_guest_impact_segment <- heat_guest_impact_model_seasonal$applied_segment

analysis_daily$potential_pv_to_heat_kwh <- ifelse(
  is.na(analysis_daily$electricity_feed_in_kwh) | is.na(analysis_daily$heat_kwh),
  NA_real_,
  pmin(analysis_daily$electricity_feed_in_kwh, analysis_daily$heat_kwh)
)
analysis_daily$remaining_heat_after_pv_surplus_kwh <- ifelse(
  is.na(analysis_daily$electricity_feed_in_kwh) | is.na(analysis_daily$heat_kwh),
  NA_real_,
  pmax(analysis_daily$heat_kwh - analysis_daily$electricity_feed_in_kwh, 0)
)
analysis_daily$pv_generation_to_load_ratio <- safe_divide(
  analysis_daily$pv_kwh,
  analysis_daily$real_electricity_kwh
)
analysis_daily$potential_pv_to_heat_ratio <- safe_divide(
  analysis_daily$potential_pv_to_heat_kwh,
  analysis_daily$heat_kwh
)
analysis_daily$electricity_kwh_per_night <- safe_divide(
  analysis_daily$electricity_kwh,
  analysis_daily$nights
)
analysis_daily$real_electricity_kwh_per_night <- safe_divide(
  analysis_daily$real_electricity_kwh,
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
  analysis_daily$real_electricity_kwh
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
analysis_daily$real_electricity_guest_impact_share <- safe_divide(
  analysis_daily$real_electricity_guest_impact_kwh,
  analysis_daily$real_electricity_kwh
)
analysis_daily$heat_guest_impact_share <- safe_divide(
  analysis_daily$heat_guest_impact_kwh,
  analysis_daily$heat_kwh
)
analysis_daily$real_electricity_guest_impact_seasonal_share <- safe_divide(
  analysis_daily$real_electricity_guest_impact_seasonal_kwh,
  analysis_daily$real_electricity_kwh
)
analysis_daily$heat_guest_impact_seasonal_share <- safe_divide(
  analysis_daily$heat_guest_impact_seasonal_kwh,
  analysis_daily$heat_kwh
)
analysis_daily$electricity_7d_avg_kwh <- rolling_mean(analysis_daily$electricity_kwh, 7L)
analysis_daily$real_electricity_7d_avg_kwh <- rolling_mean(analysis_daily$real_electricity_kwh, 7L)
analysis_daily$heat_7d_avg_kwh <- rolling_mean(analysis_daily$heat_kwh, 7L)
analysis_daily$pv_7d_avg_kwh <- rolling_mean(analysis_daily$pv_kwh, 7L)
analysis_daily$nights_7d_avg <- rolling_mean(analysis_daily$nights, 7L)
analysis_daily$temp_mean_7d_avg_c <- rolling_mean(analysis_daily$temp_mean_c, 7L)
analysis_daily$heat_weather_adjusted_7d_avg_kwh <- rolling_mean(analysis_daily$heat_weather_residual_kwh, 7L)
analysis_daily$heat_weather_adjusted_per_night_7d_avg_kwh <- rolling_mean(
  analysis_daily$heat_weather_adjusted_per_night_kwh,
  7L
)
analysis_daily$real_electricity_no_guest_baseline_seasonal_7d_avg_kwh <- rolling_mean(
  analysis_daily$real_electricity_no_guest_baseline_seasonal_kwh,
  7L
)
analysis_daily$real_electricity_guest_impact_seasonal_7d_avg_kwh <- rolling_mean(
  analysis_daily$real_electricity_guest_impact_seasonal_kwh,
  7L
)
analysis_daily$heat_no_guest_baseline_seasonal_7d_avg_kwh <- rolling_mean(
  analysis_daily$heat_no_guest_baseline_seasonal_kwh,
  7L
)
analysis_daily$heat_guest_impact_seasonal_7d_avg_kwh <- rolling_mean(
  analysis_daily$heat_guest_impact_seasonal_kwh,
  7L
)
invalid_self_consumption_bounds <- !is.na(analysis_daily$electricity_self_consumption_kwh) &
  (
    analysis_daily$electricity_self_consumption_kwh < -1e-8 |
      (
        !is.na(analysis_daily$pv_kwh) &
          analysis_daily$electricity_self_consumption_kwh > analysis_daily$pv_kwh + 1e-8
      )
  )
if (any(invalid_self_consumption_bounds)) {
  stop("Self-consumption invariant failed: electricity_self_consumption_kwh must stay within [0, pv_kwh]")
}
invalid_real_electricity_balance <- !is.na(analysis_daily$real_electricity_kwh) &
  !is.na(analysis_daily$electricity_kwh) &
  !is.na(analysis_daily$electricity_self_consumption_kwh) &
  abs(
    analysis_daily$real_electricity_kwh -
      (analysis_daily$electricity_kwh + analysis_daily$electricity_self_consumption_kwh)
  ) > 1e-8
if (any(invalid_real_electricity_balance)) {
  stop("Real electricity invariant failed: real_electricity_kwh must equal grid electricity plus self-consumption")
}
real_electricity_guest_summary_annual <- analysis_guest_impact[analysis_guest_impact$metric_key == "real_electricity", , drop = FALSE]
heat_guest_summary_annual <- analysis_guest_impact[analysis_guest_impact$metric_key == "heat", , drop = FALSE]
analysis_guest_impact_seasonal <- align_segmented_summary_to_applied_series(
  analysis_guest_impact_seasonal,
  analysis_daily,
  real_electricity_guest_summary_annual,
  "real_electricity",
  "real_electricity_kwh",
  "real_electricity_no_guest_baseline_seasonal_kwh",
  "real_electricity_guest_impact_seasonal_kwh",
  "real_electricity_guest_impact_segment"
)
analysis_guest_impact_seasonal <- align_segmented_summary_to_applied_series(
  analysis_guest_impact_seasonal,
  analysis_daily,
  heat_guest_summary_annual,
  "heat",
  "heat_kwh",
  "heat_no_guest_baseline_seasonal_kwh",
  "heat_guest_impact_seasonal_kwh",
  "heat_guest_impact_segment"
)

daily_real_electricity_model_gap <- analysis_daily$real_electricity_modeled_total_seasonal_kwh -
  (analysis_daily$real_electricity_no_guest_baseline_seasonal_kwh + analysis_daily$real_electricity_guest_impact_seasonal_kwh)
daily_heat_model_gap <- analysis_daily$heat_modeled_total_seasonal_kwh -
  (analysis_daily$heat_no_guest_baseline_seasonal_kwh + analysis_daily$heat_guest_impact_seasonal_kwh)
if (any(!is.na(daily_real_electricity_model_gap) & abs(daily_real_electricity_model_gap) > 1e-8)) {
  stop("Real electricity seasonal model invariant failed: modeled total != baseline + guest impact")
}
if (any(!is.na(daily_heat_model_gap) & abs(daily_heat_model_gap) > 1e-8)) {
  stop("Heat seasonal model invariant failed: modeled total != baseline + guest impact")
}
invalid_real_electricity_guest_rows <- !is.na(analysis_daily$real_electricity_guest_impact_segment) &
  analysis_daily$real_electricity_guest_impact_segment != "all_days_fallback" &
  !is.na(analysis_daily$real_electricity_guest_impact_seasonal_kwh) &
  analysis_daily$real_electricity_guest_impact_seasonal_kwh < -1e-8
invalid_heat_guest_rows <- !is.na(analysis_daily$heat_guest_impact_segment) &
  analysis_daily$heat_guest_impact_segment != "all_days_fallback" &
  !is.na(analysis_daily$heat_guest_impact_seasonal_kwh) &
  analysis_daily$heat_guest_impact_seasonal_kwh < -1e-8
if (any(invalid_real_electricity_guest_rows)) {
  stop("Real electricity seasonal guest impact must not be negative for applied seasonal segments")
}
if (any(invalid_heat_guest_rows)) {
  stop("Heat seasonal guest impact must not be negative for applied seasonal segments")
}

analysis_monthly <- build_monthly_dataset(analysis_daily, fernwaerme$monthly)
raw_monthly <- analysis_monthly
monthly_real_electricity_model_gap <- analysis_monthly$real_electricity_modeled_total_seasonal_kwh -
  (analysis_monthly$real_electricity_no_guest_baseline_seasonal_kwh + analysis_monthly$real_electricity_guest_impact_seasonal_kwh)
monthly_heat_model_gap <- analysis_monthly$heat_modeled_total_seasonal_kwh -
  (analysis_monthly$heat_no_guest_baseline_seasonal_kwh + analysis_monthly$heat_guest_impact_seasonal_kwh)
if (any(!is.na(monthly_real_electricity_model_gap) & abs(monthly_real_electricity_model_gap) > 1e-8)) {
  stop("Real electricity monthly seasonal model invariant failed: modeled total != baseline + guest impact")
}
if (any(!is.na(monthly_heat_model_gap) & abs(monthly_heat_model_gap) > 1e-8)) {
  stop("Heat monthly seasonal model invariant failed: modeled total != baseline + guest impact")
}
analysis_seasonal <- build_seasonal_dataset(analysis_daily)
analysis_event_summary <- build_event_summary(analysis_daily)
seasonal_pricing <- data.frame(
  season_cluster = c("summer", "winter"),
  season_label = c("Sommer", "Winter"),
    assumed_revenue_basis = c(
      "Geschaetzte Appartement-Naechte = Summe min(3, ceil(Naechtigungen / 2))",
      "Geschaetzte Appartement-Naechte = Summe min(3, ceil(Naechtigungen / 2))"
    ),
  assumed_revenue_eur_per_night = c(75, 85),
  stringsAsFactors = FALSE
)
analysis_guest_value <- build_guest_value_dataset(
  analysis_guest_impact_seasonal,
  analysis_daily,
  seasonal_pricing,
  electricity_tariff_gross_eur_per_kwh = 0.15
)
real_electricity_guest_summary <- analysis_guest_impact[analysis_guest_impact$metric_key == "real_electricity", , drop = FALSE]
heat_guest_summary <- analysis_guest_impact[analysis_guest_impact$metric_key == "heat", , drop = FALSE]
required_model_ids <- c(
  "model1_hdd18",
  "model2_hdd18_has_guests_flag",
  "model3_hdd18_has_guests_flag_nights"
)
real_electricity_model_comparison_required <- analysis_model_comparison[
  analysis_model_comparison$metric_key == "real_electricity" &
    analysis_model_comparison$model_id %in% required_model_ids,
  ,
  drop = FALSE
]
heat_model_comparison_required <- analysis_model_comparison[
  analysis_model_comparison$metric_key == "heat" &
    analysis_model_comparison$model_id %in% required_model_ids,
  ,
  drop = FALSE
]
real_electricity_fallback_segments <- unique(as.character(
  analysis_guest_impact_seasonal$segment_key[
    analysis_guest_impact_seasonal$metric_key == "real_electricity" &
      analysis_guest_impact_seasonal$segment_source != "segment"
  ]
))
heat_fallback_segments <- unique(as.character(
  analysis_guest_impact_seasonal$segment_key[
    analysis_guest_impact_seasonal$metric_key == "heat" &
      analysis_guest_impact_seasonal$segment_source != "segment"
  ]
))
real_electricity_fallback_segments <- real_electricity_fallback_segments[!is.na(real_electricity_fallback_segments) & nzchar(real_electricity_fallback_segments)]
heat_fallback_segments <- heat_fallback_segments[!is.na(heat_fallback_segments) & nzchar(heat_fallback_segments)]

pv_missing_days <- as.character(raw_daily$date[is.na(raw_daily$pv_kwh)])
feed_in_missing_days <- as.character(raw_daily$date[is.na(raw_daily$electricity_feed_in_kwh)])
heat_missing_days <- as.character(raw_daily$date[is.na(raw_daily$heat_kwh)])
occupancy_missing_days <- as.character(raw_daily$date[is.na(raw_daily$nights)])
electricity_missing_days <- as.character(raw_daily$date[is.na(raw_daily$electricity_kwh)])
real_electricity_missing_days <- as.character(analysis_daily$date[is.na(analysis_daily$real_electricity_kwh)])
weather_missing_days <- as.character(raw_daily$date[is.na(raw_daily$temp_mean_c)])
winter_rows <- analysis_daily$season_cluster == "winter"
feed_in_exceeds_pv_days_count <- safe_sum(analysis_daily$feed_in_exceeds_pv_flag)

metadata <- list(
  generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
  timezone = "Europe/Vienna",
  source_files = list(
    strom = normalizePath(strom$path, winslash = "/", mustWork = TRUE),
    strom_einspeisung = normalizePath(feed_in$path, winslash = "/", mustWork = TRUE),
    fernwaerme = normalizePath(fernwaerme$path, winslash = "/", mustWork = TRUE),
    fronius = normalizePath(file.path(data_dir, "fronius"), winslash = "/", mustWork = TRUE),
    meldewesen = normalizePath(file.path(data_dir, "meldewesen"), winslash = "/", mustWork = TRUE),
    wetter = normalizePath(wetter$path, winslash = "/", mustWork = TRUE)
  ),
  source_metadata = list(
    strom = strom$meta,
    strom_einspeisung = feed_in$meta,
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
      strom_einspeisung_quarter_hour = nrow(feed_in$raw),
      raw_daily = nrow(raw_daily),
      analysis_daily = nrow(analysis_daily),
      raw_monthly = nrow(raw_monthly),
      analysis_monthly = nrow(analysis_monthly),
      analysis_seasonal = nrow(analysis_seasonal),
      analysis_event_summary = nrow(analysis_event_summary),
      analysis_guest_impact = nrow(analysis_guest_impact),
      analysis_guest_impact_seasonal = nrow(analysis_guest_impact_seasonal),
      analysis_guest_value = nrow(analysis_guest_value),
      analysis_model_comparison = nrow(analysis_model_comparison),
      weather_daily = nrow(wetter$daily)
    ),
    missing_days = list(
      electricity = electricity_missing_days,
      strom_einspeisung = feed_in_missing_days,
      real_electricity = real_electricity_missing_days,
      fernwaerme = heat_missing_days,
      pv = pv_missing_days,
      occupancy = occupancy_missing_days,
      weather = weather_missing_days
    ),
    weather_flags = list(
      precipitation_negative_days = safe_sum(analysis_daily$precipitation_negative_flag),
      snow_depth_missing_days = safe_sum(analysis_daily$snow_depth_missing_flag)
    ),
    energy_flags = list(
      feed_in_exceeds_pv_days = feed_in_exceeds_pv_days_count
    )
  ),
  summary = list(
    total_electricity_kwh = safe_sum(raw_daily$electricity_kwh),
    total_grid_electricity_kwh = safe_sum(raw_daily$electricity_kwh),
    total_electricity_feed_in_kwh = safe_sum(raw_daily$electricity_feed_in_kwh),
    total_electricity_self_consumption_kwh = safe_sum(analysis_daily$electricity_self_consumption_kwh),
    total_real_electricity_kwh = safe_sum(analysis_daily$real_electricity_kwh),
    total_heat_kwh = safe_sum(raw_daily$heat_kwh),
    total_heat_volume_m3 = safe_sum(raw_daily$heat_volume_m3),
    total_pv_kwh_known_days = safe_sum(raw_daily$pv_kwh),
    pv_days_available = sum(!is.na(raw_daily$pv_kwh)),
    feed_in_days_available = sum(!is.na(raw_daily$electricity_feed_in_kwh)),
    real_electricity_days_available = sum(!is.na(analysis_daily$real_electricity_kwh)),
    electricity_autarky_ratio = safe_divide(
      safe_sum(analysis_daily$electricity_self_consumption_kwh),
      safe_sum(analysis_daily$real_electricity_kwh)
    ),
    pv_self_consumption_ratio = safe_divide(
      safe_sum(analysis_daily$electricity_self_consumption_kwh),
      safe_sum(raw_daily$pv_kwh)
    ),
    total_nights = safe_sum(raw_daily$nights),
    total_estimated_occupied_apartment_nights = safe_sum(analysis_daily$occupied_apartments_estimated),
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
    real_electricity_additional_per_guest_night_kwh = real_electricity_guest_summary$additional_per_guest_night_kwh[1],
    real_electricity_additional_per_guest_night_pct = real_electricity_guest_summary$additional_per_guest_night_pct_vs_baseline[1],
    real_electricity_guest_attributed_kwh = safe_sum(analysis_daily$real_electricity_guest_impact_seasonal_kwh),
    real_electricity_guest_attributed_share = safe_divide(
      safe_sum(analysis_daily$real_electricity_guest_impact_seasonal_kwh),
      safe_sum(analysis_daily$real_electricity_kwh)
    ),
    real_electricity_model_residual_kwh = safe_sum(analysis_daily$real_electricity_model_residual_seasonal_kwh),
    heat_additional_per_guest_night_kwh = heat_guest_summary$additional_per_guest_night_kwh[1],
    heat_additional_per_guest_night_pct = heat_guest_summary$additional_per_guest_night_pct_vs_baseline[1],
    heat_guest_attributed_kwh = safe_sum(analysis_daily$heat_guest_impact_seasonal_kwh),
    heat_guest_attributed_share = safe_divide(
      safe_sum(analysis_daily$heat_guest_impact_seasonal_kwh),
      safe_sum(analysis_daily$heat_kwh)
    ),
    heat_model_residual_kwh = safe_sum(analysis_daily$heat_model_residual_seasonal_kwh),
    electricity_replacement_intervals = safe_sum(raw_daily$electricity_replacement_intervals),
    electricity_replacement_share = safe_divide(
      safe_sum(raw_daily$electricity_replacement_intervals),
      safe_sum(raw_daily$electricity_intervals)
    ),
    precipitation_negative_days = safe_sum(analysis_daily$precipitation_negative_flag),
    feed_in_exceeds_pv_days = feed_in_exceeds_pv_days_count,
    correlation_nights_vs_grid_electricity = cor_or_na(analysis_daily$nights, analysis_daily$electricity_kwh),
    correlation_nights_vs_real_electricity = cor_or_na(analysis_daily$nights, analysis_daily$real_electricity_kwh),
    correlation_nights_vs_heat = cor_or_na(analysis_daily$nights, analysis_daily$heat_kwh),
    correlation_heat_vs_hdd18 = cor_or_na(analysis_daily$heat_kwh, analysis_daily$hdd18),
    correlation_heat_vs_temp_mean = cor_or_na(analysis_daily$heat_kwh, analysis_daily$temp_mean_c),
    correlation_heat_vs_nights_winter = cor_or_na(
      analysis_daily$nights[winter_rows],
      analysis_daily$heat_kwh[winter_rows]
    ),
    correlation_grid_electricity_vs_pv = cor_or_na(analysis_daily$electricity_kwh, analysis_daily$pv_kwh),
    correlation_real_electricity_vs_pv = cor_or_na(analysis_daily$real_electricity_kwh, analysis_daily$pv_kwh),
    max_abs_monthly_heat_delta_kwh = safe_max(abs(raw_monthly$heat_delta_vs_export_kwh))
  ),
  models = list(
    heat_weather_baseline = heat_weather_model$summary,
    guest_impact = list(
      real_electricity = real_electricity_guest_impact_model$model_summary,
      heat = heat_guest_impact_model$model_summary,
      baseline_definition = "modellierte Referenz ohne Gaeste"
    ),
    guest_impact_segmented = list(
      segment_col = "season_cluster",
      segments = season_levels,
      fallback_days = list(
        real_electricity = safe_sum(analysis_daily$real_electricity_guest_impact_segment == "all_days_fallback"),
        heat = safe_sum(analysis_daily$heat_guest_impact_segment == "all_days_fallback")
      ),
      fallback_segments = list(
        real_electricity = real_electricity_fallback_segments,
        heat = heat_fallback_segments
      )
    ),
    validation_model_comparison = list(
      required_model_ids = required_model_ids,
      real_electricity = real_electricity_model_comparison_required,
      heat = heat_model_comparison_required
    )
  ),
  assumptions = list(
    pricing = list(
      summer_apartment_rate_eur_per_night = 75,
      winter_apartment_rate_eur_per_night = 85,
      electricity_tariff_gross_eur_per_kwh = 0.15,
      heat_tariff_gross_eur_per_kwh = NA_real_
    ),
    economic_interpretation = list(
      occupancy_model_unit = "guest_nights",
      revenue_assumption_unit = "estimated_occupied_apartment_nights",
      note = paste(
        "Revenue scenarios estimate sold apartment nights as",
        "sum(min(3, ceiling(guest_nights / 2))) by day and season."
      )
    ),
    guest_impact_baseline = list(
      label = "modellierte Referenz ohne Gaeste",
      note = paste(
        "The no-guest baseline is a modeled counterfactual with guest-related inputs set to zero.",
        "It is not a physical idle-state measurement."
      )
    )
  ),
  notes = c(
    "Fronius data are missing for 2025-11-21, 2025-11-22, 2025-11-24 and 2025-12-25. Missing values stay NA and are not imputed.",
    "Fernwaerme export includes a closing meter row for 2026-01-01. This row is excluded from the 2025 day-level dataset.",
    "Strom timestamps are parsed in Europe/Vienna and include DST transitions with 92 and 100 quarter-hour intervals on switch days.",
    "KELAG feed-in data are aggregated on the same daily basis as grid consumption. Real electricity is defined as grid electricity plus PV self-consumption.",
    ifelse(
      feed_in_exceeds_pv_days_count > 0,
      sprintf(
        "%d days have feed-in above recorded PV day totals. These days are flagged via feed_in_exceeds_pv_flag and self-consumption is clamped to the valid range.",
        feed_in_exceeds_pv_days_count
      ),
      "No days with feed-in above recorded PV day totals were detected."
    ),
    "Weather data are integrated on daily level from Geosphere station 19821. HDD uses a base temperature of 18C.",
    "Potential PV-to-heat is calculated as the theoretical overlap between measured grid feed-in and daily Fernwaerme demand.",
    "Guest impact baseline is documented as 'modellierte Referenz ohne Gaeste'. It is a model-based reference without guests, not a physical idle state.",
    "Guest impact models use seasonally segmented daily Strom models with nonlinear HDD18, sunshine, estimated occupied apartments, arrivals, occupancy changes and calendar effects; Fernwaerme uses HDD18, HDD18^2, log1p(guest_nights), occupied_apartments_estimated, arrivals and calendar effects.",
    "Validation output analysis_model_comparison.csv compares Verbrauch ~ hdd18, + has_guests_flag and + nights against extended operating models.",
    "Guest value estimates convert guest nights to occupied apartment nights via min(3, ceil(guest_nights / 2)) on each day with guests.",
    "Line charts for baseline vs. guest-driven consumption use season-segmented daily models and monthly aggregation to reduce distortion from mixed summer/winter behavior.",
    ifelse(
      length(heat_fallback_segments),
      paste(
        "Segment fallback remains visible in analysis_guest_impact_seasonal.csv and the dashboard.",
        "Fallback segments for Fernwaerme:",
        paste(heat_fallback_segments, collapse = ", "),
        "."
      ),
      "No Fernwaerme seasonal fallback segments were required after the todo4 model upgrade."
    ),
    "Seasonal guest value scenarios use apartment-night prices of 75 EUR in summer and 85 EUR in winter.",
    "A heat tariff is not yet available in the repository. Additional heating cost per guest night is therefore shown as kWh and as a gross-price formula, not as a fixed EUR amount.",
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
  feed_in$raw,
  file.path(output_dir, "raw_feed_in_quarter_hour.csv"),
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
write.csv(
  analysis_guest_impact_seasonal,
  file.path(output_dir, "analysis_guest_impact_seasonal.csv"),
  row.names = FALSE
)
write.csv(
  analysis_guest_value,
  file.path(output_dir, "analysis_guest_value.csv"),
  row.names = FALSE
)
write.csv(
  analysis_model_comparison,
  file.path(output_dir, "analysis_model_comparison.csv"),
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
    feed_in_quarter_hour = list(
      points = to_point_pairs(feed_in$raw$timestamp_ms, feed_in$raw$kwh),
      replacement_points = to_point_pairs(
        feed_in$raw$timestamp_ms[feed_in$raw$status_class == "replacement"],
        feed_in$raw$kwh[feed_in$raw$status_class == "replacement"]
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
    guest_impact = analysis_guest_impact,
    guest_impact_seasonal = analysis_guest_impact_seasonal,
    guest_value = analysis_guest_value,
    model_comparison = analysis_model_comparison
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
