nparACT_data_hrs = function(data, a, m) {
  data_hrs = matrix(
    vapply(seq_len((a / m)), function(x) {
      mean.default(.subset2(data, "activity")[(((x - 1) * m) + 1):((x * m))])
    }, numeric(1)),
    ncol = 1
  )

  return(data_hrs)
}

nparACT_data_min = function(b, SR, data) {
  data_min = matrix(
    vapply(seq_len(b), function(x) {
      mean.default(
        .subset2(data, "activity")[(((x - 1) * (SR * 60)) + 1):((x * (SR * 60)))]
      )
    }, numeric(1)),
    ncol = 1
  )

  return(data_min)
}

nparACT_filt = function(data, cutoff) {
  ifelse(.subset2(data, "activity") < cutoff, 0, .subset2(data, "activity"))
}

nparACT_minaverage = function(b, data_min) {
  ## ---- Minutewise averages across 24hrs -> 1440 values
  c = ceiling(b / 1440)
  data_min[c * 1440] = NA

  minaverage = matrix(
    vapply(seq_len(1440), function(x) {
      mean.default(data_min[c(seq(x, c * 1440, 1440))], na.rm = TRUE)
    }, numeric(1)),
    nrow = 1440, ncol = 1
  )

  return(minaverage)
}

## ---- Hourly averages
nparACT_hraverage_GA_loop = function(minaverage, data, a, SR) {

  hraverage = matrix(
    vapply(seq_len(24), function(x) {
      mean.default(minaverage[(((x - 1) * 60) + 1):(60 * x)])
    }),
    nrow = 24, ncol = 1
  )

  daytime <- matrix(NA)
  time <- data[["time"]]
  time <- as.character(time)

  for (v in seq(1, a, (SR * 60 * 60))) {
    daytime[v] <- time[v]
  }

  daytime <- stats::na.omit(daytime)
  daytime <- as.character(daytime)

  temp = unlist(stringr::str_split(daytime, " "))
  temp_nums = seq_along(temp)
  timeinfo = temp[(temp_nums %% 2) == 0]
  temp = unlist(stringr::str_split(timeinfo, ":"))
  temp_nums = seq_along(temp)
  timeinfo = temp[(temp_nums %% 3) == 1]

  start.time <- as.numeric(timeinfo[1])
  for_hraverage_time <- rep(seq(1, 24), 2)
  seq <- seq(start.time, length.out = 24)
  hraverage_time <- for_hraverage_time[seq]
  hraverage_time <- as.numeric(hraverage_time)
  hraverage_time[hraverage_time == 24] <- 0
  df_hraverage <- data.frame(hraverage_time, hraverage)
  df_hraverage <- df_hraverage[order(hraverage_time, hraverage), ]

  return(df_hraverage[, 2])
}
