nparACT_L5M10 = function(data, minaverage, a, SR) {
  ## ---- L5 values
  data_L5_M10 = rep(minaverage, 2)
  L5_matrix = matrix(
    vapply(seq_len(1440), function(x) {
      mean.default(data_L5_M10[x:(299 + x)], na.rm = TRUE)
    }, numeric(1)),
    nrow = 1440, ncol = 1
  )

  ## --------------------------------

  ## ---- M10 values (most active 10h period)
  M10_matrix = matrix(
    vapply(seq_len(1440), function(x) {
      mean.default(data_L5_M10[x:(599 + x)], na.rm = TRUE)
    }, numeric(1)),
    nrow = 1440, ncol = 1
  )
  ## --------------------------------

  ## ---- Find min of L5 and max of M10
  L5 <- round(min(L5_matrix), digits = 2)
  L5_start <- which.min(L5_matrix)

  M10 <- round(max(M10_matrix), digits = 2)
  M10_start <- which.max(M10_matrix)

  daytime_minutes <- matrix(NA)
  time <- as.character(data$time)
  for (v in seq(1, a, (SR * 60))) {
    daytime_minutes[v] <- time[v]
  }
  daytime_minutes <- stats::na.omit(daytime_minutes)
  daytime_minutes <- as.character(daytime_minutes)

  L5_starttime <- daytime_minutes[L5_start]
  L5_starttime <- unlist(stringr::str_split(L5_starttime, " "))[2]

  M10_starttime <- daytime_minutes[M10_start]
  M10_starttime <- unlist(stringr::str_split(M10_starttime, " "))[2]

  ## --------------------------------
  RA <- round((M10 - L5) / (M10 + L5), digits = 2)

  return(data.frame(L5, L5_starttime, M10, M10_starttime, RA))
}

nparACT_L5M10Lflex = function(data, minaverage, a, SR, minutes) {
  ## ---- L5 values (least active 5h period)
  L5_matrix <- matrix(NA, 1440)
  data_L5_M10 <- rep(minaverage, 2)
  for (l in 1:1440) {
    for_L5 <- data_L5_M10[l:(299 + l)]
    L5_matrix[l] <- mean.default(for_L5, na.rm = TRUE)
  }
  ## --------------------------------

  ## ---- M10 values (most active 10h period)
  M10_matrix <- matrix(NA, 1440)
  for (m in 1:1440) {
    for_M10 <- data_L5_M10[m:(599 + m)]
    M10_matrix[m] <- mean.default(for_M10, na.rm = TRUE)
  }
  ## --------------------------------

  ## ---- Lflex values (flexible length)
  Lflex_matrix <- matrix(NA, 1440)
  data_L5_M10 <- rep(minaverage, 2)
  for (o in 1:1440) {
    for_Lflex <- data_L5_M10[o:((minutes - 1) + o)]
    Lflex_matrix[o] <- mean.default(for_Lflex, na.rm = TRUE)
  }
  ## --------------------------------

  ## ---- Find min of L5 and max of M10
  L5 <- round(min(L5_matrix), digits = 2)
  L5_start <- which.min(L5_matrix)

  M10 <- round(max(M10_matrix), digits = 2)
  M10_start <- which.max(M10_matrix)

  Lflex <- round(min(Lflex_matrix), digits = 2)
  Lflex_start <- which.min(Lflex_matrix)

  daytime_minutes <- matrix(NA)
  time <- data$time
  time <- as.character(time)
  for (v in seq(1, a, (SR * 60))) {
    daytime_minutes[v] <- time[v]
  }
  daytime_minutes <- stats::na.omit(daytime_minutes)
  daytime_minutes <- as.character(daytime_minutes)

  L5_starttime <- daytime_minutes[L5_start]
  L5_starttime <- unlist(stringr::str_split(L5_starttime, " "))[2]

  M10_starttime <- daytime_minutes[M10_start]
  M10_starttime <- unlist(stringr::str_split(M10_starttime, " "))[2]

  Lflex_starttime <- daytime_minutes[Lflex_start]
  Lflex_starttime <- unlist(stringr::str_split(Lflex_starttime, " "))[2]

  ## --------------------------------
  RA <- round((M10 - L5) / (M10 + L5), digits = 2)

  return(data.frame(L5, L5_starttime, M10, M10_starttime, Lflex, Lflex_starttime, RA))
}
