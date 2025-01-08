#' Return error message and abort if package not found
#'
#' @param package The name of the package that to be installed
check_package = function(package) {
  if (requireNamespace(package, quietly = TRUE)) {
    TRUE
  } else {
    rlang::abort(
      paste(package, "is not installed, please install it to use these functions"),
      class = "package_not_installed"
    )
  }
}

nparACT_plot_hourly = function(data, data_hrs, SR) {

  check_package("ggplot2")

  hours <- nrow(data_hrs)
  days <- ceiling(hours / 24)
  days_hours <- days * 24

  daytime <- matrix(NA, ncol = 1, nrow = length(seq(1, nrow(data), (SR * 60 * 60))))
  time <- as.character(data$time)

  for (v in nrow(daytime)) {
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

  mf_labeller <- function(day_count) {
    for (ee in seq_along(day_count)) {
      if (day_count[ee] == 1) {
        day_count[ee] <- "Days 1-2"
      } else if (day_count[ee] == 2) {
        day_count[ee] <- "Days 3-4"
      } else if (day_count[ee] == 3) {
        day_count[ee] <- "Days 5-6"
      } else if (day_count[ee] == 4) {
        day_count[ee] <- "Days 7-8"
      } else if (day_count[ee] == 5) {
        day_count[ee] <- "Days 9-10"
      } else if (day_count[ee] == 6) {
        day_count[ee] <- "Days 11-12"
      } else if (day_count[ee] == 7) {
        day_count[ee] <- "Days 13-14"
      } else if (day_count[ee] == 8) {
        day_count[ee] <- "Days 15-16"
      } else if (day_count[ee] == 9) {
        day_count[ee] <- "Days 17-18"
      } else if (day_count[ee] == 10) {
        day_count[ee] <- "Days 19-20"
      } else if (day_count[ee] == 11) {
        day_count[ee] <- "Days 21-22"
      } else if (day_count[ee] == 12) {
        day_count[ee] <- "Days 23-24"
      } else if (day_count[ee] == 13) {
        day_count[ee] <- "Days 25-26"
      } else if (day_count[ee] == 14) {
        day_count[ee] <- "Days 27-28"
      } else if (day_count[ee] == 15) {
        day_count[ee] <- "Days 29-30"
      }
    }
    return(day_count)
  }
  mf_labeller <- ggplot2::as_labeller(mf_labeller)


  fill <- rep(NA, start.time)

  data_hrs2 <- data_hrs
  data_hrs2 <- c(fill, data_hrs2)
  hours_plot <- length(data_hrs2)
  days_plot <- ceiling(hours_plot / 24)
  days_hours_plot <- days_plot * 24
  data_hrs2[days_hours_plot] <- NA

  hours_count <- seq(1:24)
  hours_count <- rep(hours_count, days_plot)

  count_plot <- seq(1:48) - 1
  count_plot <- rep(count_plot, length.out = days_hours_plot)

  day_count <- NA
  for (t in 1:(ceiling(days_plot / 2))) {
    day_count[((t - 1) * 48 + 1):(t * 24 * 2)] <- rep(t, 24)
  }
  day_count <- day_count[1:days_hours_plot]

  df_plot <- data.frame(day_count, count_plot, data_hrs2)

  p <- ggplot2::ggplot(df_plot, ggplot2::aes(x = count_plot, y = data_hrs2)) +
    ggplot2::geom_bar(stat = "identity", width = 1, position = ggplot2::position_dodge(width = 0.5)) +
    ggplot2::theme_bw() +
    ggplot2::facet_grid(day_count ~ ., labeller = mf_labeller) +
    ggplot2::scale_x_discrete(expand = c(0, 0), limits = as.factor(c(seq(0:47) - 1)), breaks = seq(0, 47, 2)) +
    ggplot2::expand_limits(x = -1) +
    ggplot2::xlab("Time \n (Start: 0am)") +
    ggplot2::ylab("Movement Intensity") +
    ggplot2::ggtitle("Actigraphy Plot (48 hrs)", subtitle = "Dual Day Display") +
    ggplot2::theme(
      plot.margin = ggplot2::unit(c(1, 14, 1, 14), "cm"),
      axis.text.x = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_text(face = "bold", size = 12),
      axis.title.y = ggplot2::element_text(face = "bold", size = 12, vjust = 1.2),
      plot.title = ggplot2::element_text(face = "bold", size = 14, vjust = 1)
    )
  print(p)
}

nparACT_plot_hraverage = function(data, minaverage, a, SR) {
  check_package("ggplot2")

  hraverage <- matrix(NA)
  for (i in 1:24) {
    hraverage[i] <- mean.default(minaverage[(((i - 1) * 60) + 1):(60 * i)])
  }
  daytime <- matrix(NA)
  time <- data$time
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

  for_hraverage_plot.time <- rep(seq(1, 24), 2)
  seq <- seq(start.time, length.out = 24)
  if (seq[1] == 0) {
    seq[1] = 24 ## changed 21 12 16, accounts for case in which start time is midnight
  }
  hraverage_plot_time <- for_hraverage_plot.time[seq]
  hraverage_plot_time[hraverage_plot_time == 24] <- 0
  hraverage_plot_df <- data.frame(hraverage_plot_time, hraverage)

  ppp <- ggplot2::ggplot(hraverage_plot_df, ggplot2::aes(x = hraverage_plot_time, y = hraverage)) +
    ggplot2::geom_bar(stat = "identity", width = 1, position = ggplot2::position_dodge(width = 0.5)) +
    ggplot2::scale_x_discrete(expand = c(0, 0), limits = as.factor(c(seq(0:23) - 1)), breaks = seq(0, 23)) +
    ggplot2::expand_limits(x = -1) +
    ggplot2::xlab("Time \n (Start: 0am)") +
    ggplot2::ylab("Movement Intensity") +
    ggplot2::ggtitle("Actigraphy Plot (24 hrs)", subtitle = "Average across days") +
    ggplot2::theme_bw() +
    ggplot2::theme(
      plot.margin = ggplot2::unit(c(1, 2, 1, 2), "cm"),
      axis.text.x = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_text(face = "bold", size = 12),
      axis.title.y = ggplot2::element_text(face = "bold", size = 12, vjust = 1.2),
      plot.title = ggplot2::element_text(face = "bold", size = 14, vjust = 1)
    )
  print(ppp)
}

nparACT_plot_minaverage = function(data, minaverage, a, SR) {
  check_package("ggplot2")

  daytime <- matrix(NA)
  time <- as.character(data$time)
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
  start.time_h <- as.numeric(timeinfo[1])
  start.time_min <- as.numeric(temp[2]) ## changed 21 12 16 -> select start minute, was [1] for hour previously

  for_minaverage_plot.time <- rep(seq(1, 1440), 2)
  seq <- seq(start.time_h * 60 + start.time_min, length.out = 1440) ## 21 12 16: was start.time*60 when input was hours only
  if (seq[1] == 0) { ## changed 21 12 16, accounts for case in which start time is midnight
    seq[1] = 1440
  }
  minaverage_plot_time <- for_minaverage_plot.time[seq] # time info, vectors will be joined in next step

  minaverage_plot_df <- data.frame(minaverage_plot_time, minaverage)

  pp <- ggplot2::ggplot(minaverage_plot_df, ggplot2::aes(x = minaverage_plot_time, y = minaverage)) +
    ggplot2::geom_bar(stat = "identity", width = 1, position = ggplot2::position_dodge(width = 0.5)) +
    ggplot2::theme_bw() +
    ggplot2::scale_x_discrete(limits = as.factor(c(seq(1:1440))), breaks = seq(1, 1440, 60)) +
    ggplot2::expand_limits(x = c(-30, 1470)) +
    ggplot2::xlab("Time \n (Start: 0am)") +
    ggplot2::ylab("Movement Intensity") +
    ggplot2::ggtitle("Actigraphy Plot (24 hrs)", subtitle = "Average across days") +
    ggplot2::theme(
      plot.margin = ggplot2::unit(c(1, 2, 1, 2), "cm"),
      axis.text.x = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_text(face = "bold", size = 12),
      axis.title.y = ggplot2::element_text(face = "bold", size = 12, vjust = 1.2),
      plot.title = ggplot2::element_text(face = "bold", size = 14, vjust = 1)
    )
  print(pp)
}

nparACT_plot_hraverage_GA_loop = function(matrix_hraverage) {
  check_package("ggplot2")

  GA_hraverage <- colMeans(matrix_hraverage, na.rm = TRUE)
  GA_hraverage_plot_time <- seq(1, 24) - 1

  GA_hraverage_plot_df <- data.frame(GA_hraverage_plot_time, GA_hraverage)

  p <- ggplot2::ggplot(GA_hraverage_plot_df, ggplot2::aes(x = GA_hraverage_plot_time, y = GA_hraverage)) +
    ggplot2::geom_bar(stat = "identity", width = 1, position = ggplot2::position_dodge(width = 0.5)) +
    ggplot2::theme_bw() +
    ggplot2::scale_x_discrete(expand = c(0, 0), limits = c(seq(0:23) - 1), breaks = seq(0, 23)) +
    ggplot2::expand_limits(x = -1) +
    ggplot2::xlab("Time \n (Start: 0am)") +
    ggplot2::ylab("Movement Intensity") +
    ggplot2::ggtitle("Grand Average Actigraphy Plot (24 hrs)", subtitle = "Average across days") +
    ggplot2::theme(
      plot.margin = ggplot2::unit(c(1, 2, 1, 2), "cm"),
      axis.text.x = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_text(face = "bold", size = 12),
      axis.title.y = ggplot2::element_text(face = "bold", size = 12, vjust = 1.2),
      plot.title = ggplot2::element_text(face = "bold", size = 14, vjust = 1)
    )
  print(p)
}
