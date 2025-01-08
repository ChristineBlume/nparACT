#' nparACT_base
#'
#' Function returns IS, IV, RA, L5, M10 as well as L5 and M10 start times for single actigraphy files.
#' The function also returns a classic dual day plot for the data, a plot of minutewise averages across
#' 24 hours as well as a plot for hourly averages across 24 hours.
#'
#' @param data Data.frame containing the actigraphy data. Actigraphy files must contain
#' a first column with a date/time vector, which must be of form YYYY-MM-DD
#' HH:MM:SS or YYYY/MM/DD HH:MM:SS, i.e. in an unambiguous date format and a
#' second column with actigraphy values.
#' @param SR Sampling rate in Hz.
#' @param cutoff Can be used to define a cutoff for actigraphy data. Default value is 1, i.e. all
#' values are taken into account.
#' @param plot If TRUE plots are produced. Default is TRUE.
#' @param fulldays If TRUE only data from multiples of 24 hours (i.e. full days) are included while
#' the rest of the data are discarded. Default is TRUE.
#' @param quiet If TRUE will supress all warnings.
#' @return Data frame containing IS, IV and RA values. Besides this, L5 and M10 values are given along with
#' the respective start times.
#' @examples
#' # example function nparACT
#' data(sleepstudy)
#' r <- nparACT_base(sleepstudy, SR = 4/60)
#' @export
nparACT_base <- function(data, SR, cutoff = 1, plot = FALSE, fulldays = TRUE, quiet = FALSE) {

  if (is.data.frame(data) == FALSE) {
    data = as.data.frame(data)
  }

  if (ncol(data) == 2) {
    if (!inherits(data[[1]], c("Date", "POSIXt"))) {
      data[[1]] <- lubridate::parse_date_time(data[[1]], c("y-m-d H:M:S", "y-m-d"), quiet = quiet)
    }

    if (!is.numeric(data[[2]])) {
      data[[2]] <- as.numeric(as.character(data[[2]]))
    }

    data.table::setnames(data, new = c("time", "activity"))
  }

  if (ncol(data) == 3) {
    if (!inherits(data[[2]], c("Date", "POSIXt"))) {
      data[[2]] <- lubridate::parse_date_time(data$time, "H:M:S", quiet = quiet)
    }

    if (!is.numeric(data[[3]])) {
      data[[3]] <- as.numeric(as.character(data[[3]]))
    }

    data.table::setnames(data, new = c("date", "time", "activity"))
    data[["date"]] <- NULL
  }

  if (any(is.na(data[["activity"]])) == TRUE) stop("Please check your data! It must not contain NAs")

  bin_hr <- 60
  a <- nrow(data)
  e <- SR * 60 ## samples per minute
  m <- bin_hr * SR * 60 ## samples per hour
  full_days <- floor(a / (e * bin_hr * 24))

  ## --- Cut data to full days
  if (fulldays == TRUE) {
    data <- data[1:(e * bin_hr * 24 * full_days), ]
  }
  a <- nrow(data)
  b <- floor(a / (SR * 60)) ## full minutes recorded
  ## ------------------------------------------

  ## ---- Filtering, Cutoff for classification as movement
  data[["activity"]] = nparACT_filt(data, cutoff)
  ## ------------------------------------------

  ## ---- Calculate average for each minute (needed if SR != 1/60)
  if (SR != 1 / 60) {
    data_min <- nparACT_data_min(b, SR, data)
  }  else {
    data_min <- .subset2(data, "activity")
  }
  ## ------------------------------------------

  ## ---- Calculate hourly averages
  data_hrs <- nparACT_data_hrs(data, a, m)
  ## -----------------------------------------------------------------------------

  ## ---- Plot hourly data
  if (plot == TRUE) {
    nparACT_plot_hourly(data, data_hrs, SR)
  }

  ## ---- IS/IV calculation (based on data_hrs!)
  result_ISIV <- nparACT_ISIV(data_hrs, bin_hr)
  IS <- result_ISIV[1]
  IV <- result_ISIV[2]
  ## ---------------------------------------------------------------------------------

  ## ---------- Relative Amplitude (RA) calculation
  ## ---- Minutewise averages across 24hrs
  minaverage <- nparACT_minaverage(a, data_min)
  ## --------------------------------

  ## ---- Plot Minutewise averages
  if (plot == TRUE) {
    start.time <- NULL
    nparACT_plot_minaverage(data, minaverage, a, SR)
  }
  ## --------------------------------

  ## ---- Plot Hourly averages
  if (plot == TRUE) {
    nparACT_plot_hraverage(data, minaverage, a, SR)
  }
  ## --------------------------------

  ## ---- L5, M10, RA calculation
  result_RA <- nparACT_L5M10(data, minaverage, a, SR)
  L5 <- result_RA[1]
  L5_starttime <- result_RA[2]
  M10 <- result_RA[3]
  M10_starttime <- result_RA[4]
  RA <- result_RA[5]

  nparACT_result <- data.frame(IS, IV, RA, L5, L5_starttime, M10, M10_starttime)
  return(nparACT_result)
}
