#' nparACT_flex_loop
#'
#' Function returns IS, IV, RA, L5, M10 as well as L5 and M10 start times for multiple actigraphy files.
#' Additionally, it also returns the L value (Lflex) and the start time for a specified number of minutes.
#' The function also returns a classic dual day plot for the data, a plot of minutewise averages across
#' 24 hours as well as a plot for hourly averages across 24 hours.
#'
#' @param path Character string containing path to location of actigraphy files. Actigraphy files
#' must contain a first column with a date/time vector, which must be of form
#' YYYY-MM-DD HH:MM:SS or YYYY/MM/DD HH:MM:SS, i.e. in an unambiguous date
#' format and a second column with numeric actigraphy values. Input files can
#' either be text or comma separated value files.
#' @param SR Sampling rate in Hz.
#' @param cutoff Can be used to define a cutoff for actigraphy data. Default value is 1, i.e. all
#' values are taken into account.
#' @param minutes Number of minutes for which the Lflex value should be computed.
#' @param plot If TRUE plots are produced. Default is TRUE.
#' @param fulldays If TRUE only data from multiples of 24 hours (i.e. full days) are included while
#' the rest of the data are discarded. Default is TRUE.
#' @param quiet If TRUE will supress all warnings.
#' @return Data frame containing IS, IV and RA values. Besides this, L5, M10 and Lflex (for the number of
#' minutes specified) values are given along with the respective start time.
#' @examples
#' # example function nparACT_flex_loop
#' data(sleepstudy)
#' name <- "sleepstudy_example"
#' newdir <- file.path(tempdir(),name)
#' dir.create(newdir, showWarnings = FALSE)
#' olddir <- setwd(newdir)
#' write.table(sleepstudy, file = "sleepstudy.txt", row.names=FALSE, col.names = FALSE)
#' r <- nparACT_flex_loop(newdir, SR = 4/60, minutes = 435)
#' setwd(olddir)
#' @export
nparACT_flex_loop <- function(path, SR, cutoff = 1, minutes, plot = FALSE, fulldays = TRUE, quiet = FALSE) {
  files <- list.files(path)
  nofiles <- length(files)

  if (nofiles == 0) {
    stop("Oops, there are no files to be processed?")
  }

  bin_hr <- 60
  nparACT_result <- matrix(NA, nofiles, 9)
  nparACT_result <- as.data.frame(nparACT_result)
  colnames(nparACT_result) <- c("IS", "IV", "RA", "L5", "L5_starttime", "M10", "M10_starttime", "Lflex", "Lflex_starttime")
  matrix_hraverage <- matrix(NA, nofiles, 24)

  for (zz in 1:nofiles){
    name <- files[zz]
    data <- data.table::fread(paste(path, name, sep = "/"), header = FALSE)

    if (is.data.frame(data) == FALSE) {
      data = as.data.frame(data)
    }
    if (ncol(data) == 2) {
      if (!inherits(data[["V1"]], c("Date", "POSIXt"))) {
        data[["V1"]] <- lubridate::parse_date_time(data[["V1"]], c("y-m-d H:M:S", "y-m-d"), quiet = quiet)
      }

      if (!is.numeric(data[["V2"]])) {
        data[["V2"]] <- as.numeric(as.character(data[["V2"]]))
      }

      data.table::setnames(data, new = c("time", "activity"))
    } else if (ncol(data) == 3) {
      if (!inherits(data[["V2"]], c("Date", "POSIXt"))) {
        data[["V2"]] <- lubridate::parse_date_time(data$time, "H:M:S", quiet = quiet)
      }

      if (!is.numeric(data[["V3"]])) {
        data[["V3"]] <- as.numeric(as.character(data[["V3"]]))
      }

      data.table::setnames(data, new = c("date", "time", "activity"))
      data[["date"]] <- NULL
    } else {
      stop("Oops, your data does not seem to have the correct format.")
    }

    if (any(is.na(data$activity)) == TRUE) stop("Please check your data! It must not contain NAs")

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

    ## ---- IS/IV calculation (based on data_hrs!)
    result_ISIV <- nparACT_ISIV(data_hrs, bin_hr)
    IS <- result_ISIV[1]
    IV <- result_ISIV[2]
    nparACT_result[zz, 1] <- IS
    nparACT_result[zz, 2] <- IV
    ## ---------------------------------------------------------------------------------

    ## ---------- Relative Amplitude (RA) calculation
    ## ---- Minutewise averages across 24hrs
    minaverage <- nparACT_minaverage(a, data_min)
    ## --------------------------------

    ## ----- Compute & Plot hourly averages (Grand Average Plot)
    if (plot == TRUE) {
      hraverage_sorted <- nparACT_hraverage_GA_loop(minaverage, data, a , SR)
      matrix_hraverage[zz, ] <- hraverage_sorted
    }
    ## --------------------------------------------------

    ## ---- L5, M10, Lflex values
    result_RA <- nparACT_L5M10Lflex(data, minaverage, a, SR, minutes)

    ## ---- Write results to results matrix
    nparACT_result[zz, 3] <- result_RA$RA
    nparACT_result[zz, 4] <- result_RA$L5
    nparACT_result[zz, 5] <- as.character(result_RA$L5_starttime)
    nparACT_result[zz, 6] <- result_RA$M10
    nparACT_result[zz, 7] <- as.character(result_RA$M10_starttime)
    nparACT_result[zz, 8] <- result_RA$Lflex
    nparACT_result[zz, 9] <- as.character(result_RA$Lflex_starttime)
    ## ------------------------------------------------------------------------------
    ## ------------------------------------------------------------------------------
  }

  if (plot == TRUE) {
    nparACT_plot_hraverage_GA_loop(matrix_hraverage)
  }

  return(nparACT_result)
}
