#' Compute classic non-parametric actigraphy measures for multiple files
#'
#' Computes the classic non-parametric actigraphy measures:
#' interdaily stability (IS), intradaily variability (IV),
#' relative amplitude (RA), as well as the L5 and M10 values
#' and their respective start times for all actigraphy files in a specified folder.
#'
#' Each file is expected to contain either two columns (time, activity) or
#' three columns (date, time, activity). Time variables are internally
#' converted to \code{POSIXct}. Missing values in the activity signal are
#' not permitted.
#'
#' @param path Character string. Path to the folder containing actigraphy files.
#' @param SR Numeric. Sampling rate in Hertz (samples per second).
#' @param cutoff Numeric. Activity threshold used to classify movement.
#'   Defaults to 1.
#' @param plot Logical. If \code{TRUE}, generates diagnostic plots of the
#'   hourly grand-average activity profiles. Defaults to \code{TRUE}.
#' @param fulldays Logical. If \code{TRUE}, data are truncated to include
#'   only complete 24-hour periods. Defaults to \code{TRUE}.
#'
#' @details
#' Each file should contain either two columns (time, activity) or three columns (date, time, activity).
#' Time variables are converted to POSIXct. Missing activity values are not allowed.
#' The function filters activity, computes minute/hourly averages, calculates IS/IV/RA,
#' and optionally plots grand-average activity profiles.
#'
#' @return
#' A data frame with one row per file and the following columns:
#' \describe{
#'   \item{IS}{Interdaily stability}
#'   \item{IV}{Intradaily variability}
#'   \item{RA}{Relative amplitude}
#'   \item{L5}{Mean activity during the least active 5-hour period}
#'   \item{L5_starttime}{Start time of the L5 period}
#'   \item{M10}{Mean activity during the most active 10-hour period}
#'   \item{M10_starttime}{Start time of the M10 period}
#' }
#' @export
nparACT_base_loop <-
function (path, SR, cutoff=1, plot = TRUE, fulldays = TRUE){
  files <- list.files(path)
  fileext <- tools::file_ext(files[1])
  nofiles <- length(files)
  if (nofiles !=0){
    bin_hr <- 60
    nparACT_result <- matrix(NA, nofiles, 7)
    nparACT_result <- as.data.frame(nparACT_result)
    colnames(nparACT_result) <- c("IS", "IV", "RA", "L5", "L5_starttime", "M10", "M10_starttime")
    matrix_hraverage <- matrix(NA, nofiles, 24)
    for (zz in 1:nofiles){
      name <- files[zz]
      if (fileext == "txt"){
        data <- utils::read.table(paste(path,name, sep="/"), header = F)
      } else {
        data <- utils::read.csv(paste(path,name, sep="/"), header = F)
      }
      if (is.data.frame(data)==F){
        data = as.data.frame(data)
      }
      if(ncol(data) == 2){
        data[,1] <- as.POSIXct(data[,1])
        data[,2] <- as.numeric(as.character(data[,2]))
        names(data)[1] <- "time"
        names(data)[2] <- "activity"
      }else if(ncol(data) == 3){
        names(data)[1] <- "date"
        names(data)[2] <- "time"
        names(data)[3] <- "activity"
        data$date <- NULL
        data$time <- as.POSIXct(data$time, format="%H:%M:%S")
        data$activity <- as.numeric(as.character(data$activity))
      }else{
        stop("Oops, your data does not seem to have the correct format.")
      }
      if (any(is.na(data$activity)) == TRUE) stop("Please check your data! It must not contain NAs")

      a <- nrow(data)
      e <- SR*60 ## samples per minute
      m <- bin_hr*SR*60  ## samples per hour
      full_days <- floor(a/(e*bin_hr*24))

      ## --- Cut data to full days
      if (fulldays == T){
        data <- data[1:(e*bin_hr*24*full_days),]
      }
      a <- nrow(data)
      b <- floor(a/(SR*60)) ## full minutes recorded
      ## ------------------------------------------

      ## ---- Filtering, Cutoff for classification as movement
      nparACT_auxfunctions1$nparACT_filt(data, a, cutoff)
      ## ------------------------------------------

      ## ---- Calculate average for each minute (needed if SR != 1/60)
      if (SR != 1/60){
        data_min <- nparACT_auxfunctions1$nparACT_data_min(b, SR, data)
      }  else {
        data_min <- data$activity
      }
      ## ------------------------------------------

      ## ---- Calculate hourly averages
      data_hrs <- nparACT_auxfunctions1$nparACT_data_hrs(data, a, m)
      ## -----------------------------------------------------------------------------

      ## ---- IS/IV calculation (based on data_hrs!)
      result_ISIV <- nparACT_ISIVfunctions$nparACT_ISIV(data_hrs, bin_hr)
      IS <- result_ISIV[1]
      IV <- result_ISIV[2]
      nparACT_result[zz,1] <- IS
      nparACT_result[zz,2] <- IV
      ## ---------------------------------------------------------------------------------

      ## ---------- Relative Amplitude (RA) calculation
      ## ---- Minutewise averages across 24hrs
      minaverage <- nparACT_auxfunctions1$nparACT_minaverage(a, data_min)
      ## --------------------------------

      ## ----- Compute & Plot hourly averages (Grand Average Plot)
      if(plot == T){
        hraverage_sorted <- nparACT_auxfunctions1$nparACT_hraverage_GA_loop(minaverage, data, a , SR)
        matrix_hraverage[zz,] <- hraverage_sorted
      }
      ## --------------------------------------------------

      ## ---- L5, M10, RA calculation
      result_RA <- nparACT_RAfunctions$nparACT_L5M10(data, minaverage, a, SR)
      result_RA <- as.data.frame(result_RA)

      nparACT_result[zz,3] <- result_RA$RA
      nparACT_result[zz,4] <- result_RA$L5
      nparACT_result[zz,5] <- as.character(result_RA$L5_starttime)
      nparACT_result[zz,6] <- result_RA$M10
      nparACT_result[zz,7] <- as.character(result_RA$M10_starttime)
    }
    nparACT_result <- nparACT_result
    if (plot == T){
      nparACT_auxfunctions2$nparACT_plot_hraverage_GA_loop(matrix_hraverage)
    }
    return (nparACT_result)
  }else{
    stop("Oops, there are no files to be processed?")
  }
}
