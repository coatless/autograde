#' Make Web Predictions
#'
#' Creates a valid autograde file for web analytics project
#' @param Rcpp,ggplot2,stringr,dplyr A single \code{numeric} value for each of the parameters.
#' @return A \code{.csv} with observations on each new line written to the working
#' directory given by \code{getwd()}.
#' @export
#' @examples
#' \dontrun{
#' # Make some predictions
#' Rcpp = 1
#' ggplot2 = 2
#' stringr = 3
#' dplyr = 4
#'
#' make_web_pred(Rcpp, ggplot2, stringr, dplyr)
#' }
make_web_pred = function(Rcpp, ggplot2, stringr, dplyr){

    a = list(Rcpp, ggplot2, stringr, dplyr)

    stopifnot(all(sapply(a, length) == 1L))

    message("Creating a 1 x 4 matrix with order:")
    message("Rcpp, ggplot2, stringer, dplyr...")

    yhat = matrix(c(Rcpp,ggplot2,stringr,dplyr), nrow = 1, ncol = 4)

    now = Sys.time()
    # Remove file extension
    file.name = paste0("web_pred_", format(now, format="%Y_%m_%d_%I_%M_%S_%p"))

    write.table(yhat, file = paste0(file.name,".csv"), sep = ",",  row.names = F, col.names = F)
    message("Wrote prediciton file ", file.name,".csv to directory ", getwd(), ".")
    message(">> Remember to submit the file to the autograder! <<")
}


#' Make Weather Predictions
#'
#' Creates a valid autograde file for Weather Project
#' @param hi,lo,mean A single \code{numeric} value for each of the parameters.
#' @return A \code{.csv} with observations on each new line written to the working
#' directory given by \code{getwd()}.
#' @export
#' @examples
#' \dontrun{
#' # Make some predictions
#' make_weather_pred(hi = 1, lo = 2, mean = 3)
#' }
make_weather_pred = function(hi,lo,mean){

    a = list(hi,lo,mean)

    stopifnot(all(sapply(a, length) == 1L))

    message("Creating a 1 x 3 matrix with order:")
    message("hi, lo, mean...")

    yhat = matrix(c(hi,lo,mean), nrow = 1, ncol = 3)

    now = Sys.time()
    # Remove file extension
    file.name = paste0("weather_pred_", format(now, format="%Y_%m_%d_%I_%M_%S_%p"))

    write.table(yhat, file = paste0(file.name,".csv"), sep = ",",  row.names = F, col.names = F)
    message("Wrote prediciton file ", file.name,".csv to directory ", getwd(), ".")
    message(">> Remember to submit the file to the autograder! <<")
}


#' Make CI Predictions
#'
#' Creates a valid autograde file for CI Mu Problem
#' @param ci90,ci95,ci99 Two \code{numeric} values for each of the parameters.
#' @return A \code{.csv} with observations on each new line written to the working
#' directory given by \code{getwd()}.
#' @export
#' @examples
#' \dontrun{
#' # Make CI AG File
#' make_ci_pred(ci90 = c(-1,1), ci95 = c(-1.5,1.5), ci99 = c(-2.5,2.5))
#' }
make_ci_pred = function(ci90, ci95, ci99){

    a = list(ci90, ci95, ci99)

    stopifnot(all(sapply(a, length) == 2L))

    message("Creating a 2 x 3 matrix with order:")
    message("ci90, ci95, ci99...")

    yhat = matrix(c(ci90, ci95, ci99), nrow = 2, ncol = 3, byrow = FALSE)

    now = Sys.time()
    # Remove file extension
    file.name = paste0("ci_pred_", format(now, format="%Y_%m_%d_%I_%M_%S_%p"))

    write.table(yhat, file = paste0(file.name,".csv"), sep = ",",  row.names = F, col.names = F)
    message("Wrote prediciton file ", file.name,".csv to directory ", getwd(), ".")
    message(">> Remember to submit the file to the autograder! <<")
}


#' Make Predictions
#'
#' Creates a valid autograde file for the prediction problems
#' @param x Ten \code{numeric} values.
#' @return A \code{.csv} with observations on each new line written to the working
#' directory given by \code{getwd()}.
#' @export
#' @examples
#' \dontrun{
#' # Make some predictions
#' make_pred(x = 1:10)
#' }
make_pred = function(x){

    stopifnot(length(x) != 10L)

    message("Creating a 10 x 1 matrix with order:")
    message("t = n + 1, t = n + 2, ... , t = n + 10")

    yhat = matrix(x, nrow = 10, ncol = 1, byrow = TRUE)

    now = Sys.time()
    # Remove file extension
    file.name = paste0("forecast_pred_", format(now, format="%Y_%m_%d_%I_%M_%S_%p"))

    write.table(yhat, file = paste0(file.name,".csv"), sep = ",",  row.names = F, col.names = F)
    message("Wrote prediciton file ", file.name,".csv to directory ", getwd(), ".")
    message(">> Remember to submit the file to the autograder! <<")
}

