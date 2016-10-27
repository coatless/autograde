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
