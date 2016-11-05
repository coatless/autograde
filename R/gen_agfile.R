#' Autograde Single Column CSV File Format
#'
#' Creates a valid autograde file when supplied with a single prediction vector
#' @param yhat      A \code{vector} of predicted \eqn{y} values.
#' @param file.name A \code{string} to name the file (without a file extension).
#' @return A \code{.csv} with observations on each new line written to the working
#' directory given by \code{getwd()}.
#' @export
#' @examples
#'
#' # Set seed for reproducibility
#' set.seed(1337)
#'
#' # Simulate values
#' n = 100
#' beta = 2.5
#' x = beta*rnorm(n, 0, 1)
#' y = x + rnorm(n, 1, 2)
#'
#' # Create a regression model
#' mod = lm(y~x)
#'
#' ## Predict under model
#' yhat = predict(mod)
#'
#' gen_agfile(yhat,file.name="ag_org")
#'
#' ## Predict under new data
#' new = data.frame(x = seq(-3, 3, 0.5))
#' yhat2 = predict(mod, new)
#'
#' gen_agfile(yhat2, file.name="ag_new")
gen_agfile = function(yhat, file.name = "ag_sub"){

    if(is.null(yhat)){
        stop("`yhat` must be contain values.")
    }

    if((is.matrix(yhat) || is.data.frame(yhat)) && ncol(yhat) > 1){
        stop("`yhat` must either be a vector or a data.frame/matrix with one column.")
    }

    yhat = as.vector(yhat)

    if(is.null(file.name)){
        stop("`file.name` must have a value.")
    }

    # Remove file extension
    file.name = tools::file_path_sans_ext(base::basename(file.name))

    write.table(yhat, file = paste0(file.name,".csv"), sep = ",",  row.names = F, col.names = F)
    message("Wrote prediciton file ", file.name,".csv to directory ", getwd(), ".")
    message(">> Remember to submit the file to the autograder! <<")
}


#' Autograde Autoformat CSV File Format
#'
#' Creates a valid autograde file when supplied with predictions
#' @param yhat      A \code{vector}, \code{matrix}, \code{data.frame} of
#'                  predicted \eqn{y} values.
#' @param prob      A \code{string} that indicates the name of the problem the
#'                  data should conform to. This name must match a value available
#'                  via ....
#' @param prob.dir  A \code{string} containing the path to where the problem
#'                  directory is found
#' @return A \code{.csv} named by the problem name alongside the date and time
#' the predictions were made where the observations are aligned on each new
#' line written to the working directory given by \code{getwd()}.
#' @export
#' @examples
#'
#' # Set seed for reproducibility
#' set.seed(1337)
#'
#' # Simulate values
#' n = 100
#' beta = 2.5
#' x = beta*rnorm(n, 0, 1)
#' y = x + rnorm(n, 1, 2)
#'
#' # Create a regression model
#' mod = lm(y~x)
#'
#' ## Predict under model
#' yhat = predict(mod)
#'
#' gen_ag_csv(yhat, "test-proj")
#'
#' ## Predict under new data
#' new = data.frame(x = seq(-3, 3, 0.5))
#' yhat2 = predict(mod, new)
#'
#' gen_ag_csv(yhat2, "test-proj")
gen_ag_csv = function(yhat, prob, prob.dir = getwd()){

    if(is.null(yhat)){
        stop("`yhat` must be contain values.")
    }

    if(!(is.atomic(yhat) || is.matrix(yhat) || is.data.frame(yhat))){
        stop("`yhat` must either be a vector or a data.frame/matrix")
    }

    ## add logic here

    write.table(yhat, file = paste0(prob,".csv"), sep = ",",  row.names = F, col.names = F)
    message("Wrote prediciton file ", prob,".csv to directory ", getwd(), ".")
    message(">> Remember to submit the file to the autograder! <<")
}

