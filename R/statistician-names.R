#' Famous Statistician Pseudonyms
#'
#' Generate a list of statistician last names to be used to mask student
#' identities.
#' @param n       An \code{integer} providing the number of last names to draw.
#' @param replace A \code{boolean} indicating if the last name can be used again.
#' @return A \code{character vector} containing the statistician last name appended
#' by a random number.
#' @export
#' @examples
#' create_stat_names(5)
#'
#' create_stat_names(10, replace = TRUE)
#' @importFrom rops is_whole
create_stat_names = function(n, replace = FALSE){

    if(length(n) != 1L || !is_whole(n) || n <= 0 ){
        stop("`n` must contain only 1 integer value")
    }

    if(nrow(db_stats) < n && !replace) replace = TRUE

    paste0(sample(db_stats$Last, n, replace), sample(n, n))
}
