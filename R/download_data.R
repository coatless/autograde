#' @importFrom utils download.file
#' @importFrom tools file_path_sans_ext
"_PACKAGE"

base_url  <- "https://rstudio.stat.illinois.edu/"

#' Fetch Data Used in Autograder
#'
#' Downloads and loads into R the problem specific data.
#' @param problem  A \code{string} indicating the problem to obtain
#'   data for.
#' @param when     A \code{string} containing either:
#'   \code{last-day}, \code{last-week}, \code{last-month}, or
#'   \code{'start-day'}. If present, then parameters \code{from} and
#'   \code{to} are ignored.
#' @param from     A \code{date} in \code{yyyy-mm-dd} format that represents the
#'   starting time. This parameter is ignored if \code{when} is given.
#' @param to       A \code{date} in \code{yyyy-mm-dd} format that represents the
#'   end time. This parameter is ignored if \code{when} is given.
#' @param save_dir A \code{string} indicating the save path to use.
#' @return A \code{data.frame} containing problem specific data.
#' @export
#' @examples
#' \dontrun{
#' ## Download all the data in one file
#' ag_fetch(problem = "twitter-proj", when = "start-day")
#'
#' ## Download data from the last day
#' ag_fetch(problem = "twitter-proj", when = "last-day")
#'
#' ## Download data for specific time intervals
#' ag_fetch(problem = "twitter-proj",
#'               from = "2016-10-09", to = "2016-10-10")
#' }
ag_fetch <- function(problem = NULL,
                     when = c("last-day", "last-week",
                              "last-month", "start-day"),
                     from = Sys.Date(), to = Sys.date(),
                     save_dir = getwd()) {

    # Verify we have a problem to grab
    if (is.null(problem)) {
        stop("Problem name must not be null.")
    } else if (length(problem) != 1) {
        stop("Please supply only 1 problem to fetch data for.")
    }

    # Obtain system date
    today <- Sys.Date()

    # Assign appropriate intervals
    if (!missing(when)) {

        when <- match.arg(when)

        when_date <- find_date(today, word_date = when)

        if(is_date(when_date)){
            interval <- day_range(from = when_date, to = today)
        } else {
            interval <- when_date
        }

    } else {

        if(!verify_date_fmt(from) && !verify_date_fmt(to)) {
            stop("Bad Date Format in `to` or `from`.")
        }

        interval <- day_range(as.Date(from), as.Date(to))

    }

    # Check if they want the "all" file
    if(!is_date(interval[1])){
        dl_rda("all", problem = problem, directory = save_dir)
        load(file.path(directory, "all.rda"))
    } else { # Only download missing days

        missing_days <- setdiff(paste0(interval),
                                tools::file_path_sans_ext(dir(save_dir), TRUE))

        for(i in seq_along(missing_days)){
            dl_rda(missing_days[i],
                   problem = paste0(problem,"/dailies"), directory = save_dir)
        }

        # Load files in
        sapply(file.path(save_dir, paste0(interval,".rda")),
               FUN = load, envir = .GlobalEnv)
    }
}

day_range <- function(from, to){
    seq(from = from, to = to, by = "day")
}

find_date <- function(today, word_date){

    switch(word_date,
           "last-day"   = today - 1,
           "last-week"  = today - 7,
           "last-month" = today - 30,
           "start-day"  = "overall",
           stop("Not a valid word date.")
           )
}

verify_date_fmt <- function(x, format = "%Y-%m-%d") {
    !is.na(as.Date(as.character(x), format = format))
}

is_date <- function(x) inherits(x, 'Date')

dl_rda <- function(x, problem = NULL, directory = getwd()){
    download.file(paste0(base_url, problem,"/", x,".rda"),
                  destfile = file.path(directory, paste0(x, ".rda")))
}
