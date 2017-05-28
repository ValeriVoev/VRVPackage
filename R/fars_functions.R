#' Read a csv file and convert it to tibble data format (tbl_df which inherits from data.frame)
#'
#' This function reads in a csv file, and if it exists converts it to a tibble data frame which
#' inherits properties from data.frame class. The tibble data frame is more suitable in some cases
#' than standard data frame, as it, among other things, doesn't convert strings to factors, keeps
#' orignal variable names (e.g., if they contain a space), uses a somewhat different print method, etc.
#'
#' @param filename A path to a file. Files ending in .gz, .bz2, .xz, or .zip will be
#'          automatically uncompressed
#'
#' @return A tibble data frame. If the file cannot be found an error is generated and execution is halted
#'
#' @examples
#' fars_read("data/accident_2013.csv")
#' fars_read("data/accident_2013.csv.bz2")
#' fars_read(list.files("data", full.names = TRUE)[1])
#'
#' @note The file to be read needs to be a csv or a compressed csv. Other file formats cannot be read with this function.
#'
#' @importFrom dplyr tbl_df
#' @importFrom readr read_csv
#'
#' @export
#'

fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}


#' Construct a filename for a dataset from the Traffic Safety Administration's Fatality Analysis Reporting System
#' for a particular year
#'
#' This function constructs a filename of the type "accident_<year>.csv.bz2" which can then be
#' passed to \code{\link{fars_read}} to be loaded in the workspace as a tibble.
#'
#' @param year A year in \%Y format, e.g., \code{2013}. A vector of year values, e.g. \code{2013:2016}
#' can also be used
#'
#' @return A filename of the type "accident_<year>.csv.bz2", where year is the input. If a vector of year values
#'      is provided, the function will return a character vector of filenames
#'
#' @examples
#' make_filename(2013)
#' make_filename(2013:2016)
#'
#' @note Input should either be a 4-digit integer year number in \%Y format or
#'     coercible to one (e.g., a string \code{"2013"})
#'
#' @export
#'


make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}


#' Extract month and year variables from several Traffic Safety Administration's Fatality Analysis Reporting System
#' annual datasets
#'
#' This function constructs a list of tibbles containing the month and year variables for each year
#' (one list element per year) for a set of years (e.g. \code{2013:2015}).
#'
#' @param years A vector of year values in \%Y format, e.g., \code{2014}, or \code{2013:2015}
#'
#' @return A list containing the month an year variable for each of the years of the Traffic Safety Administration's
#' Fatality Analysis Reporting System datasets
#'
#' @examples
#' fars_read_years(2014)
#' fars_read_years(2013:2015)
#'
#' @note The data for each year should be in format "accident_<year>.csv.bz2". If a file for a given year
#' is not found a warning is printed out and the list element for that year is \code{NULL}. This function
#' uses \code{\link{make_filename}} to construct the filename to the dataset and \code{\link{fars_read}}
#' to load the dataset as a tibble.
#'
#' @importFrom dplyr mutate select
#' @importFrom magrittr "%>%"
#'
#' @export
#'


fars_read_years <- function(years) {
        lapply(years, function(yr) {
                file <- make_filename(yr)
                tryCatch({
                        dat <- fars_read(file)
                        dplyr::mutate_(dat, .dots = setNames(list(~YEAR), "year")) %>%
                                dplyr::select_(~MONTH, ~year)
                }, error = function(e) {
                        warning("invalid year: ", yr)
                        return(NULL)
                })
        })
}


#' Number of observations (cases) by month and year from the Traffic Safety Administration's
#' Fatality Analysis Reporting System annual datasets
#'
#' This function constructs a tibble (in wide format) with the number of cases by
#' month and year from a set of annual datasets.
#'
#' @param years A vector of year values in \%Y format, e.g., \code{2014}, or \code{2013:2015}
#'
#' @return A tibble (in wide format) containing the number of cases for each month and year
#'
#' @examples
#' fars_summarize_years(2014)
#' fars_summarize_years(2013:2015)
#'
#' @note The data for each year should be in format "accident_<year>.csv.bz2". The input should be provided
#' (or coercible into) as an integer vector in \%Y format (e.g., \code{2013:2015}). This function
#' uses \code{\link{fars_read_years}} to extract the month and year variables from the Traffic Safety Administration's
#' Fatality Analysis Reporting System annual datasets.
#'
#' @importFrom dplyr bind_rows group_by summarize
#' @importFrom tidyr spread
#' @importFrom magrittr "%>%"
#'
#' @export
#'


fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>%
                dplyr::group_by_(~year, ~MONTH) %>%
                dplyr::summarize_(.dots = setNames(list(~n()), "n")) %>%
                tidyr::spread_("year", "n")
}



#' Draw a map of accidents (from the Traffic Safety Administration's
#' Fatality Analysis Reporting System) for a given state and year
#'
#' This function plots a map of all accidents (except those with missing longitude and/or latitude information)
#' for a particular state and year.
#'
#' @param state.num A state number as (or coercible into) an integer for which the map will be plotted
#' @param year A year value in \%Y format, the year for which the accidents will be plotted
#'
#' @return \code{NULL}. This function is used for it's side effect, namely a map of the accidents
#'     for a given state and year
#'
#' @examples
#' \dontrun{fars_map_state(8, 2015)}
#'
#' @note The data for each year should be in format "accident_<year>.csv.bz2". If a state number is not recognized,
#'    an error will be generated. State number 2 (Alaska) cannot be plotted.
#'
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#'
#' @export
#'


fars_map_state <- function(state.num, year) {
        filename <- make_filename(year)
        data <- fars_read(paste(filename))
        state.num <- as.integer(state.num)

        if(!(state.num %in% unique(data$STATE)))
                stop("invalid STATE number: ", state.num)
        data.sub <- dplyr::filter_(data, ~STATE == state.num)
        if(nrow(data.sub) == 0L) {
                message("no accidents to plot")
                return(invisible(NULL))
        }
        is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
        is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
        with(data.sub, {
                maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
                          xlim = range(LONGITUD, na.rm = TRUE))
                graphics::points(LONGITUD, LATITUDE, pch = 46)
        })
}
