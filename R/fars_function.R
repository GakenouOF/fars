#' Title A simple function for reading files or path from the system into the package
#'
#' @param filename A character string containing the name of the file or path to be read. An errors occurs if filename does not exist
#'
#' @importFrom dplyr tbl_df
#' @importFrom readr read_csv
#'
#' @return A dataframe from \code{\link{tbl_df}}
#' @export
#'
#' @examples
#' fars_read("accident_2013.csv")

fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#' Title A simple formatting function to create a filename which contains a formatted combination of input values
#'
#' @param year An integer from \code{\link{year}}
#'
#' @return A string which is the created filename for a particular year
#' @export
#'
#' @examples
#' filename<- make_filename(2013)

make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' Title A simple function for reading years data from the file imported
#'
#' @param years A numeric vector containing list of years
#' @param year An integer
#'
#' @importFrom dplyr mutate select
#' @importFrom magrittr %>%
#'
#' @return A dataframe containing list of Year by month or return NULL if \code{year} is invalid
#' @export
#'
#' @examples
#' fars_read_years(2018)
#' fars_read_years(as.list(2000, 2001, 2002))

fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>%
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}

#' Title Function for summarizing the data by years
#'
#' @param years A numeric vector with list of years
#'
#' @importFrom dplyr bind_rows group_by summarize
#' @importFrom tidyr spread
#' @importFrom magrittr %>%
#'
#' @return A dataframe with the total monthly number of occurrence in each year
#' @export
#'
#' @examples
#' fars_summarize_years(c(2013, 2014, 2015, 2018))

fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' Title Function for creating a map of occurrence by state and year
#'
#' @param state.num An integer containing unique state numbers
#' @param year An integer
#'
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#'
#' @return A map based on plots by latitude and longitude or Returns NULL if \code{data.sub}==0
#'stops if \code{state.num} is invalid, returns "invalid STATE number"
#' @export
#'
#' @examples
#' fars_map_state(12, 2013)
#' \dontrun{
#' fars_map_state(0, 2013)
#' }
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
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
