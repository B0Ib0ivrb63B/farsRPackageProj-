#--------------------------------------------------
# roxygen2 style comments for function 'fars_read'
#--------------------------------------------------
#' @title fars_read
#'
#' @description Function to read a semicolon (;) delimited file (including csv, tsv, .gz, .bz2, or .zip)
#' into a tibble from data from the US National Highway Traffic Safety Administration's Fatality Analysis
#' Reporting System. SOURCE: http://www.nhtsa.gov/Data/Fatality-Analysis-Reporting-System-(FARS)
#'
#' @param filename filename of data file
#'
#' @return A tibble: <number of rows> x 50 cols
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @examples
#'  \dontrun{
#'   fars_read("C://User//data.zip")
#'  }
#'
#' @note Using a non-existant filename will result in a trapped error.
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



#--------------------------------------------------
# roxygen2 style comments for function 'make_filename'
#--------------------------------------------------
#' @title make_filename
#'
#' @description Function to create a string to be used as a filename in the format "accident_<year>.csv.bz2".
#'
#' @param year Integer indicating the year desired
#'
#' @return Char vector Filename in the format "accident_<year>.csv.bz2"
#'
#' @examples
#'  \dontrun{
#'   make_filename(99)
#'   make_filename(2018)
#'  }
#'
#' @note Using anything other than an integer for the year paramenter will result in an error.
#'
#' @export
#'

make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}




#------------------------------------------------------
# roxygen2 style comments for function 'fars_read_years'
#------------------------------------------------------
#' @title fars_read_years
#'
#' @description Function to read data from the US National Highway Traffic Safety
#' Administration's Fatality Analysis Reporting System.
#' by individual file year, placing into a tibble (dataframe)
#'
#' @param year Integer list or vector of the year(s) desired
#'
#' @return A tibble: <number of rows> x 2 cols (MONTH and year)
#'
#' @importFrom magrittr "%>%"
#' @importFrom dplyr mutate select
#'
#' @examples
#'  \dontrun{
#'   fars_read_years(2013)
#'   fars_read_years((2013:2015))
#'   fars_read_years(c(2013:2015))
#'  }
#'
#' @note Requires package 'magrittr'.
#'
#' @export
#'

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




#----------------------------------------------------------
# roxygen2 style comments for function 'fars_summarize_years'
#----------------------------------------------------------
#' @title fars_summarize_years
#'
#' @description Function to read data from the US National Highway Traffic Safety
#' Administration's Fatality Analysis Reporting System.
#' and summarize number of observations for each month over one or more years
#' into a tibble (dataframe) 12 x <number of years>
#'
#' @param year Integer list or vector of the year(s) desired
#'
#' @return A tibble: 12 x <number of years> (MONTH, year1, year2, ...)
#'
#' @importFrom magrittr "%>%"
#' @importFrom dplyr group_by summarize
#' @importFrom tidyr spread
#'
#' @examples
#'  \dontrun{
#'   fars_summarize_years(2013)
#'   fars_summarize_years((2013:2015))
#'   fars_summarize_years(c(2013:2015))
#'  }
#'
#' @note Requires package 'magrittr'.
#'
#' @export
#'
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n())  %>%  # e.g. num observations within a group
    tidyr::spread(year, n)          # e.g. groups all observations into sepaprate 'year' cols
}






#-----------------------------------------------------
# roxygen2 style comments for function 'fars_map_state'
#-----------------------------------------------------
#' @title fars_map_state
#'
#' @description Function to read data from the US National Highway Traffic Safety
#' Administration's Fatality Analysis Reporting System.
#' and plot the of observations for the given year on a png state map
#' based on latitude and longitude of the observations.
#'
#' @param state.num Int vector of length 1 (Alabama = 1, Wyoming = 50, etc)
#' @param year Int vector of length 1 of the year desired (2013, 2014, etc)
#'
#' @return A png file with the number of observations plotted on a state map
#'
#' @importFrom magrittr "%>%"
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#'
#' @examples
#'  \dontrun{
#'   fars_map_state(1,2013)
#'   fars_map_state(12,2014)
#'  }
#'
#' @note parameters 'state.num' and 'year' must each be a length 1 vector
#'
#' @export
#'
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

