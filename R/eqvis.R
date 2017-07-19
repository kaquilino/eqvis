# Strips the country name from the LOCATION_NAME and converts to title case.
# This function is called by eq_clean_data and is therefore not exported.
eq_location_clean <- function(loc) {
   stopifnot(class(loc) %in% c("character","factor"))
   loc <- gsub(pattern = '[A-Z ]*: *',replacement = "", x = loc)
   loc <- gsub(pattern = "\\b([A-Za-z])([A-Za-z]*)",replacement = "\\U\\1\\L\\2",x = loc,perl=TRUE)
   loc
}

#' Clean NOAA Earthquake Data
#'
#' This function cleans an NOAA earthquake significant earthquake \code{data.frame}.
#' 
#' @param data \code{data.frame} of NOAA significant earthquake dataset
#'
#' @return Returns a cleaned \code{data.frame}. The country and colon are removed from the LOCATION_NAME 
#' and it is converted to title case. A new \code{DATE} column is derived from the date parts (\code{YEAR, MONTH, DAY}). \code{LATITUDE} 
#' and \code{LONGITUDE} are converted to numeric.
#'
#' @details If \code{MONTH} or \code{DAY} are missing, 1 is used (January or 1st of month, respectively). The data.frame 
#' must include `YEAR`, `MONTH`, `DAY`, `LATITUDE`, and `LONGITUDE` columns.
#' 
#' @note The NOAA earthquake dataset can be accessed at: 
#' \url{https://www.ngdc.noaa.gov/nndc/struts/results?type_0=Exact&query_0=$ID&t=101650&s=13&d=189&dfn=signif.txt}.
#' This dataset contains information about 5,933 earthquakes around the world over an approximately 4,000 year time span. 
#' This package was developed to work with positive years (A.D.) only.  
#' 
#' @examples
#' \dontrun{eq_clean_data(sample_eq)}
#' 
#' @export
eq_clean_data <- function(data) {
   stopifnot(all(c('YEAR','MONTH','DAY','LATITUDE','LONGITUDE') %in% colnames(data)))
   
   # Derive a date using YEAR, MONTH, DAY. If missing MONTH or DAY, use 1
   data$DATE <- as.Date(paste(data$YEAR,ifelse(is.na(data$MONTH),1,data$MONTH),ifelse(is.na(data$DAY),1,data$DAY),sep="-"),"%Y-%m-%d")
   
   # Convert LATITUDE and LONGITUDE to numeric.
   if (!all(unlist(lapply(list(data$LATITUDE,data$LONGITUDE),class))=="numeric")) {
      data$LATITUDE <- is.numeric(data$LATITUDE)
      data$LONGITUDE <- is.numeric(data$LONGITUDE)
   }
   data$LOCATION_NAME <- eq_location_clean(data$LOCATION_NAME)
   data
}

#' Create NOAA Earthquake Label
#'
#' This function creates a label vector of location, magnitude and deaths for display on a map circle marker popup.
#' 
#' @param data \code{data.frame} of cleaned (see \code{eq_clean_data}) NOAA significant earthquake dataset
#'
#' @return Returns a label vector of popup text including location, magnitude, and deaths.
#'
#' @details If \code{LOCATION_NAME}, \code{EQ_PRIMARY} or \code{DEATHS} are \code{NA}, they are not included in the label text. 
#' 
#' @examples
#' \dontrun{eq_create_label(sample_eq)}
#' 
#' @export
eq_create_label <- function(data) {
   stopifnot(all(c('LOCATION_NAME','EQ_PRIMARY','DEATHS') %in% colnames(data)))
   paste(sep = "<br/>",
         ifelse(is.na(data$LOCATION_NAME),"",paste0("<b>Location: </b>",data$LOCATION_NAME)),
         ifelse(is.na(data$EQ_PRIMARY),"",paste0("<b>Magnitude: </b>",data$EQ_PRIMARY)),
         ifelse(is.na(data$DEATHS),"",paste0("<b>Total Deaths: </b>",data$DEATHS))
   )
}

#' Map NOAA Earthquake Data
#'
#' This function creates a leaflet map of the significant earthquake data provided and includes a popup showing 
#' the value of the character vector provided. The size of the circle marker is based on the magnitude of the earthquake.
#' 
#' @param data \code{data.frame} of cleaned (see \code{eq_clean_data}) NOAA significant earthquake dataset
#' 
#' @param annot_col default 'DATE'; character vector to use for circle marker popup; See \code{eq_create_label} to create 
#' text label including location, magnitude, and deaths
#'
#' @return Returns a Leaflet map widget of the earthquake data.
#' 
#' @examples
#' \dontrun{sample_eq %>% 
#' eq_clean_data() %>% 
#' dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>% 
#' dplyr::mutate(popup_text = eq_create_label(.)) %>% 
#' eq_map(annot_col = "popup_text")}
#' 
#' @importFrom magrittr "%>%"
#' @importFrom leaflet leaflet addTiles addCircleMarkers
#' @importFrom lubridate year
#' @importFrom dplyr filter mutate
#' 
#' @export
eq_map <- function(data, annot_col = 'DATE') {
   stopifnot(all(c('LATITUDE','LONGITUDE','EQ_PRIMARY', annot_col) %in% colnames(data)))
   leaflet::leaflet(data = data) %>%
      leaflet::addTiles() %>%
      leaflet::addCircleMarkers(lat = ~LATITUDE, lng = ~LONGITUDE, radius = ~EQ_PRIMARY, weight = 1, popup = ~get(annot_col))
}

#' Sample NOAA Significant Earthquake Data
#'
#' A dataset containing a subset of the NOAA significant earthquakes. Filtered for `COUNTRY` values 
#' `USA` and `MEXICO`, `YEAR` values 2000-2017 and columns used for demonstration of this package's functions. 
#' 
#' \itemize{
#'   \item YEAR. 
#'   \item MONTH.
#'   \item DAY.
#'   \item LOCATION_NAME.
#'   \item COUNTRY. 
#'   \item LATITUDE.
#'   \item LONGITUDE.
#'   \item DEATHS.
#'   \item EQ_PRIMARY. Magnitude of earthquake. 
#' }
#'
#' @source The complete dataset can be retrieved at \url{https://www.ngdc.noaa.gov/nndc/struts/results?type_0=Exact&query_0=$ID&t=101650&s=13&d=189&dfn=signif.txt}
"sample_eq"
#> [1] "sample_eq"

#' Generate Earthquake Visualizations
#'
#' \code{eqvis} provides functions to visualize the NOAA earthquake data using either a timeline or map. The earthquake 
#' data must be cleaned first using the \code{eq_clean_data} function. With this function, the country and colon are removed from the LOCATION_NAME 
#' and it is converted to title case. A new \code{DATE} column is derived from the date parts (\code{YEAR, MONTH, DAY}). \code{LATITUDE} 
#' and \code{LONGITUDE} are converted to numeric.
#' 
#' A timeline can be produced using ggplot2 with the geom_timeline/stat_timeline functions. It will plot a timeline with
#' points for each earthquake and can optionally include a size and color/fill aesthectics. If xmin and xmax aesthetics 
#' are provided, stat_timeline will subset the data. The geom_timeline_label function can be used to add a label geom. It 
#' print the \code{label} aesthetic for the top \code{n_max} earthquakes in terms of magnitude. 
#'
#' A map can be produced using \code{eq_map()}. It will display circle markers at the lat/lon locations with a radius 
#' based on the magnitude of the earthquake and popup text reflecting the annotation column provided. An optional 
#' \code{eq_create_label} function is provided that generate label text using the location name, magnitude and deaths 
#' for the earthquake.
#'
#' The data can be retrieved with the following:
#' 
#' \code{file_url <- "https://www.ngdc.noaa.gov/nndc/struts/results?type_0=Exact&query_0=$ID&t=101650&s=13&d=189&dfn=signif.txt"}
#' 
#' \code{eq <- read.delim(file_url)}
"_PACKAGE"
#> [1] "_PACKAGE"
