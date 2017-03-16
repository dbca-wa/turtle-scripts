library(ckanr)
library(gh)

# Spatial analysis
library(rgdal)
library(sp)
library(maptools)
library(vegan)
library(PCNM)
library(adespatial)

# Sentiment analysis
library(sentiment)
library(wordcloud)

# Data management goes last to mask shared functions
library(Hmisc)
library(htmlTable)
library(httr)
library(plyr)
library(lubridate)
library(stringr)
library(tidyjson)
library(jsonlite)
library(tibble)

library(tidyverse)
library(magrittr)

# Visualisation
library(DT)
library(mapview)
library(leaflet)
library(leaflet.extras)
library(trelliscope)
library(RColorBrewer)

#' Return GeoJSON features from a WAStD API endpoint as data.table or list
#'
#' @param serializer (character) WAStD API serializer name (required)
#' @param base_url (character) Name of the new resource
#'   (default: "https://strandings.dpaw.wa.gov.au/api/1/")
#' @param query (list) API query parameters for format, limit, filtering
#'   (default: list(taxon="Cheloniidae", limit=10000, format="json"))
#' @param wastd_api_token (character) The WAStD API token
#'   (default: Sys.getenv("WASTD_APITOKEN"))
#' @param simplify (Boolean) Whether to flatten nested data frames into a single
#'   data frame with repeating unnamed groups as lists (default: TRUE), or as
#'   list of lists (simplify=FALSE)
#' @details Call the WAStD API serializer's list view with given GET parameters,
#' parse the response as text into a GeoJSON FeatureCollection.
#' Parse the FeatureCollection using jsonlite::fromJSON and return its features
#' as nested data.frame (simplify=TRUE) or as list of lists (simplify=FALSE).
#'
#' TODO: use pagination, see
#' https://cran.r-project.org/web/packages/jsonlite/vignettes/json-paging.html
#'
#' @return An S3 object of class "wastd_api" containing:
#'   content: The retrieved GeoJSON features as data.table or list
#'   serializer: The called serializer, e.g. "animal-encounters"
#'   response: The API HTTP response with all metadata
#'
#'
#' @examples \dontrun{
#' track_records <- wastd_api("turtle-nest-encounters")
#'
#' tag_records <- wastd_api("animal-encounters")
#'
#' nest_json <- wastd_api("turtle-nest-encounters",
#'                        query=list(
#'                          nest_type="hatched-nest",
#'                          limit=10000,
#'                          format="json"),
#'                        simplify=FALSE)
#' }
wastd_api <- function(serializer,
                      base_url="https://strandings.dpaw.wa.gov.au/api/1/",
                      query=list(
                        taxon="Cheloniidae",
                        limit=10000,
                        format="json"),
                      wastd_api_token=Sys.getenv("WASTD_APITOKEN"),
                      simplify=TRUE){
  require(httr)
  require(jsonlite)

  ua <- user_agent("http://github.com/parksandwildlife/turtle-scripts")

  url <- paste0(base_url, serializer)

  res <- httr::GET(url,
                   ua,
                   query = query,
                   add_headers(c(Authorization = wastd_api_token)))
  #%>% stop_for_status()

  if (res$status_code == 401) {
    stop(paste("Authorization failed. \n",
               "Set your WAStD API token as system variable with",
               "Sys.setenv(WASTD_APITOKEN=\"Token MY-WASTD-API-TOKEN\").",
               "You can find your API token under \"My Profile\" in WAStD."),
         call. = FALSE)
  }

  if (http_type(res) != "application/json") {
    stop(paste("API did not return JSON.\nIs", url ,"a valid endpoint?"),
         call. = FALSE)
  }

  text <- content(res, as = "text", encoding = "UTF-8")

  if (identical(text, "")) {
    stop("The response did not return any content.", call. = FALSE)
  }

  parsed <- jsonlite::fromJSON(text,
                            flatten = simplify,
                            simplifyVector = simplify)$features

  if (http_error(res)) {
    stop(
      sprintf(
        "WAStD API request failed [%s]\n%s\n<%s>",
        status_code(res),
        parsed$message
      ),
      call. = FALSE
    )
  }


  structure(
    list(
      content = parsed,
      serializer = serializer,
      response = res
    ),
    class = "wastd_api"
  )

}

print.wastd_api <- function(x, ...) {
  cat("<WAStD API endpoint", x$serializer, ">\n",
      "Retrieved on ", x$response$headers$date, ">\n", sep = "")
  str(x$content)
  invisible(x)
}

#------------------------------------------------------------------------------#
# Datetime helpers
#
#' Return a UTC HTTP date as GMT+08
utc_as_gmt08 <- . %>%
  parse_date_time(orders = c("YmdHMSz", "adbYHMS")) %>%
  with_tz(tzone="Australia/Perth")

#' Calculate the "turtle date" from a given datetime
as_turtle_date <- . %>% utc_as_gmt08 %>% -hours(12) %>% as_date

#------------------------------------------------------------------------------#
# Extraction helpers
#
#' Extract a numeric variable (if present, else NA) from a data.frame such as properties.observation_set
#'
#' @param ds (data.frame) e.g. a GeoJSON feature's $properties$observation_set
#' @param field (character) The desired field name, e.g. "hatching_success"
#' @param na.value (numeric) The desired value for records without a match
#' @details If the field exists as column name in the data.frame, the non-NA values
#' of the column are extracted, and the first result is returned.
#' This is somewhat clumsy as it only works for fields that are present up to once
#' per feature (e.g. hatching success, emergence success), but will be lossy,
#' and is therefore not recommended. for repeating groups (e.g. media attachments).
#' @return The first match as numeric, or the na.value (default -1).
#' @examples
#' \notrun{
#'
#' track_records <- wastd_api("turtle-nest-encounters")
#'
#' hs_column <- map(track_records$content$properties.observation_set,
#'                  get_num_field,
#'                  "hatching_success")
#'
#' # map() can be piped with %>% (es) or used as is (hs)
#' tracks <- track_records$content %>%
#'   mutate(
#'     hs =  map(properties.observation_set, get_num_field, "hatching_success"),
#'     es =  properties.observation_set %>% map(get_num_field, "emergence_success")
#' )
#' }
get_num_field <- function(ds, field, na.value=NA_real_) {
  require(purrr)
  require(magrittr)

  val <- ds %>%
    purrr::when(
      field %in% names(.) ~ magrittr::extract(., field) %>%
        na.omit %>%
        purrr::when(
          length(.) > 1, ~.[[1]],
          ~.
        ),
      ~ na.value) %>%
    as.numeric

    val
}


