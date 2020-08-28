knitr::opts_chunk$set(collapse = TRUE,
                      comment = "#>")
# devtools::install_github('Ather-Energy/ggTimeSeries')
library(here)
library(wastdr)
library(dplyr)
library(purrr)
library(tidyr)
library(magrittr)
# library(skimr)
library(leaflet)
library(RColorBrewer)
library(lubridate)
library(listviewer)
library(DT)
library(ggplot2)
library(ggTimeSeries)
library(ckanr)
library(googledrive)
library(ggmap)
library(gganimate)
library(ggalt)
library(ggTimeSeries)
library(knitr)
library(reactable)
library(DBI)
# Configure ckanr to data.dpaw.wa.gov.au with env vars from .Renviron
ckanr::ckanr_setup(url = Sys.getenv("CKAN_URL"), key = Sys.getenv("CKAN_API_KEY"))

dt <- . %>% DT::datatable(., escape = FALSE, rownames = FALSE)
dt0 <-
  . %>% DT::datatable(.,
                      escape = FALSE,
                      rownames = FALSE,
                      options = list(paging = F))
rt <- . %>% reactable::reactable(filterable = T, searchable = T)
# Filters -----------------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------------------------#
# Season filters
filter_2016 <- . %>% dplyr::filter(season == 2016)
filter_2017 <- . %>% dplyr::filter(season == 2017)
filter_2018 <- . %>% dplyr::filter(season == 2018)
filter_2019 <- . %>% dplyr::filter(season == 2019)
filter_2020 <- . %>% dplyr::filter(season == 2020)

#--------------------------------------------------------------------------------------------------#
# Site filters - depends on data inside WAStD
filter_bme <- . %>% dplyr::filter(site_id %in% c(22, 23, 24))
filter_bme_cbb1 <- . %>% dplyr::filter(site_id == 22)
filter_bme_cbb2 <- . %>% dplyr::filter(site_id == 23)
filter_bme_cbb3 <- . %>% dplyr::filter(site_id == 24)

filter_emb <- . %>% dplyr::filter(site_id %in% c(36, 37))
filter_emb_annaplains <- . %>% dplyr::filter(site_id == 37)
filter_emb_caravanpark <- . %>% dplyr::filter(site_id == 36)

filter_pth <- . %>% dplyr::filter(site_id %in% c(35, 45, 141))
filter_pth_cemetery <- . %>% dplyr::filter(site_id == 35)
filter_pth_prettypool <- . %>% dplyr::filter(site_id == 45)
filter_pth_spoilbank <- . %>% dplyr::filter(site_id == 141)


filter_wp <- . %>% dplyr::filter(site_id %in% c(25, 26, 27, 46, 47))
filter_wp_coolingwater <- . %>% dplyr::filter(site_id == 26)
filter_wp_bells <- . %>% dplyr::filter(site_id == 25)
filter_wp_yachtblub <- . %>% dplyr::filter(site_id == 27)
filter_wp_boat <- . %>% dplyr::filter(site_id == 46)
filter_wp_cleaverville <- . %>% dplyr::filter(site_id == 47)

filter_con <- . %>% dplyr::filter(site_id == 115)
filter_del <- . %>% dplyr::filter(site_id == 39)
filter_ros <- . %>% dplyr::filter(site_id == 40)
filter_thv <- . %>% dplyr::filter(site_id %in% c(20, 28, 29))
filter_thv_north <- . %>% dplyr::filter(site_id == 28)
filter_thv_south <- . %>% dplyr::filter(site_id == 29)
filter_thv_tagging <- . %>% dplyr::filter(site_id == 20)

filter_lgcs <- . %>% dplyr::filter(site_id %in% c(51, 52))

filter_nin <- . %>% dplyr::filter(site_id %in% 58:114)
filter_nin_mi <- . %>% dplyr::filter(site_id %in% c(58, 59, 113))
filter_nin_nwc <- . %>% dplyr::filter(site_id %in% 66:79)
filter_nin_nwc_tantabiddi <- . %>% dplyr::filter(site_id %in% 76:79)
filter_nin_nwc_graveyards <- . %>% dplyr::filter(site_id %in% 72:75)
filter_nin_nwc_hunters <- . %>% dplyr::filter(site_id %in% 69:71)
filter_nin_nwc_lighthouse <- . %>% dplyr::filter(site_id %in% 66:68)
filter_nin_nwc_navypier <- . %>% dplyr::filter(site_id %in% 62:65)
filter_nin_nwc_bundegi <- . %>% dplyr::filter(site_id %in% 60:61)


filter_nin_cr <- . %>% dplyr::filter(site_id %in% 80:95)
filter_nin_bn <- . %>% dplyr::filter(site_id %in% 96:106)
filter_nin_cb <- . %>% dplyr::filter(site_id %in% 107:109)
filter_nin_wr <- . %>% dplyr::filter(site_id %in% 110:111)
filter_nin_gn <- . %>% dplyr::filter(site_id == 114)




gganimate_tracks <-
  function(data,
           placename = NULL,
           prefix = NULL,
           gm_apikey = NULL) {
    require(purrr)

    pl <- placename %||% "Western Australia"
    pf <- prefix %||% "WA"

    # Basemap: Google Maps
    apikey <- gm_apikey %||%
      Sys.getenv("GOOGLE_MAPS_APIKEY") %||%
      stop("Need a Google Maps API key as system variable GOOGLE_MAPS_APIKEY")
    ggmap::register_google(key = apikey)
    bbx <- ggmap::make_bbox(longitude, latitude, data, f = 0.05)
    ctr <-
      c(mean(bbx["left"], bbx["right"]), mean(bbx["top"], bbx["bottom"]))
    # basemap <- ggmap::get_map(ctr, zoom=17) %>% ggmap::ggmap()
    basemap <-
      ggmap::get_googlemap(ctr,
                           zoom = 15,
                           scale = 2,
                           maptype = "hybrid") %>%
      ggmap::ggmap()

    # Add turtle tracks to basemap, discard warnings
    tracks_map <- suppressWarnings(basemap + ggplot2::geom_point(aes(longitude, latitude, colour = nest_type), data = data)) +
      ggplot2::ggtitle(glue::glue("Turtle Nesting at {pl}"), subtitle = "Turtle date: {frame_time}") +
      gganimate::transition_time(turtle_date) +
      gganimate::ease_aes("elastic-in") +
      gganimate::exit_fade()

    gganimate::animate(tracks_map, fps = 2, detail = 10) %T>%
      gganimate::anim_save(glue::glue("{pf}_nesting.gif"), .)
  }

#--------------------------------------------------------------------------------------------------#
# Download data ODK Central (for previews)
#


load_saved_data_odkc <- function(datafile = here::here("wa-turtle-programs", "turtleviewer.rda")) {
  if (!fs::file_exists(datafile)) {
    download_and_save_odkc(datafile = datafile)
  }
  load(datafile, envir = .GlobalEnv)
}

#--------------------------------------------------------------------------------------------------#
# Download data from TSC (reports)
add_calendar_date_awst <- . %>%
  dplyr::mutate(
    calendar_date_awst = datetime %>%
      lubridate::with_tz("Australia/Perth") %>%
      lubridate::floor_date(unit = "day")
  )

dev <- "http://localhost:8220/api/1/"
uat <- "https://tsc-uat.dbca.wa.gov.au/api/1/"
prod <- "https://tsc.dbca.wa.gov.au/api/1/"

download_and_save_tsc <- function(datafile = here::here("wa-turtle-programs", "data_tsc.rda")) {
  library(magrittr)
  wastd_url <- wastdr::get_wastd_url()
  q <- list(taxon = "Cheloniidae", format = "json")

  animal_records <-
    wastdr::wastd_GET("animal-encounters", query = q)
  animals <- animal_records %>%
    wastdr::parse_animal_encounters() %>%
    add_calendar_date_awst()

  # "turtle-morphometrics"

  track_records <- wastdr::wastd_GET("turtle-nest-encounters")
  # query = list(limit=1000, offset=10300),
  # api_url = prod)
  tracks <- track_records %>%
    wastdr::parse_turtle_nest_encounters() %>%
    add_calendar_date_awst()

  disturbance_records <- "turtle-nest-disturbance-observations" %>%
    wastdr::wastd_GET()
  nest_dist <- disturbance_records %>%
    wastdr::parse_disturbance_observations() %>%
    add_calendar_date_awst()

  nest_records <- "turtle-nest-tag-observations" %>%
    wastdr::wastd_GET()
  nest_tags <- nest_records %>%
    wastdr::parse_nesttag_observations() %>%
    add_calendar_date_awst()

  nest_excavation_records <- "turtle-nest-excavations" %>%
    wastdr::wastd_GET()
  nest_excavations <- nest_excavation_records %>%
    wastdr::parse_encounterobservations() %>%
    add_calendar_date_awst()

  hatchling_morph_records <-
    "turtle-nest-hatchling-morphometrics" %>%
    wastdr::wastd_GET()
  hatchling_morph <- hatchling_morph_records %>%
    wastdr::parse_encounterobservations() %>%
    add_calendar_date_awst()

  fan_records <- "turtle-nest-hatchling-emergences" %>%
    wastdr::wastd_GET()
  nest_fans <- fan_records %>%
    wastdr::parse_encounterobservations() %>%
    add_calendar_date_awst()

  fan_outlier_records <-
    "turtle-nest-hatchling-emergence-outliers" %>%
    wastdr::wastd_GET()
  nest_fan_outliers <- fan_outlier_records %>%
    wastdr::parse_encounterobservations() %>%
    add_calendar_date_awst()

  lightsource_records <-
    "turtle-nest-hatchling-emergence-light-sources" %>%
    wastdr::wastd_GET()
  nest_lightsources <- lightsource_records %>%
    wastdr::parse_encounterobservations() %>%
    add_calendar_date_awst()

  surveys <- "surveys" %>%
    wastdr::wastd_GET() %>%
    wastdr::parse_surveys() %>%
    dplyr::mutate(
      calendar_date_awst = start_time %>%
        lubridate::with_tz("Australia/Perth") %>%
        lubridate::floor_date(unit = "day")
    )

  areas_sf <- "area" %>%
    wastdr::wastd_GET() %>%
    magrittr::extract2("features") %>%
    geojsonio::as.json() %>%
    geojsonsf::geojson_sf()

  areas <- areas_sf %>%
    dplyr::filter(area_type == "Locality") %>%
    dplyr::transmute(area_id = pk, area_name = name)

  sites <- areas_sf %>%
    dplyr::filter(area_type == "Site") %>%
    dplyr::transmute(site_id = pk, site_name = name) %>%
    sf::st_join(areas)

  save(
    animal_records,
    animals,
    track_records,
    tracks_all,
    disturbance_records,
    nest_dist,
    nest_records,
    nest_tags,
    nest_excavation_records,
    nest_excavations,
    fan_records,
    nest_fans,
    fan_outlier_records,
    nest_fan_outliers,
    lightsource_records,
    nest_lightsources,
    surveys,
    areas_sf,
    areas,
    sites,
    file = datafile
  )
}

load_saved_data_tsc <- function(datafile = here::here("wa-turtle-programs", "data_tsc.rda")) {
  if (!fs::file_exists(datafile)) {
    download_and_save_tsc(datafile = datafile)
  }
  load(datafile, envir = .GlobalEnv)
}


# areas_sf %>%
#   dplyr::filter(area_type=="Site") %>% magrittr::extract("name") %>% plot(.)


#------------------------------------------------------------------------------#
# Turtle Tagging
#------------------------------------------------------------------------------#

