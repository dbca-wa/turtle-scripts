knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
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
# Configure ckanr to data.dpaw.wa.gov.au with env vars from .Renviron
ckanr::ckanr_setup(url = Sys.getenv("CKAN_URL"), key = Sys.getenv("CKAN_API_KEY"))

dt <- . %>% DT::datatable(., escape = FALSE, rownames = FALSE)
dt0 <- . %>% DT::datatable(., escape = FALSE, rownames = FALSE, options = list(paging = F))
rt <- . %>% reactable::reactable(filterable = T, searchable = T)
# Filters -----------------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------------------------#
# Season filters
filter_2016 <- . %>% dplyr::filter(season == 2016)
filter_2017 <- . %>% dplyr::filter(season == 2017)
filter_2018 <- . %>% dplyr::filter(season == 2018)
filter_2019 <- . %>% dplyr::filter(season == 2019)
filter_2020 <- . %>% dplyr::filter(season == 2020)

#' Filter a dataframe of tracks, disturbance, incidents, or surveys to season
#'
#' @param data A dataframe of tracks, disturbance, incidents, or surveys
#'  containing a column "season" (int) with the season start year, e.g. 2019.
#' @param season_start_year The desired season's start year, e.g. 2019.
#' @return The dataframe filtered to rows from the desired season.
wastd_season <- function(data, season_start_year){
  dplyr::filter(data, season == season_start_year)
}

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


causes_disturbance <- c("human", "unknown", "tide", "turtle", "other",
                        "vehicle", "cyclone")
causes_predation <- c("bandicoot", "bird", "cat", "crab", "croc", "dingo",
                       "dog", "fox", "goanna", "pig")
filter_disturbance <- . %>% dplyr::filter(disturbance_cause %in% causes_disturbance)
filter_predation <- . %>% dplyr::filter(disturbance_cause %in% causes_predation)


gganimate_tracks <- function(data, placename=NULL, prefix=NULL, gm_apikey=NULL) {
  require(purrr)

  pl <- placename %||% "Western Australia"
  pf <- prefix %||% "WA"

  # Basemap: Google Maps
  apikey <- gm_apikey %||%
    Sys.getenv("GOOGLE_MAPS_APIKEY") %||%
    stop("Need a Google Maps API key as system variable GOOGLE_MAPS_APIKEY")
  ggmap::register_google(key = apikey)
  bbx <- ggmap::make_bbox(longitude, latitude, data, f = 0.05)
  ctr <- c(mean(bbx["left"], bbx["right"]), mean(bbx["top"], bbx["bottom"]))
  # basemap <- ggmap::get_map(ctr, zoom=17) %>% ggmap::ggmap()
  basemap <- ggmap::get_googlemap(ctr, zoom = 15, scale = 2, maptype = "hybrid") %>%
    ggmap::ggmap()

  # Add turtle tracks to basemap, discard warnings
  tracks_map <- suppressWarnings(
    basemap + ggplot2::geom_point(aes(longitude, latitude, colour = nest_type), data = data)
  ) +
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

add_sites <- function(data, prefix="observed_at"){
  lon <- glue::glue("{prefix}_longitude") %>% as.character()
  lat <- glue::glue("{prefix}_latitude") %>% as.character()
  data %>%
    tidyr::drop_na(lon) %>%
    tidyr::drop_na(lat) %>%
    sf::st_as_sf(coords = c(lon, lat),
                 crs = 4326,
                 agr = "constant",
                 remove = FALSE) %>%
    sf::st_join(sites) %>%
    sf::st_join(areas)
}

add_dates <- function(data){
  data %>%
    dplyr::mutate(
      datetime = observation_start_time %>%
        lubridate::with_tz("Australia/Perth"),
      calendar_date_awst = datetime %>%
        lubridate::floor_date(unit = "day") %>%
        as.character(),
      turtle_date = datetime %>% datetime_as_turtle_date(),
      season = datetime %>% datetime_as_season(),
      season_week = datetime %>% datetime_as_seasonweek(),
      iso_week = datetime %>% datetime_as_isoweek()
    )
}

add_dates_svs <- function(data){
  data %>%
    dplyr::mutate(
      datetime = survey_start_time %>%
        lubridate::with_tz("Australia/Perth"),
      calendar_date_awst = datetime %>%
        lubridate::floor_date(unit = "day") %>%
        as.character(),
      turtle_date = datetime %>% datetime_as_turtle_date(),
      season = datetime %>% datetime_as_season(),
      season_week = datetime %>% datetime_as_seasonweek(),
      iso_week = datetime %>% datetime_as_isoweek()
    )
}

add_dates_sve <- function(data){
  data %>%
    dplyr::mutate(
      datetime = survey_end_time %>%
        lubridate::with_tz("Australia/Perth"),
      calendar_date_awst = datetime %>%
        lubridate::floor_date(unit = "day") %>%
        as.character(),
      turtle_date = datetime %>% datetime_as_turtle_date(),
      season = datetime %>% datetime_as_season(),
      season_week = datetime %>% datetime_as_seasonweek(),
      iso_week = datetime %>% datetime_as_isoweek()
    )
}

download_and_save_odkc <- function(
  datafile=here::here("wa-turtle-programs", "data_odkc.rda"),
  extrafile=here::here("wa-turtle-programs", "data_odkc_extra.rda"),
  local_dir = here::here("wa-turtle-programs", "media")){
  suppressMessages(library(tidyverse))
  library(wastdr)
  library(ruODK)
  prod <- "https://odkcentral.dbca.wa.gov.au"
  uat <- "https://odkcentral-uat.dbca.wa.gov.au"
  tz <- "Australia/Perth"
  loc <- local_dir
  fs::dir_create(loc)
  pl <- ruODK::project_list()
  pl

  fl <- ruODK::form_list(pid = 1)
  fl

  # SV start
  ruODK::ru_setup(pid=1, fid="build_Site-Visit-Start-0-3_1559789550", url=prod)
  message(glue::glue("Downloading {ruODK::get_default_fid()}"))
  svs_prod <- ruODK::odata_submission_get(verbose = T, wkt = T, local_dir = loc)

  # SV end
  ruODK::ru_setup(pid=1, fid="build_Site-Visit-End-0-2_1559789512", url=prod)
  message(glue::glue("Downloading {ruODK::get_default_fid()}"))
  sve_prod <- ruODK::odata_submission_get(verbose = T, wkt=T, local_dir = loc)

  # MWI
  ruODK::ru_setup(
    pid=1, fid="build_Marine-Wildlife-Incident-0-6_1559789189", url=prod)
  message(glue::glue("Downloading {ruODK::get_default_fid()}"))
  ft <- ruODK::odata_service_get()
  ft %>% knitr::kable(.)
  mwi_prod <- ft$url[[1]] %>%
    ruODK::odata_submission_get(table = ., verbose = T, wkt=T, local_dir = loc)
  mwi_dmg_prod <- ft$url[[2]] %>%
    ruODK::odata_submission_get(table = ., verbose = T, wkt=T, local_dir = loc) %>%
    dplyr::left_join(mwi_prod, by = c("submissions_id" = "id"))
  mwi_tag <- ft$url[[3]] %>%
    ruODK::odata_submission_get(table = ., verbose = T, wkt=T, local_dir = loc)  # %>%
  # dplyr::left_join(mwi, by = c("submissions_id" = "id"))

  # Dist
  ruODK::ru_setup(pid=1, fid="build_Predator-or-Disturbance-1-1_1559789410", url=prod)
  message(glue::glue("Downloading {ruODK::get_default_fid()}"))
  dist_prod <- ruODK::odata_submission_get(verbose = T, wkt=T, local_dir = loc)

  # Tracks
  ruODK::ru_setup(pid=1, fid="build_Turtle-Track-or-Nest-1-0_1559789920", url=prod)
  message(glue::glue("Downloading {ruODK::get_default_fid()}"))
  ft <- ruODK::odata_service_get()
  ft %>% knitr::kable(.)
  tracks_prod <- ft$url[1] %>%
    ruODK::odata_submission_get(table = ., verbose = T, wkt=T, local_dir = loc) %>%
    wastdr::exclude_training_species() %>%
    wastdr::add_nest_labels()

  tracks_dist_prod <- ft$url[2] %>%
    ruODK::odata_submission_get(table = ., verbose = T, wkt=T, local_dir = loc) %>%
    dplyr::left_join(tracks_prod, by = c("submissions_id" = "id"))

  # None of the following were captured in UAT, so we name them wihtout _prod:
  tracks_egg <- ft$url[3] %>%
    ruODK::odata_submission_get(table = ., verbose = T, wkt=T, local_dir = loc) #%>%
  # dplyr::left_join(tracks, by = c("submissions_id" = "id"))

  tracks_log_prod <- ft$url[4] %>%
    ruODK::odata_submission_get(table = ., verbose = T, wkt=T, local_dir = loc) %>%
    dplyr::left_join(tracks_prod, by = c("submissions_id" = "id"))

  tracks_hatch <- ft$url[5] %>%
    ruODK::odata_submission_get(table = ., verbose = T, wkt=T, local_dir = loc) #%>%
  # dplyr::left_join(tracks, by = c("submissions_id" = "id"))

  tracks_fan_outlier_prod <- ft$url[6] %>%
    ruODK::odata_submission_get(table = ., verbose = T, local_dir = loc) %>%
    dplyr::left_join(tracks_prod, by = c("submissions_id" = "id"))

  tracks_light <- ft$url[7] %>%
    ruODK::odata_submission_get(table = ., verbose = T, wkt=T, local_dir = loc) #%>%
  # dplyr::left_join(tracks_prod, by = c("submissions_id" = "id"))

  #----------------------------------------------------------------------------#
  # Fix error: PROD used UAT db for a week - what's in UAT but not in PROD?

  # SV start
  ruODK::ru_setup(pid=1, fid="build_Site-Visit-Start-0-3_1559789550", url=uat)
  message(glue::glue("Downloading {ruODK::get_default_fid()}"))
  svs_uat <- ruODK::odata_submission_get(verbose = T, wkt=T, local_dir = loc)
  svs_extra <- dplyr::anti_join(svs_uat, svs_prod, by="instance_id")

  # SV end
  ruODK::ru_setup(pid=1, fid="build_Site-Visit-End-0-2_1559789512", url=uat)
  message(glue::glue("Downloading {ruODK::get_default_fid()}"))
  sve_uat <- ruODK::odata_submission_get(verbose = T, wkt=T, local_dir = loc)
  sve_extra <- dplyr::anti_join(sve_uat, sve_prod, by="instance_id")

  # MWI
  ruODK::ru_setup(pid=1, fid="build_Marine-Wildlife-Incident-0-6_1559789189", url=uat)
  message(glue::glue("Downloading {ruODK::get_default_fid()}"))
  mwi_uat <- ruODK::odata_submission_get(verbose = T, wkt=T, local_dir = loc)
  mwi_extra <- dplyr::anti_join(mwi_uat, mwi_prod, by="instance_id")

  # Dist
  ruODK::ru_setup(pid=1, fid="build_Predator-or-Disturbance-1-1_1559789410", url=uat)
  message(glue::glue("Downloading {ruODK::get_default_fid()}"))
  dist_uat <- ruODK::odata_submission_get(wkt=T, verbose = T, local_dir = loc)
  dist_extra <- dplyr::anti_join(dist_uat, dist_prod, by="instance_id")

  # Tracks
  ruODK::ru_setup(pid=1, fid="build_Turtle-Track-or-Nest-1-0_1559789920", url=uat)
  message(glue::glue("Downloading {ruODK::get_default_fid()}"))
  ft <- ruODK::odata_service_get()
  ft %>% knitr::kable(.)
  tracks_uat <- ft$url[1] %>%
    ruODK::odata_submission_get(table = ., verbose = T, wkt=T, local_dir = loc) %>%
    wastdr::exclude_training_species() %>%
    wastdr::add_nest_labels()
  tracks_extra <- dplyr::anti_join(tracks_uat, tracks_prod, by="instance_id")

  tracks_dist_uat <- ft$url[2] %>%
    ruODK::odata_submission_get(table = ., verbose = T, wkt=T, local_dir = loc) %>%
    dplyr::left_join(tracks_uat, by = c("submissions_id" = "id"))
  tracks_dist_extra <- dplyr::anti_join(tracks_dist_uat, tracks_dist_prod, by="instance_id")

  save(svs_extra, sve_extra, mwi_extra, dist_extra,
       tracks_extra, tracks_dist_extra,
       file = extrafile)

  load(extrafile)

  areas_sf <- wastdr::wastd_GET("area") %>%
    magrittr::extract2("features") %>%
    geojsonio::as.json() %>%
    geojsonsf::geojson_sf()
  sites <- areas_sf  %>%
    dplyr::filter(area_type=="Site") %>%
    dplyr::transmute(site_id = pk, site_name = name)
  areas <- areas_sf %>%
    dplyr::filter(area_type=="Locality") %>%
    dplyr::transmute(area_id = pk, area_name = name)


  mwi <- dplyr::bind_rows(mwi_prod, mwi_extra) %>% add_sites %>% add_dates
  mwi_dmg <- mwi_dmg_prod %>% add_sites %>% add_dates
  svs <- dplyr::bind_rows(svs_prod, svs_extra) %>%
    add_sites(prefix="location") %>% add_dates_svs
  sve <- dplyr::bind_rows(sve_prod, sve_extra) %>%
    add_sites(prefix="location") %>% add_dates_sve
  dist <- dplyr::bind_rows(dist_prod, dist_extra) %>%
    add_sites(prefix="location") %>% add_dates
  tracks <- dplyr::bind_rows(tracks_prod, tracks_extra) %>% add_sites %>% add_dates
  tracks_dist <- dplyr::bind_rows(tracks_dist_prod, tracks_dist_extra) %>% add_sites %>% add_dates
  tracks_log <- tracks_log_prod %>% add_sites %>% add_dates
  tracks_fan_outlier <- tracks_fan_outlier_prod %>% add_sites %>% add_dates


  turtledata <- list(
    downloaded_on = Sys.time(),
    tracks = tracks,
    tracks_dist = tracks_dist,
    tracks_log = tracks_log,
    tracks_fan_outlier = tracks_fan_outlier,
    dist = dist,
    mwi = mwi,
    mwi_dmg = mwi_dmg,
    svs = svs,
    sve = sve,
    sites = sites,
    areas = areas
  )
  save(turtledata, file=datafile, compress = "xz")
  # save(mwi, mwi_dmg, mwi_tag, sve, svs, dist,
  #      tracks, tracks_dist, tracks_egg, tracks_fan_outlier,
  #      tracks_hatch, tracks_light, tracks_log,
  #      file = datafile)
}

load_saved_data_odkc <- function(
  datafile=here::here("wa-turtle-programs", "turtleviewer.rda")
  ){
  if (!fs::file_exists(datafile)){download_and_save_odkc(datafile=datafile)}
  load(datafile, envir = .GlobalEnv)
}

#--------------------------------------------------------------------------------------------------#
# Download data from TSC (reports)
download_and_save_tsc <- function(
  datafile=here::here("wa-turtle-programs", "data_tsc.rda")
){
  library(magrittr)
  wastd_url <- wastdr::get_wastd_url()
  q <- list(taxon = "Cheloniidae", format = "json")
  animal_records <- wastdr::wastd_GET("animal-encounters", query = q)
  animals <- animal_records %>%
    wastdr::parse_animal_encounters() %>%
    dplyr::mutate(
      calendar_date_awst = datetime %>%
        lubridate::with_tz("Australia/Perth") %>%
        lubridate::floor_date(unit = "day")
    )
  track_records <- wastdr::wastd_GET("turtle-nest-encounters")
  tracks_all <- track_records %>%
    wastdr::parse_turtle_nest_encounters() %>%
    dplyr::mutate(
      calendar_date_awst = datetime %>%
        lubridate::with_tz("Australia/Perth") %>%
        lubridate::floor_date(unit = "day")
    )
  disturbance_records <- wastdr::wastd_GET("disturbance-observations")
  disturbance <- disturbance_records %>%
    wastdr::parse_disturbance_observations() %>%
    dplyr::mutate(
      calendar_date_awst = datetime %>%
        lubridate::with_tz("Australia/Perth") %>%
        lubridate::floor_date(unit = "day")
    )
  survey_records <- wastdr::wastd_GET("surveys")
  surveys <- survey_records %>%
    wastdr::parse_surveys() %>%
    dplyr::mutate(
      calendar_date_awst = start_time %>%
        lubridate::with_tz("Australia/Perth") %>%
        lubridate::floor_date(unit = "day")
    )
  nest_records <- wastdr::wastd_GET("nesttag-observations")
  nests_all <- nest_records %>%
    wastdr::parse_nesttag_observations() %>%
    dplyr::mutate(
      calendar_date_awst = datetime %>%
        lubridate::with_tz("Australia/Perth") %>%
        lubridate::floor_date(unit = "day")
    )
  area_records <- wastdr::wastd_GET("area")
  areas <- area_records$features %>% {
    tibble::tibble(
      pk = purrr::map_chr(., c("properties", "pk")),
      area_type = purrr::map_chr(., c("properties", "area_type")),
      name = purrr::map_chr(., c("properties", "name")),
      northern_extent = purrr::map_dbl(., c("properties", "northern_extent")),
      centroid = purrr::map(., c("properties", "centroid", "coordinates")),
      length_surveyed_m = purrr::map_chr(., c("properties", "length_surveyed_m"), .default=NA),
      length_survey_roundtrip_m = purrr::map_chr(., c("properties", "length_survey_roundtrip_m"), .default=NA)
    )} %>%
    tidyr::unnest_wider("centroid") %>%
    dplyr::rename(longitude = `...1`, latitude = `...2`)
  areas_sf <- geojsonio::as.json(area_records$features) %>% geojsonsf::geojson_sf()
  save(
    animal_records,
    animals,
    track_records,
    tracks_all,
    disturbance_records,
    disturbance,
    survey_records,
    surveys,
    nest_records,
    nests_all,
    area_records,
    areas,
    areas_sf,
    file = datafile
  )
}

load_saved_data_tsc <- function(
  datafile=here::here("wa-turtle-programs", "data_tsc.rda")
){
  if (!fs::file_exists(datafile)){download_and_save_tsc(datafile=datafile)}
  load(datafile, envir = .GlobalEnv)
}


# areas_sf %>%
#   dplyr::filter(area_type=="Site") %>% magrittr::extract("name") %>% plot(.)

sf_as_tbl <- function(sf_obj){
  sf::st_geometry(sf_obj) <- NULL
  sf_obj
}
