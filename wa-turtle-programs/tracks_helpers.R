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

filter_pth <- . %>% dplyr::filter(site_id %in% c(35, 45))
filter_pth_cemetery <- . %>% dplyr::filter(site_id == 35)
filter_pth_prettypool <- . %>% dplyr::filter(site_id == 45)

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


download_and_save_data <- function(datafile=here::here("wa-turtle-programs", "data.Rda")){
  suppressMessages(library(tidyverse))
  library(ruODK)
  prod <- "https://odkcentral.dbca.wa.gov.au"
  uat <- "https://odkcentral-uat.dbca.wa.gov.au"
  tz <- "Australia/Perth"
  loc <- fs::path("media")
  fs::dir_create(loc)
  pl <- ruODK::project_list()
  pl
  
  fl <- ruODK::form_list(pid = 1)
  fl
  
  # SV start
  ruODK::ru_setup(pid=1, fid="build_Site-Visit-Start-0-3_1559789550", url=prod)
  message(glue::glue("Downloading {ruODK::get_default_fid()}"))
  ft <- ruODK::odata_service_get()
  ft %>% knitr::kable(.)
  svs_prod <- ft$url[[1]] %>% 
    ruODK::odata_submission_get(table = ., verbose = T, wkt = T)
  
  # SV end
  ruODK::ru_setup(pid=1, fid="build_Site-Visit-End-0-2_1559789512", url=prod)
  message(glue::glue("Downloading {ruODK::get_default_fid()}"))
  ft <- ruODK::odata_service_get()
  ft %>% knitr::kable(.)
  sve_prod <- ft$url[[1]] %>% 
    ruODK::odata_submission_get(table = ., verbose = T, wkt=T)
  
  # MWI
  ruODK::ru_setup(
    pid=1, fid="build_Marine-Wildlife-Incident-0-6_1559789189", url=prod)
  message(glue::glue("Downloading {ruODK::get_default_fid()}"))
  ft <- ruODK::odata_service_get()
  ft %>% knitr::kable(.)
  mwi_prod <- ft$url[[1]] %>% 
    ruODK::odata_submission_get(table = ., verbose = T, wkt=T, parse=T)
  mwi_dmg <- ft$url[[2]] %>% 
    ruODK::odata_submission_get(table = ., verbose = T, wkt=T) #%>%
  # dplyr::left_join(mwi, by = c("submissions_id" = "id"))
  mwi_tag <- ft$url[[3]] %>% 
    ruODK::odata_submission_get(table = ., verbose = T, wkt=T)  # %>%
  # dplyr::left_join(mwi, by = c("submissions_id" = "id"))
  
  # Dist
  ruODK::ru_setup(pid=1, fid="build_Predator-or-Disturbance-1-1_1559789410", url=prod)
  message(glue::glue("Downloading {ruODK::get_default_fid()}"))
  ft <- ruODK::odata_service_get()
  ft %>% knitr::kable(.)
  dist_prod <- ft$url[[1]] %>% 
    ruODK::odata_submission_get(table = ., verbose = T, wkt=T)
  
  # Tracks
  ruODK::ru_setup(pid=1, fid="build_Turtle-Track-or-Nest-1-0_1559789920", url=prod)
  message(glue::glue("Downloading {ruODK::get_default_fid()}"))
  ft <- ruODK::odata_service_get()
  ft %>% knitr::kable(.)
  tracks_prod <- ft$url[1] %>% 
    ruODK::odata_submission_get(table = ., verbose = T, wkt=T) %>% 
    wastdr::exclude_training_species() %>% 
    wastdr::add_nest_labels()
  
  tracks_dist_prod <- ft$url[2] %>% 
    ruODK::odata_submission_get(table = ., verbose = T, wkt=T) %>% 
    dplyr::left_join(tracks_prod, by = c("submissions_id" = "id"))
  
  # None of the following were captured in UAT, so we name them wihtout _prod:
  tracks_egg <- ft$url[3] %>%
    ruODK::odata_submission_get(table = ., verbose = T, wkt=T) #%>%
  # dplyr::left_join(tracks, by = c("submissions_id" = "id"))
  
  tracks_log <- ft$url[4] %>% 
    ruODK::odata_submission_get(table = ., verbose = T, wkt=T) %>%
    dplyr::left_join(tracks_prod, by = c("submissions_id" = "id"))
  
  tracks_hatch <- ft$url[5] %>% 
    ruODK::odata_submission_get(table = ., verbose = T, wkt=T) #%>%
  # dplyr::left_join(tracks, by = c("submissions_id" = "id"))
  
  tracks_fan_outlier <- ft$url[6] %>% 
    ruODK::odata_submission_get(table = ., verbose = T) %>%
    dplyr::left_join(tracks_prod, by = c("submissions_id" = "id"))
  
  tracks_light <- ft$url[7] %>% 
    ruODK::odata_submission_get(table = ., verbose = T, wkt=T) #%>%
  # dplyr::left_join(tracks_prod, by = c("submissions_id" = "id"))
  
  #----------------------------------------------------------------------------#
  # Fix error: PROD used UAT db for a week - what's in UAT but not in PROD?
  
  # SV start
  ruODK::ru_setup(pid=1, fid="build_Site-Visit-Start-0-3_1559789550", url=uat)
  message(glue::glue("Downloading {ruODK::get_default_fid()}"))
  ft <- ruODK::odata_service_get()
  ft %>% knitr::kable(.)
  svs_uat <- ruODK::odata_submission_get(table = ft$url[[1]], verbose = T)
  svs_extra <- dplyr::anti_join(svs_uat, svs_prod, by="instance_id")
  
  # SV end
  ruODK::ru_setup(pid=1, fid="build_Site-Visit-End-0-2_1559789512", url=uat)
  message(glue::glue("Downloading {ruODK::get_default_fid()}"))
  ft <- ruODK::odata_service_get()
  ft %>% knitr::kable(.)
  sve_uat <- ruODK::odata_submission_get(table = ft$url[[1]], verbose = T)
  sve_extra <- dplyr::anti_join(sve_uat, sve_prod, by="instance_id")
  
  # MWI
  ruODK::ru_setup(pid=1, fid="build_Marine-Wildlife-Incident-0-6_1559789189", url=uat)
  message(glue::glue("Downloading {ruODK::get_default_fid()}"))
  ft <- ruODK::odata_service_get()
  ft %>% knitr::kable(.)
  mwi_uat <- ft$url[[1]] %>% 
    ruODK::odata_submission_get(table = ., verbose = T, parse=T)
  mwi_extra <- dplyr::anti_join(mwi_uat, mwi_prod, by="instance_id")

  # Dist
  ruODK::ru_setup(pid=1, fid="build_Predator-or-Disturbance-1-1_1559789410", url=uat)
  message(glue::glue("Downloading {ruODK::get_default_fid()}"))
  ft <- ruODK::odata_service_get()
  ft %>% knitr::kable(.)
  dist_uat <- ruODK::odata_submission_get(table = ft$url[[1]], verbose = T)
  dist_extra <- dplyr::anti_join(dist_uat, dist_prod, by="instance_id")
  
  # Tracks
  ruODK::ru_setup(pid=1, fid="build_Turtle-Track-or-Nest-1-0_1559789920", url=uat)
  message(glue::glue("Downloading {ruODK::get_default_fid()}"))
  ft <- ruODK::odata_service_get()
  ft %>% knitr::kable(.)
  tracks_uat <- ft$url[1] %>% 
    ruODK::odata_submission_get(table = ., verbose = T, wkt=T) %>% 
    wastdr::exclude_training_species() %>% 
    wastdr::add_nest_labels()
  tracks_extra <- dplyr::anti_join(tracks_uat, tracks_prod, by="instance_id")

  tracks_dist_uat <- ft$url[2] %>% 
    ruODK::odata_submission_get(table = ., verbose = T, wkt=T) %>% 
    dplyr::left_join(tracks_uat, by = c("submissions_id" = "id"))    
  tracks_dist_extra <- dplyr::anti_join(tracks_dist_uat, tracks_dist_prod, by="instance_id")
  
  save(svs_extra, sve_extra, mwi_extra, dist_extra, 
       tracks_extra, tracks_dist_extra, file = "data_extra.Rda")
  
  mwi <- dplyr::bind_rows(mwi_prod, mwi_extra)
  svs <- dplyr::bind_rows(svs_prod, svs_extra)
  sve <- dplyr::bind_rows(sve_prod, sve_extra)
  dist <- dplyr::bind_rows(dist_prod, dist_extra)
  tracks <- dplyr::bind_rows(tracks_prod, tracks_extra)
  tracks_dist <- dplyr::bind_rows(tracks_dist_prod, tracks_dist_extra)
  
  save(mwi, mwi_dmg, mwi_tag, sve, svs, dist,
       tracks, tracks_dist_uat, tracks_egg, tracks_fan_outlier, 
       tracks_hatch, tracks_light, tracks_log, 
       file = datafile)  
}

load_saved_data <- function(datafile=here::here("wa-turtle-programs", "data.Rda")){
  if (!fs::file_exists(datafile)){download_and_save_data(datafile=datafile)}
  load(datafile, envir = .GlobalEnv)
  }
