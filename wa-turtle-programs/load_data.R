if (file.exists(here::here("data", "tracks.Rda"))) {
  load(here::here("data", "tracks.Rda"))
} else {
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
    file = here::here("data", "tracks.Rda")
  )
}

# areas_sf %>%
#   # dplyr::filter(area_type=="Site") %>%
#   # magrittr::extract("name") %>%
#   plot(.)
