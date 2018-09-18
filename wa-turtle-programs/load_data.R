if (file.exists(here::here("data", "tracks.Rda"))) {
  load(here::here("data", "tracks.Rda"))
} else {
  library(magrittr)
  wastd_url <- get_wastd_url()
  q <- list(taxon = "Cheloniidae", format = "json")
  animal_records <- wastdr::wastd_GET("animal-encounters", query = q)
  animals <- wastdr::parse_animal_encounters(animal_records)
  track_records <- wastdr::wastd_GET("turtle-nest-encounters")
  tracks_all <- wastdr::parse_turtle_nest_encounters(track_records)
  disturbance <- wastdr::wastd_GET("disturbance-observations") %>%
    wastdr::parse_disturbance_observations()
  surveys <- wastd_GET("surveys") %>% wastdr::parse_surveys() %>%
    dplyr::mutate(change_url = glue::glue(
      '<a href="{wastd_url}/admin/observations/survey/{id}/change/" target="_">Update Survey {id}</a>'))
  save(
    animal_records,
    animals,
    tracks_all,
    track_records,
    disturbance,
    surveys,
    file = here::here("data", "tracks.Rda")
  )
}
