if (file.exists(here::here("data", "tracks.Rda"))) {
  load(here::here("data", "tracks.Rda"))
} else {
  library(magrittr)
  wastd_url <- wastdr::get_wastd_url()
  q <- list(taxon = "Cheloniidae", format = "json")
  animal_records <- wastdr::wastd_GET("animal-encounters", query = q)
  animals <- wastdr::parse_animal_encounters(animal_records)
  track_records <- wastdr::wastd_GET("turtle-nest-encounters")
  tracks_all <- wastdr::parse_turtle_nest_encounters(track_records)
  disturbance_records <- wastdr::wastd_GET("disturbance-observations")
  disturbance <- disturbance_records %>% wastdr::parse_disturbance_observations()
  survey_records <- wastdr::wastd_GET("surveys")
  surveys <- survey_records %>% parse_surveys()
  nest_records <- wastdr::wastd_GET("nesttag-observations")
  nests_all <- nest_records %>% wastdr::parse_nesttag_observations()
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
    file = here::here("data", "tracks.Rda")
  )
}
