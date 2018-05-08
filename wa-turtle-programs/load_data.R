if (file.exists("../data/tracks.Rda")) {
  load("../data/tracks.Rda")
} else {
  q <- list(taxon = "Cheloniidae", format = "json")
  animal_records <- wastdr::wastd_GET("animal-encounters", query = q)
  animals <- wastdr::parse_animal_encounters(animal_records)
  track_records <- wastdr::wastd_GET("turtle-nest-encounters")
  tracks_all <- wastdr::parse_turtle_nest_encounters(track_records)
  disturbance <- wastdr::wastd_GET("disturbance-observations") %>%
    wastdr::parse_disturbance_observations()
  surveys <- wastd_GET("surveys") %>% wastdr::parse_surveys()
  save(animal_records, animals, tracks_all, track_records,
       disturbance, surveys, file = "../data/tracks.Rda"
  )
}
