# Set up
source(here::here("wa-turtle-programs/tracks_setup.R"))
source(here::here("wa-turtle-programs/tracks_helpers.R"))

# Data
load(here::here("data", "tracks.Rda"))
library(magrittr)
wastd_url <- wastdr::get_wastd_url()
q <- list(taxon = "Cheloniidae", format = "json")
# animal_records <- wastdr::wastd_GET("animal-encounters", query = q)
# animals <- wastdr::parse_animal_encounters(animal_records)
track_records <- wastdr::wastd_GET("turtle-nest-encounters")
tracks_all <- wastdr::parse_turtle_nest_encounters(track_records)
disturbance_records <- wastdr::wastd_GET("disturbance-observations")
disturbance <- disturbance_records %>% wastdr::parse_disturbance_observations()
survey_records <- wastdr::wastd_GET("surveys")
surveys <- survey_records %>%
  wastdr::parse_surveys() %>%
  dplyr::mutate(change_url = glue::glue(
    '<a href="{wastd_url}/admin/observations/survey/{id}/change/" target="_">Update Survey {id}</a>'))
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

# Analysis - upload html output after render
rmarkdown::render(here::here("wa-turtle-programs", "QA.Rmd"))
rmarkdown::render(here::here("wa-turtle-programs", "QA.Rmd"))

rmarkdown::render(here::here("wa-turtle-programs", "lgcs.Rmd"))
rmarkdown::render(here::here("wa-turtle-programs", "lgcs.Rmd"))

rmarkdown::render(here::here("wa-turtle-programs", "broome.Rmd"))
rmarkdown::render(here::here("wa-turtle-programs", "broome.Rmd"))

rmarkdown::render(here::here("wa-turtle-programs", "emb.Rmd"))
rmarkdown::render(here::here("wa-turtle-programs", "emb.Rmd"))

rmarkdown::render(here::here("wa-turtle-programs", "pthedland.Rmd"))
rmarkdown::render(here::here("wa-turtle-programs", "pthedland.Rmd"))

rmarkdown::render(here::here("wa-turtle-programs", "westpilbara.Rmd"))
rmarkdown::render(here::here("wa-turtle-programs", "westpilbara.Rmd"))

rmarkdown::render(here::here("wa-turtle-programs", "conzinc.Rmd"))
rmarkdown::render(here::here("wa-turtle-programs", "conzinc.Rmd"))

rmarkdown::render(here::here("wa-turtle-programs", "rosemary.Rmd"))
rmarkdown::render(here::here("wa-turtle-programs", "rosemary.Rmd"))

rmarkdown::render(here::here("wa-turtle-programs", "delambre.Rmd"))
rmarkdown::render(here::here("wa-turtle-programs", "delambre.Rmd"))

rmarkdown::render(here::here("wa-turtle-programs", "thevenard.Rmd"))
rmarkdown::render(here::here("wa-turtle-programs", "thevenard.Rmd"))

rmarkdown::render(here::here("wa-turtle-programs", "ningaloo.Rmd"))
rmarkdown::render(here::here("wa-turtle-programs", "ningaloo.Rmd"))
