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
ckanr::resource_update("be63cdd8-4aba-4329-b333-8090b076c792", "QA.html")

rmarkdown::render(here::here("wa-turtle-programs", "lgcs.Rmd"))
ckanr::resource_update("49b31e39-7b40-45cb-aa6f-dabb4c692b69", here::here("wa-turtle-programs", "lgcs.html"))

rmarkdown::render(here::here("wa-turtle-programs", "broome.Rmd"))
ckanr::resource_update("9572e76b-55e9-4afb-b224-7cce7beb4959", here::here("wa-turtle-programs", "broome.html"))

rmarkdown::render(here::here("wa-turtle-programs", "emb.Rmd"))
ckanr::resource_update("56f1071d-b8ed-4ac7-bb37-a5151978a9f1", here::here("wa-turtle-programs", "emb.html"))

rmarkdown::render(here::here("wa-turtle-programs", "pthedland.Rmd"))
rmarkdown::render(here::here("wa-turtle-programs", "pthedland.Rmd"))

rmarkdown::render(here::here("wa-turtle-programs", "westpilbara.Rmd"))
rmarkdown::render(here::here("wa-turtle-programs", "westpilbara.Rmd"))

rmarkdown::render(here::here("wa-turtle-programs", "rosemary.Rmd"))
ckanr::resource_update("aa7a5b85-4606-4e18-bdd4-407d6a887db1", here::here("wa-turtle-programs", "rosemary.html"))

rmarkdown::render(here::here("wa-turtle-programs", "delambre.Rmd"))
ckanr::resource_update("69d5585a-3307-41ee-980e-f829f7eef004", here::here("wa-turtle-programs", "delambre.html"))

rmarkdown::render(here::here("wa-turtle-programs", "thevenard.Rmd"))
ckanr::resource_update("19294b29-a70f-45e3-b06b-fc1eefe858d2", here::here("wa-turtle-programs", "thevenard.html"))

# rmarkdown::render(here::here("wa-turtle-programs", "ningaloo.Rmd"))
