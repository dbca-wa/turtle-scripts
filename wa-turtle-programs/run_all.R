# Set up
source(here::here("wa-turtle-programs/tracks_helpers.R"))

# Data: download and save
download_and_save_odkc(datafile=here::here("wa-turtle-programs", "data_odkc.Rda"))
download_and_save_tsc(datafile=here::here("wa-turtle-programs", "data_tsc.Rda"))

# Data: load in to memory
load_saved_data_odkc(datafile=here::here("wa-turtle-programs", "data_odkc.Rda"))
load_saved_data_tsc(datafile=here::here("wa-turtle-programs", "data_tsc.Rda"))


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
