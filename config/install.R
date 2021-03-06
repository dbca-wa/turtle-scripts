## Install from source
lib <- Sys.getenv("R_LIBS_USER")
setwd("~/projects/turtle-scripts/")


## Install from custom repository
# install.packages("AEM", repos="http://R-Forge.R-project.org")  # offline, use source
# install.packages("PCNM", repos="http://R-Forge.R-project.org") # offline, use source
# install.packages("spacemakeR", repos="http://R-Forge.R-project.org")
# install.packages("packfor", repos="http://R-Forge.R-project.org") ## superceded by adespatial
# install.packages("adespatial", repos="http://R-Forge.R-project.org") # offline, use github

install.packages("tm")
install.packages("~/projects/turtle-scripts/data/Rstem_0.4-1.tar.gz", repos = NULL, type = "source")
install.packages("~/projects/turtle-scripts/data/sentiment_0.2.tar.gz", repos = NULL, type = "source")

## Install from Github
install.packages("devtools")
# adespatial replaces packfor::forward.sel
devtools::install_github("sdray/adespatial")
# devtools::install_github("ropensci/ckanr")
devtools::install_github("ropensci/ckanr")
devtools::install_github("ropensci/geojsonio")
devtools::install_github("rstudio/leaflet")
devtools::install_github("bhaskarvk/leaflet.extras")
# leaflet.extras is cloned to ~/projects/leaflet.extras and built from source
devtools::install_github("tidyverse/ggplot2")
devtools::install_github("tidyverse/dplyr")
devtools::install_github("r-pkgs/gh")
devtools::install_github("pablobarbera/Rfacebook", subdir = "Rfacebook")
devtools::install_github("dbca-wa/wastdr")


install.packages("data/AEM_0.6.tar.gz", repos = NULL, type = "source")
install.packages("data/packfor_0.0-8.tar.gz", repos = NULL, type = "source")
install.packages("data/PCNM_2.1-4.tgz", repos = NULL, type = "source")

## Install packages from CRAN
install.packages(c(
  "RODBC",

  "rgdal",
  "sp",
  "maptools",
  "vegan",
  "ape",
  "spdep",
  "ade4",

  "Hmisc",
  "dygraphs",
  "httr",
  "plyr",
  "tidyverse",
  "magrittr",
  "tidyjson",
  "xts",
  "rvest",
  "here",

  "DT",
  "mapview",
  "datadr",
  "trelliscope",
  "listviewer",
  # "leaflet.extras",
  "wordcloud",
  "networkD3",
  "googledrive",

  "lme4",
  "rstan",
  "rstanarm",
  "shinystan",
  "bayesplot",
  "rstantools",
  "loo"
))
