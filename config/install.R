## Install from source
lib = Sys.getenv("R_LIBS_USER")
setwd("~/projects/turtle-scripts/")
install.packages("data/AEM_0.6.tar.gz", repos = NULL, type = "source", lib = lib)
install.packages("data/PCNM_2.1-4.tgz", repos = NULL, type = "source", lib = lib)

## Install from custom repository
# install.packages("AEM", repos="http://R-Forge.R-project.org", lib = lib)  # offline, use source
# install.packages("PCNM", repos="http://R-Forge.R-project.org", lib = lib) # offline, use source
install.packages("spacemakeR", repos="http://R-Forge.R-project.org", lib = lib)
# install.packages("packfor", repos="http://R-Forge.R-project.org") ## superceded by adespatial
# install.packages("adespatial", repos="http://R-Forge.R-project.org") # offline, use github

## Install from Github
require(devtools)
# adespatial replaces packfor::forward.sel
devtools::install_github("sdray/adespatial", lib = lib)
devtools::install_github("ropensci/ckanr", lib = lib)
devtools::install_github("rstudio/leaflet", lib = lib)
devtools::install_github('bhaskarvk/leaflet.extras', lib)
devtools::install_github("tidyverse/ggplot2", lib = lib)
devtools::install_github("r-pkgs/gh", lib = lib)
devtools::install_github("Rfacebook", "pablobarbera", subdir="Rfacebook", lib=lib)

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
  "httr",
  "plyr",
  "tidyverse",
  "magrittr",

  "DT",
  "mapview",
  "datadr",
  "trelliscope",
  "listviewer",
  "leaflet.extras"
  ),
  lib = lib)
