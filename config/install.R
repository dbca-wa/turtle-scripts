## Install from source
setwd("~/projects/turtle-scripts/")
install.packages("data/AEM_0.6.tar.gz", repos = NULL, type = "source", lib = Sys.getenv("R_LIBS_USER"))
install.packages("data/PCNM_2.1-4.tgz", repos = NULL, type = "source", lib = Sys.getenv("R_LIBS_USER"))

## Install from custom repository
# install.packages("AEM", repos="http://R-Forge.R-project.org", lib = Sys.getenv("R_LIBS_USER"))  # offline, use source
# install.packages("PCNM", repos="http://R-Forge.R-project.org", lib = Sys.getenv("R_LIBS_USER")) # offline, use source
install.packages("spacemakeR", repos="http://R-Forge.R-project.org", lib = Sys.getenv("R_LIBS_USER"))
# install.packages("packfor", repos="http://R-Forge.R-project.org") ## superceded by adespatial
# install.packages("adespatial", repos="http://R-Forge.R-project.org") # offline, use github

## Install from Github
require(devtools)
# adespatial replaces packfor::forward.sel
devtools::install_github("sdray/adespatial", lib = Sys.getenv("R_LIBS_USER"))
devtools::install_github("ropensci/ckanr", lib = Sys.getenv("R_LIBS_USER"))
devtools::install_github("rstudio/leaflet", lib = Sys.getenv("R_LIBS_USER"))
devtools::install_github("tidyverse/ggplot2", lib = Sys.getenv("R_LIBS_USER"))
devtools::install_github("r-pkgs/gh", lib = Sys.getenv("R_LIBS_USER"))

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
  "listviewer"
  ),
  lib = Sys.getenv("R_LIBS_USER"))
