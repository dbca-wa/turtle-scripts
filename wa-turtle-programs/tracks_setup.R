knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(here)
library(wastdr)
library(dplyr)
library(tidyr)
library(magrittr)
library(skimr)
library(leaflet)
library(RColorBrewer)
library(lubridate)
library(listviewer)
library(DT)
library(ggplot2)
library(ckanr)
library(googledrive)
library(ggmap)
library(gganimate)

# Configure ckanr to data.dpaw.wa.gov.au
if (file.exists("~/.Rprofile")) source("~/.Rprofile")
ckanr::ckanr_setup(url = Sys.getenv("CKAN_URL"), key = Sys.getenv("CKAN_API_KEY"))
