library(rayshader)
library(magrittr)
library(raster)
# https://cran.r-project.org/web/packages/elevatr/vignettes/introduction_to_elevatr.html#introduction_to_elevatr

#Here, I load a map for the River Derwent in Tasmania with the raster package:
# localtif = raster::raster("tasmania.tif")
localtif = raster::raster(here::here("data", "dem_01.tif"))

#And convert it to a matrix:
elmat = matrix(raster::extract(localtif,raster::extent(localtif),buffer=1000),
               nrow=ncol(localtif),ncol=nrow(localtif))

#We use another one of rayshader's built-in textures:
elmat %>%
  sphere_shade(texture = "desert") %>%
  plot_map()

#sphere_shade can shift the sun direction:
elmat %>%
  sphere_shade(sunangle = 45, texture = "desert") %>%
  plot_map()

#detect_water and add_water adds a water layer to the map:
elmat %>%
  sphere_shade(texture = "desert") %>%
  add_water(detect_water(elmat), color="desert") %>%
  plot_map()

#And we can add a raytraced layer from that sun direction as well:
elmat %>%
  sphere_shade(texture = "desert") %>%
  add_water(detect_water(elmat), color="desert") %>%
  add_shadow(ray_shade(elmat)) %>%
  plot_map()

elmat %>%
  sphere_shade( texture = "desert") %>%
  add_water(detect_water(elmat), color="desert") %>%
  add_shadow(ray_shade(elmat)) %>%
  add_shadow(ambient_shade(elmat)) %>%
  plot_map()

elmat %>%
  sphere_shade(texture = "desert") %>%
  add_water(detect_water(elmat), color="desert") %>%
  add_shadow(ray_shade(elmat)) %>%
  add_shadow(ambient_shade(elmat)) %>%
  plot_3d(elmat)

mtb_hillshade <- montereybay %>%
  sphere_shade(texture = "imhof1") %>%
  add_shadow(ray_shade(montereybay,zscale=200)) %>%
  add_shadow(ambient_shade(montereybay,zscale=200))

rayshader::plot_3d(
  mtb_hillshade, montereybay, water=TRUE, zscale=50, theta=-45,
  waterdepth = 0, wateralpha = 0.6, watercolor = "#88DDFF",
  waterlinecolor = "white", waterlinealpha = 0.5)
