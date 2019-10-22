dt <- . %>% DT::datatable(., escape = FALSE, rownames = FALSE)
dt0 <- . %>% DT::datatable(., escape = FALSE, rownames = FALSE, options = list(paging = F))

# Filters -----------------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------------------------#
# Season filters
filter_2016 <- . %>% dplyr::filter(season == 2016)
filter_2017 <- . %>% dplyr::filter(season == 2017)
filter_2018 <- . %>% dplyr::filter(season == 2018)
filter_2019 <- . %>% dplyr::filter(season == 2019)
filter_2020 <- . %>% dplyr::filter(season == 2020)

#--------------------------------------------------------------------------------------------------#
# Site filters - depends on data inside WAStD
filter_bme <- . %>% dplyr::filter(site_id %in% c(22, 23, 24))
filter_bme_cbb1 <- . %>% dplyr::filter(site_id == 22)
filter_bme_cbb2 <- . %>% dplyr::filter(site_id == 23)
filter_bme_cbb3 <- . %>% dplyr::filter(site_id == 24)

filter_emb <- . %>% dplyr::filter(site_id %in% c(36, 37))
filter_emb_annaplains <- . %>% dplyr::filter(site_id == 37)
filter_emb_caravanpark <- . %>% dplyr::filter(site_id == 36)

filter_pth <- . %>% dplyr::filter(site_id %in% c(35, 45))
filter_pth_cemetery <- . %>% dplyr::filter(site_id == 35)
filter_pth_prettypool <- . %>% dplyr::filter(site_id == 45)

filter_wp <- . %>% dplyr::filter(site_id %in% c(25, 26, 27, 46, 47))
filter_wp_coolingwater <- . %>% dplyr::filter(site_id == 26)
filter_wp_bells <- . %>% dplyr::filter(site_id == 25)
filter_wp_yachtblub <- . %>% dplyr::filter(site_id == 27)
filter_wp_boat <- . %>% dplyr::filter(site_id == 46)
filter_wp_cleaverville <- . %>% dplyr::filter(site_id == 47)

filter_con <- . %>% dplyr::filter(site_id == 115)
filter_del <- . %>% dplyr::filter(site_id == 39)
filter_ros <- . %>% dplyr::filter(site_id == 40)
filter_thv <- . %>% dplyr::filter(site_id %in% c(20, 28, 29))
filter_thv_north <- . %>% dplyr::filter(site_id == 28)
filter_thv_south <- . %>% dplyr::filter(site_id == 29)
filter_thv_tagging <- . %>% dplyr::filter(site_id == 20)

filter_lgcs <- . %>% dplyr::filter(site_id %in% c(51, 52))

filter_nin <- . %>% dplyr::filter(site_id %in% 58:114)
filter_nin_mi <- . %>% dplyr::filter(site_id %in% c(58, 59, 113))
filter_nin_nwc <- . %>% dplyr::filter(site_id %in% 66:79)
filter_nin_cr <- . %>% dplyr::filter(site_id %in% 80:95)
filter_nin_bn <- . %>% dplyr::filter(site_id %in% 96:106)
filter_nin_cb <- . %>% dplyr::filter(site_id %in% 107:109)
filter_nin_wr <- . %>% dplyr::filter(site_id %in% 110:111)
filter_nin_gn <- . %>% dplyr::filter(site_id == 114)


causes_disturbance <- c("human", "unknown", "tide", "turtle", "other",
                        "vehicle", "cyclone")
causes_predation <- c("bandicoot", "bird", "cat", "crab", "croc", "dingo",
                       "dog", "fox", "goanna", "pig")
filter_disturbance <- . %>% dplyr::filter(disturbance_cause %in% causes_disturbance)
filter_predation <- . %>% dplyr::filter(disturbance_cause %in% causes_predation)

areas_nin <- areas %>% dplyr::filter(area_type=="Site", pk %in% 59:113)
areas_nin_nwcape <- areas_nin %>% dplyr::filter(area_type=="Site")


gganimate_tracks <- function(data, placename=NULL, prefix=NULL, gm_apikey=NULL) {
  require(purrr)

  pl <- placename %||% "Western Australia"
  pf <- prefix %||% "WA"

  # Basemap: Google Maps
  apikey <- gm_apikey %||%
    Sys.getenv("GOOGLE_MAPS_APIKEY") %||%
    stop("Need a Google Maps API key as system variable GOOGLE_MAPS_APIKEY")
  ggmap::register_google(key = apikey)
  bbx <- ggmap::make_bbox(longitude, latitude, data, f = 0.05)
  ctr <- c(mean(bbx["left"], bbx["right"]), mean(bbx["top"], bbx["bottom"]))
  # basemap <- ggmap::get_map(ctr, zoom=17) %>% ggmap::ggmap()
  basemap <- ggmap::get_googlemap(ctr, zoom = 15, scale = 2, maptype = "hybrid") %>%
    ggmap::ggmap()

  # Add turtle tracks to basemap, discard warnings
  tracks_map <- suppressWarnings(
    basemap + ggplot2::geom_point(aes(longitude, latitude, colour = nest_type), data = data)
  ) +
    ggplot2::ggtitle(glue::glue("Turtle Nesting at {pl}"), subtitle = "Turtle date: {frame_time}") +
    gganimate::transition_time(turtle_date) +
    gganimate::ease_aes("elastic-in") +
    gganimate::exit_fade()

  gganimate::animate(tracks_map, fps = 2, detail = 10) %T>%
    gganimate::anim_save(glue::glue("{pf}_nesting.gif"), .)
}
