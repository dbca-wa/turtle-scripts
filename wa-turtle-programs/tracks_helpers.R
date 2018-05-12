daily_species_by_type <- . %>%
  filter(nest_age == "fresh") %>%
  group_by(date, species, nest_type) %>%
  tally() %>%
  ungroup()

daily_summary <- . %>%
  daily_species_by_type() %>%
  tidyr::spread(nest_type, n, fill = 0) %>%
  DT::datatable(.)

species_by_type <- . %>%
  filter(nest_age == "fresh") %>%
  group_by(species, nest_type) %>%
  tally() %>%
  ungroup() %>%
  tidyr::spread(nest_type, n, fill = 0)

tracks_ts <- . %>%
  daily_species_by_type() %>%
  {
    ggplot2::ggplot(data = ., aes(x = date, y = n, colour = nest_type)) +
      ggplot2::geom_point() +
      ggplot2::geom_smooth(method = "auto") +
      # ggplot2::geom_line() +
      ggplot2::scale_x_date(
        breaks = scales::pretty_breaks(),
        labels = scales::date_format("%d %b %Y")
      ) +
      ggplot2::scale_y_continuous(limits = c(0, NA)) +
      ggplot2::xlab("Date") +
      ggplot2::ylab("Number counted per day") +
      ggplot2::ggtitle("Nesting activity") +
      ggplot2::theme_light()
  }



map_dist <- function(dist){
  . <- NULL

  pal <- leaflet::colorFactor(
    palette = 'Set1',
    domain = dist$disturbance_cause
  )

  layersControlOptions <- NULL
  l <- leaflet(width = 800, height = 600) %>%
    addProviderTiles("Esri.WorldImagery", group = "Aerial") %>%
    addProviderTiles("OpenStreetMap.Mapnik", group = "Place names") %>%
    clearBounds()

  dist.df <- dist %>% split(dist$disturbance_cause)

  names(dist.df) %>%
    purrr::walk(function(df) {
      l <<- l %>% addAwesomeMarkers(data = dist.df[[df]],
                                    lng = ~longitude, lat = ~latitude,
                                    icon = leaflet::makeAwesomeIcon(
                                      text = ~stringr::str_sub(disturbance_cause, 0,1),
                                      markerColor = ~pal(disturbance_cause)),
                                    label = ~paste(date, disturbance_cause),
                                    popup = ~paste(date, disturbance_cause, "\n", comments),
                                    group = df)
    })

  l %>%
    addLayersControl(
      baseGroups = c("Aerial", "Place names"),
      overlayGroups = names(dist.df),
      options = layersControlOptions(collapsed = FALSE))
}

survey_count <- function(surveys, site_id){
  surveys %>% filter(site_id==site_id) %>% nrow
}
survey_ground_covered <- function(surveys, site_id, km_per_survey){
  survey_count(surveys, site_id) * km_per_survey
}


filter_2017 <- . %>% dplyr::filter(date > dmy("17/10/2017"))

filter_broome <- . %>% dplyr::filter(area_name=="Cable Beach Broome")
filter_broome_sites <- . %>% dplyr::filter(site_id %in% c(22, 23, 24))
filter_cbb1 <- . %>% dplyr::filter(site_name=="Cable Beach Broome Sector 1")
filter_cbb2 <- . %>% dplyr::filter(site_name=="Cable Beach Broome Sector 2")
filter_cbb3 <- . %>% dplyr::filter(site_name=="Cable Beach Broome Sector 3")

filter_port_hedland_sites <- . %>% dplyr::filter(site_id %in% c(35, 45))
filter_port_hedland_cemetery <- . %>% dplyr::filter(site_name=="Port Hedland Cemetery Beach")
filter_port_hedland_prettypool <- . %>% dplyr::filter(site_name=="Port Hedland Pretty Pool Beach")

filter_wp <- . %>% dplyr::filter(site_id %in% c(25, 26, 27))
filter_cw <- . %>% dplyr::filter(site_name == "Cooling Water Beach") # id 26
filter_bb <- . %>% dplyr::filter(site_name == "Bells Beach") # id 25
filter_yc <- . %>% dplyr::filter(site_name == "Wickam Yacht Club Beach") # id 27

filter_di <- . %>% dplyr::filter(site_id == 39)

filter_thv <- . %>% dplyr::filter(site_id %in% c(20, 28, 29))
filter_thvn <- . %>% dplyr::filter(site_id == 28)
filter_thvs <- . %>% dplyr::filter(site_id == 29)
filter_thvt <- . %>% dplyr::filter(site_id == 20)

