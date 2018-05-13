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

  hatching_emergence_success <- . %>%
  filter(nest_type=="hatched-nest") %>%
  dplyr::filter(hatching_success >= 0) %>%
  group_by(species) %>%
  dplyr::summarize(
    "count" = n(),
    "clutch_size_mean" = mean(clutch_size) %>% round(digits = 2),
    "clutch_size_sd" = sd(clutch_size) %>% round(digits = 2),
    "clutch_size_min" = min(clutch_size),
    "clutch_size_max" = max(clutch_size),
    "hatching_success_mean" = mean(hatching_success) %>% round(digits = 2),
    "hatching_success_sd" = sd(hatching_success) %>% round(digits = 2),
    "hatching_success_min" = min(hatching_success),
    "hatching_success_max" = max(hatching_success),
    "emergence_success_mean" = mean(emergence_success) %>% round(digits = 2),
    "emergence_success_sd" = sd(emergence_success) %>% round(digits = 2),
    "emergence_success_min" = min(emergence_success),
    "emergence_success_max" = max(emergence_success)
  )
}

ggplot_track_success_by_date <- function(data, species_name, place_name) {
  data %>%
    filter(species == species_name) %>%
    ggplot(aes(x = date)) +
    geom_bar(aes(y = all), stat = "identity", color = "black", fill = "black") +
    geom_bar(aes(y = successful), stat = "identity", color = "green", fill = "green") +
    scale_x_date(breaks = scales::pretty_breaks(),
                 labels = scales::date_format("%d %b %Y")) +
    labs(x = "Date", y = "Number of all and successful tracks") +
    ggtitle(paste("Nesting effort of", species_name %>% humanize),
            subtitle = "Number of all (black) and successful (green) tracks") +
    labs(x = "Date", y = "Number of all and successful tracks") +
    theme_minimal() +
    ggsave(paste0("track_effort_", place_name, "_", species_name, ".pdf"),
           width = 7, height = 5)
}

ggplot_track_successrate_by_date <- function(data, species_name, place_name) {
  data %>%
    filter(species == species_name) %>%
    ggplot(aes(x = date)) +
    geom_bar(aes(y = track_success), stat = "identity") +
    scale_x_date(breaks = scales::pretty_breaks(),
                 labels = scales::date_format("%d %b %Y")) +
    labs(x = "Date", y = "Fraction of tracks with nest") +
    ggtitle(paste("Nesting success of", species_name %>% humanize),
            subtitle = "Fraction of successful over total nesting crawls") +
    theme_light() +
    ggsave(paste0("track_success_", place_name, "_", species_name, ".pdf"),
           width = 7, height = 5)
}

track_success <- function(tracks){
  all_tracks_by_date <- tracks %>%
    dplyr::filter(nest_type %in% c("successful-crawl",
                                   "false-crawl",
                                   "track-unsure",
                                   "track-not-assessed")) %>%
    group_by(date, species) %>% tally() %>% ungroup() %>% rename(all = n)

  successful_tracks_by_date <- tracks %>%
    dplyr::filter(nest_type == 'successful-crawl') %>%
    group_by(date, species) %>% tally() %>% ungroup() %>% rename(successful = n)

  all_tracks_by_date %>%
    left_join(successful_tracks_by_date, by = c('date','species')) %>%
    mutate(successful = ifelse(is.na(successful), 0, successful),
           track_success = 100 * successful/all)
}

track_success_by_species <- function(track_success) {
  track_success %>%
  group_by(species) %>%
  dplyr::summarise(
    mean_nesting_success = mean(track_success) %>% round(digits = 2),
    sd_nesting_success = sd(track_success) %>% round(digits = 2)
  )
}

survey_count <- function(surveys, sid){
  nrow(filter(surveys, site_id==sid))
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

filter_emb <- . %>% dplyr::filter(site_id %in% c(36,37))
filter_emb_ap <- . %>% dplyr::filter(site_id==37) # anna plains
filter_emb_cvp <- . %>% dplyr::filter(site_id==36) # caravan park

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

