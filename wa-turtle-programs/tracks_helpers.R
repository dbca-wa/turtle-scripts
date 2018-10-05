# Deprecated --------------------------------------------------------------------------------------#
daily_species_by_type <- . %>%
  filter(nest_age == "fresh") %>%
  group_by(season, turtle_date, species, nest_type) %>%
  tally() %>%
  ungroup()

daily_summary <- . %>%
  daily_species_by_type() %>%
  tidyr::spread(nest_type, n, fill = 0) %>%
  DT::datatable(.)

# use nesting_type_by_season_species
species_by_type <- . %>%
  filter(nest_age == "fresh") %>%
  group_by(season, species, nest_type) %>%
  tally() %>%
  ungroup() %>%
  tidyr::spread(nest_type, n, fill = 0)

# wastdr staging ----------------------------------------------------------------------------------#
# Pivot table of nesting type by season and species
nesting_type_by_season_species <- . %>%
  dplyr::filter(nest_age == "fresh") %>%
  dplyr::group_by(season, species, nest_type) %>%
  dplyr::tally() %>%
  dplyr::ungroup() %>%
  tidyr::spread(nest_type, n, fill = 0)

# Pivot table of nesting type by season, season_week, iso_week, and species
nesting_type_by_season_week_species <- . %>%
  dplyr::filter(nest_age == "fresh") %>%
  dplyr::group_by(season, season_week, iso_week, species, nest_type) %>%
  dplyr::tally() %>%
  dplyr::ungroup() %>%
  tidyr::spread(nest_type, n, fill = 0)


#' True date as days since fiscal year start
tdate_as_fdate <- . %>%
  {
    lubridate::as.duration(as_date(glue::glue("{wastdr::datetime_as_season(.)}-07-01")) %--% .)
  } %>%
  as.numeric("days")

#' Days since fiscal year start as true date
fdate_as_tdate <- . %>% {
  ddays(.) + lubridate::as_date("2000-07-01")
} %>% format("%d %b")


# Pivot table of nesting type by season, turtle date and species
nesting_type_by_season_day_species <- . %>%
  dplyr::filter(nest_age == "fresh") %>%
  # dplyr::mutate(day = tdate_as_fdate(turtle_date),
  #               test_date = fdate_as_tdate(day)) %>%
  dplyr::group_by(season, turtle_date, species, nest_type) %>%
  dplyr::tally() %>%
  dplyr::ungroup()



# Plot of nesting_type_by_season_day_species over time
tracks_ts <- function(data,
                      surveys,
                      placename="",
                      prefix="") {
  fname <- glue::glue("{prefix}_track_abundance_{wastdr::urlize(placename)}.png")
  data %>%
    nesting_type_by_season_day_species() %>%
    {
      ggplot2::ggplot() +
        ggplot2::facet_grid(rows = vars(season), scales = "free_x") +
        ggplot2::scale_x_continuous(labels = function(x) fdate_as_tdate(x)) +
        ggplot2::scale_y_continuous(limits = c(0, NA)) +
        ggplot2::geom_bar(data = surveys, aes(x = tdate_as_fdate(turtle_date)), show.legend = F) +
        ggalt::geom_lollipop(data = .,
                             aes(x = tdate_as_fdate(turtle_date), y = n, colour = nest_type),
                             point.size = 2) +
        ggplot2::ggtitle(
          glue::glue("Nesting activity at {placename}"),
          subtitle = "Number counted per day (points) over number of surveys (bars)") +
        ggplot2::ylab("Number of turtle tracks or nests") +
        ggplot2::xlab("Turtle date") +
        ggplot2::guides(colour=guide_legend(title="Nest type")) +
        ggplot2::theme_classic() +
        ggplot2::ggsave(fname, width = 10, height = 6)
    }
}


hatching_emergence_success <- . %>%
  dplyr::filter(nest_type == "hatched-nest") %>%
  dplyr::filter(hatching_success >= 0) %>%
  dplyr::group_by(season, species) %>%
  dplyr::summarize(
    "count" = n(),
    "clutch_size_fresh" = mean(clutch_size_fresh) %>% round(digits = 2),
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


ggplot_track_success_by_date <- function(data, speciesname, placename="", prefix="") {
  data %>%
    dplyr::filter(species == speciesname) %>%
    ggplot2::ggplot(aes(x = tdate_as_fdate(turtle_date))) +
    ggplot2::facet_grid(rows = vars(season), scales = "free_x") +
    ggplot2::geom_bar(aes(y = all), stat = "identity", color = "black", fill = "grey") +
    ggplot2::geom_bar(aes(y = successful), stat = "identity", color = "black", fill = "green") +
    ggplot2::ggtitle(paste("Nesting effort of", speciesname %>% humanize()),
      subtitle = "Number of all (grey) and successful (green) tracks"
    ) +
    labs(x = "Date", y = "Number of all and successful tracks") +
    ggplot2::scale_x_continuous(labels = function(x) fdate_as_tdate(x)) +
    ggplot2::scale_y_continuous(limits = c(0, NA)) +
    ggplot2::theme_classic() +
    ggsave(glue::glue("{prefix}_track_effort_{wastdr::urlize(placename)}_{speciesname}.png"), width = 10, height = 6)
}

ggplot_track_successrate_by_date <- function(data, speciesname, placename="", prefix="") {
  data %>%
    dplyr::filter(species == speciesname) %>%
    ggplot2::ggplot(aes(x = tdate_as_fdate(turtle_date))) +
    ggplot2::facet_grid(rows = vars(season), scales = "free_x") +
    ggplot2::geom_bar(aes(y = track_success), stat = "identity", color = "black", fill = "grey") +
    ggplot2::ggtitle(paste("Nesting success of", speciesname %>% humanize()),
      subtitle = "Fraction of successful over total nesting crawls"
    ) +
    labs(x = "Date", y = "Fraction of tracks with nest") +
    ggplot2::scale_x_continuous(labels = function(x) fdate_as_tdate(x)) +
    ggplot2::scale_y_continuous(limits = c(0, NA)) +
    ggplot2::theme_classic() +
    ggsave(glue::glue("{prefix}_track_success_{wastdr::urlize(placename)}_{speciesname}.png"), width = 10, height = 6)
}

track_success <- function(tracks) {
  all_tracks_by_date <- tracks %>%
    dplyr::filter(nest_type %in% c(
      "successful-crawl",
      "false-crawl",
      "track-unsure",
      "track-not-assessed"
    )) %>%
    dplyr::group_by(season, turtle_date, species) %>%
    dplyr::tally() %>%
    dplyr::ungroup() %>%
    dplyr::rename(all = n)

  successful_tracks_by_date <- tracks %>%
    dplyr::filter(nest_type == "successful-crawl") %>%
    dplyr::group_by(season, turtle_date, species) %>%
    dplyr::tally() %>%
    dplyr::ungroup() %>%
    dplyr::rename(successful = n)

  all_tracks_by_date %>%
    dplyr::left_join(successful_tracks_by_date, by = c("turtle_date", "species", "season")) %>%
    dplyr::mutate(
      successful = ifelse(is.na(successful), 0, successful),
      track_success = 100 * successful / all
    )
}

track_success_by_species <- function(track_success) {
  track_success %>%
    group_by(season, species) %>%
    dplyr::summarise(
      mean_nesting_success = mean(track_success) %>% round(digits = 2),
      sd_nesting_success = sd(track_success) %>% round(digits = 2)
    )
}

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

survey_count <- function(surveys, sid, season=2017) {
  nrow(filter(surveys, site_id == sid, season == season))
}

survey_ground_covered <- function(surveys, site_id, km_per_survey, season=2017) {
  survey_count(surveys, site_id, season) * km_per_survey
}

survey_count_heatmap <- function(surveys, placename, prefix) {
  surveys %>%
    wastdr::surveys_per_site_name_and_date(.) %>%
    ggTimeSeries::ggplot_calendar_heatmap("turtle_date", "n") +
    ggplot2::scale_fill_continuous(low = "green", high = "red") +
    ggplot2::facet_grid(rows = vars(Year)) +
    ggplot2::ggtitle(glue::glue("Survey effort at {placename}")) +
    xlab(NULL) + ylab(NULL) +
    ggplot2::theme_classic() +
    ggsave(
      glue::glue("{prefix}_survey_count_heatmap_{wastdr::urlize(placename)}.png"),
      width = 10, height = 6
    )
}

survey_hours_heatmap <- function(surveys, placename, prefix) {
  surveys %>%
    wastdr::survey_hours_per_site_name_and_date(.) %>%
    ggTimeSeries::ggplot_calendar_heatmap("turtle_date", "hours_surveyed") +
    ggplot2::scale_fill_continuous(low = "green", high = "red") +
    ggplot2::facet_grid(rows = vars(Year)) +
    ggplot2::ggtitle(glue::glue("Survey effort at {placename}")) +
    xlab(NULL) + ylab(NULL) +
    ggplot2::theme_classic() +
    ggsave(
      glue::glue("{prefix}_survey_hours_heatmap_{wastdr::urlize(placename)}.png"),
      width = 10, height = 6
    )
}

# disturbance
disturbance_by_season <- . %>%
  dplyr::group_by(season, disturbance_cause) %>%
  dplyr::tally() %>%
  dplyr::arrange(-n)

dt <- . %>% DT::datatable(., escape = FALSE, rownames = FALSE)

# Filters -----------------------------------------------------------------------------------------#

filter_2017 <- . %>% dplyr::filter(season == 2017)
filter_2018 <- . %>% dplyr::filter(season == 2018)

# Sites
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

filter_del <- . %>% dplyr::filter(site_id == 39)
filter_ros <- . %>% dplyr::filter(site_id == 40)
filter_thv <- . %>% dplyr::filter(site_id %in% c(20, 28, 29))
filter_thv_north <- . %>% dplyr::filter(site_id == 28)
filter_thv_south <- . %>% dplyr::filter(site_id == 29)
filter_thv_tagging <- . %>% dplyr::filter(site_id == 20)

filter_lgcs <- . %>% dplyr::filter(site_id %in% c(51, 52))

filter_nosite <- . %>% dplyr::filter(is.na(site_id))
filter_nosurvey <- . %>% dplyr::filter(is.na(survey_id))
filter_realspecies <- . %>% dplyr::filter(species != "corolla-corolla")
filter_realsurveys <- . %>% dplyr::filter(is_production == TRUE)

# filters for wastdr ------------------------------------------------------------------------------#
# Encounters
exclude_training_species <- . %>% dplyr::filter(species != "corolla-corolla")
filter_missing_survey <- . %>% dplyr::filter(is.na(survey_id))
filter_missing_site <- . %>% dplyr::filter(is.na(site_id))

# Surveys
exclude_training_surveys <- . %>% dplyr::filter(is_production == TRUE)
filter_surveys_requiring_qa <- . %>%
  dplyr::filter(grepl("QA", start_comments) | grepl("QA", end_comments)) %>%
  dplyr::select(change_url, turtle_date, site_name, reporter, reporter_username, start_comments, end_comments)

filter_surveys_missing_end <- . %>%
  dplyr::filter(is.na(end_source_id)) %>%
  dplyr::select(
    change_url, turtle_date, site_name, reporter, season,
    start_time, end_time, start_comments, end_comments
  )

survey_season_stats <- . %>%
  dplyr::group_by(season) %>%
  dplyr::summarise(
    first_day = min(turtle_date),
    last_day = max(turtle_date),
    season_length_days = as.numeric(first_day %--% last_day)/(3600*24),
    number_surveys = n(),
    hours_surveyed = round(sum(duration_hours))
  )

survey_show_detail <- . %>%
  dplyr::select(
    change_url, site_name, season, turtle_date, is_production,
    start_time, end_time, duration_hours,
    start_comments, end_comments, status)
