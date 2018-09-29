# Deprecated
daily_species_by_type <- . %>%
  filter(nest_age == "fresh") %>%
  group_by(season, turtle_date, species, nest_type) %>%
  tally() %>%
  ungroup()

# Deprecated
daily_summary <- . %>%
  daily_species_by_type() %>%
  tidyr::spread(nest_type, n, fill = 0) %>%
  DT::datatable(.)

# Deprecated, use nesting_type_by_season_species
species_by_type <- . %>%
  filter(nest_age == "fresh") %>%
  group_by(season, species, nest_type) %>%
  tally() %>%
  ungroup() %>%
  tidyr::spread(nest_type, n, fill = 0)

# Pivot table of nesting type by season and species
nesting_type_by_season_species <- . %>%
  filter(nest_age == "fresh") %>%
  group_by(season, species, nest_type) %>%
  tally() %>%
  ungroup() %>%
  tidyr::spread(nest_type, n, fill = 0)

# Pivot table of nesting type by season, week and species
nesting_type_by_season_week_species <- . %>%
  filter(nest_age == "fresh") %>%
  group_by(season, week, species, nest_type) %>%
  tally() %>%
  ungroup() %>%
  tidyr::spread(nest_type, n, fill = 0)

# Pivot table of nesting type by season, turtle date and species
nesting_type_by_season_day_species <- . %>%
  filter(nest_age == "fresh") %>%
  group_by(season, turtle_date, species, nest_type) %>%
  tally() %>%
  ungroup()

tracks_ts <- function(data, placename="", prefix="") {
  data %>%
    nesting_type_by_season_day_species() %>%
  {
    ggplot2::ggplot(data = ., aes(x = turtle_date, y = n, colour = nest_type)) +
      ggplot2::geom_point() +
      ggplot2::geom_smooth(method = "auto") +
      # ggplot2::geom_line() +
      ggplot2::facet_grid(rows = vars(season)) +
      ggplot2::ylab("Number of turtle tracks or nests") +
      ggplot2::ggtitle("Nesting activity", subtitle="Number counted per day") +
      ggplot2::scale_x_date(
        date_breaks = "1 month",
        date_minor_breaks = "1 week",
        labels = scales::date_format("%d %b %Y")) +
      ggplot2::scale_y_continuous(limits = c(0, NA)) +
      ggplot2::xlab("Turtle date") +
      ggplot2::theme_classic() +
      ggsave(glue::glue("{prefix}_track_abundance_{wastdr::urlize(placename)}.png"), width = 7, height = 5) +
      NULL
  }
}



hatching_emergence_success <- . %>%
  filter(nest_type=="hatched-nest") %>%
  dplyr::filter(hatching_success >= 0) %>%
  group_by(season, species) %>%
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


ggplot_track_success_by_date <- function(data, speciesname, placename="", prefix="") {
  data %>%
    dplyr::filter(species == speciesname) %>%
    ggplot2::ggplot(aes(x = turtle_date)) +
    ggplot2::facet_grid(rows = vars(season)) +
    ggplot2::geom_bar(aes(y = all), stat = "identity", color = "black", fill = "grey") +
    ggplot2::geom_bar(aes(y = successful), stat = "identity", color = "black", fill = "green") +
    labs(x = "Date", y = "Number of all and successful tracks") +
    ggplot2::ggtitle(paste("Nesting effort of", speciesname %>% humanize),
            subtitle = "Number of all (grey) and successful (green) tracks") +
    ggplot2::scale_x_date(
      date_breaks = "1 month",
      date_minor_breaks = "1 week",
      labels = scales::date_format("%d %b %Y")) +
    ggplot2::scale_y_continuous(limits = c(0, NA)) +
    ggplot2::theme_classic() +
    ggsave(glue::glue("{prefix}_track_effort_{wastdr::urlize(placename)}_{speciesname}.png"), width = 7, height = 5)
}

ggplot_track_successrate_by_date <- function(data, speciesname, placename="", prefix="") {
  data %>%
    dplyr::filter(species == speciesname) %>%
    ggplot2::ggplot(aes(x = turtle_date)) +
    ggplot2::facet_grid(rows = vars(season)) +
    ggplot2::geom_bar(aes(y = track_success), stat = "identity", color = "black", fill = "grey") +
    labs(x = "Date", y = "Fraction of tracks with nest") +
    ggplot2::ggtitle(paste("Nesting success of", speciesname %>% humanize),
            subtitle = "Fraction of successful over total nesting crawls") +
    ggplot2::scale_x_date(
      date_breaks = "1 month",
      date_minor_breaks = "1 week",
      labels = scales::date_format("%d %b %Y")) +
    ggplot2::scale_y_continuous(limits = c(0, NA)) +
    ggplot2::theme_classic() +
    ggsave(glue::glue("{prefix}_track_success_{wastdr::urlize(placename)}_{speciesname}.png"), width = 7, height = 5)
}

track_success <- function(tracks){
  all_tracks_by_date <- tracks %>%
    dplyr::filter(nest_type %in% c("successful-crawl",
                                   "false-crawl",
                                   "track-unsure",
                                   "track-not-assessed")) %>%
    dplyr::group_by(season, turtle_date, species) %>%
    dplyr::tally() %>%
    dplyr::ungroup() %>%
    dplyr::rename(all = n)

  successful_tracks_by_date <- tracks %>%
    dplyr::filter(nest_type == 'successful-crawl') %>%
    dplyr::group_by(season, turtle_date, species) %>%
    dplyr::tally() %>%
    dplyr::ungroup() %>%
    dplyr::rename(successful = n)

  all_tracks_by_date %>%
    dplyr::left_join(successful_tracks_by_date, by = c('turtle_date','species', 'season')) %>%
    dplyr::mutate(
      successful = ifelse(is.na(successful), 0, successful),
      track_success = 100 * successful/all
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

survey_count <- function(surveys, sid, season=2017){
  nrow(filter(surveys, site_id==sid, season==season))
}

survey_ground_covered <- function(surveys, site_id, km_per_survey, season=2017){
  survey_count(surveys, site_id, season) * km_per_survey
}


# disturbance
disturbance_by_season <- . %>%
dplyr::group_by(season, disturbance_cause) %>%
  dplyr::tally() %>%
  dplyr::arrange(-n)

dt <- . %>% DT::datatable(., escape = FALSE, rownames = FALSE)

# Filters -----------------------------------------------------------------------------------------#
# Surveys
filter_surveys_requiring_qa <- . %>%
  dplyr::filter(grepl("QA", start_comments) | grepl("QA", end_comments)) %>%
  dplyr::select(site_name, reporter, season, turtle_date, start_time, end_time,
                start_comments, end_comments, change_url)

filter_surveys_missing_end <- . %>%
  dplyr::filter(is.na(end_source_id)) %>%
  dplyr::select(-site_type, -site_id, -reporter_username, -reporter_id, -id, -absolute_admin_url)

# Seasons
filter_2017 <- . %>% dplyr::filter(season==2017)
filter_2018 <- . %>% dplyr::filter(season==2018)

# Sites
filter_broome <- . %>% dplyr::filter(area_name=="Cable Beach Broome")
filter_broome_sites <- . %>% dplyr::filter(site_id %in% c(22, 23, 24))
filter_cbb1 <- . %>% dplyr::filter(site_name=="Cable Beach Broome Sector 1")
filter_cbb2 <- . %>% dplyr::filter(site_name=="Cable Beach Broome Sector 2")
filter_cbb3 <- . %>% dplyr::filter(site_name=="Cable Beach Broome Sector 3")

filter_emb <- . %>% dplyr::filter(site_id %in% c(36,37))
filter_emb_ap <- . %>% dplyr::filter(site_id==37) # anna plains
filter_emb_cvp <- . %>% dplyr::filter(site_id==36) # caravan park

filter_port_hedland_sites <- . %>% dplyr::filter(site_id %in% c(35, 45))
filter_port_hedland_cemetery <- . %>% dplyr::filter(site_id==35)
filter_port_hedland_prettypool <- . %>% dplyr::filter(site_id==45)

filter_wp <- . %>% dplyr::filter(site_id %in% c(25, 26, 27))
filter_cw <- . %>% dplyr::filter(site_id == 26) # id 26
filter_bb <- . %>% dplyr::filter(site_id == 25) # id 25
filter_yc <- . %>% dplyr::filter(site_id == 27) # id 27

filter_di <- . %>% dplyr::filter(site_id == 39)
filter_ri <- . %>% dplyr::filter(site_id == 40)
filter_thv <- . %>% dplyr::filter(site_id %in% c(20, 28, 29))
filter_thvn <- . %>% dplyr::filter(site_id == 28)
filter_thvs <- . %>% dplyr::filter(site_id == 29)
filter_thvt <- . %>% dplyr::filter(site_id == 20)

filter_nosite <- . %>% dplyr::filter(is.na(site_id))
filter_nosurvey <- . %>% dplyr::filter(is.na(survey_id))
filter_realspecies <- . %>% dplyr::filter(species != 'corolla-corolla')
filter_realsurveys <- . %>% dplyr::filter(is_production == TRUE)
