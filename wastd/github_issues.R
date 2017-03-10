setwd("~/projects/turtle-scripts/wastd")
if (file.exists("../config/setup.R")) source("../config/setup.R")
if (file.exists("../config/load.R")) source("../config/load.R")

#------------------------------------------------------------------------------#
# Helper functions
#
#' Map given function, handle null as NA and flatten_chr()
map_chr_hack <- function(.x, .f, ...) {
  map(.x, .f, ...) %>% map_if(is.null, ~ NA_character_) %>% flatten_chr()
}

#------------------------------------------------------------------------------#
# Settings
#
gh_issues_url <- "/repos/parksandwildlife/biosys-turtles/issues"
gh_projects_url <- "/repos/parksandwildlife/biosys-turtles/projects"

#------------------------------------------------------------------------------#
# Load data
# Following https://github.com/jennybc/analyze-github-stuff-with-r
issue_list <- gh(gh_issues_url, state = "all", .limit = Inf)
str(issue_list)
listviewer::jsonedit(issue_list)

# https://developer.github.com/v3/projects/
# Requires custom Accept header
project_list <- gh(gh_projects_url, state = "all", .limit = Inf,
                   .send_headers = c(
                     Accept = "application/vnd.github.inertia-preview+json"))
listviewer::jsonedit(project_list)

# projetcs > columns > cards = issues
# https://developer.github.com/v3/projects/cards/


#------------------------------------------------------------------------------#
# Transform data
iss_df <- issue_list %>%
{
  data_frame(
    id = map_int(., "id"),
    number = map_int(., "number"),
    title = map_chr(., "title"),
    body = map_chr(., "body"),
    state = map_chr(., "state"),
    comments_url = map_chr(., "comments_url"),
    html_url = map_chr(., "html_url"),
    created_by = map_chr(., c("user", "login")),
    assignee = map_chr(., c("assignee", "login")),
    milestone = map_chr(., c("milestone", "title")),
    created_at = map_chr(., "created_at") %>% as.Date(),
    updated_at = map_chr_hack(., "updated_at") %>% as.Date(),
    due_at = map_chr_hack(., "due_on") %>% as.Date(),
    closed_at = map_chr_hack(., "closed_at") %>% as.Date()
    )
}
str(iss_df)
View(iss_df)
