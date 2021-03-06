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
gh_milestones_url <- "/repos/parksandwildlife/biosys-turtles/milestones"

#------------------------------------------------------------------------------#
# Load data
# Following https://github.com/jennybc/analyze-github-stuff-with-r
issue_list <- gh(gh_issues_url, state = "all", .limit = Inf)
str(issue_list)
listviewer::jsonedit(issue_list)

# https://developer.github.com/v3/projects/
# Requires custom Accept header
#project_list <- gh(gh_projects_url, state = "all", .limit = Inf,
#                   .send_headers = c(
#                     Accept = "application/vnd.github.inertia-preview+json"))
#listviewer::jsonedit(project_list)

milestone_list <- gh(gh_milestones_url, state = "all", .limit = Inf)
listviewer::jsonedit(milestone_list)

#------------------------------------------------------------------------------#
# Transform data
issues <- issue_list %>% {
    tibble::tibble(
      id = map_int(., "id"),
      number = map_int(., "number"),
      title = map_chr(., "title"),
      body = map_chr_hack(., "body"),
      state = map_chr(., "state"),
      html_url = map_chr(., "html_url"),
      comments_url = map_chr(., "comments_url"),
      labels_url = map_chr(., "labels_url"),
      created_by = map_chr(., c("user", "login")),
      assignee = map_chr(., c("assignee", "login")),
      milestone = map_chr(., c("milestone", "title")),
      created_at = map_chr(., "created_at") %>% as.Date(),
      updated_at = map_chr_hack(., "updated_at") %>% as.Date(),
      due_at = map_chr_hack(., "due_on") %>% as.Date(),
      closed_at = map_chr_hack(., "closed_at") %>% as.Date()
    )
  }

str(issues)
View(issues)

milestones <- milestone_list %>% {
  tibble::tibble(
    id = map_int(., "id"),
    number = map_int(., "number"),
    title = map_chr(., "title"),
    description = map_chr_hack(., "description"),
    state = map_chr(., "state"),
    html_url = map_chr(., "html_url"),
    labels_url = map_chr(., "labels_url"),
    created_by = map_chr(., c("creator", "login")),
    created_at = map_chr(., "created_at") %>% as.Date(),
    updated_at = map_chr_hack(., "updated_at") %>% as.Date(),
    due_at = map_chr_hack(., "due_on") %>% as.Date(),
    closed_at = map_chr_hack(., "closed_at") %>% as.Date()
  )
}
View(milestones)
