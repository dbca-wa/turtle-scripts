library(shiny)
library(gh)
library(tidyverse)
library(magrittr)
library(DT)
# TODO set Github access token

map_chr_hack <- function(.x, .f, ...) {
  map(.x, .f, ...) %>% map_if(is.null, ~ NA_character_) %>% flatten_chr()
}

gh_issues_url <- "/repos/parksandwildlife/biosys-turtles/issues"
gh_projects_url <- "/repos/parksandwildlife/biosys-turtles/projects"
gh_milestones_url <- "/repos/parksandwildlife/biosys-turtles/milestones"

ui <- navbarPage("Biosys Turtles Requirements",
  tabPanel("Business Needs", dataTableOutput("business_needs")),
  tabPanel("Requirements", dataTableOutput("requirements"))
)

server <- function(input, output) {
  issue_list <- reactive(gh(gh_issues_url, state = "all", .limit = Inf))
  milestone_list <- reactive(gh(gh_milestones_url, state = "all", .limit = Inf))

  issues <- reactive(
    issue_list() %>% {
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
  })

  milestones <- reactive(
    milestone_list() %>% {
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
  })

  business_needs <- reactive(
    milestones() %>%
      transmute(
        ID = id,
        Date = due_at,
        Source = created_by,
        Requirements = title
      )
  )

  requirements <- reactive(
    issues() %>%
      transmute(
        ID = id,
        Date = due_at,
        Title = title,
        KanboardItem = html_url
      )
  )

  output$business_needs <- renderDataTable(business_needs())
  output$requirements <- renderDataTable(requirements())

}

# Run the application
shinyApp(ui = ui, server = server)
