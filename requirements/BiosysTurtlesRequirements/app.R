library(shiny)
library(gh)
library(tidyverse)
library(magrittr)
library(markdown)
library(DT)
# TODO set Github access token

map_chr_hack <- function(.x, .f, ...) {
  map(.x, .f, ...) %>% map_if(is.null, ~ NA_character_) %>% flatten_chr()
}

as_url <- function(url, label) {
  paste0("<a href=\"", url, "\" target=\"_\">", label, "</a>")
}

md_to_html <- function(md) {
  if (is.null(md)) return("")
  markdown::markdownToHTML(file = NULL, text = md, fragment.only = TRUE)
}

gh_issues_url <- "/repos/parksandwildlife/biosys-turtles/issues"
gh_projects_url <- "/repos/parksandwildlife/biosys-turtles/projects"
gh_milestones_url <- "/repos/parksandwildlife/biosys-turtles/milestones"

ui <- navbarPage(
  "Biosys Turtles Requirements",
  tabPanel("Business Needs", dataTableOutput("business_needs")),
  tabPanel("Requirements",
           dataTableOutput("requirements"),
           uiOutput("issue_selector")#,
           #uiOutput("issue_detail")
           )
)

server <- function(input, output) {

  issue_list <- reactive(
    withProgress(message = 'Loading requirements...',
                 {gh(gh_issues_url, state = "all", .limit = Inf)}))

  milestone_list <- reactive(
    withProgress(message = 'Loading business needs...',
                 {gh(gh_milestones_url, state = "all", .limit = Inf)}))

  issues <- reactive(
    issue_list() %>% {
    tibble::tibble(
      id = map_int(., "id"),
      number = map_int(., "number"),
      title = map_chr(., "title"),
      body = map_chr_hack(., "body"), # %>% map(md_to_html),
      state = map_chr(., "state"),
      html_url = map_chr(., "html_url") %>% map(as_url, "View"),
      comments_url = map_chr(., "comments_url") %>% map(as_url, "View"),
      labels_url = map_chr(., "labels_url") %>% map(as_url, "View"),
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
        # Requirement = body
      )
  )

  output$business_needs <- shiny::renderDataTable(business_needs(), escape = FALSE)

  output$requirements <- shiny::renderDataTable(requirements(), escape = FALSE)

  output$issue_selector <- renderUI(
    selectizeInput("selected_issue",
    "Show details for requirement",
    setNames(rownames(issues()), issues()$title),
    selected = NULL, multiple = FALSE, options = NULL))

  issue_body <- reactive({
    if (is.na(issues())) return("<p>Loading...</p>")
    issues()[input$selected_issue,]$body
    })

  #output$issue_detail <- reactive(renderMarkdown(file=NULL, text=issue_body()))

}

shinyApp(ui = ui, server = server)
