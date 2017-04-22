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
  if (is.null(md) || is.na(md)) {md <- "no content"}
  markdown::markdownToHTML(file = NULL, text = md, fragment.only = TRUE)
}

extract_name <- function(x, n)sapply(x, `[[`, n)

extract_related <- . %>% str_extract_all("(?<=#)\\d+(?=\\s)") %>% compact %>% unlist

gh_issues_url <- "/repos/parksandwildlife/biosys-turtles/issues"
gh_projects_url <- "/repos/parksandwildlife/biosys-turtles/projects"
gh_milestones_url <- "/repos/parksandwildlife/biosys-turtles/milestones"
gh_labels_url <- "/repos/parksandwildlife/biosys-turtles/labels"

ui <- navbarPage(
  "Biosys Turtles Requirements",
      tabPanel("Business Needs", dataTableOutput("business_needs")),
      tabPanel("Requirement List",dataTableOutput("requirements")),
      tabPanel("Requirement detail", uiOutput("issue_selector"), uiOutput("issue_detail"))
)

server <- function(input, output) {

  issue_list <- reactive(
    withProgress(message = 'Loading requirements...',
                 {gh(gh_issues_url, state = "all", .limit = Inf)}))

  milestone_list <- reactive(
    withProgress(message = 'Loading business needs...',
                 {gh(gh_milestones_url, state = "all", .limit = Inf)}))

  labels_list <- reactive(
    withProgress(message = 'Loading requirement categories...',
                 {gh(gh_labels_url, state = "all", .limit = Inf)}))

  issues <- reactive(
    issue_list() %>% {
    tibble::tibble(
      id = map_int(., "number"),
      title = map_chr(., "title"),
      body = map_chr_hack(., "body") %>% map(md_to_html),
      related = body %>% map(extract_related),
      state = map_chr(., "state"),
      html_url = map_chr(., "html_url") %>% map(as_url, "View issue"),
      comments_url = map_chr(., "comments_url") %>% map(as_url, "View comments"),
      labels_url = map_chr(., "labels_url") %>% map(as_url, "View labels"),
      labels = map(., "labels") %>% map(extract_name, "name"),
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


  labels <- reactive(
    labels_list() %>% {
    tibble::tibble(
      id = map_int(., "id"),
      url = map_chr(., "url") %>% map(as_url, "View requirements"),
      name = map_chr(., "name"),
      colour = map_chr(., "color")
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

  output$label_selector <- renderUI({
    d <- labels()
    if (is.null(d)) return(NULL)
    selectizeInput(
      "selected_labels",
      "Show categories",
      setNames(d$name, d$name),
      multiple = TRUE)
  })

  requirements <- reactive({
    d <- issues()
    if (is.null(d)) return(NULL)
    d %>%
      transmute(
        ID = id,
        # Date = due_at,
        Related = related,
        Title = title,
        KanboardItem = html_url,
        Categories = labels
        # Requirement = body
      ) #%>% filter(d, Categories %in% input$selected_labels)
  })

  output$business_needs <- shiny::renderDataTable(business_needs(), escape = FALSE)

  output$requirements <- shiny::renderDataTable(requirements(), escape = FALSE)

  output$issue_selector <- renderUI({
    d <- issues()
    if (is.null(d)) return(NULL)
    selectizeInput(
      "selected_issue",
      "Show details for requirement",
      setNames(rownames(d), d$title))
  })

  output$issue_detail <- renderUI({
    d <- issues()
    sel <- input$selected_issue
    if (is.null(d) || is.null(sel)) return(NULL)
    HTML(d$body[[as.integer(sel)]])
  })

}

shinyApp(ui = ui, server = server)
