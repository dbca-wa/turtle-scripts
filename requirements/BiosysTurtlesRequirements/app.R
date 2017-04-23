library(shiny)
library(gh)
library(tidyverse)
library(magrittr)
library(stringr)
library(markdown)
library(DT)
library(networkD3)
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

extract_related <- function (x) {
  out <- x %>%
    str_extract_all("(?<=#)\\d+(?=\\s|\\n|\\<|$)") %>%
    compact %>%
    unlist %>%
    extract_name(1) %>%
    as.integer %>%
    compact
  if (length(out) == 0) return(NA_integer_) else return(out)
}

gh_issues_url <- "/repos/parksandwildlife/biosys-turtles/issues"
gh_projects_url <- "/repos/parksandwildlife/biosys-turtles/projects"
gh_milestones_url <- "/repos/parksandwildlife/biosys-turtles/milestones"
gh_labels_url <- "/repos/parksandwildlife/biosys-turtles/labels"

ui <- navbarPage(
  "Biosys Turtles Requirements",
      tabPanel("Explore",
               column(7, forceNetworkOutput("force")),
               column(5, uiOutput("issue_selector"), uiOutput("issue_detail"))),
      tabPanel("Business Needs", dataTableOutput("business_needs")),
      tabPanel("Requirement List",dataTableOutput("requirements"))
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


  library(networkD3)

  relations <- reactive({
    d <- issues()
    if (is.null(d)) return(NULL)
    d %>%
      select(id, related) %>%
      filter(!is.na(related)) %>%
      rowwise() %>%
      do(expand.grid(.$id, .$related)) %>%
      rename(source = Var1, target = Var2)
  })

  output$force <- renderForceNetwork({
    forceNetwork(
      Links = data.frame(relations()),
      Nodes = data.frame(issues()),
      Source = "source",
      Target = "target",
      Value = 2,
      NodeID = "title",
      Group = "milestone",
      opacity = 0.8,
      fontSize = 10,
      legend = TRUE,
      arrows = TRUE,
      bounded = FALSE,
      zoom = TRUE,
      clickAction = 'Shiny.onInputChange("selected_issue", d.index + 1)'
    )
  })

  }

shinyApp(ui = ui, server = server)
