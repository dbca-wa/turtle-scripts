library(shiny)
library(gh)
library(tidyverse)
library(magrittr)
library(stringr)
library(markdown)
library(DT)
library(networkD3)

#------------------------------------------------------------------------------#
# Settings - change as appropriate
#
Sys.setenv(GH_OWNER = "parksandwildlife")
Sys.setenv(GH_REPO = "biosys-turtles")

#------------------------------------------------------------------------------#
# Helper functions and general witchcraft
#
#' Map given function, handle null as NA and flatten_chr()
map_chr_hack <- function(.x, .f, ...) {
  map(.x, .f, ...) %>% map_if(is.null, ~ NA_character_) %>% flatten_chr()
}

#' Format url and label as HTML hyperlink
as_url <- function(url, label) {
  paste0("<a href=\"", url, "\" target=\"_\">", label, "</a>")
}

#' Convert a string of markdown to HTML
md_to_html <- function(md) {
  if (is.null(md) || is.na(md)) {md <- "no content"}
  markdown::markdownToHTML(file = NULL, text = md, fragment.only = TRUE)
}

#' Extract a element with name or number n from a list x
extract_name <- function(x, n)sapply(x, `[[`, n)

#' Extract a list of mentioned GitHub issue numbers from text
#' require(testthat)
#' testthat::expect_equal(extract_related("#5 #65 #123 #7# #test # test ## #8\n#9"), c(5, 65, 123, 8, 9))
#' testthat::expect_equal(extract_related("#7# #test # test ## "), NA_integer_)
#' testthat::expect_equal(extract_related(""), NA_integer_)
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

#' Wrap gh::gh, use with "issues", "milestones", "labels"
gethub <- function(what,
                   owner = Sys.getenv("GH_OWNER"),
                   repo = Sys.getenv("GH_REPO"),
                   state = "all",
                   .limit = Inf) {
  gh(paste0("/repos/:owner/:repo/", what),
     owner = owner,
     repo = repo,
     state = state,
     .limit = .limit)
}


#------------------------------------------------------------------------------#
# UI
#
ui <- navbarPage(
  "Biosys Turtles Requirements",

  tabPanel(
    "Explore",

    column(
      7,
      forceNetworkOutput("force")),

    column(
      5,
      uiOutput("issue_selector"),
      uiOutput("issue_detail"))),

  tabPanel(
    "Business Needs",
    dataTableOutput("business_needs")),

  tabPanel(
    "Requirement List",
    # uiOutput("category_selector"),
    # uiOutput("priority_selector"),
    uiOutput("download_requirements"),
    dataTableOutput("requirements"))
)

#------------------------------------------------------------------------------#
# Server
#
server <- function(input, output) {

  #----------------------------------------------------------------------------#
  # Summon data from GitHub API
  #
  issue_list <- reactive(
    withProgress(message = 'Loading requirements...', {gethub("issues")}))

  milestone_list <- reactive(
    withProgress(message = 'Loading business needs...', {gethub("milestones")}))

  labels_list <- reactive(
    withProgress(message = 'Loading categories...', {gethub("labels")}))

  #----------------------------------------------------------------------------#
  # Transform primary data
  #
  issues <- reactive({
    i <- issue_list()
    if (is.null(i)) return(NULL)

    i1 <- issue_list() %>% {
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
    } %>%
    arrange(id)

    # untangle labels, loses a few columns, hard-codes existing tags
    i2 <- i1 %>% separate_rows(labels) %>% mutate(tagslog = TRUE) %>%
      spread(labels, tagslog, fill = FALSE)  %>%
      select(id, Business, Stakeholder, Functional, Transition, must, should)

    iss <- left_join(i1, i2, by = "id")
    iss
  })

  milestones <- reactive({
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
    }
  })

  labels <- reactive({
    labels_list() %>% {
      tibble::tibble(
        id = map_int(., "id"),
        url = map_chr(., "url") %>% map(as_url, "View requirements"),
        name = map_chr(., "name"),
        colour = map_chr(., "color")
      )
    }
  })

  #----------------------------------------------------------------------------#
  # Prepare secondary data
  #
  requirements <- reactive({
    d <- issues()
    if (is.null(d)) return(NULL)
    out <- d %>%
      transmute(
        ID = id,
        Source = html_url,
        # Date = due_at,
        Related = related,
        Title = title,
        Business = Business,
        Stakeholder = Stakeholder,
        Functional = Functional,
        # # Nonfunctinoal = Nonfunctional,
        Transition = Transition,
        must_have = must,
        should_have = should,
        # could_have = could,
        # wont_have = wont
        Categories = labels
        #, Requirement = body # too large to include
      )
    # TODO: For each element in selected_*, include rows
    out
  })

  business_needs <- reactive({
    d <- milestones()
    if (is.null(d)) return(NULL)
    d %>%
      transmute(
        ID = number,
        # Date = due_at,
        # Source = created_by,
        Requirements = title
      )
  })

  #----------------------------------------------------------------------------#
  # Build dropdowns
  #
  output$issue_selector <- renderUI({
    d <- issues()
    if (is.null(d)) return(NULL)
    selectizeInput(
      "selected_issue",
      "Show details for requirement",
      setNames(rownames(d), d$title),
      width = '100%')
  })

  output$category_selector <- renderUI({
    d <- labels()
    if (is.null(d)) return(NULL)
    vals <- d %>% filter(grepl("Req", name)) %>% select(name) %>% extract2(1)
    selectizeInput(
      "selected_categories",
      "Categories",
      vals,
      width = '50%',
      selected = vals,
      multiple = TRUE)
  })

  output$priority_selector <- renderUI({
    d <- labels()
    if (is.null(d)) return(NULL)
    vals <- d %>% filter(grepl("have", name)) %>% select(name) %>% extract2(1)
    # vals <- c("Must have", "Should have", "Could have", "Won't have")
    selectizeInput(
      "selected_priorities",
      "Priorities",
      vals,
      width = '50%',
      selected = vals,
      multiple = TRUE)
  })

  #----------------------------------------------------------------------------#
  # Build datatables
  #
  output$business_needs <- shiny::renderDataTable(
    business_needs(),
    options = list(filter = 'top'),
    escape = FALSE
  )

  output$requirements <- shiny::renderDataTable(
    requirements(),
    options = list(filter = 'top'),
    escape = FALSE
  )

  output$issue_detail <- renderUI({
    d <- issues()
    sel <- input$selected_issue
    if (is.null(d) || is.null(sel)) return(NULL)
    HTML(paste(
      d$html_url[[as.integer(sel)]],
      d$body[[as.integer(sel)]]
    ))
  })

  relations <- reactive({
    d <- issues()
    if (is.null(d)) return(NULL)
    d %>%
      select(id, related) %>%
      filter(!is.na(related)) %>%
      rowwise() %>%
      do(expand.grid(.$id, .$related)) %>%
      transmute(source = Var1 - 1,
                target = Var2 - 1)
  })

  output$force <- renderForceNetwork({
    forceNetwork(
      Links = data.frame(relations()),
      Nodes = data.frame(issues()),
      Source = "source",
      Target = "target",
      Value = 2,
      NodeID = "title",
      # Nodesize = calcNodesize(""),
      Group = "milestone",
      opacity = 0.8,
      fontSize = 14,
      fontFamily = "sans",
      charge = -50,
      legend = TRUE,
      arrows = TRUE,
      bounded = TRUE,
      zoom = TRUE,
      height = 800,
      width = 600,
      clickAction = 'Shiny.onInputChange("selected_issue", d.index + 1)'
    )
  })

  output$downloadData <- downloadHandler(
      filename = function() {return("requirements.csv")},
      content = function(file) {
        d <- requirements()
        if (is.null(d)) {return(NULL)}
        write.csv(data.frame(lapply(d, as.character), stringsAsFactors = FALSE),
                  file, sep = ",", row.names = FALSE, fileEncoding = "utf-8")
      }
    )

  output$download_requirements <- renderUI({
    d <- requirements()
    if (is.null(d)) {return(NULL)}
    downloadButton('downloadData', label = "Download CSV")
  })

  }

shinyApp(ui = ui, server = server)
