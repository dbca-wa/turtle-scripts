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
# Sys.setenv(GITHUB_PAT="my-GH-personal-access-token")

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
                   # .token = Sys.getenv("GITHUB_PAT"),
                   .limit = Inf) {
  gh(paste0("/repos/:owner/:repo/", what),
     owner = owner,
     repo = repo,
     # .token = .token,
     .limit = .limit)
}

#' Build a tbl_df of GH issues from a GH API response
make_issues <- function(ii){
  i1 <- ii %>% {
    tibble::tibble(
      id = map_int(., "number"),
      title = map_chr(., "title"),
      body = map_chr_hack(., "body") %>% map(md_to_html),
      related = body %>% map(extract_related),
      state = map_chr(., "state"),
      url = map_chr(., "html_url"),
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
  } %>% arrange(id)

  # untangle labels, loses a few columns, hard-codes existing tags
  # This will blow up on multiple "... Requirements"
  i2 <- i1 %>%
    separate_rows(labels) %>%
    mutate(tagslog = TRUE) %>%
    spread(labels, tagslog, fill = FALSE)  %>%
    select(id, Business, Stakeholder, Functional, Transition, must, should)

  iss <- left_join(i1, i2, by = "id")
  iss
}

#' Build a tbl_df of GH milestones from a GH API response
make_milestones <- function(mm){
  mm %>% {
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
}

#' Build a tbl_df of GH labels from a GH API response
make_labels <- function(ll){
  ll %>% {
    tibble::tibble(
      id = map_int(., "id"),
      url = map_chr(., "url") %>% map(as_url, "View requirements"),
      name = map_chr(., "name"),
      colour = map_chr(., "color")
    )
  }
}

#' Return selected columns for a tbl_df of GH issues (labels pivoted)
make_requirements <- function(ii){
  ii %>%
    transmute(
      ID = id,
      Source = html_url,
      # Date = due_at,
      Related = related,
      Title = title,
      Business = Business,
      Stakeholder = Stakeholder,
      Functional = Functional,
      # # Nonfunctional = Nonfunctional,
      Transition = Transition,
      must_have = must,
      should_have = should,
      # could_have = could,
      # wont_have = wont
      Categories = labels
      #, Requirement = body # too large to include
    )
}

#' Return selected columns for a tbl_df of GH milestones
make_business_needs <- function(mm){
  mm %>%
    transmute(
      ID = number,
      # Date = due_at,
      # Source = created_by,
      Requirements = title
    )
}

lookup_issue_index <- function(issue_lookup, issue_id){
  issue_lookup %>% filter(id == issue_id) %>% extract("zerorn") %>% as.integer
}

#' Extract related issues (0-indexed) if mentioned in issue body
make_relations <- function(df){
  dd <- df %>%
    mutate(zerorn = as.integer(rownames(df)) - 1) %>%
    select(id, zerorn)

  out <- df %>%
    select(id, related) %>%
    filter(!is.na(related)) %>%
    rowwise() %>%
    do(expand.grid(.$id, .$related)) %>%
    transmute(# source_id = Var1,
              # target_id = Var2,
              source = lookup_issue_index(dd, Var1),
              target = lookup_issue_index(dd, Var2)) %>%
    filter(!is.na(target)) %>%
    data.frame
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
      forceNetworkOutput("force", width = "600px", height = "600px")
      ),

    column(
      5,
      uiOutput("download_requirements"),
      uiOutput("issue_selector"),
      uiOutput("issue_detail"))),

  tabPanel(
    "Business Needs",
    shiny::dataTableOutput("business_needs_table")),

  tabPanel(
    "Requirements",
    shiny::dataTableOutput("requirements_table"))
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
    ii <- issue_list()
    if (is.null(ii)) return(NULL)
    make_issues(ii)
  })

  milestones <- reactive({
    mm <- milestone_list()
    if (is.null(mm)) return(NULL)
    make_milestones(mm)
  })

  labels <- reactive({
    ll <- labels_list()
    if (is.null(ll)) return(NULL)
    make_labels(ll)
  })

  #----------------------------------------------------------------------------#
  # Prepare secondary data
  #
  requirements <- reactive({
    ii <- issues()
    if (is.null(ii)) return(NULL)
    make_requirements(ii)
  })

  business_needs <- reactive({
    mm <- milestones()
    if (is.null(mm)) return(NULL)
    make_business_needs(mm)
  })

  relations <- reactive({
    ii <- issues()
    if (is.null(ii)) return(NULL)
    make_relations(ii)
  })

  #----------------------------------------------------------------------------#
  # Build dropdowns
  #
  output$issue_selector <- renderUI({
    ii <- issues()
    if (is.null(ii)) return(NULL)
    selectizeInput(
      "selected_issue",
      "Show details for requirement",
      setNames(rownames(ii), ii$title),
      width = '100%')
  })

  output$category_selector <- renderUI({
    ll <- labels()
    if (is.null(ll)) return(NULL)
    vals <- ll %>%
      dplyr::filter(grepl("Req", name)) %>%
      select(name) %>%
      extract2(1)
    selectizeInput(
      "selected_categories",
      "Categories",
      vals,
      width = '50%',
      selected = vals,
      multiple = TRUE)
  })

  output$priority_selector <- renderUI({
    ll <- labels()
    if (is.null(ll)) return(NULL)
    vals <- ll %>%
      dplyr::filter(grepl("have", name)) %>%
      select(name) %>%
      extract2(1)
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
  output$business_needs_table <- shiny::renderDataTable(
    business_needs(),
    options = list(filter = 'top'),
    escape = FALSE
  )

  output$requirements_table <- shiny::renderDataTable(
    requirements(),
    options = list(filter = 'top'),
    escape = FALSE
  )

  output$issue_detail <- renderUI({
    ii <- issues()
    sel <- input$selected_issue
    if (is.null(ii) || is.null(sel)) return(NULL)
    HTML(paste(
      ii$title[[as.integer(sel)]],
      ii$html_url[[as.integer(sel)]],
      ii$body[[as.integer(sel)]]
    ))
  })

  # TODO there's a mix up in relations between row numbers and issue ID.
  # the network won't render as relations target and source are issue IDs not
  # issue row numbers.
  output$force <- renderForceNetwork({
    forceNetwork(
      Links = relations(),
      Nodes =  data.frame(issues()),
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
      dd <- data.frame(lapply(d, as.character), stringsAsFactors = FALSE)
      write.csv(dd, file, sep = ",", row.names = FALSE, fileEncoding = "utf-8")
    }
  )

  output$download_requirements <- renderUI({
    d <- requirements()
    if (is.null(d)) {return(NULL)}
    downloadButton('downloadData', label = "Download all requirements")
  })

}

shinyApp(ui = ui, server = server)
