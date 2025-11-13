# ---- app.R ----
# App to explore multi-sheet Excel (cleaned) by dataset, visit, section, and variable.
# Expects: input/dataset_long.rds  (columns: Dataset, Section, Variable, Visit, Value)

library(shiny)
library(tidyverse)
library(DT)

DATA_LONG_PATH <- "input/dataset_long.rds"   # created by your cleaning script

ui <- fluidPage(
  titlePanel("Datasets × Visits × Variables — Explorer"),
  sidebarLayout(
    sidebarPanel(
      uiOutput("data_status"),
      uiOutput("dataset_ui"),
      uiOutput("visit_ui"),
      uiOutput("section_ui"),
      uiOutput("variable_ui"),
      textInput("search_value", "Search inside values (optional)", value = ""),
      radioButtons("view_mode", "Table view",
                   c("Long (one row per visit)" = "long",
                     "Wide (visits as columns)" = "wide"),
                   selected = "wide"),
      numericInput("page_len", "Rows per page", value = 25, min = 5, max = 200, step = 5)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Browse", DTOutput("tbl")),
        tabPanel("Variable view",
                 uiOutput("var_note"),
                 DTOutput("tbl_var_matrix"))
      )
    )
  )
)

server <- function(input, output, session){
  
  # ---- Load cleaned data ----
  dat_long <- reactive({
    validate(need(file.exists(DATA_LONG_PATH),
                  paste0("File not found: ", DATA_LONG_PATH,
                         ". Run your cleaning script to create it.")))
    readRDS(DATA_LONG_PATH) %>%
      mutate(across(c(Dataset, Section, Variable, Visit, Value), as.character))
  })
  
  output$data_status <- renderUI({
    req(dat_long())
    d <- dat_long()
    HTML(paste0(
      "<b>Loaded:</b> ", DATA_LONG_PATH, "<br/>",
      "Datasets: ", d %>% distinct(Dataset) %>% nrow(), " | ",
      "Visits: ",   d %>% distinct(Visit) %>% nrow(), " | ",
      "Variables: ", d %>% distinct(Variable) %>% nrow()
    ))
  })
  
  # ---- Dynamic filters ----
  observeEvent(dat_long(), {
    d <- dat_long()
    output$dataset_ui <- renderUI({
      choices <- d %>% distinct(Dataset) %>% arrange(Dataset) %>% pull()
      selectizeInput("datasets", "Datasets", choices = choices,
                     selected = choices, multiple = TRUE,
                     options = list(plugins = list("remove_button")))
    })
    output$visit_ui <- renderUI({
      choices <- d %>% distinct(Visit) %>% arrange(Visit) %>% pull()
      selectizeInput("visits", "Visits", choices = choices,
                     selected = choices, multiple = TRUE,
                     options = list(plugins = list("remove_button")))
    })
    output$section_ui <- renderUI({
      choices <- d %>% distinct(Section) %>% arrange(Section) %>% pull()
      selectizeInput("sections", "Sections", choices = choices,
                     selected = choices, multiple = TRUE,
                     options = list(plugins = list("remove_button")))
    })
    output$variable_ui <- renderUI({
      choices <- d %>% distinct(Variable) %>% arrange(Variable) %>% pull()
      selectizeInput("variables", "Variables (optional)",
                     choices = choices, selected = NULL, multiple = TRUE,
                     options = list(plugins = list("remove_button")),
                     placeholder = "Pick one or more, or leave empty for all")
    })
  }, ignoreInit = TRUE)
  
  # ---- Apply filters ----
  filtered <- reactive({
    d <- dat_long()
    req(nrow(d) > 0)
    if (!is.null(input$datasets) && length(input$datasets))
      d <- d %>% filter(Dataset %in% input$datasets)
    if (!is.null(input$visits) && length(input$visits))
      d <- d %>% filter(Visit %in% input$visits)
    if (!is.null(input$sections) && length(input$sections))
      d <- d %>% filter(Section %in% input$sections)
    if (!is.null(input$variables) && length(input$variables))
      d <- d %>% filter(Variable %in% input$variables)
    if (nzchar(input$search_value))
      d <- d %>% filter(str_detect(Value, regex(input$search_value, ignore_case = TRUE)))
    d
  })
  
  # ---- Browse table (long / wide) ----
  output$tbl <- renderDT({
    d <- filtered()
    validate(need(nrow(d) > 0, "No rows match your filters."))
    
    if (input$view_mode == "long") {
      show <- d %>% arrange(Dataset, Section, Variable, Visit) %>%
        select(Dataset, Section, Variable, Visit, Value)
    } else {
      show <- d %>%
        group_by(Dataset, Section, Variable, Visit) %>%
        summarise(Value = str_c(unique(Value), collapse = " | "), .groups = "drop") %>%
        pivot_wider(names_from = Visit, values_from = Value, values_fill = "") %>%
        arrange(Dataset, Section, Variable)
    }
    
    datatable(
      show,
      filter = "top",
      options = list(
        pageLength = input$page_len,
        scrollX = TRUE,
        lengthMenu = c(10, 25, 50, 100, 200)
      )
    )
  })
  
  # ---- Variable view: matrix Dataset × Visit for one variable (or a few) ----
  output$var_note <- renderUI({
    req(dat_long())
    HTML("Pick a variable in the left panel (Variables) to see it as a matrix by Dataset × Visit. 
         If you select multiple, they’ll be stacked.")
  })
  
  output$tbl_var_matrix <- renderDT({
    d <- filtered()
    req(nrow(d) > 0)
    validate(need(!is.null(input$variables) && length(input$variables) > 0,
                  "Select at least one Variable in the sidebar."))
    
    mat <- d %>%
      filter(Variable %in% input$variables) %>%
      group_by(Variable, Dataset, Visit) %>%
      summarise(Value = str_c(unique(Value), collapse = " | "), .groups = "drop") %>%
      arrange(Variable, Dataset, Visit) %>%
      pivot_wider(names_from = Visit, values_from = Value, values_fill = "") %>%
      arrange(Variable, Dataset)
    
    datatable(
      mat,
      rownames = FALSE,
      options = list(pageLength = input$page_len, scrollX = TRUE)
    )
  })
}

shinyApp(ui, server)
