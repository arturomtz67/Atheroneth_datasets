# ---- app.R ----
# App to explore by dataset visit, section, and variable.


library(shiny)
library(tidyverse)
library(DT)

DATA_LONG_PATH <- "input/dataset_long.rds"


# Order for sections in side panel
SECTION_ORDER <- c(
  "General",
  "Cardiovascular risk factors, athropometrics, glucose and lipid",
  "Lifestyle",
  "SES",
  "Inflammation biomarkers",
  "(Gen)Omics & Epigenomics data",
  "Imaging modalities / Surrogate cardiovascular endpoints",
  "Diseases (questionnaire/diagnosis/medication)",
  "Other variables"
)


# Logos of each data set
DATASET_LOGOS <- c(
  "Helius" = "helius.jpg",
  "Lifelines" = "lifelines.jpg",
  "NEO" = "neo.jpg",
  "PROSPER" = "prosper.jpg",
  "Rotterdam Study" = "rotterdam.jpg",
  "UK Biobank" = "ukbiobank.jpg"
)



ui <- fluidPage(
  titlePanel("Explore AtheroNeth datasets"),
  
  sidebarLayout(
    sidebarPanel(
      # Datasets / Visits / Sections as checkboxes
      uiOutput("dataset_ui"),
      checkboxInput("datasets_none", "Clear", FALSE),
      
      uiOutput("visit_ui"),
      checkboxInput("visits_none", "Clear", FALSE),
      
      uiOutput("section_ui"),
      checkboxInput("sections_none", "Clear", FALSE),
      
      tags$hr(),
      
      # One selector for variables (main + sub)
      uiOutput("var_ui"),
      tags$hr(),
      
      radioButtons(
        "view_mode",
        "Table view",
        c("Wide (visits as columns)" = "wide",
          "Long (one row per visit)" = "long"),
        inline = TRUE
      ),
      
      numericInput("page_len", "Rows per page", 25, 5, 200, 5)
    ),
    
    mainPanel(
      DTOutput("tbl")
    )
  )
)

server <- function(input, output, session) {
  
  # ---- Load cleaned data ----
  dat_long <- reactive({
    validate(need(file.exists(DATA_LONG_PATH),
                  paste0("Missing file: ", DATA_LONG_PATH)))
    
    readRDS(DATA_LONG_PATH) %>%
      mutate(across(everything(), as.character))
  })
  
  
  
  # ---- Function to create checkbox with logo ----
  create_checkbox_with_logo <- function(dataset_name, logo_file) {
    logo_path <- file.path("www", logo_file)
    
    # Check if logo file exists
    if (file.exists(logo_path)) {
      tags$div(
        style = "display: flex; align-items: center; margin-bottom: 8px;",
        tags$input(
          type = "checkbox",
          name = "datasets",
          value = dataset_name,
          id = paste0("datasets_", gsub("[^A-Za-z0-9]", "_", dataset_name)),
          checked = "checked",
          style = "margin-right: 8px;"
        ),
        tags$img(
          src = logo_file,
          height = "30px",
          style = "margin-right: 8px; vertical-align: middle;"
        ),
        tags$label(
          `for` = paste0("datasets_", gsub("[^A-Za-z0-9]", "_", dataset_name)),
          dataset_name,
          style = "margin: 0; cursor: pointer;"
        )
      )
    } else {
      # Fallback if logo doesn't exist
      tags$div(
        style = "margin-bottom: 8px;",
        tags$input(
          type = "checkbox",
          name = "datasets",
          value = dataset_name,
          id = paste0("datasets_", gsub("[^A-Za-z0-9]", "_", dataset_name)),
          checked = "checked"
        ),
        tags$label(
          `for` = paste0("datasets_", gsub("[^A-Za-z0-9]", "_", dataset_name)),
          dataset_name,
          style = "margin-left: 5px;"
        )
      )
    }
  }
  
  
  
  # ---- Sidebar inputs ----
  observeEvent(dat_long(), {
    d <- dat_long()
    
    # Datasets
    output$dataset_ui <- renderUI({
      datasets <- d %>% distinct(Dataset) %>% arrange(Dataset) %>% pull()
      
      tags$div(
        tags$strong("Datasets"),
        tags$div(
          style = "margin-top: 10px;",
          map(datasets, ~{
            logo <- DATASET_LOGOS[.x]
            if (is.na(logo)) logo <- "default_logo.png"
            create_checkbox_with_logo(.x, logo)
          })
        )
      )
    })
  
    
    # Visits
    output$visit_ui <- renderUI({
      choices <- d %>% distinct(Visit) %>% arrange(Visit) %>% pull()
      checkboxGroupInput("visits", "Visits",
                         choices = choices,
                         selected = choices)
    })
    
    # Sections
    output$section_ui <- renderUI({
      present <- d %>%
        filter(!is.na(Section)) %>%
        distinct(Section) %>%
        arrange(Section) %>%
        pull()
      
      # keep only sections that are present
      choices <- SECTION_ORDER[SECTION_ORDER %in% present]
      
      checkboxGroupInput("sections", "Sections",
                         choices = choices,
                         selected = choices)
    })
    
    
    # Variables 
    output$var_ui <- renderUI({
      var_choices <- c(d$Main_variable, d$Sub_variable) %>%
        discard(is.na) %>%
        unique() %>%
        sort()
      
      selectizeInput(
        "variables",
        "Variables (choose a specific variable)",
        choices = var_choices,
        selected = NULL,
        multiple = TRUE,
        options = list(plugins = list("remove_button"))
      )
    })
  })
  
  
  # ---- Clear checkboxes (deselect all)----
  observeEvent(input$datasets_none, {
    if (isTRUE(input$datasets_none)) {
      updateCheckboxGroupInput(session, "datasets", selected = character(0))
      updateCheckboxInput(session, "datasets_none", value = FALSE)
    }
  })
  
  observeEvent(input$visits_none, {
    if (isTRUE(input$visits_none)) {
      updateCheckboxGroupInput(session, "visits", selected = character(0))
      updateCheckboxInput(session, "visits_none", value = FALSE)
    }
  })
  
  observeEvent(input$sections_none, {
    if (isTRUE(input$sections_none)) {
      updateCheckboxGroupInput(session, "sections", selected = character(0))
      updateCheckboxInput(session, "sections_none", value = FALSE)
    }
  })
  
  
  
  # ---- Filtering logic ----
  filtered <- reactive({
    d <- dat_long()
    
    if (!is.null(input$datasets))
      d <- d %>% filter(Dataset %in% input$datasets)
    if (!is.null(input$visits))
      d <- d %>% filter(Visit %in% input$visits)
    if (!is.null(input$sections))
      d <- d %>% filter(Section %in% input$sections)
    
    # filter by variables (match either main or sub)
    if (!is.null(input$variables) && length(input$variables) > 0) {
      d <- d %>% filter(
        Main_variable %in% input$variables |
          Sub_variable %in% input$variables
      )
    }
    
    d
  })
  
  # ---- Display table ----
  output$tbl <- renderDT({
    d <- req(filtered())
    validate(need(nrow(d) > 0, "No rows match your filters."))
    
    show <- if (input$view_mode == "wide") {
      d %>%
        group_by(Dataset, Section, Main_variable, Sub_variable, Visit) %>%
        summarise(Value = str_c(unique(Value), collapse = " | "),
                  .groups = "drop") %>%
        pivot_wider(names_from = Visit,
                    values_from = Value,
                    values_fill = "") %>%
        arrange(Dataset, Section, Main_variable, Sub_variable)
    } else {
      d %>%
        arrange(Dataset, Section, Main_variable, Sub_variable, Visit) %>%
        select(Dataset, Section, Main_variable, Sub_variable, Visit, Value)
    }
    
    
    # Replace Dataset text with logo HTML
    show <- show %>%
      mutate(
        Dataset = map_chr(Dataset, ~{
          logo <- DATASET_LOGOS[.x]
          if (is.na(logo)) {
            # If logo not found, return text
            return(.x)
          } else {
            # Return HTML for logo image
            sprintf('<img src="%s" height="30px" title="%s" alt="%s"/>', 
                    logo, .x, .x)
          }
        })
      )
    
    
    
    datatable(
      show,
      escape = FALSE, # Allow HTML in cells for the logos
      filter = "top",
      options = list(
        pageLength = input$page_len,
        scrollX = TRUE,
        lengthMenu = c(10, 25, 50, 100, 200)
      )
    )
  })
}


shinyApp(ui, server)