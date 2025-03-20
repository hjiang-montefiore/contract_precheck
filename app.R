library(shiny)
library(shinythemes)
library(openxlsx)
library(tidyverse)
library(readxl)
library(DT)
library(dplyr)

# Load UOM reference
uom_ref <- read_csv('UOM.csv')

# Function to check for duplicated values
check_duplicates <- function(df, col) {
  duplicated(df[[col]]) | duplicated(df[[col]], fromLast = TRUE)
}

ui <- fluidPage(
  theme = shinytheme("cosmo"),
  titlePanel("Contract Pre-Check App"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload Excel File", accept = c(".xlsx")),
      uiOutput("sheet_selector"),
      actionButton("run", "Run Analysis", class = "btn-primary"),
      uiOutput("download_ui")  # Dynamic download button
    ),
    
    mainPanel(
      uiOutput("tabset_ui")  # Dynamic tabset panel
    )
  )
)

server <- function(input, output) {
  raw_data <- reactiveVal(NULL)
  problems <- reactiveVal(NULL)
  show_problem_tab <- reactiveVal(FALSE)  # Track if problem tab should be shown
  
  # Sheet selector UI
  output$sheet_selector <- renderUI({
    req(input$file)
    sheets <- excel_sheets(input$file$datapath)
    selectInput("sheet", "Select Sheet", choices = sheets)
  })
  
  # Main analysis
  observeEvent(input$run, {
    req(input$file, input$sheet)
    
    # Reset previous results
    problems(NULL)
    show_problem_tab(FALSE)
    
    # Read data
    df <- tryCatch(
      read_xlsx(input$file$datapath, sheet = input$sheet),
      error = function(e) NULL
    )
    
    # Check required columns
    required_cols <- c(
      "Mfg Part Num", "Vendor Part Num", "UOM", "QOE",
      "Description", "Effective Date", "Expiration Date", "Contract Price"
    )
    
    missing_cols <- setdiff(required_cols, colnames(df))
    
    if (length(missing_cols) > 0) {
      output$summary_ui <- renderUI({
        div(
          h2("😢", style = "font-size: 50px;"),
          h3("Please check your column names"),
          p("Missing columns:", paste(missing_cols, collapse = ", "))
        )
      })
      return()
    }
    
    # Convert columns
    df <- df %>%
      mutate(
        across(c(`Mfg Part Num`, `Vendor Part Num`, `UOM`, `Description`), as.character),
        QOE = as.numeric(gsub(",", "", QOE)),
        `Contract Price` = as.numeric(gsub("[$,]", "", as.character(`Contract Price`)))
      )
    
    # Duplication checks
    df$`Reduced Mfg Part Num` <- gsub("[^[:alnum:]]", "", df$`Mfg Part Num`)
    
    dup_check_mfg <- any(check_duplicates(df, "Mfg Part Num"))
    dup_check_reduced <- any(!check_duplicates(df, "Mfg Part Num") & check_duplicates(df, "Reduced Mfg Part Num"))
    
    # UOM checks
    uom_invalid <- df %>% filter(!UOM %in% uom_ref$`see UOM`) %>% mutate(Comment = "Unknown UOM")
    
    uom_ea_problems <- df %>%
      filter(UOM == "EA" & QOE != 1) %>%
      mutate(Comment = "EA UOM must have QOE = 1")
    
    uom_recommend <- df %>%
      inner_join(uom_ref, by = c('UOM' = 'see UOM')) %>%
      filter(UOM != `use UOM`) %>%
      mutate(Comment = paste0("Please use the standard UOM as ", `use UOM`)) %>%
      select(-`use UOM`)
    
    # Duplication records
    dup_check_mfg_df <- df[check_duplicates(df, "Mfg Part Num"), ] %>%
      mutate(Comment = 'Mfg Part Num Duplication')
    
    dup_check_reduced_df <- df[!check_duplicates(df, "Mfg Part Num") & check_duplicates(df, "Reduced Mfg Part Num"), ] %>%
      mutate(Comment = 'Reduced Mfg Part Num Duplication')
    
    # Collect all problems
    all_problems <- bind_rows(
      dup_check_mfg_df,
      dup_check_reduced_df,
      uom_invalid,
      uom_ea_problems,
      uom_recommend
    ) %>% relocate(Comment, .before = 1)
    
    problems(all_problems)
    raw_data(df)
    
    # Show "Problematic Data" tab only if issues exist
    if (nrow(all_problems) > 0) {
      show_problem_tab(TRUE)
    } else {
      show_problem_tab(FALSE)
    }
    
    # Create summary UI
    output$summary_ui <- renderUI({
      div(
        h3("Column Check: 😊 All required columns present"),
        h3("Duplication Check:"),
        p("Mfg Part Num:", if (dup_check_mfg) "😢 Duplicates found" else "😊 No duplicates"),
        p("Reduced Mfg Part Num:", if (dup_check_reduced) "😢 Duplicates found" else "😊 No duplicates"),
        h3("UOM Check:"),
        p(if (nrow(uom_invalid) + nrow(uom_ea_problems) + nrow(uom_recommend) == 0) '😊 No UOM issues at all' else ''),
        p(if (nrow(uom_invalid) > 0) "😢 Unknown UOMs found" else ""),
        p(if (nrow(uom_ea_problems) > 0) "😢 EA UOM with QOE ≠ 1 found" else ""),
        p(if (nrow(uom_recommend) > 0) "😢 Not Standard UOM found" else "")
      )
    })
    
    # Action message
    output$action_ui <- renderUI({
      if (nrow(all_problems) > 0) {
        div(
          h2("😢", style = "font-size: 50px;"),
          h3("Please download the issue file, fix, and try again")
        )
      } else {
        div(
          h2("😊", style = "font-size: 50px;"),
          h3("Congratulations! The uploaded file passed the test and is ready for the next step.")
        )
      }
    })
    
    # Show download button only if there are problems
    output$download_ui <- renderUI({
      if (nrow(all_problems) > 0) {
        downloadButton("download_problems", "Download Issues", class = "btn-success")
      }
    })
  })
  
  # Problem table
  output$problem_table <- renderDT({
    req(problems())
    datatable(problems() %>% select(-`Reduced Mfg Part Num`), options = list(scrollX = TRUE))
  })
  
  # Download handler
  output$download_problems <- downloadHandler(
    filename = function() {
      timestamp <- format(Sys.time(), "%Y_%m_%d_%H%M%S")
      paste0("problematic_file_", timestamp, ".xlsx")
    },
    content = function(file) {
      write.xlsx(problems(), file)
    }
  )
  
  # Dynamic tabset panel
  output$tabset_ui <- renderUI({
    if (show_problem_tab()) {
      tabsetPanel(
        tabPanel("Summary", uiOutput("summary_ui"), uiOutput("action_ui")),
        tabPanel("Problematic Data", DTOutput("problem_table"))
      )
    } else {
      tabsetPanel(
        tabPanel("Summary", uiOutput("summary_ui"), uiOutput("action_ui"))
      )
    }
  })
}

shinyApp(ui, server)
