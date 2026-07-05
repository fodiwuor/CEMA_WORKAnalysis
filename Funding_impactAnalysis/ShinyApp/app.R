library(shiny)
library(shinydashboard)
library(DT)
library(readxl)
library(dplyr)
library(stringr)
library(httr)
library(jsonlite)

github_api_url <- "https://api.github.com/repos/fodiwuor/CEMA_WORKAnalysis/contents/Funding_impactAnalysis/data?ref=main"

table_name_map <- c(
  "subcounties_usedHivsyphilsdual" = "Table S1: Sub-counties used in the analysis of HIV/syphilis dual kits",
  "used_subcountiesHIVselfkit" = "Table S2: Sub-counties used in the analysis of HIV self test kits",
  "HIV_Self_Test_Kits_Subcounty" = "Table S3: Sub-county-level stockout summary for HIV Self test kits",
  "HIV_Syphilsdualkit_Subcounty" = "Table S4: Sub-county-level stockout summary for HIV/Syphilis dual kits",
  "Sub_counties_mixed_tableHIVSyphilsdualkit" = "Table S5: Effect of 2025 external funding cut on HIV/Syphilis dual kits by Sub-county"
)

order_tables_by_number <- function(x) {
  
  tbl_no <- stringr::str_extract(
    names(x),
    "Table\\s*S?\\d+"
  )
  
  tbl_no <- as.numeric(
    stringr::str_extract(tbl_no, "\\d+")
  )
  
  x[order(tbl_no, na.last = TRUE)]
}

special_stockout_tables <- c(
  "Table S3: Sub-county-level stockout summary for HIV Self test kits",
  "Table S4: Sub-county-level stockout summary for HIV/Syphilis dual kits"
)

#special_effect_table <- "Table S5: Effect of 2025 external funding cut on HIV/Syphilis dual kits by Sub-county"
special_effect_table <- "Table S5: Effect of 2025 external funding cut on HIV/Syphilis dual kits by Sub-county"

load_excel_tables_from_github <- function() {
  res <- GET(github_api_url)
  stop_for_status(res)
  
  files_info <- fromJSON(content(res, as = "text", encoding = "UTF-8"))
  
  excel_files <- files_info |>
    filter(type == "file", str_detect(name, "\\.xlsx$"))
  
  tables <- list()
  
  for (i in seq_len(nrow(excel_files))) {
    file_name <- excel_files$name[i]
    raw_url <- excel_files$download_url[i]
    
    temp_file <- tempfile(fileext = ".xlsx")
    download.file(raw_url, temp_file, mode = "wb", quiet = TRUE)
    
    table_key <- tools::file_path_sans_ext(file_name)
    
    display_name <- ifelse(
      table_key %in% names(table_name_map),
      table_name_map[[table_key]],
      table_key
    )
    
    df <- read_excel(temp_file) |> as.data.frame()

 if (display_name == special_effect_table) {
  
  names(df) <- stringr::str_replace_all(
    names(df),
    "Trend_Prepolicy\\s*\\(IRR\\)",
    "Pre-funding cut trend (IRR)"
  )
  
}
    
    
    if (display_name %in% special_stockout_tables) {
      names(df) <- c(
        "County",
        "Sub-county",
        "Pre-funding median (IQR)",
        "Post-funding median (IQR)",
        "Pre-funding median (IQR) ",
        "Post-funding median (IQR) "
      )
    }
    
    tables[[display_name]] <- df
  }
  
  #tables
  order_tables_by_number(tables)
}

ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(title = "Results Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Results Table", tabName = "tables", icon = icon("table"))
    ),
    
    tags$hr(),
    
    tags$div(
      style = "padding: 10px; color: white;",
      tags$b("Author:"),
      tags$p("Fredrick Orwa"),
      tags$b("Email:"),
      tags$p("orwafredrick95@gmail.com"),
      tags$b("Organization:"),
      tags$p("CEMA")
    ),
    
    tags$hr(),
    
    selectInput(
      inputId = "selected_table",
      label = "Select table to view",
      choices = NULL,
      selected = NULL
    ),
    
    uiOutput("dynamic_filters")
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .main-header .logo {
          font-weight: bold;
          font-size: 22px;
        }
        .box {
          border-top: 3px solid #3c8dbc;
        }
        .content-wrapper {
          background-color: #f4f6f9;
        }
        table.dataTable thead th {
          text-align: center !important;
          vertical-align: bottom !important;
          font-weight: bold;
        }
        table.dataTable tbody td {
          vertical-align: top !important;
        }
      "))
    ),
    
    tabItems(
      tabItem(
        tabName = "tables",
        fluidRow(
          box(
            width = 12,
            title = textOutput("table_title"),
            status = "primary",
            solidHeader = FALSE,
            uiOutput("table_message"),
            DTOutput("table_output")
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  all_tables <- reactive({
    load_excel_tables_from_github()
  })
  
  observe({
    tables <- all_tables()
    
    updateSelectInput(
      session,
      "selected_table",
      choices = names(tables),
      selected = names(tables)[1]
    )
  })
  
  selected_data <- reactive({
    req(input$selected_table)
    all_tables()[[input$selected_table]]
  })
  
  output$table_title <- renderText({
    req(input$selected_table)
    input$selected_table
  })
  
  output$table_message <- renderUI({
    if (is.null(input$selected_table) || input$selected_table == "") {
      tags$div(
        style = "padding: 25px; font-size: 18px; color: #666;",
        "Please select a table from the left panel to display results."
      )
    }
  })
  
  output$dynamic_filters <- renderUI({
    req(input$selected_table)
    
    df <- selected_data()
    
    if (input$selected_table %in% special_stockout_tables) {
      cols <- intersect(c("County", "Sub-county"), names(df))
    } else {
      cols <- names(df)
    }
    
    tagList(
      tags$hr(),
      tags$h4("Filter table", style = "color:white; padding-left:10px;"),
      
      lapply(cols, function(col) {
        values <- sort(unique(as.character(df[[col]])))
        values <- values[!is.na(values)]
        
        selectizeInput(
          inputId = paste0("filter_", make.names(col)),
          label = col,
          choices = c("All" = "", values),
          selected = "",
          multiple = FALSE,
          options = list(
            placeholder = paste("Search or select", col),
            allowEmptyOption = TRUE
          )
        )
      })
    )
  })
  
  filtered_data <- reactive({
    req(input$selected_table)
    
    df <- selected_data()
    
    if (input$selected_table %in% special_stockout_tables) {
      filter_cols <- intersect(c("County", "Sub-county"), names(df))
    } else {
      filter_cols <- names(df)
    }
    
    for (col in filter_cols) {
      filter_id <- paste0("filter_", make.names(col))
      
      if (!is.null(input[[filter_id]]) && input[[filter_id]] != "") {
        df <- df |>
          filter(str_detect(
            str_to_lower(as.character(.data[[col]])),
            fixed(str_to_lower(input[[filter_id]]))
          ))
      }
    }
    
    df
  })
  
  output$table_output <- renderDT({
    req(input$selected_table)
    
    df <- filtered_data()
    
    if (input$selected_table %in% special_stockout_tables) {
      datatable(
        df,
        extensions = c("Buttons", "Scroller"),
        container = htmltools::withTags(
          table(
            class = "display",
            thead(
              tr(
                th(rowspan = 2, "County"),
                th(rowspan = 2, "Sub-county"),
                th(colspan = 2, "Number of facilities with stockout"),
                th(colspan = 2, "Percent (%) stockout rate")
              ),
              tr(
                th("Pre-funding cut median (IQR)"),
                th("Post-funding cut median (IQR)"),
                th("Pre-funding cut median (IQR)"),
                th("Post-funding cut median (IQR)")
              )
            )
          )
        ),
        options = list(
          dom = "Bfrtip",
          buttons = c("copy", "csv", "excel"),
          pageLength = 25,
          scrollX = TRUE,
          scrollY = "550px",
          scroller = TRUE,
          autoWidth = TRUE
        ),
        rownames = FALSE,
        filter = "none"
      )
    } else {
      datatable(
        df,
        extensions = c("Buttons", "Scroller"),
        options = list(
          dom = "Bfrtip",
          buttons = c("copy", "csv", "excel"),
          pageLength = 25,
          scrollX = TRUE,
          scrollY = "550px",
          scroller = TRUE,
          autoWidth = TRUE
        ),
        rownames = TRUE,
        filter = "top"
      )
    }
  })
}

shinyApp(ui, server)
