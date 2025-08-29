library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(DT)
library(readxl)

ui <- fluidPage(
  titlePanel(
    div(
      h2("R Shiny Assignment"),
      tags$small("This app enables users to create and visualize a cumulative paid claims triangle. You can enter data manually or upload an Excel file.")
    )
  ),
  sidebarLayout(
    sidebarPanel(
      radioButtons("input_mode", "Select Input Method",
                   choices = c("Manual Entry" = "manual", "Upload Excel" = "upload"),
                   selected = "manual"),
      conditionalPanel(
        condition = "input.input_mode == 'upload'",
        fileInput("file_upload", "Upload Excel File (.xlsx)", accept = ".xlsx")
      ),
      conditionalPanel(
        condition = "input.input_mode == 'manual'",
        numericInput("loss_year", "Loss Year", value = 2017),
        numericInput("dev_year", "Development Year", value = 1, min = 1),
        numericInput("claims_paid", "Claims Paid", value = 0),
        actionButton("add_row", "Add Row")
      ),
      numericInput("tail_factor", "Tail Factor", value = 1.1, step = 0.01),
      actionButton("build_triangle", "Build Triangle"),
      br(), br(),
      uiOutput("edit_ui")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Input",
                 h4("Input Data"),
                 DTOutput("input_table")
        ),
        tabPanel("Output",
                 h4("Cumulative Paid Claims ($) Triangle"),
                 tableOutput("triangle_output"),
                 h4("Cumulative Paid Claims ($) Plot"),
                 plotOutput("claims_plot")
        )
      )
    )
  )
)





server <- function(input, output, session) {
  data_store <- reactiveValues(data = data.frame(
    LossYear = numeric(0),
    DevYear = numeric(0),
    ClaimsPaid = numeric(0),
    stringsAsFactors = FALSE
  ))
  
  editing_row <- reactiveVal(NULL)
  
  output$edit_ui <- renderUI({
    idx <- editing_row()
    if (is.null(idx)) return(NULL)
    current_data <- data_store$data[idx, ]
    tagList(
      numericInput("edit_loss_year", "Loss Year", value = current_data$LossYear),
      numericInput("edit_dev_year", "Development Year", value = current_data$DevYear, min = 1),
      numericInput("edit_claims_paid", "Claims Paid", value = current_data$ClaimsPaid),
      actionButton("save_edit", "Save"),
      actionButton("cancel_edit", "Cancel")
    )
  })
  
  observeEvent(input$add_row, {
    req(input$input_mode == "manual")
    req(input$loss_year, input$dev_year, input$claims_paid)
    
    new_row <- data.frame(
      LossYear = input$loss_year,
      DevYear = input$dev_year,
      ClaimsPaid = input$claims_paid,
      stringsAsFactors = FALSE
    )
    data_store$data <- rbind(data_store$data, new_row)
  })
  
  observeEvent(input$save_edit, {
    idx <- editing_row()
    req(idx)
    data_store$data[idx, ] <- data.frame(
      LossYear = input$edit_loss_year,
      DevYear = input$edit_dev_year,
      ClaimsPaid = input$edit_claims_paid,
      stringsAsFactors = FALSE
    )
    editing_row(NULL)
  })
  
  observeEvent(input$cancel_edit, {
    editing_row(NULL)
  })
  
  observeEvent(input$file_upload, {
    req(input$file_upload)
    
    tryCatch({
      uploaded_data <- read_excel(input$file_upload$datapath)
      
      required_cols <- c("Loss Year", "Development Year", "Claims Paid")
      if (!all(required_cols %in% colnames(uploaded_data))) {
        showModal(modalDialog(
          title = "Invalid File",
          "The uploaded file must contain columns: Loss Year, Development Year, Claims Paid",
          easyClose = TRUE
        ))
        return()
      }
      
      uploaded_data <- as.data.frame(uploaded_data) %>%
        rename(
          LossYear = `Loss Year`,
          DevYear = `Development Year`,
          ClaimsPaid = `Claims Paid`
        ) %>%
        mutate(
          LossYear = as.numeric(LossYear),
          DevYear = as.numeric(DevYear),
          ClaimsPaid = as.numeric(ClaimsPaid)
        )
      
      data_store$data <- uploaded_data
      editing_row(NULL)
    }, error = function(e) {
      showModal(modalDialog(
        title = "Error Reading File",
        paste("Error:", e$message),
        easyClose = TRUE
      ))
    })
  })
  
  
  observeEvent(input$input_table_cell_clicked, {
    info <- input$input_table_cell_clicked
    if (is.null(info$value)) return()
    row <- info$row
    col <- info$col
    
    if (col == 3 && info$value == "Edit") {
      editing_row(row)
    } else if (col == 4 && info$value == "Delete") {
      data_store$data <- data_store$data[-row, , drop = FALSE]
      if (!is.null(editing_row()) && editing_row() == row) {
        editing_row(NULL)
      }
    }
  })
  
  output$input_table <- renderDT({
    df <- data_store$data
    if (nrow(df) == 0) return(NULL)
    df_display <- df %>%
      mutate(
        ClaimsPaid = comma(ClaimsPaid),
        Edit = "Edit",
        Delete = "Delete"
      )
    datatable(df_display, escape = FALSE, selection = "none", rownames = FALSE,
              options = list(
                dom = 't',
                paging = FALSE,
                ordering = FALSE,
                columnDefs = list(
                  list(targets = c(3, 4), searchable = FALSE, orderable = FALSE,
                       className = 'dt-center')
                )
              ))
  })
  
  processed_triangle <- reactive({
    req(input$build_triangle)
    isolate({
      df <- data_store$data
      if (nrow(df) == 0) return(NULL)
      
      cum_df <- df %>%
        group_by(LossYear) %>%
        arrange(DevYear) %>%
        mutate(Cumulative = cumsum(ClaimsPaid)) %>%
        ungroup()
      
      triangle <- cum_df %>%
        select(LossYear, DevYear, Cumulative) %>%
        pivot_wider(names_from = DevYear, values_from = Cumulative) %>%
        arrange(LossYear)
      
      dev_years <- sort(unique(cum_df$DevYear))
      max_dev <- max(dev_years)
      dev_years <- c(dev_years, max_dev + 1)
      
      for (dev in dev_years) {
        dev_str <- as.character(dev)
        if (!(dev_str %in% colnames(triangle))) {
          triangle[[dev_str]] <- NA
        }
      }
      
      triangle <- triangle %>% select(LossYear, all_of(as.character(dev_years)))
      
      for (dev in dev_years[-1]) {
        prev <- dev - 1
        dev_str <- as.character(dev)
        prev_str <- as.character(prev)
        
        valid_rows <- triangle %>%
          filter(!is.na(.data[[dev_str]]) & !is.na(.data[[prev_str]]))
        
        if (nrow(valid_rows) >= 1) {
          dev_factor <- sum(valid_rows[[dev_str]]) / sum(valid_rows[[prev_str]])
          
          for (i in 1:nrow(triangle)) {
            if (is.na(triangle[[dev_str]][i]) && !is.na(triangle[[prev_str]][i])) {
              triangle[[dev_str]][i] <- triangle[[prev_str]][i] * dev_factor
            }
          }
        }
      }
      
      for (i in 1:nrow(triangle)) {
        if (is.na(triangle[[as.character(max_dev + 1)]][i]) &&
            !is.na(triangle[[as.character(max_dev)]][i])) {
          triangle[[as.character(max_dev + 1)]][i] <-
            triangle[[as.character(max_dev)]][i] * input$tail_factor
        }
      }
      
      triangle
    })
  })
  
  output$triangle_output <- renderTable({
    triangle <- processed_triangle()
    if (is.null(triangle)) return(NULL)
    
    renamed_cols <- c("Loss Year", paste0("Dev Year ", colnames(triangle)[-1]))
    colnames(triangle) <- renamed_cols
    
    triangle %>%
      mutate(
        `Loss Year` = as.integer(`Loss Year`),
        across(-`Loss Year`, ~ ifelse(is.na(.), NA, comma(round(., 0))))
      )
  })
  
  output$claims_plot <- renderPlot({
    triangle <- processed_triangle()
    req(triangle)
    
    long_triangle <- triangle %>%
      pivot_longer(cols = -LossYear, names_to = "DevYear", values_to = "Cumulative") %>%
      mutate(DevYear = as.integer(DevYear))
    
    ggplot(long_triangle, aes(x = DevYear, y = Cumulative, color = factor(LossYear))) +
      geom_line(size = 1.2) +
      geom_point(size = 3) +
      geom_text(aes(label = comma(round(Cumulative, 0))), vjust = -1, size = 3.5) +
      labs(
        title = "Cumulative Paid Claims ($)",
        x = "Development Year",
        y = "Cumulative Paid Claims($)",
        color = "Loss Year"
      ) +
      theme_minimal(base_size = 14)
  })
  
  session$onSessionEnded(stopApp)
}

shinyApp(ui, server)