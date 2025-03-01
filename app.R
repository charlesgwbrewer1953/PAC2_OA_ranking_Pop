##### Factor weighting and OA ranking - V4.3.0 Operational - GCS retrieval
#####
##### REVISED VERSION - 2025
#
#
#  OA RANKING
#
# GitHub https://github.com/charlesgwbrewer1953/PAC2_OA_ranking_Pop.git
# GitHub PAC_OA_raqnking_Pop
# oa_ranking1
# Becoming generalised
# oa_ranking2
# Sums calculated to zero dec plac 2.2.3
# Number of OAs added 2.3.5
# # Revised GCS storage
# DEV
#
# Modification to exclude OA population from
# LATEST 27-2-25
version_no <- "5.0.3"
op_status <- "Deployed"

library(shiny)
library(rhandsontable)
library(dplyr)
library(tidyr)
library(stringr)
library(DT)
library(readr)
library(googlesheets4)
library(googleCloudStorageR)


# Define UI
ui <- fluidPage(

    # Import Roboto style
    tags$head(
        includeCSS("www/styles.css")
    ),
    titlePanel("Factor Weighting and OA Ranking"),
    tags$div(
        style = "font-size: 12px; font-style: italic;",
        HTML("Version: "),
        textOutput("version_no_output")
    ),
    tags$div(
        style = "font-size: 12px; font-style: italic;",
        HTML("Status: "),
        textOutput("op_status_output")
    ),

    tags$div(
        style = "color: blue; font-size: 24;",
        " ",
        textOutput("constituencyName")
    ),
    sidebarLayout(
        sidebarPanel(
            width = "30%",
            selectInput("fileDropdown", "Choose a file:", choices = NULL), #

            actionButton("loadFile", "Load File"),
            br(),
            br(),
            rHandsontableOutput("parameter_table"),
            br(),
            selectInput("partyChoice", "Choose a party:", choices = c("CON", "LAB", "LIB", "REF", "NRF", "GRN"), selected = "REF"),
            br(),
            textInput("weightingModel", "Weighting model (max 8 chars):", "", width = "120px"),
            tags$script(HTML("
                $(document).on('input', '#alphanumericInput', function() {
                    var value = $(this).val();
                    var alphanumericRegex = /^[a-z0-9]*$/i;
                    if (!alphanumericRegex.test(value) || value.length > 8) {
                        $(this).val(value.substring(0, 8).replace(/[^a-z0-9]/gi, ''));
                    }
                });
            ")),
            br(),
            radioButtons("output_columns", label = "Output Columns",
                         choices = c("Sum only", "Non-zero only", "Show all"),
                         selected = "Sum only"),
            actionButton("calculate", "Calculate"),
            br(), # Add a blank line
            tags$style(HTML(".horizontal-line {border 3px, solid, #999999;}")),
            hr(class = "horizontal-line"),
            downloadButton("download_config", "Download configuration"),
            br(), # Add a blank line
            br(),
            downloadButton("download_result", "Download result")
        ),
    mainPanel(
            DT::dataTableOutput("oa_unique_values_table"),
            tableOutput("fileContent")

        )
    )
)

# Define server logic
server <- function(input, output, session) {

    output$version_no_output <- renderText({
        version_no
    })
    output$op_status_output <- renderText({
        op_status
    })


    ########################
    #
    # Define reactive values
    #
    #########################

    result_table_data <- reactiveVal(NULL)
    parameter_table_data <- reactiveVal(NULL)  # Reactive value for input table data
    oa_unique_values_data <- reactiveVal(NULL)
    oa_unique <- reactiveVal(NULL)
    dimensions <- reactiveVal(NULL)             # Reactive value for input table
    constituencyName <- reactiveVal(NULL)
    LADname <- reactiveVal(NULL)

    #############################################################
    #
    #  File name retrieval - GCS
    #  File selection - GCS
    #  File retrieval - GCS
    #  NOTE - File name is output from PAC...0, 1, 2 processes
    #         File name must contain "term candidate"
    #
    #############################################################


    gcs_auth("./data/astral-name-419808-ab8473ded5ad.json")
    gcs_global_bucket("oa_analysis1")
    # Set bucket name
    bucket_name <- "oa_analysis1"

    # Fetch file list from GCS
    observe({
        files <- gcs_list_objects(bucket_name)
        file_names <- files$name

        # Use str_detect to filter file names containing the string "candidate"
        file_names_selected <- file_names[str_detect(file_names, "candidate")]

        # Update the dropdown choices
        updateSelectInput(session, "fileDropdown", choices = file_names_selected)
    })

    # Reactive value to store the loaded data frame
   # oa_unique <- reactiveVal(NULL)

    # Load the selected file when the action button is clicked
    observeEvent(input$loadFile, {
        req(input$fileDropdown)  # Ensure a file is selected

        # Download the selected file from GCS as raw data
        object <- gcs_get_object(input$fileDropdown, bucket = bucket_name)

        object <- object[,-1]

        # Check if input has LAD columns
        has_LAD_columns <- all(c("LAD24CD", "LAD24NM") %in% names(object))

        # Check if LAD24CD and LAD24NM are in the loaded data
        if (!("LAD24CD" %in% names(object))) object$LAD24CD <- 0
        if (!("LAD24NM" %in% names(object))) object$LAD24NM <- 0

        # Arrange columns so LAD columns are after `constituencyName`
        if ("constituencyName" %in% names(object)) {
            object <- object %>% select(1:which(names(object) == "constituencyName"), LAD24CD, LAD24NM, everything())
        }


        dimensions_names <- names(object)
        constituencyName1 <- object[1,2]
        LAD_name <- object$LAD24NM[1]
        LADname(LAD_name)
        # Store the data frame in the reactive value
        oa_unique(object)
        dimensions(dimensions_names)
        constituencyName(constituencyName1)

    })

    # Retrieve and display the content of the selected file
    # Display the contents of the loaded data frame
    output$fileContent <- renderTable({
        req(oa_unique())  # Ensure the data frame is loaded
        oa_unique()
    })
    # Reactive value to store the selected file data
    selected_file_data <- reactiveVal(NULL)

    # Read the selected file into oa_unique
    observeEvent(input$file_input, {
        req(input$file_input) # Ensure a file is selected
        file_path <- input$file_input$datapath
        selected_file_data(read_csv(file_path))
    })

    # Use the selected file data in calculations
    observeEvent(input$calculate, {
        oa_unique <- selected_file_data()

    })

    #############################################################
    #
    #  Render input table
    #
    #############################################################


output$parameter_table <- renderRHandsontable({
        dimensions_val <- dimensions()

if (!is.null(dimensions_val)) {
    # Exclude "Population" from the dimensions list
    filtered_dimensions <- dimensions_val[!(dimensions_val %in% c("Population"))]

    # If dimensions_val is not NULL, create the dataframe
    df <- data.frame(
        Title = filtered_dimensions[-(1:7)],
        Weight = 0,
        Q1 = 0,
        Q2 = 0,
        Q3 = 0,
        Q4 = 0
    )
} else {
    # If dimensions_val is NULL, create an empty dataframe
    df <- data.frame(
        Title = character(0),
        Weight = numeric(0),
        Q1 = numeric(0),
        Q2 = numeric(0),
        Q3 = numeric(0),
        Q4 = numeric(0)
    )
}
        rhandsontable(df)
    })

    #############################################################
    #
    #  Calculate result table
    #
    #############################################################

    # Define a reactive expression to update oa_unique_values when the 'Calculate' button is clicked
    observeEvent(input$calculate, {

      input_df <- hot_to_r(input$parameter_table)  # Retrieve input table data
      parameter_table_data(input_df)  # Store input table data

      oa_unique_values <- oa_unique()  # Load data from reactive storage

      # Ensure "Population" is included in the final dataset but not changed
      population_column <- oa_unique_values$Population

      # Loop over columns apart from 1 to 7 (id columns) in oa_unique_values, excluding "Population"
      for (col_name in names(oa_unique_values)[-c(1:7)]) {
        if (col_name == "Population") next  # Skip Population column in calculations

        row_index <- match(col_name, input_df$Title)

        for (i in 1:nrow(oa_unique_values)) {
          cell_content <- oa_unique_values[[col_name]][i]

          if (cell_content %in% c("Q1", "Q2", "Q3", "Q4")) {
            input_value <- input_df[row_index, cell_content]

            if (!is.na(input_value) && !is.null(input_value)) {
              oa_unique_values[[col_name]][i] <- input_value
            }
          }
        }
      }


      # Convert table values to numeric, except for Population
      numeric_columns <- setdiff(8:ncol(oa_unique_values), which(names(oa_unique_values) == "Population"))
      oa_unique_values <- oa_unique_values %>%
        mutate_at(vars(numeric_columns), as.numeric)

      # Function to apply weighting but ignore Population
      calculate_weighted_sum <- function(col_values, weight) {
        weight_numeric <- as.numeric(weight)
        result <- round(col_values * weight_numeric, 0)
        return(result)
      }

        # Iterate over each column in oa_unique_values
      # Iterate over each column, ignoring Population
      for (col_name in names(oa_unique_values)[-c(1:7)]) {
        if (col_name == "Population") next  # Skip Population column

        weight_value <- input_df[input_df$Title == col_name, "Weight"]
        weight_numeric <- as.numeric(weight_value)
        col_values <- oa_unique_values[[col_name]]
        weighted_values <- calculate_weighted_sum(col_values, weight_numeric)
        oa_unique_values[[col_name]] <- weighted_values
      }
        # Add sum of row values
      # Restore Population column unchanged
      oa_unique_values$Population <- population_column
      population_mean <- mean(population_column)
      # Compute row-wise sum for ranking, excluding Population
      sum_columns <- setdiff(names(oa_unique_values), c(names(oa_unique_values)[1:7], "Population"))
      oa_unique_values$raw_Sum <- rowSums(oa_unique_values[, sum_columns], na.rm = TRUE)
      oa_unique_values$Sum <- round((oa_unique_values$raw_Sum * oa_unique_values$Population / population_mean),0)
      # Sort results by Sum
      oa_unique_values <- oa_unique_values %>% arrange(desc(Sum))
        # Assign the result to reactive value
        oa_unique_values_data(oa_unique_values)

        # Enable the Download button
        shinyjs::enable("download_config")
    })


    ####################################
    #
    # Render the oa_unique_values table
    #
    ####################################

    output$oa_unique_values_table <- DT::renderDataTable({
        dt<-  oa_unique_values_data()
        if(input$output_columns == "Show all"){dt <- dt}
        if(input$output_columns == "Sum only"){dt <- dt[, c(1:7, ncol(dt))]}
        if(input$output_columns == "Non-zero only"){

            numeric_columns <- sapply(dt, is.numeric)
            first_row <- dt[1,numeric_columns]
            non_zero_non_na_columns <- which(!(is.na(first_row) | first_row == 0))
            # Adjust by skipping first 7 columns, then ensure the selected columns are within bounds
            non_zero_non_na_columns <- non_zero_non_na_columns + 7
            non_zero_non_na_columns <- non_zero_non_na_columns[non_zero_non_na_columns <= ncol(dt)]

            # Select the relevant columns from dt
            dt <- dt[, c(1:7, non_zero_non_na_columns)]

        }
        dt
    },  options = list(ordering = TRUE,
                      filter = 'top',
                      search = list(regex = TRUE, caseInsensitive = TRUE),
                      lengthMenu = list(c(-1))
                      ))
    ###########

     # Download handler for result table data
    output$download_result <- downloadHandler(
        filename = function() {
            # Define electoralEntityName here, accessible to both filename and content
            electoralEntityName <- if (grepl("_LAD", input$fileDropdown)) {
                paste0(LADname(), "_LAD")
            } else {
                paste0(constituencyName(), "_CON")
            }

            # Generate the filename
            paste0("result_", electoralEntityName, "_", input$weightingModel, "_", Sys.Date(), ".csv")
        },
        content = function(file) {
            # Ensure electoralEntityName is defined in the content section as well
            electoralEntityName <- if (grepl("_LAD", input$fileDropdown)) {
                paste0(LADname(), "_LAD_P")
            } else {
                paste0(constituencyName(), "_CON_P")
            }

            # Write to the local file
            write.csv(oa_unique_values_data(), file, row.names = FALSE)

            # Upload the file to Google Cloud Storage with a structured name
            gcs_upload(
                oa_unique_values_data(),
                name = paste0("result_", electoralEntityName, "_", input$weightingModel, "_", input$partyChoice),
                predefinedAcl = 'bucketLevel'
            )
        }
    )
}

# Run the application
shinyApp(ui = ui, server = server)
