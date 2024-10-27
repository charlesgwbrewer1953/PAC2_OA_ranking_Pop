##### Factor weighting and OA ranking - V4.3.0 Operational - GCS retrieval
#
#
#  OA RANKING
#
#
# oa_ranking1
# Becoming generalised
# oa_ranking2
# Sums calculated to azero dec plac 2.2.3
# Number of OAs added 2.3.5
# # Revised GCS storage
# DEV
version_no <- "4.5.1"
op_status <- "Operational"

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
    titlePanel("Factor weighting and OA ranking"),
    tags$div(
        style = "font-size: 12px; font-style: italic;",
        HTML("Version: "),
        textOutput(version_no)
    ),
    tags$div(
        style = "font-size: 12px; font-style: italic;",
        HTML("Status: "),
        textOutput("opStatus_output")
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
            selectInput("partyChoice", "Choose a party:", choices = c("CON", "LAB", "LIB", "REF", "NRF"), selected = "REF"),
            br(),
            textInput("weightingModel", "Weighting model (max 5 chars):", "", width = "100px"),
            tags$script(HTML("
                $(document).on('input', '#alphanumericInput', function() {
                    var value = $(this).val();
                    var alphanumericRegex = /^[a-z0-9]*$/i;
                    if (!alphanumericRegex.test(value) || value.length > 5) {
                        $(this).val(value.substring(0, 5).replace(/[^a-z0-9]/gi, ''));
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
        LADname <- object$LAD24NM[1]
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
    # If dimensions_val is not NULL, create the dataframe
    df <- data.frame(
        Title = dimensions_val[-(1:7)],
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

        input_df <- hot_to_r(input$parameter_table)  # Manual value present
        parameter_table_data(input_df)  # Store input table data

        oa_unique_values <- oa_unique()  # Initialize oa_unique_values with oa_unique

        # Loop over columns apart from 1 to 7 (id columns) in oa_unique_values
        for (col_name in names(oa_unique_values)[-c(1, 7)]) {
            # Get the corresponding row index in input_df for the column name
            row_index <- match(col_name, input_df$Title)

            # Loop through each row of oa_unique_values
            for (i in 1:nrow(oa_unique_values)) {
                # Get the content of the cell in oa_unique_values
                cell_content <- oa_unique_values[[col_name]][i]

                # Check if the cell content is one of "Q1", "Q2", "Q3", or "Q4"
                if (cell_content %in% c("Q1", "Q2", "Q3", "Q4")) {
                    # Get the value from input_df corresponding to the cell content
                    input_value <- input_df[row_index, cell_content]

                    # Assign input_value to oa_unique_values if it's not NA or NULL
                    if (!is.na(input_value) && !is.null(input_value)) {
                        oa_unique_values[[col_name]][i] <- input_value
                    }
                }
            }
        }


        # Convert table values to numeric
        numeric_columns <- 8:ncol(oa_unique_values)
        oa_unique_values <- oa_unique_values %>%
            mutate_at(vars(numeric_columns), as.numeric)
        # Define a function to convert Weight values to numeric and multiply with column values
        calculate_weighted_sum <- function(col_values, weight) {
            # Convert Weight to numeric
            weight_numeric <- as.numeric(weight)
            # Multiply each column value with the Weight
            result <- round(col_values * weight_numeric, 0)
            return(result)
        }

        # Iterate over each column in oa_unique_values
        for (col_name in names(oa_unique_values)[-c(1: 7)]) {
            # Get the corresponding Weight value from input_df
            weight_value <- input_df[input_df$Title == col_name, "Weight"]
            # Convert it to numeric
            weight_numeric <- as.numeric(weight_value)
            # Get the column values from oa_unique_values
            col_values <- oa_unique_values[[col_name]]
            # Calculate the weighted sum for the column
            weighted_values <- calculate_weighted_sum(col_values, weight_numeric)
            # Assign the weighted sum to the column in oa_unique_values
            oa_unique_values[[col_name]] <- weighted_values
        }
        # Add sum of row values

        oa_unique_values$Sum <- rowSums(oa_unique_values[, -(1:7)], na.rm = TRUE)
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
            non_zero_non_na_columns <- non_zero_non_na_columns + 7 # Compensate for first 7 alpha (ID) cols
            dt <- dt[,c(1:7, non_zero_non_na_columns)]
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

            timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
            constName <- constituencyName()
            paste0("result_", constName,input$weightingModel, "_" ,timestamp, ".csv")

        },
        content = function(file) {
            write.csv(oa_unique_values_data(), file, row.names = FALSE)
            # Upload the file to Google Cloud Storage
#            gcs_upload(oa_unique_values_data(), name = paste0("result_", constituencyName(),"_", input$partyChoice, format(Sys.time(), "%Y-%m-%d_%H-%M-%S")),
            gcs_upload(oa_unique_values_data(), name = paste0("result_", constituencyName(),"_", input$weightingModel,"_", input$partyChoice),
                        predefinedAcl='bucketLevel')
        }
    )
}

# Run the application
shinyApp(ui = ui, server = server)
