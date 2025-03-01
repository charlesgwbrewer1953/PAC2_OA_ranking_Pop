# Overview

This Shiny application performs factor weighting and ranking of output areas (OAs) based on user-specified parameters. Version 5.0.3 (operational status: Deployed) is designed to be integrated with Google Cloud Storage (GCS) for file retrieval and result uploads. The app allows users to load data files, adjust weighting factors interactively, calculate weighted sums for ranking, and download the final results.
Key Features

    Interactive User Interface:
    The UI is built with Shiny and includes:
        A dropdown menu to select files from GCS (filtered for filenames containing “candidate”).
        An editable parameter table (using rhandsontable) to set weights and quarterly factors (Q1–Q4) for each dimension (excluding the Population column).
        Inputs for party selection and naming a weighting model.
        Radio buttons for selecting the output display mode (Sum only, Non-zero only, or Show all).
        Download buttons for both the configuration and result files.

    File Retrieval and Data Management:
        Uses googleCloudStorageR to authenticate and retrieve files from a specified GCS bucket.
        The application automatically lists and filters available files based on a naming pattern.
        Once a file is loaded, it reorders the columns to ensure key identifiers (such as constituency name and local authority details) appear at the beginning.

    Dynamic Calculations:
        Processes the loaded data by applying user-specified weightings to various numeric columns.
        The Population column is preserved and used to adjust the weighted sums. A row-wise sum (raw and adjusted by the mean population) is computed to rank the OAs.
        The resulting table is dynamically rendered with options to display different subsets of columns.

    Output and Cloud Integration:
        Provides options to download the result as a CSV file. The filename is dynamically generated based on the electoral entity (constituency or local authority), the weighting model, and the current date.
        Uploads the final result back to the Google Cloud Storage bucket using structured naming conventions.

## Usage

    File Selection:
    The user selects a candidate file from the dropdown list populated from GCS. Clicking the “Load File” button downloads and displays the file’s content.

    Parameter Setup:
    The application renders a parameter table (excluding the Population column) where users can enter weights and quarter-specific values (Q1–Q4) for each factor.

    Calculation:
    Upon clicking “Calculate,” the app:
        Retrieves and converts the table data.
        Applies the specified weights to the corresponding columns.
        Computes the weighted sums for each OA and adjusts these using the Population column.
        Ranks the OAs by the final weighted sum.

    Result Download:
    Users can download the final results as a CSV file. The file is also automatically uploaded to GCS with a clear, structured filename.

## Dependencies

The application requires the following R packages:

    shiny
    rhandsontable
    dplyr
    tidyr
    stringr
    DT
    readr
    googlesheets4
    googleCloudStorageR
