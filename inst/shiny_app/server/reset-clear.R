clear_data <- function() {
    data$files_all         <- NULL
    data$files_selected    <- NULL
    data$input_data        <- NULL
    data$input_data_size   <- NULL
    data$input_errors      <- NULL
    data$input_years       <- NULL
    data$input_countries   <- NULL
    data$input_components  <- NULL
    data$output_dist       <- NULL
    data$output_tables     <- NULL
    data$output_years      <- NULL
    data$output_countries  <- NULL
    data$output_components <- NULL

    disable("synthpop_dl_csv")
    disable("synthpop_dl_excel")
    disable("synthpop_year_all")
    disable("synthpop_country_all")
    disable("synthpop_component_all")
    disable("dl_tables_csv")
    disable("dl_tables_excel")

    ids <- c(
        "output_table_year",
        "output_dist_plot_year",
        "output_table_country",
        "output_dist_plot_country",
        "output_time_plot_country",
        "output_table_component",
        "output_dist_plot_component",
        "output_time_plot_component",
        "synthpop_year",
        "synthpop_country",
        "synthpop_component"
    )
    for (id in ids) {
        updateSelectInput(session, id, choices=list())
        disable(id)
    }
}

reset_app <- function() {
    clear_data()
    # Reset file input
    shinyjs::reset("file_input")
}

# Clear all and go back to home when clicking title
observeEvent(input$main_logo, {
    updateNavbarPage(session, "main_navbar", selected="Input data")
    reset_app()
})
