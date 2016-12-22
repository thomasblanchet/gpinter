clear_all <- function() {
    data$data               <- NULL
    data$nb_data            <- NULL
    data$errors             <- NULL
    data$year               <- NULL
    data$countries          <- NULL
    data$components         <- NULL
    data$results            <- NULL
    data$tables             <- NULL
    data$results_years      <- NULL
    data$results_countries  <- NULL
    data$results_components <- NULL

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
