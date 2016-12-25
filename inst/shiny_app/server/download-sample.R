# Disable select inputs if the checkbox "Use all" is checked
observeEvent(input$synthpop_year_all, {
    if (input$synthpop_year_all) {
        disable("synthpop_year")
    } else {
        enable("synthpop_year")
    }
})
observeEvent(input$synthpop_country_all, {
    if (input$synthpop_country_all) {
        disable("synthpop_country")
    } else {
        enable("synthpop_country")
    }
})
observeEvent(input$synthpop_component_all, {
    if (input$synthpop_component_all) {
        disable("synthpop_component")
    } else {
        enable("synthpop_component")
    }
})

generate_samples <- function() {
    if (input$synthpop_year_all) {
        years <- data$output_years
    } else {
        years <- input$synthpop_year
    }
    if (input$synthpop_country_all) {
        countries <- data$output_countries
    } else {
        countries <- input$synthpop_country
    }
    if (input$synthpop_component_all) {
        components <- data$output_components
    } else {
        components <- input$synthpop_component
    }

    n <- isolate(input$synthpop_size)
    df <- data.frame(row.names=1:n)
    for (year in years) {
        for (country in countries) {
            for (component in components) {
                data_label <- c(component, country, year)
                data_label <- data_label[data_label != "n/a"]
                data_label <- paste(data_label, collapse=", ")

                res <- data$output_dist[[year]][[country]][[component]]
                if (is.null(res)) {
                    next
                }

                col <- data.frame(sort(simulate_gpinter(res, n)))
                colnames(col) <- data_label

                df <- cbind(df, col)
            }
        }
    }

    return(df)
}

output$synthpop_dl_csv <- downloadHandler(
    filename = function() {
        return(paste0("sample-", format.Date(Sys.time(), "%Y-%m-%d-%H-%M-%S"), ".csv"))
    },
    content = function(dest) {
        df <- generate_samples()

        write.table(df,
            file = dest,
            na = "",
            row.names = FALSE,
            sep = isolate(input$csv_output_field_separator),
            dec = isolate(input$csv_output_dec_separator)
        )
    }
)

output$synthpop_dl_excel <- downloadHandler(
    filename = function() {
        return(paste0("simulation-", format.Date(Sys.time(), "%Y-%m-%d-%H-%M-%S"), ".xlsx"))
    },
    content = function(dest) {
        df <- generate_samples()

        wb <- createWorkbook()
        sheet <- createSheet(wb, "Samples")
        cs <- CellStyle(wb, dataFormat=DataFormat("# ##0.00"))
        cs <- rep(list(cs), ncol(df))
        names(cs) <- as.character(1:ncol(df))
        print(str(cs))
        addDataFrame(df, sheet, colStyle=rep(cs, ncol(df)), row.names=FALSE)
        saveWorkbook(wb, dest)
    }
)
