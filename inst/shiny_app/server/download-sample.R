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

output$synthpop_dl_csv <- downloadHandler(
    filename = function() {
        return(paste0("sample-", format.Date(Sys.time(), "%Y-%m-%d-%H-%M-%S"), ".csv"))
    },
    content = function(dest) {
        if (input$synthpop_year_all) {
            years <- data$years
        } else {
            years <- input$synthpop_year
        }
        if (input$synthpop_country_all) {
            countries <- data$countries
        } else {
            countries <- input$synthpop_country
        }
        if (input$synthpop_component_all) {
            components <- data$components
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

                    res <- data$results[[year]][[country]][[component]]
                    if (is.null(res)) {
                        next
                    }

                    col <- data.frame(sort(round(simulate_gpinter(res, n), digits=3)))
                    colnames(col) <- data_label

                    df <- cbind(df, col)
                }
            }
        }

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
    content = function(dest) {if (input$synthpop_year_all) {
        years <- data$years
    } else {
        years <- input$synthpop_year
    }
        if (input$synthpop_country_all) {
            countries <- data$countries
        } else {
            countries <- input$synthpop_country
        }
        if (input$synthpop_component_all) {
            components <- data$components
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

                    res <- data$results[[year]][[country]][[component]]
                    if (is.null(res)) {
                        next
                    }

                    col <- data.frame(sort(round(simulate_gpinter(res, n), digits=3)))
                    colnames(col) <- data_label

                    df <- cbind(df, col)
                }
            }
        }

        write.xlsx2(df, dest, row.names = FALSE)
    }
)
