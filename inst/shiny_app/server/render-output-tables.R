output$output_table <- renderUI({
    if (is.null(data$results)) {
        return(tags$div(icon("info-circle"), HTML("&nbsp;"),
            "The results will appear here once the programs have been successfully executed.",
            class="alert alert-info", role="alert"))
    }

    year <- input$output_table_year
    country <- input$output_table_country
    component <- input$output_table_component

    result <- data$results[[year]][[country]][[component]]
    table <- data$tables[[year]][[country]][[component]]

    if (is.null(result)) {
        return(tags$div(icon("info-circle"), HTML("&nbsp;"),
            "No data available for your selection.",
            class="alert alert-info", role="alert"))
    }

    summary_table <- renderTable(
        data.frame(
            "Average" = format(round(result$average), big.mark=" ", scientific=FALSE),
            "Bottom 50%" = sprintf("%.1f%%", 100*table$bottom50),
            "Middle 40%" = sprintf("%.1f%%", 100*table$middle40),
            "Top 10%" = sprintf("%.1f%%", 100*table$top10),
            "Top 1%" = sprintf("%.1f%%", 100*table$top1),
            "Gini" = sprintf("%.3f", table$gini),
            check.names = FALSE
        ),
        striped = TRUE,
        width = "100%"
    )

    # Detailed tabulation
    out_df <- data.frame("Fractiles" = sprintf("%1.5f", gperc))

    if ("thres" %in% input$results_display) {
        out_df["Threshold"] <- ifelse(is.na(table$threshold), NA, format(round(table$threshold), big.mark=" ", scientific=FALSE))
        out_df[is.infinite(table$threshold), "Threshold"] <- "–∞"
    }
    if ("topshare" %in% input$results_display) {
        out_df["Top share"] <- ifelse(is.na(table$top_share), NA, sprintf("%.2f%%", 100*table$top_share))
    }
    if ("bottomshare" %in% input$results_display) {
        out_df["Bottom share"] <- ifelse(is.na(table$bottom_share), NA, sprintf("%.2f%%", 100*table$bottom_share))
    }
    if ("bracketshare" %in% input$results_display) {
        out_df["Bracket share"] <- ifelse(is.na(table$bracket_share), NA, sprintf("%.2f%%", 100*table$bracket_share))
    }
    if ("topavg" %in% input$results_display) {
        out_df["Top average"] <- ifelse(is.na(table$top_average), NA, format(round(table$top_average), big.mark=" ", scientific=FALSE))
    }
    if ("bracketavg" %in% input$results_display) {
        out_df["Bracket average"] <- ifelse(is.na(table$bracket_average), NA, format(round(table$bracket_average), big.mark=" ", scientific=FALSE))
    }
    if ("invpareto" %in% input$results_display) {
        out_df["Inverted Pareto coefficient"] <- ifelse(is.na(table$invpareto), NA, sprintf("%.2f", table$invpareto))
        out_df[is.infinite(table$invpareto), "Inverted Pareto coefficient"] <- "∞"
    }

    detailed_table <- renderTable(out_df,
        striped = TRUE,
        width = "100%",
        na = "n/a"
    )

    return(tagList(
        tags$h4("Summary"),
        summary_table,
        tags$h4("Details"),
        detailed_table
    ))
})

# Download handler for CSV
output$dl_tables_csv <- downloadHandler(
    filename = function() {
        return(paste0("tables-", format.Date(Sys.time(), "%Y-%m-%d-%H-%M-%S"), ".zip"))
    },
    content = function(dest) {
        tmp <- tempdir()
        files <- c()
        for (country in data$results_countries) {
            for (component in data$results_components) {
                # Times series for the given country and income concept
                series_label <- c(component, country)
                series_label <- series_label[series_label != "n/a"]
                series_label <- paste(series_label, collapse=", ")
                if (series_label == "") {
                    series_label <- "series"
                } else {
                    series_label <- paste("series", series_label, sep=" - ")
                }

                df_series <- data.frame(
                    "Year" = data$years,
                    "Average" = sapply(data$years, function(year) {
                        result <- data$results[[year]][[country]][[component]]
                        if (is.null(result)) {
                            return(NA)
                        } else {
                            return(result$average)
                        }
                    }),
                    "Bottom 50%" = sapply(data$years, function(year) {
                        table <- data$tables[[year]][[country]][[component]]
                        if (is.null(table)) {
                            return(NA)
                        } else {
                            return(table$bottom50)
                        }
                    }),
                    "Middle 40%" = sapply(data$years, function(year) {
                        table <- data$tables[[year]][[country]][[component]]
                        if (is.null(table)) {
                            return(NA)
                        } else {
                            return(table$middle40)
                        }
                    }),
                    "Top 10%" = sapply(data$years, function(year) {
                        table <- data$tables[[year]][[country]][[component]]
                        if (is.null(table)) {
                            return(NA)
                        } else {
                            return(table$top10)
                        }
                    }),
                    "Top 1%" = sapply(data$years, function(year) {
                        table <- data$tables[[year]][[country]][[component]]
                        if (is.null(table)) {
                            return(NA)
                        } else {
                            return(table$top1)
                        }
                    }),
                    "Gini" = sapply(data$years, function(year) {
                        table <- data$tables[[year]][[country]][[component]]
                        if (is.null(table)) {
                            return(NA)
                        } else {
                            return(table$gini)
                        }
                    }),
                    check.names  = FALSE,
                    stringsAsFactors = FALSE
                )
                df_series[, "Year"] <- as.numeric(df_series[, "Year"])
                df_series <- df_series[!is.na(df_series[, "Year"]), ]
                df_series <- df_series[order(df_series[, "Year"]), ]

                filename_series <- paste0(tmp, "/", series_label, ".csv")
                write.table(df_series,
                    file = filename_series,
                    na = "",
                    row.names = FALSE,
                    sep = isolate(input$csv_output_field_separator),
                    dec = isolate(input$csv_output_dec_separator)
                )
                files <- c(files, filename_series)

                for (year in data$results_years) {
                    result <- data$results[[year]][[country]][[component]]
                    table <- data$tables[[year]][[country]][[component]]
                    if (is.null(result)) {
                        next
                    }

                    data_label <- c(component, country, year)
                    data_label <- data_label[data_label != "n/a"]
                    data_label <- paste(data_label, collapse=", ")

                    out_df <- data.frame("Fractiles" = gperc)

                    if ("thres" %in% input$results_display) {
                        out_df["Threshold"] <- table$threshold
                    }
                    if ("topshare" %in% input$results_display) {
                        out_df["Top share"] <- table$top_share
                    }
                    if ("bottomshare" %in% input$results_display) {
                        out_df["Bottom share"] <- table$bottom_share
                    }
                    if ("bracketshare" %in% input$results_display) {
                        out_df["Bracket share"] <- table$bracket_share
                    }
                    if ("topavg" %in% input$results_display) {
                        out_df["Top average"] <- table$top_average
                    }
                    if ("bracketavg" %in% input$results_display) {
                        out_df["Bracket average"] <- table$bracket_average
                    }
                    if ("invpareto" %in% input$results_display) {
                        out_df["Inverted Pareto coefficient"] <- table$invpareto
                    }

                    filename <- paste0(tmp, "/", data_label, ".csv")
                    write.table(out_df,
                        file = filename,
                        na = "",
                        row.names = FALSE,
                        sep = isolate(input$csv_output_field_separator),
                        dec = isolate(input$csv_output_dec_separator)
                    )
                    files <- c(files, filename)
                }
            }
        }

        # Zip the files to destination
        zip(dest, files, flags="-r9Xj")
    }
)

# Download handler for Excel
output$dl_tables_excel <- downloadHandler(
    filename = function() {
        return(paste0("tables-", format.Date(Sys.time(), "%Y-%m-%d-%H-%M-%S"), ".xlsx"))
    },
    content = function(dest) {
        gperc <- c(
            seq(0, 0.99, 0.01), seq(0.991, 0.999, 0.001),
            seq(0.9991, 0.9999, 0.0001), seq(0.99991, 0.99999, 0.00001)
        )
        # Create the workbook
        wb <- createWorkbook()
        for (country in data$results_countries) {
            for (component in data$results_components) {
                # Times series for the given country and income concept
                series_label <- c(component, country)
                series_label <- series_label[series_label != "n/a"]
                series_label <- paste(series_label, collapse=", ")
                if (series_label == "") {
                    series_label <- "series"
                } else {
                    series_label <- paste("series", series_label, sep=" - ")
                }

                df_series <- data.frame(
                    "Year" = data$years,
                    "Average" = sapply(data$years, function(year) {
                        result <- data$results[[year]][[country]][[component]]
                        if (is.null(result)) {
                            return(NA)
                        } else {
                            return(result$average)
                        }
                    }),
                    "Bottom 50%" = sapply(data$years, function(year) {
                        table <- data$tables[[year]][[country]][[component]]
                        if (is.null(table)) {
                            return(NA)
                        } else {
                            return(table$bottom50)
                        }
                    }),
                    "Middle 40%" = sapply(data$years, function(year) {
                        table <- data$tables[[year]][[country]][[component]]
                        if (is.null(table)) {
                            return(NA)
                        } else {
                            return(table$middle40)
                        }
                    }),
                    "Top 10%" = sapply(data$years, function(year) {
                        table <- data$tables[[year]][[country]][[component]]
                        if (is.null(table)) {
                            return(NA)
                        } else {
                            return(table$top10)
                        }
                    }),
                    "Top 1%" = sapply(data$years, function(year) {
                        table <- data$tables[[year]][[country]][[component]]
                        if (is.null(table)) {
                            return(NA)
                        } else {
                            return(table$top1)
                        }
                    }),
                    "Gini" = sapply(data$years, function(year) {
                        table <- data$tables[[year]][[country]][[component]]
                        if (is.null(table)) {
                            return(NA)
                        } else {
                            return(table$gini)
                        }
                    }),
                    check.names  = FALSE,
                    stringsAsFactors = FALSE
                )
                df_series[, "Year"] <- as.numeric(df_series[, "Year"])
                df_series <- df_series[!is.na(df_series[, "Year"]), ]
                df_series <- df_series[order(df_series[, "Year"]), ]

                sheet <- createSheet(wb, strtrim(series_label, 31))
                addDataFrame(df_series, sheet, row.names=FALSE)

                for (year in data$results_years) {
                    result <- data$results[[year]][[country]][[component]]
                    table <- data$tables[[year]][[country]][[component]]
                    if (is.null(result)) {
                        next
                    }

                    data_label <- c(component, country, year)
                    data_label <- data_label[data_label != "n/a"]
                    data_label <- paste(data_label, collapse=", ")
                    if (data_label == "") {
                        data_label <- "no name"
                    }

                    out_df <- data.frame("Percentiles" = gperc)

                    if ("thres" %in% input$results_display) {
                        out_df["Threshold"] <- table$threshold
                    }
                    if ("topshare" %in% input$results_display) {
                        out_df["Top share"] <- table$top_share
                    }
                    if ("bottomshare" %in% input$results_display) {
                        out_df["Bottom share"] <- table$bottom_share
                    }
                    if ("bracketshare" %in% input$results_display) {
                        out_df["Bracket share"] <- table$bracket_share
                    }
                    if ("topavg" %in% input$results_display) {
                        out_df["Top average"] <- table$top_average
                    }
                    if ("bracketavg" %in% input$results_display) {
                        out_df["Bracket average"] <- table$bracket_average
                    }
                    if ("invpareto" %in% input$results_display) {
                        out_df["Inverted Pareto coefficient"] <- table$invpareto
                    }

                    sheet <- createSheet(wb, strtrim(data_label, 31))
                    addDataFrame(out_df, sheet, row.names=FALSE)
                }
            }
        }

        # Save the workbook
        saveWorkbook(wb, dest)
    }
)
