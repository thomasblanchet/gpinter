output$output_table <- renderUI({
    if (is.null(data$output_dist)) {
        return(tags$div(icon("info-circle"), HTML("&nbsp;"),
            "Tables of output results will appear here once the programs have been successfully executed.",
            class="alert alert-info", role="alert"))
    }

    year <- input$output_table_year
    country <- input$output_table_country
    component <- input$output_table_component

    result <- data$output_dist[[year]][[country]][[component]]
    table <- data$output_tables[[year]][[country]][[component]]

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
        align = "cccccc",
        striped = TRUE,
        width = "100%"
    )

    # Detailed tabulation
    out_df <- data.frame("Rank" = sprintf("%1.5f", gperc))

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
        align = paste0("l", paste0(rep("r", ncol(out_df) - 1), collapse = "")),
        striped = TRUE,
        width = "100%",
        na = "n.a."
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
        var <- list(
            year          = trimws(isolate(input$var_year)),
            country       = trimws(isolate(input$var_country)),
            component     = trimws(isolate(input$var_component)),
            p             = trimws(isolate(input$var_p)),
            q             = trimws(isolate(input$var_q)),
            b             = trimws(isolate(input$var_b)),
            bracketshare  = trimws(isolate(input$var_bracketshare)),
            topshare      = trimws(isolate(input$var_topshare)),
            bracketavg    = trimws(isolate(input$var_bracketavg)),
            topavg        = trimws(isolate(input$var_topavg)),
            average       = trimws(isolate(input$var_average))
        )

        tmp <- tempdir()
        # Time series for each country and income concept
        df_all_series <- data.frame()
        for (country in data$output_countries) {
            for (component in data$output_components) {
                for (year in data$output_years) {
                    result <- data$output_dist[[year]][[country]][[component]]
                    table <- data$output_tables[[year]][[country]][[component]]
                    if (is.null(result)) {
                        next
                    }
                    row <- data.frame(
                        "Country"    = country,
                        "Component"  = component,
                        "Year"       = year,
                        "Average"    = result$average,
                        "Bottom 50%" = table$bottom50,
                        "Middle 40%" = table$middle40,
                        "Top 10%"    = table$top10,
                        "Top 1%"     = table$top1,
                        "Gini"       = table$gini,
                        check.names  = FALSE,
                        stringsAsFactors = FALSE
                    )
                    df_all_series <- rbind(df_all_series, row)
                }
            }
        }
        write.table(df_all_series,
            file = paste0(tmp, "/series.csv"),
            na = "",
            row.names = FALSE,
            sep = isolate(input$csv_output_field_separator),
            dec = isolate(input$csv_output_dec_separator)
        )
        files <- paste0(tmp, "/series.csv")

        # Files with detailed g-perc data
        for (country in data$output_countries) {
            for (component in data$output_components) {
                for (year in data$output_years) {
                    result <- data$output_dist[[year]][[country]][[component]]
                    table <- data$output_tables[[year]][[country]][[component]]
                    if (is.null(result)) {
                        next
                    }

                    data_label <- c(component, country, year)
                    data_label <- data_label[!data_label %in% c("n.a.", "merged", "added up")]
                    data_label <- paste(data_label, collapse=", ")

                    out_df <- data.frame(
                        "year" = c(year, rep(NA, length(gperc) - 1)),
                        "country" = c(country, rep("", length(gperc) - 1)),
                        "component" = c(component, rep("", length(gperc) - 1)),
                        "average" = c(result$average, rep(NA, length(gperc) - 1)),
                        "p" = gperc
                    )
                    colnames(out_df) <- c(var$year, var$country, var$component, var$average, var$p)
                    if (year == "n.a.") {
                        out_df[var$year] <- NULL
                    }
                    if (country == "n.a.") {
                        out_df[var$country] <- NULL
                    }
                    if (component == "n.a.") {
                        out_df[var$component] <- NULL
                    }

                    if ("thres" %in% input$results_display) {
                        out_df[var$q] <- table$threshold
                    }
                    if ("topshare" %in% input$results_display) {
                        out_df[var$topshare] <- table$top_share
                    }
                    if ("bottomshare" %in% input$results_display) {
                        out_df[var$bottomshare] <- table$bottom_share
                    }
                    if ("bracketshare" %in% input$results_display) {
                        out_df[var$bracketshare] <- table$bracket_share
                    }
                    if ("topavg" %in% input$results_display) {
                        out_df[var$topavg] <- table$top_average
                    }
                    if ("bracketavg" %in% input$results_display) {
                        out_df[var$bracketavg] <- table$bracket_average
                    }
                    if ("invpareto" %in% input$results_display) {
                        out_df[var$b] <- table$invpareto
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
        var <- list(
            year          = trimws(isolate(input$var_year)),
            country       = trimws(isolate(input$var_country)),
            component     = trimws(isolate(input$var_component)),
            p             = trimws(isolate(input$var_p)),
            q             = trimws(isolate(input$var_q)),
            b             = trimws(isolate(input$var_b)),
            bracketshare  = trimws(isolate(input$var_bracketshare)),
            topshare      = trimws(isolate(input$var_topshare)),
            bracketavg    = trimws(isolate(input$var_bracketavg)),
            topavg        = trimws(isolate(input$var_topavg)),
            average       = trimws(isolate(input$var_average))
        )

        # Keep a list of sheet names to avoid duplicate names
        all_sheet_names <- c()
        # Create the workbook
        wb <- createWorkbook()

        # Time series for each country and income concept
        df_all_series <- data.frame()
        for (country in data$output_countries) {
            for (component in data$output_components) {
                for (year in data$output_years) {
                    result <- data$output_dist[[year]][[country]][[component]]
                    table <- data$output_tables[[year]][[country]][[component]]
                    if (is.null(result)) {
                        next
                    }
                    row <- data.frame(
                        "Country"    = country,
                        "Component"  = component,
                        "Year"       = year,
                        "Average"    = result$average,
                        "Bottom 50%" = table$bottom50,
                        "Middle 40%" = table$middle40,
                        "Top 10%"    = table$top10,
                        "Top 1%"     = table$top1,
                        "Gini"       = table$gini,
                        check.names  = FALSE,
                        stringsAsFactors = FALSE
                    )
                    df_all_series <- rbind(df_all_series, row)
                }
            }
        }
        sheet_name <- "series"
        all_sheet_names <- sheet_name
        sheet <- createSheet(wb, sheet_name)
        addDataFrame(df_all_series, sheet, row.names=FALSE)

        counter <- 1
        for (country in data$output_countries) {
            for (component in data$output_components) {
                for (year in data$output_years) {
                    result <- data$output_dist[[year]][[country]][[component]]
                    table <- data$output_tables[[year]][[country]][[component]]
                    if (is.null(result)) {
                        next
                    }

                    data_label <- c(component, country, year)
                    data_label <- data_label[!data_label %in% c("n.a.", "merged", "added up")]
                    data_label <- paste(data_label, collapse=", ")

                    out_df <- data.frame(
                        "year" = c(year, rep(NA, length(gperc) - 1)),
                        "country" = c(country, rep("", length(gperc) - 1)),
                        "component" = c(component, rep("", length(gperc) - 1)),
                        "average" = c(result$average, rep(NA, length(gperc) - 1)),
                        "p" = gperc
                    )
                    colnames(out_df) <- c(var$year, var$country, var$component, var$average, var$p)
                    if (year == "n.a.") {
                        out_df[var$year] <- NULL
                    }
                    if (country == "n.a.") {
                        out_df[var$country] <- NULL
                    }
                    if (component == "n.a.") {
                        out_df[var$component] <- NULL
                    }

                    if ("thres" %in% input$results_display) {
                        out_df[var$q] <- table$threshold
                    }
                    if ("topshare" %in% input$results_display) {
                        out_df[var$topshare] <- table$top_share
                    }
                    if ("bottomshare" %in% input$results_display) {
                        out_df[var$bottomshare] <- table$bottom_share
                    }
                    if ("bracketshare" %in% input$results_display) {
                        out_df[var$bracketshare] <- table$bracket_share
                    }
                    if ("topavg" %in% input$results_display) {
                        out_df[var$topavg] <- table$top_average
                    }
                    if ("bracketavg" %in% input$results_display) {
                        out_df[var$bracketavg] <- table$bracket_average
                    }
                    if ("invpareto" %in% input$results_display) {
                        out_df[var$b] <- table$invpareto
                    }

                    sheet_name <- strtrim(data_label, 31)
                    i <- 1
                    while (tolower(sheet_name) %in% all_sheet_names) {
                        to_add <- paste0(" (", i, ")")
                        sheet_name <- paste0(strtrim(data_label, 31 - nchar(to_add)), to_add)
                        i <- i + 1
                    }
                    all_sheet_names <- c(all_sheet_names, tolower(sheet_name))
                    sheet <- createSheet(wb, sheet_name)
                    addDataFrame(out_df, sheet, row.names=FALSE)
                    # Call Java garbage collector
                    jgc()
                }
            }
        }

        # Save the workbook
        saveWorkbook(wb, dest)
    }
)
