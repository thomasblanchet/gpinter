output$extra_tables_buttons <- renderUI({
    # Check if some countries have been merged: if yes, display a button
    # to show the extra table; if no, show that no extra table is available.
    # Make a list of years/components with merged countries
    years_merged <- NULL
    components_merged <- NULL
    for (country in data$output_countries) {
        for (component in data$output_components) {
            for (year in data$output_years) {
                result <- data$output_dist[[year]][[country]][[component]]
                if (is(result, "gpinter_dist_merge")) {
                    years_merged <- c(years_merged, year)
                    components_merged <- c(components_merged, component)
                }
            }
        }
    }
    years_merged <- unique(years_merged)
    components_merged <- unique(components_merged)
    has_merged <- (length(years_merged) > 0) && (length(components_merged) > 0)

    if (has_merged) {
        data$years_merged <- years_merged
        data$components_merged <- components_merged
        return(tagList(
            tags$h5(tagList(icon("percent"), tags$span(HTML("&nbsp; Country decomposition"))),
                style="margin-top: 0;"),
            tags$p("Table showing the contribution of each country to each
                percentile of the merged distribution of all countries.",
                style="font-size: small; color: #666;"),
            actionButton("show_country_contribution", "Show table",
                class = "btn-block btn-xs")
        ))
        return()
    } else {
        return(tags$p("No extra tables available for your inputs.",
            style="text-align: center; color: #666; margin: 0; font-size: small;"))
    }
})

observeEvent(input$show_country_contribution, {
    # Show a modal with the table
    showModal(modalDialog(
        fluidRow(
            column(12,
                tags$p("This table shows how each country contributes to each bracket share
                    or top share of the distribution of income of all the countries merged together.
                    Contributions can be expressed in terms of population (eg. what is the proportion
                    of Americans in the global top 1%?) or income (eg. what proportion of income
                    in the global top 1% belongs to Americans?) Contributions in each bracket should
                    always sum to 100%."),
                fixedRow(
                    tags$div(
                        column(4,
                            tags$div(
                                tags$div(
                                    "Decomposition type",
                                    class = "panel-heading"
                                ),
                                tags$div(
                                    radioButtons("contrib_type_choice", NULL,
                                        selected = "population",
                                        choices = c("population", "income or wealth")
                                    ),
                                    class = "panel-body"
                                ),
                                class = "panel panel-default"
                            )
                        ),
                        column(4,
                            tags$div(
                                tags$div(
                                    "Bracket type",
                                    class = "panel-heading"
                                ),
                                tags$div(
                                    radioButtons("contrib_display_choice", NULL,
                                        selected = "top contributions",
                                        choices = c("top contributions", "bracket contributions")
                                    ),
                                    class = "panel-body"
                                ),
                                class = "panel panel-default"
                            )
                        ),
                        column(4,
                            tags$div(
                                tags$div(
                                    "Smoothing",
                                    class = "panel-heading"
                                ),
                                tags$div(
                                    radioButtons("contrib_smoothing_choice", NULL,
                                        selected = "no",
                                        choices = c("no", "yes")
                                    ),
                                    class = "panel-body"
                                ),
                                class = "panel panel-default"
                            )
                        ),
                        id = "contrib_choice"
                    )
                ),
                fixedRow(
                    column(6, downloadButton("download_contrib_csv", label="Download as CSV", class="btn-primary btn-block")),
                    column(6, downloadButton("download_contrib_excel", label="Download as Excel", class="btn-primary btn-block"))
                ),
                tags$hr(),
                fixedRow(
                    column(6, selectInput("contrib_table_year", "Year", choices=data$years_merged, width="100%")),
                    column(6, selectInput("contrib_table_component", "Component", choices=data$components_merged, width="100%"))
                ),
                uiOutput("contrib_table")
            )
        ),
        title = tagList(
            tags$button(
                tags$span(HTML("&times;"), `aria-hidden`="true"),
                type="button",
                class="close",
                `data-dismiss`="modal",
                `aria-label`="Close"
            ),
            icon("percent"), tags$span(HTML("&nbsp; Country decomposition"))),
        footer = NULL,
        size = "l",
        easyClose = TRUE
    ))
    if (length(data$years_merged) == 1) {
        shinyjs::disable("contrib_table_year")
    }
    if (length(data$components_merged) == 1) {
        shinyjs::disable("contrib_table_component")
    }
})

output$contrib_table <- renderUI({
    selected_year <- input$contrib_table_year
    selected_component <- input$contrib_table_component

    merged_result <- data$output_dist[[selected_year]][["merged"]][[selected_component]]
    merged_table <- data$output_tables[[selected_year]][["merged"]][[selected_component]]

    if (is.null(merged_result)) {
        return(tags$div(icon("info-circle"), HTML("&nbsp;"),
            "No data available for your selection.",
            class="alert alert-info", role="alert"))
    }

    thr <- merged_table$threshold
    if ((input$contrib_display_choice == "top contributions")
        && (input$contrib_type_choice == "population")) {

        pop_contrib <- sapply(seq_along(merged_result$parent_dist), function(i) {
            parent <- merged_result$parent_dist[[i]]
            relsize <- merged_result$relsize[i]
            contrib <- (1 - fitted_cdf(parent, thr))*relsize/(1 - gperc)
            if (input$contrib_smoothing_choice == "yes") {
                contrib <- movavg(contrib, 3)
            }
            return(sprintf("%.2f%%", 100*contrib))
        })
        colnames(pop_contrib) <- sapply(seq_along(merged_result$parent_dist), function(i) {
            return(merged_result$parent_dist[[i]]$country)
        })

        df <- data.frame(
            "Rank" = sprintf("%1.5f", gperc),
            "Threshold" = format(round(thr), big.mark=" ", scientific=FALSE),
            check.names = FALSE
        )
        df <- cbind(df, pop_contrib)

        return(tagList(renderTable(df,
            align = paste0("l", paste0(rep("r", ncol(df) - 1), collapse = "")),
            striped = TRUE,
            width = "100%"
        )))
    } else if ((input$contrib_display_choice == "bracket contributions")
        && (input$contrib_type_choice == "population")) {

        pop_contrib <- sapply(seq_along(merged_result$parent_dist), function(i) {
            parent <- merged_result$parent_dist[[i]]
            relsize <- merged_result$relsize[i]
            contrib <- diff(c(fitted_cdf(parent, thr), 1))*relsize/diff(c(gperc, 1))
            if (input$contrib_smoothing_choice == "yes") {
                contrib <- movavg(contrib, 3)
            }
            return(sprintf("%.2f%%", 100*contrib))
        })
        colnames(pop_contrib) <- sapply(seq_along(merged_result$parent_dist), function(i) {
            return(merged_result$parent_dist[[i]]$country)
        })

        df <- data.frame(
            "Rank" = sprintf("%1.5f", gperc),
            "Threshold" = format(round(thr), big.mark=" ", scientific=FALSE),
            check.names = FALSE
        )
        df <- cbind(df, pop_contrib)

        return(tagList(renderTable(df,
            align = paste0("l", paste0(rep("r", ncol(df) - 1), collapse = "")),
            striped = TRUE,
            width = "100%"
        )))
    } else if ((input$contrib_display_choice == "top contributions")
        && (input$contrib_type_choice == "income or wealth")) {

        pop_contrib <- sapply(seq_along(merged_result$parent_dist), function(i) {
            parent <- merged_result$parent_dist[[i]]
            relsize <- merged_result$relsize[i]*parent$average/merged_result$average
            contrib <- threshold_share(parent, thr)*relsize/merged_table$top_share
            if (input$contrib_smoothing_choice == "yes") {
                contrib <- movavg(contrib, 3)
            }
            return(sprintf("%.2f%%", 100*contrib))
        })
        colnames(pop_contrib) <- sapply(seq_along(merged_result$parent_dist), function(i) {
            return(merged_result$parent_dist[[i]]$country)
        })

        df <- data.frame(
            "Rank" = sprintf("%1.5f", gperc),
            "Threshold" = format(round(thr), big.mark=" ", scientific=FALSE),
            check.names = FALSE
        )
        df <- cbind(df, pop_contrib)

        return(tagList(renderTable(df,
            align = paste0("l", paste0(rep("r", ncol(df) - 1), collapse = "")),
            striped = TRUE,
            width = "100%"
        )))
    } else if ((input$contrib_display_choice == "bracket contributions")
        && (input$contrib_type_choice == "income or wealth")) {

        pop_contrib <- sapply(seq_along(merged_result$parent_dist), function(i) {
            parent <- merged_result$parent_dist[[i]]
            relsize <- merged_result$relsize[i]*parent$average/merged_result$average
            contrib <- diff(c(threshold_share(parent, thr), 0))*relsize/diff(c(merged_table$top_share, 0))
            if (input$contrib_smoothing_choice == "yes") {
                contrib <- movavg(contrib, 3)
            }
            return(sprintf("%.2f%%", 100*contrib))
        })
        colnames(pop_contrib) <- sapply(seq_along(merged_result$parent_dist), function(i) {
            return(merged_result$parent_dist[[i]]$country)
        })

        df <- data.frame(
            "Rank" = sprintf("%1.5f", gperc),
            "Threshold" = format(round(thr), big.mark=" ", scientific=FALSE),
            check.names = FALSE
        )
        df <- cbind(df, pop_contrib)

        return(tagList(renderTable(df,
            align = paste0("l", paste0(rep("r", ncol(df) - 1), collapse = "")),
            striped = TRUE,
            width = "100%"
        )))
    }
})

# Download handler for CSV
output$download_contrib_csv <- downloadHandler(
    filename = function() {
        return(paste0("contribution-", format.Date(Sys.time(), "%Y-%m-%d-%H-%M-%S"), ".zip"))
    },
    content = function(dest) {
        display_choice <- input$contrib_display_choice
        type_choice <- input$contrib_type_choice

        files <- NULL
        tmp <- tempdir()

        for (year in data$years_merged) {
            for (component in data$components_merged) {
                merged_result <- data$output_dist[[year]][["merged"]][[component]]
                merged_table <- data$output_tables[[year]][["merged"]][[component]]

                if (is.null(merged_result)) {
                    next
                }

                data_label <- c(component, year)
                data_label <- data_label[!data_label %in% c("n.a.", "merged", "added up")]
                data_label <- paste(data_label, collapse=", ")

                thr <- merged_table$threshold
                if ((display_choice == "top contributions") && (type_choice == "population")) {

                    pop_contrib <- sapply(seq_along(merged_result$parent_dist), function(i) {
                        parent <- merged_result$parent_dist[[i]]
                        relsize <- merged_result$relsize[i]
                        contrib <- (1 - fitted_cdf(parent, thr))*relsize/(1 - gperc)
                        if (input$contrib_smoothing_choice == "yes") {
                            contrib <- movavg(contrib, 3)
                        }
                        return(contrib)
                    })
                    colnames(pop_contrib) <- sapply(seq_along(merged_result$parent_dist), function(i) {
                        return(merged_result$parent_dist[[i]]$country)
                    })

                    df <- data.frame(p=gperc, thr=thr)
                    df <- cbind(df, pop_contrib)

                } else if ((display_choice == "bracket contributions") && (type_choice == "population")) {

                    pop_contrib <- sapply(seq_along(merged_result$parent_dist), function(i) {
                        parent <- merged_result$parent_dist[[i]]
                        relsize <- merged_result$relsize[i]
                        contrib <- diff(c(fitted_cdf(parent, thr), 1))*relsize/diff(c(gperc, 1))
                        if (input$contrib_smoothing_choice == "yes") {
                            contrib <- movavg(contrib, 3)
                        }
                        return(contrib)
                    })
                    colnames(pop_contrib) <- sapply(seq_along(merged_result$parent_dist), function(i) {
                        return(merged_result$parent_dist[[i]]$country)
                    })

                    df <- data.frame(p=gperc, thr=thr)
                    df <- cbind(df, pop_contrib)

                } else if ((display_choice == "top contributions") && (type_choice == "income or wealth")) {

                    pop_contrib <- sapply(seq_along(merged_result$parent_dist), function(i) {
                        parent <- merged_result$parent_dist[[i]]
                        relsize <- merged_result$relsize[i]*parent$average/merged_result$average
                        contrib <- threshold_share(parent, thr)*relsize/merged_table$top_share
                        if (input$contrib_smoothing_choice == "yes") {
                            contrib <- movavg(contrib, 3)
                        }
                        return(contrib)
                    })
                    colnames(pop_contrib) <- sapply(seq_along(merged_result$parent_dist), function(i) {
                        return(merged_result$parent_dist[[i]]$country)
                    })

                    df <- data.frame(p=gperc, thr=thr)
                    df <- cbind(df, pop_contrib)

                } else if ((display_choice == "bracket contributions") && (type_choice == "income or wealth")) {

                    pop_contrib <- sapply(seq_along(merged_result$parent_dist), function(i) {
                        parent <- merged_result$parent_dist[[i]]
                        relsize <- merged_result$relsize[i]*parent$average/merged_result$average
                        contrib <- diff(c(threshold_share(parent, thr), 0))*relsize/diff(c(merged_table$top_share, 0))
                        if (input$contrib_smoothing_choice == "yes") {
                            contrib <- movavg(contrib, 3)
                        }
                        return(contrib)
                    })
                    colnames(pop_contrib) <- sapply(seq_along(merged_result$parent_dist), function(i) {
                        return(merged_result$parent_dist[[i]]$country)
                    })

                    df <- data.frame(p=gperc, thr=thr)
                    df <- cbind(df, pop_contrib)

                }

                filename <- paste0(tmp, "/", data_label, ".csv")
                write.table(df,
                    file = filename,
                    na = "",
                    row.names = FALSE,
                    sep = isolate(input$csv_output_field_separator),
                    dec = isolate(input$csv_output_dec_separator)
                )
                files <- c(files, filename)
            }
        }

        # Zip the files to destination
        zip(dest, files, flags="-r9Xj")
    }
)


# Download handler for CSV
output$download_contrib_excel <- downloadHandler(
    filename = function() {
        return(paste0("contribution-", format.Date(Sys.time(), "%Y-%m-%d-%H-%M-%S"), ".xlsx"))
    },
    content = function(dest) {
        display_choice <- input$contrib_display_choice
        type_choice <- input$contrib_type_choice

        # Keep a list of sheet names to avoid duplicate names
        all_sheet_names <- c()
        # Create the workbook
        wb <- createWorkbook()

        for (year in data$years_merged) {
            for (component in data$components_merged) {
                merged_result <- data$output_dist[[year]][["merged"]][[component]]
                merged_table <- data$output_tables[[year]][["merged"]][[component]]

                if (is.null(merged_result)) {
                    next
                }

                data_label <- c(component, year)
                data_label <- data_label[!data_label %in% c("n.a.", "merged", "added up")]
                data_label <- paste(data_label, collapse=", ")

                thr <- merged_table$threshold
                if ((display_choice == "top contributions") && (type_choice == "population")) {

                    pop_contrib <- sapply(seq_along(merged_result$parent_dist), function(i) {
                        parent <- merged_result$parent_dist[[i]]
                        relsize <- merged_result$relsize[i]
                        contrib <- (1 - fitted_cdf(parent, thr))*relsize/(1 - gperc)
                        if (input$contrib_smoothing_choice == "yes") {
                            contrib <- movavg(contrib, 3)
                        }
                        return(contrib)
                    })
                    colnames(pop_contrib) <- sapply(seq_along(merged_result$parent_dist), function(i) {
                        return(merged_result$parent_dist[[i]]$country)
                    })

                    df <- data.frame(p=gperc, thr=thr)
                    df <- cbind(df, pop_contrib)

                } else if ((display_choice == "bracket contributions") && (type_choice == "population")) {

                    pop_contrib <- sapply(seq_along(merged_result$parent_dist), function(i) {
                        parent <- merged_result$parent_dist[[i]]
                        relsize <- merged_result$relsize[i]
                        contrib <- diff(c(fitted_cdf(parent, thr), 1))*relsize/diff(c(gperc, 1))
                        if (input$contrib_smoothing_choice == "yes") {
                            contrib <- movavg(contrib, 3)
                        }
                        return(contrib)
                    })
                    colnames(pop_contrib) <- sapply(seq_along(merged_result$parent_dist), function(i) {
                        return(merged_result$parent_dist[[i]]$country)
                    })

                    df <- data.frame(p=gperc, thr=thr)
                    df <- cbind(df, pop_contrib)

                } else if ((display_choice == "top contributions") && (type_choice == "income or wealth")) {

                    pop_contrib <- sapply(seq_along(merged_result$parent_dist), function(i) {
                        parent <- merged_result$parent_dist[[i]]
                        relsize <- merged_result$relsize[i]*parent$average/merged_result$average
                        contrib <- threshold_share(parent, thr)*relsize/merged_table$top_share
                        if (input$contrib_smoothing_choice == "yes") {
                            contrib <- movavg(contrib, 3)
                        }
                        return(contrib)
                    })
                    colnames(pop_contrib) <- sapply(seq_along(merged_result$parent_dist), function(i) {
                        return(merged_result$parent_dist[[i]]$country)
                    })

                    df <- data.frame(p=gperc, thr=thr)
                    df <- cbind(df, pop_contrib)

                } else if ((display_choice == "bracket contributions") && (type_choice == "income or wealth")) {

                    pop_contrib <- sapply(seq_along(merged_result$parent_dist), function(i) {
                        parent <- merged_result$parent_dist[[i]]
                        relsize <- merged_result$relsize[i]*parent$average/merged_result$average
                        contrib <- diff(c(threshold_share(parent, thr), 0))*relsize/diff(c(merged_table$top_share, 0))
                        if (input$contrib_smoothing_choice == "yes") {
                            contrib <- movavg(contrib, 3)
                        }
                        return(contrib)
                    })
                    colnames(pop_contrib) <- sapply(seq_along(merged_result$parent_dist), function(i) {
                        return(merged_result$parent_dist[[i]]$country)
                    })

                    df <- data.frame(p=gperc, thr=thr)
                    df <- cbind(df, pop_contrib)

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
                addDataFrame(df, sheet, row.names=FALSE)
                # Call Java garbage collector
                jgc()
            }
        }

        # Save the workbook
        saveWorkbook(wb, dest)
    }
)


