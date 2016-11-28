library(shiny)
library(shinyjs)
library(gpinter)
library(evaluate)
library(xlsx)

# Parse the user input data into a suitable R list
parse_input <- function(data, filename, sheetname=NULL) {
    data_list <- list()

    # For Excel files, combine the file and sheet names
    if (!is.null(sheetname)) {
        filename <- paste0(filename, "!", sheetname)
    }
    data_list$filename <- filename

    # Look for percentiles and use that column to identify and remove empty rows
    if ("p" %in% colnames(data)) {
        data <- data[!is.na(data$p), ]
        data_list$p <- data$p
    } else {
        return(simpleError(paste0(
            "'p' is missing in ", filename, ". See help for more information."
        )))
    }

    # Look for the label: if none, use the file and sheet name instead
    if ("label" %in% colnames(data)) {
        data_list$label <- data$label[1]
    } else {
        data_list$label <- filename
    }

    # Look for merge ID
    if ("mergeid" %in% colnames(data)) {
        data_list$mergeid <- data$mergeid[1]
    } else {
        data_list$mergeid <- NA
    }

    # Look for add up ID
    if ("mergeid" %in% colnames(data)) {
        data_list$addupid <- data$addupid[1]
    } else {
        data_list$addupid <- NA
    }

    # Look for the population size
    if ("popsize" %in% colnames(data)) {
        data_list$popsize <- data$popsize[1]
    } else {
        data_list$popsize <- NA
    }

    # Look for the average
    if ("average" %in% colnames(data)) {
        if (is.na(data$average[1])) {
            return(simpleError(paste0(
                "'average' is missing in ", filename, ". See help for for more information."
            )))
        }
        data_list$average <- data$average[1]
    } else {
        return(simpleError(paste0(
            "'average' is missing in ", filename, ". See help for for more information."
        )))
    }

    # Look for the thresholds
    if ("threshold" %in% colnames(data)) {
        if (anyNA(data$threshold)) {
            return(simpleError(paste0(
                "'threshold' should not contain missing values in ", filename, "."
            )))
        }
        data_list$threshold <- data$threshold
    }

    # Look for the averages/shares
    if ("bracketshare" %in% colnames(data)) {
        if (anyNA(data$bracketshare)) {
            return(simpleError(paste0(
                "'bracketshare' should not contain missing values in ", filename, "."
            )))
        }
        data_list$whichavgsh <- "bracketshare"
        data_list$bracketshare <- data$bracketshare
    } else if ("topshare" %in% colnames(data)) {
        if (anyNA(data$topshare)) {
            return(simpleError(paste0(
                "'topshare' should not contain missing values in ", filename, "."
            )))
        }
        data_list$whichavgsh <- "topshare"
        data_list$topshare <- data$topshare
    } else if ("bracketavg" %in% colnames(data)) {
        if (anyNA(data$bracketavg)) {
            return(simpleError(paste0(
                "'bracketavg' should not contain missing values in ", filename, "."
            )))
        }
        data_list$whichavgsh <- "bracketavg"
        data_list$bracketavg <- data$bracketavg
    } else if ("topavg" %in% colnames(data)) {
        if (anyNA(data$topavg)) {
            return(simpleError(paste0(
                "'topavg' should not contain missing values in ", filename, "."
            )))
        }
        data_list$whichavgsh <- "topavg"
        data_list$topavg <- data$topavg
    } else if ("invpareto" %in% colnames(data)) {
        if (anyNA(data$invpareto)) {
            return(simpleError(paste0(
                "'invpareto' should not contain missing values in ", filename, "."
            )))
        }
        data_list$whichavgsh <- "invpareto"
        data_list$invpareto <- data$invpareto
    }
    return(data_list)
}

shinyServer(function(input, output, session) {

    # Import the input data
    input_data <- reactive({
        if (is.null(input$file_input)) {
            # If the user hasn't specified any file yet, just return NULL
            return(NULL)
        } else {
            # Number of files to import
            nfiles <- length(input$file_input$name)
            # Keep a list of labels to ensure no duplicates
            used_labels <- c()
            # This list will contain all the parsed user input data
            data <- NULL
            for (i in 1:nfiles) {
                filename <- input$file_input$name[i]
                filepath <- input$file_input$datapath[i]
                # Identify the type of file
                extension <- tail(strsplit(filename, ".", fixed=TRUE)[[1]], n=1)
                if (extension %in% c("csv", "tsv", "txt")) {
                    # Try reading the CSV file, or return the error message
                    table <- tryCatch(suppressWarnings(read.csv(filepath,
                        header = TRUE,
                        stringsAsFactors = FALSE,
                        sep = isolate(input$csv_input_field_separator),
                        quote = isolate(input$csv_input_quote),
                        dec = isolate(input$csv_input_dec_separator),
                        colClasses = list(
                            "p"             = "numeric",
                            "threshold"     = "numeric",
                            "average"       = "numeric",
                            "bracketshare"  = "numeric",
                            "topshare"      = "numeric",
                            "bracketavg"    = "numeric",
                            "topavg"        = "numeric",
                            "invpareto"     = "numeric",
                            "popsize"       = "numeric",
                            "bracketsingle" = "numeric",
                            "topsingle"     = "numeric",
                            "label"         = "character"
                        )
                    )), error = function(e) {
                        return(simpleError(paste0(
                            "The program encountered the following error while reading ",
                            filename, ": ", e$message, ". Please check the format of your file. ",
                            "You can change the specification of the CSV format in ",
                            "the “Options” panel."
                        )))
                    })
                    # If there is any error, abandon data import and return the
                    # error message.
                    if (is.error(table)) {
                        return(table)
                    }
                    parsed_input <- parse_input(table, filename)
                    if (is.error(parsed_input)) {
                        return(parsed_input)
                    }
                    data <- c(data, list(parsed_input))
                    label <- as.character(parsed_input$label)
                    if (label %in% used_labels) {
                        return(simpleError(paste0(
                            "You cannot have more than one tabulation with the label ",
                            label, "."
                        )))
                    }
                    used_labels <- c(used_labels, label)
                } else if (extension %in% c("xls", "xlsx")) {
                    # First, list the sheets of the Excel file
                    sheets <- tryCatch(names(getSheets(loadWorkbook(filepath))), error = function(e) {
                        return(simpleError(paste0(
                            "The program encountered the following error while reading ",
                            filename, ": ", e$message, ". Please check the format of your file."
                        )))
                    })
                    # If the Excel file can't be read, return an error
                    if (is.error(sheets)) {
                        return(sheets)
                    }
                    # Then, loop over the sheets of the Excel file and import them
                    # one by one
                    for (sh in sheets) {
                        table <- tryCatch(read.xlsx(filepath, sheetName=sh), error = function(e) {
                            return(simpleError(paste0(
                                "The program encountered the following error while reading ",
                                "the sheet ", sh, " of ", filename, ": ", e$message, ". Please check the ",
                                "format of your file."
                            )))
                        })
                        # If the sheet can't be read, return an error
                        if (is.error(table)) {
                            return(table)
                        }
                        parsed_input <- parse_input(table, filename, sh)
                        if (is.error(parsed_input)) {
                            return(parsed_input)
                        }

                        data <- c(data, list(parsed_input))
                        label <- as.character(parsed_input$label)
                        if (label %in% used_labels) {
                            return(simpleError(paste0(
                                "You cannot have more than one tabulation with the label ",
                                label, "."
                            )))
                        }
                        used_labels <- c(used_labels, label)
                    }
                } else {
                    return(simpleError(paste0(
                        "The program cannot recognize the file format '", extension,
                        "'. Please use CSV or Excel files."
                    )))
                }
            }
            return(data)
        }
    })

    output$input_tabs <- renderUI({
        if (is.null(input_data())) {
            disable("run")
            return(tags$div(icon("exclamation-circle"), HTML("&nbsp;"),
                "Your input data will appear here once you have imported them.",
                class="alert alert-info", role="alert"
            ))
        } else if (is.error(input_data())) {
            disable("run")
            return(tags$div(icon("exclamation-circle"), HTML("&nbsp;"),
                input_data()$message,
                class="alert alert-danger", role="alert"
            ))
        } else {
            enable("run")
            # Create a tab for each inputfile
            input_tabs <- list()
            for (i in 1:length(input_data())) {
                input_tabs[[i]] <- tabPanel(input_data()[[i]]$label,
                    tags$h4("Summary"),
                    tags$table(
                        tags$tbody(
                            tags$tr(
                                tags$th("file name", style="white-space: nowrap;"),
                                tags$td(input_data()[[i]]$filename, style="width: 100%;")
                            ),
                            tags$tr(
                                tags$th("average", style="white-space: nowrap;"),
                                tags$td(sprintf("%.2f", input_data()[[i]]$average))
                            ),
                            tags$tr(
                                tags$th("population size", style="white-space: nowrap;"),
                                tags$td(ifelse(is.na(input_data()[[i]]$popsize),
                                    "n/a",
                                    input_data()[[i]]$popsize
                                ), style = ifelse(
                                    is.na(input_data()[[i]]$popsize),
                                    "color: #999;", ""
                                ))
                            ),
                            tags$tr(
                                tags$th("merge ID", style="white-space: nowrap;"),
                                tags$td(ifelse(is.na(input_data()[[i]]$mergeid),
                                    "n/a",
                                    input_data()[[i]]$mergeid
                                ), style = ifelse(
                                    is.na(input_data()[[i]]$mergeid),
                                    "color: #999;", ""
                                ))
                            ),
                            tags$tr(
                                tags$th("add up ID", style="white-space: nowrap;"),
                                tags$td(ifelse(is.na(input_data()[[i]]$addupid),
                                    "n/a",
                                    input_data()[[i]]$addupid
                                ), style = ifelse(
                                    is.na(input_data()[[i]]$addupid),
                                    "color: #999;", ""
                                ))
                            )
                        ),
                        class = "table table-condensed table-striped"
                    ),
                    tags$h4("Tabulation"),
                    tableOutput(paste0("input_table_", i))
                )
            }
            return(do.call(tabsetPanel, input_tabs))
        }
    })

    # Create the input data tables
    observe({
        if (!is.null(input_data()) & !is.error(input_data())) {
            lapply(1:length(input_data()), function(i) {
                # Average/share variable for this data
                avgsh <- input_data()[[i]]$whichavgsh
                df <- data.frame(
                    "p" = sprintf("%1.5f", input_data()[[i]]$p),
                    "threshold" = sprintf("%.0f", input_data()[[i]]$threshold)
                )
                df[avgsh] <- sprintf("%.3f", input_data()[[i]][[avgsh]])
                output[[paste0("input_table_", i)]] <- renderTable(df,
                    striped = TRUE,
                    width = "100%"
                )
            })
        }
    })

    # Make a reactive variable with the model's results
    results <- NULL
    makeReactiveBinding("results")

    # Launch the programs when the user clicks the "Run" button
    observeEvent(input$run, {
        # Number of files to run trhough the program
        nfiles <- length(input_data())

        # Show a modal dialog with a custom progress bar
        showModal(modalDialog(
            tags$div(
                tags$h4(id="run-status", style="text-align: center;"),
                tags$hr(),
                tags$div(
                    tags$div(
                        id = "run-progress",
                        class = "progress-bar progress-bar-striped",
                        role = "progressbar",
                        `aria-valuenow` = "0",
                        `aria-valuemin` = "0",
                        `aria-valuemax` = nfiles,
                        style = "width: 0%"
                    ),
                    class = "progress"
                ),
                tags$div(
                    tags$p("The execution ended successfully. You can now explore
                        the different panels of the application with the results."),
                    tags$table(
                        tags$tr(
                            tags$td(tags$i(class="fa fa-table fa-2x", `aria-hidden`="true")),
                            tags$td(tags$p("The", tags$b("Tables"), "tab provides detailed tabulations
                                of your data. You can pick which shares, quantiles and averages
                                you want to include, and download the result to your computer."
                            ))
                        ),
                        tags$tr(
                            tags$td(tags$i(class="fa fa-area-chart fa-2x", `aria-hidden`="true")),
                            tags$td(tags$p("The", tags$b("Plots"), "tab provides several plots to visualize
                                the distribution of your data. That includes the interpolated function,
                                but also the density or the Lorenz curve. The interface lets you
                                adjust the range to focus on specific parts of the distribution."
                            ))
                        ),
                        tags$tr(
                            tags$td(tags$i(class="fa fa-random fa-2x", `aria-hidden`="true")),
                            tags$td(tags$p("The", tags$b("Simulate"), "tab lets you simulate and download
                                new random samples of arbitrary size according to distribution
                                of your data."
                            ))
                        ),
                        tags$tr(
                            tags$td(tags$i(class="fa fa-stethoscope fa-2x", `aria-hidden`="true")),
                            tags$td(tags$p("The", tags$b("Diagnostic"), "tab can help you identify pathological
                                features of your data which may indicate mistakes or inconsistencies."
                            ))
                        ),
                        class = "tabs-presentation"
                    ),
                    id = "success_message"
                ),
                tags$div(
                    tags$p(id="error_message1"),
                    tags$div(
                        tags$p(id="error_message2"),
                        class = "alert alert-danger",
                        role = "alert"
                    ),
                    id = "failure_message"
                ),
                actionButton("dismiss_run_success", label="Dismiss", icon=icon("check"),
                    width="100%", class="btn-success"),
                actionButton("dismiss_run_failure", label="Dismiss", icon=icon("ban"),
                    width="100%"),
                style = "padding: 5px 20px 20px 20px;"
            ),
            title = NULL,
            footer = NULL
        ))

        # Only show the different messages once the execution is over
        shinyjs::hide("dismiss_run_success")
        shinyjs::hide("dismiss_run_failure")
        shinyjs::hide("success_message")
        shinyjs::hide("failure_message")

        # Clear the previous results
        results <<- list()
        # Run the program on each file
        for (i in 1:nfiles) {
            data <- input_data()[[i]]
            # Update the status message in the dialog
            shinyjs::runjs(paste0("$('#run-status').html('<i class=\"fa fa-cog fa-spin fa-fw\"></i> ",
                "Currently working on: ", data$label, "')"))

            # Run the program on the current data
            results[[i]] <<- tryCatch({
                args <- list(
                    p = data$p,
                    threshold = data$threshold,
                    average = data$average
                )
                avgsh <- data$whichavgsh
                args[avgsh] <- data[avgsh]
                result <- do.call(tabulation_fit, args)
                result$label <- data$label
                result
            }, error = function(e) {
                return(simpleError(e$message))
            })

            # If the program failed stop and show the error to the use
            if (is.error(results[[i]])) {
                # Show the error to the user
                shinyjs::show("failure_message")
                shinyjs::show("dismiss_run_failure")
                shinyjs::runjs(paste0("$('#run-status').html('<i class=\"fa fa-frown-o\" aria-hidden=\"true\"></i> Something went wrong.')"))

                shinyjs::runjs(paste0("$('#error_message1').text('An error occurred while working on ", data$label, ". ",
                    "Please check the consistency of your data.')"))
                shinyjs::runjs(paste0("$('#error_message2').html('<i class=\"fa fa-exclamation-circle\" aria-hidden=\"true\"></i> &nbsp;",
                    results[[i]]$message, "')"))

                # Clear the results
                results <<- NULL

                return(NULL)
            }

            # Update the progress bar
            shinyjs::runjs(paste0("$('#run-progress').attr('aria-valuenow',", i, ")"))
            shinyjs::runjs(paste0("$('#run-progress').attr('style', 'width: ", 100*i/nfiles, "%')"))
        }

        # Update the status message to show success
        shinyjs::runjs(paste0("$('#run-status').html('<i class=\"fa fa-thumbs-up\" aria-hidden=\"true\"></i> All done!')"))

        shinyjs::show("success_message")
        shinyjs::show("dismiss_run_success")
    })

    observeEvent(input$dismiss_run_success, {
        removeModal()
    })

    observeEvent(input$dismiss_run_failure, {
        removeModal()
    })

    # Generate the "Tables" panel
    output$results_tabs <- renderUI({
        if (is.null(results)) {
            disable("dl_tables_csv")
            disable("dl_tables_excel")
            return(tags$div(icon("info-circle"), HTML("&nbsp;"),
                "The results will appear here once the programs have been successfully executed.",
                class="alert alert-info", role="alert"))
        } else {
            # Enable download buttons
            enable("dl_tables_csv")
            enable("dl_tables_excel")
            results_tabs <- lapply(seq_along(results), function(i) {
                return(tabPanel(results[[i]]$label,
                    tableOutput(paste0("table_gperc_", i))
                ))
            })
            return(do.call(tabsetPanel, results_tabs))
        }
    })

    # Fill tables in the table panel
    observe({
        if (!is.null(results)) {
            # Create the list of percentiles we want to show the user
            gperc <- c(
                seq(0, 0.99, 0.01), seq(0.991, 0.999, 0.001),
                seq(0.9991, 0.9999, 0.0001), seq(0.99991, 0.99999, 0.00001)
            )

            lapply(seq_along(results), function(i) {
                out_df <- data.frame("Percentiles" = sprintf("%1.5f", gperc))

                if ("thres" %in% input$results_display) {
                    col <- fitted_quantile(results[[i]], gperc)
                    out_df["Threshold"] <- ifelse(is.na(col), NA, sprintf("%.0f", col))
                }
                if ("topshare" %in% input$results_display) {
                    col <- top_share(results[[i]], gperc)
                    out_df["Top share"] <- ifelse(is.na(col), NA, sprintf("%.2f%%", 100*col))
                }
                if ("bottomshare" %in% input$results_display) {
                    col <- bottom_share(results[[i]], gperc)
                    out_df["Bottom share"] <- ifelse(is.na(col), NA, sprintf("%.2f%%", 100*col))
                }
                if ("bracketshare" %in% input$results_display) {
                    col <- bracket_share(results[[i]], gperc, c(gperc, 1)[2:(length(gperc) + 1)])
                    out_df["Bracket share"] <- ifelse(is.na(col), NA, sprintf("%.2f%%", 100*col))
                }
                if ("topavg" %in% input$results_display) {
                    col <- top_average(results[[i]], gperc)
                    out_df["Top average"] <- ifelse(is.na(col), NA, sprintf("%.0f", col))
                }
                if ("bracketavg" %in% input$results_display) {
                    col <- bracket_average(results[[i]], gperc, c(gperc, 1)[2:(length(gperc) + 1)])
                    out_df["Bracket average"] <- ifelse(is.na(col), NA, sprintf("%.0f", col))
                }
                if ("invpareto" %in% input$results_display) {
                    col <- invpareto(results[[i]], gperc)
                    out_df["Inverted Pareto coefficient"] <- ifelse(is.na(col), NA, sprintf("%.2f", col))
                }

                output[[paste0("table_gperc_", i)]] <- renderTable(out_df,
                    striped = TRUE,
                    width = "100%",
                    na = "n/a"
                )
            })
        }
    })

    # Download handler for CSV
    output$dl_tables_csv <- downloadHandler(
        filename = function() {
            return(paste0("tables-", format.Date(Sys.time(), "%Y-%m-%d-%H-%M-%S"), ".zip"))
        },
        content = function(dest) {
            gperc <- c(
                seq(0, 0.99, 0.01), seq(0.991, 0.999, 0.001),
                seq(0.9991, 0.9999, 0.0001), seq(0.99991, 1, 0.00001)
            )
            # Create a file for each input
            tmp <- tempdir()
            indivfile <- sapply(seq_along(results), function(i) {
                out_df <- data.frame("Percentiles" = gperc)

                if ("thres" %in% input$results_display) {
                    out_df["Threshold"] <- fitted_quantile(results[[i]], gperc)
                }
                if ("topshare" %in% input$results_display) {
                    out_df["Top share"] <- top_share(results[[i]], gperc)
                }
                if ("bottomshare" %in% input$results_display) {
                    out_df["Bottom share"] <- bottom_share(results[[i]], gperc)
                }
                if ("bracketshare" %in% input$results_display) {
                    out_df["Bracket share"] <- bracket_share(results[[i]], gperc, c(gperc, 1)[2:(length(gperc) + 1)])
                }
                if ("topavg" %in% input$results_display) {
                    out_df["Top average"] <- top_average(results[[i]], gperc)
                }
                if ("bracketavg" %in% input$results_display) {
                    out_df["Bracket average"] <- bracket_average(results[[i]], gperc, c(gperc, 1)[2:(length(gperc) + 1)])
                }
                if ("invpareto" %in% input$results_display) {
                    out_df["Inverted Pareto coefficient"] <- invpareto(results[[i]], gperc)
                }

                filename <- paste0(tmp, "/", label, ".csv")
                write.table(out_df,
                    file = filename,
                    na = "",
                    row.names = FALSE,
                    sep = isolate(input$csv_output_field_separator),
                    dec = isolate(input$csv_output_dec_separator)
                )

                return(filename)
            })
            # Zip the files to destination
            zip(dest, indivfile, flags="-r9Xj")
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
                seq(0.9991, 0.9999, 0.0001), seq(0.99991, 1, 0.00001)
            )
            # Create the workbook
            wb <- createWorkbook()
            sapply(seq_along(results), function(i) {
                out_df <- data.frame("Percentiles" = gperc)

                if ("thres" %in% input$results_display) {
                    out_df["Threshold"] <- fitted_quantile(results[[i]], gperc)
                }
                if ("topshare" %in% input$results_display) {
                    out_df["Top share"] <- top_share(results[[i]], gperc)
                }
                if ("bottomshare" %in% input$results_display) {
                    out_df["Bottom share"] <- bottom_share(results[[i]], gperc)
                }
                if ("bracketshare" %in% input$results_display) {
                    out_df["Bracket share"] <- bracket_share(results[[i]], gperc, c(gperc, 1)[2:(length(gperc) + 1)])
                }
                if ("topavg" %in% input$results_display) {
                    out_df["Top average"] <- top_average(results[[i]], gperc)
                }
                if ("bracketavg" %in% input$results_display) {
                    out_df["Bracket average"] <- bracket_average(results[[i]], gperc, c(gperc, 1)[2:(length(gperc) + 1)])
                }
                if ("invpareto" %in% input$results_display) {
                    out_df["Inverted Pareto coefficient"] <- invpareto(results[[i]], gperc)
                }

                sheet <- createSheet(wb, label)
                addDataFrame(out_df, sheet, row.names=FALSE)
            })

            saveWorkbook(wb, dest)
        }
    )

    # Generate the Lorenz curves
    output$plots_tabs_lorenz <- renderUI({
        if (is.null(results)) {
            return(tags$div(icon("info-circle"), HTML("&nbsp;"),
                "The results will appear here once the programs have been successfully executed.",
                class="alert alert-info", role="alert"))
        } else {
            plot_lorenz_tabs <- lapply(seq_along(results), function(i) {
                return(tabPanel(results[[i]]$label,
                    tags$div(
                        plotOutput(paste0("lorenz_plot_", i)),
                        style = "margin: 20px 20px 0 0;"
                    ),
                    sliderInput(paste0("lorenz_range_", i),
                        min = 0,
                        max = 1,
                        value = c(0, 1),
                        step = 0.01,
                        label = NULL,
                        width = "100%"
                    )
                ))
            })
            return(do.call(tabsetPanel, plot_lorenz_tabs))
        }
    })

    observe({
        if (!is.null(results)) {
            lapply(seq_along(results), function(i) {
                if (!is.null(input[[paste0("lorenz_range_", i)]])) {
                    pmin <- min(input[[paste0("lorenz_range_", i)]])
                    pmax <- max(input[[paste0("lorenz_range_", i)]])

                    output[[paste0("lorenz_plot_", i)]] <- renderPlot({
                        plot_lorenz(results[[i]], xlim=c(pmin, pmax))
                    })
                }
            })
        }
    })

    # Generate the generalized Pareto curves
    output$plots_tabs_gpc <- renderUI({
        if (is.null(results)) {
            return(tags$div(icon("info-circle"), HTML("&nbsp;"),
                "The results will appear here once the programs have been successfully executed.",
                class="alert alert-info", role="alert"))
        } else {
            plot_gpc_tabs <- lapply(seq_along(results), function(i) {
                return(tabPanel(results[[i]]$label,
                    tags$div(
                        plotOutput(paste0("gpc_plot_", i)),
                        style = "margin: 20px 20px 0 0;"
                    ),
                    sliderInput(paste0("gpc_range_", i),
                        min = 0,
                        max = 1,
                        value = c(0.1, 1),
                        step = 0.01,
                        label = NULL,
                        width = "100%"
                    )
                ))
            })
            return(do.call(tabsetPanel, plot_gpc_tabs))
        }
    })

    observe({
        if (!is.null(results)) {
            lapply(seq_along(results), function(i) {
                if (!is.null(input[[paste0("gpc_range_", i)]])) {
                    pmin <- min(input[[paste0("gpc_range_", i)]])
                    pmax <- max(input[[paste0("gpc_range_", i)]])

                    output[[paste0("gpc_plot_", i)]] <- renderPlot({
                        plot_gpc(results[[i]], xlim=c(pmin, pmax))
                    })
                }
            })
        }
    })

    # Generate the density curves
    output$plots_tabs_pdf <- renderUI({
        if (is.null(results)) {
            return(tags$div(icon("info-circle"), HTML("&nbsp;"),
                "The results will appear here once the programs have been successfully executed.",
                class="alert alert-info", role="alert"))
        } else {
            plot_pdf_tabs <- lapply(seq_along(results), function(i) {
                supp <- support(results[[i]])
                if (is.infinite(supp$lower)) {
                    xmin <- floor(fitted_quantile(results[[i]], 1e-4))
                } else {
                    xmin <- floor(supp$lower)
                }
                if (is.infinite(supp$upper)) {
                    xmax <- ceiling(fitted_quantile(results[[i]], 1 - 1e-2))
                } else {
                    xmax <- ceiling(supp$upper)
                }
                return(tabPanel(results[[i]]$label,
                    tags$div(
                        plotOutput(paste0("pdf_plot_", i)),
                        style = "margin: 20px 20px 0 0;"
                    ),
                    sliderInput(paste0("pdf_range_", i),
                        min = xmin,
                        max = xmax,
                        value = c(xmin, xmax),
                        step = 1,
                        label = NULL,
                        width = "100%"
                    )
                ))
            })
            return(do.call(tabsetPanel, plot_pdf_tabs))
        }
    })

    observe({
        if (!is.null(results)) {
            lapply(seq_along(results), function(i) {
                if (!is.null(input[[paste0("pdf_range_", i)]])) {
                    xmin <- min(input[[paste0("pdf_range_", i)]])
                    xmax <- max(input[[paste0("pdf_range_", i)]])

                    output[[paste0("pdf_plot_", i)]] <- renderPlot({
                        plot_density(results[[i]], xlim=c(xmin, xmax))
                    })
                }
            })
        }
    })

    # Generate the CDF curves
    output$plots_tabs_cdf <- renderUI({
        if (is.null(results)) {
            return(tags$div(icon("info-circle"), HTML("&nbsp;"),
                "The results will appear here once the programs have been successfully executed.",
                class="alert alert-info", role="alert"))
        } else {
            plot_cdf_tabs <- lapply(seq_along(results), function(i) {
                supp <- support(results[[i]])
                if (is.infinite(supp$lower)) {
                    xmin <- floor(fitted_quantile(results[[i]], 1e-4))
                } else {
                    xmin <- floor(supp$lower)
                }
                if (is.infinite(supp$upper)) {
                    xmax <- ceiling(fitted_quantile(results[[i]], 1 - 1e-2))
                } else {
                    xmax <- ceiling(supp$upper)
                }
                return(tabPanel(results[[i]]$label,
                    tags$div(
                        plotOutput(paste0("cdf_plot_", i)),
                        style = "margin: 20px 20px 0 0;"
                    ),
                    sliderInput(paste0("cdf_range_", i),
                        min = xmin,
                        max = xmax,
                        value = c(xmin, xmax),
                        step = 1,
                        label = NULL,
                        width = "100%"
                    )
                ))
            })
            return(do.call(tabsetPanel, plot_cdf_tabs))
        }
    })

    observe({
        if (!is.null(results)) {
            lapply(seq_along(results), function(i) {
                if (!is.null(input[[paste0("cdf_range_", i)]])) {
                    xmin <- min(input[[paste0("cdf_range_", i)]])
                    xmax <- max(input[[paste0("cdf_range_", i)]])

                    output[[paste0("cdf_plot_", i)]] <- renderPlot({
                        plot_cdf(results[[i]], xlim=c(xmin, xmax))
                    })
                }
            })
        }
    })

    # Generate the quantile curves
    output$plots_tabs_quantile <- renderUI({
        if (is.null(results)) {
            return(tags$div(icon("info-circle"), HTML("&nbsp;"),
                "The results will appear here once the programs have been successfully executed.",
                class="alert alert-info", role="alert"))
        } else {
            plot_quantile_tabs <- lapply(seq_along(results), function(i) {
                return(tabPanel(results[[i]]$label,
                    tags$div(
                        plotOutput(paste0("quantile_plot_", i)),
                        style = "margin: 20px 20px 0 0;"
                    ),
                    sliderInput(paste0("quantile_range_", i),
                        min = 0,
                        max = 1,
                        value = c(0.05, 0.95),
                        step = 0.01,
                        label = NULL,
                        width = "100%"
                    )
                ))
            })
            return(do.call(tabsetPanel, plot_quantile_tabs))
        }
    })

    observe({
        if (!is.null(results)) {
            lapply(seq_along(results), function(i) {
                if (!is.null(input[[paste0("quantile_range_", i)]])) {
                    xmin <- min(input[[paste0("quantile_range_", i)]])
                    xmax <- max(input[[paste0("quantile_range_", i)]])

                    output[[paste0("quantile_plot_", i)]] <- renderPlot({
                        plot_quantile(results[[i]], xlim=c(xmin, xmax))
                    })
                }
            })
        }
    })

    # Generate the tail function curves
    output$plots_tabs_tail <- renderUI({
        if (is.null(results)) {
            return(tags$div(icon("info-circle"), HTML("&nbsp;"),
                "The results will appear here once the programs have been successfully executed.",
                class="alert alert-info", role="alert"))
        } else {
            plot_tail_tabs <- lapply(seq_along(results), function(i) {
                return(tabPanel(results[[i]]$label,
                    tags$div(
                        plotOutput(paste0("tail_plot_", i)),
                        style = "margin: 20px 20px 0 0;"
                    ),
                    sliderInput(paste0("tail_range_", i),
                        min = 0,
                        max = 10,
                        value = c(3, 7),
                        step = 0.1,
                        label = NULL,
                        width = "100%"
                    )
                ))
            })
            return(do.call(tabsetPanel, plot_tail_tabs))
        }
    })

    observe({
        if (!is.null(results)) {
            lapply(seq_along(results), function(i) {
                if (!is.null(input[[paste0("tail_range_", i)]])) {
                    xmin <- min(input[[paste0("tail_range_", i)]])
                    xmax <- max(input[[paste0("tail_range_", i)]])

                    output[[paste0("tail_plot_", i)]] <- renderPlot({
                        plot_tail(results[[i]], xlim=c(xmin, xmax))
                    })
                }
            })
        }
    })

    # Generate the interpolation function curves
    output$plots_tabs_phi <- renderUI({
        if (is.null(results)) {
            return(tags$div(icon("info-circle"), HTML("&nbsp;"),
                "The results will appear here once the programs have been successfully executed.",
                class="alert alert-info", role="alert"))
        } else {
            plot_phi_tabs <- lapply(seq_along(results), function(i) {
                return(tabPanel(results[[i]]$label,
                    tags$div(
                        plotOutput(paste0("phi_plot_", i)),
                        style = "margin: 20px 20px 0 0;"
                    ),
                    sliderInput(paste0("phi_range_", i),
                        min = 0,
                        max = 10,
                        value = c(0, 7),
                        step = 0.1,
                        label = NULL,
                        width = "100%"
                    )
                ))
            })
            return(do.call(tabsetPanel, plot_phi_tabs))
        }
    })

    observe({
        if (!is.null(results)) {
            lapply(seq_along(results), function(i) {
                if (!is.null(input[[paste0("phi_range_", i)]])) {
                    xmin <- min(input[[paste0("phi_range_", i)]])
                    xmax <- max(input[[paste0("phi_range_", i)]])

                    output[[paste0("phi_plot_", i)]] <- renderPlot({
                        plot_phi(results[[i]], xlim=c(xmin, xmax))
                    })
                }
            })
        }
    })

    # Generate the curves for the derivative of the interpolation function
    output$plots_tabs_deriv_phi <- renderUI({
        if (is.null(results)) {
            return(tags$div(icon("info-circle"), HTML("&nbsp;"),
                "The results will appear here once the programs have been successfully executed.",
                class="alert alert-info", role="alert"))
        } else {
            plot_deriv_phi_tabs <- lapply(seq_along(results), function(i) {
                return(tabPanel(results[[i]]$label,
                    tags$div(
                        plotOutput(paste0("deriv_phi_plot_", i)),
                        style = "margin: 20px 20px 0 0;"
                    ),
                    sliderInput(paste0("deriv_phi_range_", i),
                        min = 0,
                        max = 10,
                        value = c(0, 7),
                        step = 0.1,
                        label = NULL,
                        width = "100%"
                    )
                ))
            })
            return(do.call(tabsetPanel, plot_deriv_phi_tabs))
        }
    })

    observe({
        if (!is.null(results)) {
            lapply(seq_along(results), function(i) {
                if (!is.null(input[[paste0("deriv_phi_range_", i)]])) {
                    xmin <- min(input[[paste0("deriv_phi_range_", i)]])
                    xmax <- max(input[[paste0("deriv_phi_range_", i)]])

                    output[[paste0("deriv_phi_plot_", i)]] <- renderPlot({
                        plot_deriv_phi(results[[i]], xlim=c(xmin, xmax))
                    })
                }
            })
        }
    })

    # Select input for which distribution to simulate
    output$synthpop_select_file <- renderUI({
        if (!is.null(results)) {
            enable("synthpop_dl_csv")
            enable("synthpop_dl_excel")
            files <- list()
            files[["All"]] <- 0
            for (i in seq_along(results)) {
                files[[as.character(results[[i]]$label)]] <- i
            }
            return(selectInput("synthpop_file", "Choose file(s)", files, 0, width="100%"))
        } else {
            return(disabled(
                selectInput("synthpop_file", "Choose file(s)",
                    c("–"), "–", width="100%")
            ))
        }
    })

    output$synthpop_dl_csv <- downloadHandler(
        filename = function() {
            return(paste0("simulation-", format.Date(Sys.time(), "%Y-%m-%d-%H-%M-%S"), ".csv"))
        },
        content = function(dest) {
            i <- isolate(input$synthpop_file)
            if (i == 0) {
                data <- as.data.frame(sapply(results, function(res) {
                    return(simulate_gpinter(res, isolate(input$synthpop_size)))
                }))
                colnames(data) <- sapply(results, function(res) res$label)
                write.table(data,
                    file = dest,
                    na = "",
                    row.names = FALSE,
                    sep = isolate(input$csv_output_field_separator),
                    dec = isolate(input$csv_output_dec_separator)
                )
            } else {
                res <- results[[as.numeric(i)]]
                data <- as.data.frame(simulate_gpinter(res, isolate(input$synthpop_size)))
                colnames(data) <- res$label
                write.table(data,
                    file = dest,
                    na = "",
                    row.names = FALSE,
                    sep = isolate(input$csv_output_field_separator),
                    dec = isolate(input$csv_output_dec_separator)
                )
            }
        }
    )

    output$synthpop_dl_excel <- downloadHandler(
        filename = function() {
            return(paste0("simulation-", format.Date(Sys.time(), "%Y-%m-%d-%H-%M-%S"), ".xlsx"))
        },
        content = function(dest) {
            i <- isolate(input$synthpop_file)
            if (i == 0) {
                data <- as.data.frame(sapply(results, function(res) {
                    return(simulate_gpinter(res, isolate(input$synthpop_size)))
                }))
                colnames(data) <- lapply(results, function(res) res$label)
                write.xlsx(data, dest, row.names=FALSE)
            } else {
                res <- results[[as.numeric(i)]]
                data <- as.data.frame(simulate_gpinter(res, isolate(input$synthpop_size)))
                colnames(data) <- res$label
                write.xlsx(data, dest, row.names=FALSE)
            }
        }
    )
})
