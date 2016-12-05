library(shiny)
library(shinyjs)
library(evaluate)
library(xlsx)
library(readxl)
library(gpinter)

# Parse the user input data into a suitable R list
parse_input <- function(data, var, dpcomma, filename, sheetname=NULL) {
    data_list <- list()

    # For Excel files, combine the file and sheet names
    if (!is.null(sheetname)) {
        filename <- paste0(filename, "!", sheetname)
    }
    data_list$filename <- filename

    # Identify the layout of the table: look at data[1, 2]. If it is a
    # variable name, then the layout is entirely in columns. Otherwise,
    # there are scalar variables in the top rows.
    if (!trimws(data[1, 2]) %in% var) {
        # Look for scalar variables in the top rows
        i <- 1
        while (i <= nrow(data)) {
            if (trimws(data[i, 1]) == var$average) {
                data_list$average <- as.numeric(data[i, 2])
            } else if (trimws(data[i, 1]) == var$label) {
                data_list$label <- data[i, 2]
            } else if (trimws(data[i, 1]) == var$mergeid) {
                data_list$mergeid <- data[i, 2]
            } else if (trimws(data[i, 1]) == var$addupid) {
                data_list$addupid <- data[i, 2]
            } else if (trimws(data[i, 1]) == var$popsize) {
                data_list$popsize <- as.numeric(data[i, 2])
            } else {
                # We've reached the end of the top rows
                break
            }
            i <- i + 1
        }
        # Remove the top rows and analyse the rest of table
        names <- sapply(data[i, ], trimws)
        data <- data[-c(1:i), ]
        colnames(data) <- names
    }

    # Look for percentiles and use that column to identify and remove empty rows
    if (var$p %in% colnames(data)) {
        if (dpcomma) {
            data[, var$p] <- gsub(",", ".", data[, var$p])
        }
        data[, var$p] <- as.numeric(data[, var$p])
        data <- data[!is.na(data[, var$p]), ]
        data_list$p <- data[, var$p]
    } else {
        return(simpleError("fractiles are missing"))
    }

    # Look for the label: if none, use the file and sheet name instead
    if (var$label %in% colnames(data)) {
        data_list$label <- data[1, var$label]
    } else if (is.na(data_list$label)) {
        data_list$label <- filename
    }

    # Look for merge ID
    if (var$mergeid %in% colnames(data)) {
        data_list$mergeid <- data[1, var$mergeid]
    }

    # Look for add up ID
    if (var$addupid %in% colnames(data)) {
        data_list$addupid <- data[1, var$addupid]
    }

    # Look for the population size
    if (var$popsize %in% colnames(data)) {
        if (dpcomma) {
            data[, var$popsize] <- gsub(",", ".", data[, var$popsize])
        }
        data[, var$popsize] <- as.numeric(data[, var$popsize])
        data_list$popsize <- data[1, var$popsize]
    }

    # Look for the average
    if (var$average %in% colnames(data)) {
        if (dpcomma) {
            data[, var$average] <- gsub(",", ".", data[, var$average])
        }
        data[, var$average] <- as.numeric(data[, var$average])
        data_list$average <- data[1, var$average]
    }

    # Look for the thresholds
    if (var$q %in% colnames(data)) {
        if (dpcomma) {
            data[, var$q] <- gsub(",", ".", data[, var$q])
        }
        data[, var$q] <- as.numeric(data[, var$q])
        if (anyNA(data[, var$q])) {
            return(simpleError("thresholds contain missing values"))
        }
        data_list$threshold <- data[, var$q]
    } else {
        return(simpleError("no data on thresholds"))
    }

    # Look for the averages/shares
    if (var$bracketshare %in% colnames(data)) {
        if (dpcomma) {
            data[, var$bracketshare] <- gsub(",", ".", data[, var$bracketshare])
        }
        data[, var$bracketshare] <- as.numeric(data[, var$bracketshare])
        if (anyNA(data[, var$bracketshare])) {
            return(simpleError("bracket shares contain missing values"))
        }
        data_list$whichavgsh <- "bracketshare"
        data_list$bracketshare <- data[, var$bracketshare]
    } else if (var$topshare %in% colnames(data)) {
        if (dpcomma) {
            data[, var$topshare] <- gsub(",", ".", data[, var$topshare])
        }
        data[, var$topshare] <- as.numeric(data[, var$topshare])
        if (anyNA(data[, var$topshare])) {
            return(simpleError("top shares contain missing values"))
        }
        data_list$whichavgsh <- "topshare"
        data_list$topshare <- data[, var$topshare]
    } else if (var$bracketavg %in% colnames(data)) {
        if (dpcomma) {
            data[, var$bracketavg] <- gsub(",", ".", data[, var$bracketavg])
        }
        data[, var$bracketavg] <- as.numeric(data[, var$bracketavg])
        if (anyNA(data[, var$bracketavg])) {
            return(simpleError("bracket averages contain missing values"))
        }
        data_list$whichavgsh <- "bracketavg"
        data_list$bracketavg <- data[, var$bracketavg]
    } else if (var$topavg %in% colnames(data)) {
        if (dpcomma) {
            data[, var$topavg] <- gsub(",", ".", data[, var$topavg])
        }
        data[, var$topavg] <- as.numeric(data[, var$topavg])
        if (anyNA(data[, var$topavg])) {
            return(simpleError("top averages contain missing values"))
        }
        data_list$whichavgsh <- "topavg"
        data_list$topavg <- data[, var$topavg]
    } else if (var$b %in% colnames(data)) {
        if (dpcomma) {
            data[, var$b] <- gsub(",", ".", data[, var$b])
        }
        data[, var$b] <- as.numeric(data[, var$b])
        if (anyNA(data[data_list$p != 0, var$b])) {
            return(simpleError("inverted Pareto coefficients contain missing values"))
        }
        data_list$whichavgsh <- "invpareto"
        data_list$invpareto <- data[, var$b]
    } else {
        return(simpleError("no data on shares/averages/inverted Pareto coefficients"))
    }


    if (is.na(data_list$average) | is.null(data_list$average)) {
        return(simpleError("average is missing"))
    }
    if (is.null(data_list$popsize)) {
        data_list$popsize <- NA
    }
    if (is.null(data_list$mergeid)) {
        data_list$mergeid <- NA
    }
    if (is.null(data_list$addupid)) {
        data_list$addupid <- NA
    }

    return(data_list)
}

shinyServer(function(input, output, session) {
    # Import the input data
    input_data <- reactive(withProgress({
            if (is.null(input$file_input)) {
                # If the user hasn't specified any file yet, just return NULL
                return(NULL)
            } else {
                # Retrieve variable names from the preference panel
                varnames <- list(
                    p             = trimws(isolate(input$var_p)),
                    q             = trimws(isolate(input$var_q)),
                    b             = trimws(isolate(input$var_b)),
                    bracketshare  = trimws(isolate(input$var_bracketshare)),
                    topshare      = trimws(isolate(input$var_topshare)),
                    bracketavg    = trimws(isolate(input$var_bracketavg)),
                    topavg        = trimws(isolate(input$var_topavg)),
                    bracketsingle = trimws(isolate(input$var_bracketsingle)),
                    topsingle     = trimws(isolate(input$var_topsingle)),
                    label         = trimws(isolate(input$var_label)),
                    average       = trimws(isolate(input$var_average)),
                    popsize       = trimws(isolate(input$var_popsize)),
                    gumbel        = trimws(isolate(input$var_gumbel)),
                    addupid       = trimws(isolate(input$var_addupid)),
                    mergeid       = trimws(isolate(input$var_mergeid))
                )

                # Number of files to import
                nfiles <- length(input$file_input$name)
                # Keep a list of labels to ensure no duplicates
                used_labels <- NULL
                # This list will contain all the parsed user input data
                data <- NULL
                for (i in 1:nfiles) {
                    # Update the progress bar
                    message <- paste0("reading “", input$file_input$name[i], "”")
                    setProgress(i - 1, message)

                    filename <- input$file_input$name[i]
                    filepath <- input$file_input$datapath[i]
                    # Identify the type of file
                    extension <- tail(strsplit(filename, ".", fixed=TRUE)[[1]], n=1)
                    if (extension %in% c("csv", "tsv", "txt")) {
                        # Try reading the CSV file, or return the error message
                        table <- tryCatch(suppressWarnings(read.csv(filepath,
                            header = FALSE,
                            stringsAsFactors = FALSE,
                            sep = isolate(input$csv_input_field_separator),
                            colClasses = "character"
                        )), error = function(e) {
                            return(simpleError(sprintf(
                                "“", filename, "” was ignored because of the following error: ", e$message, "."
                            )))
                        })
                        # In case of error, move on to the next file
                        if (is.error(table)) {
                            data <- c(data, list(table))
                            next
                        } else {
                            # Otherwise, parse the content of the file
                            dpcomma <- (isolate(input$csv_input_dec_separator) == ",")
                            parsed_input <- tryCatch(
                                parse_input(table, varnames, dpcomma, filename),
                                error = function(e) simpleError(e$message)
                            )
                            # If parsing was successful, make sure that the label is not a
                            # duplicate, and that it is different from "SUMMARY", which is already
                            # used by the program.
                            if (!is.error(parsed_input)) {
                                label <- as.character(parsed_input$label)
                                if (label %in% used_labels) {
                                    parsed_input <- simpleError(paste0(
                                        "“", label, "” was ignored because there is already a tabulation with the same label."
                                    ))
                                } else if (tolower(label) == "summary") {
                                    parsed_input <- simpleError(paste0(
                                        "“", label, "” was ignored because it is not a valid label."
                                    ))
                                } else {
                                    used_labels <- c(used_labels, label)
                                }
                            } else {
                                parsed_input <- simpleError(paste0(
                                    "“", filename, "” was ignored because of the following error: ",
                                    parsed_input$message, "."
                                ))
                            }
                            data <- c(data, list(parsed_input))
                        }
                        if (is.error(parsed_input)) {
                            data <- c(data, simpleError(paste0(
                                "“", filename, "” was ignored because of the following error: ", parsed_input$message,
                                "."
                            )))
                        } else {
                            data <- c(data, list(parsed_input))
                        }
                    } else if (extension %in% c("xls", "xlsx")) {
                        # Rename the file to use the proper extension (required by readxl)
                        newpath <- paste0(filepath, ".", extension)
                        file.rename(filepath, newpath)
                        filepath <- newpath
                        # First, list the sheets of the Excel file
                        sheets <- tryCatch(excel_sheets(filepath), error = function(e) {
                                return(simpleError(paste0(
                                "“", filename, "” was ignored because of the following error: ", e$message, "."
                            )))
                        })
                        # If the Excel file can't be read, move on to the next file
                        if (is.error(sheets)) {
                            data <- c(data, sheets)
                            next
                        }
                        # Otherwise, loop over the sheets of the Excel file and import them
                        # one by one
                        for (sh in sheets) {
                            table <- tryCatch(as.data.frame(read_excel(
                                filepath,
                                sheet = sh,
                                col_names = FALSE
                            )), error = function(e) {
                                return(simpleError(paste0(
                                    "The sheet “", sh, "” of “", filename, "” was ignored because of the ",
                                    "following error: ", e$message, "."
                                )))
                            })
                            incProgress(1/length(sheets), message)
                            # If the sheet can't be read, move on to the next
                            if (is.error(table)) {
                                data <- c(data, list(table))
                                next
                            } else {
                                # Otherwise, parse the content of the file
                                parsed_input <- tryCatch(
                                    parse_input(table, varnames, FALSE, filename, sh),
                                    error = function(e) simpleError(e$message)
                                )
                                # If parsing was successful, make sure that the label is not a
                                # duplicate, and that it is different from "__summary", which is already
                                # used by the program.
                                if (!is.error(parsed_input)) {
                                    label <- as.character(parsed_input$label)
                                    if (label %in% used_labels) {
                                        parsed_input <- simpleError(paste0(
                                            "“", label, "” was ignored because there is already a tabulation with the same label."
                                        ))
                                    } else if (tolower(label) == "__summary") {
                                        parsed_input <- simpleError(paste0(
                                            "“", label, "” was ignored because it is not a valid label."
                                        ))
                                    } else {
                                        used_labels <- c(used_labels, label)
                                    }
                                } else {
                                    parsed_input <- simpleError(paste0(
                                        "Sheet “", sh, "” of “", filename, "” was ignored because of the following error: ",
                                        parsed_input$message, "."
                                    ))
                                }
                                data <- c(data, list(parsed_input))
                            }
                        }
                    } else {
                        # Can't read the file: ignore
                        data <- c(data, simpleError(paste0(
                            "“", filename, "” was ignored because the format ‘", extension,
                            "’ is not supported."
                        )))
                    }
                }
                return(data)
            }
        },
        max = length(input$file_input$name),
        value = 0,
        message = paste0("reading “", input$file_input$name[1], "”")
    ))

    output$input_tabs <- renderUI({
        if (is.null(input_data())) {
            disable("run")
            return(tags$div(icon("info-circle"), HTML("&nbsp;"),
                "Your input data will appear here once you have imported them.",
                class="alert alert-info", role="alert"
            ))
        } else if (all(sapply(input_data(), is.error))) {
            disable("run")
            return(tags$div(
                    tags$ul(lapply(input_data(), function(e) {
                    tags$li(tags$i(class="fa fa-li fa-times-circle"), e$message)
                }), class="fa-ul"),
                class="alert alert-danger", role="alert"
            ))
        } else {
            enable("run")
            # List of tabs (for non error entries in input_data())
            input_tabs <- list()
            # List of error messages
            error_list <- list()
            # Create an index variable for non error entries in input_data()
            j <- 1
            # Create an index variable for error entries in input_data()
            k <- 1
            for (i in 1:length(input_data())) {
                if (is.error(input_data()[[i]])) {
                    error_list[[k]] <- tags$li(tags$i(class="fa fa-li fa-exclamation-triangle"), input_data()[[i]]$message)
                    k <- k + 1
                } else {
                    input_tabs[[j]] <- tabPanel(input_data()[[i]]$label,
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
                    j <- j + 1
                }

            }
            if (length(error_list) > 0) {
                return(tagList(
                    tags$div(
                        tags$ul(error_list, class="fa-ul"),
                        style = "max-height: 200px; overflow: scroll;",
                        class = "alert alert-warning",
                        role = "alert"
                    ),
                    do.call(tabsetPanel, c(input_tabs, type="pills"))
                ))
            } else {
                return(do.call(tabsetPanel, c(input_tabs, type="pills")))
            }
        }
    })

    # Create the input data tables
    observe({
        if (!is.null(input_data()) & !all(sapply(input_data(), is.error))) {
            lapply(1:length(input_data()), function(i) {
                if (!is.error(input_data()[[i]])) {
                    # Average/share variable for this data
                    avgsh <- input_data()[[i]]$whichavgsh
                    # Clean name for average/share variable
                    if (avgsh == "bracketshare") {
                        avgsh_clean <- "Bracket share"
                    } else if (avgsh == "topshare") {
                        avgsh_clean <- "Top share"
                    } else if (avgsh == "bracketavg") {
                        avgsh_clean <- "Bracket average"
                    } else if (avgsh == "topavg") {
                        avgsh_clean <- "Top average"
                    } else if (avgsh == "invpareto") {
                        avgsh_clean <- "Inverted Pareto coefficient"
                    }

                    df <- data.frame(
                        "Fractiles" = sprintf("%1.5f", input_data()[[i]]$p),
                        "Thresholds" = sprintf("%.0f", input_data()[[i]]$threshold)
                    )
                    df[avgsh_clean] <- sprintf("%.3f", input_data()[[i]][[avgsh]])
                    df[is.na(input_data()[[i]][[avgsh]]), avgsh_clean] <- NA
                    output[[paste0("input_table_", i)]] <- renderTable(df,
                        striped = TRUE,
                        width = "100%",
                        na = "n/a"
                    )
                }
            })
        }
    })

    # Make a reactive variable with the model's results
    results <- NULL
    makeReactiveBinding("results")

    # Launch the programs when the user clicks the "Run" button
    observeEvent(input$run, {
        # Number of files in total (incl. with errors)
        nfiles_total <- length(input_data())
        nfiles_error <- sum(sapply(input_data(), is.error))
        nfiles_valid <- nfiles_total - nfiles_error

        # Show a modal dialog with a custom progress bar
        showModal(modalDialog(
            tags$div(
                tags$h4(id="run_status", style="text-align: center;"),
                tags$hr(),
                tags$div(
                    tags$div(
                        id = "run_progress",
                        class = "progress-bar progress-bar-striped",
                        role = "progressbar",
                        `aria-valuenow` = "0",
                        `aria-valuemin` = "0",
                        `aria-valuemax` = nfiles_valid,
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

        shinyjs::addClass("run_progress", "active")

        # Clear the previous results
        results <<- list()
        # Run the program on each file
        j <- 1
        for (i in 1:nfiles_total) {
            data <- input_data()[[i]]
            if (!is.error(data)) {
                # Update the status message in the dialog
                shinyjs::runjs(paste0("$('#run_status').html('<i class=\"fa fa-cog fa-spin fa-fw\"></i> ",
                    "Currently working on: ", data$label, "')"))

                # Run the program on the current data
                results[[j]] <<- tryCatch({
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

                # If the program failed, stop and show the error to the user
                if (is.error(results[[j]])) {
                    # Show the error to the user
                    shinyjs::show("failure_message")
                    shinyjs::show("dismiss_run_failure")
                    shinyjs::runjs(paste0("$('#run_status').html('<i class=\"fa fa-frown-o\" aria-hidden=\"true\"></i> Something went wrong.')"))

                    shinyjs::runjs(paste0("$('#error_message1').text('An error occurred while working on ", data$label, ". ",
                        "Please check the consistency of your data.')"))
                    # Sanitize & display error message
                    msg <- results[[j]]$message
                    msg <- gsub("\n", "", msg)
                    msg <- gsub("'", "\\'", msg)
                    shinyjs::runjs(paste0("$('#error_message2').html('<i class=\"fa fa-exclamation-circle\" aria-hidden=\"true\"></i> &nbsp; ", msg, "')"))
                    shinyjs::removeClass("run_progress", "active")

                    # Clear the results
                    results <<- NULL

                    return(NULL)
                }

                # Update the progress bar
                shinyjs::runjs(paste0("$('#run_progress').attr('aria-valuenow',", j, ")"))
                shinyjs::runjs(paste0("$('#run_progress').attr('style', 'width: ", 100*j/nfiles_valid, "%')"))
                j <- j + 1
            }
        }

        # Update the status message to show success
        shinyjs::runjs(paste0("$('#run_status').html('<i class=\"fa fa-thumbs-up\" aria-hidden=\"true\"></i> All done!')"))
        shinyjs::removeClass("run_progress", "active")

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
                    tags$h4("Summary"),
                    tableOutput(paste0("table_summary_", i)),
                    tags$h4("Details"),
                    tableOutput(paste0("table_gperc_", i))
                ))
            })
            return(do.call(tabsetPanel, c(results_tabs, type="pills")))
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
                # Summary
                output[[paste0("table_summary_", i)]] <- renderTable(
                    data.frame(
                        "Average" = sprintf("%.0f", results[[i]]$average),
                        "Bottom 50%" = sprintf("%.1f%%", 100*bottom_share(results[[i]], 0.5)),
                        "Middle 40%" = sprintf("%.1f%%", 100*bracket_share(results[[i]], 0.5, 0.9)),
                        "Top 10%" = sprintf("%.1f%%", 100*top_share(results[[i]], 0.9)),
                        "Top 1%" = sprintf("%.1f%%", 100*top_share(results[[i]], 0.99)),
                        "Gini" = sprintf("%.3f", gini(results[[i]])),
                        check.names = FALSE
                    ),
                    striped = TRUE,
                    width = "100%"
                )

                # Detailed tabulation
                out_df <- data.frame("Percentiles" = sprintf("%1.5f", gperc))

                if ("thres" %in% input$results_display) {
                    col <- fitted_quantile(results[[i]], gperc)
                    out_df["Threshold"] <- ifelse(is.na(col), NA, sprintf("%.0f", col))
                    out_df[is.infinite(col), "Threshold"] <- "–∞"
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
                    out_df[is.infinite(col), "Inverted Pareto coefficient"] <- "∞"
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
                seq(0.9991, 0.9999, 0.0001), seq(0.99991, 0.99999, 0.00001)
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

                filename <- paste0(tmp, "/", results[[i]]$label, ".csv")
                write.table(out_df,
                    file = filename,
                    na = "",
                    row.names = FALSE,
                    sep = isolate(input$csv_output_field_separator),
                    dec = isolate(input$csv_output_dec_separator)
                )

                return(filename)
            })
            # Create the summary file
            filename_summary <- paste0(tmp, "/SUMMARY.csv")
            write.table(
                data.frame(
                    "Label"      = sapply(results, function(r) r$label),
                    "Average"    = sapply(results, function(r) r$average),
                    "Bottom 50%" = sapply(results, function(r) bottom_share(r, 0.5)),
                    "Middle 40%" = sapply(results, function(r) bracket_share(r, 0.5, 0.9)),
                    "Top 10%"    = sapply(results, function(r) top_share(r, 0.9)),
                    "Top 1%"     = sapply(results, function(r) top_share(r, 0.99)),
                    "Gini"       = sapply(results, function(r) gini(r)),
                    check.names = FALSE
                ),
                file = filename_summary,
                na = "",
                row.names = FALSE,
                sep = isolate(input$csv_output_field_separator),
                dec = isolate(input$csv_output_dec_separator)
            )
            indivfile <- c(filename_summary, indivfile)

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
                seq(0.9991, 0.9999, 0.0001), seq(0.99991, 0.99999, 0.00001)
            )
            # Create the workbook
            wb <- createWorkbook()

            # Add a sheet with summary data
            summary_sheet <- createSheet(wb, "SUMMARY")
            addDataFrame(data.frame(
                "Label"      = sapply(results, function(r) r$label),
                "Average"    = sapply(results, function(r) r$average),
                "Bottom 50%" = sapply(results, function(r) bottom_share(r, 0.5)),
                "Middle 40%" = sapply(results, function(r) bracket_share(r, 0.5, 0.9)),
                "Top 10%"    = sapply(results, function(r) top_share(r, 0.9)),
                "Top 1%"     = sapply(results, function(r) top_share(r, 0.99)),
                "Gini"       = sapply(results, function(r) gini(r)),
                check.names = FALSE
            ), summary_sheet, row.names=FALSE)

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

                sheet <- createSheet(wb, results[[i]]$label)
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
            return(do.call(tabsetPanel, c(plot_lorenz_tabs, type="pills")))
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
            return(do.call(tabsetPanel, c(plot_gpc_tabs, type="pills")))
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
            return(do.call(tabsetPanel, c(plot_pdf_tabs, type="pills")))
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
            return(do.call(tabsetPanel, c(plot_cdf_tabs, type="pills")))
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
            return(do.call(tabsetPanel, c(plot_quantile_tabs, type="pills")))
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
            return(do.call(tabsetPanel, c(plot_tail_tabs, type="pills")))
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
            return(do.call(tabsetPanel, c(plot_phi_tabs, type="pills")))
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
            return(do.call(tabsetPanel, c(plot_deriv_phi_tabs, type="pills")))
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
