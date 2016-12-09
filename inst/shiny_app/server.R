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
            } else if (trimws(data[i, 1]) == var$year) {
                data_list$year <- as.numeric(data[i, 2])
            } else if (trimws(data[i, 1]) == var$country) {
                data_list$country <- data[i, 2]
            } else if (trimws(data[i, 1]) == var$component) {
                data_list$component <- data[i, 2]
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
    } else {
        names <- sapply(data[1, ], trimws)
        data <- data[-1, ]
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

    # Look for the year, country and component
    if (var$year %in% colnames(data)) {
        data_list$year <- as.numeric(data[1, var$year])
    }
    if (var$country %in% colnames(data)) {
        data_list$country <- data[1, var$country]
    }
    if (var$component %in% colnames(data)) {
        data_list$component <- data[1, var$component]
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
    if (is.null(data_list$year)) {
        data_list$year <- "n/a"
    } else if (is.na(data_list$year)) {
        data_list$year <- "n/a"
    }
    if (is.null(data_list$country)) {
        data_list$country <- "n/a"
    } else if (is.na(data_list$country)) {
        data_list$country <- "n/a"
    }
    if (is.null(data_list$component)) {
        data_list$component <- "n/a"
    } else if (is.na(data_list$component)) {
        data_list$component <- "n/a"
    }
    if (is.null(data_list$popsize)) {
        data_list$popsize <- NA
    }

    return(data_list)
}

# Plot text only
plot_text <- function(text) {
    plot(c(0, 1), c(0, 1), ann=FALSE, bty='n', type='n', xaxt='n', yaxt='n')
    text(
        x = 0.5,
        y = 0.5,
        text,
        cex = 1.6,
        col = "darkgrey"
    )
}

shinyServer(function(input, output, session) {
    # Go back to home when clicking title
    observeEvent(input$main_logo, {
        updateNavbarPage(session, "main_navbar", selected="Input data")
    })

    # Reactive values for the input data and the results
    data <- reactiveValues(
        data       = NULL,
        nb_data    = NULL,
        errors     = NULL,
        years      = NULL,
        countries  = NULL,
        components = NULL,
        results    = NULL
    )



    clear_all <- function() {
        data$data       <- NULL
        data$nb_data    <- NULL
        data$errors     <- NULL
        data$year       <- NULL
        data$countries  <- NULL
        data$components <- NULL
        data$results    <- NULL

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

    # Import the input data
    observe({
        if (is.null(input$file_input)) {
            # If the user hasn't specified any file yet, do nothing
        } else {
            # Clear all previous data
            clear_all()

            # Retrieve variable names from the preference panel
            varnames <- list(
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
                bracketsingle = trimws(isolate(input$var_bracketsingle)),
                topsingle     = trimws(isolate(input$var_topsingle)),
                average       = trimws(isolate(input$var_average)),
                popsize       = trimws(isolate(input$var_popsize)),
                gumbel        = trimws(isolate(input$var_gumbel))
            )

            # Number of files to import
            nfiles <- length(input$file_input$name)

            # Initialize the import progress bar
            shinyjs::addClass("import_progress", "active")
            shinyjs::removeClass("import_progress", "progress-bar-danger")
            shinyjs::runjs(paste0("$('#import_progress').attr('aria-valuenow',", 0, ")"))
            shinyjs::runjs(paste0("$('#import_progress').attr('aria-valuemax',", nfiles, ")"))
            shinyjs::runjs(paste0("$('#import_progress').attr('style', 'width: ", 0, "%')"))

            # Keep a list of years, countries and components
            list_years <- list()
            list_countries <- list()
            list_components <- list()
            # This list will contain all the parsed user input data
            list_data <- list()
            nb_data <- 0
            # This list will contain the error messages
            list_errors <- list()
            for (i in 1:nfiles) {
                # Update the progress bar
                shinyjs::runjs(paste0("$('#import_progress').text('Importing file ", i, "/", nfiles, "')"))
                shinyjs::runjs(paste0("$('#import_progress').attr('aria-valuenow',", i - 1, ")"))
                shinyjs::runjs(paste0("$('#import_progress').attr('style', 'width: ", 100*(i - 1)/nfiles, "%')"))

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
                    # In case of error, add it to the list and move on to the next file
                    if (is.error(table)) {
                        list_errors <- c(list_errors, list(table))
                    } else {
                        # Otherwise, parse the content of the file
                        dpcomma <- (isolate(input$csv_input_dec_separator) == ",")
                        parsed_input <- tryCatch(
                            parse_input(table, varnames, dpcomma, filename),
                            error = function(e) simpleError(e$message)
                        )
                        # If parsing was successful, make sure that there isn't already
                        # a file with the same year, country and component
                        if (!is.error(parsed_input)) {
                            year <- as.character(parsed_input$year)
                            country <- parsed_input$country
                            component <- parsed_input$component
                            if ((year %in% list_years) &
                                (country %in% list_countries) &
                                (component %in% list_components)) {
                                list_errors <- c(errors, list(simpleError(paste0(
                                    "“", filename, "” was ignored because there is already a tabulation with
                                    the same year, country and component."
                                ))))
                            } else {
                                if (!year %in% list_years) {
                                    list_years <- c(list_years, year)
                                    list_data[[year]] <- list()
                                }
                                if (!country %in% list_countries) {
                                    list_countries <- c(list_countries, country)
                                    list_data[[year]][[country]] <- list()
                                }
                                if (!component %in% list_components) {
                                    list_components <- c(list_components, component)
                                    list_data[[year]][[country]][[component]] <- list()
                                }
                                list_data[[year]][[country]][[component]] <- parsed_input
                                nb_data <- nb_data + 1
                            }
                        } else {
                            list_errors <- c(list_errors, list(simpleError(paste0(
                                "“", filename, "” was ignored because of the following error: ",
                                parsed_input$message, "."
                            ))))
                        }
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
                    # In case of error, add it to the list and move on to the next file
                    if (is.error(sheets)) {
                        list_errors <- c(list_errors, list(sheets))
                    } else {# Otherwise, loop over the sheets of the Excel file and import them
                        # one by one
                        k <- 1
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
                            shinyjs::runjs(paste0("$('#import_progress').attr('aria-valuenow',", i - 1 + k/length(sheets), ")"))
                            shinyjs::runjs(paste0("$('#import_progress').attr('style', 'width: ", 100*(i - 1 + k/length(sheets))/nfiles, "%')"))
                            # If the sheet can't be read, move on to the next
                            if (is.error(table)) {
                                list_errors <- c(list_errors, list(table))
                                k <- k + 1
                            } else {
                                # Otherwise, parse the content of the file
                                parsed_input <- tryCatch(
                                    parse_input(table, varnames, FALSE, filename, sh),
                                    error = function(e) simpleError(e$message)
                                )
                                # If parsing was successful, make sure that there isn't already
                                # a file with the same year, country and component
                                if (!is.error(parsed_input)) {
                                    year <- as.character(parsed_input$year)
                                    country <- parsed_input$country
                                    component <- parsed_input$component
                                    if ((year %in% list_years) &
                                            (country %in% list_countries) &
                                            (component %in% list_components)) {
                                        list_errors <- c(list_errors, list(simpleError(paste0(
                                            "The sheet “", sh, "” of “", filename, "” was ignored because there is already a tabulation with
                                            the same year, country and component."
                                        ))))
                                    } else {
                                        if (!year %in% list_years) {
                                            list_years <- c(list_years, year)
                                            list_data[[year]] <- list()
                                        }
                                        if (!country %in% list_countries) {
                                            list_countries <- c(list_countries, country)
                                            list_data[[year]][[country]] <- list()
                                        }
                                        if (!component %in% list_components) {
                                            list_components <- c(list_components, component)
                                            list_data[[year]][[country]][[component]] <- list()
                                        }
                                        list_data[[year]][[country]][[component]] <- parsed_input
                                        nb_data <- nb_data + 1
                                    }
                                } else {
                                    list_errors <- c(list_errors, list(simpleError(paste0(
                                        "The sheet “", sh, "” of “", filename, "” was ignored because of the following error: ",
                                        parsed_input$message, "."
                                    ))))
                                }
                                k <- k + 1
                            }
                        }
                    }
                } else {
                    # Can't read the file: ignore
                    list_errors <- c(list_errors, list(simpleError(paste0(
                        "“", filename, "” was ignored because the extension ", extension, "is unknown."
                    ))))
                }
            }
            shinyjs::runjs(paste0("$('#import_progress').attr('aria-valuenow',", nfiles, ")"))
            shinyjs::runjs(paste0("$('#import_progress').attr('style', 'width: 100%')"))
            if (length(list_data) > 0) {
                shinyjs::runjs(paste0("$('#import_progress').text('Import complete')"))
                shinyjs::removeClass("import_progress", "active")
            } else {
                shinyjs::runjs(paste0("$('#import_progress').text('Import failed')"))
                shinyjs::addClass("import_progress", "progress-bar-danger")
                shinyjs::removeClass("import_progress", "active")
            }

            data$data       <- list_data
            data$nb_data    <- nb_data
            data$errors     <- list_errors
            data$years      <- unlist(list_years)
            data$countries  <- unlist(list_countries)
            data$components <- unlist(list_components)
        }
    })

    observeEvent(input$import_example, {
        data$data <- list("2010" = list("US" = list("n/a" = list(
            filename = "example file",
            p = c(0.10, 0.50, 0.90, 0.95, 0.99),
            year = 2010,
            country = "US",
            average = 53587,
            threshold = c(5665, 31829, 96480, 136910, 351366),
            whichavgsh = "bracketshare",
            bracketshare = c(0.13459, 0.41007, 0.10537, 0.14840, 0.19946),
            component = "n/a",
            popsize = NA
        ))))
        data$nb_data    <- 1
        data$errors     <- list()
        data$years      <- c("2010")
        data$countries  <- c("US")
        data$components <- c("n/a")
    })

    output$input_data_view_header <- renderUI({
        if (is.null(data$data) & is.null(data$errors)) {
            return(tags$div(
                tags$div(
                    tags$p("This interface lets you reconstruct the full distribution of income or
                        wealth based on tabulated data files such as those provided by tax autorities."),
                    tags$p("To import the tabulation files, use the “Browse” button
                        on the left and choose or more file from your computer. You must have one CSV file or
                        Excel sheet per tabulation. Each must take the form of a table with the following format:"),
                    tags$table(
                        tags$tr(
                            tags$th("year"), tags$th("country"),
                            tags$th("average"), tags$th("p"), tags$th("thr"), tags$th("bracketsh")
                        ),
                        tags$tr(
                            tags$td("2010"), tags$td("US"), tags$td("53 587"), tags$td("0.1"),
                            tags$td("5 665"), tags$td("0.13459")
                        ),
                        tags$tr(
                            tags$td(""), tags$td(""), tags$td(""), tags$td("0.5"),
                            tags$td("31 829"), tags$td("0.41007")
                        ),
                        tags$tr(
                            tags$td(""), tags$td(""), tags$td(""), tags$td("0.9"),
                            tags$td("96 480"), tags$td("0.10537")
                        ),
                        tags$tr(
                            tags$td(""), tags$td(""), tags$td(""), tags$td("0.95"),
                            tags$td("136 910"), tags$td("0.14840")
                        ),
                        tags$tr(
                            tags$td(""), tags$td(""), tags$td(""), tags$td("0.99"),
                            tags$td("351 366"), tags$td("0.19946")
                        ),
                        class = "table table-bordered table-condensed",
                        style = "margin-bottom: 2px;"
                    ),
                    tags$p("Download this sample file as", tags$a(icon("download"), "CSV", href="sample.csv"),
                        "/", tags$a(icon("download"), "Excel", href="sample.xlsx"), "or",
                        actionLink("import_example", "import it directly to the interface.", icon("arrow-down")),
                        style="font-size: small;"),
                    tags$p("Each column of the table correspond to a variable. You need to at least specify:",
                        tags$ul(
                            tags$li(tags$code("p"), "for fractiles"),
                            tags$li(tags$code("thr"), "for matching quantiles"),
                            tags$li(tags$code("average"), "for the overall average")
                        )
                    ), tags$p("You must also specify one of the following:",
                        tags$ul(
                            tags$li(tags$code("bracketsh"), "for the share of the bracket"),
                            tags$li(tags$code("topsh"), "for the top share"),
                            tags$li(tags$code("bracketavg"), "for the average in the bracket"),
                            tags$li(tags$code("topavg"), "for the top average"),
                            tags$li(tags$code("b"), "for the inverted Pareto coefficient")
                        )
                    ), tags$p("Finally, if you have several tabulations, you will need to identify them
                        using at least one of the following fields:", tags$ul(
                            tags$li(tags$code("year"), "for the period covered by the tabulation"),
                            tags$li(tags$code("country"), "for the country or region"),
                            tags$li(tags$code("component"), "for the component (for example labor or capital income)")
                        )
                    ),
                    class = "panel-body"
                ),
                class = "panel panel-default",
                style = "box-shadow: none; border-style: dashed; color: #666;"
            ))
        } else if (length(data$data) == 0 & length(data$errors) > 0) {
            return(tags$div(
                tags$p("There is nothing to display because all of your files
                    generated an error during the importation. Please check
                    the format of your input data and try again."), tags$p(
                    tags$ul(lapply(data$errors, function(e) {
                        tags$li(tags$i(class="fa fa-li fa-times-circle"), e$message)
                    }), class="fa-ul")
                ),
                class="alert alert-danger", role="alert"
            ))
        } else {
            if (length(data$errors) > 0) {
                warning_message <- tags$div(
                    tags$button(type="button", class="close", `data-dismiss`="alert", `aria-label`="Close",
                        tags$span(HTML("&times;"), `aria-hidden`="true")
                    ),
                    tags$p("Some of your files were ignored because of errors.
                        You can proceed nonetheless, but you may
                        want to check the format of some of your data."), tags$p(
                    tags$ul(lapply(data$errors, function(e) {
                        tags$li(tags$i(class="fa fa-li fa-exclamation-triangle"), e$message)
                    }), class="fa-ul")),
                    style = "max-height: 150px; overflow: scroll;",
                    class = "alert alert-warning alert-dismissible",
                    role = "alert"
                )
            } else {
                warning_message <- NULL
            }

            # Select input menu
            select_input_menu <- tagList()
            if (length(data$years) > 1) {
                select_input_menu <- tagList(select_input_menu,
                    column(4, selectInput("input_view_year", "Year", choices=data$years))
                )
            } else {
                select_input_menu <- tagList(select_input_menu,
                    disabled(column(4, selectInput("input_view_year", "Year", choices=data$years)))
                )
            }

            if (length(data$countries) > 1) {
                select_input_menu <- tagList(select_input_menu,
                    column(4, selectInput("input_view_country", "Country", choices=data$countries))
                )
            } else {
                select_input_menu <- tagList(select_input_menu,
                    disabled(column(4, selectInput("input_view_country", "Country", choices=data$countries)))
                )
            }

            if (length(data$component) > 1) {
                select_input_menu <- tagList(select_input_menu,
                    column(4, selectInput("input_view_component", "Component", choices=data$components))
                )
            } else {
                select_input_menu <- tagList(select_input_menu,
                    disabled(column(4, selectInput("input_view_component", "Component", choices=data$components)))
                )
            }

            return(tagList(
                fixedRow(
                    column(6, actionButton("run", "Run", icon=icon("play"), class="btn-block btn-success")),
                    column(6, actionButton("clear", "Clear data", icon=icon("eraser"), class="btn-block btn-danger")),
                    style = "margin-bottom: 20px;"
                ),
                warning_message,
                fixedRow(select_input_menu)
            ))
        }
    })

    output$input_data_view <- renderUI({
        year <- input$input_view_year
        country <- input$input_view_country
        component <- input$input_view_component

        if ((length(data$data) == 0) ||
            (is.null(year)) ||
            (is.null(country)) ||
            (is.null(component))) {
            return(NULL)
        }

        data_view <- data$data[[year]][[country]][[component]]

        if (is.null(data_view)) {
            return(tags$div(
                tags$p(icon("info-circle"), "No data available for your selection."),
                class="alert alert-info", role="alert"
            ))
        }

        # Average/share variable for this data
        avgsh <- data_view$whichavgsh
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
            "Fractiles" = sprintf("%1.5f", data_view$p),
            "Thresholds" = sprintf("%.0f", data_view$threshold)
        )
        df[avgsh_clean] <- sprintf("%.3f", data_view[[avgsh]])
        df[is.na(data_view[[avgsh]]), avgsh_clean] <- NA

        return(tagList(
            tags$h4("Summary"),
            tags$table(
                tags$tbody(
                    tags$tr(
                        tags$th("file name", style="white-space: nowrap;"),
                        tags$td(data_view$filename, style="width: 100%;")
                    ),
                    tags$tr(
                        tags$th("average", style="white-space: nowrap;"),
                        tags$td(sprintf("%.2f", data_view$average))
                    ),
                    tags$tr(
                        tags$th("population size", style="white-space: nowrap;"),
                        tags$td(ifelse(is.na(data_view$popsize),
                            "n/a",
                            data_view$popsize
                        ), style = ifelse(
                            is.na(data_view$popsize),
                            "color: #999;", ""
                        ))
                    )
                ),
                class = "table table-condensed table-striped"
            ),
            tags$h4("Tabulation"),
            renderTable(df,
                striped = TRUE,
                width = "100%",
                na = "n/a"
            )
        ))
    })

    observeEvent(input$clear, {
        clear_all()

        # Reset file input
        shinyjs::reset("file_input")
        shinyjs::runjs("$('#file_input_progress').css('visibility', 'visible');")
        shinyjs::removeClass("import_progress", "progress-bar-danger")
        shinyjs::runjs(paste0("$('#import_progress').attr('aria-valuenow',", 0, ")"))
        shinyjs::runjs(paste0("$('#import_progress').attr('style', 'width: ", 0, "%')"))
        shinyjs::runjs(paste0("$('#import_progress').text('')"))
        shinyjs::runjs(paste0("$('#file_input_progress .progress-bar').text('')"))
    })

    # Launch the programs when the user clicks the "Run" button
    observeEvent(input$run, {
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
                        `aria-valuemax` = data$nb_data,
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
                actionButton("dismiss_run_success", label="Close", icon=icon("check"),
                    width="100%", class="btn-success"),
                actionButton("dismiss_run_failure", label="Close", icon=icon("ban"),
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

        # List to store the results
        list_results <- list()
        i <- 1
        for (year in data$years) {
            list_results[[year]] <- list()
            for (country in data$countries) {
                list_results[[year]][[country]] <- list()
                for (component in data$components) {
                    data_model <- data$data[[year]][[country]][[component]]

                    data_label <- c(component, country, year)
                    data_label <- data_label[data_label != "n/a"]
                    data_label <- paste(data_label, collapse=", ")

                    # Move on to next loop if the data doesn't exist
                    if (is.null(data_model)) {
                        next
                    }

                    # Update the status message in the dialog
                    shinyjs::runjs(paste0("$('#run_status').html('<i class=\"fa fa-cog fa-spin fa-fw\"></i> ",
                        "Currently working on: ", data_label, "')"))

                    result_model <- tryCatch({
                        args <- list(
                            p = data_model$p,
                            threshold = data_model$threshold,
                            average = data_model$average
                        )
                        avgsh <- data_model$whichavgsh
                        args[avgsh] <- data_model[avgsh]
                        result <- do.call(tabulation_fit, args)
                        result$label <- data_model$label
                        result
                    }, error = function(e) {
                        return(simpleError(e$message))
                    })

                    # If the program failed, stop and show the error to the user
                    if (is.error(result_model)) {
                        # Show the error to the user
                        shinyjs::show("failure_message")
                        shinyjs::show("dismiss_run_failure")
                        shinyjs::runjs(paste0("$('#run_status').html('<i class=\"fa fa-frown-o\" aria-hidden=\"true\"></i> Something went wrong.')"))

                        shinyjs::runjs(paste0("$('#error_message1').text('An error occurred while working on ", data_label, ". ",
                            "Please check the consistency of your data.')"))
                        # Sanitize & display error message
                        msg <- result_model$message
                        msg <- gsub("\n", "", msg, fixed=TRUE)
                        msg <- gsub("'", "\\'", msg, fixed=TRUE)
                        shinyjs::runjs(paste0("$('#error_message2').html('<i class=\"fa fa-exclamation-circle\" aria-hidden=\"true\"></i> &nbsp; ", msg, "')"))
                        shinyjs::removeClass("run_progress", "active")

                        # Clear the results
                        data$results <- NULL

                        return(NULL)
                    }

                    list_results[[year]][[country]][[component]] <- result_model

                    # Update the progress bar
                    i <- i + 1
                    shinyjs::runjs(paste0("$('#run_progress').attr('aria-valuenow',", i, ")"))
                    shinyjs::runjs(paste0("$('#run_progress').attr('style', 'width: ", 100*i/data$nb_data, "%')"))
                }
            }
        }

        # Update the status message to show success
        shinyjs::runjs(paste0("$('#run_status').html('<i class=\"fa fa-thumbs-up\" aria-hidden=\"true\"></i> All done!')"))
        shinyjs::removeClass("run_progress", "active")

        shinyjs::show("success_message")
        shinyjs::show("dismiss_run_success")

        # Store the results
        data$results <- list_results

        # Update the interface
        updateSelectInput(session, "output_table_year", choices=data$years)
        updateSelectInput(session, "output_dist_plot_year", choices=data$years)
        updateSelectInput(session, "synthpop_year", choices=data$years)
        if (length(data$years) > 1) {
            enable("output_table_year")
            enable("output_dist_plot_year")
            enable("synthpop_year_all")
            if (!input$synthpop_year_all) {
                enable("synthpop_year")
            }
        }
        updateSelectInput(session, "output_table_country", choices=data$countries)
        updateSelectInput(session, "output_dist_plot_country", choices=data$countries)
        updateSelectInput(session, "output_time_plot_country", choices=data$countries)
        updateSelectInput(session, "synthpop_country", choices=data$countries)
        if (length(data$countries) > 1) {
            enable("output_table_country")
            enable("output_dist_plot_country")
            enable("output_time_plot_country")
            enable("synthpop_country_all")
            if (!input$synthpop_country_all) {
                enable("synthpop_country")
            }
        }
        updateSelectInput(session, "output_table_component", choices=data$components)
        updateSelectInput(session, "output_dist_plot_component", choices=data$components)
        updateSelectInput(session, "output_time_plot_component", choices=data$components)
        updateSelectInput(session, "synthpop_component", choices=data$components)
        if (length(data$components) > 1) {
            enable("output_table_component")
            enable("output_dist_plot_component")
            enable("output_time_plot_component")
            enable("synthpop_component_all")
            if (!input$synthpop_component_all) {
                enable("synthpop_component")
            }
        }

        enable("synthpop_dl_csv")
        enable("synthpop_dl_excel")

        enable("dl_tables_csv")
        enable("dl_tables_excel")
    })

    observeEvent(input$dismiss_run_success, {
        removeModal()
    })

    observeEvent(input$dismiss_run_failure, {
        removeModal()
    })

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

        if (is.null(result)) {
            return(tags$div(icon("info-circle"), HTML("&nbsp;"),
                "No data available for your selection.",
                class="alert alert-info", role="alert"))
        }

        # Create the list of percentiles we want to show the user
        gperc <- c(
            seq(0, 0.99, 0.01), seq(0.991, 0.999, 0.001),
            seq(0.9991, 0.9999, 0.0001), seq(0.99991, 0.99999, 0.00001)
        )

        summary_table <- renderTable(
            data.frame(
                "Average" = sprintf("%.0f", result$average),
                "Bottom 50%" = sprintf("%.1f%%", 100*bottom_share(result, 0.5)),
                "Middle 40%" = sprintf("%.1f%%", 100*bracket_share(result, 0.5, 0.9)),
                "Top 10%" = sprintf("%.1f%%", 100*top_share(result, 0.9)),
                "Top 1%" = sprintf("%.1f%%", 100*top_share(result, 0.99)),
                "Gini" = sprintf("%.3f", gini(result)),
                check.names = FALSE
            ),
            striped = TRUE,
            width = "100%"
        )

        # Detailed tabulation
        out_df <- data.frame("Percentiles" = sprintf("%1.5f", gperc))

        if ("thres" %in% input$results_display) {
            col <- fitted_quantile(result, gperc)
            out_df["Threshold"] <- ifelse(is.na(col), NA, sprintf("%.0f", col))
            out_df[is.infinite(col), "Threshold"] <- "–∞"
        }
        if ("topshare" %in% input$results_display) {
            col <- top_share(result, gperc)
            out_df["Top share"] <- ifelse(is.na(col), NA, sprintf("%.2f%%", 100*col))
        }
        if ("bottomshare" %in% input$results_display) {
            col <- bottom_share(result, gperc)
            out_df["Bottom share"] <- ifelse(is.na(col), NA, sprintf("%.2f%%", 100*col))
        }
        if ("bracketshare" %in% input$results_display) {
            col <- bracket_share(result, gperc, c(gperc, 1)[2:(length(gperc) + 1)])
            out_df["Bracket share"] <- ifelse(is.na(col), NA, sprintf("%.2f%%", 100*col))
        }
        if ("topavg" %in% input$results_display) {
            col <- top_average(result, gperc)
            out_df["Top average"] <- ifelse(is.na(col), NA, sprintf("%.0f", col))
        }
        if ("bracketavg" %in% input$results_display) {
            col <- bracket_average(result, gperc, c(gperc, 1)[2:(length(gperc) + 1)])
            out_df["Bracket average"] <- ifelse(is.na(col), NA, sprintf("%.0f", col))
        }
        if ("invpareto" %in% input$results_display) {
            col <- invpareto(result, gperc)
            out_df["Inverted Pareto coefficient"] <- ifelse(is.na(col), NA, sprintf("%.2f", col))
            out_df[is.infinite(col), "Inverted Pareto coefficient"] <- "∞"
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
            gperc <- c(
                seq(0, 0.99, 0.01), seq(0.991, 0.999, 0.001),
                seq(0.9991, 0.9999, 0.0001), seq(0.99991, 0.99999, 0.00001)
            )

            tmp <- tempdir()
            files <- c()
            for (country in data$countries) {
                for (component in data$components) {
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
                            result <- data$results[[year]][[country]][[component]]
                            if (is.null(result)) {
                                return(NA)
                            } else {
                                return(bottom_share(result, 0.5))
                            }
                        }),
                        "Middle 40%" = sapply(data$years, function(year) {
                            result <- data$results[[year]][[country]][[component]]
                            if (is.null(result)) {
                                return(NA)
                            } else {
                                return(bracket_share(result, 0.5, 0.9))
                            }
                        }),
                        "Top 10%" = sapply(data$years, function(year) {
                            result <- data$results[[year]][[country]][[component]]
                            if (is.null(result)) {
                                return(NA)
                            } else {
                                return(top_share(result, 0.9))
                            }
                        }),
                        "Top 1%" = sapply(data$years, function(year) {
                            result <- data$results[[year]][[country]][[component]]
                            if (is.null(result)) {
                                return(NA)
                            } else {
                                return(top_share(result, 0.99))
                            }
                        }),
                        "Gini" = sapply(data$years, function(year) {
                            result <- data$results[[year]][[country]][[component]]
                            if (is.null(result)) {
                                return(NA)
                            } else {
                                return(gini(result))
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

                    for (year in data$years) {
                        result <- data$results[[year]][[country]][[component]]
                        if (is.null(result)) {
                            next
                        }

                        data_label <- c(component, country, year)
                        data_label <- data_label[data_label != "n/a"]
                        data_label <- paste(data_label, collapse=", ")

                        out_df <- data.frame("Percentiles" = gperc)

                        if ("thres" %in% input$results_display) {
                            out_df["Threshold"] <- fitted_quantile(result, gperc)
                        }
                        if ("topshare" %in% input$results_display) {
                            out_df["Top share"] <- top_share(result, gperc)
                        }
                        if ("bottomshare" %in% input$results_display) {
                            out_df["Bottom share"] <- bottom_share(result, gperc)
                        }
                        if ("bracketshare" %in% input$results_display) {
                            out_df["Bracket share"] <- bracket_share(result, gperc, c(gperc, 1)[2:(length(gperc) + 1)])
                        }
                        if ("topavg" %in% input$results_display) {
                            out_df["Top average"] <- top_average(result, gperc)
                        }
                        if ("bracketavg" %in% input$results_display) {
                            out_df["Bracket average"] <- bracket_average(result, gperc, c(gperc, 1)[2:(length(gperc) + 1)])
                        }
                        if ("invpareto" %in% input$results_display) {
                            out_df["Inverted Pareto coefficient"] <- invpareto(result, gperc)
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
            for (country in data$countries) {
                for (component in data$components) {
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
                            result <- data$results[[year]][[country]][[component]]
                            if (is.null(result)) {
                                return(NA)
                            } else {
                                return(bottom_share(result, 0.5))
                            }
                        }),
                        "Middle 40%" = sapply(data$years, function(year) {
                            result <- data$results[[year]][[country]][[component]]
                            if (is.null(result)) {
                                return(NA)
                            } else {
                                return(bracket_share(result, 0.5, 0.9))
                            }
                        }),
                        "Top 10%" = sapply(data$years, function(year) {
                            result <- data$results[[year]][[country]][[component]]
                            if (is.null(result)) {
                                return(NA)
                            } else {
                                return(top_share(result, 0.9))
                            }
                        }),
                        "Top 1%" = sapply(data$years, function(year) {
                            result <- data$results[[year]][[country]][[component]]
                            if (is.null(result)) {
                                return(NA)
                            } else {
                                return(top_share(result, 0.99))
                            }
                        }),
                        "Gini" = sapply(data$years, function(year) {
                            result <- data$results[[year]][[country]][[component]]
                            if (is.null(result)) {
                                return(NA)
                            } else {
                                return(gini(result))
                            }
                        }),
                        check.names = FALSE,
                        stringsAsFactors = FALSE
                    )
                    df_series[, "Year"] <- as.numeric(df_series[, "Year"])
                    df_series <- df_series[!is.na(df_series[, "Year"]), ]
                    df_series <- df_series[order(df_series[, "Year"]), ]

                    sheet <- createSheet(wb, strtrim(series_label, 31))
                    addDataFrame(df_series, sheet, row.names=FALSE)

                    for (year in data$years) {
                        result <- data$results[[year]][[country]][[component]]
                        if (is.null(result)) {
                            next
                        }

                        data_label <- c(component, country, year)
                        data_label <- data_label[data_label != "n/a"]
                        data_label <- paste(data_label, collapse=", ")
                        if (data_label == "") {
                            data_label = "no name"
                        }

                        out_df <- data.frame("Percentiles" = gperc)

                        if ("thres" %in% input$results_display) {
                            out_df["Threshold"] <- fitted_quantile(result, gperc)
                        }
                        if ("topshare" %in% input$results_display) {
                            out_df["Top share"] <- top_share(result, gperc)
                        }
                        if ("bottomshare" %in% input$results_display) {
                            out_df["Bottom share"] <- bottom_share(result, gperc)
                        }
                        if ("bracketshare" %in% input$results_display) {
                            out_df["Bracket share"] <- bracket_share(result, gperc, c(gperc, 1)[2:(length(gperc) + 1)])
                        }
                        if ("topavg" %in% input$results_display) {
                            out_df["Top average"] <- top_average(result, gperc)
                        }
                        if ("bracketavg" %in% input$results_display) {
                            out_df["Bracket average"] <- bracket_average(result, gperc, c(gperc, 1)[2:(length(gperc) + 1)])
                        }
                        if ("invpareto" %in% input$results_display) {
                            out_df["Inverted Pareto coefficient"] <- invpareto(result, gperc)
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

    # Generate the distribution plots
    result_plot <- reactive({
        if (is.null(data$results)) {
            return(NULL)
        }

        year <- input$output_dist_plot_year
        country <- input$output_dist_plot_country
        component <- input$output_dist_plot_component

        result <- data$results[[year]][[country]][[component]]

        return(result)
    })

    observe({
        result <- result_plot()

        if (is.null(result)) {
            disable("slider_lorenz")
            disable("slider_gpc")
            disable("slider_pdf")
            disable("slider_cdf")
            disable("slider_quantile")
            disable("slider_tail")
            disable("slider_phi")
            disable("slider_deriv_phi")
        } else {
            supp <- support(result)
            if (is.infinite(supp$lower)) {
                q_min <- round(fitted_quantile(result, 0.01))
            } else {
                q_min <- round(supp$lower)
            }
            q_max <- round(fitted_quantile(result, 0.99))

            enable("slider_lorenz")
            enable("slider_gpc")
            enable("slider_pdf")
            enable("slider_cdf")
            enable("slider_quantile")
            enable("slider_tail")
            enable("slider_phi")
            enable("slider_deriv_phi")

            updateSliderInput(session, "slider_pdf",
                min = q_min,
                max = q_max,
                value = c(q_min, q_max)
            )
            updateSliderInput(session, "slider_cdf",
                min = q_min,
                max = q_max,
                value = c(q_min, q_max)
            )
        }
    })

    output$plot_lorenz <- renderPlot({
        result <- result_plot()

        if (is.null(result) || is.null(input$slider_lorenz)) {
            return(plot_text("No data"))
        }

        if (min(input$slider_lorenz) == max(input$slider_lorenz)) {
            return(plot_text("Need more than one year"))
        }

        pmin <- min(input$slider_lorenz)
        pmax <- max(input$slider_lorenz)

        return(plot_lorenz(result, xlim=c(pmin, pmax)))
    })

    output$plot_gpc <- renderPlot({
        result <- result_plot()

        if (is.null(result) || is.null(input$slider_gpc)) {
            return(plot_text("No data"))
        }

        if (min(input$slider_gpc) == max(input$slider_gpc)) {
            return(plot_text("Need more than one year"))
        }

        pmin <- min(input$slider_gpc)
        pmax <- max(input$slider_gpc)

        return(plot_gpc(result, xlim=c(pmin, pmax)))
    })

    output$plot_pdf <- renderPlot({
        result <- result_plot()

        if (is.null(result) || is.null(input$slider_pdf)) {
            return(plot_text("No data"))
        }

        if (min(input$slider_pdf) == max(input$slider_pdf)) {
            return(plot_text("Need more than one year"))
        }

        qmin <- min(input$slider_pdf)
        qmax <- max(input$slider_pdf)

        return(plot_density(result, xlim=c(qmin, qmax)))
    })

    output$plot_cdf <- renderPlot({
        result <- result_plot()

        if (is.null(result) || is.null(input$slider_cdf)) {
            return(plot_text("No data"))
        }

        if (min(input$slider_cdf) == max(input$slider_cdf)) {
            return(plot_text("Need more than one year"))
        }

        qmin <- min(input$slider_cdf)
        qmax <- max(input$slider_cdf)

        return(plot_cdf(result, xlim=c(qmin, qmax)))
    })

    output$plot_quantile <- renderPlot({
        result <- result_plot()

        if (is.null(result) || is.null(input$slider_quantile)) {
            return(plot_text("No data"))
        }

        if (min(input$slider_quantile) == max(input$slider_quantile)) {
            return(plot_text("Need more than one year"))
        }

        pmin <- min(input$slider_quantile)
        pmax <- max(input$slider_quantile)

        return(plot_quantile(result, xlim=c(pmin, pmax)))
    })

    output$plot_tail <- renderPlot({
        result <- result_plot()

        if (is.null(result) || is.null(input$slider_tail)) {
            return(plot_text("No data"))
        }

        if (min(input$slider_tail) == max(input$slider_tail)) {
            return(plot_text("Need more than one year"))
        }

        xmin <- min(input$slider_tail)
        xmax <- max(input$slider_tail)

        return(plot_tail(result, xlim=c(xmin, xmax)))
    })

    output$plot_phi <- renderPlot({
        result <- result_plot()

        if (is.null(result) || is.null(input$slider_phi)) {
            return(plot_text("No data"))
        }

        if (min(input$slider_phi) == max(input$slider_phi)) {
            return(plot_text("Need more than one year"))
        }

        xmin <- min(input$slider_phi)
        xmax <- max(input$slider_phi)

        return(plot_phi(result, xlim=c(xmin, xmax)))
    })

    output$plot_deriv_phi <- renderPlot({
        result <- result_plot()

        if (is.null(result) || is.null(input$slider_deriv_phi)) {
            return(plot_text("No data"))
        }

        if (min(input$slider_deriv_phi) == max(input$slider_deriv_phi)) {
            return(plot_text("Need more than one year"))
        }

        xmin <- min(input$slider_deriv_phi)
        xmax <- max(input$slider_deriv_phi)

        return(plot_deriv_phi(result, xlim=c(xmin, xmax)))
    })

    # Generate the time plots
    result_plot_allyears <- reactive({
        if (is.null(data$results)) {
            return(NULL)
        }

        country <- input$output_time_plot_country
        component <- input$output_time_plot_component

        return(lapply(data$years, function(year) {
            return(data$results[[year]][[country]][[component]])
        }))
    })

    observe({
        results <- result_plot_allyears()

        if (is.null(results)) {
            disable("slider_top_1")
            disable("slider_top_10")
            disable("slider_middle_40")
            disable("slider_bottom_50")
            disable("slider_gini")
        } else {
            enable("slider_top_1")
            enable("slider_top_10")
            enable("slider_middle_40")
            enable("slider_bottom_50")
            enable("slider_gini")

            minyear <- min(na.omit(as.numeric(data$years)))
            maxyear <- max(na.omit(as.numeric(data$years)))

            for (id in c("slider_top_1", "slider_top_10", "slider_middle_40",
                         "slider_bottom_50", "slider_gini")) {
                updateSliderInput(session, id,
                    min = minyear,
                    max = maxyear,
                    value = c(minyear, maxyear)
                )
            }
        }
    })

    output$plot_top_1 <- renderPlot({
        result <- result_plot_allyears()

        if (is.null(result) || is.null(input$slider_top_1)) {
            return(plot_text("No data"))
        }

        if (min(input$slider_top_1) == max(input$slider_top_1)) {
            return(plot_text("Need more than one year"))
        }

        ymin <- min(input$slider_top_1)
        ymax <- max(input$slider_top_1)

        df <- data.frame(
            year = as.numeric(data$years),
            top1 = sapply(result, function(dist) top_share(dist, 0.99))
        )
        df$year <- as.numeric(df$year)
        df <- df[!is.na(df$year), ]
        df <- df[df$year >= ymin & df$year <= ymax, ]

        plot <- ggplot2::ggplot(data=df, ggplot2::aes(x=year, y=top1)) +
            ggplot2::geom_line() + ggplot2::geom_point() +
            ggplot2::scale_y_continuous(labels = scales::percent) +
            ggplot2::xlab("Year") + ggplot2::ylab("Top 1% share")

        return(plot)
    })

    output$plot_top_10 <- renderPlot({
        result <- result_plot_allyears()

        if (is.null(result) || is.null(input$slider_top_10)) {
            return(plot_text("No data"))
        }

        if (min(input$slider_top_1) == max(input$slider_top_10)) {
            return(plot_text("Need more than one year"))
        }

        ymin <- min(input$slider_top_10)
        ymax <- max(input$slider_top_10)

        df <- data.frame(
            year = as.numeric(data$years),
            top10 = sapply(result, function(dist) top_share(dist, 0.90))
        )
        df$year <- as.numeric(df$year)
        df <- df[!is.na(df$year), ]
        df <- df[df$year >= ymin & df$year <= ymax, ]

        plot <- ggplot2::ggplot(data=df, ggplot2::aes(x=year, y=top10)) +
            ggplot2::geom_line() + ggplot2::geom_point() +
            ggplot2::scale_y_continuous(labels = scales::percent) +
            ggplot2::xlab("Year") + ggplot2::ylab("Top 10% share")

        return(plot)
    })

    output$plot_middle_40 <- renderPlot({
        result <- result_plot_allyears()

        if (is.null(result) || is.null(input$slider_middle_40)) {
            return(plot_text("No data"))
        }

        if (min(input$slider_middle_40) == max(input$slider_middle_40)) {
            return(plot_text("Need more than one year"))
        }

        ymin <- min(input$slider_middle_40)
        ymax <- max(input$slider_middle_40)

        df <- data.frame(
            year = as.numeric(data$years),
            middle40 = sapply(result, function(dist) bracket_share(dist, 0.50, 0.90))
        )
        df$year <- as.numeric(df$year)
        df <- df[!is.na(df$year), ]
        df <- df[df$year >= ymin & df$year <= ymax, ]

        plot <- ggplot2::ggplot(data=df, ggplot2::aes(x=year, y=middle40)) +
            ggplot2::geom_line() + ggplot2::geom_point() +
            ggplot2::scale_y_continuous(labels = scales::percent) +
            ggplot2::xlab("Year") + ggplot2::ylab("Middle 40% share")

        return(plot)
    })

    output$plot_bottom_50 <- renderPlot({
        result <- result_plot_allyears()

        if (is.null(result) || is.null(input$slider_bottom_50)) {
            return(plot_text("No data"))
        }

        if (min(input$slider_bottom_50) == max(input$slider_bottom_50)) {
            return(plot_text("Need more than one year"))
        }

        ymin <- min(input$slider_bottom_50)
        ymax <- max(input$slider_bottom_50)

        df <- data.frame(
            year = as.numeric(data$years),
            bottom50 = sapply(result, function(dist) bottom_share(dist, 0.50))
        )
        df$year <- as.numeric(df$year)
        df <- df[!is.na(df$year), ]
        df <- df[df$year >= ymin & df$year <= ymax, ]

        plot <- ggplot2::ggplot(data=df, ggplot2::aes(x=year, y=bottom50)) +
            ggplot2::geom_line() + ggplot2::geom_point() +
            ggplot2::scale_y_continuous(labels = scales::percent) +
            ggplot2::xlab("Year") + ggplot2::ylab("Bottom 50% share")

        return(plot)
    })

    output$plot_gini <- renderPlot({
        result <- result_plot_allyears()

        if (is.null(result) || is.null(input$slider_gini)) {
            return(plot_text("No data"))
        }

        if (min(input$slider_gini) == max(input$slider_gini)) {
            return(plot_text("Need more than one year"))
        }

        ymin <- min(input$slider_gini)
        ymax <- max(input$slider_gini)

        df <- data.frame(
            year = as.numeric(data$years),
            gini = sapply(result, function(dist) gini(dist))
        )
        df$year <- as.numeric(df$year)
        df <- df[!is.na(df$year), ]
        df <- df[df$year >= ymin & df$year <= ymax, ]

        plot <- ggplot2::ggplot(data=df, ggplot2::aes(x=year, y=gini)) +
            ggplot2::geom_line() + ggplot2::geom_point() +
            ggplot2::xlab("Year") + ggplot2::ylab("Gini index")

        return(plot)
    })

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
})
