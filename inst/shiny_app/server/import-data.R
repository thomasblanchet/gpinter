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
            singleshare   = trimws(isolate(input$var_singleshare)),
            coupleshare   = trimws(isolate(input$var_coupleshare)),
            singlebracket = trimws(isolate(input$var_singlebracket)),
            singletop     = trimws(isolate(input$var_singletop)),
            couplebracket = trimws(isolate(input$var_couplebracket)),
            coupletop     = trimws(isolate(input$var_coupletop)),
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
                        }

                        if (!is.null(list_data[[year]][[country]][[component]])) {
                            list_errors <- c(list_errors, list(simpleError(paste0(
                                "“", filename, "” was ignored because there is already a tabulation with
                                the same year, country and component."
                            ))))
                        } else {
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
                    } else {
                        # Otherwise, loop over the sheets of the Excel file and import them
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
                                    }

                                    if (!is.null(list_data[[year]][[country]][[component]])) {
                                        list_errors <- c(list_errors, list(simpleError(paste0(
                                            "The sheet “", sh, "” of “", filename, "” was ignored because there is already a tabulation with
                                            the same year, country and component."
                                        ))))
                                    } else {
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
