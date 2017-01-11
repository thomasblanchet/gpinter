# Global values to monitor the advancement of the import process
import_progress <- list(
    value     = 0,
    value_max = 100
)

# Update the progress bar
init_import_progressbar <- function() {
    import_progress$value <- 0
    shinyjs::runjs(paste0("$('#import_progress').attr('aria-valuemin',", 0, ")"))
    shinyjs::runjs(paste0("$('#import_progress').attr('aria-valuenow',", 0, ")"))
    shinyjs::runjs(paste0("$('#import_progress').attr('style', 'width: 0%')"))
}

show_import_progressbar <- function() {
    shinyjs::show("panel_import_progress", anim=TRUE, animType="fade")
}

hide_import_progressbar <- function() {
    shinyjs::hide("panel_import_progress", anim=TRUE, animType="fade")
}

update_import_progressbar_max <- function(n) {
    import_progress$value_max <<- n
    i <- import_progress$value

    shinyjs::runjs(paste0("$('#import_progress').attr('aria-valuemax',", n, ")"))
    shinyjs::runjs(paste0("$('#import_progress').attr('style', 'width: ", 100*i/n, "%')"))
}

update_import_progressbar_value <- function(i) {
    import_progress$value <<- i
    n <- import_progress$value_max

    if (i < n) {
        shinyjs::addClass(selector="#import_progress", class="active")
        shinyjs::addClass(selector="#panel_import_progress i.fa-cog", class="fa-spin")
    } else {
        shinyjs::removeClass(selector="#import_progress", class="active")
        shinyjs::removeClass(selector="#panel_import_progress i.fa-cog", class="fa-spin")
    }

    shinyjs::runjs(paste0("$('#import_progress').attr('aria-valuenow',", i, ")"))
    shinyjs::runjs(paste0("$('#import_progress').attr('style', 'width: ", 100*i/n, "%')"))
}

increase_import_progressbar_value <- function(i) {
    update_import_progressbar_value(import_progress$value + i)
}

update_import_progressbar_message <- function(msg) {
    msg <- gsub("\n", "", msg, fixed=TRUE)
    msg <- gsub("'", "\\'", msg, fixed=TRUE)
    shinyjs::runjs(paste0("$('#import_progress_message').text('", msg, "')"))
}

# List the files and Excel sheets
observeEvent(input$file_input, ignoreNULL = FALSE, handlerExpr = {
    if (is.null(input$file_input)) {
        shinyjs::show("help_intro")
        hide_import_progressbar()
    } else {
        shinyjs::hide("help_intro")
        shinyjs::hide("input_data_view_header")
        shinyjs::hide("input_data_view")

        # Number of files to import
        n_files <- length(input$file_input$name)

        init_import_progressbar()
        update_import_progressbar_max(10*n_files)
        update_import_progressbar_message("Listing the files")
        show_import_progressbar()

        # Make a list of files/sheets to import
        list_files <- list()
        list_errors <- list()
        id <- 1
        for (i in 1:n_files) {
            filename <- input$file_input$name[i]
            filepath <- input$file_input$datapath[i]
            # Identify the type of file
            extension <- tail(strsplit(filename, ".", fixed=TRUE)[[1]], n=1)

            if (extension %in% c("csv", "tsv", "txt")) {
                list_files <- c(list_files, list(list(
                    type = "csv",
                    filename = filename,
                    path = filepath,
                    id = id
                )))
                id <- id + 1
            } else if (extension %in% c("xls", "xlsx")) {
                # Rename the file to use the proper extension (required by readxl)
                newpath <- paste0(filepath, ".", extension)
                file.rename(filepath, newpath)
                filepath <- newpath

                # List the Excel sheets
                sheets <- tryCatch(excel_sheets(filepath), error = function(e) {
                    return(simpleError(paste0(
                        "“", filename, "” was ignored because of the following error: ", e$message, "."
                    )))
                })

                if (is.error(sheets)) {
                    list_errors <- c(list_errors, list(sheets))
                } else {
                    for (sh in sheets) {
                        list_files <- c(list_files, list(list(
                            type = "excel",
                            filename = filename,
                            sheetname = sh,
                            path = filepath,
                            id = id
                        )))
                        id <- id + 1
                    }
                }
            }
            update_import_progressbar_value(i)
        }
        data$input_errors <- list_errors
        data$files_all <- list_files
    }
})

# Once the files have been listed, make the user select the Excel sheets
# he wants to import

sheet_select <- reactiveValues(
    sheet_name = NULL,
    sheet_id = NULL
)

observeEvent(data$files_all, ignoreNULL = FALSE, handlerExpr = {
    if (is.null(data$files_all)) {
        data$files_selected <- NULL
    } else {
        # Scan the list of files for Excel sheets
        sheet_select$sheet_name <- list()
        sheet_select$sheet_id <- list()
        for (i in seq_along(data$files_all)) {
            if (data$files_all[[i]]$type == "excel") {
                file <- data$files_all[[i]]$filename
                sheet <- data$files_all[[i]]$sheetname
                id <- data$files_all[[i]]$id

                sheet_select$sheet_name[[file]] <- c(sheet_select$sheet_name[[file]], sheet)
                sheet_select$sheet_id[[file]] <- c(sheet_select$sheet_id[[file]], id)
            }
        }

        # Show a dialog to the user if there is at least one Excel file
        # with two sheets or more
        if (length(sheet_select$sheet_name) > 0 && max(sapply(sheet_select$sheet_name, length)) > 1) {
            # Build the checkboxes
            checkboxes <- NULL
            for (i in seq_along(sheet_select$sheet_name)) {
                filename <- names(sheet_select$sheet_name)[i]
                sheets <- sheet_select$sheet_name[[i]]
                ids <- sheet_select$sheet_id[[i]]

                for (j in seq_along(sheets)) {
                    sh <- sheets[[j]]
                    id <- ids[[j]]

                    checkboxes <- c(checkboxes,
                        list(tags$tr(
                            tags$td(checkboxInput(paste0("sheet_import_", id), label=sh, value=TRUE, width="100%"), style="width: 100%;"),
                            tags$td(filename, style="white-space: nowrap;")
                        ))
                    )
                }
            }

            showModal(modalDialog(
                tags$p("Your Excel workbooks have several sheets. Please select which ones you want to import."),
                fixedRow(
                    column(4, actionButton("sheets_import_selected", "Import selected",
                        class = "btn-block btn-success btn-sm",
                        icon = icon("arrow-down")
                    )),
                    column(4, actionButton("sheets_select_all", "Select all",
                        class = "btn-block btn-primary btn-sm",
                        icon = icon("check")
                    )),
                    column(4, actionButton("sheets_unselect_all", "Unselect all",
                        class = "btn-block btn-danger btn-sm",
                        icon = icon("times")
                    ))
                ),
                tags$table(
                    tags$thead(tags$tr(
                        tags$th("Sheet"),
                        tags$th("Workbook")
                    )),
                    checkboxes,
                    class = "table table-striped table-condensed",
                    id = "sheet_select",
                    style = "margin-top: 10px; margin-bottom: 0;"
                ),
                title = HTML("<i class='fa fa-file-excel-o' aria-hidden='true'></i> &nbsp; Select Excel sheets to import"),
                footer = NULL
            ))
        } else {
            data$files_selected <- data$files_all
        }
    }
})

# "Select all" button
observeEvent(input$sheets_select_all, {
    for (i in seq_along(sheet_select$sheet_name)) {
        filename <- names(sheet_select$sheet_name)[i]
        sheets <- sheet_select$sheet_name[[i]]
        ids <- sheet_select$sheet_id[[i]]

        for (j in seq_along(sheets)) {
            sh <- sheets[[j]]
            id <- ids[[j]]

            updateCheckboxInput(session, paste0("sheet_import_", id), value=TRUE)
        }
    }
})

# "Unselect all" button
observeEvent(input$sheets_unselect_all, {
    for (i in seq_along(sheet_select$sheet_name)) {
        filename <- names(sheet_select$sheet_name)[i]
        sheets <- sheet_select$sheet_name[[i]]
        ids <- sheet_select$sheet_id[[i]]

        for (j in seq_along(sheets)) {
            sh <- sheets[[j]]
            id <- ids[[j]]

            updateCheckboxInput(session, paste0("sheet_import_", id), value=FALSE)
        }
    }
})

# "Import selected" button
observeEvent(input$sheets_import_selected, {
    removeModal()

    # Remove files corresponding to unselected sheets
    selected_files <- list()
    for (file in data$files_all) {
        if (file$type == "csv") {
            selected_files <- c(selected_files, list(file))
        } else {
            selected <- input[[paste0("sheet_import_", file$id)]]
            if (selected) {
                selected_files <- c(selected_files, list(file))
            }
        }
    }
    data$files_selected <- selected_files
})

# Import the selected files
observeEvent(data$files_selected, ignoreNULL = TRUE, handlerExpr = {
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

    # Number of files to process
    nb_selected <- length(data$files_selected)
    if (nb_selected == 0) {
        showModal(modalDialog(
            tags$p("You did not select any data."),
            title = tagList(icon("times-circle"), tags$span("No data")),
            footer = modalButton("Close", icon=icon("times")),
            easyClose = TRUE
        ))

        hide_import_progressbar()
        shinyjs::show("help_intro")
        reset_app()

        return(NULL)
    }

    # Adapt the progress step to the remaining space in the progress bar
    step <- (import_progress$value_max - import_progress$value)/nb_selected

    list_data <- list()
    list_error <- list()
    data_size <- 0

    list_years <- c()
    list_countries <- c()
    list_components <- c()

    for (file in data$files_selected) {
        if (file$type == "csv") {
            # Update the message
            update_import_progressbar_message(paste("Importing file", file$filename))
            # Name of the file to be used in a sentence in case of error
            sentence_name <- paste0("The file “", file$filename, "”")
            # Read the file
            table <- tryCatch(suppressWarnings(read.csv(file$path,
                header = FALSE,
                stringsAsFactors = FALSE,
                sep = isolate(input$csv_input_field_separator),
                colClasses = "character"
            )), error = function(e) {
                return(simpleError(paste0(
                    sentence_name, " was ignored because of the following error: ", e$message, "."
                )))
            })
        } else {
            # Update the message
            update_import_progressbar_message(paste("Importing sheet", file$sheet, "of file", file$filename))
            # Name of the file to be used in a sentence in case of error
            sentence_name <- paste0("The sheet “", file$sheet, "” of “", file$filename, "”")
            # Read the file
            table <- tryCatch(as.data.frame(read_excel(
                path = file$path,
                sheet = file$sheet,
                col_names = FALSE
            )), error = function(e) {
                return(simpleError(paste0(
                    sentence_name, " was ignored because of the following error: ", e$message, "."
                )))
            })
        }
        if (is.error(table)) {
            list_error <- c(list_error, list(table))
        } else {
            dpcomma <- (isolate(input$csv_input_dec_separator) == ",")
            parsed_input <- parse_input(table, varnames, dpcomma)
            if (is.error(parsed_input)) {
                list_error <- c(list_error, list(simpleError(paste0(
                    sentence_name, " was ignored because of the following error: ", parsed_input$message, "."
                ))))
            } else {
                # Make sure there isn't already a file with the same year, country and component
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
                    list_error <- c(list_error, list(simpleError(paste0(
                        sentence_name, " was ignored because there is already a tabulation with year “", year, "”, ",
                        "country “", country, "” and component “", component, "”."
                    ))))
                } else {
                    # Make sure the tabulations are internally consistent
                    if (!is_input_consistent(parsed_input)) {
                        list_error <- c(list_error, list(simpleError(paste0(
                            sentence_name, " was ignored because it is logically inconsistent."
                        ))))
                    } else {
                        list_data[[year]][[country]][[component]] <- parsed_input
                        data_size <- data_size + 1
                    }
                }
            }
        }
        increase_import_progressbar_value(step)
    }

    data$input_data <- list_data
    data$input_error <- list_error
    data$input_data_size <- data_size
    data$input_years <- list_years
    data$input_countries <- list_countries
    data$input_components <- list_components

    hide_import_progressbar()
    shinyjs::show("input_data_view_header")
    shinyjs::show("input_data_view")

    if (data_size == 0) {
        showModal(modalDialog(
            title = tagList(icon("times-circle"), tags$span("Error")),
            footer = modalButton("Close", icon=icon("times")),
            easyClose = TRUE,
            tags$p("There is nothing to display because all of your files
                generated an error during the importation. Please check
                your input data and try again."),
            tags$div(
                tags$ul(lapply(data$input_error, function(e) {
                    tags$li(tags$i(class="fa fa-li fa-times-circle"), e$message)
                }), class="fa-ul"),
                style = "max-height: 400px; overflow: scroll;",
                class = "alert alert-danger",
                role = "alert"
            )
        ))
        reset_app()
    } else if (length(list_error) > 0) {
        showModal(modalDialog(
            title = tagList(icon("exclamation-triangle"), tags$span("Warning")),
            footer = modalButton("Close", icon=icon("times")),
            easyClose = TRUE,
            tags$p("Some of your files were ignored because of errors. You can
                    proceed nonetheless, but you may want to check some of your data."),
            tags$div(
                tags$ul(lapply(data$input_error, function(e) {
                    tags$li(tags$i(class="fa fa-li fa-exclamation-triangle"), e$message)
                }), class="fa-ul"),
                style = "max-height: 400px; overflow: scroll;",
                class = "alert alert-warning",
                role = "alert"
            )
        ))
    }
})

observeEvent(input$import_example, {
    data$input_data <- list("2010" = list(
        "US" = list("labor" = list(
                p = c(0.10, 0.50, 0.90, 0.99),
                year = 2010,
                country = "US",
                component = "labor",
                average = 37208.059,
                threshold = c(4130, 23686, 76252, 211861),
                whichavgsh = "bracketavg",
                bracketavg = c(12643.3, 43908.3, 108329.2, 471463.3),
                popsize = 2.257e+08,
                gumbel = NA,
                coupleshare = NA,
                singleshare = NA
            ), "capital" = list(
                p = c(0.10, 0.50, 0.90, 0.99),
                year = 2010,
                country = "US",
                component = "capital",
                average = 16370.471,
                threshold = c(-1176, 2780, 28939, 173917),
                whichavgsh = "bracketavg",
                bracketavg = c(328.6372, 10657.18, 59412.08, 688689.3),
                popsize = 2.257e+08,
                gumbel = NA,
                coupleshare = NA,
                singleshare = NA
            )
        ),
        "FR" = list("labor" = list(
            p = c(0, 0.26386523, 0.32899058, 0.43231946, 0.58923674, 0.76089203, 0.91522479, 0.98419398),
            year = 2010,
            country = "FR",
            component = "labor",
            average = 27727.8037383178,
            threshold = c(0, 10000, 12000, 15000, 20000, 30000, 50000, 100000)/0.856,
            whichavgsh = "bracketavg",
            bracketavg = c(4611, 11015, 13595, 17331, 24597, 38050, 65546, 193674)/0.856,
            popsize = 36962517,
            gumbel = NA,
            coupleshare = NA,
            singleshare = 0.64423762,
            whichcouple = "singlebracket",
            singlebracket = c(0.87221801, 0.87220490, 0.87220490, 0.75579684,
                0.55689047, 0.26108370, 0.14955399, 0.14955399)
        ))
    ))
    data$input_data_size  <- 2
    data$input_errors     <- list()
    data$input_years      <- c("2010")
    data$input_countries  <- c("US", "FR")
    data$input_components <- c("labor", "capital")
})

