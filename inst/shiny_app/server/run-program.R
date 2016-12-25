observeEvent(input$run, {
    # Determine the amount of computation to perform to properly display
    # the progress bar to the user
    progressmax <- 5*data$input_data_size

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
                    `aria-valuemax` = progressmax,
                    style = "width: 0%"
                ),
                class = "progress"
            ),
            tags$div(
                tags$p("The execution ended successfully. You can now explore
                    the different panels of the application with the results."),
                tags$table(
                    tags$tr(
                        tags$td(tags$i(class="fa fa-table fa-3x", `aria-hidden`="true")),
                        tags$td(tags$p("The", tags$b("Tables"), "tab provides detailed tabulations
                            of your data. You can pick which shares, quantiles and averages
                            you want to include, and download the result to your computer."
                        ))
                    ),
                    tags$tr(
                        tags$td(tags$i(class="fa fa-area-chart fa-3x", `aria-hidden`="true")),
                        tags$td(tags$p("The", tags$b("Plots"), "tab provides several plots to visualize
                            the distribution of your data. That includes the interpolated function,
                            but also the density or the Lorenz curve. The interface lets you
                            adjust the range to focus on specific parts of the distribution."
                        ))
                    ),
                    tags$tr(
                        tags$td(tags$i(class="fa fa-random fa-3x", `aria-hidden`="true")),
                        tags$td(tags$p("The", tags$b("Simulate"), "tab lets you simulate and download
                            new random samples of arbitrary size according to distribution
                            of your data."
                        ))
                    ),
                    tags$tr(
                        tags$td(tags$i(class="fa fa-stethoscope fa-3x", `aria-hidden`="true")),
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
    # List to store the tables we generated
    list_tables <- list()
    i <- 0
    for (year in data$input_years) {
        list_results[[year]] <- list()
        list_tables[[year]] <- list()
        for (country in data$input_countries) {
            list_results[[year]][[country]] <- list()
            list_tables[[year]][[country]] <- list()
            for (component in data$input_components) {
                data_model <- data$input_data[[year]][[country]][[component]]

                data_label <- c(component, country, year)
                data_label <- data_label[data_label != "n/a"]
                data_label <- paste(data_label, collapse=", ")

                # Move on to next loop if the data doesn't exist
                if (is.null(data_model)) {
                    next
                }

                # Update the status message in the dialog
                shinyjs::runjs(paste0("$('#run_status').html('<i class=\"fa fa-cog fa-spin fa-fw\"></i> ",
                    "Interpolating: ", data_label, "')"))

                result_model <- tryCatch({
                    args <- list(
                        p = data_model$p,
                        threshold = data_model$threshold,
                        average = data_model$average
                    )
                    avgsh <- data_model$whichavgsh
                    args[avgsh] <- data_model[avgsh]
                    result <- do.call(tabulation_fit, args)
                    result
                }, error = function(e) {
                    return(simpleError(e$message))
                })

                if (!is.error(result_model) & input$interpolation_options == "individualize") {
                    result_model <- tryCatch({
                        args <- list(
                            dist = result_model,
                            p = data_model$p,
                            coupleshare = data_model$coupleshare,
                            singleshare = data_model$singleshare
                        )
                        args[data_model$whichcouple] <- data_model[data_model$whichcouple]
                        result <- do.call(individualize_dist, args)
                        result
                    }, error = function(e) {
                        return(simpleError(e$message))
                    })
                }

                # If the program failed, stop and show the error to the user
                if (is.error(result_model)) {
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
                shinyjs::runjs(paste0("$('#run_progress').attr('style', 'width: ", 100*i/progressmax, "%')"))
            }
        }
    }

    results_years <- data$input_years
    results_countries <- data$input_countries
    results_components <- data$input_components

    # Merge countries if required
    if (input$interpolation_options == "merge") {
        list_results_merged <- list()
        shinyjs::runjs(paste0("$('#run_status').html('<i class=\"fa fa-cog fa-spin fa-fw\"></i> ",
            "Merging countries')"))
        for (year in results_years) {
            list_results_merged[[year]] <- list("merged"=list())
            for (component in results_components) {
                list_dist <- list()
                popsize <- c()
                for (country in results_countries) {
                    dist <- list_results[[year]][[country]][[component]]
                    if (!is.null(dist)) {
                        list_dist <- c(list_dist, list(dist))
                        popsize <- c(popsize, data$data[[year]][[country]][[component]]$popsize)
                    }
                }
                merged_dist <- tryCatch(merge_dist(list_dist, popsize), error = function(e) {
                    return(simpleError(e$message))
                })
                if (is.error(merged_dist)) {
                    shinyjs::show("failure_message")
                    shinyjs::show("dismiss_run_failure")
                    shinyjs::runjs(paste0("$('#run_status').html('<i class=\"fa fa-frown-o\" aria-hidden=\"true\"></i> Something went wrong.')"))

                    shinyjs::runjs(paste0("$('#error_message1').text('An error occurred while mergeing countries.",
                        "Please check your data.')"))
                    # Sanitize & display error message
                    msg <- merged_dist$message
                    msg <- gsub("\n", "", msg, fixed=TRUE)
                    msg <- gsub("'", "\\'", msg, fixed=TRUE)
                    shinyjs::runjs(paste0("$('#error_message2').html('<i class=\"fa fa-exclamation-circle\" aria-hidden=\"true\"></i> &nbsp; ", msg, "')"))
                    shinyjs::removeClass("run_progress", "active")

                    # Clear the results
                    data$results <- NULL

                    return(NULL)
                }
                list_results_merged[[year]][["merged"]][[component]] <- merged_dist
            }
        }
        list_results <- list_results_merged
        results_countries <- "merged"
    }

    # Add up components if required
    if (input$interpolation_options == "addup") {
        list_results_addedup <- list()
        shinyjs::runjs(paste0("$('#run_status').html('<i class=\"fa fa-cog fa-spin fa-fw\"></i> ",
            "Adding up components')"))
        for (year in results_years) {
            list_results_addedup[[year]] <- list()
            for (country in results_countries) {
                list_dist <- list()
                for (component in results_components) {
                    dist <- list_results[[year]][[country]][[component]]
                    list_dist <- c(list_dist, list(dist))
                }
                # Error if the user has not exactly two components to add up
                if (length(list_dist) != 2) {
                    shinyjs::show("failure_message")
                    shinyjs::show("dismiss_run_failure")
                    shinyjs::runjs(paste0("$('#run_status').html('<i class=\"fa fa-frown-o\" aria-hidden=\"true\"></i> Something went wrong.')"))

                    shinyjs::runjs("$('#error_message1').text('You may only add up exactly two components.')")
                    # Sanitize & display error message
                    msg <- paste0("Country ", country, " in year ", year, " has ", length(list_dist), " component(s).")
                    shinyjs::runjs(paste0("$('#error_message2').html('<i class=\"fa fa-exclamation-circle\" aria-hidden=\"true\"></i> &nbsp; ", msg, "')"))
                    shinyjs::removeClass("run_progress", "active")

                    # Clear the results
                    data$results <- NULL

                    return(NULL)
                }
                # Get the Gumbel copula parameter
                if (!is.null(list_dist[[1]]$gumbel) && !is.null(list_dist[[2]]$gumbel)) {
                    if (list_dist[[1]]$gumbel != list_dist[[2]]$gumbel) {
                        shinyjs::show("failure_message")
                        shinyjs::show("dismiss_run_failure")
                        shinyjs::runjs(paste0("$('#run_status').html('<i class=\"fa fa-frown-o\" aria-hidden=\"true\"></i> Something went wrong.')"))

                        shinyjs::runjs("$('#error_message1').text('You may not have two values for the Gumbel parameter.')")
                        # Sanitize & display error message
                        msg <- paste0("There are two different values of the Gumbel parameter in country ", country, " for year ", year, ".")
                        shinyjs::runjs(paste0("$('#error_message2').html('<i class=\"fa fa-exclamation-circle\" aria-hidden=\"true\"></i> &nbsp; ", msg, "')"))
                        shinyjs::removeClass("run_progress", "active")

                        # Clear the results
                        data$results <- NULL

                        return(NULL)
                    } else {
                        theta <- list_dist[[1]]$gumbel
                    }
                } else if (!is.null(list_dist[[1]]$gumbel)) {
                    theta <- list_dist[[1]]$gumbel
                } else if (!is.null(list_dist[[2]]$gumbel)) {
                    theta <- list_dist[[2]]$gumbel
                } else {
                    theta <- isolate(input$gumbel_param)
                }
                addedup_dist <- tryCatch(addup_dist(list_dist[[1]], list_dist[[2]], theta), error = function(e) {
                    return(simpleError(e$message))
                })
                if (is.error(addedup_dist)) {
                    shinyjs::show("failure_message")
                    shinyjs::show("dismiss_run_failure")
                    shinyjs::runjs(paste0("$('#run_status').html('<i class=\"fa fa-frown-o\" aria-hidden=\"true\"></i> Something went wrong.')"))

                    shinyjs::runjs(paste0("$('#error_message1').text('An error occurred while adding up components. ",
                        "Please check your data.')"))
                    # Sanitize & display error message
                    msg <- addedup_dist$message
                    msg <- gsub("\n", "", msg, fixed=TRUE)
                    msg <- gsub("'", "\\'", msg, fixed=TRUE)
                    shinyjs::runjs(paste0("$('#error_message2').html('<i class=\"fa fa-exclamation-circle\" aria-hidden=\"true\"></i> &nbsp; ", msg, "')"))
                    shinyjs::removeClass("run_progress", "active")

                    # Clear the results
                    data$results <- NULL

                    return(NULL)
                }
                list_results_addedup[[year]][[country]][["added up"]] <- addedup_dist
            }
        }
        list_results <- list_results_addedup
        results_components <- "added up"
    }

    # Count the number of tabulations to generate
    progressmax2 <- 0
    for (year in results_years) {
        for (country in results_countries) {
            for (component in results_components) {
                result <- list_results[[year]][[country]][[component]]
                if (!is.null(result)) {
                    progressmax2 <- progressmax2 + 1
                }
            }
        }
    }
    # At each step, we know increase the progress bar by:
    progress_step <- (progressmax - i)/progressmax2

    # Create the tabulations
    list_tables <- list()
    for (year in results_years) {
        list_tables[[year]] <- list()
        for (country in results_countries) {
            list_tables[[year]][[country]] <- list()
            for (component in results_components) {
                result <- list_results[[year]][[country]][[component]]
                if (!is.null(result)) {
                    # Update the status message in the dialog
                    table_label <- c(component, country, year)
                    table_label <- table_label[!table_label %in% c("n/a", "merged", "added up")]
                    table_label <- paste(table_label, collapse=", ")
                    shinyjs::runjs(paste0("$('#run_status').html('<i class=\"fa fa-cog fa-spin fa-fw\"></i> ",
                        "Generating table: ", table_label, "')"))

                    table <- as.list(generate_tabulation(result, gperc))
                    table$bottom50 <- bottom_share(result, 0.5)
                    table$middle40 <- bracket_share(result, 0.5, 0.9)
                    table$top10 <- top_share(result, 0.9)
                    table$top1 <- top_share(result, 0.99)
                    table$gini <- gini(result)
                    list_tables[[year]][[country]][[component]] <- table

                    # Update the progress bar
                    i <- i + progress_step
                    shinyjs::runjs(paste0("$('#run_progress').attr('aria-valuenow',", i, ")"))
                    shinyjs::runjs(paste0("$('#run_progress').attr('style', 'width: ", 100*i/progressmax, "%')"))
                }
            }
        }
    }

    # Update the status message to show success
    shinyjs::runjs(paste0("$('#run_progress').attr('aria-valuenow',", progressmax, ")"))
    shinyjs::runjs(paste0("$('#run_progress').attr('style', 'width: 100%')"))
    shinyjs::runjs(paste0("$('#run_status').html('<i class=\"fa fa-thumbs-up\" aria-hidden=\"true\"></i> All done!')"))
    shinyjs::removeClass("run_progress", "active")

    shinyjs::show("success_message")
    shinyjs::show("dismiss_run_success")

    # Store the results
    data$output_dist <- list_results
    data$output_tables <- list_tables
    data$output_years <- results_years
    data$output_countries <- results_countries
    data$output_components <- results_components

    # Update the interface
    updateSelectInput(session, "output_table_year", choices=data$output_years)
    updateSelectInput(session, "output_dist_plot_year", choices=data$output_years)
    updateSelectInput(session, "synthpop_year", choices=data$output_years)
    if (length(data$output_years) > 1) {
        enable("output_table_year")
        enable("output_dist_plot_year")
        enable("synthpop_year_all")
        if (!input$synthpop_year_all) {
            enable("synthpop_year")
        }
    } else {
        disable("output_table_year")
        disable("output_dist_plot_year")
        disable("synthpop_year_all")
    }
    updateSelectInput(session, "output_table_country", choices=data$output_countries)
    updateSelectInput(session, "output_dist_plot_country", choices=data$output_countries)
    updateSelectInput(session, "output_time_plot_country", choices=data$output_countries)
    updateSelectInput(session, "synthpop_country", choices=data$output_countries)
    if (length(data$output_countries) > 1) {
        enable("output_table_country")
        enable("output_dist_plot_country")
        enable("output_time_plot_country")
        enable("synthpop_country_all")
        if (!input$synthpop_country_all) {
            enable("synthpop_country")
        }
    } else {
        disable("output_table_country")
        disable("output_dist_plot_country")
        disable("output_time_plot_country")
        disable("synthpop_country_all")
    }
    updateSelectInput(session, "output_table_component", choices=data$output_components)
    updateSelectInput(session, "output_dist_plot_component", choices=data$output_components)
    updateSelectInput(session, "output_time_plot_component", choices=data$output_components)
    updateSelectInput(session, "synthpop_component", choices=data$output_components)
    if (length(data$output_components) > 1) {
        enable("output_table_component")
        enable("output_dist_plot_component")
        enable("output_time_plot_component")
        enable("synthpop_component_all")
        if (!input$synthpop_component_all) {
            enable("synthpop_component")
        }
    } else {
        disable("output_table_component")
        disable("output_dist_plot_component")
        disable("output_time_plot_component")
        disable("synthpop_component_all")
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
