# Global values to monitor the advancement of the import process
run_progress <- list(
    value     = 0,
    value_max = 100
)

update_run_progressbar_max <- function(n) {
    run_progress$value_max <<- n
    i <- run_progress$value

    shinyjs::runjs(paste0("$('#run_progress').attr('aria-valuemax',", n, ")"))
    shinyjs::runjs(paste0("$('#run_progress').attr('style', 'width: ", 100*i/n, "%')"))
}

update_run_progressbar_value <- function(i) {
    run_progress$value <<- i
    n <- run_progress$value_max

    shinyjs::runjs(paste0("$('#run_progress').attr('aria-valuenow',", i, ")"))
    shinyjs::runjs(paste0("$('#run_progress').attr('style', 'width: ", 100*i/n, "%')"))
}

increase_run_progressbar_value <- function(i) {
    update_run_progressbar_value(run_progress$value + i)
}

update_run_progressbar_message <- function(msg) {
    msg <- gsub("\n", "", msg, fixed=TRUE)
    msg <- gsub("'", "\\'", msg, fixed=TRUE)
    shinyjs::runjs(paste0("$('#import_run_message').text('", msg, "')"))
}

show_run_modal <- function() {
    # Show a modal dialog with a custom progress bar
    showModal(modalDialog(
        tags$div(
            tags$h4(
                tags$i(class="fa fa-cog fa-fw", id="cog_running"),
                tags$span(id="import_run_message"),
                style="text-align: center;"
            ),
            tags$hr(),
            tags$div(
                tags$div(
                    id = "run_progress",
                    class = "progress-bar progress-bar-striped",
                    role = "progressbar",
                    `aria-valuenow` = "0",
                    `aria-valuemin` = "0",
                    `aria-valuemax` = run_progress$value_max,
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
                    #tags$tr(
                    #    tags$td(tags$i(class="fa fa-stethoscope fa-3x", `aria-hidden`="true")),
                    #    tags$td(tags$p("The", tags$b("Diagnostic"), "tab can help you identify pathological
                    #        features of your data which may indicate mistakes or inconsistencies."
                    #    ))
                    #),
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
}

show_success <- function() {
    update_run_progressbar_message("All done!")
    shinyjs::removeClass("cog_running", "fa-cog")
    shinyjs::addClass("cog_running", "fa-thumbs-up")
    shinyjs::addClass("run_progress", "progress-bar-success")

    shinyjs::show("success_message")
    shinyjs::show("dismiss_run_success")
}

show_failure <- function(data_label, msg) {
    shinyjs::show("failure_message")
    shinyjs::show("dismiss_run_failure")

    update_run_progressbar_message("Something went wrong.")
    shinyjs::removeClass("cog_running", "fa-cog")
    shinyjs::addClass("cog_running", "fa-frown-o")
    shinyjs::addClass("run_progress", "progress-bar-danger")

    shinyjs::runjs(paste0("$('#error_message1').text('An error occurred while working on “", data_label, "”. ",
        "Please check the consistency of your data.')"))

    # Sanitize & display error message
    msg <- gsub("\n", "", msg, fixed=TRUE)
    msg <- gsub("'", "\\'", msg, fixed=TRUE)
    shinyjs::runjs(paste0("$('#error_message2').html('<i class=\"fa fa-exclamation-circle\" aria-hidden=\"true\"></i> &nbsp; ", msg, "')"))
    shinyjs::removeClass("run_progress", "active")
}

set_active <- function(active) {
    if (active) {
        shinyjs::addClass("cog_running", "fa-spin")
        shinyjs::addClass("run_progress", "active")
    } else {
        shinyjs::removeClass("cog_running", "fa-spin")
        shinyjs::removeClass("run_progress", "active")
    }
}

make_tabulation <- function(result) {
    table <- as.list(generate_tabulation(result, gperc))
    table$bottom50 <- bottom_share(result, 0.5)
    table$middle40 <- bracket_share(result, 0.5, 0.9)
    table$top10 <- top_share(result, 0.9)
    table$top1 <- top_share(result, 0.99)
    table$gini <- gini(result)
    return(table)
}

interpolate_only <- function() {
    run_progress$value <<- 0
    run_progress$value_max <<- data$input_data_size

    show_run_modal()
    set_active(TRUE)

    results_years <- data$input_years
    results_countries <- data$input_countries
    results_components <- data$input_components
    # List to store the results
    list_results <- list()
    # List to store the tables we generated
    list_tables <- list()
    for (year in data$input_years) {
        list_results[[year]] <- list()
        list_tables[[year]] <- list()
        for (country in data$input_countries) {
            list_results[[year]][[country]] <- list()
            list_tables[[year]][[country]] <- list()
            for (component in data$input_components) {
                data_model <- data$input_data[[year]][[country]][[component]]

                # Move on to next loop if the data doesn't exist
                if (is.null(data_model)) {
                    next
                }

                data_label <- c(component, country, year)
                data_label <- data_label[data_label != "n/a"]
                data_label <- paste(data_label, collapse=" – ")

                update_run_progressbar_message(paste(data_label))

                if (is.na(data_model$threshold[1])) {
                    result_model <- tryCatch({
                        args <- list(
                            p = data_model$p,
                            average = data_model$average
                        )
                        avgsh <- data_model$whichavgsh
                        args[avgsh] <- data_model[avgsh]
                        if (!is.na(data_model$lowerbound)) {
                            if (min(data_model$p) == 0) {
                                args["first_threshold"] <- data_model$lowerbound
                            } else {
                                args["lower_bound"] <- data_model$lowerbound
                            }
                        }
                        result <- do.call(shares_fit, args)
                        result
                    }, error = function(e) {
                        return(simpleError(e$message))
                    })
                } else if (is.na(data_model$whichavgsh)) {
                    result_model <- tryCatch({
                        args <- list(
                            p = data_model$p,
                            threshold = data_model$threshold,
                            average = data_model$average
                        )
                        if (!is.na(data_model$lowerbound)) {
                            args["lower_bound"] <- data_model$lowerbound
                        }
                        result <- do.call(thresholds_fit, args)
                        result
                    }, error = function(e) {
                        return(simpleError(e$message))
                    })
                } else {
                    result_model <- tryCatch({
                        args <- list(
                            p = data_model$p,
                            threshold = data_model$threshold,
                            average = data_model$average
                        )
                        avgsh <- data_model$whichavgsh
                        args[avgsh] <- data_model[avgsh]
                        if (!is.na(data_model$lowerbound)) {
                            args["lower_bound"] <- data_model$lowerbound
                        }
                        result <- do.call(tabulation_fit, args)
                        result
                    }, error = function(e) {
                        return(simpleError(e$message))
                    })
                }
                increase_run_progressbar_value(1/2)

                if (is.error(result_model)) {
                    set_active(FALSE)
                    show_failure(data_label, result_model$message)

                    # Clear the results
                    data$results <- NULL

                    # Abandon
                    return(NULL)
                } else {
                    list_results[[year]][[country]][[component]] <- result_model

                    # Generate the tabulation
                    list_tables[[year]][[country]][[component]] <- make_tabulation(result_model)

                    increase_run_progressbar_value(1/2)
                }
            }
        }
    }

    set_active(FALSE)
    show_success()

    # Store the results
    data$output_dist <- list_results
    data$output_tables <- list_tables
    data$output_years <- results_years
    data$output_countries <- results_countries
    data$output_components <- results_components
}

interpolate_and_individualize <- function() {
    run_progress$value <<- 0
    run_progress$value_max <<- data$input_data_size

    show_run_modal()
    set_active(TRUE)

    results_years <- data$input_years
    results_countries <- data$input_countries
    results_components <- data$input_components
    # List to store the results
    list_results <- list()
    # List to store the tables we generated
    list_tables <- list()
    for (year in data$input_years) {
        list_results[[year]] <- list()
        list_tables[[year]] <- list()
        for (country in data$input_countries) {
            list_results[[year]][[country]] <- list()
            list_tables[[year]][[country]] <- list()
            for (component in data$input_components) {
                data_model <- data$input_data[[year]][[country]][[component]]

                # Move on to next loop if the data doesn't exist
                if (is.null(data_model)) {
                    next
                }

                data_label <- c(component, country, year)
                data_label <- data_label[data_label != "n/a"]
                data_label <- paste(data_label, collapse=" – ")

                update_run_progressbar_message(paste(data_label))

                if (is.na(data_model$threshold[1])) {
                    result_model <- tryCatch({
                        args <- list(
                            p = data_model$p,
                            average = data_model$average
                        )
                        avgsh <- data_model$whichavgsh
                        args[avgsh] <- data_model[avgsh]
                        if (!is.na(data_model$lowerbound)) {
                            if (min(data_model$p) == 0) {
                                args["first_threshold"] <- data_model$lowerbound
                            } else {
                                args["lower_bound"] <- data_model$lowerbound
                            }
                        }
                        result <- do.call(shares_fit, args)
                        result
                    }, error = function(e) {
                        return(simpleError(e$message))
                    })
                } else {
                    result_model <- tryCatch({
                        args <- list(
                            p = data_model$p,
                            threshold = data_model$threshold,
                            average = data_model$average
                        )
                        avgsh <- data_model$whichavgsh
                        args[avgsh] <- data_model[avgsh]
                        if (!is.na(data_model$lowerbound)) {
                            args["lower_bound"] <- data_model$lowerbound
                        }
                        result <- do.call(tabulation_fit, args)
                        result
                    }, error = function(e) {
                        return(simpleError(e$message))
                    })
                }
                increase_run_progressbar_value(1/5)

                if (is.error(result_model)) {
                    set_active(FALSE)
                    show_failure(data_label, result_model$message)

                    # Clear the results
                    data$results <- NULL

                    # Abandon
                    return(NULL)
                } else {
                    list_results[[year]][[country]][[component]] <- result_model

                    # Generate the tabulation
                    table <- as.list(generate_tabulation(result, gperc))
                    table$bottom50 <- bottom_share(result, 0.5)
                    table$middle40 <- bracket_share(result, 0.5, 0.9)
                    table$top10 <- top_share(result, 0.9)
                    table$top1 <- top_share(result, 0.99)
                    table$gini <- gini(result)
                    list_tables[[year]][[country]][[component]] <- table

                    increase_run_progressbar_value(1/5)
                }

                # Individualize the data, if possible
                if (is.null(data_model$whichcouple)) {
                    increase_run_progressbar_value(3/5)
                } else {
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
                    increase_run_progressbar_value(1/5)

                    if (is.error(result_model)) {
                        set_active(FALSE)
                        show_failure(data_label, result_model$message)

                        # Clear the results
                        data$results <- NULL

                        # Abandon
                        return(NULL)
                    } else {
                        country_singles <- paste(country, "– singles")
                        country_couples <- paste(country, "– couples")
                        country_equalsplit <- paste(country, "– equal split")
                        if (!country_singles %in% results_countries) {
                            results_countries <- c(country_singles, results_countries)
                        }
                        list_results[[year]][[country_singles]][[component]] <- result_model$singles$dist
                        if (!country_couples %in% results_countries) {
                            results_countries <- c(country_couples, results_countries)
                        }
                        list_results[[year]][[country_couples]][[component]] <- result_model$couples$dist
                        if (!country_equalsplit %in% results_countries) {
                            results_countries <- c(country_equalsplit, results_countries)
                        }
                        list_results[[year]][[country_equalsplit]][[component]] <- result_model

                        # Generate the three additional tabulations
                        list_tables[[year]][[country_singles]][[component]] <- make_tabulation(result_model$singles$dist)
                        list_tables[[year]][[country_couples]][[component]] <- make_tabulation(result_model$couples$dist)
                        increase_run_progressbar_value(1/5)

                        list_tables[[year]][[country_equalsplit]][[component]] <- make_tabulation(result_model)
                        increase_run_progressbar_value(1/5)
                    }
                }
            }
        }
    }

    set_active(FALSE)
    show_success()

    # Store the results
    data$output_dist <- list_results
    data$output_tables <- list_tables
    data$output_years <- results_years
    data$output_countries <- results_countries
    data$output_components <- results_components
}

interpolate_and_merge <- function() {
    run_progress$value <<- 0
    run_progress$value_max <<- data$input_data_size

    show_run_modal()
    set_active(TRUE)

    has_merged <- FALSE

    results_years <- data$input_years
    results_countries <- data$input_countries
    results_components <- data$input_components
    # List to store the results
    list_results <- list()
    # List to store the tables we generated
    list_tables <- list()
    for (year in data$input_years) {
        list_results[[year]] <- list()
        list_tables[[year]] <- list()
        for (component in data$input_components) {
            models_to_merge <- list()
            populations <- c()
            for (country in data$input_countries) {
                data_model <- data$input_data[[year]][[country]][[component]]

                # Move on to next loop if the data doesn't exist
                if (is.null(data_model)) {
                    next
                }

                data_label <- c(component, country, year)
                data_label <- data_label[data_label != "n/a"]
                data_label <- paste(data_label, collapse=" – ")

                update_run_progressbar_message(paste(data_label))

                if (is.na(data_model$threshold[1])) {
                    result_model <- tryCatch({
                        args <- list(
                            p = data_model$p,
                            average = data_model$average
                        )
                        avgsh <- data_model$whichavgsh
                        args[avgsh] <- data_model[avgsh]
                        if (!is.na(data_model$lowerbound)) {
                            if (min(data_model$p) == 0) {
                                args["first_threshold"] <- data_model$lowerbound
                            } else {
                                args["lower_bound"] <- data_model$lowerbound
                            }
                        }
                        result <- do.call(shares_fit, args)
                        result
                    }, error = function(e) {
                        return(simpleError(e$message))
                    })
                } else {
                    result_model <- tryCatch({
                        args <- list(
                            p = data_model$p,
                            threshold = data_model$threshold,
                            average = data_model$average
                        )
                        avgsh <- data_model$whichavgsh
                        args[avgsh] <- data_model[avgsh]
                        if (!is.na(data_model$lowerbound)) {
                            args["lower_bound"] <- data_model$lowerbound
                        }
                        result <- do.call(tabulation_fit, args)
                        result
                    }, error = function(e) {
                        return(simpleError(e$message))
                    })
                }
                increase_run_progressbar_value(1/2)

                if (is.error(result_model)) {
                    set_active(FALSE)
                    show_failure(data_label, result_model$message)

                    # Clear the results
                    data$results <- NULL

                    # Abandon
                    return(NULL)
                } else {
                    list_results[[year]][[country]][[component]] <- result_model

                    models_to_merge <- c(models_to_merge, list(result_model))
                    populations <- c(populations, data_model$popsize)

                    # Generate the tabulation
                    list_tables[[year]][[country]][[component]] <- make_tabulation(result_model)

                    increase_run_progressbar_value(1/2)
                }
            }
            data_label <- c(component, year)
            data_label <- data_label[data_label != "n/a"]
            data_label <- paste(data_label, collapse=" – ")
            # Merge the models
            update_run_progressbar_message(paste("Merging:", data_label))

            result_model <- tryCatch(merge_dist(models_to_merge, populations), error = function(e) {
                return(simpleError(e$message))
            })

            if (is.error(result_model)) {
                set_active(FALSE)
                show_failure(data_label, result_model$message)

                # Clear the results
                data$results <- NULL

                # Abandon
                return(NULL)
            } else {
                has_merged <- TRUE
                list_results[[year]][["merged"]][[component]] <- result_model
                list_tables[[year]][["merged"]][[component]] <- make_tabulation(result_model)
            }
        }
    }

    set_active(FALSE)
    show_success()

    # Store the results
    data$output_dist <- list_results
    data$output_tables <- list_tables
    data$output_years <- results_years
    if (has_merged) {
        results_countries <- c(results_countries, "merged")
    }
    data$output_countries <- results_countries
    data$output_components <- results_components
}

interpolate_and_addup <- function() {
    run_progress$value <<- 0
    run_progress$value_max <<- data$input_data_size

    show_run_modal()
    set_active(TRUE)

    has_added_up <- FALSE

    results_years <- data$input_years
    results_countries <- data$input_countries
    results_components <- data$input_components
    # List to store the results
    list_results <- list()
    # List to store the tables we generated
    list_tables <- list()
    for (year in data$input_years) {
        list_results[[year]] <- list()
        list_tables[[year]] <- list()
        for (country in data$input_countries) {
            list_results[[year]][[country]] <- list()
            list_tables[[year]][[country]] <- list()
            components_to_add_up <- list()
            gumbel_parameters <- c()
            for (component in data$input_components) {
                data_model <- data$input_data[[year]][[country]][[component]]

                # Move on to next loop if the data doesn't exist
                if (is.null(data_model)) {
                    next
                }

                data_label <- c(component, country, year)
                data_label <- data_label[data_label != "n/a"]
                data_label <- paste(data_label, collapse=" – ")

                update_run_progressbar_message(paste(data_label))

                if (is.na(data_model$threshold[1])) {
                    result_model <- tryCatch({
                        args <- list(
                            p = data_model$p,
                            average = data_model$average
                        )
                        avgsh <- data_model$whichavgsh
                        args[avgsh] <- data_model[avgsh]
                        if (!is.na(data_model$lowerbound)) {
                            if (min(data_model$p) == 0) {
                                args["first_threshold"] <- data_model$lowerbound
                            } else {
                                args["lower_bound"] <- data_model$lowerbound
                            }
                        }
                        result <- do.call(shares_fit, args)
                        result
                    }, error = function(e) {
                        return(simpleError(e$message))
                    })
                } else {
                    result_model <- tryCatch({
                        args <- list(
                            p = data_model$p,
                            threshold = data_model$threshold,
                            average = data_model$average
                        )
                        avgsh <- data_model$whichavgsh
                        args[avgsh] <- data_model[avgsh]
                        if (!is.na(data_model$lowerbound)) {
                            args["lower_bound"] <- data_model$lowerbound
                        }
                        result <- do.call(tabulation_fit, args)
                        result
                    }, error = function(e) {
                        return(simpleError(e$message))
                    })
                }
                increase_run_progressbar_value(1/2)

                if (is.error(result_model)) {
                    set_active(FALSE)
                    show_failure(data_label, result_model$message)

                    # Clear the results
                    data$results <- NULL

                    # Abandon
                    return(NULL)
                } else {
                    list_results[[year]][[country]][[component]] <- result_model

                    components_to_add_up <- c(components_to_add_up, list(result_model))
                    gumbel_parameters <- c(gumbel_parameters, data_model$gumbel)

                    # Generate the tabulation
                    list_tables[[year]][[country]][[component]] <- make_tabulation(result_model)

                    increase_run_progressbar_value(1/2)
                }
            }
            # Add up components
            data_label <- c(country, year)
            data_label <- data_label[data_label != "n/a"]
            data_label <- paste(data_label, collapse=" – ")

            if (length(components_to_add_up) < 2) {
                list_results[[year]][[country]][["added up"]] <-
                    list_results[[year]][[country]][[component]]
                list_tables[[year]][[country]][["added up"]] <-
                    list_tables[[year]][[country]][[component]]
            } else if (length(components_to_add_up) > 2) {
                set_active(FALSE)
                show_failure(data_label, "You may only add up two components exactly.")

                # Clear the results
                data$results <- NULL

                # Abandon
                return(NULL)
            } else {
                if (all(is.na(gumbel_parameters))) {
                    theta <- isolate(input$gumbel_param)
                } else if (min(gumbel_parameters, na.rm=TRUE) != max(gumbel_parameters, na.rm=TRUE)) {
                    set_active(FALSE)
                    show_failure(data_label, "The Gumbel parameter must be the same for both distributions.")

                    # Clear the results
                    data$results <- NULL

                    # Abandon
                    return(NULL)
                } else {
                    theta <- min(gumbel_parameters, na.rm=TRUE)
                }
                data_label <- c(country, year)
                data_label <- data_label[data_label != "n/a"]
                data_label <- paste(data_label, collapse=" – ")
                # Merge the models
                update_run_progressbar_message(paste("Adding up:", data_label))

                addedup_dist <- tryCatch(addup_dist(components_to_add_up[[1]], components_to_add_up[[2]], theta), error = function(e) {
                    return(simpleError(e$message))
                })
                if (is.error(addedup_dist)) {
                    set_active(FALSE)
                    show_failure(data_label, addedup_dist$message)

                    # Clear the results
                    data$results <- NULL

                    # Abandon
                    return(NULL)
                }
                has_added_up <- TRUE
                list_results[[year]][[country]][["added up"]] <- addedup_dist
                list_tables[[year]][[country]][["added up"]] <- make_tabulation(addedup_dist)
            }
        }
    }

    set_active(FALSE)
    show_success()

    # Store the results
    data$output_dist <- list_results
    data$output_tables <- list_tables
    data$output_years <- results_years
    data$output_countries <- results_countries
    if (has_added_up) {
        results_components <- c(results_components, "added up")
    }
    data$output_components <- results_components
}

observeEvent(input$run, {
    if (input$interpolation_options == "basic") {
        interpolate_only()
    } else if (input$interpolation_options == "individualize") {
        interpolate_and_individualize()
    } else if (input$interpolation_options == "merge") {
        interpolate_and_merge()
    } else if (input$interpolation_options == "addup") {
        interpolate_and_addup()
    }

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
