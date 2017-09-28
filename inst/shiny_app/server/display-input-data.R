output$input_data_view_header <- renderUI({
    if (is.null(data$input_data)) {
        shinyjs::show("help_intro")
        return(NULL)
    } else {
        shinyjs::hide("help_intro")

        # Select input menu
        select_input_menu <- tagList()
        if (length(data$input_years) > 1) {
            select_input_menu <- tagList(select_input_menu,
                column(4, selectInput("input_view_year", "Year", choices=data$input_years, width="100%"))
            )
        } else {
            select_input_menu <- tagList(select_input_menu,
                disabled(column(4, selectInput("input_view_year", "Year", choices=data$input_years, width="100%")))
            )
        }

        if (length(data$input_countries) > 1) {
            select_input_menu <- tagList(select_input_menu,
                column(4, selectInput("input_view_country", "Country", choices=data$input_countries, width="100%"))
            )
        } else {
            select_input_menu <- tagList(select_input_menu,
                disabled(column(4, selectInput("input_view_country", "Country", choices=data$input_countries, width="100%")))
            )
        }

        if (length(data$input_components) > 1) {
            select_input_menu <- tagList(select_input_menu,
                column(4, selectInput("input_view_component", "Component", choices=data$input_components, width="100%"))
            )
        } else {
            select_input_menu <- tagList(select_input_menu,
                disabled(column(4, selectInput("input_view_component", "Component", choices=data$input_components, width="100%")))
            )
        }

        return(tagList(
            fixedRow(
                column(6, actionButton("run", "Run", icon=icon("play"), class="btn-block btn-success")),
                column(6, actionButton("clear", "Clear data", icon=icon("eraser"), class="btn-block btn-danger")),
                style = "margin-bottom: 20px;"
            ),
            fixedRow(select_input_menu)
        ))
    }
})

output$input_data_view <- renderUI({
    year <- input$input_view_year
    country <- input$input_view_country
    component <- input$input_view_component

    if ((length(data$input_data) == 0) || (is.null(year)) || (is.null(country)) || (is.null(component))) {
        return(NULL)
    }

    data_view <- data$input_data[[year]][[country]][[component]]

    if (is.null(data_view)) {
        return(tags$div(
            tags$p(icon("info-circle"), "No data available for your selection."),
            class="alert alert-info", role="alert"
        ))
    }

    df <- data.frame(
        "Fractiles" = sprintf("%1.5f", data_view$p),
        "Thresholds" = format(round(data_view$threshold), big.mark=" ", scientific=FALSE)
    )
    if (is.na(data_view$threshold[1])) {
        df[, "Thresholds"] <- NA
    }

    if (!is.na(data_view$whichavgsh)) {
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

        if (avgsh %in% c("bracketavg", "topavg")) {
            df[avgsh_clean] <- format(round(data_view[[avgsh]]), big.mark=" ", scientific=FALSE)
        } else {
            df[avgsh_clean] <- sprintf("%.3f", data_view[[avgsh]])
        }
        df[is.na(data_view[[avgsh]]), avgsh_clean] <- NA
    }

    # Couple/single share for this data
    if (!is.null(data_view$whichcouple)) {
        couplevar <- data_view$whichcouple
        if (couplevar == "singletop") {
            couplevar_clean <- "Top single share"
        } else if (couplevar == "singlebracket") {
            couplevar_clean <- "Bracket single share"
        } else if (couplevar == "coupletop") {
            couplevar_clean <- "Top couple share"
        } else if (couplevar == "couplebracket") {
            couplevar_clean <- "Bracket couple share"
        }
        if (!is.null(couplevar) & !is.na(couplevar)) {
            df[couplevar_clean] <- sprintf("%.3f", data_view[[couplevar]])
            df[is.na(data_view[[couplevar]]), couplevar_clean] <- NA
        }
    }

    return(tagList(
        tags$h4("Summary"),
        tags$table(
            tags$tbody(
                tags$tr(
                    tags$th("Average", style="white-space: nowrap;"),
                    tags$td(ifelse(is.na(data_view$average),
                        "n/a",
                        format(round(data_view$average), big.mark=" ", scientific=FALSE)
                    ), style = ifelse(
                        is.na(data_view$average),
                        "color: #999; text-align: right;", "text-align: right;"
                    ))
                ),
                tags$tr(
                    tags$th("Overall single share", style="white-space: nowrap;"),
                    tags$td(ifelse(is.na(data_view$singleshare),
                        "n/a",
                        format(data_view$singleshare, digits=2, scientific=FALSE)
                    ), style = ifelse(
                        is.na(data_view$singleshare),
                        "color: #999; text-align: right;", "text-align: right;"
                    ))
                ),
                tags$tr(
                    tags$th("Overall couple share", style="white-space: nowrap;"),
                    tags$td(ifelse(is.na(data_view$coupleshare),
                        "n/a",
                        format(data_view$coupleshare, digits=2, scientific=FALSE)
                    ), style = ifelse(
                        is.na(data_view$coupleshare),
                        "color: #999; text-align: right;", "text-align: right;"
                    ))
                ),
                tags$tr(
                    tags$th("Population size", style="white-space: nowrap;"),
                    tags$td(ifelse(is.na(data_view$popsize),
                        "n/a",
                        format(round(data_view$popsize), scientific=FALSE, big.mark=" ")
                    ), style = ifelse(
                        is.na(data_view$popsize),
                        "color: #999; text-align: right;", "text-align: right;"
                    ))
                ),
                tags$tr(
                    tags$th("Gumbel parameter", style="white-space: nowrap;"),
                    tags$td(ifelse(is.na(data_view$gumbel),
                        "n/a",
                        format(data_view$gumbel, digits=2, scientific=FALSE)
                    ), style = ifelse(
                        is.na(data_view$gumbel),
                        "color: #999; text-align: right;", "text-align: right;"
                    ))
                ),
                tags$tr(
                    tags$th("Distribution lower bound", style="white-space: nowrap;"),
                    tags$td(ifelse(is.na(data_view$lowerbound),
                        "n/a",
                        format(round(data_view$lowerbound), scientific=FALSE, big.mark=" ")
                    ), style = ifelse(
                        is.na(data_view$lowerbound),
                        "color: #999; text-align: right;", "text-align: right;"
                    ))
                ),
                tags$tr(
                    tags$th("Average in the last bracket", style="white-space: nowrap;"),
                    tags$td(ifelse(is.na(data_view$last_bracketavg),
                        "n/a",
                        format(round(data_view$last_bracketavg), scientific=FALSE, big.mark=" ")
                    ), style = ifelse(
                        is.na(data_view$last_bracketavg),
                        "color: #999; text-align: right;", "text-align: right;"
                    ))
                ),
                tags$tr(
                    tags$th("Inverted Pareto coefficient at the last threshold", style="white-space: nowrap;"),
                    tags$td(ifelse(is.na(data_view$last_invpareto),
                        "n/a",
                        format(data_view$last_invpareto, digit=2, scientific=FALSE, big.mark=" ")
                    ), style = ifelse(
                        is.na(data_view$last_invpareto),
                        "color: #999; text-align: right;", "text-align: right;"
                    ))
                ),
                tags$tr(
                    tags$th("Asymptotic Pareto coefficient", style="white-space: nowrap;"),
                    tags$td(ifelse(is.null(data_view$binf) || is.na(data_view$binf),
                        "n/a",
                        format(data_view$binf, digit=2, scientific=FALSE, big.mark=" ")
                    ), style = ifelse(
                        is.null(data_view$binf) || is.na(data_view$binf),
                        "color: #999; text-align: right;", "text-align: right;"
                    ))
                )
            ),
            class = "table table-condensed table-striped"
        ),
        tags$h4("Tabulation"),
        renderTable(df,
            align = paste0("l", paste0(rep("r", ncol(df) - 1), collapse = "")),
            striped = TRUE,
            width = "100%",
            na = "n/a"
        )
    ))
})

observeEvent(input$clear, {
    reset_app()
})
