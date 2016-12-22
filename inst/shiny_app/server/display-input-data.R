output$input_data_view_header <- renderUI({
    if (is.null(data$data) & is.null(data$errors)) {
        shinyjs::show("help_intro")
        return(NULL)
    } else if (length(data$data) == 0 & length(data$errors) > 0) {
        shinyjs::hide("help_intro")
        return(tagList(tags$div(
            tags$p("There is nothing to display because all of your files
                generated an error during the importation. Please check
                the format of your input data and try again."), tags$p(
                    tags$ul(lapply(data$errors, function(e) {
                        tags$li(tags$i(class="fa fa-li fa-times-circle"), e$message)
                    }), class="fa-ul")
            ),
            class = "alert alert-danger",
            role = "alert",
            style = "max-height: 500px; overflow: scroll;"
        ), actionButton("clear", "Clear data", icon=icon("eraser"), class="btn-block btn-danger")))
} else {
    shinyjs::hide("help_intro")
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
            column(4, selectInput("input_view_year", "Year", choices=data$years, width="100%"))
        )
    } else {
        select_input_menu <- tagList(select_input_menu,
            disabled(column(4, selectInput("input_view_year", "Year", choices=data$years, width="100%")))
        )
    }

    if (length(data$countries) > 1) {
        select_input_menu <- tagList(select_input_menu,
            column(4, selectInput("input_view_country", "Country", choices=data$countries, width="100%"))
        )
    } else {
        select_input_menu <- tagList(select_input_menu,
            disabled(column(4, selectInput("input_view_country", "Country", choices=data$countries, width="100%")))
        )
    }

    if (length(data$component) > 1) {
        select_input_menu <- tagList(select_input_menu,
            column(4, selectInput("input_view_component", "Component", choices=data$components, width="100%"))
        )
    } else {
        select_input_menu <- tagList(select_input_menu,
            disabled(column(4, selectInput("input_view_component", "Component", choices=data$components, width="100%")))
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
        "Thresholds" = format(round(data_view$threshold), big.mark=" ", scientific=FALSE)
    )
    df[avgsh_clean] <- sprintf("%.3f", data_view[[avgsh]])
    df[is.na(data_view[[avgsh]]), avgsh_clean] <- NA

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
                ),
                tags$tr(
                    tags$th("Gumbel parameter", style="white-space: nowrap;"),
                    tags$td(ifelse(is.na(data_view$gumbel),
                        "n/a",
                        data_view$gumbel
                    ), style = ifelse(
                        is.na(data_view$gumbel),
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
