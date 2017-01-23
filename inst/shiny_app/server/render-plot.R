output$message_plots <- renderUI({
    if (is.null(data$output_dist)) {
        return(
            return(tags$div(icon("info-circle"), HTML("&nbsp;"),
                "Plots of output results will appear here once the programs have been successfully executed.",
                class="alert alert-info", role="alert"))
        )
    } else {
        return(NULL)
    }
})

# Generate the distribution plots
result_plot <- reactive({
    if (is.null(data$output_dist)) {
        return(NULL)
    }

    year <- input$output_dist_plot_year
    country <- input$output_dist_plot_country
    component <- input$output_dist_plot_component

    result <- data$output_dist[[year]][[country]][[component]]

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
        enable("slider_hist")
        enable("slider_cdf")
        enable("slider_quantile")
        enable("slider_tail")
        enable("slider_phi")
        enable("slider_deriv_phi")

        updateSliderInput(session, "slider_hist",
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
        return(plot_text("Please select a valid range"))
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
        return(plot_text("Please select a valid range"))
    }

    pmin <- min(input$slider_gpc)
    pmax <- max(input$slider_gpc)

    return(plot_gpc(result, xlim=c(pmin, pmax)))
})

output$plot_hist <- renderPlot({
    result <- result_plot()

    if (is.null(result) || is.null(input$slider_hist)) {
        return(plot_text("No data"))
    }

    if (min(input$slider_hist) == max(input$slider_hist)) {
        return(plot_text("Please select a valid range"))
    }

    qmin <- min(input$slider_hist)
    qmax <- max(input$slider_hist)

    return(plot_hist(result, xlim=c(qmin, qmax)))
})

output$plot_cdf <- renderPlot({
    result <- result_plot()

    if (is.null(result) || is.null(input$slider_cdf)) {
        return(plot_text("No data"))
    }

    if (min(input$slider_cdf) == max(input$slider_cdf)) {
        return(plot_text("Please select a valid range"))
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
        return(plot_text("Please select a valid range"))
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
        return(plot_text("Please select a valid range"))
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
        return(plot_text("Please select a valid range"))
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
        return(plot_text("Please select a valid range"))
    }

    xmin <- min(input$slider_deriv_phi)
    xmax <- max(input$slider_deriv_phi)

    return(plot_deriv_phi(result, xlim=c(xmin, xmax)))
})

# Generate the time plots
tables_plot_allyears <- reactive({
    if (is.null(data$output_dist)) {
        return(NULL)
    }

    country <- input$output_time_plot_country
    component <- input$output_time_plot_component

    return(lapply(data$output_years, function(year) {
        return(data$output_tables[[year]][[country]][[component]])
    }))
})

observe({
    tables <- tables_plot_allyears()

    if (is.null(tables)) {
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

        minyear <- min(na.omit(as.numeric(data$output_years)))
        maxyear <- max(na.omit(as.numeric(data$output_years)))

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
    tables <- tables_plot_allyears()

    if (is.null(tables) || is.null(input$slider_top_1)) {
        return(plot_text("No data"))
    }

    if (min(input$slider_top_1) == max(input$slider_top_1)) {
        return(plot_text("Need more than one year"))
    }

    ymin <- min(input$slider_top_1)
    ymax <- max(input$slider_top_1)

    df <- data.frame(
        year = as.numeric(data$output_years),
        top1 = sapply(tables, function(tab) {
            if (!is.null(tab)) {
                return(tab$top1)
            } else {
                return(NA)
            }
        })
    )
    # Check if there is at least a nonmissing value corresponding to a
    # nonmissing year
    if (length(na.omit(df$top1[!is.na(df$year)])) == 0) {
        return(plot_text("No data"))
    }
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
    tables <- tables_plot_allyears()

    if (is.null(tables) || is.null(input$slider_top_10)) {
        return(plot_text("No data"))
    }

    if (min(input$slider_top_1) == max(input$slider_top_10)) {
        return(plot_text("Need more than one year"))
    }

    ymin <- min(input$slider_top_10)
    ymax <- max(input$slider_top_10)

    df <- data.frame(
        year = as.numeric(data$output_years),
        top10 = sapply(tables, function(tab) {
            if (!is.null(tab)) {
                return(tab$top10)
            } else {
                return(NA)
            }
        })
    )
    # Check if there is at least a nonmissing value corresponding to a
    # nonmissing year
    if (length(na.omit(df$top10[!is.na(df$year)])) == 0) {
        return(plot_text("No data"))
    }
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
    tables <- tables_plot_allyears()

    if (is.null(tables) || is.null(input$slider_middle_40)) {
        return(plot_text("No data"))
    }

    if (min(input$slider_middle_40) == max(input$slider_middle_40)) {
        return(plot_text("Need more than one year"))
    }

    ymin <- min(input$slider_middle_40)
    ymax <- max(input$slider_middle_40)

    df <- data.frame(
        year = as.numeric(data$output_years),
        middle40 = sapply(tables, function(tab) {
            if (!is.null(tab)) {
                return(tab$middle40)
            } else {
                return(NA)
            }
        })
    )
    # Check if there is at least a nonmissing value corresponding to a
    # nonmissing year
    if (length(na.omit(df$middle40[!is.na(df$year)])) == 0) {
        return(plot_text("No data"))
    }
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
    tables <- tables_plot_allyears()

    if (is.null(tables) || is.null(input$slider_bottom_50)) {
        return(plot_text("No data"))
    }

    if (min(input$slider_bottom_50) == max(input$slider_bottom_50)) {
        return(plot_text("Need more than one year"))
    }

    ymin <- min(input$slider_bottom_50)
    ymax <- max(input$slider_bottom_50)

    df <- data.frame(
        year = as.numeric(data$output_years),
        bottom50 = sapply(tables, function(tab) {
            if (!is.null(tab)) {
                return(tab$bottom50)
            } else {
                return(NA)
            }
        })
    )
    # Check if there is at least a nonmissing value corresponding to a
    # nonmissing year
    if (length(na.omit(df$bottom50[!is.na(df$year)])) == 0) {
        return(plot_text("No data"))
    }
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
    tables <- tables_plot_allyears()

    if (is.null(tables) || is.null(input$slider_gini)) {
        return(plot_text("No data"))
    }

    if (min(input$slider_gini) == max(input$slider_gini)) {
        return(plot_text("Need more than one year"))
    }

    ymin <- min(input$slider_gini)
    ymax <- max(input$slider_gini)

    df <- data.frame(
        year = as.numeric(data$output_years),
        gini = sapply(tables, function(tab) {
            if (!is.null(tab)) {
                return(tab$gini)
            } else {
                return(NA)
            }
        })
    )
    # Check if there is at least a nonmissing value corresponding to a
    # nonmissing year
    if (length(na.omit(df$gini[!is.na(df$year)])) == 0) {
        return(plot_text("No data"))
    }
    df$year <- as.numeric(df$year)
    df <- df[!is.na(df$year), ]
    df <- df[df$year >= ymin & df$year <= ymax, ]

    plot <- ggplot2::ggplot(data=df, ggplot2::aes(x=year, y=gini)) +
        ggplot2::geom_line() + ggplot2::geom_point() +
        ggplot2::xlab("Year") + ggplot2::ylab("Gini index")

    return(plot)
})
