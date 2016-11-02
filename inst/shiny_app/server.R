library(shiny)
library(gpinter)
library(evaluate)
library(xlsx)

shinyServer(function(input, output, session) {

    # Import the input data
    input_data <- reactive({
        if (is.null(input$file_input)) {
            return(NULL)
        } else {
            nfiles <- length(input$file_input$name)
            input_list <- list()
            for (i in 1:nfiles) {
                input_list[[i]] <- list()
                filename <- input$file_input$name[i]
                filepath <- input$file_input$datapath[i]
                # Identify the type of file
                extension <- tail(strsplit(filename, ".", fixed=TRUE)[[1]], n=1)
                if (extension %in% c("csv", "tsv", "txt")) {
                    data <- tryCatch(suppressWarnings(read.csv(filepath,
                        header = TRUE,
                        stringsAsFactors = FALSE,
                        sep = isolate(input$sep_input),
                        quote = isolate(input$quote_input),
                        dec = isolate(input$decim_input),
                        colClasses = list(
                            "p" = "numeric",
                            "threshold" = "numeric",
                            "average" = "numeric",
                            "bracketshare" = "numeric",
                            "topshare" = "numeric",
                            "bracketavg" = "numeric",
                            "topavg" = "numeric",
                            "invpareto" = "numeric",
                            "label" = "character",
                            "samplesize" = "numeric"
                        )
                    )), error = function(e) {
                        return(simpleError(paste0("while reading ", filename, ": ", e, ".")))
                    })
                    if (is.error(data)) {
                        return(data)
                    }
                } else if (extension == "xlsx") {
                    data <- read.xlsx(filepath, header=TRUE, sheetIndex=1, stringAsFactors=FALSE)
                } else {
                    return(simpleError(paste0("unknown file extension: ", extension, ".")))
                }
                input_list[[i]]$filename <- filename
                # Look for the label
                if ("label" %in% colnames(data)) {
                    input_list[[i]]$label <- data$label[1]
                } else {
                    input_list[[i]]$label <- filename
                }
                # Look for the sample size
                if ("samplesize" %in% colnames(data)) {
                    input_list[[i]]$samplesize <- data$samplesize[1]
                } else {
                    input_list[[i]]$samplesize <- NA
                }
                # Look for the average
                if ("average" %in% colnames(data)) {
                    input_list[[i]]$average <- data$average[1]
                    if (is.na(data$average[1])) {
                        return(simpleError(paste0("average is missing in ", filename, ".")))
                    }
                } else {
                    return(simpleError(paste0("average is missing in ", filename, ".")))
                }
                # Look for percentiles
                if ("p" %in% colnames(data)) {
                    input_list[[i]]$p <- data$p
                    if (anyNA(input_list[[i]]$p)) {
                        return(simpleError(paste0("missing values in 'p' for ", filename, ".")))
                    }
                }
                # Look for the thresholds
                if ("threshold" %in% colnames(data)) {
                    input_list[[i]]$threshold <- data$threshold
                    if (anyNA(input_list[[i]]$threshold)) {
                        return(simpleError(paste0("missing values in 'threshold' for ", filename, ".")))
                    }
                } else {
                    return(simpleError(paste0("thresholds are missing in ", filename, ".")))
                }
                # Look for the averages/shares
                if ("bracketshare" %in% colnames(data)) {
                    input_list[[i]]$whichavgsh <- "bracketshare"
                    input_list[[i]]$bracketshare <- data$bracketshare
                    if (anyNA(input_list[[i]]$bracketshare)) {
                        return(simpleError(paste0("missing values in 'bracketshare' for ", filename, ".")))
                    }
                } else if ("topshare" %in% colnames(data)) {
                    input_list[[i]]$whichavgsh <- "topshare"
                    input_list[[i]]$topshare <- data$topshare
                    if (anyNA(input_list[[i]]$topshare)) {
                        return(simpleError(paste0("missing values in 'topshare' for ", filename, ".")))
                    }
                } else if ("bracketavg" %in% colnames(data)) {
                    input_list[[i]]$whichavgsh <- "bracketavg"
                    input_list[[i]]$bracketavg <- data$bracketavg
                    if (anyNA(input_list[[i]]$bracketavg)) {
                        return(simpleError(paste0("missing values in 'bracketavg' for ", filename, ".")))
                    }
                } else if ("topavg" %in% colnames(data)) {
                    input_list[[i]]$whichavgsh <- "topavg"
                    input_list[[i]]$topavg <- data$topavg
                    if (anyNA(input_list[[i]]$topavg)) {
                        return(simpleError(paste0("missing values in 'topavg' for ", filename, ".")))
                    }
                } else if ("invpareto" %in% colnames(data)) {
                    input_list[[i]]$whichavgsh <- "invpareto"
                    input_list[[i]]$invpareto <- data$invpareto
                    if (anyNA(input_list[[i]]$invpareto)) {
                        return(simpleError(paste0("missing values in 'invpareto' for ", filename, ".")))
                    }
                }
            }
            return(input_list)
        }
    })

    # Estimate the distribution of each input data
    model_results <- reactive({
        if (is.null(input_data()) | is.error(input_data())) {
            return(NULL)
        } else {
            return(withProgress(tryCatch(lapply(input_data(), function(data) {
                incProgress(amount=1, message=data$label)
                args <- list(
                    p = data$p,
                    threshold = data$threshold,
                    average = data$average,
                    citype = isolate(input$citype),
                    samplesize = data$samplesize
                )
                avgsh <- data$whichavgsh
                args[avgsh] <- data[avgsh]
                result <- do.call(tabulation_fit, args)
                result$label <- data$label
                return(result)
            }), error = function(e) return(simpleError(e))), min=0, max=length(input_data()) + 1, value=0))
        }
    })

    # Create the input data tabs
    output$input_tabs <- renderUI({
        if (is.null(input_data())) {
            return(tags$div(icon("info-circle"), HTML("&nbsp;"),
                "Your input data will appear here once you have imported them.",
                class="alert alert-info", role="alert"))
        } else if (is.error(input_data())) {
            # Hide instruction to show the error message
            updateCollapse(session, "collapsible-instructions", close="instructions")
            return(tags$div(icon("info-circle"), HTML("&nbsp;"),
                as.character(input_data()),
                class="alert alert-danger", role="alert"))
        } else {
            # Hide instruction when the user imports some data
            updateCollapse(session, "collapsible-instructions", close="instructions")
            # Create a tab for each inputfile
            input_tabs <- list()
            for (i in 1:length(input_data())) {
                input_tabs[[i]] <- tabPanel(input_data()[[i]]$label,
                    tags$h4("Summary"),
                    tags$table(
                        tags$tbody(
                            tags$tr(
                                tags$th("file name"),
                                tags$td(input_data()[[i]]$filename)
                            ),
                            tags$tr(
                                tags$th("average"),
                                tags$td(sprintf("%.2f", input_data()[[i]]$average))
                            ),
                            tags$tr(
                                tags$th("sample size"),
                                tags$td(ifelse(is.na(input_data()[[i]]$samplesize),
                                    "n/a", input_data()[[i]]$samplesize))
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

    output$results_tabs <- renderUI({
        if (is.null(model_results())) {
            return(tags$div(icon("info-circle"), HTML("&nbsp;"),
                "The results will appear here once you have imported your data.",
                class="alert alert-info", role="alert"))
        } else if (is.error(model_results())) {
            return(tags$div(icon("info-circle"), HTML("&nbsp;"),
                paste0("The program returned the following error: ", model_results()),
                class="alert alert-danger", role="alert"))
        } else {
            # Enable download button
            enable("dl_all_results")
            results_tabs <- lapply(seq_along(model_results()), function(i) {
                return(tabPanel(model_results()[[i]]$label,
                    tags$h3("G-percentiles", style="float: left;"),
                    tags$div(
                        downloadButton(paste0("dl_result_", i), "Download as CSV", class="btn-primary btn-sm"),
                        style = "float: right; margin-top: 15px;"
                    ),
                    tags$div(style="clear: both;"),
                    tableOutput(paste0("table_gperc_", i))
                ))
            })
            return(do.call(tabsetPanel, results_tabs))
        }
    })

    output$dl_all_results <- downloadHandler(
        filename = function() {
            return(paste0("gperc-all-", format.Date(Sys.time(), "%Y-%m-%d-%H-%M-%S"), ".zip"))
        },
        content = function(dest) {
            gperc <- c(
                seq(0, 0.99, 0.01), seq(0.991, 0.999, 0.001),
                seq(0.9991, 0.9999, 0.0001), seq(0.99991, 1, 0.00001)
            )
            # Create a file for each input
            tmp <- tempdir()
            indivfile <- sapply(model_results(), function(res) {
                threshold <- suppressWarnings(res$fitted_quantile(gperc))
                topshare <- suppressWarnings(res$fitted_top_share(gperc))
                invpareto <- suppressWarnings(res$fitted_invpareto(gperc))
                # Errors
                e_threshold <- suppressWarnings(res$ci_quantile(gperc))
                e_topshare <- suppressWarnings(res$ci_top_share(gperc))
                e_invpareto <- suppressWarnings(res$ci_invpareto(gperc))

                filename <- paste0(tmp, "/", res$label, ".csv")
                write.table(
                    data.frame(
                        gperc = gperc,
                        threshold = threshold,
                        threshold_lb = e_threshold$lower,
                        threshold_ub = e_threshold$upper,
                        topshare = topshare,
                        topshare_lb = e_topshare$lower,
                        topshare_ub = e_topshare$upper,
                        invpareto = invpareto,
                        invpareto_lb = e_invpareto$lower,
                        invpareto_ub = e_invpareto$upper
                    ),
                    file = filename,
                    na = "",
                    row.names = FALSE,
                    sep = isolate(input$sep_results_out),
                    dec = isolate(input$decim_results_out)
                )

                return(filename)
            })
            # Zip the files
            zip(dest, indivfile, flags="-r9Xj")
        }
    )

    # Individual "Download results" button
    observe({
        if (!is.null(model_results()) & !is.error(model_results())) {
            lapply(seq_along(model_results()), function(i) {
                output[[paste0("dl_result_", i)]] <- downloadHandler(
                    filename = function() {
                        return(paste0("gperc-[", model_results()[[i]]$label, "]-",
                            format.Date(Sys.time(), "%Y-%m-%d-%H-%M-%S"), ".csv"))
                    },
                    content = function(dest) {
                        gperc <- c(
                            seq(0, 0.99, 0.01), seq(0.991, 0.999, 0.001),
                            seq(0.9991, 0.9999, 0.0001), seq(0.99991, 1, 0.00001)
                        )
                        threshold <- suppressWarnings(model_results()[[i]]$fitted_quantile(gperc))
                        topshare <- suppressWarnings(model_results()[[i]]$fitted_top_share(gperc))
                        invpareto <- suppressWarnings(model_results()[[i]]$fitted_invpareto(gperc))
                        # Errors
                        e_threshold <- suppressWarnings(model_results()[[i]]$ci_quantile(gperc))
                        e_topshare <- suppressWarnings(model_results()[[i]]$ci_top_share(gperc))
                        e_invpareto <- suppressWarnings(model_results()[[i]]$ci_invpareto(gperc))

                        write.table(
                            data.frame(
                                gperc = gperc,
                                threshold = threshold,
                                threshold_lb = e_threshold$lower,
                                threshold_ub = e_threshold$upper,
                                topshare = topshare,
                                topshare_lb = e_topshare$lower,
                                topshare_ub = e_topshare$upper,
                                invpareto = invpareto,
                                invpareto_lb = e_invpareto$lower,
                                invpareto_ub = e_invpareto$upper
                            ),
                            file = dest,
                            na = "",
                            row.names = FALSE,
                            sep = isolate(input$sep_results_out),
                            dec = isolate(input$decim_results_out)
                        )
                    }
                )
            })
        }
    })

    # Result tabs content
    observe({
        if (!is.null(model_results()) & !is.error(model_results())) {
            gperc <- c(
                seq(0, 0.99, 0.01), seq(0.991, 0.999, 0.001),
                seq(0.9991, 0.9999, 0.0001), seq(0.99991, 1, 0.00001)
            )
            lapply(seq_along(model_results()), function(i) {
                threshold <- suppressWarnings(model_results()[[i]]$fitted_quantile(gperc))
                topshare <- suppressWarnings(model_results()[[i]]$fitted_top_share(gperc))
                invpareto <- suppressWarnings(model_results()[[i]]$fitted_invpareto(gperc))
                # Errors
                e_threshold <- suppressWarnings(model_results()[[i]]$ci_quantile(gperc))
                e_topshare <- suppressWarnings(model_results()[[i]]$ci_top_share(gperc))
                e_invpareto <- suppressWarnings(model_results()[[i]]$ci_invpareto(gperc))

                output[[paste0("table_gperc_", i)]] <- renderTable(data.frame(
                    p = sprintf("%1.5f", gperc),
                    threshold = ifelse(is.na(threshold), NA, sprintf("%.0f", threshold)),
                    ci = ifelse(is.na(e_threshold$lower), "", sprintf("[%.0f, %.0f]",
                        e_threshold$lower, e_threshold$upper)),
                    topshare = ifelse(is.na(topshare), NA, sprintf("%.2f%%", 100*topshare)),
                    ci = ifelse(is.na(e_topshare$lower), "", sprintf("[%.3f%%, %.3f%%]",
                        100*e_topshare$lower, 100*e_topshare$upper)),
                    invpareto = ifelse(is.na(invpareto), NA, sprintf("%.2f", invpareto)),
                    ci = ifelse(is.na(e_invpareto$lower), "", sprintf("[%.3f, %.3f]",
                        e_invpareto$lower, e_invpareto$upper)),
                    check.names = FALSE
                ), striped=TRUE, width="100%", na="n/a")
            })
        }
    })

    output$plot_tabs <- renderUI({
        if (is.null(model_results())) {
            return(tags$div(icon("info-circle"), HTML("&nbsp;"),
                "The results will appear here once you have imported your data.",
                class="alert alert-info", role="alert"))
        } else if (is.error(model_results())) {
            return(tags$div(icon("info-circle"), HTML("&nbsp;"),
                paste0("The program returned the following error: ", model_results()),
                class="alert alert-danger", role="alert"))
        } else {
            plots_tabs <- lapply(seq_along(model_results()), function(i) {
                support_lower <- model_results()[[i]]$fitted_quantile(0.01)
                support_upper <- model_results()[[i]]$fitted_quantile(0.99)
                return(tabPanel(model_results()[[i]]$label,
                    fixedRow(
                        column(6,
                            tags$div(
                                tags$div(
                                    plotOutput(paste0("lorenz_plot_", i)),
                                    class = "panel-body"
                                ),
                                tags$div(
                                    tags$h5("Plot range", style="text-align: center;"),
                                    sliderInput(paste0("lorenz_range_", i),
                                        min = 0,
                                        max = 1,
                                        value = c(0, 1),
                                        step = 0.001,
                                        label = NULL,
                                        width = "100%"
                                    ),
                                    class = "panel-footer"
                                ),
                                class = "panel panel-default",
                                style = "margin-top: 20px;"
                            )
                        ),
                        column(6,
                            tags$div(
                                tags$div(
                                    plotOutput(paste0("gpc_plot_", i)),
                                    class = "panel-body"
                                ),
                                tags$div(
                                    tags$h5("Plot range", style="text-align: center;"),
                                    sliderInput(paste0("gpc_range_", i),
                                        min = 0,
                                        max = 1,
                                        value = c(0.1, 1),
                                        step = 0.001,
                                        label = NULL,
                                        width = "100%"
                                    ),
                                    class = "panel-footer"
                                ),
                                class = "panel panel-default",
                                style = "margin-top: 20px;"
                            )
                        )
                    ),
                    fixedRow(
                        column(6,
                            tags$div(
                                tags$div(
                                    plotOutput(paste0("phi_plot_", i)),
                                    class = "panel-body"
                                ),
                                tags$div(
                                    tags$h5("Plot range", style="text-align: center;"),
                                    sliderInput(paste0("phi_range_", i),
                                        min = 0,
                                        max = 10,
                                        value = c(0, 7),
                                        step = 0.1,
                                        label = NULL,
                                        width = "100%"
                                    ),
                                    class = "panel-footer"
                                ),
                                class = "panel panel-default",
                                style = "margin-top: 20px;"
                            )
                        ),
                        column(6,
                            tags$div(
                                tags$div(
                                    plotOutput(paste0("phid_plot_", i)),
                                    class = "panel-body"
                                ),
                                tags$div(
                                    tags$h5("Plot range", style="text-align: center;"),
                                    sliderInput(paste0("phid_range_", i),
                                        min = 0,
                                        max = 10,
                                        value = c(0, 7),
                                        step = 0.1,
                                        label = NULL,
                                        width = "100%"
                                    ),
                                    class = "panel-footer"
                                ),
                                class = "panel panel-default",
                                style = "margin-top: 20px;"
                            )
                        )
                    ),
                    fixedRow(
                        column(6,
                            tags$div(
                                tags$div(
                                    plotOutput(paste0("density_plot_", i)),
                                    class = "panel-body"
                                ),
                                tags$div(
                                    tags$h5("Plot range", style="text-align: center;"),
                                    sliderInput(paste0("density_range_", i),
                                        min = ifelse(
                                            is.finite(model_results()[[i]]$fitted_quantile(0)),
                                            floor(model_results()[[i]]$fitted_quantile(0)),
                                            floor(model_results()[[i]]$fitted_quantile(0.00001))
                                        ),
                                        max = ceiling(model_results()[[i]]$fitted_quantile(0.995)),
                                        value = c(
                                            ifelse(
                                                is.finite(model_results()[[i]]$fitted_quantile(0)),
                                                floor(model_results()[[i]]$fitted_quantile(0)),
                                                floor(model_results()[[i]]$fitted_quantile(0.001))
                                            ),
                                            model_results()[[i]]$fitted_quantile(0.99)
                                        ),
                                        step = 0.1,
                                        label = NULL,
                                        width = "100%"
                                    ),
                                    class = "panel-footer"
                                ),
                                class = "panel panel-default",
                                style = "margin-top: 20px;"
                            )
                        )
                    )
                ))
            })
            return(do.call(tabsetPanel, plots_tabs))
        }
    })

    # Draw the plots
    # Interpolated function
    observe({
        if (!is.null(model_results()) & !is.error(model_results())) {
            lapply(seq_along(model_results()), function(i) {
                if (!is.null(input[[paste0("phi_range_", i)]])) {
                    xk <- model_results()[[i]]$xk
                    yk <- model_results()[[i]]$yk
                    x1 <- head(xk, n=1)
                    xn <- tail(xk, n=1)
                    xmin <- min(input[[paste0("phi_range_", i)]])
                    xmax <- max(input[[paste0("phi_range_", i)]])
                    yk <- yk[xk >= xmin & xk <= xmax]
                    xk <- xk[xk >= xmin & xk <= xmax]
                    x_range <- sort(c(xk, seq(xmin, xmax, length.out=200)))
                    y <- suppressWarnings(model_results()[[i]]$phi(x_range))
                    e <- suppressWarnings(model_results()[[i]]$ci_phi(x_range))
                    x_err <- x_range[!is.na(e$lower)]
                    e_lb <- na.omit(e$lower)
                    e_ub <- na.omit(e$upper)
                    output[[paste0("phi_plot_", i)]] <- renderPlot({
                        plot(x_range, y,
                            xlab = "x",
                            ylab = "ϕ(x)",
                            main = "Interpolated function",
                            type = 'n'
                        )
                        polygon(c(x_err, rev(x_err)), c(e_lb, rev(e_ub)),
                            col="light blue", border=NA
                        )
                        points(xk, yk, pch=20)
                        lines(x_range[x_range <= x1], y[x_range <= x1], type='l', lty=2)
                        lines(x_range[x_range <= xn & x_range >= x1], y[x_range <= xn & x_range >= x1], type='l')
                        lines(x_range[x_range >= xn], y[x_range >= xn], type='l', lty=2)
                    })
                }
            })
        }
    })

    # Derivative
    observe({
        if (!is.null(model_results()) & !is.error(model_results())) {
            lapply(seq_along(model_results()), function(i) {
                # Derivative
                if (!is.null(input[[paste0("phid_range_", i)]])) {
                    xk <- model_results()[[i]]$xk
                    sk <- model_results()[[i]]$sk
                    x1 <- head(xk, n=1)
                    xn <- tail(xk, n=1)
                    xmin <- min(input[[paste0("phid_range_", i)]])
                    xmax <- max(input[[paste0("phid_range_", i)]])
                    sk <- sk[xk >= xmin & xk <= xmax]
                    xk <- xk[xk >= xmin & xk <= xmax]
                    x_range <- sort(c(xk, seq(xmin, xmax, length.out=200)))
                    y <- suppressWarnings(model_results()[[i]]$deriv_phi(x_range))
                    e <- suppressWarnings(model_results()[[i]]$ci_deriv_phi(x_range))
                    x_err <- x_range[!is.na(e$lower)]
                    e_lb <- na.omit(e$lower)
                    e_ub <- na.omit(e$upper)
                    output[[paste0("phid_plot_", i)]] <- renderPlot({
                        plot(x_range, y,
                            xlab = "x",
                            ylab = "ϕ’(x)",
                            main = "Derivative",
                            type = 'n'
                        )
                        polygon(c(x_err, rev(x_err)), c(e_lb, rev(e_ub)),
                            col="light blue", border=NA
                        )
                        points(xk, sk, pch=20)
                        lines(x_range[x_range <= x1], y[x_range <= x1], type='l', lty=2)
                        lines(x_range[x_range <= xn & x_range >= x1], y[x_range <= xn & x_range >= x1], type='l')
                        lines(x_range[x_range >= xn], y[x_range >= xn], type='l', lty=2)
                    })
                }
            })
        }
    })

    # Lorenz curve
    observe({
        if (!is.null(model_results()) & !is.error(model_results())) {
            lapply(seq_along(model_results()), function(i) {
                # Derivative
                if (!is.null(input[[paste0("lorenz_range_", i)]])) {
                    pk <- model_results()[[i]]$pk
                    tsk <- 1 - model_results()[[i]]$mk/model_results()[[i]]$average
                    p1 <- head(pk, n=1)
                    pn <- tail(pk, n=1)
                    pmin <- min(input[[paste0("lorenz_range_", i)]])
                    pmax <- max(input[[paste0("lorenz_range_", i)]])
                    tsk <- tsk[pk >= pmin & pk <= pmax]
                    pk <- pk[pk >= pmin & pk <= pmax]
                    p_range <- sort(c(pk, seq(pmin, pmax, length.out=200)))
                    y <- 1 - suppressWarnings(model_results()[[i]]$fitted_top_share(p_range))
                    if (pmax == 1 & !any(p_range == 1)) {
                        p_range <- c(p_range, 1)
                        y <- c(y, 1)
                    }
                    e <- suppressWarnings(model_results()[[i]]$ci_top_share(p_range))
                    p_err <- p_range[!is.na(e$lower)]
                    e_lb <- 1 - na.omit(e$upper)
                    e_ub <- 1 - na.omit(e$lower)
                    output[[paste0("lorenz_plot_", i)]] <- renderPlot({
                        plot(p_range, y,
                            xlab = "p",
                            ylab = "L(p)",
                            main = "Lorenz curve",
                            type = 'n'
                        )
                        polygon(c(p_err, rev(p_err)), c(e_lb, rev(e_ub)),
                            col="light blue", border=NA
                        )
                        points(pk, tsk, pch=20)
                        points(c(0, 1), c(0, 1), pch=20)
                        lines(c(0, 1), c(0, 1), lty=3)
                        lines(p_range[p_range <= p1], y[p_range <= p1], type='l', lty=2)
                        lines(p_range[p_range <= pn & p_range >= p1], y[p_range <= pn & p_range >= p1], type='l')
                        lines(p_range[p_range >= pn], y[p_range >= pn], type='l', lty=2)
                    })
                }
            })
        }
    })

    # Generalized Pareto curve
    observe({
        if (!is.null(model_results()) & !is.error(model_results())) {
            lapply(seq_along(model_results()), function(i) {
                if (!is.null(input[[paste0("gpc_range_", i)]])) {
                    xi <- model_results()[[i]]$xi_top
                    pk <- model_results()[[i]]$pk
                    bk <- model_results()[[i]]$mk/((1 - pk)*model_results()[[i]]$qk)
                    p1 <- head(pk, n=1)
                    pn <- tail(pk, n=1)
                    pmin <- min(input[[paste0("gpc_range_", i)]])
                    pmax <- max(input[[paste0("gpc_range_", i)]])
                    bk <- bk[pk >= pmin & pk <= pmax]
                    pk <- pk[pk >= pmin & pk <= pmax]
                    p_range <- sort(c(pk, seq(pmin, pmax, length.out=200)))
                    y <- suppressWarnings(model_results()[[i]]$fitted_invpareto(p_range))
                    if (pmax == 1 & !any(p_range == 1)) {
                        p_range <- c(p_range, 1)
                        y <- c(y, 1/(1 - xi))
                    }
                    e <- suppressWarnings(model_results()[[i]]$ci_invpareto(p_range))
                    p_err <- p_range[!is.na(e$lower)]
                    e_lb <- na.omit(e$lower)
                    e_ub <- na.omit(e$upper)
                    output[[paste0("gpc_plot_", i)]] <- renderPlot({
                        plot(p_range, y,
                            xlab = "p",
                            ylab = "b(p)",
                            main = "Generalized Pareto curve",
                            type = 'n'
                        )
                        polygon(c(p_err, rev(p_err)), c(e_lb, rev(e_ub)),
                            col="light blue", border=NA
                        )
                        points(pk, bk, pch=20)
                        points(1, 1/(1 - xi), pch=20)
                        lines(p_range[p_range <= p1], y[p_range <= p1], type='l', lty=2)
                        lines(p_range[p_range <= pn & p_range >= p1], y[p_range <= pn & p_range >= p1], type='l')
                        lines(p_range[p_range >= pn], y[p_range >= pn], type='l', lty=2)
                    })
                }
            })
        }
    })

    # Density
    observe({
        if (!is.null(model_results()) & !is.error(model_results())) {
            lapply(seq_along(model_results()), function(i) {
                if (!is.null(input[[paste0("density_range_", i)]])) {
                    qmin <- min(input[[paste0("density_range_", i)]])
                    qmax <- max(input[[paste0("density_range_", i)]])
                    q_range <- seq(qmin, qmax, length.out=200)
                    y <- suppressWarnings(model_results()[[i]]$fitted_density(q_range))
                    output[[paste0("density_plot_", i)]] <- renderPlot({
                        plot(q_range, y,
                            xlab = "q",
                            ylab = "f(q)",
                            main = "Probability density function",
                            type = 'l'
                        )
                    })
                }
            })
        }
    })

    # Select file for simulated population
    output$synthpop_select_file <- renderUI({
        if (!is.null(model_results()) & !is.error(model_results())) {
            enable("synthpop_dl")
            files <- list()
            files[["All"]] <- 0
            for (i in seq_along(model_results())) {
                files[[as.character(model_results()[[i]]$label)]] <- i
            }
            return(selectInput("synthpop_file", "Choose file(s)", files, 0, width="100%"))
        } else {
            return(disabled(
                selectInput("synthpop_file", "Choose file(s)",
                    c("–"), "–", width="100%")
            ))
        }
    })

    output$synthpop_dl <- downloadHandler(
        filename = function() {
            return(paste0("simulpop-", format.Date(Sys.time(), "%Y-%m-%d-%H-%M-%S"), ".csv"))
        },
        content = function(dest) {
            i <- isolate(input$synthpop_file)
            if (i == 0) {
                data <- as.data.frame(sapply(model_results(), function(res) {
                    sample <- res$fitted_quantile(runif(isolate(input$synthpop_size)))
                }))
                colnames(data) <- lapply(model_results(), function(res) res$label)
                write.table(data,
                    file = dest,
                    na = "",
                    row.names = FALSE,
                    sep = isolate(input$synthpop_sep_out),
                    dec = isolate(input$synthpop_decim_out)
                )
            } else {
                res <- model_results()[[as.numeric(i)]]
                data <- as.data.frame(res$fitted_quantile(runif(isolate(input$synthpop_size))))
                colnames(data) <- res$label
                write.table(data,
                    file = dest,
                    na = "",
                    row.names = FALSE,
                    sep = isolate(input$synthpop_sep_out),
                    dec = isolate(input$synthpop_decim_out)
                )
            }
        }
    )
})
