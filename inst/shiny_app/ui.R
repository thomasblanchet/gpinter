library(shiny)
library(shinyBS)
library(shinyjs)

# Define UI for the application
shinyUI(tagList(useShinyjs(), navbarPage("Generalized Pareto interpolation",
    tabPanel("Help",
        fixedPage(
            fixedRow(
                column(8,
                    withMathJax(includeHTML("help.html"))
                ),
                column(4,
                    tags$div(
                        tags$ul(
                            tags$li(a("Introduction", href="#")),
                            tags$li(a("Functionalities", href="#functionalities")),
                            tags$li(a("Usage", href="#usage"), tags$ul(
                                tags$li(a("Input data format", href="#input-format"))
                            ))
                        ),
                        class = "nav-doc"
                    )
                )
            )
        ),
        icon = icon("question-circle")
    ),
    tabPanel("Input data",
        fixedPage(
            fixedRow(
                column(7,
                    tags$h4(icon("files-o"), HTML("&nbsp;"), "Import file(s)"),
                    tags$p("Supports CSV and Excel formats."),
                    fileInput("file_input",
                        width = "100%",
                        label = NULL,
                        multiple = TRUE,
                        accept = c(
                            'text/csv',
                            'text/comma-separated-values',
                            'text/tab-separated-values',
                            'text/plain',
                            'application/vnd.ms-excel',
                            'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet',
                            '.txt',
                            '.csv',
                            '.tsv',
                            '.xls',
                            '.xlsx'
                        )
                    ),
                    disabled(actionButton('run', "Run",
                        icon = icon("play"),
                        width = "100%",
                        class = "btn-success",
                        style = "margin-bottom: 20px;"
                    )),
                    uiOutput("input_tabs")
                ),
                column(5,
                    tags$div(
                        tags$div(
                            tags$h3(icon("sliders"), HTML("&nbsp;"), "Interpolation settings", class="panel-title"),
                            class = "panel-heading"
                        ),
                        tags$div(
                            disabled(
                                tags$h4(icon("user"), HTML("&nbsp;"), "Individualisation", tags$span("In progress", class="label label-primary")),
                                tags$p("The program can individualize the distribution of income or wealth
                                    under the assumption of equal sharing among spouses. If you select this
                                    option, you must specify the share of singles in your input files. See help
                                    for details.",
                                    style = "font-size: small; color: #666;"
                                ),
                                checkboxInput('indiv', "Individualize the distribution", value=FALSE)
                            ),
                            hr(),
                            disabled(
                                tags$h4(icon("plus-square"), HTML("&nbsp;"), "Add up distributions", tags$span("In progress", class="label label-primary")),
                                tags$p("The program can add up two component income or wealth (typically, labor
                                    and capital income). The distributions of the two components to be added
                                    must share the same", tags$code("addupid"), "in the input files. See help for details.",
                                    style = "font-size: small; color: #666;"
                            ),
                                checkboxInput('addup', "Add up distributions", value=FALSE),
                                withMathJax(tags$p("The dependency between is assumed to be characterized by a Gumbel copula
                                    with a user-specified parameter \\(\\theta\\). The higher the parameter, the higher the
                                    dependency, with \\(\\theta = 1\\) meaning independence.",
                                    style = "font-size: small; color: #666;"
                                )),
                                numericInput('gumbelparam', "Gumbel copula parameter \\(\\theta\\)", value=3, min=1, width="100%")
                            ),
                            hr(),
                            disabled(
                                tags$h4(icon("compress"), HTML("&nbsp;"), "Merge distributions", tags$span("In progress", class="label label-primary")),
                                tags$p("The program can merge several distributions (typically from several
                                    countries). The distributions to be merged must share the same", tags$code("mergeid"),
                                    "in the input files. See help for details.",
                                    style = "font-size: small; color: #666;"
                                ),
                                checkboxInput('merge', "Merge distributions", value=FALSE)
                            ),
                            class = "panel-body"
                        ),
                        class = "panel panel-default"
                    )
                )
            )
        ),
        icon=icon("arrow-down")
    ),
    tabPanel("Tables",
        fixedPage(
            fixedRow(
                column(9,
                    uiOutput("results_tabs")
                ),
                column(3,
                    disabled(downloadButton("dl_tables_csv",
                        label = "Download as CSV",
                        class = "btn-primary btn-block"
                    )),
                    disabled(downloadButton("dl_tables_excel",
                        label = "Download as Excel",
                        class = "btn-primary btn-block"
                    )),
                    tags$div(
                        tags$div(
                            tags$h3(icon("columns"), HTML("&nbsp;"), "Customize columns", class="panel-title"),
                            class = "panel-heading"
                        ),
                        tags$div(
                            tags$p("Select the columns you want to include in the tables.",
                                style = "font-size: small; color: #666;"
                            ),
                            checkboxGroupInput('results_display', NULL,
                                choices = list(
                                    "Threshold" = "thres",
                                    "Top share" = "topshare",
                                    "Bottom share" = "bottomshare",
                                    "Bracket share" = "bracketshare",
                                    "Top average" = "topavg",
                                    "Bracket average" = "bracketavg",
                                    "Inverted Pareto coefficient" = "invpareto"
                                ),
                                selected = c("perc", "thres", "topshare", "invpareto"),
                                width = "100%"
                            ),
                            class = "panel-body"
                        ),
                        class = "panel panel-default",
                        style = "margin-top: 20px;"
                    )
                )
            )
        ),
        icon=icon("table")
    ),
    tabPanel("Plots",
        fixedPage(
            navlistPanel(
                tabPanel("Lorenz curve",
                    uiOutput("plots_tabs_lorenz")
                ),
                tabPanel("Generalized Pareto curve",
                    uiOutput("plots_tabs_gpc")
                ),
                tabPanel("Probability density function",
                    uiOutput("plots_tabs_pdf")
                ),
                tabPanel("Cumulative density function",
                    uiOutput("plots_tabs_cdf")
                ),
                tabPanel("Quantile function",
                    uiOutput("plots_tabs_quantile")
                ),
                tabPanel("Tail function",
                    uiOutput("plots_tabs_tail")
                ),
                tabPanel("Interpolation function",
                    uiOutput("plots_tabs_phi")
                ),
                tabPanel("Derivative of interpolation function",
                    uiOutput("plots_tabs_deriv_phi")
                ),
                well = FALSE
            )
        ),
        icon=icon("area-chart")
    ),
    tabPanel("Simulate",
        fixedPage(
            uiOutput("synthpop_select_file"),
            numericInput("synthpop_size", "Choose sample size",
                value = 1e6,
                min = 1,
                width = "100%"
            ),
            hr(),
            fixedRow(
                column(6,
                    downloadButton("synthpop_dl_csv", "Download as CSV",
                        class = "btn-block btn-primary disabled"
                    )
                ),
                column(6,
                    downloadButton("synthpop_dl_excel", "Download as Excel",
                        class = "btn-block btn-primary disabled"
                    )
                )
            )
        ),
        icon = icon("random")
    ),
    tabPanel("Diagnostic",
        icon = icon("stethoscope")
    ),
    tabPanel("Options",
        fixedPage(
            fixedRow(
                column(6, tags$div(
                    tags$div(
                        tags$h3("CSV import options", class="panel-title"),
                        class = "panel-heading"
                    ),
                    tags$div(
                        tags$h4("Field separator"),
                        radioButtons('csv_input_field_separator', NULL, list(
                            "Comma" = ",",
                            "Tabs" = "\t",
                            "Semicolon" = ";"
                        ), selected = ";"),
                        tags$h4("Decimal separator"),
                        radioButtons('csv_input_dec_separator', NULL, list(
                            "Point" = ".",
                            "Comma" = ","
                        ), selected = ","),
                        tags$h4("Quotes"),
                        radioButtons('csv_input_quote', NULL, list(
                            "None" = "",
                            "Single quotes" = "'",
                            "Double quotes" = "\""
                        ), selected = "\""),
                        class = "panel-body"
                    ),
                    class = "panel panel-default"
                )),
                column(6, tags$div(
                    tags$div(
                        tags$h3("CSV export options", class="panel-title"),
                        class = "panel-heading"
                    ),
                    tags$div(
                        tags$h4("Field separator"),
                        radioButtons('csv_output_field_separator', NULL, list(
                            "Comma" = ",",
                            "Tabs" = "\t",
                            "Semicolon" = ";"
                        ), selected = ";"),
                        tags$h4("Decimal separator"),
                        radioButtons('csv_output_dec_separator', NULL, list(
                            "Point" = ".",
                            "Comma" = ","
                        ), selected = ","),
                        class = "panel-body"
                    ),
                    class = "panel panel-default"
                ))
            )
        ),
        icon = icon("cogs")
    ),
    selected = "Input data",
    inverse = TRUE,
    position = "fixed-top",
    theme = "style.css"
)))
