library(shiny)
library(shinyBS)
library(shinyjs)

# Define UI for the application
shinyUI(tagList(useShinyjs(),
    navbarPage("Generalized Pareto interpolation",
    tabPanel("Input data",
        fixedPage(
            fixedRow(
                column(8,
                    bsCollapse(id="collapsible-instructions", bsCollapsePanel(
                        tags$span(icon("question-circle"), HTML("&nbsp;"), "Instructions"),
                        tags$p(
                            "This application provides an interface to the function",
                            tags$code("tabulation_fit"), "of the R package",
                            tags$code("gpinter"), ". It can estimate nonparametrically
                            the entire distribution of income or wealth from tabulated
                            data such as those provided by tax authorities."),
                        tags$p(
                            "This application takes an arbitrary number of Excel 2007
                            or CSV files as input. You can import those files using the",
                            tags$span("Browse...", style="border: 1px solid #ccc;
                            border-radius: 3px; padding: 1px 3px 1px 3px;"), "button
                            in the panel on the right. For the CSV files, you can
                            adjust the exact format with the", tags$span(icon("cog"), "CSV options",
                            style="font-weight: bold;"), "menu. For the Excel files, only the
                            first sheet is read."
                        ),
                        tags$p(
                            "The input files must take the form of tables whose first
                            row is the name of each column. The following three columns
                            are mandatory:",
                            tags$ul(
                                tags$li(tags$kbd("p"), ": a set of values greater
                                or equal to 0 and lower than 1,  associated to a
                                percentile of the distribution."),
                                tags$li(tags$kbd("threshold"), ": the quantile
                                associated to each value of", tags$kbd("p"), "."),
                                tags$li(tags$kbd("average"), "(first row only) :
                                the average over the entire distribution.")
                            ),
                            "At least one of the following columns are required:",
                            tags$ul(
                                tags$li(tags$kbd("bracketshare"), ": the income/wealth
                                share of the bracket."),
                                tags$li(tags$kbd("topshare"), ": the income/wealth
                                share of the bracket and the brackets above it."),
                                tags$li(tags$kbd("bracketavg"), ": the average
                                income/wealth within the bracket."),
                                tags$li(tags$kbd("topavg"), ": the average income/wealth
                                above the bracket."),
                                tags$li(tags$kbd("invpareto"), ": the inverted Pareto coefficient.")
                            ),
                            "The following columns are optional:",
                            tags$ul(
                                tags$li(tags$kbd("label"), "(first row only) : a name for the dataset. If not
                                specified, the file name is used."),
                                tags$li(tags$kbd("samplesize"), "(first row only) : the size of the sample the
                                tabulation is based on. Only used is full confidence intervals
                                are requested.")
                            )
                        ),
                        tags$p("An example of input data is provided below:"),
                        tags$table(
                            tags$thead(
                                tags$tr(
                                    tags$th("label"),
                                    tags$th("average"),
                                    tags$th("p"),
                                    tags$th("threshold"),
                                    tags$th("bracketshare")
                                )
                            ),
                            tags$tbody(
                                tags$tr(
                                    tags$td("US 2010"),
                                    tags$td("53587"),
                                    tags$td("0.900"),
                                    tags$td("96480"),
                                    tags$td("0.10537")
                                ),
                                tags$tr(
                                    tags$td(""),
                                    tags$td(""),
                                    tags$td("0.950"),
                                    tags$td("136910"),
                                    tags$td("0.14840")
                                ),
                                tags$tr(
                                    tags$td(""),
                                    tags$td(""),
                                    tags$td("0.990"),
                                    tags$td("351366"),
                                    tags$td("0.04007")
                                ),
                                tags$tr(
                                    tags$td(""),
                                    tags$td(""),
                                    tags$td("0.995"),
                                    tags$td("544503"),
                                    tags$td("0.15940")
                                )
                            ),
                            class = "table table-striped table-condensed"
                        ),
                        value = "instructions"
                    ), open="instructions"),
                    uiOutput("input_tabs")
                ),
                column(4,
                    tags$h4(icon("files-o"), HTML("&nbsp;"), "Import file(s)"),
                    tags$p("Supports CSV and Excel 2007 formats."),
                    fileInput("file_input",
                        width = "100%",
                        label = NULL,
                        multiple = TRUE,
                        accept = c(
                            'text/csv',
                            'text/comma-separated-values',
                            'text/tab-separated-values',
                            'text/plain',
                            'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet',
                            '.txt',
                            '.csv',
                            '.tsv',
                            '.xlsx'
                        )
                    ),
                    tags$hr(),
                    tags$h4(icon("cog"), HTML("&nbsp;"), "CSV import options"),
                    radioButtons('sep_input', 'Separator', c(
                        "Comma" = ',',
                        "Semicolon" = ';',
                        "Tab" = '\t'),
                        ';'
                    ),
                    radioButtons('quote_input', 'Quote', c(
                        "None" = '',
                        "Double Quote" = '"',
                        "Single Quote"="'"),
                        '"'
                    ),
                    radioButtons('decim_input', 'Decimal separator', c(
                        "Point" = '.',
                        "Comma" = ','),
                        ','
                    ),
                    tags$hr(),
                    tags$h4(icon("arrows-h"), HTML("&nbsp;"), "Confidence intervals"),
                    radioButtons('citype', NULL, c(
                        "Interpolation error only" = 1,
                        "Interpolation and sampling error (slow)" = 2),
                        1
                    )
                )
            )
        ),
        icon=icon("arrow-circle-down")
    ),
    tabPanel("Results",
        fixedPage(
            fixedRow(
                column(9,
                    uiOutput("results_tabs")
                ),
                column(3,
                    downloadButton("dl_all_results",
                        label = "Download all as CSV",
                        class = "btn-primary btn-block disabled"
                    ),
                    hr(),
                    tags$h4(icon("cog"), HTML("&nbsp;"), "CSV export options"),
                    radioButtons('sep_results_out', 'Separator', c(
                        "Comma" = ',',
                        "Semicolon" = ';',
                        "Tab" = '\t'),
                        ';'
                    ),
                    radioButtons('decim_results_out', 'Decimal separator', c(
                        "Point" = '.',
                        "Comma" = ','),
                        ','
                    )
                )
            )
        ),
        icon=icon("table")
    ),
    tabPanel("Plots",
        fixedPage(uiOutput("plot_tabs")),
        icon=icon("area-chart")
    ),
    tabPanel("Simulate population",
        fixedPage(
            fixedRow(
                column(9,
                    uiOutput("synthpop_select_file"),
                    numericInput("synthpop_size", "Choose sample size",
                        value = 1e6,
                        min = 1,
                        width = "100%"
                    ),
                    hr(),
                    fixedRow(
                        column(12,
                            downloadButton("synthpop_dl", "Download as CSV",
                                class = "btn-block btn-primary disabled"
                            )
                        )
                    )
                ),
                column(3,
                    tags$h4(icon("cog"), HTML("&nbsp;"), "CSV export options"),
                    radioButtons('synthpop_sep_out', 'Separator', c(
                        "Comma" = ',',
                        "Semicolon" = ';',
                        "Tab" = '\t'),
                        ';'
                    ),
                    radioButtons('synthpop_decim_out', 'Decimal separator', c(
                        "Point" = '.',
                        "Comma" = ','),
                        ','
                    )
                )
            )
        ),
        icon=icon("users")
    ),
    inverse = TRUE,
    position = "static-top"
)))
