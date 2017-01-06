tabPanel("Input data",
    fixedPage(
        fixedRow(
            column(5,
                tags$div(
                    tags$div(
                        tags$h3(icon("files-o"), HTML("&nbsp;"), "Import file(s)", class="panel-title"),
                        class = "panel-heading"
                    ),
                    tags$div(
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
                        class = "panel-body",
                        id = "panel_file_input"
                    ),
                    class = "panel panel-default"
                ),
                tags$div(
                    tags$div(
                        tags$h3(icon("cogs"), HTML("&nbsp;"), "Interpolation options", class="panel-title"),
                        class = "panel-heading"
                    ),
                    tags$div(
                        withMathJax(radioButtons_withHTML("interpolation_options", NULL, width="100%", choices=c(
                            "<h4 style='margin-top: 0;'>Interpolate only</h4>
                            <p style='font-size: small; color: #666;'>Interpolate the distribution
                            of your data directly, without any transformation.</p>" = "basic",
                            "<h4 style='margin-top: 0;'>Interpolate and individualize</h4>
                            <p style='font-size: small; color: #666;'>Individualize the distribution of income or wealth
                            under the assumption of equal sharing among spouses. If you select this
                            option, you must specify the share of singles in your input files.</p>" = "individualize",
                            "<h4 style='margin-top: 0;'>Interpolate and merge countries</h4>
                            <p style='font-size: small; color: #666;'>Merge the distribution of several countries
                            into a single one. If you select this option, you must specify the population size of each country.</p>" = "merge",
                            "<h4 style='margin-top: 0;'>Interpolate and add up components</h4>
                            <p style='font-size: small; color: #666;'>Add up two components of income or wealth (for example,
                            labor and capital income), assuming that the dependence between both components
                            is characterized by a Gumbel copula.</p>
                            <p style='font-size: small; color: #666; margin-bottom: 0;'>The dependence between the two components is assumed to be characterized
                            by a Gumbel copula with parameter \\(\\theta\\). The higher
                            \\(\\theta\\), the stronger the dependence, with \\(\\theta = 1\\) meaning
                            full independence. You may specify a value for \\(\\theta\\) in each files,
                            or set a global value below.</p>" = "addup"
                        ))),
                        tags$div(
                            numericInput("gumbel_param", "Gumbel copula parameter \\(\\theta\\)", value=3, min=1, width="100%"),
                            style = "margin-left: 20px;"
                        ),
                        class = "panel-body"
                    ),
                    class = "panel panel-default"
                )
            ),
            column(7,
                hidden(tags$div(
                    tags$h4(tags$i(class = "fa fa-cog"), HTML(" &nbsp; "), tags$span("", id = "import_progress_message"),
                        style = "text-align: center; margin-top: 10px; margin-bottom: 20px;"
                    ),
                    tags$div(
                        tags$div(
                            id = "import_progress",
                            role = "progressbar",
                            `aria-valuenow` = "0",
                            `aria-valuemin` = "0",
                            `aria-valuemax` = "100",
                            style = "width: 0%;",
                            class = "progress-bar progress-bar-striped"
                        ),
                        class = "progress"
                    ),
                    id = "panel_import_progress"
                )),
                tags$div(
                    tags$div(
                        tags$p("This interface lets you reconstruct the full distribution of income or
                            wealth based on tabulated data files such as those provided by tax autorities."),
                        tags$p("To import the tabulation files, use the “Browse” button
                            on the left and choose or more file from your computer. You must have one CSV file or
                            Excel sheet per tabulation. Each must take the form of a table with the following format:"),
                        tabsetPanel(
                            tabPanel("File #1",
                                tags$table(
                                    tags$tr(
                                        tags$th("year"), tags$th("country"), tags$th("component"),
                                        tags$th("average"), tags$th("p"), tags$th("thr"), tags$th("bracketavg")
                                    ),
                                    tags$tr(
                                        tags$td("2010"), tags$td("US"), tags$td("labor"),
                                        tags$td("37 208"), tags$td("0.1"), tags$td("4 130"), tags$td("12 643")
                                    ),
                                    tags$tr(
                                        tags$td(""), tags$td(""), tags$td(""), tags$td(""),
                                        tags$td("0.5"), tags$td("23 686"), tags$td("43 908")
                                    ),
                                    tags$tr(
                                        tags$td(""), tags$td(""), tags$td(""), tags$td(""),
                                        tags$td("0.9"), tags$td("76 252"), tags$td("108 329")
                                    ),
                                    tags$tr(
                                        tags$td(""), tags$td(""), tags$td(""), tags$td(""),
                                        tags$td("0.99"), tags$td("211 861"), tags$td("471 463")
                                    ),
                                    class = "table table-bordered table-condensed",
                                    style = "margin-bottom: 2px;"
                                )
                            ),
                            tabPanel("File #2",
                                tags$table(
                                    tags$tr(
                                        tags$th("year"), tags$th("country"), tags$th("component"),
                                        tags$th("average"), tags$th("p"), tags$th("thr"), tags$th("bracketavg")
                                    ),
                                    tags$tr(
                                        tags$td("2010"), tags$td("US"), tags$td("capital"),
                                        tags$td("16 370"), tags$td("0.1"), tags$td("-1 176"), tags$td("328")
                                    ),
                                    tags$tr(
                                        tags$td(""), tags$td(""), tags$td(""), tags$td(""),
                                        tags$td("0.5"), tags$td("2 780"), tags$td("10 657")
                                    ),
                                    tags$tr(
                                        tags$td(""), tags$td(""), tags$td(""), tags$td(""),
                                        tags$td("0.9"), tags$td("28 939"), tags$td("59 412")
                                    ),
                                    tags$tr(
                                        tags$td(""), tags$td(""), tags$td(""), tags$td(""),
                                        tags$td("0.99"), tags$td("173 917"), tags$td("688 689")
                                    ),
                                    class = "table table-bordered table-condensed",
                                    style = "margin-bottom: 2px;"
                                )
                            ),
                            id = "example_tabs",
                            type = "pills"
                        ),
                        tags$p("Download this sample file as", tags$a(icon("download"), "CSV", href="sample.csv"),
                            "/", tags$a(icon("download"), "Excel", href="sample.xlsx"), "or",
                            actionLink("import_example", "import it directly to the interface.", icon("arrow-down")),
                            style="text-align: right; font-size: small;"),
                        tags$p("Each column of the table correspond to a variable. You need to at least specify:",
                            tags$ul(
                                tags$li(tags$code("p"), "for fractiles"),
                                tags$li(tags$code("thr"), "for matching quantiles"),
                                tags$li(tags$code("average"), "for the overall average")
                            )
                        ),
                        tags$p("You must also specify one of the following:",
                            tags$ul(
                                tags$li(tags$code("bracketsh"), "for the share of the bracket"),
                                tags$li(tags$code("topsh"), "for the top share"),
                                tags$li(tags$code("bracketavg"), "for the average in the bracket"),
                                tags$li(tags$code("topavg"), "for the top average"),
                                tags$li(tags$code("b"), "for the inverted Pareto coefficient")
                            )
                        ),
                        tags$p("Finally, if you have several tabulations, you will need to identify them
                            using at least one of the following fields:",
                            tags$ul(
                                tags$li(tags$code("year"), "for the period covered by the tabulation"),
                                tags$li(tags$code("country"), "for the country or region"),
                                tags$li(tags$code("component"), "for the component (for example labor or capital income)")
                            )
                        ),
                        class = "panel-body"
                        ),
                        class = "panel panel-default",
                        style = "box-shadow: none; border-style: dashed; color: #666;",
                        id = "help_intro"
                    ),
                    uiOutput("input_data_view_header"),
                    uiOutput("input_data_view")
                )
            )
        ),
    icon = icon("arrow-down")
)
