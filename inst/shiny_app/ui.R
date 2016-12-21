library(shiny)
library(shinyBS)
library(shinyjs)

# Modified radioButtons to include HTML
radioButtons_withHTML <- function (inputId, label, choices, selected=NULL, inline=FALSE, width=NULL) {
    choices <- shiny:::choicesWithNames(choices)
    selected <- if (is.null(selected))
        choices[[1]]
    else {
        shiny:::validateSelected(selected, choices, inputId)
    }
    if (length(selected) > 1)
        stop("The 'selected' argument must be of length 1")
    options <- generateOptions_withHTML(inputId, choices, selected, inline,
        type = "radio")
    divClass <- "form-group shiny-input-radiogroup shiny-input-container"
    if (inline)
        divClass <- paste(divClass, "shiny-input-container-inline")
    tags$div(id = inputId, style = if (!is.null(width))
        paste0("width: ", validateCssUnit(width), ";"), class = divClass,
        shiny:::controlLabel(inputId, label), options)
}

generateOptions_withHTML <- function (inputId, choices, selected, inline, type="checkbox") {
    options <- mapply(choices, names(choices), FUN = function(value,
        name) {
        inputTag <- tags$input(type = type, name = inputId, value = value)
        if (value %in% selected)
            inputTag$attribs$checked <- "checked"
        if (inline) {
            tags$label(class = paste0(type, "-inline"), inputTag,
                tags$span(HTML(name)))
        }
        else {
            tags$div(class = type, tags$label(inputTag, tags$span(HTML(name))))
        }
    }, SIMPLIFY = FALSE, USE.NAMES = FALSE)
    div(class = "shiny-options-group", options)
}

# Define UI for the application
shinyUI(tagList(useShinyjs(), navbarPage(actionLink("main_logo", tagList(
        tags$p("WID.WORLD"),
        tags$p("generalized Pareto interpolation")
    )), tabPanel("Input data",
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
                                id = "import_progress_container",
                                class = "progress"
                            ),
                            class = "panel-body"
                        ),
                        class = "panel panel-default"
                    ),
                    tags$div(
                        tags$div(
                            tags$h3(icon("cogs"), HTML("&nbsp;"), "Interpolation options", class="panel-title"),
                            class = "panel-heading"
                        ),
                        tags$div(
                            radioButtons_withHTML("interpolation_options", NULL, width="100%", choices=c(
                                "<h4 style='margin-top: 0;'>Basic interpolation</h4>
                                <p style='font-size: small; color: #666;'>Interpolate the distribution
                                of your data directly, without any transformation.</p>" = "basic",
                                "<h4 style='margin-top: 0;'>Individualize</h4>
                                <p style='font-size: small; color: #666;'>Individualize the distribution of income or wealth
                                under the assumption of equal sharing among spouses. If you select this
                                option, you must specify the share of singles in your input files.</p>" = "individualize",
                                "<h4 style='margin-top: 0;'>Merge countries</h4>
                                <p style='font-size: small; color: #666;'>Merge the distribution of several countries
                                into a single one. If you select this option, you must specify the population size of each country.</p>" = "merge",
                                "<h4 style='margin-top: 0;'>Add up components</h4>
                                <p style='font-size: small; color: #666;'>Add up two components of income or wealth (for example,
                                labor and capital income), assuming that the dependence between both components
                                is characterized by a Gumbel copula.</p>" = "addup"
                            )),
                            class = "panel-body"
                        ),
                        class = "panel panel-default"
                    )
                ),
                column(7,
                    tags$div(
                        tags$div(
                            tags$p("This interface lets you reconstruct the full distribution of income or
                                wealth based on tabulated data files such as those provided by tax autorities."),
                            tags$p("To import the tabulation files, use the “Browse” button
                                on the left and choose or more file from your computer. You must have one CSV file or
                                Excel sheet per tabulation. Each must take the form of a table with the following format:"),
                            tags$table(
                                tags$tr(
                                    tags$th("year"), tags$th("country"),
                                    tags$th("average"), tags$th("p"), tags$th("thr"), tags$th("bracketsh")
                                ),
                                tags$tr(
                                    tags$td("2010"), tags$td("US"), tags$td("53 587"), tags$td("0.1"),
                                    tags$td("5 665"), tags$td("0.13459")
                                ),
                                tags$tr(
                                    tags$td(""), tags$td(""), tags$td(""), tags$td("0.5"),
                                    tags$td("31 829"), tags$td("0.41007")
                                ),
                                tags$tr(
                                    tags$td(""), tags$td(""), tags$td(""), tags$td("0.9"),
                                    tags$td("96 480"), tags$td("0.10537")
                                ),
                                tags$tr(
                                    tags$td(""), tags$td(""), tags$td(""), tags$td("0.95"),
                                    tags$td("136 910"), tags$td("0.14840")
                                ),
                                tags$tr(
                                    tags$td(""), tags$td(""), tags$td(""), tags$td("0.99"),
                                    tags$td("351 366"), tags$td("0.19946")
                                ),
                                class = "table table-bordered table-condensed",
                                style = "margin-bottom: 2px;"
                            ),
                            tags$p("Download this sample file as", tags$a(icon("download"), "CSV", href="sample.csv"),
                                "/", tags$a(icon("download"), "Excel", href="sample.xlsx"), "or",
                                actionLink("import_example", "import it directly to the interface.", icon("arrow-down")),
                                style="text-align: center; font-size: small;"),
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
        icon=icon("arrow-down")
    ),
    tabPanel("Output tables",
        fixedPage(
            fixedRow(
                column(9,
                    fixedRow(
                        column(4, disabled(selectInput("output_table_year", "Year", choices=NULL, width="100%"))),
                        column(4, disabled(selectInput("output_table_country", "Country", choices=NULL, width="100%"))),
                        column(4, disabled(selectInput("output_table_component", "Component", choices=NULL, width="100%")))
                    ),
                    uiOutput("output_table")
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
                                selected = c("perc", "thres", "topshare", "topavg", "invpareto"),
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
            tabsetPanel(
                tabPanel("Distribution",
                    fixedRow(
                        column(4, disabled(selectInput("output_dist_plot_year", "Year", choices=NULL, width="100%"))),
                        column(4, disabled(selectInput("output_dist_plot_country", "Country", choices=NULL, width="100%"))),
                        column(4, disabled(selectInput("output_dist_plot_component", "Component", choices=NULL, width="100%"))),
                        style = "margin-top: 10px;"
                    ),
                    hr(style="margin-top: 0;"),
                    navlistPanel(
                        tabPanel("Lorenz curve",
                            plotOutput("plot_lorenz"),
                            disabled(sliderInput("slider_lorenz",
                                min = 0,
                                max = 1,
                                value = c(0, 1),
                                step = 0.01,
                                label = NULL,
                                width = "100%"
                            ))
                        ),
                        tabPanel("Generalized Pareto curve",
                            plotOutput("plot_gpc"),
                            disabled(sliderInput("slider_gpc",
                                min = 0,
                                max = 1,
                                value = c(0.1, 1),
                                step = 0.01,
                                label = NULL,
                                width = "100%"
                            ))
                        ),
                        tabPanel("Histogram",
                            plotOutput("plot_hist"),
                            disabled(sliderInput("slider_hist",
                                min = 0,
                                max = 1e5,
                                value = c(0, 1e5),
                                step = 1,
                                label = NULL,
                                width = "100%"
                            ))
                        ),
                        tabPanel("Cumulative distribution function",
                            plotOutput("plot_cdf"),
                            disabled(sliderInput("slider_cdf",
                                min = 0,
                                max = 1e5,
                                value = c(0, 1e5),
                                step = 1,
                                label = NULL,
                                width = "100%"
                            ))
                        ),
                        tabPanel("Quantile function",
                            plotOutput("plot_quantile"),
                            disabled(sliderInput("slider_quantile",
                                min = 0,
                                max = 1,
                                value = c(0.05, 0.95),
                                step = 0.01,
                                label = NULL,
                                width = "100%"
                            ))
                        ),
                        tabPanel("Top tail",
                            plotOutput("plot_tail"),
                            disabled(sliderInput("slider_tail",
                                min = 0,
                                max = 10,
                                value = c(1, 7),
                                step = 0.1,
                                label = NULL,
                                width = "100%"
                            ))
                        ),
                        tabPanel("Interpolation function",
                            plotOutput("plot_phi"),
                            disabled(sliderInput("slider_phi",
                                min = 0,
                                max = 10,
                                value = c(0, 7),
                                step = 0.1,
                                label = NULL,
                                width = "100%"
                            ))
                        ),
                        tabPanel("Derivative of interpolation function",
                            plotOutput("plot_deriv_phi"),
                            disabled(sliderInput("slider_deriv_phi",
                                min = 0,
                                max = 10,
                                value = c(0, 7),
                                step = 0.1,
                                label = NULL,
                                width = "100%"
                            ))
                        ),
                        well = FALSE
                    )
                ),
                tabPanel("Time series",
                    fixedRow(
                        column(6, disabled(selectInput("output_time_plot_country", "Country", choices=NULL, width="100%"))),
                        column(6, disabled(selectInput("output_time_plot_component", "Component", choices=NULL, width="100%"))),
                        style = "margin-top: 10px;"
                    ),
                    hr(style="margin-top: 0;"),
                    navlistPanel(
                        tabPanel("Top 1%",
                            plotOutput("plot_top_1"),
                            disabled(sliderInput("slider_top_1",
                                min = 1900,
                                max = 2015,
                                value = c(1900, 2015),
                                step = 1,
                                label = NULL,
                                width = "100%",
                                sep = ""
                            ))
                        ),
                        tabPanel("Top 10%",
                            plotOutput("plot_top_10"),
                            disabled(sliderInput("slider_top_10",
                                min = 1900,
                                max = 2015,
                                value = c(1900, 2015),
                                step = 1,
                                label = NULL,
                                width = "100%",
                                sep = ""
                            ))
                        ),
                        tabPanel("Middle 40%",
                            plotOutput("plot_middle_40"),
                            disabled(sliderInput("slider_middle_40",
                                min = 1900,
                                max = 2015,
                                value = c(1900, 2015),
                                step = 1,
                                label = NULL,
                                width = "100%",
                                sep = ""
                            ))
                        ),
                        tabPanel("Bottom 50%",
                            plotOutput("plot_bottom_50"),
                            disabled(sliderInput("slider_bottom_50",
                                min = 1900,
                                max = 2015,
                                value = c(1900, 2015),
                                step = 1,
                                label = NULL,
                                width = "100%",
                                sep = ""
                            ))
                        ),
                        tabPanel("Gini index",
                            plotOutput("plot_gini"),
                            disabled(sliderInput("slider_gini",
                                min = 1900,
                                max = 2015,
                                value = c(1900, 2015),
                                step = 1,
                                label = NULL,
                                width = "100%",
                                sep = ""
                            ))
                        ),
                        well = FALSE
                    )
                ),
                id = "plot_tabs"
            )
        ),
        icon=icon("area-chart")
    ),
    tabPanel("Sample",
        fixedPage(
            fixedRow(
                column(4,
                    tags$form(
                        disabled(selectInput("synthpop_year", "Year", choices=NULL, width="100%")),
                        disabled(checkboxInput("synthpop_year_all", "Use all years", value=TRUE, width="100%")),
                        class = "select-synthpop"
                    )
                ),
                column(4,
                    tags$form(
                        disabled(selectInput("synthpop_country", "Country", choices=NULL, width="100%")),
                        disabled(checkboxInput("synthpop_country_all", "Use all countries", value=TRUE, width="100%")),
                        class = "select-synthpop"
                    )
                ),
                column(4,
                    tags$form(
                        disabled(selectInput("synthpop_component", "Component", choices=NULL, width="100%")),
                        disabled(checkboxInput("synthpop_component_all", "Use all components", value=TRUE, width="100%")),
                        class = "select-synthpop"
                    )
                )
            ),
            numericInput("synthpop_size", "Sample size",
                value = 1e3,
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
        fixedPage(
            tags$h1(tags$span("In progress", class="label label-primary"), style="text-align: center;")
        ),
        icon = icon("stethoscope")
    ),
    tabPanel("Settings",
        fixedPage(
            fixedRow(
                column(6,
                    tags$div(
                        tags$div(
                            tags$h3("Variable names", class="panel-title"),
                            class = "panel-heading"
                        ),
                        tags$div(
                            tags$form(
                                tags$div(
                                    textInput("var_year", "Year", "year", width="100%"),
                                    textInput("var_country", "Country", "country", width="100%"),
                                    textInput("var_component", "Component", "component", width="100%"),
                                    textInput("var_p", "Fractiles", "p", width="100%"),
                                    textInput("var_q", "Thresholds", "thr", width="100%"),
                                    textInput("var_b", "Inverted Pareto coefficient", "b", width="100%"),
                                    textInput("var_bracketshare", "Bracket share", "bracketsh", width="100%"),
                                    textInput("var_topshare", "Top share", "topsh", width="100%"),
                                    textInput("var_bracketavg", "Bracket average", "bracketavg", width="100%"),
                                    textInput("var_topavg", "Top average", "topavg", width="100%"),
                                    textInput("var_singleshare", "Overall share of singles", "singleshare", width="100%"),
                                    textInput("var_coupleshare", "Overall share of couples", "coupleshare", width="100%"),
                                    textInput("var_singlebracket", "Share of singles inside bracket", "s", width="100%"),
                                    textInput("var_singletop", "Share of singles in bracket and above", "topsingle", width="100%"),
                                    textInput("var_couplebracket", "Share of couples inside bracket", "bracketcouple", width="100%"),
                                    textInput("var_coupletop", "Share of couples in bracket and above", "topcouple", width="100%"),
                                    textInput("var_average", "Average", "average", width="100%"),
                                    textInput("var_popsize", "Population size", "popsize", width="100%"),
                                    textInput("var_gumbel", "Gumbel copula parameter", "gumbel", width="100%"),
                                    class = "form-group"
                                )
                            ),
                            class = "panel-body"
                        ),
                        class = "panel panel-default"
                    )
                ),
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
                        class = "panel-body"
                    ),
                    class = "panel panel-default"
                ),
                    tags$div(
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
        icon = icon("sliders")
    ),
    navbarMenu("Help",
        tabPanel("Usage",
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
            icon = icon("wrench")
        ),
        tabPanel("About generalized Pareto interpolation",
            fixedPage(
                fixedRow(
                    p("")
                )
            ),
            icon = icon("info-circle")
        ),
        icon = icon("question-circle")
    ),
    id = "main_navbar",
    selected = "Input data",
    position = "fixed-top",
    inverse = TRUE,
    theme = "style.css",
    windowTitle = "Generalized Pareto interpolation - WID.WORLD"
)))
