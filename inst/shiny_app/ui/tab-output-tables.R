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
)
