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
)
