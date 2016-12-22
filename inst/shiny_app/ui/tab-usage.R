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
)
