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
                        tags$li(a("How to use the interface", href="#usage")),
                        tags$li(a("Contact us", href="#contact"))
                    ),
                    class = "nav-doc"
                )
            )
        )
    ),
    icon = icon("life-ring")
)
