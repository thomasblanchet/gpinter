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
                        tags$li(a("Share-based interpolation", href="#share-based")),
                        tags$li(a("Threshold-based interpolation", href="#threshold-based")),
                        tags$li(a("Adding up income or wealth components", href="#copula")),
                        tags$li(a("Source code", href="#source")),
                        tags$li(a("Contact us", href="#contact"))
                    ),
                    class = "nav-doc"
                )
            )
        )
    ),
    icon = icon("life-ring")
)
