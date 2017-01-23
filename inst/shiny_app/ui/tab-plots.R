tabPanel("Plots",
    fixedPage(
        uiOutput("message_plots"),
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
                        )),
                        tags$h4(icon("info-circle"), HTML(" &nbsp; "), "About this plot"),
                        tags$p(
                            "The Lorenz curve shows on the y-axis the fraction of income or wealth owned by ",
                            "the bottom x% of the population. The dashed line correspond to perfect equality. ",
                            "The area between the curve and the dashed line corresponds to the Gini coefficient ",
                            "and is a measure of inequality.",
                            style = "color: #666;"
                        )
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
                        )),
                        tags$h4(icon("info-circle"), HTML(" &nbsp; "), "About this plot"),
                        tags$p(
                            "The generalized Pareto curve associate a fractile \\(p\\) to its ",
                            "inverted Pareto coefficient \\(b(p)=\\frac{\\mathbb{E}[X|X > Q(p)]}{Q(p)}\\). ",
                            "For a Pareto distribution with coefficient \\(\\alpha\\), the inverted Pareto ",
                            "is constant and equal to \\(\\frac{\\alpha}{\\alpha - 1}\\).",
                            style = "color: #666;"
                        )
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
                        )),
                        tags$h4(icon("info-circle"), HTML(" &nbsp; "), "About this plot"),
                        tags$p(
                            "The histogram approximates the probability density function of the distribution. ",
                            "The range starts either from the lower bound of the distribution ",
                            "(if it is finite) or the 1st percentile, and ends at the 99th percentile. ",
                            "It is divided into 100 bins of equal size.",
                            style = "color: #666;"
                        )
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
                        )),
                        tags$h4(icon("info-circle"), HTML(" &nbsp; "), "About this plot"),
                        tags$p(
                            "The cumulative distribution function represents the share of people below ",
                            "a certain income or wealth level.",
                            style = "color: #666;"
                        )
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
                        )),
                        tags$h4(icon("info-circle"), HTML(" &nbsp; "), "About this plot"),
                        tags$p(
                            "The quantile function gives the income or wealth level associated to ",
                            "a fractile.",
                            style = "color: #666;"
                        )
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
                        )),
                        tags$h4(icon("info-circle"), HTML(" &nbsp; "), "About this plot"),
                        tags$p(
                            "This plot show the top of the quantile function with a logarithmic ",
                            "scale on both axes, so that we get a linear curve in the case of the ",
                            "Pareto distribution: \\(x \\mapsto \\log(Q(1 - \\text{e}^{-x}))\\).",
                            style = "color: #666;"
                        )
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
                        )),
                        tags$h4(icon("info-circle"), HTML(" &nbsp; "), "About this plot"),
                        tags$p(
                            "This plot shows the curve that the generalized Pareto interpolation ",
                            "method interpolates. It correspond to a logarithmic tranform of the ",
                            "Lorenz curve: \\(x \\mapsto  -\\log\\left(\\int_{1-\\text{e}^{-x}}^\\infty Q(u) \\, \\text{d}u\\right)\\).",
                            style = "color: #666;"
                        )
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
                        )),
                        tags$h4(icon("info-circle"), HTML(" &nbsp; "), "About this plot"),
                        tags$p(
                            "This plot shows the derivative of the interpolation function, which ",
                            "corresponds to the inverse of the generalized Pareto curve (with ",
                            "with a different scale on the x-axis).",
                            style = "color: #666;"
                        )
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
)
