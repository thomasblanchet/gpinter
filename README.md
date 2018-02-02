# ```gpinter```: R package for generalized Pareto interpolation methods

For more details on generalized Pareto interpolation, please see [T.&nbsp;Blanchet, J.&nbsp;Fournier and T.&nbsp;Piketty, “Generalized Pareto curves: theory and applications”, 2017](http://wid.world/document/blanchet-t-fournier-j-piketty-t-generalized-pareto-curves-theory-applications-2017/).

## Installation

You can install this package using the following R code:
```{R}
install.packages("devtools")
devtools::install_github("thomasblanchet/gpinter")
```

## Documentation

For the explanations of how the package works, please refer to the [package's vignette](https://thomasblanchet.fr/documents/gpinter-vignette.pdf) and to the documentation of each individual function by typing `?function_name` in the R console.

## Application

If you do not want to use R, you can still run [the online web application built on the package](http://wid.world/gpinter/). Alternatively, you can run in locally on your computer using the R code:
```{R}
library(gpinter)
run_app()
```

