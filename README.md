# ```gpinter```: R package for generalized Pareto interpolation methods

Generalized Pareto Interpolation is a method for reconstructing complete distributions based on tabulations that only contain information on a few thresholds and bracket shares. It is usually applied in the study of income and wealth, but it can also be used with other types of data.

For more detail on generalized Pareto interpolation, please see [T.&nbsp;Blanchet, J.&nbsp;Fournier and T.&nbsp;Piketty, “Generalized Pareto curves: theory and applications”, 2017](http://wid.world/document/blanchet-t-fournier-j-piketty-t-generalized-pareto-curves-theory-applications-2017/).

## Installation

You can install this package using the following R code:
```{r}
install.packages("devtools")
devtools::install_github("thomasblanchet/gpinter")
```

## Documentation

For the explanations of how the package works, please see the [package's vignette](https://thomasblanchet.fr/wp-content/uploads/2020/04/gpinter-vignette.pdf) and the documentation of each individual function by typing `?function_name` in the R console.

## Note for Stata users

Although there is no native Stata command equivalent to `gpinter`, you can apply generalized Pareto interpolation from within Stata by calling R code with the user-written `rsource` command. [This example .do file](inst/stata/gpinter-stata-example.do) demonstrates how to make this work.

## Online Application

If you do not want to use R, you can still run [the online web application built on the package](http://wid.world/gpinter/). Alternatively, you can run in locally on your computer using the R code:
```{r}
library(gpinter)
run_app()
```
To run that code, you will need additional R packages not installed by default with `gpinter`. You can install them all with the following command:
```{r}
devtools::install_github("thomasblanchet/gpinter",
    dependencies = c("Depends", "Imports", "LinkingTo", "Suggests"))
```
