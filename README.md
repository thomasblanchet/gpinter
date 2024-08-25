> [!IMPORTANT]
> 
> Thank you for your interest in this project. However, please be aware that this repository is **no longer maintained**.
> 
> - No further updates or bug fixes will be made.
> - Issues and pull requests will not be responded to.
>   
> For any critical needs, please consider forking the repository and making your own updates.
>
> Additionally, this package **should be considered softly deprecated**. It will remain available in the foreseeable future, and its use in existing codebases remains acceptable. However, **I do not recommend its usage in a new project**, for reasons that go beyond the lack of maintenance.
>
> Experience has shown that the approach the package implements **causes more problems than it solves**, especially when used by people who do not understand the underlying methodology and treat it as a black box. Older approaches, such as the **mean-split histogram** (see e.g., [Cowell, 2012, *Measuring Inequality*, Appendix A7, p. 172](http://econdse.org/wp-content/uploads/2012/02/Cowell-measuring-inequality.pdf)), provide nearly identical results at a fraction of the cost and complexity (and to the extent that the results diverge, it almost certainly indicates a problem with the raw data that should rather be addressed directly.)
>
> I also wish to warn people against using this methodology as a starting point or as a benchmark for developing new interpolation strategies. **I will not reply to any request for advice or feedback on the topic.** Additional work on distribution interpolation is **not needed**. The distribution interpolation problem should have been considered solved by the mean-split histogram method. All the [more](https://onlinelibrary.wiley.com/doi/10.1111/roiw.12510) [recent](https://arxiv.org/pdf/2204.05480) attempts to improve upon it are a solution in search of a problem, and they are a waste of everyone’s time.

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
