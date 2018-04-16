// The R package 'gpinter' implements generalized Pareto interpolation. There
// is no native Stata command to perform the same task, but if you are a Stata
// user, you can integrate gpinter in your workflow quite easily by calling
// R from your Stata programs.
//
// This example file shows you how to do this. You do not need a thorough
// knowledge of R to run the interpolation and import the results into Stata.
//
// The method requires the following:
//     - A working version of Stata.
//     - A working version of R installed on the computer. If it is not already
//       the case, you should download and install R from the web address
//       <https://cran.r-project.org/mirrors.html>. Make sure to choose
//       a mirror near to your location and to pick the right operating system.
//     - The packages 'haven' and 'gpinter' installed in R. To do so, launch R
//       and type in the commands:
//        > install.packages(c("haven", "devtools"))
//        > devtools::install_github("thomasblanchet/gpinter")
//     - The user-written Stata command 'rsource'. You can install it by typing:
//        > ssc install rsource
//
// If you need more help with this file, please contact
// <thomas.blanchet@wid.world>.

// -------------------------------------------------------------------------- //
// Preliminaries
// -------------------------------------------------------------------------- //

clear

// Set your working directory here
cd "~/GitHub/gpinter/inst/stata"

// If necessary, specify where R is installed on your computer. You only need
// to do it if the program cannot find R by default. On Linux and macOS, you
// should find it at the addresses "/usr/bin/r" or "/usr/local/bin/r". On
// Windows, you should locate the file "Rterm.exe".
// Type "help rsource" for more details.

*global Rterm_path `"/usr/local/bin/r"'

// -------------------------------------------------------------------------- //
// Import tabulation example (US labor income, 2010)
// -------------------------------------------------------------------------- //

input average     p  threshold  bracketavg
        37208  0.10       4130       12643
            .  0.50      23686       43908
            .  0.90      76252      108329
            .  0.99     211861      471463
end

// -------------------------------------------------------------------------- //
// Save the tabulation as a Stata file
// -------------------------------------------------------------------------- //

// Using "saveold" is a useful precaution to make sure R will be able to read
// the file even if have a very recent version of Stata
saveold "tabulation-input.dta", version(11) replace

// -------------------------------------------------------------------------- //
// Call R from Stata and run the interpolation in it
// -------------------------------------------------------------------------- //

rsource, terminator(END_OF_R) roptions(--vanilla)

	// 'haven' is a R package for importing Stata '.dta' file
	library(haven)

	// 'gpinter' is the R package to perform generalized Pareto interpoaltion
	library(gpinter)

	// Import the Stata data into R
	data <- read_dta("tabulation-input.dta")

	// Perform interpolation
	distribution <- tabulation_fit(
		p = data$p,
		thr = data$threshold,
		bracketavg = data$bracketavg,
		average = data$average[1]
	)

	// Percentiles to include in the output
	percentiles_output <- c(
		seq(0, 0.99, 0.01), // Every percentile
		seq(0.991, 0.999, 0.001), // Every 1/10 of a percentile in top 1%
		seq(0.9991, 0.9999, 0.0001), // Every 1/100 of a percentile in top 0.1%
		seq(0.99991, 0.99999, 0.00001) // Every 1/1000 of a percentile in top 0.01%
	)

	// Create a tabulation for these detailed percentiles
	tabulation <- generate_tabulation(distribution, percentiles_output)

	// You may only keep the columns you are interested in by removing one of
	// these rows. You can also rename the columns by changing the names on the
	// left of the equal sign.
	tabulation <- data.frame(
		p               = tabulation$fractile,
		threshold       = tabulation$threshold,
		top_share       = tabulation$top_share,
		bottom_share    = tabulation$bottom_share,
		bracket_share   = tabulation$bracket_share,
		top_average     = tabulation$top_average,
		bottom_average  = tabulation$bottom_average,
		bracket_average = tabulation$bracket_average,
		invpareto       = tabulation$invpareto
	)

	// Export the detailed tabulation
	write_dta(tabulation, "tabulation-output.dta")

END_OF_R

// -------------------------------------------------------------------------- //
// Import the results of the R program in Stata
// -------------------------------------------------------------------------- //

use "tabulation-output.dta", clear

// You can now use the interpolation results in Stata...

