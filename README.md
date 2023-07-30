# checkPlotR

This repo is for developing the R package checkPlotR, Authors Michael Roswell,
Michael Li, and Jonathan Dushoff.

The package produces slugPlots (and other rangePlots), which show the behavior
of point and interval estimators on the scale of the statistic, for easy
visualization of statistical coverage, bias, and other weirdness, and also
checkPlots, which show the behavior of the p-values that underlie the
construction of frequentist CI.

To use these tools, one must first simulate data and implement a p-value
estimate.

<!-- badges: start -->
  [![R-CMD-check](https://github.com/dushoff/checkPlots/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/dushoff/checkPlots/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->
