% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mapower_se.R
\name{mapower_se}
\alias{mapower_se}
\title{Calculate statistical power using effect size and standard error data}
\usage{
mapower_se(dat, size = "medium", observed_es, name)
}
\arguments{
\item{dat}{A dataset that contains one column with observed effect sizes or outcomes
labelled "yi" and a column with standard error data, labelled "sei".}

\item{size}{The effect size range. The default is a medium range (0.1 to 1, in increments of 0.1).
In other words, if you do not use this argument, the medium range will be used.
Other options include a "small" range (0.05 to 0.5 in increments of 0.05) and a "large"
range (0.25 to 2.5 in increments of 0.25).}

\item{observed_es}{The observed summary effect size estimate for the meta-analysis, which is one output
for the set of possible "true" effect sizes. See 'Details'}

\item{name}{A label with a name for the meta-analysis, which is required for using the "firepower" function.
See 'Examples'}
}
\value{
This function returns the following:
      \item{dat}{A dataset with results from power analyses for a range of effect sizes, including the
      specified observed effect size, in a column labelled "es_observed". The additional added columns include
      results for power analysis assuming a range of true effect sizes, with the default beginning at 0.1 ("power_es01"),
      then 0.2 ("power_es02"), then continuing in increments of 0.1 up to 1 ("power_es1"). A smaller
      or larger range of effect sizes can be specified (see the 'size' argument for details)}
}
\description{
Calculate statistical power using effect size and standard error data that
is typically reported in meta-analyses. Power is calculated assuming
the summary effect size in a meta-analysis is true effect size and assuming a 
range of effect sizes. The script to calculate power
was adapted from the "viz_sunset" function of the metaviz package.
}
\details{
\strong{The true effect size}

For the purposes of power analysis a "true" effect size needs to be specified. However,
the true effect size is unknown when testing hypotheses and reported summary
effect sizes are typically inflated. Here, the observed summary effect size estimate 
reported in the meta-analysis results is used as one possible true effect size, however, 
other effect sizes can be specified here instead. Additionally, statistical power for a range 
of true effect sizes are returned. The default is a "medium" range from 0.1 to 1, 
in increments of 0.1. However, a "small" and "large" range is also available 
(see the "size" argument below for details).
}
\examples{
### An analysis using the default effect size range

power_ooi <- mapower_se(
             dat = dat_ooi,
             observed_es = 0.178,
             name = "ooi et al 2017")
power_ooi

### An analysis using the large effect size range

power_ooi_l <- mapower_se(
               dat = dat_ooi,
               size = "large",
               observed_es = 0.178,
               name = "ooi et al 2017")
power_ooi_l

### An analysis using the small effect size range

power_ooi_s <- mapower_se(
               dat = dat_ooi,
               size = "small",
               observed_es = 0.178,
               name = "ooi et al 2017")
power_ooi_s

}
