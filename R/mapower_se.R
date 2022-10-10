#' Calculate statistical power using effect size and standard error data
#'
#' Calculate statistical power using effect size and standard error data that
#' is typically reported in meta-analyses. Power is calculated assuming
#' the true effect size and a range of effect sizes. The script to calculate power
#' was adapted from the "viz_sunset" function of the metaviz package.
#'
#' \strong{The true effect size}
#'
#' For the purposes of power analysis a "true" effect size needs to be specified. This
#' is typically difficult to establish in practice, as reported effect sizes are typically
#' inflated. Here, the observed summary effect size estimate reported in the
#' meta-analysis results is used as one possible true effect size, however, other effect sizes can
#' be specified here instead. Additionally, statistical power for a range of true effect sizes
#' are returned. The default is a range from 0.1 to 1, in increments of 0.1. However, a "small" 
#' range and a "large" range is also available.
#'
#' @param dat A dataset that contains one column with observed effect sizes or outcomes
#'     labelled "yi" and a column with standard error data, labelled "sei".
#' @param size The effect size range. The default is a medium range (0.1 to 1, in increments of 0.1).
#'     Other options include a "small" range (0.05 to 0.5 in increments of 0.05) and a "large"
#'     range (0.25 to 2.5 in increments of 0.25).
#' @param observed_es The observed summary effect size estimate for the meta-analysis, which is one output
#'     for the set of possible "true" effect sizes. See 'Details'
#' @param name A label with a name for the meta-analysis, which is required for using the "firepower" function.
#'     See 'Examples'
#' @return This function returns the following:
#'       \item{dat}{A dataset with results from power analyses for a range of effect sizes, including the
#'       specified observed effect size, in a column labelled "es_observed". The additional added columns include
#'       results for power analysis assuming a range of true effect sizes, with the default beginning at 0.1 ("power_es01"),
#'       then 0.2 ("power_es02"), then continuing in increments of 0.1 up to 1 ("power_es1").}
#' @examples
#' power_ooi <- mapower_se(
#'         dat = dat_ooi,
#'         observed_es = 0.178,
#'         name = "ooi et al 2017")
#' power_ooi
#'

mapower_se <-
  function(dat,
           size = "medium",
           observed_es,
           name)
#'@import dplyr
  {
    if(size == "small"){
      dat[["power_es_observed"]] <-
        (1 - stats::pnorm(stats::qnorm(1 - 0.05 / 2) * dat[["sei"]],
                          abs(observed_es), dat[["sei"]])) +
        stats::pnorm(stats::qnorm(0.05 / 2) *
                       dat[["sei"]], abs(observed_es),
                     dat[["sei"]]) # Calculate power for each study for observed effect
      dat[["power_es005"]] <-
        (1 - stats::pnorm(stats::qnorm(1 - 0.05 / 2) * dat[["sei"]],
                          abs(0.05), dat[["sei"]])) +
        stats::pnorm(stats::qnorm(0.05 / 2) *
                       dat[["sei"]], abs(0.05),
                     dat[["sei"]]) # Calculate power for each study for an effect of 0.05
      dat[["power_es01"]] <-
        (1 - stats::pnorm(stats::qnorm(1 - 0.05 / 2) * dat[["sei"]],
                          abs(0.1), dat[["sei"]])) +
        stats::pnorm(stats::qnorm(0.05 / 2) *
                       dat[["sei"]], abs(0.1),
                     dat[["sei"]]) # Calculate power for each study for an effect of 0.1
      dat[["power_es015"]] <-
        (1 - stats::pnorm(stats::qnorm(1 - 0.05 / 2) * dat[["sei"]],
                          abs(0.15), dat[["sei"]])) +
        stats::pnorm(stats::qnorm(0.05 / 2) *
                       dat[["sei"]], abs(0.15),
                     dat[["sei"]]) # Calculate power for each study for an effect of 0.15
      dat[["power_es02"]] <-
        (1 - stats::pnorm(stats::qnorm(1 - 0.05 / 2) * dat[["sei"]],
                          abs(0.2), dat[["sei"]])) +
        stats::pnorm(stats::qnorm(0.05 / 2) *
                       dat[["sei"]], abs(0.2),
                     dat[["sei"]]) # Calculate power for each study for an effect of 0.2
      dat[["power_es025"]] <-
        (1 - stats::pnorm(stats::qnorm(1 - 0.05 / 2) * dat[["sei"]],
                          abs(0.25), dat[["sei"]])) +
        stats::pnorm(stats::qnorm(0.05 / 2) *
                       dat[["sei"]], abs(0.25),
                     dat[["sei"]]) # Calculate power for each study for an effect of 0.25
      dat[["power_es03"]] <-
        (1 - stats::pnorm(stats::qnorm(1 - 0.05 / 2) * dat[["sei"]],
                          abs(0.3), dat[["sei"]])) +
        stats::pnorm(stats::qnorm(0.05 / 2) *
                       dat[["sei"]], abs(0.3),
                     dat[["sei"]]) # Calculate power for each study for an effect of 0.3
      dat[["power_es035"]] <-
        (1 - stats::pnorm(stats::qnorm(1 - 0.05 / 2) * dat[["sei"]],
                          abs(0.35), dat[["sei"]])) +
        stats::pnorm(stats::qnorm(0.05 / 2) *
                       dat[["sei"]], abs(0.35),
                     dat[["sei"]]) # Calculate power for each study for an effect of 0.35
      dat[["power_es04"]] <-
        (1 - stats::pnorm(stats::qnorm(1 - 0.05 / 2) * dat[["sei"]],
                          abs(0.4), dat[["sei"]])) +
        stats::pnorm(stats::qnorm(0.05 / 2) *
                       dat[["sei"]], abs(0.4),
                     dat[["sei"]]) # Calculate power for each study for an effect of 0.4
      dat[["power_es045"]] <-
        (1 - stats::pnorm(stats::qnorm(1 - 0.05 / 2) * dat[["sei"]],
                          abs(0.45), dat[["sei"]])) +
        stats::pnorm(stats::qnorm(0.05 / 2) *
                       dat[["sei"]], abs(0.45),
                     dat[["sei"]]) # Calculate power for each study for an effect of 0.45
      dat[["power_es05"]] <-
        (1 - stats::pnorm(stats::qnorm(1 - 0.05 / 2) * dat[["sei"]],
                          abs(0.5), dat[["sei"]])) +
        stats::pnorm(stats::qnorm(0.05 / 2) *
                       dat[["sei"]], abs(0.5),
                     dat[["sei"]]) # Calculate power for each study for an effect of 0.5
      dat <- as.data.frame(dat)
      power_median_dat <- data.frame(power_es_observed=numeric(1),
                                     es005=numeric(1),
                                     es01=numeric(1),
                                     es015=numeric(1),
                                     es02=numeric(1),
                                     es025=numeric(1),
                                     es03=numeric(1),
                                     es035=numeric(1),
                                     es04=numeric(1),
                                     es045=numeric(1),
                                     es05=numeric(1)
      )
      power_median_dat[["es_observed"]] <- median(dat[["power_es_observed"]])
      power_median_dat[["es005"]] <- median(dat[["power_es005"]])
      power_median_dat[["es01"]] <- median(dat[["power_es01"]])
      power_median_dat[["es015"]] <- median(dat[["power_es015"]])
      power_median_dat[["es02"]] <- median(dat[["power_es02"]])
      power_median_dat[["es025"]] <- median(dat[["power_es025"]])
      power_median_dat[["es03"]] <- median(dat[["power_es03"]])
      power_median_dat[["es035"]] <- median(dat[["power_es035"]])
      power_median_dat[["es04"]] <- median(dat[["power_es04"]])
      power_median_dat[["es045"]] <- median(dat[["power_es045"]])
      power_median_dat[["es05"]] <- median(dat[["power_es05"]])
      power_median_dat <- as.data.frame(power_median_dat)
      power_median_dat <- dplyr::mutate(power_median_dat,
                                        meta_analysis_name = name)
    } else {
      if(size == "medium"){
        dat[["power_es_observed"]] <-
          (1 - stats::pnorm(stats::qnorm(1 - 0.05 / 2) * dat[["sei"]],
                            abs(observed_es), dat[["sei"]])) +
          stats::pnorm(stats::qnorm(0.05 / 2) *
                         dat[["sei"]], abs(observed_es),
                       dat[["sei"]]) # Calculate power for each study for observed effect
        dat[["power_es01"]] <-
          (1 - stats::pnorm(stats::qnorm(1 - 0.05 / 2) * dat[["sei"]],
                            abs(0.1), dat[["sei"]])) +
          stats::pnorm(stats::qnorm(0.05 / 2) *
                         dat[["sei"]], abs(0.1),
                       dat[["sei"]]) # Calculate power for each study for an effect of 0.1
        dat[["power_es02"]] <-
          (1 - stats::pnorm(stats::qnorm(1 - 0.05 / 2) * dat[["sei"]],
                            abs(0.2), dat[["sei"]])) +
          stats::pnorm(stats::qnorm(0.05 / 2) *
                         dat[["sei"]], abs(0.2),
                       dat[["sei"]]) # Calculate power for each study for an effect of 0.2
        dat[["power_es03"]] <-
          (1 - stats::pnorm(stats::qnorm(1 - 0.05 / 2) * dat[["sei"]],
                            abs(0.30), dat[["sei"]])) +
          stats::pnorm(stats::qnorm(0.05 / 2) *
                         dat[["sei"]], abs(0.30),
                       dat[["sei"]]) # Calculate power for each study for an effect of 0.3
        dat[["power_es04"]] <-
          (1 - stats::pnorm(stats::qnorm(1 - 0.05 / 2) * dat[["sei"]],
                            abs(0.40), dat[["sei"]])) +
          stats::pnorm(stats::qnorm(0.05 / 2) *
                         dat[["sei"]], abs(0.40),
                       dat[["sei"]]) # Calculate power for each study for an effect of 0.4
        dat[["power_es05"]] <-
          (1 - stats::pnorm(stats::qnorm(1 - 0.05 / 2) * dat[["sei"]],
                            abs(0.50), dat[["sei"]])) +
          stats::pnorm(stats::qnorm(0.05 / 2) *
                         dat[["sei"]], abs(0.50),
                       dat[["sei"]]) # Calculate power for each study for an effect of 0.5
        dat[["power_es06"]] <-
          (1 - stats::pnorm(stats::qnorm(1 - 0.05 / 2) * dat[["sei"]],
                            abs(0.60), dat[["sei"]])) +
          stats::pnorm(stats::qnorm(0.05 / 2) *
                         dat[["sei"]], abs(0.60),
                       dat[["sei"]]) # Calculate power for each study for an effect of 0.6
        dat[["power_es07"]] <-
          (1 - stats::pnorm(stats::qnorm(1 - 0.05 / 2) * dat[["sei"]],
                            abs(0.70), dat[["sei"]])) +
          stats::pnorm(stats::qnorm(0.05 / 2) *
                         dat[["sei"]], abs(0.70),
                       dat[["sei"]]) # Calculate power for each study for an effect of 0.7
        dat[["power_es08"]] <-
          (1 - stats::pnorm(stats::qnorm(1 - 0.05 / 2) * dat[["sei"]],
                            abs(0.80), dat[["sei"]])) +
          stats::pnorm(stats::qnorm(0.05 / 2) *
                         dat[["sei"]], abs(0.80),
                       dat[["sei"]]) # Calculate power for each study for an effect of 0.8
        dat[["power_es09"]] <-
          (1 - stats::pnorm(stats::qnorm(1 - 0.05 / 2) * dat[["sei"]],
                            abs(0.90), dat[["sei"]])) +
          stats::pnorm(stats::qnorm(0.05 / 2) *
                         dat[["sei"]], abs(0.90),
                       dat[["sei"]]) # Calculate power for each study for an effect of 0.9
        dat[["power_es1"]] <-
          (1 - stats::pnorm(stats::qnorm(1 - 0.05 / 2) * dat[["sei"]],
                            abs(1), dat[["sei"]])) +
          stats::pnorm(stats::qnorm(0.05 / 2) *
                         dat[["sei"]], abs(1),
                       dat[["sei"]]) # Calculate power for each study for an effect of 1
        dat <- as.data.frame(dat)
        power_median_dat <- data.frame(power_es_observed=numeric(1),
                                       es01=numeric(1),
                                       es02=numeric(1),
                                       es03=numeric(1),
                                       es04=numeric(1),
                                       es05=numeric(1),
                                       es06=numeric(1),
                                       es07=numeric(1),
                                       es08=numeric(1),
                                       es09=numeric(1),
                                       es1=numeric(1)
                                       
        )
        power_median_dat[["es_observed"]] <- median(dat[["power_es_observed"]])
        power_median_dat[["es01"]] <- median(dat[["power_es01"]])
        power_median_dat[["es02"]] <- median(dat[["power_es02"]])
        power_median_dat[["es03"]] <- median(dat[["power_es03"]])
        power_median_dat[["es04"]] <- median(dat[["power_es04"]])
        power_median_dat[["es05"]] <- median(dat[["power_es05"]])
        power_median_dat[["es06"]] <- median(dat[["power_es06"]])
        power_median_dat[["es07"]] <- median(dat[["power_es07"]])
        power_median_dat[["es08"]] <- median(dat[["power_es08"]])
        power_median_dat[["es09"]] <- median(dat[["power_es09"]])
        power_median_dat[["es1"]] <- median(dat[["power_es1"]])
        power_median_dat <- as.data.frame(power_median_dat)
        power_median_dat <- dplyr::mutate(power_median_dat,
                                          meta_analysis_name = name)
      } else {
        if(size == "large"){
          dat[["power_es_observed"]] <-
            (1 - stats::pnorm(stats::qnorm(1 - 0.05 / 2) * dat[["sei"]],
                              abs(observed_es), dat[["sei"]])) +
            stats::pnorm(stats::qnorm(0.05 / 2) *
                           dat[["sei"]], abs(observed_es),
                         dat[["sei"]]) # Calculate power for each study for observed effect
          dat[["power_es025"]] <-
            (1 - stats::pnorm(stats::qnorm(1 - 0.05 / 2) * dat[["sei"]],
                              abs(0.25), dat[["sei"]])) +
            stats::pnorm(stats::qnorm(0.05 / 2) *
                           dat[["sei"]], abs(0.25),
                         dat[["sei"]]) # Calculate power for each study for an effect of 0.25
          dat[["power_es05"]] <-
            (1 - stats::pnorm(stats::qnorm(1 - 0.05 / 2) * dat[["sei"]],
                              abs(0.5), dat[["sei"]])) +
            stats::pnorm(stats::qnorm(0.05 / 2) *
                           dat[["sei"]], abs(0.5),
                         dat[["sei"]]) # Calculate power for each study for an effect of 0.5
          dat[["power_es075"]] <-
            (1 - stats::pnorm(stats::qnorm(1 - 0.05 / 2) * dat[["sei"]],
                              abs(0.75), dat[["sei"]])) +
            stats::pnorm(stats::qnorm(0.05 / 2) *
                           dat[["sei"]], abs(0.75),
                         dat[["sei"]]) # Calculate power for each study for an effect of 0.75
          dat[["power_es1"]] <-
            (1 - stats::pnorm(stats::qnorm(1 - 0.05 / 2) * dat[["sei"]],
                              abs(1), dat[["sei"]])) +
            stats::pnorm(stats::qnorm(0.05 / 2) *
                           dat[["sei"]], abs(1),
                         dat[["sei"]]) # Calculate power for each study for an effect of 1
          dat[["power_es125"]] <-
            (1 - stats::pnorm(stats::qnorm(1 - 0.05 / 2) * dat[["sei"]],
                              abs(1.25), dat[["sei"]])) +
            stats::pnorm(stats::qnorm(0.05 / 2) *
                           dat[["sei"]], abs(1.25),
                         dat[["sei"]]) # Calculate power for each study for an effect of 1.25
          dat[["power_es15"]] <-
            (1 - stats::pnorm(stats::qnorm(1 - 0.05 / 2) * dat[["sei"]],
                              abs(1.5), dat[["sei"]])) +
            stats::pnorm(stats::qnorm(0.05 / 2) *
                           dat[["sei"]], abs(1.5),
                         dat[["sei"]]) # Calculate power for each study for an effect of 1.5
          dat[["power_es175"]] <-
            (1 - stats::pnorm(stats::qnorm(1 - 0.05 / 2) * dat[["sei"]],
                              abs(1.75), dat[["sei"]])) +
            stats::pnorm(stats::qnorm(0.05 / 2) *
                           dat[["sei"]], abs(1.75),
                         dat[["sei"]]) # Calculate power for each study for an effect of 1.75
          dat[["power_es2"]] <-
            (1 - stats::pnorm(stats::qnorm(1 - 0.05 / 2) * dat[["sei"]],
                              abs(2), dat[["sei"]])) +
            stats::pnorm(stats::qnorm(0.05 / 2) *
                           dat[["sei"]], abs(2),
                         dat[["sei"]]) # Calculate power for each study for an effect of 2
          dat[["power_es225"]] <-
            (1 - stats::pnorm(stats::qnorm(1 - 0.05 / 2) * dat[["sei"]],
                              abs(2.25), dat[["sei"]])) +
            stats::pnorm(stats::qnorm(0.05 / 2) *
                           dat[["sei"]], abs(2.25),
                         dat[["sei"]]) # Calculate power for each study for an effect of 2.25
          dat[["power_es25"]] <-
            (1 - stats::pnorm(stats::qnorm(1 - 0.05 / 2) * dat[["sei"]],
                              abs(2.5), dat[["sei"]])) +
            stats::pnorm(stats::qnorm(0.05 / 2) *
                           dat[["sei"]], abs(2.5),
                         dat[["sei"]]) # Calculate power for each study for an effect of 2.5
          dat <- as.data.frame(dat)
          power_median_dat <- data.frame(power_es_observed=numeric(1),
                                         es025=numeric(1),
                                         es05=numeric(1),
                                         es075=numeric(1),
                                         es1=numeric(1),
                                         es125=numeric(1),
                                         es15=numeric(1),
                                         es175=numeric(1),
                                         es2=numeric(1),
                                         es225=numeric(1),
                                         es25=numeric(1)
                                         
          )
          power_median_dat[["es_observed"]] <- median(dat[["power_es_observed"]])
          power_median_dat[["es025"]] <- median(dat[["power_es025"]])
          power_median_dat[["es05"]] <- median(dat[["power_es05"]])
          power_median_dat[["es075"]] <- median(dat[["power_es075"]])
          power_median_dat[["es1"]] <- median(dat[["power_es1"]])
          power_median_dat[["es125"]] <- median(dat[["power_es125"]])
          power_median_dat[["es15"]] <- median(dat[["power_es15"]])
          power_median_dat[["es175"]] <- median(dat[["power_es175"]])
          power_median_dat[["es2"]] <- median(dat[["power_es2"]])
          power_median_dat[["es225"]] <- median(dat[["power_es225"]])
          power_median_dat[["es25"]] <- median(dat[["power_es25"]])
          power_median_dat <- as.data.frame(power_median_dat)
          power_median_dat <- dplyr::mutate(power_median_dat,
                                            meta_analysis_name = name)
        }
        
      }
    }
    value <- list(
      dat = dat,
      power_median_dat = power_median_dat
    ) # Create a list of output objects
    attr(value, "class") <- "mapower_se_t2"
    value
  }
