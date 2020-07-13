#' Studies on the impact of intranasal oxytocin on social cognition in autism spectrum disorders
#'
#' This is data from a meta-analysis on 9 studies investigating the impact of intranasal oxytocin
#' administration on social cognition in autism spectrum disorders. Postive effect size values
#' are indicative of a positive effect of intranasal oxytocin on social cognition. Effect size
#' measure and confidence intervals were extracted from Figure 2 of the article (see reference below).
#'
#'
#' @format A data frame with 9 rows and 3 columns:
#' \describe{
#'   \item{study}{The name of the first author of the study and the year it was published}
#'   \item{yi}{Effect size measure (Hedges' g)}
#'   \item{sei}{The standard error}
#'   ...
#' }
#
#' @references{
#'    Ooi YP, Weng SJ, Kossowsky J, Gerger H, Sung M. (2017). Oxytocin and Autism Spectrum Disorders:
#'    A Systematic Review and Meta-Analysis of Randomized Controlled Trials.
#'     \emph{Pharmacopsychiatry}, \bold{50}, 5--13.
#'  }
#'@examples
#'power_ooi <-
#'    mapower_se(
#'    dat = dat_ooi,
#'    observed_es = 0.08,
#'    name = "Ooi et al 2017"
#'    )
"dat_ooi"
