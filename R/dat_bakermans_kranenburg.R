#' Studies on the impact of intranasal oxytocin in clinical groups
#'
#' This is data from a meta-analysis on 19 studies investigating the impact of intranasal oxytocin
#' administration on various clinical-related outcomes in clinical groups. Postive effect size values
#' are indicative of a positive effect of intranasal oxytocin on outcomes. Effect size
#' measure confidence intervals, and standard errors were extracted from Figure 2 of the
#' article (see reference below).
#'
#'
#' @format A data frame with 19 rows and 5 columns:
#' \describe{
#'   \item{study}{The name of the first author of the study and the year it was published}
#'   \item{yi}{Effect size measure (Cohen's d)}
#'   \item{sei}{The standard error}
#'   ...
#' }
#
#' @references{
#'    Bakermans-Kranenburg MJ, van I Jzendoorn MH. Sniffing around oxytocin: review and
#'    meta-analyses of trials in healthy and clinical groups with implications for
#'    pharmacotherapy.
#'     \emph{Translational Psychiatry}, \bold{3}, e258.
#'  }
#'@examples
#'power_bakermans_kranenburg <-
#'    mapower_se(
#'    dat = dat_bakermans_kranenburg,
#'    observed_es = 0.32,
#'    name = "Bakermans-Kranenburg et al 2013"
#'    )
#'
"dat_bakermans_kranenburg"



