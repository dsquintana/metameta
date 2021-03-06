#' Studies on the impact of intranasal oxytocin on emotion recognition in neurodevelopmental disorders
#'
#' This is data from a meta-analysis on 12 studies investigating the impact of intranasal oxytocin
#' administration on emotion recognition in neurodevelopmental disorders. Postive effect size values
#' are indicative of a positive effect of intranasal oxytocin on emotion recognition. Effect size
#' measure and confidence intervals were extracted from Figure 2 of the article (see reference below).
#'
#'
#' @format A data frame with 13 rows and 3 columns:
#' \describe{
#'   \item{study}{The name of the first author of the study and the year it was published}
#'   \item{yi}{Effect size measure (Hedges' g)}
#'   \item{lower}{The lower bound of a 95\% confidence interval}
#'   \item{upper}{The upper bound of a 95\% confidence interval}
#'   ...
#' }
#'
#' @references{
#'    Keech, B., Crowe, S., & Hocking, D. R. (2018). Intranasal oxytocin,
#'    social cognition and neurodevelopmental disorders: a meta-analysis
#'     \emph{Psychoneuroendocrinology}, \bold{87}, 9--19.
#'  }
#'@examples
#'power_keech <-
#'    mapower_ul(
#'    dat = dat_keech,
#'    observed_es = 0.08,
#'    name = "Keech et al 2018"
#'    )
"dat_keech"
