#' Calculate the median study power for a meta-analysis and visualise this data using a Firepower plot
#'
#' Creates a dataset with median power and a firepower plot, which visualises the median power for a 
#' meta-analysis assuming the observed effect size in the meta-analysis is the true effect size and assuming a
#' range of true effect sizes ranging from 0.1 to 1 in increments of 0.1. See 'Details'.
#'
#' \strong{The true effect size}
#'
#' For the purposes of power analysis a "true" effect size needs to be specified. However,
#' the true effect size is unknown when testing hypotheses and reported summary
#' effect sizes are typically inflated. Thus, Firepower plots implements two alternative 
#' categories for a true effect for the purposes of power
#' analysis: The observed summary effect size estimate reported in the meta-analysis and statistical power for
#' a range of true effect sizes, The default "medium" range is from 0.1 to 1, in increments of 0.1.
#' However, a "small" and "large" range is also available (see the "size" argument below for details).
#'
#' @param power_list A list of dataframes with calculated power, derived from the "mapower_ul" or "mapower_se"
#'                   functions. See 'Examples' for how to make a list.
#' @param size The effect size range, as specified using the mapower_se() or mapower_ul() functions,
#'             which created the 'power_list' dataframe or dataframes.
#'             The default is a medium range is 0.1 to 1, in increments of 0.1.
#'             Other options include a "small" range (0.05 to 0.5 in increments of 0.05) and a "large"
#'             range (0.25 to 2.5 in increments of 0.25).
#' @param es An optional string for the name of the effect size for the plot (e.g., "Hedges g").
#'                   The default string is "Effect size".
#' @return This function returns the following:
#'       \item{dat}{A dataset with the median results from power analyses for a range of effect sizes, including the
#'       specified true effect size, in a column labelled "power_true". The additional added columns include
#'       results for power analysis assuming a range of true effect sizes, with the default beginning at 0.1 ("power_es01"),
#'       then 0.2 ("power_es02"), then continuing in increments of 0.1 up to 1 ("power_es1"). A
#'       "small" and "large" effect size range is also possible (see the 'size' argument
#'       for details)}
#'       \item{fp_plot}{A firepower plot}
#' @examples
#' ### Calcuate median power for meta-analyses
#' power_ooi <- mapower_se(dat = dat_ooi, observed_es = 0.178, name = "ooi et al 2017")
#' power_med_ooi <- power_ooi$power_median_dat
#'
#' keech_power <- mapower_ul(dat = dat_keech, observed_es = 0.08, name = "Keech et al 2017")
#' power_med_keech <- keech_power$power_median_dat
#'
#' power_bakermans_kranenburg <- mapower_se(dat = dat_bakermans_kranenburg, observed_es = 0.32, name = "Bakermans-Kranenburg et al 2013")
#' power_med_bakermans_kranenburg <- power_bakermans_kranenburg$power_median_dat
#'
#' ### Create a list
#' list_power <- list(power_med_ooi, power_med_keech, power_med_bakermans_kranenburg)
#'
#' ### Run firepower function
#' fp <- firepower(list_power)
#'
#' ### Extract data from datasets in list
#' power_dat <- fp$dat
#' power_dat
#'
#' ### Create firepower plot
#' fp_plot <- fp$fp_plot
#' fp_plot
#' 
#' ### Create a firepower plot using a large range of effect sizes with a "Hedges' g" label
#' 
#' power_ooi_l <- mapower_se(dat = dat_ooi, size = "large", observed_es = 0.178, name = "ooi et al 2017")
#' power_med_ooi_l <- power_ooi_l$power_median_dat
#' 
#' firepower(list(power_ooi_l$power_median_dat),
#' size = “large”,
#' es = “Hedges’ g”).
#' 



firepower <- function(power_list,
                      size = "medium",
                      es = "Effect size"){
#'@import ggplot2
#'@import tidyr

  if(size == "small"){
    firepower_list <- lapply(power_list, function(x) x[,
                                                       c("es_observed",
                                                         "es005", "es01",
                                                         "es015", "es02",
                                                         "es025", "es03",
                                                         "es035", "es04",
                                                         "es045", "es05",
                                                         "meta_analysis_name"
                                                       )])
    
    firepower_dat_wide <- do.call(rbind.data.frame, firepower_list)
    firepower_dat_wide
    power_med_dat_long <- tidyr::gather(firepower_dat_wide,
                                        effect, power, es_observed:es05,
                                        factor_key=TRUE)
    power_med_dat_long$es_cat <-
      ifelse(power_med_dat_long$effect == "es_observed",
             c("observed_es"),
             c("range_es"))
    power_med_dat_long
    firepower_plot <- ggplot2::ggplot(data = power_med_dat_long) +
      ggplot2::geom_tile(ggplot2::aes(
        x = effect,
        y = meta_analysis_name,
        fill = power
      )) +
      ggplot2::theme(aspect.ratio = 0.3) +
      ggplot2::scale_fill_gradient2(
        name = "Power",
        midpoint = 0.5,
        low = "white",
        mid = "orange",
        high = "red"
      ) +
      ggplot2::facet_grid( ~ es_cat, scale = 'free_x', space = "free_x") +
      ggthemes::theme_tufte(base_family = "sans")
    
    firepower_plot <-
      firepower_plot + ggplot2::theme(strip.text.x = ggplot2::element_blank())
    firepower_plot <- firepower_plot + ggplot2::labs(x = es, y = "")
    firepower_plot <-
      firepower_plot + ggplot2::scale_x_discrete(
        labels = c(
          "es_observed" = "Observed",
          "es005" = "0.05",
          "es01" = "0.1",
          "es015" = "0.15",
          "es02" = "0.2",
          "es025" = "0.25",
          "es03" = "0.3",
          "es035" = "0.35",
          "es04" = "0.4",
          "es045" = "0.45",
          "es05" = "0.5"
        ))
    plot <- firepower_plot
    plot
    
  } else {
    if(size == "medium"){
      firepower_list<- lapply(power_list, function(x) x[,
                                                        c("es_observed",
                                                          "es01", "es02",
                                                          "es03", "es04",
                                                          "es05", "es06",
                                                          "es07", "es08",
                                                          "es09", "es1",
                                                          "meta_analysis_name"
                                                        )])
      
      firepower_dat_wide <- do.call(rbind.data.frame, firepower_list)
      firepower_dat_wide
      power_med_dat_long <- tidyr::gather(firepower_dat_wide,
                                          effect, power, es_observed:es1,
                                          factor_key=TRUE)
      power_med_dat_long$es_cat <-
        ifelse(power_med_dat_long$effect == "es_observed",
               c("observed_es"),
               c("range_es"))
      power_med_dat_long
      firepower_plot <- ggplot2::ggplot(data = power_med_dat_long) +
        ggplot2::geom_tile(ggplot2::aes(
          x = effect,
          y = meta_analysis_name,
          fill = power
        )) +
        ggplot2::theme(aspect.ratio = 0.3) +
        ggplot2::scale_fill_gradient2(
          name = "Power",
          midpoint = 0.5,
          low = "white",
          mid = "orange",
          high = "red"
        ) +
        ggplot2::facet_grid( ~ es_cat, scale = 'free_x', space = "free_x") +
        ggthemes::theme_tufte(base_family = "sans")
      
      firepower_plot <-
        firepower_plot + ggplot2::theme(strip.text.x = ggplot2::element_blank())
      firepower_plot <- firepower_plot + ggplot2::labs(x = es, y = "")
      firepower_plot <-
        firepower_plot + ggplot2::scale_x_discrete(
          labels = c(
            "es_observed" = "Observed",
            "es01" = "0.1",
            "es02" = "0.2",
            "es03" = "0.3",
            "es04" = "0.4",
            "es05" = "0.5",
            "es06" = "0.6",
            "es07" = "0.7",
            "es08" = "0.8",
            "es09" = "0.9",
            "es1" = "1"
          ))
      plot <- firepower_plot
      plot  
      
    } else {
      if(size == "large"){
        firepower_list<- lapply(power_list, function(x) x[,
                                                          c("es_observed",
                                                            "es025", "es05",
                                                            "es075", "es1",
                                                            "es125", "es15",
                                                            "es175", "es2",
                                                            "es225", "es25",
                                                            "meta_analysis_name"
                                                          )])
        
        firepower_dat_wide <- do.call(rbind.data.frame, firepower_list)
        firepower_dat_wide
        power_med_dat_long <- tidyr::gather(firepower_dat_wide,
                                            effect, power, es_observed:es25,
                                            factor_key=TRUE)
        power_med_dat_long$es_cat <-
          ifelse(power_med_dat_long$effect == "es_observed",
                 c("observed_es"),
                 c("range_es"))
        power_med_dat_long
        firepower_plot <- ggplot2::ggplot(data = power_med_dat_long) +
          ggplot2::geom_tile(ggplot2::aes(
            x = effect,
            y = meta_analysis_name,
            fill = power
          )) +
          ggplot2::theme(aspect.ratio = 0.3) +
          ggplot2::scale_fill_gradient2(
            name = "Power",
            midpoint = 0.5,
            low = "white",
            mid = "orange",
            high = "red"
          ) +
          ggplot2::facet_grid( ~ es_cat, scale = 'free_x', space = "free_x") +
          ggthemes::theme_tufte(base_family = "sans")
        
        firepower_plot <-
          firepower_plot + ggplot2::theme(strip.text.x = ggplot2::element_blank())
        firepower_plot <- firepower_plot + ggplot2::labs(x = es, y = "")
        firepower_plot <-
          firepower_plot + ggplot2::scale_x_discrete(
            labels = c(
              "es_observed" = "Observed",
              "es025" = "0.25",
              "es05" = "0.5",
              "es075" = "0.75",
              "es1" = "1",
              "es125" = "1.25",
              "es15" = "1.5",
              "es175" = "1.75",
              "es2" = "2",
              "es225" = "2.25",
              "es25" = "2.5"
            ))
        plot <- firepower_plot
        plot  
        
      }
    }  
  }
  
  value <- list(
    fp_plot = plot,
    dat = firepower_dat_wide
  ) # Create a list of output objects
  attr(value, "class") <- "firepower"
  value
}
