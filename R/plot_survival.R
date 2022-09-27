#'
#' Plot of survival functions of samples from TCGA study with CNH below or equal to median vs samples with CNH above median
#'
#' \code{plot_survival} uses survival data for TCGA study and values of CNH to create plot
#' exhibiting Kaplan Meier survival curves for two groups of samples:
#' those with CNH below median and those with CNH above median. P-value for the log-rank test
#' of equality of the survival curves is returned in the plot together with the number at risk table.
#'
#' @param study_name name of TCGA study
#' @param survival_time vector of survival times of subjects in the study
#' @param survival_event vector of survival events for subjects in the study
#' @param cnh vector of CNH values
#' @param type string to be placed in legend (e.g., 'unfiltered CNH+')
#' @param ylim permits to limit the y axis in survival plot
#' @return ggsurvplot
#' @seealso \code{analyze_TCGA_study}
#' @export
plot_survival = function(study_name, survival_time, survival_event, cnh, type, ylim = c(0,1)) {
  #
  checkmate::assert(
    checkmate::check_character(study_name),
    checkmate::check_vector(survival_time),
    checkmate::check_vector(survival_event),
    checkmate::check_vector(cnh)
  )
  #
  #
  survival_time = survival_time/365.25  # survival in yrs
  #
  # median cnh
  med_cnh = median(cnh, na.rm=TRUE)
  #
  # create variable
  group = cnh <= med_cnh # le median
  group = as.factor(group)
  levels(group)= c('CNH high', 'CNH low')
  group = relevel(group, ref = 'CNH low')
  #
  # exclude NAs in cnh
  iex = which(is.na(cnh) == TRUE)
  if (length(iex) > 0) {
    survival_time = survival_time[-iex]
    survival_event = survival_event[-iex]
    group = group[-iex]
  }
  #
  # km fit
  da = data.frame(group = group, survival_time = survival_time, survival_event = survival_event)
  km_cnh = survival::survfit(survival::Surv(time = survival_time, event = survival_event) ~ group, data = da, na.action = "na.omit")
  #
  # plot
  gg_cnh = survminer::ggsurvplot(km_cnh, data = da,
                                 pval = TRUE,
                                 pval.coord = c(0, ylim[1] + 0.1*(ylim[2] - ylim[1])),
                                 xlim = c(0,10),
                                 ylim = ylim,
                                 break.time.by = 2,
                                 palette = 'lancet',
                                 legend.title = '',
                                 legend.labs = c(paste0(type, ' low'),
                                                 paste0(type, ' high')),
                                 risk.table = TRUE,
                                 risk.table.col = 'strata',
                                 risk.table.y.text = FALSE,
                                 risk.table.height = 0.15,
                                 title = paste0('TCGA-', study_name, ', KM for ', type))
  # larger font in legend
  gg_cnh$plot = gg_cnh$plot +
    ggplot2::theme(legend.text = ggplot2::element_text(size = 12)) #, color = "black", face = "bold"))
  #
  #
  return(gg_cnh)
  #
}
