#' Caterpillar Plot
#'
#' Plots empirical Bayes both point prediction and prediction intervals for each random effect parameter across all groups.
#'
#' @param model A given lmer model.
#'
#' @param grouping The name of the grouping variable of interest, as a character string.
#'
#' @param title The title of the plot.
#'
#' @param tall Logical argument specifying whether the plot should be plotted vertically or horizontally.
#'
#' @param grey Logical argument specifying whether the intervals should be plotted in color or greyscale.
#'
#' @references Rabe-Hesketh S, Skrondal A (2012). Multilevel and Longitudinal Modeling Using Stata, Volumes I and II, Third Edition. 3 edition edition. Stata Press. ISBN 978-1-59718-108-2.
#'
#' @return Produces a caterpillar plot.
#'
#' @examples
#' # Read in data
#' data(instruction)
#' # Create model
#' mod <- lme4::lmer(mathgain ~ (1 | classid), data = instruction)
#' # Produce caterpillar plot
#' caterpillarPlot(mod, title = "title", grouping = "classid", grey = TRUE)
#'
#' @export caterpillarPlot

caterpillarPlot <- function(model, grouping, title = print(grouping), tall = TRUE, grey = FALSE){
  condsd <- condval <- grp <- interval_95 <- NULL

  model.re <- data.frame(lme4::ranef(model))
  model.re <- model.re[which(model.re$grpvar == grouping),]

  model.re$int_pos2 <- model.re$condval + 2*model.re$condsd
  model.re$int_neg2 <- model.re$condval - 2*model.re$condsd
  model.re$interval_95 <- rep(NA, length(model.re$int_pos2))

  for(i in 1:length(model.re$int_pos2)){

    if(model.re$int_pos2[i] > 0 & model.re$int_neg2[i] > 0){

      model.re$interval_95[i] <- "all positive"

    }else if(model.re$int_pos2[i] < 0 & model.re$int_neg2[i] < 0){

      model.re$interval_95[i] <- "all negative"

    }else{

      model.re$interval_95[i] <- "includes 0"

    }

  }

  caterpillar.plot <- if(tall == TRUE){

    if(grey == TRUE){

      ggplot2::ggplot(model.re, ggplot2::aes(y=grp,x=condval, color=interval_95)) +
        ggplot2::scale_colour_manual(values = c("Grey27", "Black", "Grey87"), name = "95% Prediction Interval") +
        ggplot2::xlab("Empirical Bayes Prediction") +
        ggplot2::ylab("Group ID") +
        ggplot2::ggtitle(title) +
        ggplot2::theme(axis.text.y = ggplot2::element_blank(),
                       axis.ticks = ggplot2::element_blank()) +
        ggplot2::geom_point() + ggplot2::facet_wrap(~term,scales="free_x") +
        ggplot2::geom_vline(ggplot2::aes(xintercept = 0), linetype = 2) +
        ggplot2::geom_errorbarh(ggplot2::aes(xmin=condval -2*condsd,
                                             xmax=condval +2*condsd), height=0)

    }else if(grey == FALSE){

      ggplot2::ggplot(model.re, ggplot2::aes(y=grp,x=condval, color=interval_95)) +
        ggplot2::scale_colour_manual(values = c("Red", "Green", "Blue"), name = "95% Prediction Interval") +
        ggplot2::xlab("Empirical Bayes Prediction") +
        ggplot2::ylab("Group ID") +
        ggplot2::ggtitle(title) +
        ggplot2::theme(axis.text.y = ggplot2::element_blank(),
                       axis.ticks = ggplot2::element_blank()) +
        ggplot2::geom_point() + ggplot2::facet_wrap(~term,scales="free_x") +
        ggplot2::geom_vline(ggplot2::aes(xintercept = 0), linetype = 2) +
        ggplot2::geom_errorbarh(ggplot2::aes(xmin=condval -2*condsd,
                                             xmax=condval +2*condsd), height=0)

    }

  }else if(tall == FALSE){

    if(grey == TRUE){

      ggplot2::ggplot(model.re, ggplot2::aes(y=grp,x=condval, color=interval_95)) +
        ggplot2::scale_colour_manual(values = c("Grey27", "Black", "Grey87"), name = "95% Prediction Interval") +
        ggplot2::xlab("Empirical Bayes Prediction") +
        ggplot2::ylab("Group ID") +
        ggplot2::ggtitle(title) +
        ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                       axis.ticks = ggplot2::element_blank()) +
        ggplot2::geom_point() + ggplot2::facet_wrap(~term,scales="free_x", ncol = 1) +
        ggplot2::geom_vline(ggplot2::aes(xintercept = 0), linetype = 2) +
        ggplot2::geom_errorbarh(ggplot2::aes(xmin=condval -2*condsd,
                                             xmax=condval +2*condsd), height=0) +
        ggplot2::coord_flip()

    }else if(grey == FALSE){

      ggplot2::ggplot(model.re, ggplot2::aes(y=grp,x=condval, color=interval_95)) +
        ggplot2::scale_colour_manual(values = c("Red", "Green", "Blue"), name = "95% Prediction Interval") +
        ggplot2::xlab("Empirical Bayes Prediction") +
        ggplot2::ylab("Group ID") +
        ggplot2::ggtitle(title) +
        ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                       axis.ticks = ggplot2::element_blank()) +
        ggplot2::geom_point() + ggplot2::facet_wrap(~term,scales="free_x", ncol = 1) +
        ggplot2::geom_vline(ggplot2::aes(xintercept = 0), linetype = 2) +
        ggplot2::geom_errorbarh(ggplot2::aes(xmin=condval -2*condsd,
                                             xmax=condval +2*condsd), height=0) +
        ggplot2::coord_flip()

    }

  }

  result <- caterpillar.plot
  return(result)

}
