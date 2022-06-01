#-------------------------------------------
## S3Methods print() // Updated 09.30.2021
#-------------------------------------------

#' S3Methods for Printing
#'
#' @name prints
#'
#' @aliases
#' print.center
#' print.ICCm
#' print.levelCompare
#' print.rsqmlm
#' print.varCompare
#' print.mlm_assumptions
#'
#' @usage
#' \method{print}{center}(x, ...)
#'
#' \method{print}{ICCm}(x, ...)
#'
#' \method{print}{levelCompare}(x, ...)
#'
#' \method{print}{rsqmlm}(x, ...)
#'
#' \method{print}{varCompare}(x, ...)
#'
#' \method{print}{mlm_assumptions}(x, ...)
#'
#' @description Prints for \code{mlmtools} objects
#'
#' @param x Object from \code{mlmtools} package
#'
#' @param ... Additional arguments
#'
#' @return Prints \code{mlmtools} object
#'
# Print center
#' @export
print.center <- function(x, ...){
  cat("The following variables (deviation, group summary) were added to the dataset: \n", x$Variables, "\n")
  cat("See mlmtools documentation for detailed description of variables added.")
} # DELETE ME

# Print ICCm
#' @export
print.ICCm <- function(x, ...){
  # one random effect
  if(x$RandEff == 1){
    cat("Likeness of", x$outcome, "values of units in the same", x$factor1, "factor:", x$ICC)
    # two random effects - re_type = 'nested' - three level model
  } else if(x$RandEff == 2 & x$type == 'nested') {
    cat("Likeness of", x$outcome, "values of units in the same", x$factor1, "level:", x$ICC1, '\n')
    cat("Likeness of", x$outcome, "values of units in the same", x$factor1, "level and the same", x$factor2, "level:", x$ICC2, '\n')
    cat("Likeness of", x$outcome, "values of units in the same", x$factor2, "level after controlling for the", x$factor1, "level:", x$ICC3, '\n')
    # two random effects - re_type = 'cc' - cross-classified model
  } else if(x$RandEff == 2 & x$type == 'cc') {
    cat("Likeness of", x$outcome, "values of units in the same", x$factor2, "factor but different", x$factor1, "factor:", x$ICC1, '\n')
    cat("Likeness of", x$outcome, "values of units in the same", x$factor1, "factor but different", x$factor2, "factor:", x$ICC2, '\n')
    cat("Likeness of", x$outcome, "values of units in the same", x$factor1, "factor and same", x$factor2, "factor:", x$ICC3)
  }
}

# Print levelCompare
#' @export
print.levelCompare <- function(x, ...){
  if (x$resModel == "lmerModel"){
    cat("Chisq(", x$Df, ") = ", format(round(x$Chisq, 2), nsmall = 2), ", ", x$pVal.print, ", logLik = ", format(round(x$logLik, 2), nsmall = 2),".", '\n', sep = "")
    cat("The model that accounts for nesting (lmer model) fits the data significantly \nbetter than a model that does not account for nesting (lm model).", '\n')
    cat("This suggests that the random-effects model is needed to account for the \nobserved nesting structure.")
  } else {
    cat("The model that accounts for nesting (lmer model) does not fit the data significantly \nbetter than a model that does not account for nesting (lm model).", '\n')
    cat("This suggests that the fixed-effexts model fits the data just as well as \nthe random-effects model.")}
}

# Print rsqmlm
#' @export
print.rsqmlm <- function(x, ...){
  if (x$byCluster == FALSE){
    cat(format(round(x$marginal, 2), nsmall = 2), "% of the total variance is explained by the fixed effects.", "\n", sep="")
    cat(format(round(x$conditional, 2), nsmall = 2),"% of the total variance is explained by both fixed and random effects.", sep="")
  } else {
    cat(format(round(x$fixed[1], 2), nsmall = 2), "% of the variance is explained by the fixed effects at Level 1", "\n", sep="")
    for(i in 1:length(x$random)){
      cat(format(round(x$random[i], 2), nsmall = 2), "% of the variance is explained at the ",strsplit(x$Level[i + 1], ":")[[1]][1], " level", "\n", sep="")
    }}
}

# Print varCompare
#' @export
print.varCompare <- function(x, ...){
  if (x$fe1 > x$fe2){
    cat(x$model1, "explains", "")
    cat(x$varEx, "%", sep="")
    cat("","more variance than", x$model2)
  } else {
    cat(x$model2, "explains", "")
    cat(x$varEx, "%", sep="")
    cat("","more variance than", x$model1)}
}

# Print mlm_assumptions
#' @export
print.mlm_assumptions <- function(x, ...){
  if(x$homo.test$`Pr(>F)`[1] >= .05){
    cat("Homogeneity of variance assumption met.")
  } else {
    cat("Homogeneity of variance assumption NOT met. See: TO DO ADD RESOURCES")
  }
  if(is.character(x$multicollinearity)){
    cat(x$multicollinearity)
  } else {
    if(any(x$multicollinearity > 5)){
      cat("Multicollinearity detected - VIF value above 5. This might be problematic for the model - consider removing the variable from the model. Check the multicollinearity object for more details.")
    } else {
      cat("No multicollinearity detected in the model.")
    }
  }
  if(length(x$outliers) > 0){
    cat("Outliers detected. See outliers object for more information.")
  } else {
    cat("No outliers detected.")
  }
  cat("Visually inspect all plot objects.  See ?mlm_asssumptions for more information on how to inspect these plots.")
}
