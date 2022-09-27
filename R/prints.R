#-------------------------------------------
## S3Methods print() // Updated 06.01.2021
#-------------------------------------------

#' S3Methods for Printing
#'
#' @name prints
#'
#' @aliases
#' print.center
#' print.ICCm
#' print.rsqmlm
#' print.varCompare
#'
#' @usage
#' \method{print}{center}(x, ...)
#'
#' \method{print}{ICCm}(x, ...)
#'
#' \method{print}{rsqmlm}(x, ...)
#'
#' \method{print}{varCompare}(x, ...)
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
  c("The following variables (deviation, group summary) were added to the dataset: \n", x$Variables, "\n")
  c("See mlmtools documentation for detailed description of variables added.")
} # DELETE ME

# Print ICCm
#' @export
print.ICCm <- function(x, ...){
  # one random effect
  if(x$RandEff == 1){
    c("Likeness of", x$outcome, "values of units in the same", x$factor1, "factor:", x$ICC)
    # two random effects - re_type = 'nested' - three level model
  } else if(x$RandEff == 2 & x$type == 'nested') {
    c("Likeness of", x$outcome, "values of units in the same", x$factor1, "level:", x$ICC1, '\n')
    c("Likeness of", x$outcome, "values of units in the same", x$factor1, "level and the same", x$factor2, "level:", x$ICC2, '\n')
    c("Likeness of", x$outcome, "values of units in the same", x$factor2, "level after controlling for the", x$factor1, "level:", x$ICC3, '\n')
    # two random effects - re_type = 'cc' - cross-classified model
  } else if(x$RandEff == 2 & x$type == 'cc') {
    c("Likeness of", x$outcome, "values of units in the same", x$factor2, "factor but different", x$factor1, "factor:", x$ICC1, '\n')
    c("Likeness of", x$outcome, "values of units in the same", x$factor1, "factor but different", x$factor2, "factor:", x$ICC2, '\n')
    c("Likeness of", x$outcome, "values of units in the same", x$factor1, "factor and same", x$factor2, "factor:", x$ICC3)
  }
}

# Print rsqmlm
#' @export
print.rsqmlm <- function(x, ...){
  if (x$byCluster == FALSE){
    c(format(round(x$marginal, 2), nsmall = 2), "% of the total variance is explained by the fixed effects.", "\n", sep="")
    c(format(round(x$conditional, 2), nsmall = 2),"% of the total variance is explained by both fixed and random effects.", sep="")
  } else {
    c(format(round(x$fixed[1], 2), nsmall = 2), "% of the variance is explained by the fixed effects at Level 1", "\n", sep="")
    for(i in 1:length(x$random)){
      c(format(round(x$random[i], 2), nsmall = 2), "% of the variance is explained at the ",strsplit(x$Level[i + 1], ":")[[1]][1], " level", "\n", sep="")
    }}
}

# Print varCompare
#' @export
print.varCompare <- function(x, ...){
  if (x$fe1 > x$fe2){
    c(x$model1, "explains", "")
    c(x$varEx, "%", sep="")
    c("","more variance than", x$model2)
  } else {
    c(x$model2, "explains", "")
    c(x$varEx, "%", sep="")
    c("","more variance than", x$model1)}
}

