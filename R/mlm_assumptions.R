#' Reports the output of testing all assumptions for a multilevel model
#'
#' Reports the results from testing all assumptions of a multilevel model and provides suggestions if an assumption is not passed
#'
#' @param model A linear mixed-effects model of class lmerMod or lmerModLmerTest
#'
#' @return
#'
#' @details
#'
#' @references

mlm_assumptions <- function(model) {
  # Resources:

  #https://stats.stackexchange.com/questions/77891/checking-assumptions-lmer-lme-mixed-models-in-r
  #https://stats.stackexchange.com/questions/376273/assumptions-for-lmer-models
  #https://ademos.people.uic.edu/Chapter18.html#62_assumption_2_homogeneity_of_variance
  #https://bookdown.org/animestina/phd_july_19/testing-the-assumptions.html

  # Model class must be 'lmerMod' or 'lmerModLmerTest'
  classCheck <- class(model) == "lmerMod" || class(model) == "lmerModLmerTest"
  if (!all(classCheck)) {
    stop("Model class is not 'lmerMod' or 'lmerModLmerTest'.", call. = FALSE)
    return(NULL)
  }

  # Data

  call <- stats::getCall(model)
  data <- call[["data"]]
  data <- eval(data, parent.frame())

  # Original y variable

  form <- deparse(call[["formula"]])
  y <- trimws(strsplit(form, "[~+]")[[1]][1])

  # Linearity

  ##### TO DO: should these plot all variables?

  linearity.plot <- plot(resid(model),#extract the residuals
                         data[,y]) #specify original y variable

  # Homogeneity of Variance

  data$model.Res<- abs(residuals(model))^2 # squares the absolute values of the residuals to provide the more robust estimate
  Levene.model <- lm(model.Res ~ classid, data=data) #ANOVA of the squared residuals
  homo.test <- anova(Levene.model) #displays the results
  homo.test$`Pr(>F)`[1] >= .05
  Plot.model <- plot(model) #creates a fitted vs residual plot

  # Normally distributed residuals

  qqnorm(resid(model))
  qqline(resid(model))

  lattice::qqmath(model, id=0.05)

  # Multicollinearity

  ##### TO DO: vif is from car, eigen requires numeric

  # Autocorrelation

  ##### TO DO: do we need this? Longitudinal argument?

}

