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
  data <- getData(model)

  # Original y variable

  form <- deparse(formula(model))
  y <- trimws(strsplit(form, "[~+]")[[1]][1]) # Extracts dependent variable from the model
  x <- attributes(terms(model))$term.labels # Extracts independent variables from the model

  # Linearity

  linearityplot_fun <- function(xvar){
    ggplot2::ggplot(data, ggplot2::aes_string(x=xvar, y=y)) +
      ggplot2::geom_point()+
      ggplot2::geom_smooth(formula = y ~ x, method=stats::loess) +
      ggplot2::theme_classic()
  }
  linearity.plots <- lapply(x, linearityplot_fun)

  # Homogeneity of Variance

  data$model.Res<- abs(residuals(model))^2 # squares the absolute values of the residuals to provide the more robust estimate
  Levene.model <- lm(model.Res ~ classid, data=data) #ANOVA of the squared residuals
  homo.test <- anova(Levene.model) #displays the results
  data$predicted <- predict(model)
  #create a fitted vs residual plot
  Plot.model <- ggplot2::ggplot(data=data,mapping=ggplot2::aes(x=predicted,y=residuals(model))) +
    ggplot2::geom_point() +
    ggplot2::geom_hline(yintercept=0,linetype="dashed") +
    ggplot2::theme_classic() +
    ggplot2::xlab("Predicted Values") +
    ggplot2::ylab("Model Residuals")

  # Normally distributed residuals

  residlinearity.plot <- ggplot2::ggplot(as.data.frame(cbind(resid(model),data[,y])),
                                         ggplot2::aes(x = V1, y = V2)) +
    ggplot2::geom_point() +
    ggplot2::geom_smooth(formula = y ~ x, method=stats::loess) +
    ggplot2::theme_classic() +
    ggplot2::xlab("Residuals") +
    ggplot2::ylab("Original y")

  data$Leverage = data$predicted/(1 - data$predicted)
  data$mse = (residuals(model))^2/var(residuals(model))
  data$CooksD <- (1/6)*(mse)*Leverage
  Outliers <- rownames(data[data$CooksD > (4/nrow(data)),])

  residNorm <- ggplot2::ggplot(data, ggplot2::aes(qqnorm(residuals(model,scaled = TRUE))[[1]], residuals(model, scaled = TRUE))) +
    ggplot2::geom_point(na.rm = TRUE) +
    try(ggplot2::geom_abline(ggplot2::aes(qqline(residuals(model,scaled = TRUE)))), silent = TRUE) +
    ggplot2::xlab("Theoretical Quantiles") +
    ggplot2::ylab("Standardized Residuals") +
    ggplot2::ggtitle("Normal Q-Q") +
    ggplot2::theme_classic()

  # Multicollinearity

  if (length(x) < 2) stop("model contains fewer than 2 terms")
  v <- as.matrix(vcov(model))
  assign <- attr(model.matrix(model), "assign")
  if (names(fixef(model)[1]) == "(Intercept)") {
    v <- v[-1, -1]
    assign <- assign[-1]
  }
  R <- cov2cor(v)
  detR <- det(R)
  Multicollinearity <- matrix(0, length(x), 3)
  rownames(Multicollinearity) <- x
  colnames(Multicollinearity) <- c("GVIF", "Df", "GVIF^(1/(2*Df))")
  for (term in 1:length(x)) {
    subs <- which(assign == x)
    Multicollinearity[term, 1] <- det(as.matrix(R[subs, subs])) *
      det(as.matrix(R[-subs, -subs])) / detR
    Multicollinearity[x, 2] <- length(subs)
  }
  if (all(Multicollinearity[, 2] == 1)){
    Multicollinearity <- Multicollinearity[, 1]
    } else {
      Multicollinearity[, 3] <- Multicollinearity[, 1]^(1/(2 * Multicollinearity[, 2]))
    }
  Multicollinearity <- Multicollinearity[,1]

  ### TO DO: PRINTS
  if(homo.test$`Pr(>F)`[1] >= .05){
    print("Homogeneity of variance assumption met")
  } else {
    print("Homogeneity of variance assumption NOT met. See: TO DO ADD RESOURCES")
  }
  if(any(Multicollinearity > 5)){
    print("Multicollinearity detected - VIF value above 5. This might be problematic for the model - consider removing the variable from the model. Check the Multicollinearity object for more details.")
  } else {
    print("No multicollinearity detected in the model.")
  }
  if(length(Outliers) > 0){
    print("Outliers detected. See Outliers object for more information.")
  } else {
    print("No outliers detected.")
  }

  result <- list(linearity.plots, homo.test, residlinearity.plot, residNorm, Multicollinearity, Outliers)
  names(result) <- c("linearity.plots", "homo.test", "residlinearity.plot", "residNorm", "Multicollinearity","Outliers")
  suppressMessages(return(result))
}
test <- mlm_assumptions(model)
