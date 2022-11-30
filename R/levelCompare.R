#' Reports on a comparison of the mixed model vs. a model that does not account for nesting
#'
#' Uses AIC and BIC to compare whether the model that accounts for the correlation of responses within the same unit fits the data better than a model that assumes 0 correlation between responses within the same unit.
#'
#' @param model A linear mixed-effects model of class lmerMod or lmerModLmerTest
#'
#' @return Computes the AIC and BIC of the requested model and a model with the same predictors but absent the random intercept(s) and slope(s).
#'
#' @references Burnham, K. P., & Anderson, D. R. (2004). Multimodel inference: understanding AIC and BIC in model selection. Sociological methods & research, 33(2), 261-304.
#' Raftery, A. E. (1995). Bayesian model selection in social research. Sociological methodology, 111-163.
#'
#' @examples
#'
#' \donttest{
#' data(instruction)
#' mod <- lme4::lmer(mathgain ~ (1 | classid), data = instruction)
#' levelCompare(mod)
#' }
#'
#' @export levelCompare

levelCompare <- function(model) {

  mods <- list(model)

  # Model class must be 'lmerMod' or 'lmerModLmerTest'
  classCheck <- vapply(mods, function(x) class(x) == "lmerMod" || class(x) == "lmerModLmerTest", NA)
  if (!all(classCheck)) {
    stop("Model class is not 'lmerMod' or 'lmerModLmerTest'.", call. = FALSE)
    return(NULL)
  }

  # Random slopes
  if (lme4::getME(model, name = 'n_rtrms') == 1 & lme4::getME(model, name = 'cnms')[1] != "(Intercept)") {
    stop("levelCompare cannot be calculated for models containing random slopes.", call. = FALSE)
    return(NULL)
  }

  if (lme4::getME(model, name = 'n_rtrms') == 2 & lme4::getME(model, name = 'cnms')[1] != "(Intercept)" & lme4::getME(model, name = 'cnms')[2] != "(Intercept)") {
    stop("levelCompare cannot be calculated for models containing random slopes.", call. = FALSE)
    return(NULL)
  }

  # Number of random intercepts
  if (lme4::getME(model, name = 'n_rtrms') > 2) {
    stop("levelCompare cannot be calculated for models containing more than two random effects.", call. = FALSE)
    return(NULL)
  }

  # Offset
  calls <- lapply(mods, stats::getCall)
  off <- lapply(calls, `[[`, "offset")[[1]]
  if(!is.null(off)) {
    stop("levelCompare cannot be calculated for models using the offset argument", call. = FALSE)
    return(NULL)
  }

  #Weights
  w <- lapply(calls, `[[`, "weights")[[1]]
  #Subsets
  subsets <- lapply(calls, `[[`, "subset")[[1]]
  # Get data
  d <- lapply(calls, `[[`, "data")[[1]]
  forms <- lapply(lapply(calls, `[[`, "formula"), deparse)
  ff1 <- forms[[1]]
  ff2 <- strsplit(ff1, "[~+]")[[1]]
  ff2 <- ff2[!grepl("|", ff2, fixed = TRUE)]
  int.fill <- ifelse(length(ff2) == 1, "1", "")
  ff3 <- paste(ff2[1], "~", int.fill, paste(ff2[-1], collapse = " + "))
  ff4 <- ifelse(is.null(subsets) & is.null(w), paste("lm(",ff3, ", data = ", deparse(d), ")"),
                ifelse(!is.null(subsets) & is.null(w), paste("lm(", ff3, ", data = ", deparse(d), ", subset = ", deparse(subsets), ")"),
                       ifelse(is.null(subsets) & !is.null(w), paste("lm(", ff3, ", data = ", deparse(d), ", weights = ", w, ")"),
                              paste("lm(", ff3, ", data = ", deparse(d),", subset = ", deparse(subsets), ", weights = ", w, ")"))))

  lmModel <- eval(parse(text = ff4), parent.frame())

  # BIC
  bic1 <- stats::BIC(lmModel) # lm
  bic2 <- stats::BIC(model)  # lmer
  bDiff <- (bic1 - bic2)

  if(bDiff <= 0){
    eviB <- "no"
  } else {
    eviB <- ifelse(bDiff < 2, "weak",
                   ifelse(bDiff >= 2 & bDiff < 6, "positive",
                          ifelse(bDiff >= 6 & bDiff < 10, "strong", "very strong")))
  }

  # AIC
  aic1 <- stats::AIC(lmModel) # lm
  aic2 <- stats::AIC(model)  # lmer

  minMod <- ifelse(aic1 < aic2, aic1, aic2)
  otherAIC <- ifelse(minMod == aic1, aic2, aic1)
  aDiff <- otherAIC - minMod

  betterModName <- ifelse(minMod == aic1, "single-level model", "multi-level model")
  worseModName <- ifelse(minMod == aic1, "multi-level model", "single-level model")

  eviA <- ifelse(aDiff <= 2, "strong",
                 ifelse(aDiff > 2 & aDiff <= 7, "moderate",
                        ifelse(aDiff > 7 & aDiff <= 10, "weak", "no")))

  # Output
  res <- (list(
    "eviB" = eviB,
    "eviA" = eviA,
    "betterModName" = betterModName,
    "worseModName" = worseModName))
  class(res) <- "levelCompare"
  return(res)
}





















