#' Reports on a likelihood ratio test of the mixed model vs. a model that does not account for nesting
#'
#' Formal test of whether the model that accounts for the correlation of responses within the same unit fits the data better than a model that assumes 0 correlation between responses within the same unit.
#'
#' @param model A linear mixed-effects model of class lmerMod or lmerModLmerTest
#'
#' @return NEED
#'
#' @details NEED
#'
#' @references NEED

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

  anova.output <- stats::anova(model,lmModel)
  pVal <- anova.output$`Pr(>Chisq)`[2]
  pVal.print <- ifelse(pVal < .001, "p < .001", paste("p = ", format(round(pVal, 3),3)))
  logLik <- anova.output$logLik[2]
  Chisq <- anova.output$Chisq[2]
  Df <- anova.output$Df[2]

  resModel <- ifelse(pVal < .05, "lmerModel", "lmModel")

  # Output
  res <- (list(
    "resModel" = resModel,
    "pVal.print" = pVal.print,
    "logLik" = logLik,
    "Chisq" = Chisq,
    "Df" = Df))
  class(res) <- "mlmtools_levelCompare"
  return(res)
}

print.mlmtools_levelCompare <- function(x){
  if (x$resModel == "lmerModel"){
    cat("Chisq(", x$Df, ") = ", format(round(x$Chisq, 2), nsmall = 2), ", ", x$pVal.print, ", logLik = ", format(round(x$logLik, 2), nsmall = 2),".", '\n', sep = "")
    cat("The model that accounts for nesting (lmer model) fits the data significantly \nbetter than a model that does not account for nesting (lm model).", '\n')
    cat("This suggests that the random-effects model is needed to account for the \nobserved nesting structure.")
  } else {
    cat("The model that accounts for nesting (lmer model) does not fit the data significantly \nbetter than a model that does not account for nesting (lm model).", '\n')
    cat("This suggests that the fixed-effexts model fits the data just as well as \nthe random-effects model.")}
}




