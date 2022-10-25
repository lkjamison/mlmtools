#' Calculates R-squared from lmer models
#'
#' Calculates variance explained by lme4-fitted mixed-effects models.
#'
#' @param model A linear mixed-effects model of class lmerMod or lmerModLmerTest
#'
#' @param by_cluster Logical, if TRUE returns variance explained at each level
#'
#' @return Computes the percent variance explained by the model.
#'
#' @references Nakagawa, S., Johnson, P. C., & Schielzeth, H. (2017). The coefficient of determination R 2 and intra-class correlation coefficient from generalized linear mixed-effects models revisited and expanded. Journal of the Royal Society Interface, 14(134), 20170213.
#'
#' @examples
#' # Read in data
#' data(instruction)
#' # Center mathkind by classid
#' center(dataset = instruction, x = "mathkind", grouping = "classid")
#' # Create model
#' mod <- lme4::lmer(mathgain ~ classid_mathkind.cmn +
#' classid_mathkind.devcmn + (1 | classid), data = instruction)
#' # Calculate r-squared
#' rsq <- rsqmlm(mod)
#' rsq
#' rsq$marginal
#' rsq$conditional
#'
#' @export rsqmlm

rsqmlm <- function (model, by_cluster = FALSE)
{
  mods <- list(model)
  if (length(mods) > 1) {
    stop("Only one model can be assessed at one time.", call. = FALSE)
    return(NULL)
  }
  if (!(inherits(model,"lmerMod") | inherits(model,"lmerModLmerTest"))) {
    stop("Model class is not 'lmerMod' or 'lmerModLmerTest'.",
         call. = FALSE)
    return(NULL)
  }
  if (length(by_cluster) != 1) {
    stop("The argument by_cluster is not specified correctly.",
         call. = FALSE)
    return(NULL)
  }
  if (!(by_cluster %in% c(TRUE, FALSE))) {
    stop("The argument by_cluster is not specified correctly.",
         call. = FALSE)
    return(NULL)
  }
  if (lme4::getME(model, name = "n_rtrms") == 2 & lme4::getME(model,
                                                              name = "cnms")[1] != "(Intercept)" & lme4::getME(model,
                                                                                                               name = "cnms")[2] != "(Intercept)") {
    stop("rsqmlm cannot be calculated for models containing random slopes.",
         call. = FALSE)
    return(NULL)
  }
  if (length(names(lme4::getME(model, name = "fixef"))) ==
      0) {
    stop("Model must contain at least one fixed effect.",
         call. = FALSE)
    return(NULL)
  }
  calls <- lapply(mods, stats::getCall)
  off <- lapply(calls, `[[`, "offset")[[1]]
  if (!is.null(off)) {
    stop("levelCompare cannot be calculated for models using the offset argument",
         call. = FALSE)
    return(NULL)
  }
  if (by_cluster == FALSE) {
    vars <- tryCatch(lme4::VarCorr(model))
    if (exists("vars") == FALSE) {
      stop("Error in extracting estimated variances.",
           call. = FALSE)
      return(NULL)
    }
    if (anyNA(vars, recursive = TRUE)) {
      stop("Error in estimating variances.", call. = FALSE)
      return(NULL)
    }
    vars <- as.data.frame(vars)
    fe <- lme4::fixef(model)
    ok <- !is.na(fe)
    fitted <- (stats::model.matrix(model)[, ok, drop = FALSE] %*%
                 fe[ok])[, 1L]
    varFE <- stats::var(fitted)
    matmultdiag <- function(x, y, ty = t(y)) {
      if (ncol(x) != ncol(ty))
        stop("non-conformable arguments")
      if (nrow(x) != nrow(ty))
        stop("result is not a square matrix")
      return(rowSums(x * ty))
    }
    vc <- lme4::VarCorr(model)
    remodmat <- function(object) {
      rval <- do.call("cbind", stats::model.matrix(object,
                                                   type = "randomListRaw"))
      rval[, !duplicated(colnames(rval)), drop = FALSE]
    }
    mmRE <- suppressWarnings(remodmat(model))
    varRESum <- function(vc, X) {
      n <- nrow(X)
      sum(sapply(vc, function(sig) {
        mm1 <- X[, rownames(sig), drop = FALSE]
        sum(matmultdiag(mm1 %*% sig, ty = mm1))/n
      }))
    }
    varRE <- varRESum(vc, mmRE)
    vc <- as.data.frame(vc)
    varE <- vc[vc$grp == "Residual", ]$vcov
    r2_marginal <- varFE/(varFE + varRE + varE)
    r2_conditional <- (varFE + varRE)/(varFE + varRE + varE)
    res <- (list(marginal = r2_marginal * 100, conditional = r2_conditional *
                   100, byCluster = by_cluster))
    class(res) <- "rsqmlm"
    return(res)
  }
  if (by_cluster == TRUE) {
    w <- lapply(calls, `[[`, "weights")[[1]]
    subsets <- lapply(calls, `[[`, "subset")[[1]]
    d <- lapply(calls, `[[`, "data")[[1]]
    forms <- stats::as.formula(paste(lapply(lapply(calls, `[[`, "formula"), deparse)[[1]], collapse = " "))
    y_term <- deparse(forms[[2L]])
    rand_terms <- paste0("(", sapply(lme4::findbars(forms), deparse), ")")
    nullform <- stats::reformulate(rand_terms, response = y_term)
    nullmodel <- tryCatch(
      {
        stats::update(model, nullform)
      },
      error = function(e) {
        msg <- e$message
          if (grepl("(^object)(.*)(not found$)", msg)) {
            print("Unable to estimate the null model, data may not be accessible in the Global Environment.\n")
          } else if (grepl("^could not find function", msg)) {
            print("Unable to estimate the null model, please load the package used to estimate the original model.\n")
          }
      }
    )
    vars_null <- tryCatch(lme4::VarCorr(nullmodel))
    vars_null <- as.data.frame(vars_null)
    group_names <- names(lme4::VarCorr(model))
    vc <- as.data.frame(lme4::VarCorr(model))
    varINT <- vc[which(vc$var1 == "(Intercept)"), ]$vcov
    var.nullINT <- vars_null[which(vars_null$var1 == "(Intercept)"),
    ]$vcov
    varE <- vc[vc$grp == "Residual", ]$vcov
    var.nullE <- vars_null[vars_null$grp == "Residual", ]$vcov
    r2_random <- 1 - (varINT/var.nullINT)
    r2_fixed <- 1 - (varE/var.nullE)
    out <- data.frame(Level = c("Level 1", group_names),
                      R2 = c(r2_fixed, r2_random), stringsAsFactors = FALSE)
    res <- (list(fixed = r2_fixed * 100, random = r2_random *
                   100, byCluster = by_cluster, Level = out$Level))
    class(res) <- "rsqmlm"
    return(res)
  }
}











