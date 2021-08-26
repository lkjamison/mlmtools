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
#' @examples
#'
#' \donttest{
#' mod <- lmer(mathgain ~ classid_mathkind.cmn + classid_mathkind.devcmn + (1 | classid), data = instruction)
#' rsq.mlm(mod)
#'}
#'
#' @references Nakagawa, S., Johnson, P. C., & Schielzeth, H. (2017). The coefficient of determination R 2 and intra-class correlation coefficient from generalized linear mixed-effects models revisited and expanded. Journal of the Royal Society Interface, 14(134), 20170213.
#' @export

rsq.mlm <- function(model, by_cluster = FALSE){

  mods <- list(model)

  # Only one model at a time
  if (length(mods)>1) {
    stop("Only one model can be assessed at one time.", call. = FALSE)
    return(NULL)
  }

  # Not an lmer object
  if (!(class(model)=="lmerMod"|class(model)=="lmerModLmerTest")) {
    stop("Model class is not 'lmerMod' or 'lmerModLmerTest'.", call. = FALSE)
    return(NULL)
  }

  # Misspecification of by_cluster argument
  if (length(by_cluster)!=1){
    stop("The argument by_cluster is not specified correctly.", call. = FALSE)
    return(NULL)
  }

  if (!(by_cluster%in%c(TRUE, FALSE))){
    stop("The argument by_cluster is not specified correctly.", call. = FALSE)
    return(NULL)
  }

  # Model contains random slopes
  if (lme4::getME(model, name = 'n_rtrms') == 2 & lme4::getME(model, name = 'cnms')[1] != "(Intercept)" & lme4::getME(model, name = 'cnms')[2] != "(Intercept)") {
    stop("rsq.mlm cannot be calculated for models containing random slopes.", call. = FALSE)
    return(NULL)
  }

  # Model must contain at least one fixef
  if (length(names(lme4::getME(model, name = 'fixef'))) == 0) {
    stop("Model must contain at least one fixed effect.", call. = FALSE)
    return(NULL)
  }

  # Offset
  calls <- lapply(mods, stats::getCall)
  off <- lapply(calls, `[[`, "offset")[[1]]
  if(!is.null(off)) {
    stop("levelCompare cannot be calculated for models using the offset argument", call. = FALSE)
    return(NULL)
  }

  if (by_cluster == FALSE) {
    vars <- tryCatch(lme4::VarCorr(model))
    if (exists("vars")==FALSE) {
      stop("Error in extracting estimated variances.", call. = FALSE)
      return(NULL)
    }
    if (anyNA(vars, recursive = TRUE)) {
      stop("Error in estimating variances.", call. = FALSE)
      return(NULL)
    }

    vars <- as.data.frame(vars)

    # Variance of fixed effects - done
    fe <- lme4::fixef(model)
    ok <- !is.na(fe)
    fitted <- (stats::model.matrix(model)[, ok, drop = FALSE] %*% fe[ok])[, 1L]
    varFE <- stats::var(fitted)

    # Variance of the random effects - done
    matmultdiag <-
      function(x, y, ty = t(y)) {
        if(ncol(x) != ncol(ty)) stop('non-conformable arguments')
        if(nrow(x) != nrow(ty)) stop('result is not a square matrix')
        return(rowSums(x * ty))
      }
    vc <- lme4::VarCorr(model)
    remodmat <- function(object) {
      rval <- do.call("cbind", stats::model.matrix(object, type = "randomListRaw"))
      rval[, !duplicated(colnames(rval)), drop = FALSE]
    }
    mmRE <- suppressWarnings(remodmat(model))
    varRESum <- function(vc, X) {
      n <- nrow(X)
      sum(sapply(vc, function(sig) {
        mm1 <-  X[, rownames(sig), drop = FALSE]
        sum(matmultdiag(mm1 %*% sig, ty = mm1)) / n
      }))
    }
    varRE <- varRESum(vc, mmRE)

    # Variance of Residuals - done
    vc <- as.data.frame(vc)
    varE <- vc[vc$grp == "Residual",]$vcov

    # Computing R2
    r2_marginal <- varFE/(varFE + varRE + varE)
    r2_conditional <- (varFE + varRE)/(varFE + varRE + varE)

    # Output
    res <- (list(
      "marginal" = r2_marginal*100,
      "conditional" = r2_conditional*100,
      "byCluster" = by_cluster))
    class(res) <- "mlmtools_rsq.mlm"
    return(res)
  }

  if (by_cluster == TRUE) {
    #Weights
    w <- lapply(calls, `[[`, "weights")[[1]]
    #Subsets
    subsets <- lapply(calls, `[[`, "subset")[[1]]
    #Data
    d <- lapply(calls, `[[`, "data")[[1]]
    # Null model
    forms <- lapply(lapply(calls, `[[`, "formula"), deparse)
    ff1 <- forms[[1]]
    ff2 <- strsplit(ff1, "[~+]")[[1]]
    ff2 <- ff2[grepl("|", ff2, fixed = TRUE)]
    ff2 <- paste(ff2, collapse = "+")
    ff3 <- (paste(sub('(?<=\\~).*$', '', ff1, perl=TRUE), "1"))
    ff3.5 <- paste(ff3,ff2,sep="+")
    ff4 <- ifelse(is.null(subsets) & is.null(w), paste("lmer(",ff3.5, ", data = ", deparse(d), ")"),
                  ifelse(!is.null(subsets) & is.null(w), paste("lmer(", ff3.5, ", data = ", deparse(d), ", subset = ", deparse(subsets), ")"),
                         ifelse(is.null(subsets) & !is.null(w), paste("lmer(", ff3.5, ", data = ", deparse(d), ", weights = ", w, ")"),
                                paste("lmer(", ff3.5, ", data = ", deparse(d),", subset = ", deparse(subsets), ", weights = ", w, ")"))))

    nullModel <- eval(parse(text = ff4), parent.frame())
    vars_null <- tryCatch(lme4::VarCorr(nullModel))
    vars_null <- as.data.frame(vars_null)
    group_names <- names(lme4::VarCorr(model))
    vc <- as.data.frame(lme4::VarCorr(model))
    varINT <- vc[which(vc$var1 == "(Intercept)"),]$vcov # var intercept original model
    var.nullINT <- vars_null[which(vars_null$var1 == "(Intercept)"),]$vcov # var intercept null model
    varE <- vc[vc$grp == "Residual",]$vcov # var residual original model
    var.nullE <- vars_null[vars_null$grp == "Residual",]$vcov # var residual null model
    r2_random <- 1 - (varINT/var.nullINT)
    r2_fixed <- 1 - (varE/var.nullE)
    out <- data.frame(Level = c("Level 1", group_names),
                      R2 = c(r2_fixed, r2_random), stringsAsFactors = FALSE)

    # Output
    res <- (list(
      "fixed" = r2_fixed*100,
      "random" = r2_random*100,
      "byCluster" = by_cluster,
      "Level" = out$Level))
    class(res) <- "mlmtools_rsq.mlm"
    return(res)
  }
}


print.mlmtools_rsq.mlm <- function(x){
  if (x$byCluster == FALSE){
    cat(format(round(x$marginal, 2), nsmall = 2), "% of the total variance is explained by the fixed effects.", "\n", sep="")
    cat(format(round(x$conditional, 2), nsmall = 2),"% of the total variance is explained by both fixed and random effects.", sep="")
  } else {
    cat(format(round(x$fixed[1], 2), nsmall = 2), "% of the variance is explained by the fixed effects at Level 1", "\n", sep="")
    for(i in 1:length(x$random)){
      cat(format(round(x$random[i], 2), nsmall = 2), "% of the variance is explained at the ",strsplit(x$Level[i + 1], ":")[[1]][1], " level", "\n", sep="")
    }}
}









