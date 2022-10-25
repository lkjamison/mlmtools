#' Compares variance explained for two mixed effects models
#'
#' Compares variance explained by additional fixed effects for two lme4-fitted mixed-effects models.
#'
#' @param model1 A linear mixed-effects model of class lmerMod or lmerModLmerTest
#' @param model2 A linear mixed-effects model of class lmerMod or lmerModLmerTest
#'
#' @return Computes the percent increase in variance explained by the less parsimonious (more complicated) model compared to the more parsimonious (less complicated) model.
#'
#' @details Specifically,  1-(total variance for less parsimonious model/total variance for more parsimonious model).
#'
#' @examples
#' # Read in data
#' data(instruction)
#' # Create null model
#' mod0 <- lme4::lmer(mathgain ~ (1 | classid), data = instruction)
#' # Create model of interest
#' mod1 <- lme4::lmer(mathgain ~ mathkind + (1 | classid), data = instruction)
#' # Compare variance explained
#' ### To repress output: use invisible()
#' varCompare(mod0, mod1)
#'
#' @references Snijders, T. A. B. & Bosker, R. J. (2012). Multilevel Analysis (2nd Ed.). Sage Publications Ltd.
#'
#' @export varCompare


varCompare <- function(model1, model2) {

  mods <- list(model1, model2)

  # Model class must be 'lmerMod' or 'lmerModLmerTest'
  classCheck <- vapply(mods, function(x) class(x) == "lmerMod" || class(x) == "lmerModLmerTest", NA)
  if (!all(classCheck)) {
    stop("Model class is not 'lmerMod' or 'lmerModLmerTest'.", call. = FALSE)
    return(NULL)
  }

  # Only two model may be compared
  if (length(mods)>2) {
    stop("Only two models can be compared at one time.", call. = FALSE)
    return(NULL)
  }

  # Models must be nested
  if ((length(names(lme4::getME(model1, name = 'fixef'))) == length(names(lme4::getME(model2, name = 'fixef')))) & ((lme4::getME(model1, name = 'n_rtrms')) == (lme4::getME(model2, name = 'n_rtrms')))){
    stop("Models must be nested.", call. = FALSE)
    return(NULL)
  }

  # Models must be nested II
  calls <- lapply(mods, stats::getCall)
  data <- lapply(calls, `[[`, "data")
  if (!all(vapply(data, identical, NA, data[[1]]))){
    stop("All models must be fit to the same data object.", call. = FALSE)
    return(NULL)
  }
  subset <- lapply(calls, `[[`, "subset")
  if (!all(vapply(subset, identical, NA, subset[[1]]))){
    stop("All models must use the same subset.", call. = FALSE)
    return(NULL)
  }

  # Models must have same observations
  nobs.vec <- vapply(mods, stats::nobs, 1L)
  if (stats::var(nobs.vec) > 0) {
    stop("Models were not all fitted to the same size of dataset.", call. = FALSE)
    return(NULL)
  }

  # Models cannot contain random slopes
  if (as.data.frame((lme4::getME(model1, name = 'cnms')))[1,1]!="(Intercept)") {
    stop("Variance comparison cannot be calculated for models containing random slopes.", call. = FALSE)
    return(NULL)
  }

  if (dim(as.data.frame((lme4::getME(model1, name = 'cnms'))))[1] != 1) {
    stop("Variance comparison cannot be calculated for models containing random slopes.", call. = FALSE)
    return(NULL)
  }
  if (as.data.frame((lme4::getME(model2, name = 'cnms')))[1,1]!="(Intercept)") {
    stop("Variance comparison cannot be calculated for models containing random slopes.", call. = FALSE)
    return(NULL)
  }

  if (dim(as.data.frame((lme4::getME(model2, name = 'cnms'))))[1] != 1) {
    stop("Variance comparison cannot be calculated for models containing random slopes.", call. = FALSE)
    return(NULL)
  }

  # Models can only have two random intercepts
  if (lme4::getME(model1, name = 'n_rtrms') > 2 || lme4::getME(model2, name = 'n_rtrms') > 2) {
    stop("Variance comparison cannot be calculated for models containing more than two random effects.", call. = FALSE)
    return(NULL)
  }

  # Models must have the same random intercept
  if ((sum(names(lme4::VarCorr(model1))==names(lme4::VarCorr(model2))))!=lme4::getME(model1, name = 'n_rtrms')) {
    stop("Models must have the same random intercepts.", call. = FALSE)
    return(NULL)
  }





  # Models must be nested III
  if (all((names(lme4::getME(model1, name = 'fixef'))) %in% (names(lme4::getME(model2, name = 'fixef')))) == FALSE &
      all((names(lme4::getME(model2, name = 'fixef'))) %in% (names(lme4::getME(model1, name = 'fixef')))) == FALSE){
    stop("Models must be nested.", call. = FALSE)
    return(NULL)
  }

	# Two-level model

	if(lme4::getME(model1, name = 'n_rtrms') == 1){
	  var1 <- tryCatch(lme4::VarCorr(model1))
	  if (exists("var1")==FALSE) {
	    stop("Error in extracting estimated variances for model1.", call. = FALSE)
	    return(NULL)
	  }
	  mvcovmodel1 <- as.data.frame(var1)
	  if (is.na(mvcovmodel1[1,'vcov']) | is.na(mvcovmodel1[2,'vcov'])) {
	    stop("Error in estimating variances for model1.", call. = FALSE)
	    return(NULL)
	  }
	  var2 <- tryCatch(lme4::VarCorr(model2))
	  if (exists("var2")==FALSE) {
	    stop("Error in extracting estimated variances for model2.", call. = FALSE)
	    return(NULL)
	  }
	  mvcovmodel2 <- as.data.frame(var2)
	  if (is.na(mvcovmodel2[1,'vcov']) | is.na(mvcovmodel2[2,'vcov'])) {
	    stop("Error in estimating variances for model2.", call. = FALSE)
	    return(NULL)
	  }
	  if ((mvcovmodel1[1,'vcov']+mvcovmodel1[2,'vcov'])==0) {
	    stop("Zero variance in random-effects terms for model1.", call. = FALSE)
	    return(NULL)
	  }
	  r2m3_m2 <- 1-(mvcovmodel2[1,'vcov']+mvcovmodel2[2,'vcov'])/(mvcovmodel1[1,'vcov']+mvcovmodel1[2,'vcov'])
	}

	  ## Three-level model
	  if(lme4::getME(model1, name = 'n_rtrms') == 2){
	    var1 <- tryCatch(lme4::VarCorr(model1))
	    if (exists("var1")==FALSE) {
	      stop("Error in extracting estimated variances for model1.", call. = FALSE)
	      return(NULL)
	    }
	    mvcovmodel1 <- as.data.frame(var1)
	    if (is.na(mvcovmodel1[1,'vcov']) | is.na(mvcovmodel1[2,'vcov']) | is.na(mvcovmodel1[3,'vcov'])) {
	      stop("Error in estimating variances for model1.", call. = FALSE)
	      return(NULL)
	    }
	    var2 <- tryCatch(lme4::VarCorr(model2))
	    if (exists("var2")==FALSE) {
	      stop("Error in extracting estimated variances for model2.", call. = FALSE)
	      return(NULL)
	    }
	    mvcovmodel2 <- as.data.frame(var2)
	    if (is.na(mvcovmodel2[1,'vcov']) | is.na(mvcovmodel2[2,'vcov']) | is.na(mvcovmodel2[3,'vcov'])) {
	      stop("Error in estimating variances for model2.", call. = FALSE)
	      return(NULL)
	    }
	    mvcovmodel2 <- as.data.frame(var2)
	    if ((mvcovmodel1[1,'vcov']+mvcovmodel1[2,'vcov']+mvcovmodel1[3,'vcov'])==0) {
	      stop("Zero variance in random-effects terms for model1.", call. = FALSE)
	      return(NULL)
	    }
	  r2m3_m2 <- 1-(mvcovmodel2[1,'vcov']+mvcovmodel2[2,'vcov']+mvcovmodel2[3,'vcov'])/(mvcovmodel1[1,'vcov']+mvcovmodel1[2,'vcov']+mvcovmodel1[3,'vcov'])
	  }

	# Model
	fixEffmod1 <- sum(names(lme4::getME(model1, name = 'fixef'))!="(Intercept)")
	fixEffmod2 <- sum(names(lme4::getME(model2, name = 'fixef'))!="(Intercept)")
	if (fixEffmod1 > fixEffmod2){
	  varEx <- -1*(round((r2m3_m2*100), 2))
	} else {
	  varEx <- (round((r2m3_m2*100), 2))
	}

  # Output
	  res <- (list(
	    "model1" = deparse(substitute(model1)),
  	  "model2" = deparse(substitute(model2)),
	    "varEx" = varEx,
	    "fe1" = fixEffmod1,
	    "fe2" = fixEffmod2))
   		class(res) <- "varCompare"
   		return(res)
 	}








