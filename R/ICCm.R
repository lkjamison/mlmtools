#' Computes ICC values for mixed-effects models
#'
#' Computes ICC values for lme4-fitted mixed-effects models.
#'
#' @param model A linear mixed-effects model of class lmerMod or lmerModLmerTest
#' @param re_type A value indicating whether a model with two random effects is nested or cross-classified
#'
#' @return If \code{re_type} is "NA", the proportion of variance at the random effect is computed.
#' @return If re_type = "nested", the likeness of y scores in the same level 3 unit (the proportion of variance at Level3_factor), the likeness of y scores in the same level 2 units in the same level 3 unit (proportion of variance at Level3_factor and Level2_factor), and the likeness of level 2 units in the same level 3 unit (proportion of Level2_factor variance at Level3_factor) are computed.
#' @return If re_type = "cc", the likeness of y scores in the same C1_factor unit (correlation between outcome values of units in same C1_factor but different C2_factor), the likeness of y scores in the same C2_factor (correlation between outcome values of units in the same C2_factor but different C2_factor), and the likeness of y scores in the same C1_factor and C2_factor combination (correlation between outcome values of units in the same C2_factor and C2_factor) are computed.
#'
#' @examples
#' # Read in data
#' data(instruction)
#' # Create model
#' mod <- lme4::lmer(mathgain ~ (1 | classid), data = instruction)
#' # Estimate ICC
#' ICCm(mod)
#'
#' @references Snijders, T. A. B. & Bosker, R. J. (2012). Multilevel Analysis (2nd Ed.). Sage Publications Ltd.
#'
#' @export ICCm


ICCm <- function(model, re_type = c("NA")) {

  # Model class must be 'lmerMod' or 'lmerModLmerTest'
  if (!(inherits(model,"lmerMod")|!inherits(model,"lmerModLmerTest")|!inherits(model,"glmerMod"))) {
    stop("Model class is not 'lmerMod', 'lmerModLmerTest', or 'glmerMod'.", call. = FALSE)
    return(NULL)
  }
  if(inherits(model,"glmerMod") & !stats::family(model)[[1]]=="binomial"){
    stop("Function currently only supports binomial family glmerMod models.", call. = FALSE)
    return(NULL)
  }

    if (length(re_type)!=1){
    stop("The argument re_type is not specified correctly.", call. = FALSE)
    return(NULL)
  }

  if (is.character(re_type)==FALSE){
    stop("The argument re_type is not specified correctly.", call. = FALSE)
    return(NULL)
  }

  if (!(re_type%in%c("NA","nested","cc"))){
    stop("The argument re_type is not specified correctly.", call. = FALSE)
    return(NULL)
  }

  if (lme4::getME(model, name = 'n_rtrms') == 1 & re_type != "NA"){
    message("Warning: re_type argument set to 'NA' when the number of random intercepts equals 1.")
  }

  if (lme4::getME(model, name = 'n_rtrms') == 1 & lme4::getME(model, name = 'cnms')[1] != "(Intercept)") {
    stop("ICCs cannot be calculated for models containing random slopes.", call. = FALSE)
    return(NULL)
  }

  if (lme4::getME(model, name = 'n_rtrms') == 2 & lme4::getME(model, name = 'cnms')[1] != "(Intercept)" & lme4::getME(model, name = 'cnms')[2] != "(Intercept)") {
    stop("ICCs cannot be calculated for models containing random slopes.", call. = FALSE)
    return(NULL)
  }

  if (lme4::getME(model, name = 'n_rtrms') > 2) {
    stop("ICCs cannot be calculated for models containing more than two random effects.", call. = FALSE)
    return(NULL)
  }


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

  # Calculate ICC values
 	# one random effect
 	if(lme4::getME(model, name = 'n_rtrms') == 1){
 	 	icc <- vars$vcov[1]/(vars$vcov[1]+vars$vcov[2])
 	# two random effects - re_type not specified
 	} else if(lme4::getME(model, name = 'n_rtrms') == 2 & re_type == 'NA') {
 		stop("Please specify re_type = 'nested' or re_type = 'cc'.", call. = FALSE)
 	# two random effects - re_type = 'nested' - three level model
 	} else if(lme4::getME(model, name = 'n_rtrms') == 2 & re_type == 'nested') {
 		# We can calculate the ICC representing likeness of observations in the same level 3 unit:
		icc1 <- vars$vcov[2]/(vars$vcov[1]+vars$vcov[2]+vars$vcov[3])
		# Next is the ICC representing likeness of same level 2 units in the same level 3 unit:
		icc2 <- (vars$vcov[1]+vars$vcov[2])/(vars$vcov[1]+vars$vcov[2]+vars$vcov[3])
		# Finally is the ICC representing likeness of level 2 units in the same level 3 unit:
		icc3 <- 	vars$vcov[1]/(vars$vcov[1]+vars$vcov[2])
 	# two random effects - re_type = 'cc' - cross-classified model
 	} else if(lme4::getME(model, name = 'n_rtrms') == 2 & re_type == 'cc') {
 		# We can calculate the ICC representing likeness of attainment scores in the same sid unit:
 		icc1 <- vars$vcov[2]/(vars$vcov[1]+vars$vcov[2]+vars$vcov[3])
 		# Next is the ICC representing likeness of attainment scores in the same pid unit:
 		icc2 <- vars$vcov[1]/(vars$vcov[1]+vars$vcov[2]+vars$vcov[3])
 		# Finally is the ICC representing likeness of attainment scores in the same pid and sid unit combination:
 		icc3 <- (vars$vcov[1]+vars$vcov[2])/(vars$vcov[1]+vars$vcov[2]+vars$vcov[3])
 	}

  # Output
  	# one random effect
 	if(lme4::getME(model, name = 'n_rtrms') == 1){
 			res <- (list(
  	  	"RandEff" = 1,
  	  	"type" = NA,
  	  	"factor1" = names(lme4::VarCorr(model))[1],
  	  	"factor2" = NA,
  	  	"outcome" = names(stats::model.frame(model))[1],
      	"ICC" = round(icc, 3),
      	"ICC1" = NA,
      	"ICC2" = NA,
      	"ICC3" = NA))
   		class(res) <- "ICCm"
   		return(res)
 	# two random effects - three level model
 	} else if(lme4::getME(model, name = 'n_rtrms') == 2) {
 	 	res <- (list(
  	  	"RandEff" = 2,
  	  	"type" = re_type,
  	  	"factor1" = gsub("(:).*","",names(lme4::VarCorr(model)))[2],
  	  	"factor2" = if(sum(grepl("/", model@call, fixed = TRUE)) == 0) {
  	  	   gsub(".*(:)","",names(lme4::VarCorr(model)))[1] # If model is specified as (1|a)+(1|a:b)) or (1|a)+(1|b)
  	  	} else { gsub(").*","",gsub(".*(/)","",model@call)[2]) # If model is specified as (1|a/b)
  	  	},
  	  	"outcome" = names(stats::model.frame(model))[1],
      	"ICC" = NA,
      	"ICC1" = round(icc1,3),
      	"ICC2" = round(icc2,3),
      	"ICC3" = round(icc3,3)))
   		class(res) <- "ICCm"
   		return(res)
 	}

}


