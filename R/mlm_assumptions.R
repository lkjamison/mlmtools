#' Reports the output of testing all assumptions for a multilevel model
#'
#' Reports the results from testing all assumptions of a multilevel model and provides suggestions if an assumption is not passed
#'
#' @param model A linear mixed-effects model of class lmerMod, lmerModLmerTest, or glmerMod of type binomial.
#'
#' @return Tests the relevant assumptions of the specified multilevel model.
#'
#' @references Glaser, R. E. (2006). Levene’s Robust Test of Homogeneity of Variances. Encyclopedia of Statistical Sciences. 6.
#'
#' @examples
#' # Read in data
#' data(instruction)
#' # Create model
#' mod <- lme4::lmer(mathgain ~ mathkind + (1 | classid), data = instruction)
#' # Evaluate assumptions
#' mlm_assumptions(mod)
#'
#' @export mlm_assumptions

mlm_assumptions <- function(model) {

  # Model class must be 'lmerMod' or 'lmerModLmerTest'
  if (!(inherits(model,"lmerMod")|!inherits(model,"lmerModLmerTest")|!inherits(model,"glmerMod"))) {
    stop("Model class is not 'lmerMod', 'lmerModLmerTest', or 'glmerMod'.", call. = FALSE)
    return(NULL)
  }
  if(inherits(model,"glmerMod") & !stats::family(model)[[1]]=="binomial"){
    stop("Function currently only supports binomial family glmerMod models.", call. = FALSE)
    return(NULL)
  }

  # Data
  data <- lme4::getData(model)

  # Original y variable
  form <- deparse(stats::formula(model))
  y <- trimws(strsplit(form, "[~+]")[[1]][1]) # Extracts dependent variable from the model

  # Predictors
  x <- attributes(stats::terms(model))$term.labels # Extracts independent variables from the model

  # Model has to have predictors
  if(length(x) == 0){
    stop("mlm_assumptions requires 1 or more predictors to be present in the model.", call. = FALSE)
    return(NULL)
  }

  if(inherits(model,"glmerMod")){
    data$probabilities <- stats::predict(model, type = "response")
    data$logit <- log(data$probabilities/(1-data$probabilities))
  }

  # Multiply two continuous interactors
  # Plot continuous at every level of categorical interactor
  # Ignore two categorical variable interactions


  # Linearity
  linearityplot_fun <- function(xvar){
    if(inherits(model,"glmerMod")){
      yvar <- "logit"
    } else {
      yvar <- y
    }
    # If interaction
    if(grepl(":",xvar)){
      int <- xvar[grepl(":",xvar)] # Interaction name
      int.var <- strsplit(int,"[~:]")[[1]] # Split interaction(s) into variables
      n.int <- length(int.var)
      # If three way or more interaction
      if(n.int > 2){
        return(paste0("Linearity plot cannot be estimated for ", xvar, ". Function currently does not support interactions beyond 2 variables."))
      }
      # If two way interaction
      else {
        # For each column, check the class of the variable in data
        int.class <- sapply(int.var, function(x) class(data[,x]))
        # Check the combination of the variables
        if(all(int.class %in% c("numeric","integer"))){
          data$interaction <- data[,int.var[1]]*data[,int.var[2]]
          int <- gsub("\\:", ".", int)
          colnames(data)[which(colnames(data)=="interaction")] <- int
          ggplot2::ggplot(data, ggplot2::aes_string(x=int, y=yvar)) +
            ggplot2::geom_point()+
            ggplot2::geom_smooth(formula = y ~ x, method=stats::loess) +
            ggplot2::theme_classic()
        }
        ### Both are character factor or logical with numeric or integer
        else {
          if(all(int.class %in% c("character", "factor","logical"))){
            return(paste0("Linearity plot cannot be estimated for ", xvar, ". Function currently does not support interactions between only character, factor, or logical variables."))
          }
          ### One character, factor, or logical with numeric or integer
          else {
            data$facet <- data[,names(which(int.class != c("numeric","integer")))]
            ggplot2::ggplot(data, ggplot2::aes_string(x=names(which(int.class == c("numeric","integer"))), y=yvar)) +
              ggplot2::geom_point()+
              ggplot2::geom_smooth(formula = y ~ x, method=stats::loess) +
              ggplot2::theme_classic() +
              ggplot2::facet_wrap(~facet) +
              ggplot2::ggtitle(xvar)
          }
        }
      }
    }
    # If not an interaction
    else {
      ggplot2::ggplot(data, ggplot2::aes_string(x=xvar, y=yvar)) +
        ggplot2::geom_point()+
        ggplot2::geom_smooth(formula = y ~ x, method=stats::loess) +
        ggplot2::theme_classic() +
        ggplot2::ggtitle(xvar)
    }
  }

  linearity.plots <- lapply(x, linearityplot_fun)

  if(!inherits(model,"glmerMod")){
    # Homogeneity of Variance
    data$model.Res2<- abs(stats::residuals(model))^2 # squares the absolute values of the residuals to provide the more robust estimate
    Levene.model <- stats::lm(model.Res2 ~ classid, data=data) #ANOVA of the squared residuals
    homo.test <- stats::anova(Levene.model) #displays the results
    predicted <- stats::predict(model)
    data$predicted <- predicted
    #create a fitted vs residual plot
    fitted.residual.plot <- ggplot2::ggplot(data=data,mapping=ggplot2::aes(x=predicted,y=stats::residuals(model))) +
      ggplot2::geom_point() +
      ggplot2::geom_hline(yintercept=0,linetype="dashed") +
      ggplot2::theme_classic() +
      ggplot2::xlab("Predicted Values") +
      ggplot2::ylab("Model Residuals") +
      ggplot2::ggtitle("Fitted vs. Residuals")

    # Normally distributed residuals
    V1 <- stats::resid(model)
    V2 <- data[,y]
    resid.data <- as.data.frame(cbind(V1,V2))
    resid.linearity.plot <- ggplot2::ggplot(resid.data,
                                            ggplot2::aes(x = V1, y = V2)) +
      ggplot2::geom_point() +
      ggplot2::geom_smooth(formula = y ~ x, method=stats::loess) +
      ggplot2::theme_classic() +
      ggplot2::xlab("Residuals") +
      ggplot2::ylab("Original y")
  }
  if(inherits(model,"glmerMod")){
    data$predicted <- stats::predict(model)
  }
  data$Leverage = data$predicted/(1 - data$predicted)
  data$mse = (stats::residuals(model))^2/stats::var(stats::residuals(model))
  data$CooksD <- (1/6)*(data$mse)*data$Leverage
  outliers <- rownames(data[data$CooksD > (4/nrow(data)),])
  if(length(outliers) == 0){
    outliers <- "No outliers detected."
  }
  model.Res <- stats::residuals(model)
  data$model.Res <- model.Res
  if(!inherits(model,"glmerMod")){
    resid.normality.plot <- ggplot2::qplot(sample = model.Res, data = data) +
      ggplot2::stat_qq_line() +
      ggplot2::xlab("Theoretical Quantiles") +
      ggplot2::ylab("Standardized Residuals") +
      ggplot2::ggtitle("Normal Q-Q") +
      ggplot2::theme_classic()
  }

  # Component + Residual plots
  ### continuous predictors only - Will not produce a plot for logical, unordered factor, character, < 3 unique values in predictors, interactions between categorical variables, or interactions of > 3 variables
  if(!inherits(model,"glmerMod")){
    x.ResidComponent <- c(x[!grepl(":",x)][sapply(x[!grepl(":",x)], function(x) ifelse(!is.factor(data[,x]), TRUE, is.ordered(data[,x])) & !is.character(data[,x]) & length(unique((data[,x])))>2)],x[grepl(":",x)])
    #data$model.Res <- residuals(model)
    ResidComponent_fun <- function(xvar){
      # If interaction
      if(grepl(":",xvar)){
        int <- xvar[grepl(":",xvar)] # Interaction name
        int.var <- strsplit(int,"[~:]")[[1]] # Split interaction(s) into variables
        n.int <- length(int.var)
        # If three way or more interaction
        if(n.int > 2){
          return(paste0("Residual Component plot cannot be estimated for ", xvar, ". Function currently does not support interactions beyond 2 variables."))
        }
        # If two way interaction
        else {
          # For each column, check the class of the variable in data
          int.class <- sapply(int.var, function(x) class(data[,x]))
          # Check the combination of the variables
          if(all(int.class %in% c("numeric","integer"))){
            data$interaction <- data[,int.var[1]]*data[,int.var[2]]
            int <- gsub("\\:", ".", int)
            colnames(data)[which(colnames(data)=="interaction")] <- int
            ggplot2::ggplot(data, ggplot2::aes_string(x=int, y=data[,"model.Res"])) +
              ggplot2::geom_point() +
              ggplot2::geom_smooth() +
              ggplot2::geom_hline(yintercept = 0) +
              ggplot2::xlab(xvar) +
              ggplot2::ylab("Residuals") +
              ggplot2::theme_classic() +
              ggplot2::ggtitle(xvar)
          }
          ### Both are character factor or logical with numeric or integer
          else {
            if(all(int.class %in% c("character", "factor","logical"))){
              return(paste0("Residual Component plot cannot be estimated for ", xvar, ". Function currently does not support interactions between only character, factor, or logical variables."))
            }
            ### One character, factor, or logical with numeric or integer
            else {
              data$facet <- data[,names(which(int.class != c("numeric","integer")))]
              ggplot2::ggplot(data, ggplot2::aes_string(x=names(which(int.class == c("numeric","integer"))), y=data[,"model.Res"])) +
                ggplot2::geom_point() +
                ggplot2::geom_smooth() +
                ggplot2::facet_wrap(~facet) +
                ggplot2::geom_hline(yintercept = 0) +
                ggplot2::xlab(xvar) +
                ggplot2::ylab("Residuals") +
                ggplot2::theme_classic() +
                ggplot2::ggtitle(xvar)
            }
          }
        }
      }
      # If not an interaction
      else {
        ggplot2::ggplot(data, ggplot2::aes_string(x=data[,xvar], y=data[,"model.Res"])) +
          ggplot2::geom_point() +
          ggplot2::geom_smooth() +
          ggplot2::geom_hline(yintercept = 0) +
          ggplot2::xlab(xvar) +
          ggplot2::ylab("Residuals") +
          ggplot2::theme_classic()
      }
    }
    resid.component.plots <- lapply(x.ResidComponent, ResidComponent_fun)
  }

  # Multicollinearity
  x_character <- which(sapply(data[,x],class) == "character")
  if(length(x_character > 0)) {
    x_vif <- x[-which(sapply(data[,x],class) == "character")]
  } else {
    x_vif <- x
  }
  if (length(x_vif) < 2) {
    multicollinearity <- ("Model contains fewer than 2 terms, multicollinearity cannot be assessed.\n")
  } else {
    v <- as.matrix(stats::vcov(model))
    assign <- attr(stats::model.matrix(model),"assign")[-which(attr(stats::model.matrix(model), "assign")==which(sapply(data[,x],class) == "character"))]
    if (names(lme4::fixef(model)[1]) == "(Intercept)") {
      v <- v[-1, -1]
      assign <- assign[-1]
    }
    R <- stats::cov2cor(v)
    detR <- det(R)
    multicollinearity <- matrix(0, length(x_vif), 3)
    rownames(multicollinearity) <- x_vif
    colnames(multicollinearity) <- c("GVIF", "Df", "GVIF^(1/(2*Df))")
    for (term in 1:length(x_vif)) {
      subs <- which(assign == x_vif)
      multicollinearity[term, 1] <- det(as.matrix(R[subs, subs])) *
        det(as.matrix(R[-subs, -subs])) / detR
      multicollinearity[x_vif, 2] <- length(subs)
    }
    if (all(multicollinearity[, 2] == 1)){
      multicollinearity <- multicollinearity[, 1]
    } else {
      multicollinearity[, 3] <- multicollinearity[, 1]^(1/(2 * multicollinearity[, 2]))
    }
    multicollinearity <- multicollinearity[,1]
  }

  # Combining Results
  result <- if(inherits(model,"glmerMod")){
    list(linearity.plots,outliers,multicollinearity)
    } else {
      list(linearity.plots,homo.test,fitted.residual.plot,outliers,resid.normality.plot,resid.component.plots,multicollinearity)
    }
  names(result) <- if(inherits(model,"glmerMod")){
    c("linearity.plots","outliers","multicollinearity")
  } else {
    c("linearity.plots","homo.test","fitted.residual.plot","outliers","resid.normality.plot","resid.component.plots","multicollinearity")
  }

  # Adding messages for summary/interpretation
  if(!inherits(model,"glmerMod")){
    message(if(result$homo.test$`Pr(>F)`[1] >= .05){
      c("Homogeneity of variance assumption met.\n")
    } else {
      c("Homogeneity of variance assumption NOT met. See: TO DO ADD RESOURCES\n")
    })
    message(if(is.character(result$multicollinearity)){
      c(result$multicollinearity)
    } else {
      if(any(result$multicollinearity > 5)){
        c("Multicollinearity detected - VIF value above 5. This might be problematic for the model - consider removing the variable from the model.\n Check the multicollinearity object for more details.\n")
      } else {
        c("No multicollinearity detected in the model.\n")
      }
    })
    message(if(outliers != "No outliers detected."){
      c("Outliers detected. See outliers object for more information.\n")
    } else {
      c("No outliers detected.\n")
    })
    message(c("Visually inspect all 4 plot types by calling them from the object created by mlm_assumptions() such as object$fitted.residual.plot and object$resid.normality.plot. linearity.plots and resid.component.plots may contain more than one plot depending on the model specified. Check how many there are, for example using length(object$linearity.plots). Then inspect each plot within each object, for example using object$linearity.plots[[1]] to access the first plot within the linearity.plots list.\n"))
    message(c("See ?mlm_assumptions for more details and resources."))
  } else {
    message(if(is.character(result$multicollinearity)){
      c(result$multicollinearity)
    } else {
      if(any(result$multicollinearity > 5)){
        c("Multicollinearity detected - VIF value above 5. This might be problematic for the model - consider removing the variable from the model.\n Check the multicollinearity object for more details.\n")
      } else {
        c("No multicollinearity detected in the model.\n")
      }
    })
    message(if(result$outliers!="No outliers detected."){
      c("Outliers detected. See outliers object for more information.\n")
    } else {
      c("No outliers detected.\n")
    })
    message(c("Visually inspect the linearity.plots object by calling it from the object created by mlm_assumptions() such as object$linearity.plots. linearity.plots may contain more than one plot depending on the model specified. Check how many there are, for example using length(object$linearity.plots). Then inspect each plot within the object, for example using object$linearity.plots[[1]] to access the first plot within the linearity.plots list.\n"))
    message(c("See ?mlm_assumptions for more details and resources."))
  }

  # Assigning object class
  class(result) <- "mlm_assumptions"

  # Returning object
  suppressMessages(return(result))

}

