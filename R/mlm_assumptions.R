#' Reports the output of testing all assumptions for a multilevel model
#'
#' Reports the results from testing all assumptions of a multilevel model and provides suggestions if an assumption is not passed
#'
#' @param model A linear mixed-effects model of class lmerMod or lmerModLmerTest
#'
#' @return TO DO
#'
#' @references TO DO
#'
#' @export mlm_assumptions

mlm_assumptions <- function(model) {

  # TO DO: ACCOUNT FOR NA'S

  # Model class must be 'lmerMod' or 'lmerModLmerTest'
  if (!(class(model)=="lmerMod"|class(model)=="lmerModLmerTest")) {
    stop("Model class is not 'lmerMod' or 'lmerModLmerTest'.", call. = FALSE)
    return(NULL)
  }

  # Data
  data <- lme4::getData(model)

  # Original y variable
  form <- deparse(formula(model))
  y <- trimws(strsplit(form, "[~+]")[[1]][1]) # Extracts dependent variable from the model

  # Predictors
  x <- attributes(terms(model))$term.labels # Extracts independent variables from the model

  # Multiply two continuous interactors
  # Plot continuous at every level of categorical interactor
  # Ignore two categorical variable interactions

  # Linearity
  linearityplot_fun <- function(xvar){
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
          ggplot2::ggplot(data, ggplot2::aes_string(x=int, y=y)) +
            ggplot2::geom_point()+
            ggplot2::geom_smooth(formula = y ~ x, method=stats::loess) +
            ggplot2::theme_classic() +
            ggplot2::ggtitle(xvar)
        }
        ### Both are character factor or logical with numeric or integer
        else {
          if(all(int.class %in% c("character", "factor","logical"))){
            return(paste0("Linearity plot cannot be estimated for ", xvar, ". Function currently does not support interactions between only character, factor, or logical variables."))
          }
          ### One character, factor, or logical with numeric or integer
          else {
            data$facet <- data[,names(which(int.class != c("numeric","integer")))]
            ggplot2::ggplot(data, ggplot2::aes_string(x=names(which(int.class == c("numeric","integer"))), y=y)) +
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
      ggplot2::ggplot(data, ggplot2::aes_string(x=xvar, y=y)) +
        ggplot2::geom_point()+
        ggplot2::geom_smooth(formula = y ~ x, method=stats::loess) +
        ggplot2::theme_classic() +
        ggplot2::ggtitle(xvar)
    }
  }

  linearity.plots <- lapply(x, linearityplot_fun)

  # Homogeneity of Variance
  data$model.Res2<- abs(residuals(model))^2 # squares the absolute values of the residuals to provide the more robust estimate
  Levene.model <- lm(model.Res2 ~ classid, data=data) #ANOVA of the squared residuals
  homo.test <- anova(Levene.model) #displays the results

  data$predicted <- predict(model)
  #create a fitted vs residual plot
  fitted.residual.plot <- ggplot2::ggplot(data=data,mapping=ggplot2::aes(x=predicted,y=residuals(model))) +
    ggplot2::geom_point() +
    ggplot2::geom_hline(yintercept=0,linetype="dashed") +
    ggplot2::theme_classic() +
    ggplot2::xlab("Predicted Values") +
    ggplot2::ylab("Model Residuals") +
    ggplot2::ggtitle("Fitted vs. Residuals")

  # Normally distributed residuals

  resid.linearity.plot <- ggplot2::ggplot(as.data.frame(cbind(resid(model),data[,y])),
                                         ggplot2::aes(x = V1, y = V2)) +
    ggplot2::geom_point() +
    ggplot2::geom_smooth(formula = y ~ x, method=stats::loess) +
    ggplot2::theme_classic() +
    ggplot2::xlab("Residuals") +
    ggplot2::ylab("Original y")

  data$Leverage = data$predicted/(1 - data$predicted)
  data$mse = (residuals(model))^2/var(residuals(model))
  data$CooksD <- (1/6)*(data$mse)*data$Leverage
  outliers <- rownames(data[data$CooksD > (4/nrow(data)),])
  y.resid <- as.vector(quantile(residuals(model,scaled = TRUE), c(0.25, 0.75), names = FALSE, type = 7, na.rm = TRUE))
  x.resid <- qnorm(c(0.25, 0.75))
  slope <- diff(y.resid)/diff(x.resid)
  int <- y.resid[[1L]] - slope * x.resid[[1L]]
  resid.normality.plot <- ggplot2::ggplot(data, ggplot2::aes(qqnorm(residuals(model,scaled = TRUE))[[1]], residuals(model, scaled = TRUE))) +
    ggplot2::geom_point(na.rm = TRUE) +
    ggplot2::geom_abline(slope = slope, intercept = int) +
    ggplot2::xlab("Theoretical Quantiles") +
    ggplot2::ylab("Standardized Residuals") +
    ggplot2::ggtitle("Normal Q-Q") +
    ggplot2::theme_classic()

  # Component + Residual plots
  ### continuous predictors only - Will not produce a plot for logical, unordered factor, character, < 3 unique values in predictors, or an interaction

  x.ResidComponent <- x[!grepl(":",x)][sapply(x[!grepl(":",x)], function(x) ifelse(!is.factor(data[,x]), TRUE, is.ordered(data[,x])) & !is.character(data[,x]) & length(unique((data[,x])))>2)]

  data$model.Res <- residuals(model)
  ResidComponent_fun <- function(xvar){
    ggplot2::ggplot(data, ggplot2::aes_string(x=data[,xvar], y=data[,"model.Res"])) +
      ggplot2::geom_point() +
      ggplot2::geom_smooth() +
      ggplot2::geom_hline(yintercept = 0) +
      ggplot2::xlab(xvar) +
      ggplot2::ylab("Residuals") +
      ggplot2::theme_classic()
  }
  resid.component.plots <- lapply(x.ResidComponent, ResidComponent_fun)

  # Multicollinearity
  if (length(x) < 2) {
    multicollinearity <- ("Model contains fewer than 2 terms, multicollinearity cannot be assessed.")
  } else {
    v <- as.matrix(vcov(model))
    assign <- attr(model.matrix(model), "assign")
    if (names(fixef(model)[1]) == "(Intercept)") {
      v <- v[-1, -1]
      assign <- assign[-1]
    }
    R <- cov2cor(v)
    detR <- det(R)
    multicollinearity <- matrix(0, length(x), 3)
    rownames(multicollinearity) <- x
    colnames(multicollinearity) <- c("GVIF", "Df", "GVIF^(1/(2*Df))")
    for (term in 1:length(x)) {
      subs <- which(assign == x)
      multicollinearity[term, 1] <- det(as.matrix(R[subs, subs])) *
        det(as.matrix(R[-subs, -subs])) / detR
      multicollinearity[x, 2] <- length(subs)
    }
    if (all(multicollinearity[, 2] == 1)){
      multicollinearity <- multicollinearity[, 1]
    } else {
      multicollinearity[, 3] <- multicollinearity[, 1]^(1/(2 * multicollinearity[, 2]))
    }
    multicollinearity <- multicollinearity[,1]
  }

  result <- list(linearity.plots,homo.test,fitted.residual.plot,outliers,resid.normality.plot,resid.component.plots,multicollinearity)
  names(result) <- c("linearity.plots","homo.test","fitted.residual.plot","outliers","resid.normality.plot","resid.component.plots","multicollinearity")
  suppressMessages(return(result))

}

