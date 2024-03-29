#' Plots comparison of accounting for nesting vs. not accounting for nesting
#'
#' Plots the line of best for the relationship between two variables accounting for nesting and not accounting for nesting.
#'
#' @param model A linear mixed-effects model of class lmerMod or lmerModLmerTest.
#'
#' @param x Predictor variable.
#'
#' @param y Outcome variable.
#'
#' @param grouping Grouping variable.
#'
#' @param dataset A dataset containing the predictor, outcome, and grouping variables.
#'
#' @param paneled A logical value indicating whether the plot accounting for nesting should be split into panels.
#'
#' @param center A logical value indicating whether the x variable should be centered
#'
#' @param select A vector indicating the index of the groups to be included in the plots.
#'
#' @param xlab Character vector specifying the horizontal axis label.
#'
#' @param ylab Character vector specifying the vertical axis label.
#'
#' @param glab Character vector specifying the legend title for the plot accounting for nesting.
#'
#' @param plot_titles Character vectors specifying the titles for the plots.
#'
#' @examples
#' # Gaussian
#' ## Read in data
#' data(instruction)
#' ## Create model
#' mod <- lme4::lmer(mathgain ~ mathkind + (1 | classid), data = instruction)
#' ## Generate plots
#' levelComparePlot(mod, x = "mathkind", y = "mathgain", grouping = "classid", dataset = instruction)
#'
#' # Logistic
#' ## Read in data
#' data(reporting)
#' reporting$final.sample.size <- scale(as.numeric(reporting$final.sample.size))
#' reporting$mention.outliers <- ifelse(reporting$mention.outliers=="No",0,1)
#' mod <- lme4::glmer(mention.outliers ~ final.sample.size +
#' (1 | Journal), data = reporting, family = "binomial")
#' levelComparePlot(mod, x = "final.sample.size", y = "mention.outliers",
#' grouping = "Journal", dataset = reporting, paneled = FALSE)
#'
#'
#' @export levelComparePlot

levelComparePlot <- function(model, x, y, grouping, dataset, paneled = TRUE, select = c("select"), center = FALSE, xlab = x, ylab = y, glab = grouping, plot_titles = c("Scatter Plot", "Scatter Plot by Group")){

  mods <- list(model)

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

  # Error dataset must be a dataframe
  if (!is.data.frame(dataset)) {
    stop("Dataset provided is not of type data.frame.", call. = FALSE)
    return(NULL)
  }

  temp <- as.data.frame(dataset)

  xlab <- ifelse(xlab==x, x, xlab)
  ylab <- ifelse(ylab==y, y, ylab)
  glab <- ifelse(glab==grouping, grouping, glab)

  # x must be one of the variables in the dataset
  if(!(x%in%names(temp))){
    stop("The x variable specified is not a variable in the dataset provided.", call. = FALSE)
    return(NULL)
  }

  # y must be one of the variables in the dataset
  if(!(y%in%names(temp))){
    stop("The y variable specified is not a variable in the dataset provided.", call. = FALSE)
    return(NULL)
  }

  # grouping must be one of the variables in the dataset
  if(!(grouping%in%names(temp))){
    stop("The grouping variable specified is not a variable in the dataset provided.", call. = FALSE)
    return(NULL)
  }

  # xlab must be in quotes and of class character
  if(!inherits(xlab,"character")){
    stop("xlab argument must be character type.", call. = FALSE)
    return(NULL)
  }

  # ylab must be in quotes and of class character
  if(!inherits(ylab,"character")){
    stop("ylab argument must be character type.", call. = FALSE)
    return(NULL)
  }

  # glab must be in quotes and of class character
  if(!inherits(glab,"character")){
    stop("glab argument must be character type.", call. = FALSE)
    return(NULL)
  }

  # plot_titles must be in quotes and of class character
  if(!inherits(plot_titles,"character")){
    stop("plot_titles argument must be character type.", call. = FALSE)
    return(NULL)
  }

  temp <- temp[,c(x,y,grouping)]
  colnames(temp) <- c("x","y","grouping")

  # grouping varable must contain at least two groups
  if (length(unique(temp$grouping)) < 2) {
    stop("Must have at least two groups within grouping variable.", call. = FALSE)
    return(NULL)
  }

  # grouping: at least 1 cluster has more than 1 entry
  if (sum(table(temp$grouping) > 1) == 0) {
    stop("At least one group must contain more than one entry.", call. = FALSE)
    return(NULL)
  }

  # paneled argument must be logical
  if(!is.logical(paneled)){
    stop("Check that paneled is either TRUE or FALSE.", call. = FALSE)
    return(NULL)
  }

  # center argument must be logical
  if(!is.logical(center)){
    stop("Check that center is either TRUE or FALSE.", call. = FALSE)
    return(NULL)
  }

  # respecify variables as centered
  if(center == TRUE) {
    invisible(center(temp, c("x","y"), "grouping", type = "mean", standardize = FALSE, centerResult = FALSE))
  }

  # family
  fam <- lapply(calls, `[[`, "family")[[1]]

  # identify whether family is binomial
  if(is.null(fam)) {
    ##########
    # OLS

    # Model class must be 'lmerMod' or 'lmerModLmerTest'
    if (!(class(model)[1] == "lmerMod" || class(model)[1] =="lmerModLmerTest")) {
      stop("Model class is not 'lmerMod' or 'lmerModLmerTest'.", call. = FALSE)
      return(NULL)
    }

    # Form single level model
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

    # get ols info
    ols_coef <- as.data.frame(stats::coef(lmModel))
    ols_intercept <- ols_coef[which(rownames(ols_coef)=="(Intercept)"),]
    ols_slope <- ols_coef[which(rownames(ols_coef)==x),]

    # get mlm info
    mlm_coef <- stats::coef(model)[[grouping]]
    mlm_coef <- mlm_coef[c("(Intercept)",x)]
    mlm_coef$grouping <- unique(lme4::getME(model, name = "flist")[[grouping]])
    colnames(mlm_coef) <- c("intercept","slope","grouping")

    ### select groups
    # relabel data frame
    if(center == TRUE) {
      levelCompareData <- temp[,c("grouping_x.cmn", "grouping_y.cmn", "grouping")]
    } else {
      levelCompareData <- temp[,c("x","y","grouping")]
    }
    # rename variables
    colnames(levelCompareData) <- c("x","y","grouping")
    # grouping is factor
    levelCompareData$grouping <- as.factor(levelCompareData$grouping)
    # select groups
    suppressWarnings(if(length(select)==1 & select == "select"){
      selectNew <- unique(levelCompareData$grouping)[1:10]
      selectNew <- selectNew[!is.na(selectNew)]
    } else {
      # select must be character type
      if(!inherits(select,"character")){
        stop("Select argument must be character type.", call. = FALSE)
        return(NULL)
      }
      # select must contain at least 2 but not more than 10 numbers
      if(!length(select) <= 10 | !length(select) >= 2){
        stop("Number of groups in select argument must be between 2 and 10.", call. = FALSE)
        return(NULL)
      }
      # select entries must be in grouping variable
      if(!(all(select%in%levelCompareData$grouping))){
        stop("Groups in select argument must be present in the grouping variable.", call. = FALSE)
        return(NULL)
      }
      selectNew <- select
    })

    # Select new groups
    mlm_coef <- mlm_coef[mlm_coef$grouping %in% selectNew,]

    # create new dataset based on selection
    levelCompareData <- levelCompareData[levelCompareData$grouping %in% selectNew,]

    ### plots

    # ols plot

    # plot title
    title_OLS_plot <- plot_titles[1]
    # save plot
    OLS.plot <- ggplot2::ggplot(data = levelCompareData, ggplot2::aes(x = x, y = y)) +
      ggplot2::geom_point(size = 1, color = "grey45") +
      ggplot2::geom_abline(intercept = ols_intercept, slope = ols_slope, linewidth = 1) +
      ggplot2::ggtitle(title_OLS_plot) +
      ggplot2::xlab(xlab) +
      ggplot2::ylab(ylab) +
      ggplot2::theme_bw() +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))

    # mlm plot

    # plot title
    title_levels_plot <- plot_titles[2]
    # save plot
    levels.plot <- if(paneled == TRUE){
      ggplot2::ggplot(data = levelCompareData, ggplot2::aes(x = x, y = y, group = grouping)) +
        ggplot2::facet_wrap( ~ grouping) +
        ggplot2::geom_point(size = 1, ggplot2::aes(colour = grouping)) +
        ggplot2::geom_abline(data = mlm_coef, ggplot2::aes(intercept = mlm_coef$intercept, slope = mlm_coef$slope), col = "black") +
        ggplot2::ggtitle(title_levels_plot) +
        ggplot2::xlab(xlab) +
        ggplot2::ylab(ylab) +
        ggplot2::theme_bw() +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
              legend.position="None")
    } else {
      ggplot2::ggplot(data = levelCompareData, ggplot2::aes(x = x, y = y, color = grouping)) +
        ggplot2::geom_point(size = 1, show.legend = TRUE) +
        ggplot2::guides(color = ggplot2::guide_legend(override.aes=list(shape = 15, size = 3))) +
        ggplot2::geom_abline(data = mlm_coef, ggplot2::aes(intercept = mlm_coef$intercept, slope = mlm_coef$slope), col = mlm_coef$grouping) +
        ggplot2::ggtitle(title_levels_plot) +
        ggplot2::xlab(xlab) +
        ggplot2::ylab(ylab) +
        ggplot2::labs(color=glab) +
        ggplot2::theme_bw() +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
    }
  } else if(fam=="binomial") {
    ##########
    # Logistic

    # Model class must be glmerMod
    if (!(class(model)[1] == "glmerMod")) {
      stop("Model class is not 'glmerMod'.", call. = FALSE)
      return(NULL)
    }

    # Form single level model
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
    ff4 <- ifelse(is.null(subsets) & is.null(w), paste("glm(",ff3, ", data = ", deparse(d), ", family = binomial", ")"),
                  ifelse(!is.null(subsets) & is.null(w), paste("glm(", ff3, ", data = ", deparse(d), ", family = binomial", ", subset = ", deparse(subsets), ")"),
                         ifelse(is.null(subsets) & !is.null(w), paste("glm(", ff3, ", data = ", deparse(d), ", family = binomial", ", weights = ", w, ")"),
                                paste("glm(", ff3, ", data = ", deparse(d), ", family = binomial", ", subset = ", deparse(subsets), ", weights = ", w, ")"))))
    glmModel <- eval(parse(text = ff4), parent.frame())

    # ols
    # plot title
    title_OLS_plot <- plot_titles[1]
    coefs <- stats::coef(glmModel)
    x_plot <- seq(min(dataset[,x]), max(dataset[,x]), by = 0.1)
    y_plot <- stats::plogis(coefs[1] + coefs[2] * x_plot)
    plot_data <- data.frame(x_plot, y_plot)
    # select groups
    suppressWarnings(if(length(select)==1 & select == "select"){
      selectNew <- unique(dataset[,grouping])[1:10]
      selectNew <- selectNew[!is.na(selectNew)]
    } else {
      # select must be character type
      if(!inherits(select,"character")){
        stop("Select argument must be character type.", call. = FALSE)
        return(NULL)
      }
      # select must contain at least 2 but not more than 10 numbers
      if(!length(select) <= 10 | !length(select) >= 2){
        stop("Number of groups in select argument must be between 2 and 10.", call. = FALSE)
        return(NULL)
      }
      # select entries must be in grouping variable
      if(!(all(select%in%levelCompareData$grouping))){
        stop("Groups in select argument must be present in the grouping variable.", call. = FALSE)
        return(NULL)
      }
      selectNew <- select
    })
    title_levels_plot <- plot_titles[2]
    subset <- dataset[dataset[,grouping] %in% selectNew,]
    # save plot
    OLS.plot <- ggplot2::ggplot(plot_data) +
      ggplot2::geom_line(ggplot2::aes(x_plot, y_plot), col = "blue") +
      ggplot2::geom_point(size = 1, color = "grey45", data = subset, ggplot2::aes(x = get(x), y = get(y))) +
      ggplot2::ggtitle(title_OLS_plot) +
      ggplot2::xlab(xlab) +
      ggplot2::ylab(ylab) +
      ggplot2::theme_bw() +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))

    # Select new groups
    ## get mlm info
    title_MLM_plot <- plot_titles[2]
    mlm_coef <- stats::coef(model)[[grouping]]
    mlm_coef <- mlm_coef[c("(Intercept)",x)]
    mlm_coef$grouping <- unique(lme4::getME(model, name = "flist")[[grouping]])
    colnames(mlm_coef) <- c("intercept","slope","grouping")
    mlm_coef <- mlm_coef[mlm_coef$grouping %in% selectNew,]
    y_plots <- stats::plogis(t(apply(outer(x_plot,mlm_coef[,2], FUN = "*"), 1, function(x) x + mlm_coef[,1])))
    plot_datas <- data.frame(x_plot, y_plots)

    plot_datas <- data.frame(grouping = rep(unique(subset[,grouping]),each=length(x_plot)),  # Create data for lines
                             x_plot = rep(x_plot,length(unique(subset[,grouping]))),
                             y_plot = c(y_plots))


    # save plot
    levels.plot <- if(paneled == TRUE){
      ggplot2::ggplot(data = subset, ggplot2::aes(x = get(x), y = get(y), group = "grouping")) +
        ggplot2::facet_wrap( ~ grouping) +
        ggplot2::geom_point(size = 1,color = "grey45") +
        ggplot2::geom_line(data = plot_datas, ggplot2::aes(x_plot, y_plot), color = "blue") +
        ggplot2::ggtitle(title_levels_plot) +
        ggplot2::xlab(xlab) +
        ggplot2::ylab(ylab) +
        ggplot2::theme_bw() +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                       legend.position="None")
    } else {
      subset[,grouping] <- as.factor(subset[,grouping])
      plot_datas[,"grouping"] <- as.factor(plot_datas[,"grouping"])
      ggplot2::ggplot(data = subset, ggplot2::aes(x = get(x), y = get(y))) +
        ggplot2::geom_point(size = 1, ggplot2::aes(color = get(grouping))) +
        ggplot2::geom_line(data = plot_datas, ggplot2::aes(x_plot, y_plot, group = grouping, color = grouping)) +
        ggplot2::ggtitle(title_levels_plot) +
        ggplot2::xlab(xlab) +
        ggplot2::ylab(ylab) +
        ggplot2::theme_bw() +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                       legend.position="None")
    }

  } else {
    stop("This family of model is not supported.", call. = FALSE)
    return(NULL)
  }
  message(c("Two plots have been returned: 'OLS.plot' and 'levels.plot'. Toggle through your 'Plots' window to view them or save them to an object to call them individually (e.g., object$OLS.plot)"))
  result <- list(OLS.plot, levels.plot)
  names(result) <- c("OLS.plot", "levels.plot")
  return(result)
}














