#' Plots Between Group Associations
#'
#' Plots the between-group associations between an outcome and predictor variable.
#'
#' @param x Predictor variable.
#'
#' @param y Outcome variable.
#'
#' @param grouping Grouping variable.
#'
#' @param dataset A dataset containing the predictor, outcome, and grouping variables.
#'
#' @param xlab Character vector specifying the horizontal axis label.
#'
#' @param ylab Character vector specifying the vertical axis label.
#'
#' @param between_title Character vector specifying the title for the between group plot.
#'
#' @param point_color Color for points.
#'
#' @param line_color Color for lines.
#'
#' @param se A logical value indicating whether confidence intervals should be displayed.
#'
#' @param full_range A logical value indicating whether the fit line should span the full range of the plot or just the data.
#'
#' @param lty Line type.
#'
#' @param size Width of fit line.
#'
#' @references Chow, S., Gilmore, R. O., Hallquist, M., Ram, N., & Brinberg, M. (2019). Introduction to multilevel model and interactions. GitHub. https://github.com/psu-psychology/r-bootcamp-2019/blob/master/talks/RBootcamp_MLMInteractions_2019_0820_Final2.Rmd
#'
#' @return Produces a plot of the between-group associations between an outcome and predictor variable.
#'
#' @examples
#' # Read in data
#' data(instruction)
#' # Produce between plot
#' betweenPlot(x = "mathkind", y = "mathgain", grouping = "classid",
#' dataset = instruction, xlab = "Kindergarten Math Score",
#' ylab = "Gain in Math Score")
#'
#' @export betweenPlot

betweenPlot <- function(x, y, grouping, dataset, xlab = x, ylab = y, between_title = "Between-Group Association Plot", point_color = "gray40", line_color="black", se = FALSE, full_range = FALSE, lty=1, size=2){

  # Error dataset must be a dataframe
  if (!is.data.frame(dataset)) {
    stop("Dataset provided is not of type data.frame.", call. = FALSE)
    return(NULL)
  }

  temp <- as.data.frame(dataset)

  xlab <- ifelse(xlab==x, x, xlab)
  ylab <- ifelse(ylab==y, y, ylab)

  # x must be one of the variables in the dataset
  if(!(x%in%names(temp))){
    stop("The x variable specified is a variable in the dataset provided.", call. = FALSE)
    return(NULL)
  }

  # y must be one of the variables in the dataset
  if(!(y%in%names(temp))){
    stop("The y variable specified is a variable in the dataset provided.", call. = FALSE)
    return(NULL)
  }

  # grouping must be one of the variables in the dataset
  if(!(grouping%in%names(temp))){
    stop("The grouping variable specified is a variable in the dataset provided.", call. = FALSE)
    return(NULL)
  }

  # xlab must be in quotes and of class character
  if(!inherits(xlab,"character")){
    stop("Check that your xlab is in quotes.", call. = FALSE)
    return(NULL)
  }

  # ylab must be in quotes and of class character
  if(!inherits(ylab,"character")){
    stop("Check that your ylab is in quotes.", call. = FALSE)
    return(NULL)
  }

  # between_title must be in quotes and of class character
  if(!inherits(between_title,"character")){
    stop("Check that your between_title is in quotes.", call. = FALSE)
    return(NULL)
  }

  # point_color must be in quotes and of class character
  if(!inherits(point_color,"character")){
    stop("Check that your point_color is in quotes.", call. = FALSE)
    return(NULL)
  }

  # line_color must be in quotes and of class character
  if(!inherits(line_color,"character")){
    stop("Check that your line_color is in quotes.", call. = FALSE)
    return(NULL)
  }

  # se must be in quotes and of class character
  if(!is.logical(se)){
    stop("Check that your se is either TRUE or FALSE.", call. = FALSE)
    return(NULL)
  }

  # full_range must be in quotes and of class character
  if(!is.logical(full_range)){
    stop("Check that your full_range is either TRUE or FALSE.", call. = FALSE)
    return(NULL)
  }

  # full_range must be in quotes and of class character
  if(!is.numeric(size)){
    stop("Check that your size is numeric.", call. = FALSE)
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

  # respecify variables as centered
  invisible(center(temp, c("x","y"), "grouping", type = "mean", standardize = FALSE, centerResult = FALSE))

  ##### Between-group plot
  # select variables:
  betweenData <- temp[,c("grouping", "grouping_x.cmn", "grouping_y.cmn")]

  between.plot <- ggplot2::ggplot(data=betweenData, ggplot2::aes(x=grouping_x.cmn, y=grouping_y.cmn, group=factor(grouping)), legend=FALSE) +
    ggplot2::geom_point(colour=point_color) +
    ggplot2::geom_smooth(ggplot2::aes(group=1), method=stats::lm, se=se, fullrange=full_range, lty=lty, size=size, color=line_color) +
    ggplot2::xlab(xlab) + ggplot2::ylab(ylab) +
    ggplot2::theme_classic() +
    ggplot2::theme(axis.title=ggplot2::element_text(size=16),
          axis.text=ggplot2::element_text(size=12),
          plot.title=ggplot2::element_text(size=16, hjust=.5)) +
    ggplot2::ggtitle(between_title)

  result <- between.plot
  return(result)
}











