#' Plots comparison of accounting for nesting vs. not accounting for nesting
#'
#' Plots the line of best for the relationship between two variables accounting for nesting and not accounting for nesting.
#'
#' @param x
#'
#' @param y
#'
#' @param grouping Grouping variable.
#'
#' @param dataset A dataset containing the predictor, outcome, and grouping variables.
#'
#' @param xlab Character vector specifying the horizontal axis label.
#'
#' @param ylab Character vector specifying the vertical axis label.
#'
#' @param compare_title Character vector specifying the title for the comparison plot.
#'
#' @param center A logical value indicating whether x variable should be centered
#'
#' @param paneled A logical value indicating whether the plot accounting for nesting should be split into panels
#'
#' @references
#'
#' @examples
#'
#' \donttest{
#'
#' }
#'
#' @export levelComparePlot

levelComparePlot <- function(x, y, grouping, dataset, xlab = x, ylab = y, compare_title = "Comparison Plot", center = FALSE, paneled = FALSE){

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
  if(!class(xlab) == "character"){
    stop("Check that your xlab is in quotes.", call. = FALSE)
    return(NULL)
  }

  # ylab must be in quotes and of class character
  if(!class(ylab) == "character"){
    stop("Check that your ylab is in quotes.", call. = FALSE)
    return(NULL)
  }

  # comparison_title must be in quotes and of class character
  if(!class(comparison_title) == "character"){
    stop("Check that your comparison_title is in quotes.", call. = FALSE)
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

  # center must be in quotes and of class character
  if(!is.logical(center)){
    stop("Check that center is either TRUE or FALSE.", call. = FALSE)
    return(NULL)
  }

  # respecify variables as centered
  if(center == TRUE) {
    invisible(center(temp, c("x","y"), "grouping", type = "mean", standardize = FALSE, centerResult = FALSE))
  }

  ##### levelCompare plot
  # select variables:
  levelCompareData <- temp[,c("grouping", "grouping_x.cmn", "grouping_y.cmn")]

  OLS.plot <- ggplot2::ggplot(data = test, ggplot2::aes(x = SES, y = Score)) +
    ggplot2::geom_point() +
    ggplot2::geom_smooth(method = "lm", se = FALSE) +
    ggplot2::theme_classic() +
    ggplot2::labs(title = "Title",
         x = "SES",
         y = "Score")

  levels.plot <- if(paneled == TRUE){
    ggplot2::ggplot(data = test, ggplot2::aes(x =SES, y=Score,group=ID))+
      ggplot2::facet_grid( ~ ID)+
      ggplot2::geom_point(ggplot2::aes(colour = ID))+
      ggplot2::geom_smooth(method = "lm", se = TRUE, ggplot2::aes(colour = ID))+
      ggplot2::xlab("SES")+
      ggplot2::ylab("Score")+
      ggplot2::theme(legend.position = "none") +
      ggplot2::theme_bw(base_size = 12, base_family = "")
  } else {
    ggplot2::ggplot(data = test, ggplot2::aes(x = SES, y = Score, color = ID)) +
      ggplot2::geom_point() +
      ggplot2::geom_smooth(method = "lm", se = FALSE) +
      ggplot2::theme_classic() +
      ggplot2::labs(title = "Title",
                    x = "SES",
                    y = "Score")
  }

  suppressMessages(print(OLS.plot, levels.plot))
}











