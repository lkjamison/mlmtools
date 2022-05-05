#' Plots estimations of the model
#'
#' Plots the  estimations from the model
#'
#' @param model
#'
#' @param xlab
#'
#' @param ylab
#'
#' @param estimation_title
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

estimationsPlot <- function(model, xlab = x, ylab = y, estimation_title = "Estimations Plot"){

  # Error if model not of class merMod

  temp <- as.data.frame(dataset)

  xlab <- ifelse(xlab==x, x, xlab)
  ylab <- ifelse(ylab==y, y, ylab)

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

  # estimation_title must be in quotes and of class character
  if(!class(estimation_title) == "character"){
    stop("Check that your estimation_title is in quotes.", call. = FALSE)
    return(NULL)
  }

  ##### estimation plot
  # Adding in predictions as a column
  predicted_data <- cbind(Orthodont,predict(fm3))

  # Predict does not work in the same way as nlme, creating a new object and taking the mean
  predicted_data <- predicted_data[(predicted_data$age==min(predicted_data$age) | predicted_data$age==max(predicted_data$age)),]
  colnames(predicted_data)[5] <- "predicted"

  means <- aggregate(predicted ~ age + Sex, data = predicted_data, FUN= "mean" )

  ### TRY USING GGPREDICT AND RANEF - CHECK IF GGEFFECTS DOES THIS BETTER OR IN A MORE USEFUL WAY

  estimations.plot <- ggplot(Orthodont, aes(x=age, y=distance, colour=Sex)) +
    geom_point(size=3) +
    geom_line(aes(y=predict(fm3), group=Subject, size="Subjects")) +
    geom_line(data=means,
              aes(y = predicted, size="Population")) +
    scale_size_manual(name="Predictions", values=c("Subjects"=0.5, "Population"=3)) +
    theme_bw(base_size=22)

  suppressMessages(print(estimations.plot))
}











