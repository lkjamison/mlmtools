#' Centers variables for mixed effects models
#'
#' Centers variables using the group-mean (person-mean) centering approach for mixed-effects models, and adds these variables to the data frame.
#'
#' @param dataset A dataset containing the variables to be centered and the grouping variable
#' @param x The variable or variables to be centered
#' @param grouping  The variable or variables that define the grouping structure of the data
#' @param type a function to compute the grouping summary variable
#' @param standardize a logical value indicating whether x should be standardized before the computaion proceeds
#' @param centerResult a logical value indicating whether resulting grouping summary variable values should be centered at 0
#'
#'
#' @return Creates two new variables in the data frame - a mean of the desired variable computed for each unique value in the grouping variable and a deviation score for each observation within the grouping variable that is that observation's raw score subtracted from the group mean.
#'
#' @examples
#' data(instruction)
#' #Center student level socioeconomic status, "ses", around class mean "ses"
#' center(dataset = instruction, x = "ses", grouping = "classid")
#' #Center class-level variable teacher's mathematic prepartion,
#' # mathprep, around school mean "mathprep"
#' center(dataset = instruction, x = "mathprep", grouping = "schoolid")
#'
#' @references Enders, C. & Tofighi, D. (2007). Centering predictor variables in cross-sectional multilevel models: A new look at an old problem. Psychological Methods, 12(2), 121–138
#'
#' @export center

center <- function(dataset, x, grouping, type = "mean", standardize = FALSE, centerResult = FALSE) {

  # Error dataset must be a dataframe
  if (!is.data.frame(dataset)) {
    stop("Dataset provided is not of type data.frame.", call. = FALSE)
    return(NULL)
  }

  temp <- as.data.frame(dataset)
  original.data <- as.data.frame(dataset)

  # add id variable
  temp$idMLM <- 1:nrow(temp)
  original.data$idMLM <- 1:nrow(original.data)

  # x must be in dataset
  if (!all(x %in% colnames(temp))) {
    stop("x variables are not in the dataset.", call. = FALSE)
    return(NULL)
  }

  # grouping must be in dataset
  if (!all(grouping %in% colnames(temp))) {
    stop("grouping variables are not in the dataset.", call. = FALSE)
    return(NULL)
  }

  # x variable must be integer or numeric
  if (!all(sapply(temp[,x], typeof) %in% c("double", "integer", "numeric"))) {
    stop("x variables must be type integer or numeric.", call. = FALSE)
    return(NULL)
  }

  # Error only two grouping variables allowed
  if (length(grouping)>2) {
    stop("Only two grouping variables can be used at one time.", call. = FALSE)
    return(NULL)
  }

  # Define subscript for new variables
  if (type=='mean'){
    subscript <- c('.cmn','.devcmn')
  } else if (type=='sd') {
    subscript <- c('.csd','.devcsd')
  } else if (type=='var') {
    subscript <- c('.cvar','.devcvar')
  } else {
    subscript <- c('.cmd','.devcmd')
  }

  # remove rows where cluster varible = NA
  if (length(grouping)==1) {
    temp <- as.data.frame(temp[!is.na(temp[,grouping[1]]),])
  } else {
    temp <- as.data.frame(temp[!is.na(temp[,grouping[1]]),])
    temp <- as.data.frame(temp[!is.na(temp[,grouping[2]]),])
  }

  # NA in cluster variable
  if (nrow(temp)!=nrow(original.data)){
    message("Warning: NAs present in clustering variable(s). Cluster centering conducted only on availabile values. Consider checking data.")
  }

  # Define new grouping variable
  if (length(grouping)==1) {
    temp <- as.data.frame(temp)
    temp$newGroupingMLM <- as.character(temp[,grouping[1]])
    newGroupingMLMname <- grouping[1]
  } else {
    temp <- as.data.frame(temp)
    temp$newGroupingMLM <- paste0(as.character(temp[,grouping[[1]]]),as.character(temp[,grouping[[2]]]))
    newGroupingMLMname <- paste0(grouping[1], grouping[2])
  }

  groupSumVarNoCenter <- NULL
  for (i in x){
    groupSumVarNoCenter <- c(groupSumVarNoCenter,paste0(newGroupingMLMname, "_", i, subscript[1]))
  }
  groupSumVarCenter <- NULL
  for (i in groupSumVarNoCenter){
    groupSumVarCenter <- c(groupSumVarCenter,paste0(i,".c"))
  }
  subjectDev <- NULL
  for (i in x){
    subjectDev <- c(subjectDev,paste0(newGroupingMLMname, "_", i, subscript[2]))
  }

  futureVarNames <- c(groupSumVarNoCenter, groupSumVarCenter, subjectDev)

  # Variable names already exist
  if (sum(names(original.data) %in% futureVarNames) > 0){
    stop(message(c("Duplicate variable names detected. Remove", names(original.data[which(names(original.data) %in% futureVarNames)]), "variables before re-running function.")), call. = FALSE)
    return(NULL)
  }

  # grouping: at least 2 clusters
  if (length(unique(temp$newGroupingMLM)) < 2) {
    stop("Must have at least two groups within grouping variable.", call. = FALSE)
    return(NULL)
  }

  # grouping: at least 1 cluster has more than 1 entry
  if (sum(table(temp$newGroupingMLM) > 1) == 0) {
    stop("At least one group must contain more than one entry.", call. = FALSE)
    return(NULL)
  }

  # type has to equal mean, median, sd or var
  if (length(type)!=1){
    stop("Only one type argument can be specified.", call. = FALSE)
    return(NULL)
  }

  if (!(type%in%c("mean","median","sd","var"))){
    stop("The type argument is not specified correctly.", call. = FALSE)
    return(NULL)
  }

  # standardize & centerResult must be logical operators
  if(!is.logical(standardize)){
    stop("The argument standardize is not specified correctly.", call. = FALSE)
    return(NULL)
  }

  if(!is.logical(centerResult)){
    stop("The argument centerResult is not specified correctly.", call. = FALSE)
    return(NULL)
  }

  # Standardize x variables
  if (standardize == TRUE){
    centVars <- NULL
    centVarsNames <- NULL
    for (i in x){
      stan <- temp[,i]/stats::sd(temp[,i])
      stanName <- paste0(i,".s")
      centVars <- cbind(centVars, stan)
      centVarsNames <- c(centVarsNames, stanName)
    }
    colnames(centVars) <- centVarsNames
    x <- centVarsNames
    temp <- as.data.frame(cbind(temp, centVars))
  } else {
    temp <- as.data.frame(temp)
  }

  # Compute grouping summary variables
  tempDat <- NA
  group.centVarsNames.nc <- NULL

    for (i in x){
      summary <- stats::aggregate(temp[,i] ~ newGroupingMLM, temp, FUN = type, na.rm = TRUE) # with formula notation aggregate uses listwise deletion when na.rm=TRUE
      colnames(summary)[2] <- paste0(newGroupingMLMname, "_", i, subscript[1])
      tempDat <- cbind(tempDat, summary)
      group.centVarsNames.nc <- c(group.centVarsNames.nc, colnames(summary)[2])
    }

  keep <- c("newGroupingMLM", group.centVarsNames.nc)
  tempDat <- tempDat[,keep]
  varRes <- as.data.frame(merge(temp,tempDat,by='newGroupingMLM',sort=FALSE))

  # Center grouping summary variable: mean - column
  if (centerResult == TRUE) {
    group.centVars <- NULL
    group.centVarsNames <- NULL
    for (i in group.centVarsNames.nc){
      centered <- tempDat[,i] - mean(tempDat[,i], na.rm=TRUE)
      centered.names <- paste0(i,".c")
      group.centVars <- cbind(group.centVars, centered)
      group.centVarsNames <- c(group.centVarsNames, centered.names)
    }
    colnames(group.centVars) <- group.centVarsNames
    tempDat <- as.data.frame(cbind(tempDat, group.centVars))
    tempDat <- tempDat[,c('newGroupingMLM',group.centVarsNames)]
    varRes <- as.data.frame(merge(varRes,tempDat,by='newGroupingMLM',sort=FALSE))
  } else {
    group.centVarsNames <- group.centVarsNames.nc
  }

  # Compute subject deviation from group summary score
  tempDat2 <- NA
  tempDat2names <- NULL

  for (i in x){
    dev <- varRes[,i] - varRes[,which(colnames(varRes)==paste0(newGroupingMLMname, "_", i, subscript[1]))]
    name <- paste0(newGroupingMLMname, "_", i, subscript[2])
    tempDat2 <- cbind(tempDat2, dev)
    tempDat2names <- c(tempDat2names, name)
  }
  tempDat2 <- as.data.frame(tempDat2)
  tempDat2 <- tempDat2[,-which(colnames(tempDat2)=="tempDat2")]
  tempDat2 <- as.data.frame(tempDat2)
  colnames(tempDat2) <- tempDat2names
  varRes <- as.data.frame(cbind(varRes, tempDat2))

  # Add variables to original dataset
  res1 <- tempDat2names # deviations
  if (centerResult == TRUE) {
    res2 <- group.centVarsNames# group summary
  } else {
    res2 <- group.centVarsNames.nc
  }
  resToAdd <- varRes[,c('idMLM', res1, res2)]

  finalData <- as.data.frame(cbind(original.data,resToAdd))
  dataName <- deparse(substitute(dataset))
  assign(dataName, finalData, envir = parent.frame())

  finalData <- as.data.frame(merge(original.data,resToAdd,by='idMLM',sort=FALSE))
  finalData$idMLM <- NULL
  resToAdd$idMLM <- NULL
  dataName <- deparse(substitute(dataset))
  assign(dataName, finalData, envir = parent.frame())

  # Output Results

  # Variable names added to dataset
  resNames <- (list(
    "Variables" = colnames(resToAdd)))
  class(resNames) <- "center"
  return(resNames)

}


