#' Instruction Data
#'
#' Data from a QTL experiment on gravitropism in
#' Arabidopsis, with data on 162 recombinant inbred lines (Ler x
#' Cvi). The outcome is the root tip angle (in degrees) at two-minute
#' increments over eight hours.
#'
#' @docType data
#'
#' @usage data(instruction)
#'
#' @format A data frame with 1190 observations on the following 8 variables.
#' \describe{
#' \item{\code{female - female}}{Dummy variable for being female}
#' \code{mathkind - mathkind}{Math achievement score in the spring of kindergarten}
#' \code{mathgain - mathgain}{Gain in math achievement score from spring of kindergarten to spring of first grade}
#' \code{ses - ses}{Socioeconomic status}
#' \code{mathprep - mathprep}{First grade teacher's mathematic preparation (based on number of courses taken)}
#' \code{classid - classid}{Classroom identifier}
#' \code{schoolid - schoolid}{School identifier}
#' \code{childid - childid}{Student identifier}
#' }
#'
#'
#' @keywords datasets
#'
#' @references Rabe-Hesketh, Sophia, and Brian Everitt. Handbook of statistical analyses using Stata. CRC Press, 2003.
#'
#' @source \href{https://www.stata-press.com/data/mlmus3/instruction.dta}{Stata Press}
#'
#' @examples
#' data(instruction)
"instruction"
