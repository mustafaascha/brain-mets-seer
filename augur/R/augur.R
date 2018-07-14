
#' @author Mustafa Ascha, \email{mustafa.ascha@case.edu}
#' @seealso \code{\link{augur}}
#' @keywords SEER-Medicare SEER 
#'
#' @import magrittr dplyr readr
#' @importFrom graphics par plot rect text

#https://github.com/STAT545-UBC/Discussion/issues/451
#https://github.com/jennybc/googlesheets/blob/master/R/googlesheets.R
## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1") utils::globalVariables(c("."))






