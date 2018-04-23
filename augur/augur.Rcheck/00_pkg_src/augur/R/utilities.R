#' Return the matching regex
#' 
#' I suspect there's a better way to do this.
#' 
#' @param x The thing to examine
#' @param p The pattern to grep

grep_raw <- function(x, p){
  regmatches(x, regexpr(p, x))
}
