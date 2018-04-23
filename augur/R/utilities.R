#' Return the matching regex
#' 
#' I suspect there's a better way to do this.
#' 
#' @param x The thing to examine
#' @param p The pattern to grep
#' @param v Whether to return non-matching characters

grep_raw <- function(x, p, v = FALSE){
  regmatches(x, regexpr(p, x), invert = v)
}



