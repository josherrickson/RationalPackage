setClass("Rational",
         slots = c(numerator = "integer",
                   denominator = "integer"))


### I'm using a deliberately short and non-descriptive function name here. I
### envision that instead of c(5, 2), you'd call R(5, 2)
##' @title Rational number creator
##' @param num Numerator integer
##' @param denom Denominator integer
##' @return A `Rational` object
##' @export
R <- function(num, denom) {
  stopifnot(num%%1 == 0, denom%%1 == 0)
  num <- as.integer(num)
  denom <- as.integer(denom)

  new("Rational", numerator = num, denominator = denom)
}

setValidity("Rational", function(object) {
  if (is.na(object@denominator) || is.na(object@numerator)) {
    stop("Cannot contain NA")
  }
  if (object@denominator == 0) {
    stop("Rational number must be finite")
  }
  return(TRUE)
})
