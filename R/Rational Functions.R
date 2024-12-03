##' @title Show method for `Rational` object
##' @param object A `Rational` object
##' @return `object`, invisibly
##' @export
setMethod("show", "Rational", function(object) {
  cat(object@numerator, "/", object@denominator, "\n")
  return(invisible(object))
})

##' @title Simplify a `Rational` object
##' @param object A `Rational` object
##' @return A `Rational` object equal to `object`, but whose numerator and
##'   denominator have GCD of 1.
##' @export
simplify <- function(object) {
  stopifnot(is(object, "Rational"))
  divisor <- gcd(object@numerator, object@denominator)
  return(R(object@numerator/divisor,
           object@denominator/divisor))
}

##' @title Convert a `Rational` into a decimal
##' @param object A `Rational` object
##' @param digits The number of digits to print. The returned value has machine
##'   precision.
##' @return The decimal numeric created from the `Rational`
##' @export
##' @examples
##' rat <- R(2, 4)
##' quotient(rat)
quotient <- function(object, digits = NULL) {
  stopifnot(is(object, "Rational"))
  if(!(is.null(digits) ||
       is.integer(digits) ||
       (is.numeric(digits) & digits == round(digits) & digits > 0))) {
    stop("digits must be a positive integer or NULL")
  }

  return(print(object@numerator/object@denominator, digits = digits))
}

##' @title Operators for `Rational` Objects
##' @param e1 a `Rational` Object
##' @param e2 a `Rational` Object
##' @return a `Rational` Object
##' @rdname rational_ops
setMethod("+", signature("Rational", "Rational"),
          function(e1, e2) {
            lcm <- lcm(e1@denominator, e2@denominator)
            scale1 <- lcm/e1@denominator
            scale2 <- lcm/e2@denominator

            return(R(e1@numerator*scale1 + e2@numerator*scale2,
                     lcm))
          })

# This isn't explicitly needed, but is nice to have. One argument implies "-R()"
# as opposed to the two argument version below for "R() - R()".
##' @rdname rational_ops
setMethod("-", signature("Rational"),
          function(e1) {
            e1@numerator <- -e1@numerator
            return(e1)
          })

##' @rdname rational_ops
setMethod("-", signature("Rational", "Rational"),
          function(e1, e2) {
            return(e1 + (-e2))
          })


##' @rdname rational_ops
setMethod("*", signature("Rational", "Rational"),
          function(e1, e2) {
            return(R(e1@numerator*e2@numerator,
                     e1@denominator*e2@denominator))
          })

##' @rdname rational_ops
setMethod("/", signature("Rational", "Rational"),
          function(e1, e2) {
            e2 <- R(e2@numerator, e2@denominator)
            return(e1 * e2)
          })

##' @rdname rational_ops
setMethod("^", signature("Rational", "numeric"),
          function(e1, e2) {
            return(R(e1@numerator^e2,
                     e1@denominator^e2))
          })


#' Rational to string
#' @param x A `Rational` object
#' @return Character version of `x`
#' @export
as.character.Rational <- function(x) {
  return(paste0(x@numerator, "/", x@denominator))
}



