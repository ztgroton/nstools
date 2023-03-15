
#' S3 Constructor for Class 'col_expr'
#'
#' @param col_expr character
#'
#' @return S3 Object
#' @export
#'
#' @examples
#' \dontrun{
#' result <- new_col_expr(x)
#' }
new_col_expr <- function(col_expr) {

  # Validate Input
  if (missing(col_expr)) {stop("`col_expr` is missing in call to `new_col_expr`")}

  # Initialize Empty S3 Object
  rs <- new.env()

  # Store `col_expr` in `rs`
  rs$col_expr <- col_expr

  # Set Class
  class(rs) <- c(setdiff('col_expr', class(rs)), class(rs))

  # Lock `rs`
  rlang::env_lock(rs)

  # Return S3 Object
  return(rs)

}

#' S3 Validator for Class 'col_expr'
#'
#' @param obj S3 Object
#' @param bool TRUE/FALSE
#'
#' @return S3 Object
#' @export
#'
#' @examples
#' \dontrun{
#' test <- validate_col_expr(obj, FALSE)
#' }
validate_col_expr <- function(obj, bool) {

  # Validate Inputs
  if (missing(obj)) {stop("`obj` is missing in call to `validate_col_expr`")}
  if (missing(bool)) {bool <- FALSE}

  # Initialize Empty Character Vector for Error Messages
  err <- vector(mode = 'character')

  # Validate Input Expectations

  # * `obj`
  if (!isTRUE(inherits(obj, 'col_expr'))) {
    msg <- "`obj` must inherit from 'col_expr'"
    err <- c(msg, err)
  }

  # * `bool`
  if (!isTRUE(identical(bool, TRUE)) && !isTRUE(identical(bool, FALSE))) {
    msg <- "`bool` must be identical with TRUE/FALSE"
    err <- c(msg, err)
  }

  # ADD CUSTOM INPUT VALIDATIONS HERE (USE SAME TEMPLATE AS `obj` and `bool`)

  # * `obj`
  if (!isTRUE(is.environment(obj)) || !isTRUE(identical(names(obj), 'col_expr'))) {
    msg <- "`obj` is not a validly formatted environment in call to `validate_col_expr`"
    err <- c(msg, err)
  }

  # Conditionally Return Error Messages (if any at this point in execution)
  if (!isTRUE(length(err) == 0)) {return(err)}

  # * `obj$col_expr`
  is_expr <- isTRUE(rlang::is_expression(obj$col_expr))

  if (!isTRUE(is_expr)) {
    msg <- "`obj$col_expr` must be 'expression' in call to `validate_col_expr`"
    err <- c(msg, err)
  }

  # Final Output
  if (!isTRUE(bool)) {

    if (isTRUE(length(err) == 0)) {
      return(obj)
    } else {
      return(err)
    }

  } else {

    if (isTRUE(length(err) == 0)) {
      return(TRUE)
    } else {
      return(FALSE)
    }

  }

}

#' S3 Helper Function for Class 'col_expr'
#'
#' @param col_expr character
#'
#' @return S3 Object
#' @export
#'
#' @examples
#' \dontrun{
#' result <- col_expr(x)
#' }
col_expr <- function(col_expr) {

  # Validate Input
  if (missing(col_expr)) {stop("`col_expr` is missing in call to `col_expr`")}

  validate_col_expr(new_col_expr(substitute(col_expr)))

}
