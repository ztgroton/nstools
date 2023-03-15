
#' S3 Constructor for Class 'bool_formula'
#'
#' @param bool_formula character
#' @param join_op character
#'
#' @return S3 Object
#' @export
#'
#' @examples
#' \dontrun{
#' result <- new_bool_formula(x)
#' }
new_bool_formula <- function(bool_formula, join_op) {

  # Validate Input
  if (missing(bool_formula)) {bool_formula <- list()}
  if (missing(join_op)) {join_op <- 'and'}

  # Validate Input Expectations

  # * `join_op`
  if (!isTRUE(identical(join_op, 'and')) && !isTRUE(identical(join_op, 'or'))) {
    stop("`join_op` must equal 'and'/'or' in call to `new_bool_formula`")
  }

  # Initialize Empty S3 Object
  rs <- new.env()

  # Store `bool_formula` in `rs`
  rs$bool_formula <- bool_formula

  # Set Class
  class(rs) <- c(setdiff('bool_formula', class(rs)), class(rs))

  # Lock `rs`
  rlang::env_lock(rs)

  # Return S3 Object
  return(rs)

}

#' S3 Validator for Class 'bool_formula'
#'
#' @param obj S3 Object
#' @param bool TRUE/FALSE
#'
#' @return S3 Object
#' @export
#'
#' @examples
#' \dontrun{
#' test <- validate_bool_formula(obj, FALSE)
#' }
validate_bool_formula <- function(obj, bool) {

  # Validate Inputs
  if (missing(obj)) {stop("`obj` is missing in call to `validate_bool_formula`")}
  if (missing(bool)) {bool <- FALSE}

  # Initialize Empty Character Vector for Error Messages
  err <- vector(mode = 'character')

  # Validate Input Expectations

  # * `obj`
  if (!isTRUE(inherits(obj, 'bool_formula'))) {
    msg <- "`obj` must inherit from 'bool_formula'"
    err <- c(msg, err)
  }

  # * `bool`
  if (!isTRUE(identical(bool, TRUE)) && !isTRUE(identical(bool, FALSE))) {
    msg <- "`bool` must be identical with TRUE/FALSE"
    err <- c(msg, err)
  }

  # ADD CUSTOM INPUT VALIDATIONS HERE (USE SAME TEMPLATE AS `obj` and `bool`)

  # * `obj`
  if (!isTRUE(is.environment(obj)) || !isTRUE(identical(names(obj), 'bool_formula'))) {
    msg <- "`obj` is not a validly formatted environment in call to `validate_bool_formula`"
    err <- c(msg, err)
  }

  # Conditionally Return Error Messages (if any at this point in execution)
  if (!isTRUE(length(err) == 0)) {return(err)}

  # * `obj$bool_formula`
  is_valid_bool_formula <- purrr::map_lgl(obj$bool_formula, function(x){

    if (isTRUE(validate_bool_expr(x, TRUE))) {
      return(TRUE)
    } else if (isTRUE(validate_bool_formula(x, TRUE))) {
      return(TRUE)
    } else {
      return(FALSE)
    }

  })

  if (!isTRUE(all(is_valid_bool_formula))) {
    msg <- "`obj$bool_formula` must be validly formatted in call to `validate_bool_formula`"
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

#' S3 Helper Function for Class 'bool_formula'
#'
#' @param bool_formula character
#' @param join_op character
#'
#' @return S3 Object
#' @export
#'
#' @examples
#' \dontrun{
#' result <- bool_formula(x)
#' }
bool_formula <- function(bool_formula, join_op) {

  # Validate Input
  if (missing(bool_formula)) {bool_formula <- list()}
  if (missing(join_op)) {join_op <- 'and'}

  validate_bool_formula(new_bool_formula(bool_formula))

}
