
#' S3 Method - Evaluate Expression for S3 Class 'bool_expr'
#'
#' @param obj S3 Object
#' @param frame data.frame
#'
#' @return character
#' @export
#'
#' @examples
#' \dontrun{
#' result <- ns_eval(bool_expr)
#' }
ns_eval.bool_expr <- function(obj, frame) {

  # Validate Inputs
  if (missing(obj)) {stop("`obj` is missing in call to `ns_eval.bool_expr`")}
  if (missing(frame)) {stop("`frame` is missing in call to `ns_eval.bool_expr`")}

  # Validate Input Expectations
  if (!isTRUE(inherits(obj, 'bool_expr'))) {
    stop("`obj` must inherit from 'bool_expr' in call to `ns_eval.bool_expr`")
  } else if (!isTRUE(validate_bool_expr(obj, TRUE))) {
    stop("`obj` must be valid 'bool_expr' in call to `ns_eval.bool_expr`")
  }

  # Evaluate Expression `obj$col_expr` against `frame`
  result <- eval(expr = obj$col_expr, envir = frame, enclos = parent.frame())

  # Validate that `result` is 'vector'
  if (!isTRUE(is.vector(result))) {
    stop("`result` must be a vector in call to `ns_eval.bool_expr`")
  }

  # Validate that `result` is 'logical'
  if (!isTRUE(is.logical(result))) {
    stop("`result` must be type 'logical' in call to `ns_eval.bool_expr`")
  }

  # Return Result
  return(result)

}
