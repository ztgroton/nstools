
#' S3 Method - Evaluate Expression for S3 Class 'col_expr'
#'
#' @param obj S3 Object
#' @param frame data.frame
#'
#' @return character
#' @export
#'
#' @examples
#' \dontrun{
#' result <- ns_eval(col_expr)
#' }
ns_eval.col_expr <- function(obj, frame) {

  # Validate Inputs
  if (missing(obj)) {stop("`obj` is missing in call to `ns_eval.col_expr`")}
  if (missing(frame)) {stop("`frame` is missing in call to `ns_eval.col_expr`")}

  # Validate Input Expectations
  if (!isTRUE(inherits(obj, 'col_expr'))) {
    stop("`obj` must inherit from 'col_expr' in call to `ns_eval.col_expr`")
  } else if (!isTRUE(validate_col_expr(obj, TRUE))) {
    stop("`obj` must be valid 'col_expr' in call to `ns_eval.col_expr`")
  }

  # Evaluate Expression `obj$col_expr` against `frame`
  result <- eval(expr = obj$col_expr, envir = frame, enclos = parent.frame())

  # Validate that `result` is 'vector'
  if (!isTRUE(is.vector(result))) {
    stop("`result` must be a vector in call to `ns_eval.col_expr`")
  }

  # Return Result
  return(result)

}
