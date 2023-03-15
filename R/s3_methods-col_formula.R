
#' S3 Method - Evaluate Expression for S3 Class 'col_formula'
#'
#' @param obj S3 Object
#' @param frame data.frame
#'
#' @return character
#' @export
#'
#' @examples
#' \dontrun{
#' result <- ns_eval(col_formula)
#' }
ns_eval.col_formula <- function(obj, frame) {

  # Validate Inputs
  if (missing(obj)) {stop("`obj` is missing in call to `ns_eval.col_formula`")}
  if (missing(frame)) {stop("`frame` is missing in call to `ns_eval.col_formula`")}

  # Validate Input Expectations
  if (!isTRUE(inherits(obj, 'col_formula'))) {
    stop("`obj` must inherit from 'col_formula' in call to `ns_eval.col_formula`")
  } else if (!isTRUE(validate_col_formula(obj, TRUE))) {
    stop("`obj` must be valid 'col_formula' in call to `ns_eval.col_formula`")
  }

  # Evaluate Formula Components
  component_results <- purrr::map(obj$col_formula, function(x) {

    if (isTRUE(validate_col_expr(x, TRUE))) {
      return(ns_eval.col_expr(x, frame))
    } else if (isTRUE(validate_col_formula(x, TRUE))) {
      return(ns_eval.col_formula(x, frame))
    } else {
      stop("`obj$col_formula` contains an invalid element in call to `ns_eval.col_formula`")
    }

  })

  # Join Formula Components
  if (isTRUE(identical(obj$join_op, 'and'))) {
    formula_result <- purrr::reduce(component_results, `&`)
  } else if (isTRUE(identical(obj$join_op, 'or'))) {
    formula_result <- purrr::reduce(component_results, `|`)
  } else {
    stop("`obj$join_op` must equal 'and'/'or' in call to `ns_eval.col_formula`")
  }

  # Validate that `formula_result` is 'vector'
  if (!isTRUE(is.vector(formula_result))) {
    stop("`formula_result` must be a vector in call to `ns_eval.col_formula`")
  }

  # Return Result
  return(formula_result)

}
