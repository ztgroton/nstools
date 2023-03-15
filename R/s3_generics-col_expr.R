
#' S3 Generic - Evaluate Expression for S3 Class 'col_expr'
#'
#' @param obj S3 Object
#' @param frame data.frame
#'
#' @return R Object
#' @export
#'
#' @examples
#' \dontrun{
#' result <- ns_eval(col_expr)
#' result <- ns_eval(bool_col_expr)
#' }
ns_eval <- function(obj, frame) {UseMethod("ns_eval", obj)}
