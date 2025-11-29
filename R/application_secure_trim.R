#' Safely run rtrim::trim and handle potential errors
#'
#' @description This function wraps \code{rtrim::trim} in a try-catch block
#' to prevent package execution failure due to common TRIM convergence errors.
#'
#' @param ... Arguments passed to rtrim::trim
#'
#' @return The result of \code{rtrim::trim} (a TRIM model object) or \code{NULL} if an error occurs.
#' @export
#' @importFrom rtrim trim
#'
#' @examples
safe_trim <- function(...) {
  tryCatch(rtrim::trim(...), error = function(e) {
    base::message("rtrim::trim() failed: ", e$message)
    return(NULL)
  })
}


#' Safely run rtrim::wald
#'
#' @param model TRIM model object
#'
#' @return The result of rtrim::wald or NULL if an error occurs
#' @export
#'
#' @examples
safe_wald <- function(model) {
  tryCatch(rtrim::wald(model), error = function(e) {
    base::message("rtrim::wald() failed: ", e$message)
    return(NULL)
  })
}
