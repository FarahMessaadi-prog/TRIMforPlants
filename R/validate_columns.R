#' Validate that a dataframe contains required columns
#'
#' Checks if all columns specified in \code{cols} are present in the input dataframe.
#' @param df A `data.frame`.
#' @param cols A character vector of required column names.
#'
#' @return \code{NULL} invisibly.
#' @details Throws a **stop error** if one or more required columns are missing.
#' @export
#'
#' @examples
#' df <- data.frame(a = 1:3, b = 4:6)
#' validate_columns(df, c("a", "b"))   # OK
#' # validate_columns(df, c("a", "c"))  # erreur : colonne manquante
validate_columns <- function(df, cols) {
  missing <- base::setdiff(cols, base::names(df))
  if (base::length(missing) > 0) {
    base::stop("Colonnes manquantes dans les données: ",
               base::paste(missing, collapse = ", "))
  }
  invisible(NULL)
}
