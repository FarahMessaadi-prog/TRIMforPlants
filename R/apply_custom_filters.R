#' Apply custom filters to a dataframe
#'
#' Filters a dataframe based on various criteria (year, geography, precision, origin)
#' using flexible column names and relational operators.
#'
#' @param data A dataframe
#' @param year_col Name of the year column (default: "year")
#' @param year_filter List of the form (`>`, 2000)
#' @param population_type_col Name of the population type column (default: "origine")
#' @param population_type_values Vector of values to keep for population type
#' @param precision_col Name of the precision column (default: "precision")
#' @param precision_filter List of the form (`<=`, 100)
#' @param canton_col Name of the canton column (default: "canton")
#' @param canton_filter Vector of canton values to keep
#' @param other_geo_filters List of other geographic filters (named list: list(commune = c("Chancy", "Dardagny")))
#'
#' @return A **list** with two elements:
#' \itemize{
#'  \item \code{data}: The filtered dataframe.
#'  \item \code{filters}: A list summarizing the filters applied and the reduction in rows (initial_n, final_n, removed_n).
#' }
#' @export
#' @seealso \code{\link{create_clean_data}} pour la standardisation des colonnes.
#'
#' @examples
#' # Exemple avec colonnes standard :
#' apply_custom_filters(data, year_filter = list(`>`, 2010),
#'                      population_type_values = c("Suisse"),
#'                      precision_filter = list(`<=`, 100),
#'                      canton_filter = c("VD", "GE"))
#'
#' # Exemple avec colonnes personnalisees :
#' apply_custom_filters(data,
#'                      year_col = "annee",
#'                      population_type_col = "type_pop",
#'                      canton_col = "canton_nom",
#'                      year_filter = list(`>`, 2010),
#'                      other_geo_filters = list(region = c("CH")))
#'
apply_custom_filters <- function(data,
                                 year_col = "year",
                                 year_filter = NULL,
                                 population_type_col = "origine",
                                 population_type_values = NULL,
                                 precision_col = "precision",
                                 precision_filter = NULL,
                                 canton_col = "canton",
                                 canton_filter = NULL,
                                 other_geo_filters = list()) {
  initial_n <- nrow(data)
  filters_applied <- list()

  # Vérification que les colonnes existent
  cols <- c(year_col, population_type_col, precision_col, canton_col)
  missing_cols <- cols[!cols %in% names(data)]
  if (length(missing_cols) > 0) {
    warning("Colonnes manquantes dans les donnees : ", paste(missing_cols, collapse = ", "))
  }

  # Year filter
  if (!is.null(year_filter) && year_col %in% names(data)) {
    op <- year_filter[[1]]
    val <- year_filter[[2]]
    condition <- do.call(op, list(data[[year_col]], val))
    data <- data[condition, ]
    filters_applied$year_filter <- paste0(year_col, " ", deparse(substitute(op)), " ", val)
  }

  # Population type filter
  if (!is.null(population_type_values) && population_type_col %in% names(data)) {
    data <- data[data[[population_type_col]] %in% population_type_values, ]
    filters_applied$population_type_filter <- paste0(population_type_col, " in [",
                                                     paste(population_type_values, collapse = ", "), "]")
  }

  # Precision filter
  if (!is.null(precision_filter) && precision_col %in% names(data)) {
    op <- precision_filter[[1]]
    val <- precision_filter[[2]]
    condition <- do.call(op, list(data[[precision_col]], val))
    data <- data[condition, ]
    filters_applied$precision_filter <- paste0(precision_col, " ", deparse(substitute(op)), " ", val)
  }

  # Canton filter
  if (!is.null(canton_filter) && canton_col %in% names(data)) {
    data <- data[data[[canton_col]] %in% canton_filter, ]
    filters_applied$canton_filter <- paste0(canton_col, " in [",
                                            paste(canton_filter, collapse = ", "), "]")
  }

  # Other geographic filters
  if (length(other_geo_filters) > 0) {
    for (geo_col in names(other_geo_filters)) {
      if (geo_col %in% names(data)) {
        values <- other_geo_filters[[geo_col]]
        data <- data[data[[geo_col]] %in% values, ]
        filters_applied[[paste0("geo_filter_", geo_col)]] <- paste0(geo_col, " in [",
                                                                    paste(values, collapse = ", "), "]")
      } else {
        warning(paste("Colonne geographique", geo_col, "introuvable dans les donnees."))
      }
    }
  }

  final_n <- nrow(data)
  filters_applied$rows_before <- initial_n
  filters_applied$rows_after <- final_n
  filters_applied$rows_removed <- initial_n - final_n

  return(list(
    data = data,
    filters = filters_applied
  ))
}

