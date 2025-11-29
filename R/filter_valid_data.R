#' Filter valid data from an InfoFlora-like dataset
#'
#' @param data A dataframe
#' @param presence_col Name of the column containing presence/absence info (default: "presence_absence")
#' @param abundance_indication_col Name of the column with abundance indications (default: "abundance_indication")
#' @param abundance_code_col Name of the column with abundance codes (default: "abundance_code")
#' @param unity_col Name of the column with the unit info (default: "unity")
#' @param absence_values Vector of values considered as absence (default: c("N", "I", "P", "-"))
#' @param colors Named character vector of colors for the categories ('Usable', 'Potentially usable', 'Unusable')
#'
#' @return A **list** with:
#' \itemize{
#'  \item \code{data}: Filtered dataset (includes 'Usable' and 'Potentially usable' records).
#'  \item \code{percent_usable}: Percentage of fully usable records.
#'  \item \code{percent_potential}: Percentage of potentially usable records.
#'  \item \code{percent_unusable}: Percentage of unusable records.
#'  \item \code{plot}: A \code{ggplot2} object (pie chart) visualizing the breakdown.
#' }
#' @export
#'
#' @examples
#' # Cas avec noms standards
#' filter_valid_data(df)
#'
#' # Cas avec colonnes personnalisees
#' filter_valid_data(df,
#'                   presence_col = "presence",
#'                   abundance_indication_col = "abundance_text",
#'                   abundance_code_col = "ab_code",
#'                   unity_col = "unit")
#'
filter_valid_data <- function(data,
                              presence_col = "presence_absence",
                              abundance_indication_col = "abundance_indication",
                              abundance_code_col = "abundance_code",
                              unity_col = "unity",
                              absence_values = c("N", "I", "P", "-"),
                              colors = c("Usable" = "#5D7052",
                                         "Potentially usable" = "#FCD581",
                                         "Unusable" = "#A7001E")) {
  # Vérification des colonnes
  required_cols <- c(presence_col, abundance_indication_col, abundance_code_col, unity_col)
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Les colonnes suivantes sont manquantes dans les données : ",
         paste(missing_cols, collapse = ", "))
  }

  # Catégorisation des lignes
  sure <- data[[presence_col]] %in% absence_values |
    (!is.na(data[[unity_col]]) & !is.na(data[[abundance_code_col]])) |
    suppressWarnings(!is.na(as.numeric(data[[abundance_indication_col]])))

  maybe <- !sure & !is.na(data[[abundance_indication_col]])
  unusable <- !sure & !maybe

  # Calcul des statistiques
  total <- nrow(data)
  n_sure <- sum(sure, na.rm = TRUE)
  n_maybe <- sum(maybe, na.rm = TRUE)
  n_unusable <- sum(unusable, na.rm = TRUE)

  percent_sure <- round(n_sure / total * 100, 1)
  percent_maybe <- round(n_maybe / total * 100, 1)
  percent_unusable <- round(n_unusable / total * 100, 1)

  # Préparation des données pour le graphique
  df_plot <- data.frame(
    Category = factor(c("Usable", "Potentially usable", "Unusable"),
                      levels = c("Usable", "Potentially usable", "Unusable")),
    Percentage = c(percent_sure, percent_maybe, percent_unusable)
  ) |>
    dplyr::mutate(label = paste0(Percentage, "%"))

  # Camembert (ggplot2)
  pie_chart <- ggplot2::ggplot(df_plot, ggplot2::aes(x = "", y = Percentage, fill = Category)) +
    ggplot2::geom_bar(stat = "identity", width = 1, color = "white") +
    ggplot2::coord_polar("y", start = 0) +
    ggplot2::theme_void() +
    ggplot2::labs(title = "Data usability breakdown") +
    ggplot2::geom_text(ggplot2::aes(label = label),
                       position = ggplot2::position_stack(vjust = 0.5),
                       size = 5, color = "black") +
    ggplot2::scale_fill_manual(values = colors)

  # Filtrage final : on garde les données "sûres" ou "potentielles"
  data_filtered <- data[sure | maybe, ]

  # Retour du résultat
  return(list(
    data = data_filtered,
    percent_usable = percent_sure,
    percent_potential = percent_maybe,
    percent_unusable = percent_unusable,
    plot = pie_chart
  ))
}
