#' Create and standardize a clean dataset
#'
#' This function cleans, reformats, and standardizes a raw biodiversity dataset.
#' It:
#' \itemize{
#'   \item Maps raw column names to standard names.
#'   \item Computes standardized abundance values (`effectif`) based on codes or indications.
#'   \item Standardizes origin codes (e.g., 'X' becomes NA).
#'   \item Harmonizes taxon names based on synonym groups (if provided).
#' }
#'
#' @param df_raw A dataframe containing raw biodiversity data.
#' @param columns_map A named list mapping standard column names to actual column names in the raw data.
#' @param interval_table A dataframe describing abundance code intervals (e.g., \code{abundance_interval_boundaries}).
#' @param taxon_synonyms Optional list of synonym groups for taxon names. Each element is a character vector where the first element is the **canonical name**.
#'
#' @return A **tibble** (dataframe) cleaned and standardized with standardized column names.
#' @export
#' @importFrom tibble tibble
#' @importFrom dplyr case_when
#' @examples
#' \dontrun{
#' df_clean <- create_clean_data(
#'   df_raw = my_raw_data,
#'   columns_map = columns_map,
#'   interval_table = abundance_interval_boundaries,
#'   taxon_synonyms = list(c("Panthera leo", "Felis leo"))
#' )
#' }
#'
create_clean_data <- function(df_raw,
                              columns_map,
                              interval_table,
                              taxon_synonyms = NULL) {

  base::message("Création du jeu de données nettoyé et standardisé...")

  # --- Helper: retrieve mapped column safely
  get_col <- function(key) {
    colname <- columns_map[[key]]
    if (!base::is.null(colname) && colname %in% base::names(df_raw)) {
      return(df_raw[[colname]])
    } else {
      return(NULL)
    }
  }

  # --- Créer un tibble vide avec bon nombre de lignes
  df_clean <- tibble::tibble(.rows = nrow(df_raw))

  possible_cols <- c(
    "obs_id", "taxon", "observer", "day", "month", "year",
    "canton", "commune", "x", "y", "precision", "xy_type",
    "origine", "presence_absence", "unity",
    "abundance_code", "abundance_indication"
  )

  # --- Ajouter les colonnes disponibles
  for (col in possible_cols) {
    value <- get_col(col)
    if (!base::is.null(value)) {
      df_clean[[col]] <- value
    }
  }

  # --- Calcul de l'effectif
  presence <- get_col("presence_absence")
  abundance_val <- get_col("abundance_indication")
  abundance_code <- get_col("abundance_code")

  if (!base::is.null(presence) || !base::is.null(abundance_val) || !base::is.null(abundance_code)) {

    get_effectif_from_code <- function(code) {
      row <- interval_table[as.character(interval_table$code) == as.character(code), , drop = FALSE]
      if (nrow(row) == 0) {
        return(NA_character_)
      } else {
        return(as.character(row$start[1]))  # borne inférieure
      }
    }

    df_clean$effectif <- dplyr::case_when(
      !base::is.null(presence) & presence %in% c("N", "I", "P", "-") ~ "0",
      !base::is.null(abundance_val) & !base::is.na(abundance_val) ~ as.character(abundance_val),
      !base::is.null(abundance_code) & !base::is.na(abundance_code) ~ base::as.character(base::mapply(get_effectif_from_code, abundance_code)),
      TRUE ~ NA_character_
    )
  }

  # --- Standardiser la colonne "origine"
  if ("origine" %in% base::names(df_clean)) {
    df_clean$origine <- base::ifelse(df_clean$origine == "X", NA, df_clean$origine)
  }

  # --- Standardiser les noms taxonomiques (si synonymes fournis)
  if (!is.null(taxon_synonyms) && "taxon" %in% names(df_clean)) {
    for (syn_group in taxon_synonyms) {
      canonical_name <- syn_group[1]
      df_clean$taxon[df_clean$taxon %in% syn_group] <- canonical_name
    }
  }

  base::message("Nettoyage termine")

  return(df_clean)
}
