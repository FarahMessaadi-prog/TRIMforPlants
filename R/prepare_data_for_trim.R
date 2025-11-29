#' Prepare data for TRIM analysis
#'
#' Aggregates abundance data (mean \code{effectif} per site/year/taxon/unit combination)
#' and fills missing years to create a continuous time series suitable for TRIM analysis.
#'
#' @param df A clean dataframe with abundance data.
#' @param taxon_col Character of the taxon column name (default = "taxon")
#' @param site_col Character of the site column name (default = "commune")
#' @param year_col Character of the year column name (default = "year")
#' @param unit_col Character of the unit column name (default = "unity")
#' @param effectiv_col Character of the abundance column name (default = "effectif")
#' @param extra_col Optional vector of extra column names to keep (covariables)
#' @param file_out Optional character path to save the output table (TSV format)
#'
#' @return A **dataframe** prepared for TRIM, with one row per Site/Year combination,
#'         including aggregated abundance (\code{effectiv_col}).
#' @export
#' @importFrom dplyr group_by across all_of summarise mutate
#' @importFrom utils write.table
#'
#' @examples
#' # df_trim <- prepare_data_for_trim(df_clean)
#' # df_trim <- prepare_data_for_trim(df_clean, taxon_col = "species_name", site_col = "station")
#'
prepare_data_for_trim <- function(df,
                                  taxon_col = "taxon",
                                  site_col = "commune",
                                  year_col = "year",
                                  unit_col = "unity",
                                  effectiv_col = "effectif",
                                  extra_col = NULL,
                                  file_out = NULL) {
  # --- Vérification des colonnes ---
  required_cols <- c(taxon_col, site_col, year_col, unit_col, effectiv_col)
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop("Colonnes manquantes dans le jeu de donnees : ", paste(missing_cols, collapse = ", "))
  }

  # --- Forcer effectif en numérique ---
  df[[effectiv_col]] <- suppressWarnings(as.numeric(df[[effectiv_col]]))

  # --- Étape 0 : Dupliquer les lignes avec effectif = 0 ---
  df_expanded <- do.call(rbind, lapply(split(df, df[[taxon_col]]), function(taxon_df) {
    unities <- unique(taxon_df[[unit_col]])
    zero_rows <- taxon_df[taxon_df[[effectiv_col]] == 0 & !is.na(taxon_df[[effectiv_col]]), ]

    if (nrow(zero_rows) > 0) {
      extra_rows <- do.call(rbind, lapply(seq_len(nrow(zero_rows)), function(i) {
        row <- zero_rows[i, ]
        missing_unities <- setdiff(unities, row[[unit_col]])
        if (length(missing_unities) > 0) {
          do.call(rbind, lapply(missing_unities, function(u) {
            row_copy <- row
            row_copy[[unit_col]] <- u
            row_copy
          }))
        } else NULL
      }))
      if (!is.null(extra_rows)) {
        taxon_df <- rbind(taxon_df, extra_rows)
      }
    }
    taxon_df
  }))

  # --- Étape 1 : Agrégation (moyenne des effectifs en ignorant les NA) ---
  group_cols <- c(year_col, site_col, taxon_col, unit_col)
  if (!is.null(extra_col)) group_cols <- c(group_cols, extra_col)

  df_agg <- df_expanded |>
    dplyr::group_by(dplyr::across(all_of(group_cols))) |>
    dplyr::summarise(
      !!effectiv_col := if (all(is.na(.data[[effectiv_col]]))) NA_real_
      else round(mean(.data[[effectiv_col]], na.rm = TRUE), 0),
      .groups = "drop"
    )


  # --- Étape 2 : Compléter les années manquantes ---
  group_vars <- c(taxon_col, unit_col, site_col)
  if (!is.null(extra_col)) group_vars <- c(group_vars, extra_col)

  res <- do.call(rbind, lapply(split(df_agg, df_agg[group_vars]), function(sub) {
    if (all(is.na(sub[[year_col]]))) return(sub)

    min_year <- suppressWarnings(min(sub[[year_col]], na.rm = TRUE))
    max_year <- suppressWarnings(max(sub[[year_col]], na.rm = TRUE))
    if (!is.finite(min_year) || !is.finite(max_year)) return(sub)

    yrs <- seq(min_year, max_year)
    blank <- data.frame(year = yrs)
    names(blank)[1] <- year_col
    for (col in setdiff(group_vars, year_col)) {
      blank[[col]] <- unique(sub[[col]])
    }
    merge(blank, sub, by = c(group_vars, year_col), all.x = TRUE)
  }))

  # Nettoyage final
  if (effectiv_col %in% names(res)) {
    res[[effectiv_col]][is.infinite(res[[effectiv_col]])] <- NA
  }
  rownames(res) <- NULL

  # --- Étape 3 : Sauvegarde optionnelle ---
  if (!is.null(file_out)) {
    utils::write.table(res, file = file_out, sep = "\t", row.names = FALSE, quote = FALSE)
    message("Fichier sauvegarde : ", file_out)
  }

  return(res)
}



