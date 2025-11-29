#' Apply multiple TRIM models automatically to a dataset
#'
#' This function systematically applies different TRIM model configurations (changepoints, covariates)
#' to a prepared dataset, using the **safe wrappers** \code{\link{safe_trim}} and \code{\link{safe_wald}}.
#' It tests various models, calculates statistics, and identifies valid/significant models based on internal criteria.
#'
#' @param dataset A data.frame prepared for TRIM (output of \code{\link{prepare_data_for_trim}}).
#' @param site_column Character. Name of the column containing site names. Default: "commune".
#' @param year_column Character. Name of the column containing the year. Default: "year".
#' @param effectiv_column Character. Name of the column containing abundance values. Default: "effectif".
#' @param unity_column Character. Name of the column identifying the unit (e.g., species). Default: "unity".
#' @param covariables List of character. Optional columns to include as covariates. Default: list(NULL).
#' @param cp_candidates Numeric vector. Candidate years for changepoints. Default: NULL.
#' @param n_cps_max Numeric. Maximum number of changepoints to test per model. Default: NULL.
#'
#' @details
#' The function uses standard criteria to validate models: overdispersion (< 3),
#' serial correlation (< 0.4), likelihood ratio (\code{p_LR >= 0.05}), chi-square tests (\code{p_Chi2 >= 0.05}),
#' and slope standard error (\code{slope_se <= 0.02}).
#'
#' @return A list where each element corresponds to a unit (level of `unity_column`) containing:
#' \itemize{
#'   \item \code{models} : list of all generated TRIM models.
#'   \item \code{rtrim_result_table} : summary table of all model results.
#'   \item \code{valid_results_table} : subset of models considered valid.
#'   \item \code{significant_results_table} : subset of models considered significant.
#' }
#'
#' @export
#' @importFrom rtrim gof overall
#' @importFrom base tryCatch summary message
#' @importFrom utils combn
#'
#' @examples
#' # Example using default column names
#' results_default <- rtrim_multi_application(
#'   dataset = my_data
#' )
#'
#' # Example with custom column names
#' results_custom <- rtrim_multi_application(
#'   dataset = my_data,
#'   site_column = "site_name",
#'   year_column = "survey_year",
#'   effectiv_column = "abundance",
#'   unity_column = "species",
#'   covariables = list("habitat_type", "temperature"),
#'   cp_candidates = c(2005, 2010, 2015),
#'   n_cps_max = 2
#' )
#'
#'
rtrim_multi_application <- function(dataset, site_column = "commune",
                                    year_column = "year",
                                    effectiv_column = "effectif",
                                    unity_column = "unity",
                                    covariables = list(NULL),
                                    cp_candidates = NULL,
                                    n_cps_max = NULL) {

  message("Debut de rtrim_multi_application")

  # Conversion en facteur avec base::factor pour viter d'utiliser dplyr::mutate
  dataset[[unity_column]] <- base::as.factor(dataset[[unity_column]])

  result_per_unity <- list()

  # --- Fonction interne pour collecter les resultats rtrim ---
  collect_results <- function(dataset, model, site_column, year_column, model_name) {
    result <- base::data.frame(
      model_name = model_name,
      nb_site = base::length(base::unique(dataset[[site_column]])),
      duration = base::max(dataset[[year_column]]) - base::min(dataset[[year_column]]) + 1,
      overdisp = NA,
      serialcor = NA,
      chi_sq = NA,
      df_chi_sq = NA,
      p_chi_sq = NA,
      LR = NA,
      df_LR = NA,
      p_LR = NA,
      AIC = NA,
      slope_add = NA,
      slope_se = NA,
      meaning = NA
    )

    if (!base::is.null(model)) {
      gof_stats     <- base::tryCatch(rtrim::gof(model), error = function(e) NULL)
      overall_stats <- base::tryCatch(rtrim::overall(model), error = function(e) NULL)
      summary_stats <- base::tryCatch(base::summary(model), error = function(e) NULL)

      if (!base::is.null(overall_stats)) {
        result$slope_add <- overall_stats$slope$add
        result$slope_se  <- overall_stats$slope$se_add
        result$meaning   <- overall_stats$slope$meaning
      }

      if (!base::is.null(summary_stats)) {
        result$chi_sq    <- summary_stats$gof$chi2$chi2
        result$df_chi_sq <- summary_stats$gof$chi2$df
        result$p_chi_sq  <- summary_stats$gof$chi2$p
        result$LR        <- summary_stats$gof$LR$LR
        result$df_LR     <- summary_stats$gof$LR$df
        result$p_LR      <- summary_stats$gof$LR$p
        result$AIC       <- summary_stats$gof$AIC
        result$overdisp  <- summary_stats$overdispersion
        result$serialcor <- summary_stats$serialcorrelation
      }
    }

    return(result)
  }

  # --- Fonction interne pour valider les modeles ---
  validate_models <- function(models, dataset, site_column, year_column) {
    valid_models <- list()
    significant_models <- list()

    for (name in base::names(models)) {
      model <- models[[name]]
      if (base::is.null(model)) next

      summary_stats <- base::tryCatch(base::summary(model), error = function(e) NULL)
      overall_stats <- base::tryCatch(rtrim::overall(model), error = function(e) NULL)
      gof_stats     <- base::tryCatch(rtrim::gof(model), error = function(e) NULL)
      if (base::is.null(summary_stats) || base::is.null(overall_stats) || base::is.null(gof_stats)) next

      overdisp <- summary_stats$overdispersion
      serialcor <- summary_stats$serialcorrelation
      slope_se  <- overall_stats$slope$se_add
      p_LR   <- gof_stats$LR$p
      p_Chi2 <- gof_stats$chi2$p

      if (base::any(base::is.na(c(overdisp, serialcor, slope_se, p_LR, p_Chi2)))) next

      # Validation
      is_valid <- FALSE
      if (overdisp < 3 && serialcor < 0.4) {
        is_valid <- p_LR >= 0.05 && p_Chi2 >= 0.05
      } else {
        is_valid <- slope_se <= 0.02
      }
      if (!is_valid) next

      valid_models[[name]] <- model

      wald_res <- base::tryCatch(rtrim::wald(model), error = function(e) NULL)
      if (!base::is.null(wald_res) && !base::is.null(wald_res$dslope$p)) {
        p_vals <- wald_res$dslope$p
        if (base::all(!base::is.na(p_vals) & p_vals <= 0.20)) {
          significant_models[[name]] <- model
        }
      }
    }
    return(list(valid_models = valid_models, significant_models = significant_models))
  }

  # --- Boucle par unite ---
  for (u in base::levels(dataset[[unity_column]])) {
    df <- dataset[dataset[[unity_column]] == u, ]
    message("\n============================")
    message("Traitement de l'unite : ", u)

    if (base::nrow(df) == 0 || base::all(base::is.na(df[[effectiv_column]]))) {
      warning("Donnees vides ou sans effectif pour l'unite ", u)
      result_per_unity[[u]] <- NULL
      next
    }

    model_history <- list()
    rtrim_result <- base::data.frame()
    model_counter <- 1

    # Changepoints initiaux
    list_changepoints <- list("auto", NULL)
    if (!base::is.null(cp_candidates) && base::length(cp_candidates) > 0) {
      if (base::is.null(n_cps_max)) n_cps_max <- base::length(cp_candidates)
      for (n in 1:base::min(n_cps_max, base::length(cp_candidates))) {
        combs <- utils::combn(cp_candidates, n, simplify = FALSE)
        list_changepoints <- c(list_changepoints, combs)
      }
    }

    # Covariables
    covariables_to_use <- covariables
    if (base::length(covariables) > 1) {
      all_covars <- base::unique(base::unlist(covariables))
      cov_combs <- list()
      for (k in 1:base::length(all_covars)) {
        combs_cov <- utils::combn(all_covars, k, simplify = FALSE)
        cov_combs <- c(cov_combs, combs_cov)
      }
      covariables_to_use <- cov_combs
    }

    # Boucle CP × covars × modeles
    for (cp in list_changepoints) {
      for (covars in covariables_to_use) {
        for (model_type_iter in c(2, 3)) {
          rhs_terms <- c(site_column, year_column)
          if (!base::is.null(covars) && base::length(covars) > 0) {
            rhs_terms <- c(rhs_terms, covars)
          }
          formula_str <- base::paste(effectiv_column, "~", base::paste(rhs_terms, collapse = " + "))
          formula <- stats::as.formula(formula_str)

          cp_label <- if (base::is.null(cp)) "no_cp" else if (identical(cp, "auto")) "cp_auto" else base::paste0("cp_", base::paste(cp, collapse = "-"))
          covar_label <- if (base::is.null(covars) || base::length(covars) == 0) "no_covars" else base::paste0("covars_", base::paste(covars, collapse = "-"))
          model_name <- base::paste0("model_", model_counter, "_", cp_label, "_", covar_label, "_m", model_type_iter)

          new_model <- base::tryCatch({
            if (base::is.null(cp)) {
              safe_trim(formula, data = df, model = model_type_iter, serialcor = TRUE, overdisp = TRUE)
            } else {
              safe_trim(formula, data = df, model = model_type_iter, serialcor = TRUE, overdisp = TRUE, changepoint = cp)
            }
          }, error = function(e) {
            warning("Erreur safe_trim pour ", model_name, " : ", e$message)
            return(NULL)
          })

          model_history[[model_name]] <- new_model

          if (!base::is.null(new_model)) {
            result_row <- base::tryCatch({
              collect_results(df, new_model, site_column, year_column, model_name)
            }, error = function(e) NULL)

            if (!base::is.null(result_row)) {
              rtrim_result <- base::rbind(rtrim_result, result_row)
            }
          }

          model_counter <- model_counter + 1
        }
      }
    }

    # Validation
    validated <- base::tryCatch({
      validate_models(model_history, df, site_column, year_column)
    }, error = function(e) {
      warning("Erreur validate_models : ", e$message)
      return(list(valid_models = list(), significant_models = list()))
    })

    valid_results_table <- rtrim_result[rtrim_result$model_name %in% base::names(validated$valid_models), ]
    significant_results_table <- rtrim_result[rtrim_result$model_name %in% base::names(validated$significant_models), ]

    # Resultats stockes
    result_per_unity[[u]] <- list(
      models = model_history,
      rtrim_result_table = rtrim_result,
      valid_results_table = valid_results_table,
      significant_results_table = significant_results_table
    )
  }

  message("\nTraitement termine pour toutes les unites.")
  return(result_per_unity)
}
