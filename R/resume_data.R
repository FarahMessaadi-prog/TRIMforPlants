#' Resume data for a species with table and plots
#'
#' Generates a comprehensive summary of the time series data for a given species,
#' including statistics on missing data and visualization plots (log-transformed scatter plot and heatmap).
#'
# ... paramètres ...
#' @param donneessp Dataframe **prepared for TRIM** (\code{prepare_data_for_trim} output) for a given species.
#' @param colonnesite Character: column name containing site
#' @param colonneannee Character: column name containing year
#' @param colonneeffectif Character: column name containing abundance
#'
#' @return A **list** with three elements:
#' \itemize{
#'  \item \code{resume_table}: Data frame summarizing sites, years, and missing data percentages.
#'  \item \code{plot_points}: A \code{ggplot2} scatter plot of abundance (log10) over time.
#'  \item \code{plot_heatmap}: A \code{ggplot2} heatmap of abundance by site and year.
#' }
#' @export
#' @import ggplot2
#' @importFrom dplyr filter summarise group_by rowwise mutate pull desc left_join
#' @importFrom tidyr unnest
#' @importFrom scales pretty_breaks
#'
resume_data <- function(donneessp, colonnesite, colonneannee, colonneeffectif) {

  # --- Partie tableau résumé ---
  site_unique <- base::unique(donneessp[[colonnesite]])
  nb_site <- base::length(site_unique)
  site_list <- base::paste(site_unique, collapse = ";")
  first_year <- base::min(donneessp[[colonneannee]], na.rm = TRUE)
  last_year <- base::max(donneessp[[colonneannee]], na.rm = TRUE)
  seq_years <- first_year:last_year

  actual_years <- base::unique(donneessp[[colonneannee]])
  missing_years_absent <- base::setdiff(seq_years, actual_years)

  years_all_na <- dplyr::filter(
    dplyr::summarise(
      dplyr::group_by(donneessp, .data[[colonneannee]]),
      all_na = base::all(base::is.na(.data[[colonneeffectif]])),
      .groups = "drop"
    ),
    all_na
  ) |> dplyr::pull(.data[[colonneannee]])

  years_fully_missing <- base::union(missing_years_absent, years_all_na)
  nb_missing_years <- base::length(years_fully_missing)
  pct_missing_years <- nb_missing_years / base::length(seq_years) * 100

  site_year_complet <- dplyr::mutate(
    dplyr::rowwise(
      dplyr::mutate(
        dplyr::summarise(
          dplyr::group_by(donneessp, .data[[colonnesite]]),
          first_obs = base::min(.data[[colonneannee]], na.rm = TRUE),
          .groups = "drop"
        ),
        last_obs = last_year
      ),
      years = list(first_obs:last_obs)
    ),
    years = tidyr::unnest(years)
  )

  expected_combinations <- dplyr::rename(
    dplyr::select(site_year_complet, !!rlang::sym(colonnesite), years),
    !!colonneannee := years
  )

  joined <- dplyr::left_join(expected_combinations, donneessp, by = c(colonnesite, colonneannee))
  total_expected <- base::nrow(joined)
  total_missing <- base::sum(base::is.na(joined[[colonneeffectif]]))
  pct_missing_data <- total_missing / total_expected * 100

  result_donnees <- base::data.frame(
    nb_site = nb_site,
    site_list = site_list,
    first_year = first_year,
    last_year = last_year,
    duration = base::length(seq_years),
    pct_missing_years = pct_missing_years,
    pct_missing_data = pct_missing_data
  )

  # --- Plot 1 : scatter log10 ---
  plot_points <- ggplot2::ggplot(
    dplyr::mutate(
      donneessp,
      log_effectif = base::ifelse(.data[[colonneeffectif]] > 0, base::log10(.data[[colonneeffectif]]), NA_real_)
    ),
    ggplot2::aes_string(x = colonneannee, y = "log_effectif")
  ) +
    ggplot2::geom_point(size = 2, color = "#5D7052", alpha = 0.5) +
    ggplot2::scale_y_continuous(
      breaks = base::log10(c(1, 10, 100, 1000, 10000)),
      labels = c("1", "10", "100", "1000", "10000")
    ) +
    ggplot2::theme_minimal() +
    ggplot2::labs(x = "Year", y = "Abundance (log10)", title = "Observed data") +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5),
      plot.title = ggplot2::element_text(hjust = 0.5, size = 14),
      axis.title = ggplot2::element_text(size = 14),
      axis.text = ggplot2::element_text(size = 12)
    )

  # --- Plot 2 : heatmap ---
  full_grid <- base::expand.grid(
    site = base::unique(donneessp[[colonnesite]]),
    year = base::unique(donneessp[[colonneannee]])
  )
  base::names(full_grid) <- c(colonnesite, colonneannee)

  data_heatmap <- dplyr::mutate(
    dplyr::left_join(full_grid, donneessp, by = c(colonnesite, colonneannee)),
    log_effectif = base::ifelse(.data[[colonneeffectif]] > 0, base::log10(.data[[colonneeffectif]]), NA_real_),
    is_zero = .data[[colonneeffectif]] == 0
  )

  site_order <- dplyr::pull(
    dplyr::arrange(
      dplyr::summarise(
        dplyr::group_by(dplyr::filter(data_heatmap, .data[[colonneeffectif]] > 0), .data[[colonnesite]]),
        first_pos_year = base::min(.data[[colonneannee]], na.rm = TRUE),
        .groups = "drop"
      ),
      dplyr::desc(first_pos_year)
    ),
    .data[[colonnesite]]
  )

  data_heatmap[[colonnesite]] <- base::factor(data_heatmap[[colonnesite]], levels = site_order)

  plot_heatmap <- ggplot2::ggplot() +
    ggplot2::geom_tile(
      data = dplyr::filter(data_heatmap, !base::is.na(log_effectif)),
      ggplot2::aes_string(x = colonneannee, y = colonnesite, fill = "log_effectif"),
      color = "white", linewidth = 0.3, width = 1
    ) +
    ggplot2::geom_tile(
      data = dplyr::filter(data_heatmap, is_zero),
      ggplot2::aes_string(x = colonneannee, y = colonnesite),
      fill = "gray90", color = "white", linewidth = 0.3, width = 1
    ) +
    ggplot2::geom_text(
      data = dplyr::filter(data_heatmap, is_zero),
      ggplot2::aes_string(x = colonneannee, y = colonnesite, label = "\"-\""),
      color = "black", size = 2.5
    ) +
    ggplot2::scale_fill_gradientn(
      colours = c("#CAD4C4", "#8AA17D", "#313B2B"),
      na.value = "gray80",
      name = "Abundance",
      breaks = base::log10(c(1, 10, 100, 200)),
      labels = c("1", "10", "100", "200")
    ) +
    ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 10)(data_heatmap[[colonneannee]])) +
    ggplot2::theme_minimal() +
    ggplot2::labs(x = "Year", y = "Site", title = "Observed abundance by year and site") +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5),
      axis.text = ggplot2::element_text(size = 12),
      axis.title = ggplot2::element_text(size = 14),
      plot.title = ggplot2::element_text(size = 14, hjust = 0.5),
      legend.position = "right"
    )

  return(list(
    resume_table = result_donnees,
    plot_points = plot_points,
    plot_heatmap = plot_heatmap
  ))
}

