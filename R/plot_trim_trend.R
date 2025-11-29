#' Plot TRIM model trend
#'
#' @param model A TRIM model object
#'
#' @return A ggplot2 plot showing abundance trend over time
#' @export
#'
#' @examples
#'
plot_trim_trend <- function(model) {
  # Options
  base::options(scipen = 999)

  # Calcul de l'overall_model à partir du modèle rtrim
  overall_model <- rtrim::overall(model)

  # Données principales
  df_trendline <- rtrim::trendlines(overall_model)
  df_points <- tibble::tibble(
    year = overall_model$timept,
    estimate = overall_model$tt,
    ymin = overall_model$tt - overall_model$err,
    ymax = overall_model$tt + overall_model$err
  )

  # Filtrer les valeurs non-positives pour le log
  df_trendline <- dplyr::filter(df_trendline, lo > 0, hi > 0, value > 0)
  df_points <- dplyr::filter(df_points, estimate > 0, ymin > 0, ymax > 0)

  # Étendue y globale (log)
  y_range <- base::range(df_trendline$lo, df_trendline$hi, df_points$ymin, df_points$ymax, na.rm = TRUE)
  main_breaks <- scales::log_breaks()(y_range)
  minor_breaks <- main_breaks[-length(main_breaks)] * sqrt(main_breaks[-1] / main_breaks[-length(main_breaks)])

  # Graphique principal (log)
  main_plot <- ggplot2::ggplot() +
    ggplot2::geom_ribbon(data = df_trendline, ggplot2::aes(x = year, ymin = lo, ymax = hi), fill = "#5D7052", alpha = 0.3) +
    ggplot2::geom_line(data = df_trendline, ggplot2::aes(x = year, y = value), color = "#5D7052", size = 1.5) +
    ggplot2::geom_point(data = df_points, ggplot2::aes(x = year, y = estimate), color = "#A7001E", size = 3) +
    ggplot2::geom_line(data = df_points, ggplot2::aes(x = year, y = estimate), color = "#A7001E") +
    ggplot2::geom_errorbar(data = df_points, ggplot2::aes(x = year, ymin = ymin, ymax = ymax),
                           color = "#A7001E", width = 0.2, alpha = 0.7) +
    ggplot2::scale_y_log10(breaks = main_breaks, minor_breaks = minor_breaks) +
    ggplot2::theme_minimal() +
    ggplot2::labs(x = "Year", y = "Abundance (log scale)", title = "Abundance Over Time (log10)") +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5),
      axis.text = ggplot2::element_text(size = 12),
      axis.title = ggplot2::element_text(size = 14)
    )

  return(main_plot)
}
