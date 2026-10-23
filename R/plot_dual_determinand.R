#' Plot two determinands on dual axes
#'
#' Creates a dual-axis time series plot for two determinands contained
#' within a WIMS dataset.
#'
#' The function automatically calculates a scaling factor using a
#' specified quantile of each determinand's distribution, allowing two
#' determinands with different units or magnitudes to be displayed on
#' the same chart with independent y-axes.
#'
#' @param data Data frame returned by `get_site()` or `get_sites()`.
#' @param determinand_1 Name of the first determinand.
#' @param determinand_2 Name of the second determinand.
#' @param colour_1 Colour used for the first determinand.
#' @param colour_2 Colour used for the second determinand.
#' @param scale_quantile Numeric value between 0 and 1 used to calculate
#'   the scaling factor between the two axes. Defaults to `0.95`.
#' @param y1_label Optional label for the primary y-axis. If `NULL`,
#'   a label is generated automatically from the determinand name and
#'   unit.
#' @param y2_label Optional label for the secondary y-axis. If `NULL`,
#'   a label is generated automatically from the determinand name and
#'   unit.
#' @param date_breaks Character string passed to
#'   `ggplot2::scale_x_datetime()` controlling x-axis date breaks.
#' @param plot_theme A ggplot theme object. Defaults to
#'   `ggplot2::theme_bw()`.
#'
#' @return
#' A `ggplot2` object showing both determinands through time using
#' separate y-axes.
#'
#' The plot:
#' \itemize{
#'   \item displays observations as points;
#'   \item uses a primary axis for the first determinand;
#'   \item uses a secondary axis for the second determinand;
#'   \item automatically derives axis labels from available units when
#'         not supplied.
#' }
#'
#' @details
#' The scaling factor is calculated from the selected quantile of each
#' determinand's distribution:
#'
#' \deqn{
#' scale\ factor = q_1 / q_2
#' }
#'
#' where \eqn{q_1} and \eqn{q_2} are the chosen quantiles of the first
#' and second determinands respectively.
#'
#' Using a quantile rather than the maximum value helps reduce the
#' influence of extreme outliers.
#'
#' @examples
#' data <- get_site(
#'   sample_point_id = "TH-PCNR0051",
#'   from = "2010-01-01",
#'   to = "2012-01-01",
#'   determinands = c(
#'     "Nitrate as N",
#'     "Chlorophyll : Acetone Extract"
#'   )
#' )
#'
#' plot_dual_determinand(
#'   data,
#'   determinand_1 = "Nitrate as N",
#'   determinand_2 = "Chlorophyll : Acetone Extract"
#' )
#'
#' plot_dual_determinand(
#'   data,
#'   determinand_1 = "Nitrate as N",
#'   determinand_2 = "Chlorophyll : Acetone Extract",
#'   scale_quantile = 0.9
#' )
#'
#' @seealso
#' \code{\link{get_site}},
#' \code{\link{get_sites}}
#'
#' @export
plot_dual_determinand <- function(
    data,
    determinand_1,
    determinand_2,
    colour_1 = "red",
    colour_2 = "darkgreen",
    scale_quantile = 0.95,
    y1_label = NULL,
    y2_label = NULL,
    date_breaks = "3 months",
    plot_theme = ggplot2::theme_bw()
) {

  if (scale_quantile <= 0 ||
      scale_quantile > 1) {
    stop(
      "'scale_quantile' must be greater than 0 and less than or equal to 1"
    )
  }

  plot_data <- data |>
    dplyr::filter(
      !is.na(result),
      determinand %in% c(
        determinand_1,
        determinand_2
      )
    )

  det_names <- unique(plot_data$determinand)

  if (length(det_names) != 2) {
    stop(
      "Exactly two determinands must be present in the supplied data."
    )
  }

  det1 <- plot_data |>
    dplyr::filter(
      determinand == det_names[1]
    ) |>
    dplyr::pull(result)

  det2 <- plot_data |>
    dplyr::filter(
      determinand == det_names[2]
    ) |>
    dplyr::pull(result)

  q1 <- stats::quantile(
    det1,
    probs = scale_quantile,
    na.rm = TRUE
  )

  q2 <- stats::quantile(
    det2,
    probs = scale_quantile,
    na.rm = TRUE
  )

  if (q2 == 0) {
    stop(
      "Cannot calculate scale factor because the second determinand quantile is zero."
    )
  }

  scale_factor <- q1 / q2

  unit_1 <- plot_data |>
    dplyr::filter(
      determinand == det_names[1]
    ) |>
    dplyr::pull(unit) |>
    unique()

  unit_2 <- plot_data |>
    dplyr::filter(
      determinand == det_names[2]
    ) |>
    dplyr::pull(unit) |>
    unique()

  unit_1 <- unit_1[!is.na(unit_1)][1]
  unit_2 <- unit_2[!is.na(unit_2)][1]

  if (is.null(y1_label)) {
    y1_label <- paste0(
      det_names[1],
      " (",
      unit_1,
      ")"
    )
  }

  if (is.null(y2_label)) {
    y2_label <- paste0(
      det_names[2],
      " (",
      unit_2,
      ")"
    )
  }

  ggplot2::ggplot() +

    ggplot2::geom_point(
      data = dplyr::filter(
        plot_data,
        determinand == det_names[1]
      ),
      ggplot2::aes(
        sample_date,
        result,
        colour = det_names[1]
      ),
      size = 4,
      alpha = 0.5
    ) +

    ggplot2::geom_point(
      data = dplyr::filter(
        plot_data,
        determinand == det_names[2]
      ),
      ggplot2::aes(
        sample_date,
        result * scale_factor,
        colour = det_names[2]
      ),
      size = 4,
      alpha = 0.5
    ) +

    ggplot2::scale_colour_manual(
      values = c(
        colour_1,
        colour_2
      )
    ) +

    ggplot2::scale_y_continuous(
      name = y1_label,
      sec.axis = ggplot2::sec_axis(
        ~ . / scale_factor,
        name = y2_label
      )
    ) +

    ggplot2::scale_x_datetime(
      date_breaks = date_breaks,
      date_labels = "%b\n%Y"
    ) +

    ggplot2::labs(
      x = NULL,
      colour = NULL
    ) +

    plot_theme +

    ggplot2::theme(
      axis.text.x = ggplot2::element_text(
        angle = 45,
        hjust = 1
      )
    )

}
