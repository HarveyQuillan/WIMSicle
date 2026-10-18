#' Plot two determinands on dual axes
#'
#' Creates a dual-axis time series plot for two determinands contained
#' within a WIMS dataset.
#'
#' @param data Data frame returned by `get_site()` or `get_sites()`.
#' @param determinand_1 Name of the first determinand.
#' @param determinand_2 Name of the second determinand.
#' @param colour_1 Colour for first determinand.
#' @param colour_2 Colour for second determinand.
#' @param scale_quantile Quantile used to align the two axes.
#' @param y1_label Optional primary y-axis label.
#' @param y2_label Optional secondary y-axis label.
#' @param date_breaks Date break interval passed to
#'   `ggplot2::scale_x_datetime()`.
#' @param plot_theme ggplot theme object.
#'
#' @return A ggplot object.
#'
#' @examples
#' data <- get_site(
#'   sample_point_id = "TH-PCNR0051",
#'   from = "2010-01-01",
#'   to = "2015-01-01",
#'   determinands = c("Nitrate", "Chlorophyll")
#' )
#'
#' plot_dual_determinand(
#'   data,
#'   determinand_1 = "Nitrate",
#'   determinand_2 = "Chlorophyll"
#' )
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
