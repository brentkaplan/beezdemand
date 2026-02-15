#' beezdemand Plot Theme and Color Palette
#'
#' @description
#' Central plotting style API for beezdemand, with a modern default based on the
#' codedbx "Refined Contemporary" brand and an APA alternative.
#'
#' @name plot-theme
NULL

# Brand colors (codedbx "Refined Contemporary")
beezdemand_colors <- c(
  primary = "#2B4560",
  secondary = "#A25F5F",
  accent = "#5D8AA8",
  cat4 = "#7D9C7F",
  cat5 = "#8A7BA3",
  cat6 = "#B08C6A",
  dark = "#1E3242",
  light = "#F2F2F2"
)

#' beezdemand Color Palette
#'
#' @return Named vector of brand colors.
#' @examples
#' palette_beezdemand()
#' @export
palette_beezdemand <- function() {
  beezdemand_colors
}

#' beezdemand Color Scale (Discrete)
#'
#' @param ... Additional arguments passed to \code{ggplot2::scale_color_manual}.
#' @return A ggplot2 discrete color scale.
#' @examples
#' \donttest{
#' library(ggplot2)
#' ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
#'   geom_point() +
#'   scale_color_beezdemand()
#' }
#' @export
scale_color_beezdemand <- function(...) {
  ggplot2::scale_color_manual(
    values = unname(beezdemand_colors[c("primary", "secondary", "accent", "cat4", "cat5", "cat6")]),
    ...
  )
}

#' beezdemand Fill Scale (Discrete)
#'
#' @param ... Additional arguments passed to \code{ggplot2::scale_fill_manual}.
#' @return A ggplot2 discrete fill scale.
#' @examples
#' \donttest{
#' library(ggplot2)
#' ggplot(iris, aes(Species, Sepal.Length, fill = Species)) +
#'   geom_boxplot() +
#'   scale_fill_beezdemand()
#' }
#' @export
scale_fill_beezdemand <- function(...) {
  ggplot2::scale_fill_manual(
    values = unname(beezdemand_colors[c("primary", "secondary", "accent", "cat4", "cat5", "cat6")]),
    ...
  )
}

#' beezdemand Plot Theme
#'
#' @param style Character. One of \code{"modern"} or \code{"apa"}.
#' @param base_size Base font size (default: 11).
#' @param base_family Base font family (default: "sans").
#' @return A ggplot2 theme object.
#' @examples
#' \donttest{
#' library(ggplot2)
#' ggplot(iris, aes(Sepal.Length, Sepal.Width)) +
#'   geom_point() +
#'   theme_beezdemand()
#' }
#' @export
theme_beezdemand <- function(style = c("modern", "apa"),
                             base_size = 11,
                             base_family = "sans") {
  style <- match.arg(style)

  if (style == "apa") {
    return(theme_apa())
  }

  ggplot2::theme_minimal(
    base_size = base_size,
    base_family = base_family
  ) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      panel.background = ggplot2::element_rect(fill = "white", color = NA),
      panel.grid.major = ggplot2::element_line(color = "#E0E0E0", linewidth = 0.3),
      panel.grid.minor = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(color = beezdemand_colors["dark"], linewidth = 0.5),
      axis.ticks = ggplot2::element_line(color = beezdemand_colors["dark"], linewidth = 0.4),
      axis.text = ggplot2::element_text(color = beezdemand_colors["dark"]),
      axis.title = ggplot2::element_text(color = beezdemand_colors["dark"], face = "bold"),
      plot.title = ggplot2::element_text(
        color = beezdemand_colors["primary"],
        face = "bold",
        hjust = 0,
        margin = ggplot2::margin(b = base_size * 0.5)
      ),
      plot.subtitle = ggplot2::element_text(
        color = beezdemand_colors["dark"],
        hjust = 0,
        margin = ggplot2::margin(b = base_size * 0.5)
      ),
      plot.caption = ggplot2::element_text(
        color = "#666666",
        hjust = 1,
        margin = ggplot2::margin(t = base_size * 0.5)
      ),
      legend.background = ggplot2::element_rect(fill = "white", color = NA),
      legend.key = ggplot2::element_rect(fill = "white", color = NA),
      legend.text = ggplot2::element_text(color = beezdemand_colors["dark"]),
      legend.title = ggplot2::element_text(color = beezdemand_colors["dark"], face = "bold"),
      strip.background = ggplot2::element_rect(fill = beezdemand_colors["light"], color = NA),
      strip.text = ggplot2::element_text(
        color = beezdemand_colors["primary"],
        face = "bold",
        margin = ggplot2::margin(4, 4, 4, 4)
      ),
      panel.spacing = ggplot2::unit(1, "lines"),
      plot.margin = ggplot2::margin(10, 10, 10, 10)
    )
}
