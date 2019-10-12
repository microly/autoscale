# most of the code is copied from
# https://stackoverflow.com/questions/49961135/how-to-tell-ggplot2-to-use-an-user-created-scale-for-a-new-aesthetic




"%||%" <- function(a, b) {
    if (!is.null(a)) a else b
}




#' @import ggplot2
NULL


#' geom_arrow
#'
#' @param mapping
#' @param data
#' @param stat
#' @param position
#' @param ...
#' @param start
#' @param direction
#' @param min.mag
#' @param skip
#' @param skip.x
#' @param skip.y
#' @param arrow.angle
#' @param arrow.length
#' @param arrow.ends
#' @param arrow.type
#' @param arrow
#' @param lineend
#' @param na.rm
#' @param show.legend
#' @param inherit.aes
#'
#' @return
#' @export
#' @import ggplot2
#'
#' @examples
#'
#' library(tibble)
#' geo <- tibble(lon = 1:10, lat = 1:10, mag = 1:10, angle = 1:10)
#'
#' # scale_mag_continuous <- scale_mag
#'
#' ggplot(geo, aes(lon, lat)) +
#'     geom_arrow(aes(mag = mag, angle = angle))
#'
geom_arrow <- function(mapping = NULL, data = NULL,
                       stat = "arrow",
                       position = "identity", ...,
                       start = 0,
                       direction = 1,
                       # scale = 1,
                       min.mag = 0,
                       skip = 0,
                       skip.x = skip,
                       skip.y = skip,
                       arrow.angle = 15,
                       arrow.length = 0.5,
                       arrow.ends = "last",
                       arrow.type = "closed",
                       arrow = grid::arrow(arrow.angle, unit(arrow.length, "lines"),
                                           ends = arrow.ends, type = arrow.type),
                       lineend = "butt",
                       na.rm = FALSE,
                       show.legend = NA,
                       inherit.aes = TRUE) {
    layer(geom = GeomArrow,
          mapping = mapping,
          data = data,
          stat = stat,
          position = position,
          show.legend = show.legend,
          inherit.aes = inherit.aes,
          params = list(
              start = start,
              direction = direction,
              arrow = arrow,
              lineend = lineend,
              na.rm = na.rm,
              # scale = scale,
              skip.x = skip.x,
              skip.y = skip.y,
              min.mag = min.mag,
              ...)
    )
}

#' @import ggplot2
GeomArrow <- ggplot2::ggproto("GeomArrow", ggplot2::Geom,
                              required_aes = c("x", "y"),
                              default_aes = ggplot2::aes(color = "black", size = 0.5, min.mag = 0,
                                                         linetype = 1, alpha = NA),
                              draw_key = ggplot2::draw_key_path,
                              draw_panel = function(data, panel_scales, coord,
                                                    arrow = arrow, lineend = lineend,
                                                    start = start, direction = direction,
                                                    preserve.dir = TRUE) {
                                  coords <- coord$transform(data, panel_scales)
                                  unit.delta <- "snpc"
                                  if (preserve.dir == FALSE) {
                                      coords$angle <- with(coords, atan2(yend - y, xend - x)*180/pi)
                                      unit.delta <- "npc"
                                  }

                                  coords$dx <- with(coords, mag*cos(angle*pi/180))
                                  coords$dy <- with(coords, mag*sin(angle*pi/180))

                                  # from https://stackoverflow.com/questions/47814998/how-to-make-segments-that-preserve-angles-in-different-aspect-ratios-in-ggplot2
                                  xx <- grid::unit.c(grid::unit(coords$x, "npc"),
                                                     grid::unit(coords$x, "npc") + grid::unit(coords$dx, unit.delta))
                                  yy <- grid::unit.c(grid::unit(coords$y, "npc"),
                                                     grid::unit(coords$y, "npc") + grid::unit(coords$dy, unit.delta))


                                  mag <- with(coords, mag/max(mag, na.rm = T))
                                  arrow$length <- unit(as.numeric(arrow$length)*mag, attr(arrow$length, "unit"))

                                  pol <- grid::polylineGrob(x = xx, y = yy,
                                                            default.units = "npc",
                                                            arrow = arrow,
                                                            gp = grid::gpar(col = coords$colour,
                                                                            fill = scales::alpha(coords$colour, coords$alpha),
                                                                            alpha = ifelse(is.na(coords$alpha), 1, coords$alpha),
                                                                            lwd = coords$size*.pt,
                                                                            lty = coords$linetype,
                                                                            lineend = lineend),
                                                            id = rep(seq(nrow(coords)), 2))
                                  pol
                              })

#' @import ggplot2
StatArrow <- ggplot2::ggproto("StatArrow", ggplot2::Stat,
                              required_aes = c("x", "y"),
                              default_aes = ggplot2::aes(min.mag = 0, dx = NULL, dy = NULL,
                                                         mag = NULL, angle = NULL),
                              compute_group = function(data, scales,
                                                       skip.x = skip.x, skip.y = skip.y,
                                                       min.mag = min.mag) {
                                  min.mag <- data$min.mag %||% min.mag

                                  if (is.null(data$mag) | is.null(data$angle)) {
                                      if (is.null(data$dx) | is.null(data$dy)) stop("stat_arrow need dx, dy or mag angle (improve mesage!!)")
                                      data$mag <- with(data, Mag(dx, dy))
                                      data$angle <- with(data, atan2(dy, dx)*180/pi)
                                  } else {
                                      data$dx <- with(data, mag*cos(angle*pi/180))
                                      data$dy <- with(data, mag*sin(angle*pi/180))
                                  }



                                  data$xend = with(data, x + dx)
                                  data$yend = with(data, y + dy)
                                  data

                              }
)


#' scale_mag
#'
#' @param length
#' @param max
#' @param default_unit
#'
#' @return
# #' @export
#' @import ggplot2
#'
#' @examples
scale_mag <- function(length = 0.1,
                      max = waiver(),
                      default_unit = "lines") {
    # if (!is.unit(length)) length <- ggplot2::unit(length, default_unit)

    continuous_scale("mag",
                     "mag",
                     identity,
                     rescaler = rescale_mag(length, max),
                     guide = "none")
}

# scale_type.mag <- function(x) "vector"


#' @import ggplot2
rescale_mag <- function(length, max) {
    function(x, from) {
        if (ggplot2:::is.waive(max)) max <- max(x, na.rm = T)
        scales::rescale(x, c(0, length), c(0, max))
    }
}


#' @export
scale_mag_continuous <- scale_mag


