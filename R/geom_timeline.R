StatTimeline <- ggplot2::ggproto("StatTimeline", ggplot2::Stat,
                                 required_aes = c("x"),
                                 default_aes = ggplot2::aes(xmin = NULL, xmax = NULL, n_max = NULL),
                                 compute_group = function(data, scales) {
                                    if (is.null(data$xmin)) data$xmin <- min(data$x)
                                    if (is.null(data$xmax)) data$xmax <- max(data$x)
                                    df <- data[data$x >= data$xmin & data$x <= data$xmax,]
                                    if (is.null(df$n_max)) df$n_max <- length(df$x)
                                    df <- df[order(df$size,decreasing=T)[1:df$n_max[1]],]
                                    df
                                 }
)

#' Timeline Statistic, for NOAA Earthquake Dataset
#'
#' The timeline stat is used to subset the earthquake data for the specified data range 
#' (aesthetics \code{xmin} and \code{xmax}) and max n points (aesthetic \code{n_max}) based on
#' the size (magnitude) aesthetic.
#' 
#' @inheritParams ggplot2::geom_point
#'
#' @param geom Use to override the default connection between
#'   `geom_timeline` and `stat_timeline`.
#' 
#' @section Aesthetics:
#' \Sexpr[results=rd,stage=build]{eqvis:::rd_aesthetics("StatTimeline","stat_timeline")}
#' 
#' @examples
#' \dontrun{
#' ggplot(data=subset(eq_clean_data(sample_eq), !is.na(EQ_PRIMARY) & COUNTRY %in% c("USA","MEXICO")), 
#'    aes(x = DATE, y = COUNTRY, size = EQ_PRIMARY, xmin = as.Date('2000-01-01',"%Y-%m-%d"), 
#'    xmax = as.Date('2016-12-31',"%Y-%m-%d"), color = DEATHS, fill = DEATHS, label = LOCATION_NAME)) +
#' stat_timeline(alpha = .5) +
#' scale_size_continuous(name = "Richter Scale Value") +
#' scale_color_continuous(name = "# Deaths") +
#' scale_fill_continuous(guide=FALSE) +
#' labs(y = "") +
#' theme(legend.position = "bottom",
#'    legend.key = element_blank(), legend.box = "horizontal", panel.background = element_blank(), 
#'    axis.line.x = element_line(size = 1, color = "black"), axis.ticks.x = element_line(size = 1))
#' }
#' 
#' @export
stat_timeline <- function(mapping = NULL, data = NULL, geom = "timeline",
                          position = "identity", na.rm = FALSE, show.legend = NA,  
                          inherit.aes = TRUE, ...) {
   ggplot2::layer(
      stat = StatTimeline, data = data, mapping = mapping, geom = geom, 
      position = position, show.legend = show.legend, inherit.aes = inherit.aes,
      params = list(na.rm = na.rm, ...)
   )
}

GeomTimeline <- ggplot2::ggproto("GeomTimeline", ggplot2::Geom,
                        required_aes = c("x"),
                        default_aes = ggplot2::aes(shape=21, y=0, alpha = 1, colour = "black", fill = "black", size = 1.5, stroke = .5),
                        draw_key = ggplot2::draw_key_point,
                        draw_panel = function(data, panel_scales, coord) {
                           coords <- coord$transform(data, panel_scales)
                           pt_grob <- grid::pointsGrob(
                              coords$x, coords$y, pch = coords$shape,
                              gp = grid::gpar(
                                 col = scales::alpha(coords$colour, coords$alpha),
                                 fill = scales::alpha(coords$fill, coords$alpha),
                                 fontsize = coords$size * .pt + coords$stroke * .stroke/2
                              )
                           )
                           hline_grob <- grid::segmentsGrob(
                              coords$xmin, coords$y, coords$xmax, coords$y,
                              gp = grid::gpar(
                                 col = "gray",
                                 lwd = 2
                              )
                           )
                           grid::grobTree(hline_grob, pt_grob, name = "mygrobs")
                        }
)

#' Timeline plot, for NOAA Earthquake Dataset
#'
#' The timeline displays the significant earthquakes from the NOAA dataset along a timeline.
#'
#' @details The timeline is created over the time period of the dataset with points for each earthquake.
#' If the default stat_timeline is used as the stat, the stat xmin and xmax aesthetics subset the data.
#'
#' @inheritParams ggplot2::geom_point
#' 
#' @param stat Use to override the default connection between
#'   `geom_timeline` and `stat_timeline`.
#' 
#' @note The NOAA earthquake dataset can be accessed at: 
#' \href{https://www.ngdc.noaa.gov/nndc/struts/form?t=101650&s=1&d=1}{NOAA Significant Earthquake Database}.
#' This dataset contains information about 5,933 earthquakes around the world over an approximately 4,000 year time span. 
#' This package was developed to work with years AC only.  
#' 
#' @section Aesthetics:
#' \Sexpr[results=rd,stage=build]{eqvis:::rd_aesthetics("GeomTimeline","geom_timeline")}
#' 
#' @examples
#' \dontrun{
#' ggplot(data=subset(eq_clean_data(sample_eq), !is.na(EQ_PRIMARY) & COUNTRY %in% c("USA","MEXICO")), 
#'    aes(x = DATE, y = COUNTRY, size = EQ_PRIMARY, xmin = as.Date('2000-01-01',"%Y-%m-%d"), 
#'    xmax = as.Date('2016-12-31',"%Y-%m-%d"), color = DEATHS, fill = DEATHS, label = LOCATION_NAME)) +
#' geom_timeline(alpha = .5) +
#' scale_size_continuous(name = "Richter Scale Value") +
#' scale_color_continuous(name = "# Deaths") +
#' scale_fill_continuous(guide=FALSE) +
#' labs(y = "") +
#' theme(legend.position = "bottom",
#'    legend.key = element_blank(), legend.box = "horizontal", panel.background = element_blank(), 
#'    axis.line.x = element_line(size = 1, color = "black"), axis.ticks.x = element_line(size = 1))
#' }
#' 
#' @export
geom_timeline <- function(mapping = NULL, data = NULL, stat = "timeline",
                          position = "identity", na.rm = FALSE, show.legend = NA,
                          inherit.aes = TRUE, ...) {
   ggplot2::layer(
      geom = GeomTimeline,
      mapping = mapping,
      data = data,
      stat = stat,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(na.rm = na.rm, ...)
   )
}

# Geom Proto
# @rdname timeline-ggproto
# @format NULL
# @usage NULL
# @keywords internal
# @import scales
# @export
GeomTimelineLabel <- ggplot2::ggproto("GeomTimelineLabel", ggplot2::Geom,
                             required_aes = c("x","label"),
                             default_aes = ggplot2::aes(y = 0),
                             draw_panel = function(data, panel_scales, coord) {
                                prop_length = .1 / length(unique(data$y))
                                coords <- coord$transform(data, panel_scales)
                                txt_grob <- grid::textGrob(
                                   coords$label, coords$x, coords$y + prop_length, hjust = 0, vjust = 0, rot = 30
                                )
                                vline_grob <- grid::segmentsGrob(
                                   coords$x, coords$y, coords$x, coords$y + prop_length
                                )
                                grid::grobTree(vline_grob, txt_grob, name = "mygrob")
                             }
)

#' Timeline Labels, for NOAA Earthquake Dataset
#'
#' The timeline label geom is used to create lables for the timeline of earthquake data.
#' 
#' @inheritParams geom_timeline
#' 
#' @details The timeline label will add labels to the point data showing the values of the label aesthetic. 
#' If the default stat_timeline is used as the stat, N_max aesthetics will subset the labels to the top n based on magnitude.
#' 
#' @note The NOAA earthquake dataset can be accessed at: 
#' \href{https://www.ngdc.noaa.gov/nndc/struts/form?t=101650&s=1&d=1}{NOAA Significant Earthquake Database}.
#' This dataset contains information about 5,933 earthquakes around the world over an approximately 4,000 year time span. 
#' This package was developed to work with years AC only.  
#' 
#' @examples
#' \dontrun{
#' ggplot(data=subset(eq_clean_data(sample_eq), !is.na(EQ_PRIMARY) & COUNTRY %in% c("USA","MEXICO")), 
#'    aes(x = DATE, y = COUNTRY, size = EQ_PRIMARY, xmin = as.Date('2000-01-01',"%Y-%m-%d"), 
#'    xmax = as.Date('2016-12-31',"%Y-%m-%d"), color = DEATHS, fill = DEATHS, label = LOCATION_NAME)) +
#' geom_timeline(alpha = .5) +
#' scale_size_continuous(name = "Richter Scale Value") +
#' scale_color_continuous(name = "# Deaths") +
#' scale_fill_continuous(guide=FALSE) +
#' labs(y = "") +
#' geom_timeline_label(aes(n_max = 5)) +
#' theme(legend.position = "bottom",
#'    legend.key = element_blank(), legend.box = "horizontal", panel.background = element_blank(), 
#'    axis.line.x = element_line(size = 1, color = "black"), axis.ticks.x = element_line(size = 1))
#' }
#' 
#' @export
geom_timeline_label <- function(mapping = NULL, data = NULL, stat = "timeline",
                                position = "identity", na.rm = FALSE, show.legend = NA,
                                inherit.aes = TRUE, ...) {
   ggplot2::layer(
      geom = GeomTimelineLabel,
      mapping = mapping,
      data = data,
      stat = stat,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(na.rm = na.rm, ...)
   )
}
