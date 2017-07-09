#' Geom Proto
#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @keywords internal
#' @export
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
#' The timeline stat is used to subset the earthquake data for the specified data range and max n points based on
#' the size (magnitude) aesthetic.
#' 
#' @param mapping Set of aesthetic mappings created by \code{aes} or \code{aes_} If specified and 
#' inherit.aes = TRUE (the default), it is combined with the default mapping at the top level of 
#' the plot. You must supply mapping if there is no plot mapping.
#' 
#' @param data The data to be displayed in this layer. There are three options:
#' If NULL, the default, the data is inherited from the plot data as specified in the call to ggplot.
#' A data.frame, or other object, will override the plot data. All objects will be fortified to produce a data frame. See fortify for which variables will be created
#' A function will be called with a single argument, the plot data. The return value must be a data.frame., and will be used as the layer data.
#' 
#' @param geom The geometric object to use to display the data.
#' 
#' @param position Position adjustment, either as a string, or the result of a call to a position adjustment function.
#' 
#' @param ... other arguments passed on to layer. These are often aesthetics, used to set an aesthetic to a fixed value, like color = "red" or size = 3. They may also be parameters to the paired geom/stat.
#' 
#' @param na.rm If FALSE (the default), removes missing values with a warning. If TRUE silently removes missing values.
#' 
#' @param show.legend logical. Should this layer be included in the legends? NA, the default, includes if any aesthetics are mapped. FALSE never includes, and TRUE always includes.
#' 
#' @param inherit.aes If FALSE, overrides the default aesthetics, rather than combining with them. This is most useful for helper functions that define both data and aesthetics and shouldn't inherit behaviour from the default plot specification, e.g. borders.
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

#' Geom Proto
#' @rdname timeline-ggproto
#' @format NULL
#' @usage NULL
#' @keywords internal
#' @import scales
#' @export
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

#' Timeline, for NOAA Earthquake Dataset
#'
#' The timeline geom is used to create a timeline of earthquake data.
#' 
#' @param mapping Set of aesthetic mappings created by \code{aes} or \code{aes_} If specified and 
#' inherit.aes = TRUE (the default), it is combined with the default mapping at the top level of 
#' the plot. You must supply mapping if there is no plot mapping.
#' 
#' @param data The data to be displayed in this layer. There are three options:
#' If NULL, the default, the data is inherited from the plot data as specified in the call to ggplot.
#' A data.frame, or other object, will override the plot data. All objects will be fortified to produce a data frame. See fortify for which variables will be created
#' A function will be called with a single argument, the plot data. The return value must be a data.frame., and will be used as the layer data.
#' 
#' @param stat The statistical transformation to use on the data for this layer, as a string.
#' 
#' @param position Position adjustment, either as a string, or the result of a call to a position adjustment function.
#' 
#' @param ... other arguments passed on to layer. These are often aesthetics, used to set an aesthetic to a fixed value, like color = "red" or size = 3. They may also be parameters to the paired geom/stat.
#' 
#' @param na.rm If FALSE (the default), removes missing values with a warning. If TRUE silently removes missing values.
#' 
#' @param show.legend logical. Should this layer be included in the legends? NA, the default, includes if any aesthetics are mapped. FALSE never includes, and TRUE always includes.
#' 
#' @param inherit.aes If FALSE, overrides the default aesthetics, rather than combining with them. This is most useful for helper functions that define both data and aesthetics and shouldn't inherit behaviour from the default plot specification, e.g. borders.
#'
#' @details The timeline is created over the time period of the dataset with points for each earthquake.
#' If the default stat_timeline is used as the stat, xmin and xmax aesthetics subset the data.
#' 
#' @note The NOAA earthquake dataset can be accessed at: 
#' \url{https://www.ngdc.noaa.gov/nndc/struts/results?type_0=Exact&query_0=$ID&t=101650&s=13&d=189&dfn=signif.txt}.
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

#' Geom Proto
#' @rdname timeline-ggproto
#' @format NULL
#' @usage NULL
#' @keywords internal
#' @import scales
#' @export
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
#' \url{https://www.ngdc.noaa.gov/nndc/struts/results?type_0=Exact&query_0=$ID&t=101650&s=13&d=189&dfn=signif.txt}.
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
