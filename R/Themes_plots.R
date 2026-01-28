#' Theme for IVs
#'
#' @param base_size
#' @param base_family
#'
#' @return
#' @export
#'
#' @examples
theme_chris_IV <-
  function(base_size = 11,
           axis_size = 10,
           base_family = "Arial") {
    theme_foundation(base_size = base_size, base_family = base_family) %+replace%

      theme(
        legend.key = element_blank(),
        legend.title = element_blank(),
        axis.text.x = element_text(
          color = "black",
          vjust = 1,
          size = axis_size,
          margin = margin(t = 1.5)
        ),
        axis.text.y = element_text(
          color = "black",
          hjust = 1,
          size = axis_size,
          margin = margin(r = 1.5)
        ),
        axis.text = element_text(color = "black", size = axis_size),
        axis.title.y = element_text(angle = 90, vjust = 1),
        legend.background = element_blank(),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black", linetype = "solid"),
        plot.margin = unit(c(0.2, 0.5, 0, 0), "cm"),
        # panel.grid.major = element_line(colour = "grey",size = 0.25),
        # panel.grid.minor = element_line(colour = "grey", size = 0.25),
        # panel.grid.minor.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        # aspect.ratio = 0.618,
        axis.line.y = element_blank(),
        axis.line.x = element_blank(),
        legend.justification = "left",
        axis.ticks = element_line(colour = "black", size = 0.5)
      )
  }

#' Theme for IVs better suited for analysis
#'
#' @param base_size
#' @param base_family
#'
#' @return
#' @export
#'
#' @examples
theme_chris_IV_analysis <-
  function(base_size = 11,
           axis_size = 10,
           base_family = "Arial") {
    theme_foundation(base_size = base_size, base_family = base_family) %+replace%

      theme(
        legend.key = element_blank(),
        legend.title = element_blank(),
        legend.background = element_blank(),
        axis.text.x = element_text(color = "black", size = axis_size),
        axis.text.y = element_text(
          color = "black",
          hjust = 1,
          size = axis_size,
          margin = margin(r = 5)
        ),
        axis.text = element_text(color = "black", size = axis_size),
        panel.background = element_blank(),
        plot.background = element_blank(),
        # panel.grid.major = element_line(size = 0.5, linetype = 'solid',
        #                                 colour = "grey87"),
        # panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
        #                                 colour = "grey87"),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black", linetype = "solid"),
        plot.margin = unit(c(0.4, 0.5, 0.4, 0), "cm"),
        panel.grid.major.x = element_line(colour = "grey", size = 0.25),
        panel.grid.minor.x = element_line(colour = "grey", size = 0.25),
        panel.grid.major.y = element_line(colour = "grey", size = 0.25),
        panel.grid.minor.y = element_line(colour = "grey", size = 0.25),
        # panel.grid.minor.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        # aspect.ratio = 0.618,
        axis.line.y = element_blank(),
        axis.line.x = element_blank(),
        legend.justification = "left",
        axis.ticks = element_line(colour = "black", size = 0.5)
      )
  }


#' Title
#'
#' @param base_size
#' @param axis_size
#' @param base_family
#'
#' @return
#' @export
#'
#' @examples
theme_chris_P_values <-
  function(base_size = 11,
           axis_size = 10,
           base_family = "Arial") {
    theme_foundation(base_size = base_size, base_family = base_family) %+replace%

      theme(
        axis.text.x = element_text(
          color = "black",
          vjust = 1,
          size = axis_size,
          margin = margin(t = 1.5)
        ),
        axis.text.y = element_text(
          color = "black",
          hjust = 1,
          size = axis_size,
          margin = margin(r = 1.5)
        ),
        axis.text = element_text(color = "black", size = axis_size),
        axis.title.x = element_blank(),
        plot.title = element_blank(),
        axis.title.y = element_text(angle = 90, vjust = 1),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black", linetype = "solid"),
        plot.margin = unit(c(0.2, 0.5, 0.2, 0), "cm"),
        # panel.grid.major = element_line(colour = "grey",size = 0.25),
        # panel.grid.minor = element_line(colour = "grey", size = 0.25),
        # panel.grid.minor.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        # aspect.ratio = 0.618,
        legend.justification = "left",
        axis.line.y = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks = element_line(colour = "black", size = 0.5)
      )
  }


#' theme for p-plots better suited for analysis
#'
#' @param base_size
#' @param axis_size
#' @param base_family
#'
#' @return
#' @export
#'
#' @examples
theme_chris_P_values_analysis <-
  function(base_size = 11,
           axis_size = 10,
           base_family = "Arial") {
    theme_foundation(base_size = base_size, base_family = base_family) %+replace%

      theme(
        axis.text.x = element_text(color = "black", size = axis_size),
        axis.text.y = element_text(
          color = "black",
          hjust = 1,
          size = axis_size,
          margin = margin(r = 5)
        ),
        axis.text = element_text(color = "black", size = axis_size),
        axis.title.x = element_blank(),
        plot.title = element_blank(),
        axis.title.y = element_text(angle = 90),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black", linetype = "solid"),
        plot.margin = unit(c(0.4, 0.5, 0.4, 0), "cm"),
        panel.grid.major.x = element_line(colour = "grey", size = 0.25),
        panel.grid.minor.x = element_line(colour = "grey", size = 0.25),
        panel.grid.major.y = element_line(colour = "grey", size = 0.25),
        panel.grid.minor.y = element_line(colour = "grey", size = 0.25),
        # panel.grid.minor.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        # aspect.ratio = 0.618,
        legend.justification = "left",
        axis.line.y = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks = element_line(colour = "black", size = 0.5)
      )
  }

#' Theme for Boxplots
#'
#' @param base_size
#' @param base_family
#'
#' @return
#' @export
#'
#' @examples
theme_chris_boxplot <-
  function(base_size = 11,
           axis_size = 10,
           base_family = "Arial") {
    theme_foundation(base_size = base_size, base_family = base_family) %+replace%

      theme(
        axis.text.x = element_text(
          color = "black",
          size = axis_size,
          angle = 45
        ),
        axis.text.y = element_text(
          color = "black",
          size = axis_size,
          margin = margin(r = 1.5),
          hjust = 1
        ),
        axis.text = element_text(color = "black", size = axis_size),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        panel.background = element_blank(),
        plot.background = element_blank(),
        plot.title = element_blank(),
        legend.position = "none",
        panel.border = element_blank(),
        axis.line = element_line(colour = "black", linetype = "solid"),
        axis.line.x = element_blank(),
        axis.ticks.y = element_line(size = 0.5),
        # plot.margin = unit(c(0.1,0.2,0.2,1),"cm"),
        plot.margin = margin(10, 0, 0, 0),
        # panel.grid.major = element_line(colour = "grey",size = 0.25),
        # panel.grid.minor = element_line(colour = "grey", size = 0.25),
        # panel.grid.minor.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        # aspect.ratio = 0.618,
        legend.title = element_blank()
      )
  }


#' Axes for IV plots
#'
#' @param plot1
#' @param name
#' @param linesize
#' @param overrideYlim
#' @param xlims
#' @param xend1
#' @param xends2
#'
#' @return
#' @export
#'
#' @examples
IV_plot_axes <-
  function(plot1,
           linesize,
           name = paste0("Potential (", settings_envir$voltage_unit, ")"),
           overrideYlim = NULL,
           xlims = c(-104, 100),
           xend1 = -104,
           xends2 = c(-100, 100)) {
    ymax <-
      max(ggplot_build(plot1)[["layout"]][["panel_scales_y"]][[1]][["range"]][["range"]])
    ymin <-
      min(ggplot_build(plot1)[["layout"]][["panel_scales_y"]][[1]][["range"]][["range"]])
    ymin_orig <- ymin
    ymin <- ymin - (abs(ymin) + abs(ymax)) * 0.03
    ymax <- ymax + (abs(ymin) + abs(ymax)) * 0.03
    if (!is.null(overrideYlim) &&
      !sum(unlist(map(overrideYlim, is.na))) > 0) {
      ymin <- overrideYlim[1]
      ymin_orig <- ymin
      ymax <- overrideYlim[2]
      ymin - (abs(ymin) + abs(ymax)) * 0.03
    }
    # print(ymin)
    plot1 <-
      plot1 + scale_x_continuous(
        name = name,
        limits = xlims,
        minor_breaks = seq(xends2[1], xends2[2], 10),
        expand = expansion(mult = c(0.0, 0)),
        labels = signs_format()
      )
    plot1 <-
      plot1 + geom_segment(aes(
        x = xend1,
        y = ymin_orig,
        xend = xend1,
        yend = ymax
      )) + geom_segment(aes(
        x = xends2[1],
        y = ymin,
        xend = xends2[2],
        yend = ymin
      )) + scale_y_continuous(labels = signs_format()) + coord_cartesian(ylim = c(ymin, ymax), expand = F)
    plot1 <-
      plot1 + geom_line(
        data = data.frame(x = c(xends2[1], xends2[2]), y = 0),
        aes(x = x, y = y),
        color = "grey87"
      )
    plot1 <-
      plot1 + geom_line(
        data = data.frame(x = 0, y = c(ymin_orig, ymax)),
        aes(x = x, y = y),
        color = "grey87"
      )
    plot1$layers <-
      plot1$layers[c(
        length(plot1$layers),
        length(plot1$layers) - 1,
        1:(length(plot1$layers) - 2)
      )]
    # plot1 <- add_horizontal_line_to_plot(plot1,0, col = "gray87", lty = 1, lwd = linesize)
    # plot1 <- add_vertical_line_to_plot(plot1,0, col = "gray87", lty = 1,lwd = linesize)
    # plot1 <- plot1 + guides(colour = guide_legend(override.aes = list(size=02,linetype = 1)))
    return(plot1)
  }


#' Title
#'
#' @param plot1
#' @param linesize
#' @param name
#' @param overrideYlim
#' @param xlims
#' @param xend1
#' @param xends2
#'
#' @return
#' @export
#'
#' @examples
IV_plot_axes_wo_AxisCross <-
  function(plot1,
           linesize,
           name = paste0("Potential (", settings_envir$voltage_unit, ")"),
           overrideYlim = NULL,
           xlims = c(-104, 100),
           xend1 = -104,
           xends2 = c(-100, 100)) {
    ymax <-
      max(ggplot_build(plot1)[["layout"]][["panel_scales_y"]][[1]][["range"]][["range"]])
    ymin <-
      min(ggplot_build(plot1)[["layout"]][["panel_scales_y"]][[1]][["range"]][["range"]])
    ymin_orig <- ymin
    ymin <- ymin - (abs(ymin) + abs(ymax)) * 0.03
    ymax <- ymax + (abs(ymin) + abs(ymax)) * 0.03
    if (!is.null(overrideYlim) &&
      !sum(unlist(map(overrideYlim, is.na))) > 0) {
      ymin <- overrideYlim[1]
      ymin_orig <- ymin
      ymax <- overrideYlim[2]
      ymin - (abs(ymin) + abs(ymax)) * 0.03
    }
    # print(ymin)
    plot1 <-
      plot1 + scale_x_continuous(
        name = name,
        limits = xlims,
        minor_breaks = seq(xends2[1], xends2[2], 10),
        labels = signs_format(),
        expand = expansion(mult = c(0.0, 0))
      )
    plot1 <-
      plot1 + geom_segment(aes(
        x = xend1,
        y = ymin_orig,
        xend = xend1,
        yend = ymax
      )) + geom_segment(aes(
        x = xends2[1],
        y = ymin,
        xend = xends2[2],
        yend = ymin
      )) + scale_y_continuous(labels = signs_format()) + coord_cartesian(ylim = c(ymin, ymax), expand = F)
    # plot1 <- plot1 + geom_line(data = data.frame(x = xends2[1]:xends2[2], y = 0),aes(x=x, y = y), color = "grey87")
    # plot1 <- plot1 + geom_line(data = data.frame(x = 0, y = seq(ymin_orig, ymax, 0.1)),aes(x=x, y = y), color = "grey87")
    # plot1$layers <- plot1$layers[c(length(plot1$layers),length(plot1$layers)-1,1:(length(plot1$layers)-2))]
    # plot1 <- add_horizontal_line_to_plot(plot1,0, col = "gray87", lty = 1, lwd = linesize)
    # plot1 <- add_vertical_line_to_plot(plot1,0, col = "gray87", lty = 1,lwd = linesize)
    # plot1 <- plot1 + guides(colour = guide_legend(override.aes = list(size=02,linetype = 1)))
    return(plot1)
  }




#' Axes for IV plots
#'
#' @param plot1
#' @param name
#' @param linesize
#' @param overrideYlim
#' @param xlims
#' @param xend1
#' @param xends2
#'
#' @return
#' @export
#'
#' @examples
IV_plot_axes_analysis <-
  function(plot1,
           linesize,
           name = paste0("Potential (", settings_envir$voltage_unit, ")"),
           overrideYlim = NULL,
           xlims = c(-104, 100),
           xend1 = -104,
           xends2 = c(-100, 100)) {
    ymax <-
      max(ggplot_build(plot1)[["layout"]][["panel_scales_y"]][[1]][["range"]][["range"]])
    ymin <-
      min(ggplot_build(plot1)[["layout"]][["panel_scales_y"]][[1]][["range"]][["range"]])
    ymin_orig <- ymin
    ymin <- ymin - (abs(ymin) + abs(ymax)) * 0.03
    ymax <- ymax + (abs(ymin) + abs(ymax)) * 0.03
    if (!is.null(overrideYlim) &&
      !sum(unlist(map(overrideYlim, is.na))) > 0) {
      ymin <- overrideYlim[1]
      ymin_orig <- ymin
      ymax <- overrideYlim[2]
      ymin - (abs(ymin) + abs(ymax)) * 0.03
    }
    # print(ymin)
    plot1 <-
      plot1 + scale_x_continuous(
        name = name,
        limits = xlims,
        breaks = pretty_breaks(n = 21),
        minor_breaks = pretty_breaks(n = 41),
        expand = expansion(mult = c(0.0, 0)),
        labels = signs_format()
      )
    plot1 <-
      plot1 + geom_segment(aes(
        x = xend1,
        y = ymin_orig,
        xend = xend1,
        yend = ymax
      )) + geom_segment(aes(
        x = xends2[1],
        y = ymin,
        xend = xends2[2],
        yend = ymin
      )) + scale_y_continuous(breaks = pretty_breaks(n = 20),labels = signs_format()) + coord_cartesian(ylim = c(ymin, ymax), expand = F)
    plot1 <-
      plot1 + geom_line(
        data = data.frame(x = -100:100, y = 0),
        aes(x = x, y = y),
        color = "grey87"
      )
    plot1 <-
      plot1 + geom_line(
        data = data.frame(x = 0, y = seq(ymin_orig, ymax, 0.1)),
        aes(x = x, y = y),
        color = "grey87"
      )
    plot1$layers <-
      plot1$layers[c(
        length(plot1$layers),
        length(plot1$layers) - 1,
        1:(length(plot1$layers) - 2)
      )]
    # plot1 <- add_horizontal_line_to_plot(plot1,0, col = "gray87", lty = 1, lwd = linesize)
    # plot1 <- add_vertical_line_to_plot(plot1,0, col = "gray87", lty = 1,lwd = linesize)
    # plot1 <- plot1 + guides(colour = guide_legend(override.aes = list(size=02,linetype = 1)))
    return(plot1)
  }






#' Axes for P-Plot
#'
#' @param plot1
#' @param name
#' @param labels
#' @param xlims
#' @param xend1
#' @param xends2
#'
#' @return
#' @export
#'
#' @examples
p_plot_axes <-
  function(plot1,
           name = paste0("Potential (", settings_envir$voltage_unit, ")"),
           labels = T,
           xlims = c(-104, 100),
           xend1 = -104,
           xends2 = c(-100, 100)) {

    if (name != "") {
      plot1 <-
        plot1 + scale_x_continuous(
          name = name,
          limits = xlims,
          minor_breaks = seq(xends2[1], xends2[2], 10),
          expand = expansion(mult = c(0, 0)),
          labels = signs_format()
        )
    }
    if (name == "") {
      plot1 <-
        plot1 + scale_x_continuous(
          name = "",
          limits = c(-104, 100),
          minor_breaks = seq(-100, 100, 10),
          expand = expansion(mult = c(0, 0)),
          labels = signs_format()
        )
    }
    if (labels == F) {
      plot1 <-
        plot1 + scale_x_continuous(
          limits = c(-104, 100),
          minor_breaks = seq(-100, 100, 10),
          expand = expansion(mult = c(0, 0)),
          labels = c(),
          labels = signs_format()
        )
    }
    # plot1 <- add_vertical_line_to_plot(plot1,0.05)
    # plot1 <- plot1 + draw_text("0.05",x = -100, y = 0.06)
    # plot1 <- plot1 + scale_y_log10(sec.axis = sec_axis(~ ., breaks = 0.05))
    plot1 <-
      plot1 + geom_segment(aes(
        x = xend1,
        y = 0.001,
        xend = xend1,
        yend = 0.05
      )) + geom_segment(aes(
        x = xends2[1],
        y = 0,
        xend = xends2[2],
        yend = 0
      )) + scale_y_log10(
        name = TeX(add_phantom_supersubscript("$\\textit{P}$ $value$")),
        breaks = c(0.05, 0.01, 0.001),
        labels = c(0.05, 0.01, 0.001)
      ) + coord_cartesian(ylim = c(0.0008, 0.05), expand = F)

    plot1 <- plot1 + theme(legend.position = "none")
    return(plot1)
  }





#' Axes for P-Plot
#'
#' @param plot1
#' @param name
#' @param labels
#'
#' @return
#' @export
#'
#' @examples
p_plot_axes_analysis <-
  function(plot1,
           name = paste0("Potential (", settings_envir$voltage_unit, ")"),
           labels = T) {
    if (name != "") {
      plot1 <-
        plot1 + scale_x_continuous(
          name = name,
          limits = c(-104, 100),
          breaks = pretty_breaks(n = 21),
          minor_breaks = seq(-100, 100, 10),
          expand = expansion(mult = c(0, 0)),
          labels = signs_format()
        )
    }
    if (name == "") {
      plot1 <-
        plot1 + scale_x_continuous(
          name = "",
          limits = c(-104, 100),
          breaks = pretty_breaks(n = 21),
          minor_breaks = seq(-100, 100, 10),
          expand = expansion(mult = c(0, 0)),
          labels = signs_format()
        )
    }
    if (labels == F) {
      plot1 <-
        plot1 + scale_x_continuous(
          limits = c(-104, 100),
          breaks = pretty_breaks(n = 21),
          minor_breaks = seq(-100, 100, 10),
          expand = expansion(mult = c(0, 0)),
          labels = c(),
          labels = signs_format()
        )
    }
    # plot1 <- add_vertical_line_to_plot(plot1,0.05)
    # plot1 <- plot1 + draw_text("0.05",x = -100, y = 0.06)
    # plot1 <- plot1 + scale_y_log10(sec.axis = sec_axis(~ ., breaks = 0.05))
    plot1 <-
      plot1 + geom_segment(aes(
        x = -104,
        y = 0.001,
        xend = -104,
        yend = 0.05
      )) + geom_segment(aes(
        x = -100,
        y = 0,
        xend = 100,
        yend = 0
      )) + scale_y_log10(
        name = "P-value",
        breaks = c(0.05, 0.01, 0.001),
        labels = c(0.05, 0.01, 0.001),
        minor_breaks = pretty_breaks(n = 10)
      ) + coord_cartesian(ylim = c(0.0008, 0.05), expand = F)

    plot1 <- plot1 + theme(legend.position = "none")
    return(plot1)
  }
