#' Creates an emtpy plot as base for a matplot
#'
#' @param y_lab
#' @param title
#'
#' @return
#' @export
#'
#' @examples
matplot_base <-
  function(y_lab = "", title = "") {
    p <-
      ggplot() +
      theme(legend.position = "none") +
      xlab("Potential in [V]") +
      ylab(y_lab) +
      ggtitle(title)
    #  p <- add_horizontal_line_to_plot( p,0,col = "gray87")
    #  p <- add_vertical_line_to_plot( p,0,col = "gray87")


    return(p)
  }

#' Creates a line geom for a matplot
#'
#' @param plot_data
#' @param measurement Name of the measurement for color and legend
#' @param line_size
#'
#' @return
#' @export
#'
#' @examples
create_matplot_geom_line <- function(plot_data, measurement, line_size) {
  check_tibble(plot_data)
  check_character(measurement)
  geom <-
    geom_line(
      data = plot_data,
      aes(
        x = x_axis,
        y = IV,
        group = Measurement,
        color = measurement
      ),
      size = line_size
    )
  return(geom)
}

#' Prepares Data to be in the right format for a matplot
#'
#' @param plot_data
#' @param x_axis
#'
#' @return
#' @export
#'
#' @examples
get_matplot_data <- function(plot_data,
                             x_axis = seq(
                               min(settings_envir$ramp_data),
                               max(settings_envir$ramp_data),
                               settings_envir$step
                             )) {
  check_tibble(plot_data)
  check_numeric(x_axis)
  plot_data <- add_column(plot_data, x_axis = x_axis)

  keycol <- "Measurement"
  valuecol <- "IV"
  gathercols <- colnames(plot_data)[-length(plot_data)]

  plot_data <- pivot_longer(plot_data, gathercols, names_to = keycol, values_to = valuecol)

  return(plot_data)
}


#' Used to combine a ggplot with a new geom
#'
#' @param plot
#' @param geom
#'
#' @return
#' @export
#'
#' @examples
combine_plot_with_geom <- function(plot, geom) {
  check_list(plot)
  check_environment(geom)
  combined_plot <-
    plot + geom + theme(legend.position = "top")

  return(combined_plot)
}



#' Zooms in a on plot on the given Y-Axis values
#'
#' @param plot
#' @param y_min
#' @param y_max
#'
#' @return
#' @export
#'
#' @examples
zoom_plot <- function(plot, y_min, y_max) {
  check_list(plot)
  check_number(y_min)
  check_number(y_max)
  zoomed_plot <- plot + coord_cartesian(ylim = c(y_min, y_max), expand = FALSE)
  return(zoomed_plot)
}





#' Prepares Data to be in the right format for a median plot
#'
#' @param old_data
#' @param data
#'
#' @return
#' @export
#'
#' @examples
get_median_plot_data <- function(old_data,
                                 x_axis = seq(
                                   min(settings_envir$ramp_data),
                                   max(settings_envir$ramp_data),
                                   settings_envir$step
                                 ),
                                 sdMAD = "SD") {
  check_tibble(old_data)
  check_numeric(x_axis)

  median_vec <- rowMedians(as.matrix(old_data))

  if (sdMAD == "MAD") sd_vec <- rowMads(as.matrix(old_data))
  if (sdMAD == "SD") sd_vec <- rowSds(as.matrix(old_data))


  plot_data <-
    tibble(
      median = median_vec,
      SD = sd_vec,
      x_axis = x_axis
    )

  return(plot_data)
}

#' Creates a line geom for a median plot
#'
#' @param plot_data
#' @param measurement name of the measurement for coloring and the legend
#'
#' @return
#' @export
#'
#' @examples
create_median_plot_geom_line <- function(plot_data, measurement, size = 2, show_legend) {
  check_tibble(plot_data)
  check_string(measurement)
  geom <-
    geom_line(data = plot_data, aes(x = x_axis, y = median, color = measurement), size = size, show.legend = show_legend)
  return(geom)
}

#' Creates a ribbon geom for a median plot
#'
#' @param plot_data
#' @param measurement name of the measurement for coloring and the legend
#'
#' @return
#' @export
#'
#' @examples
create_median_plot_geom_ribbon <- function(plot_data, measurement, sdMAD_shadow_dir, show_legend) {
  check_tibble(plot_data)
  check_string(measurement)

  if (sdMAD_shadow_dir == "+") {
    geom <-
      geom_ribbon(
        data = plot_data,
        aes(
          x = x_axis,
          ymin = median,
          ymax = median + SD,
          fill = measurement
        ),
        alpha = 0.3, show.legend = show_legend
      )
  }
  if (sdMAD_shadow_dir == "-") {
    geom <-
      geom_ribbon(
        data = plot_data,
        aes(
          x = x_axis,
          ymax = median,
          ymin = median - SD,
          fill = measurement
        ),
        alpha = 0.3, show.legend = show_legend
      )
  }
  return(geom)
}


#' Creates an emtpy plot as base for a median plot
#'
#' @param y_lab
#' @param title
#'
#' @return
#' @export
#'
#' @examples
medianplot_base <-
  function(y_lab = "", title = "") {
    check_string(y_lab)
    check_string(title)

    base <-
      ggplot() +
      theme(legend.position = "none") +
      xlab("Potential in [V]") +
      ylab(y_lab) +
      ggtitle(title)
    #    base <- add_horizontal_line_to_plot( base,0,col = "gray87")
    #    base <- add_vertical_line_to_plot( base,0,col = "gray87")

    return(base)
  }

#' Change the ammount of columns in a plot legend
#'
#' @param old_plot
#' @param ncols amount of the columns
#'
#' @return
#' @export
#'
#' @examples
change_legend_cols <- function(old_plot, ncols) {
  return(old_plot + scale_color_discrete(guide = guide_legend(ncol = ncols)))
}



#' Creates the base for a single IV plot
#'
#' @param y_lab
#' @param title
#'
#' @return
#' @export
#'
#' @examples
singleplot_base <-
  function(y_lab = "", title = "") {
    check_string(y_lab)
    check_string(title)

    base <-
      ggplot() +
      theme(legend.position = "none") +
      xlab("Potential in [V]") +
      ylab(y_lab) +
      ggtitle(title)
    base <- add_horizontal_line_to_plot(base, 0, col = "gray87")
    base <- add_vertical_line_to_plot(base, 0, col = "gray87")

    return(base)
  }

#' Prepares data to be in the right format for a single plot
#'
#' @param old_data
#' @param x_axis
#'
#' @return
#' @export
#'
#' @examples
get_single_plot_data <- function(old_data,
                                 x_axis = seq(
                                   min(settings_envir$ramp_data),
                                   max(settings_envir$ramp_data),
                                   settings_envir$step
                                 )) {
  check_vector(old_data)
  check_numeric(x_axis)
  plot_data <-
    tibble(
      IV = old_data,
      x_axis = x_axis
    )

  return(plot_data)
}

#' Creates a line geom for a single IV Plot
#'
#' @param plot_data
#' @param measurement name of the measurement for coloring and the legend
#'
#' @return
#' @export
#'
#' @examples
create_single_plot_geom_line <- function(plot_data, measurement, size = 2) {
  check_tibble(plot_data)
  check_character(measurement)
  geom <-
    geom_line(
      data = plot_data,
      aes(
        x = x_axis,
        y = IV,
        color = measurement
      ), size = size
    )
  return(geom)
}

#' Creates the plot base for a p_value-plot
#'
#' @param y_lab
#' @param title
#'
#' @return
#' @export
#'
#' @examples
p_plot_base <- function(y_lab = "", title = "") {
  check_string(y_lab)
  check_string(title)

  base <-
    ggplot() +
    theme(legend.position = "none") +
    xlab("Potential in [V]") +
    ylab(TeX(y_lab)) +
    ggtitle(title)

  return(base)
}

#' Prepares data to be in the right format for a single plot
#'
#' @param old_data
#' @param x_axis
#'
#' @return
#' @export
#'
#' @examples
get_p_plot_data <- function(old_data,
                            x_axis = seq(
                              min(settings_envir$ramp_data),
                              max(settings_envir$ramp_data),
                              settings_envir$step
                            )) {
  check_vector(old_data)
  check_numeric(x_axis)
  plot_data <-
    tibble(
      IV = old_data,
      x_axis = x_axis
    )
  return(plot_data)
}

#' Creates a line geom for a p_plot
#'
#' @param plot_data Creates a line geom for a p_value-ztPlot
#' @param measurement name of the measurement for coloring and the legend
#'
#' @return
#' @export
#'
#' @examples
create_p_plot_geom_line <- function(plot_data, measurement, size = 1.2) {
  check_tibble(plot_data)
  check_character(measurement)
  geom <-
    geom_line(
      data = plot_data,
      aes(
        x = x_axis,
        y = IV,
        color = measurement
      ), size = size
    )
  return(geom)
}

#' Creates an area geom for a p_plot
#'
#' @param plot_data Creates a line geom for a p_value-ztPlot
#' @param measurement name of the measurement for coloring and the legend
#'
#' @return
#' @export
#'
#' @examples
create_p_plot_geom_area <- function(plot_data, measurement, size = 0.5) {
  check_tibble(plot_data)
  check_character(measurement)
  geom <-
    geom_ribbon(
      data = plot_data,
      aes(
        x = x_axis,
        ymin = 1,
        ymax = IV,
        color = measurement, fill = measurement
      ), size = size
    )
  return(geom)
}

#' Adds a horizontal line to a plot
#'
#' @param loc  y-axis intercept
#' @param ...
#' @param plot_old
#'
#' @return
#' @export
#'
#' @examples
add_horizontal_line_to_plot <- function(plot_old, loc, ...) {
  plot_new <- plot_old + geom_hline(yintercept = loc, ...)
  return(plot_new)
}

#' Title
#'
#' @param plot_old
#' @param loc x-axis intercept
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
add_vertical_line_to_plot <- function(plot_old, loc, ...) {
  plot_new <- plot_old + geom_vline(xintercept = loc, ...)
  return(plot_new)
}

#' changes the y-axis of a plot to a logarithmic one
#' @param old_plot
#'
#' @return
#' @export
#'
#' @examples
change_y_axis_to_log <- function(old_plot) {
  new_plot <- old_plot + scale_y_log10()
  return(new_plot)
}

#' Changes the theme of a plot
#'
#' @param old_plot
#' @param theme
#'
#' @return
#' @export
#'
#' @examples change_plot_theme(plot1, theme_cowplot())
change_plot_theme <- function(old_plot, theme) {
  new_plot <- old_plot + theme
  return(new_plot)
}

#' Changes the color theme of an plot
#'
#' @param old_plot
#' @param color_theme
#'
#' @return
#' @export
#'
#' @examples
change_plot_color <- function(old_plot, color_theme) {
  new_plot <- old_plot + color_theme
  return(new_plot)
}


#' change the color of a plot manually
#'
#' @param old_plot
#' @param colors
#'
#' @return
#' @export
#'
#' @examples change_plot_color_fill_manuel(plot1, c("OAG", "EA"), c("red", "green"))
change_plot_color_fill_manuel <- function(old_plot, breaks, colors, ncols) {
  new_plot <-
    old_plot + scale_color_manual(breaks = breaks, values = colors, guide = guide_legend(ncol = ncols)) +
    scale_fill_manual(breaks = breaks, values = colors, guide = guide_legend(ncol = ncols))
  return(new_plot)
}


#' change the color of a boxplot manually
#'
#' @param old_plot
#' @param colors
#' @param breaks
#' @param ncols
#'
#' @return
#' @export
#'
#' @examples change_boxplot_color_fill_manuel(plot1, c("red", "green", 1))
change_boxplot_color_fill_manuel <- function(old_plot, colors, ncols) {
  new_plot <-
    old_plot + scale_fill_manual(values = colors) + scale_color_manual(values = colors, guide = guide_legend(ncol = ncols))
  return(new_plot)
}


#' Do any manuel changes to the plot
#'
#' @param old_plot
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
change_plot_manually <- function(old_plot, ...) {
  new_plot <- old_plot + ...
  return(new_plot)
}




# Dirty müssen noch bearbeitet werden

#' Title
#'
#' @param IVs
#' @param path
#' @param series
#' @param filters
#' @param measurement
#' @param plot_column
#' @param line_size
#' @param title
#' @param base_size
#' @param axis_size
#'
#' @return
#' @export
#'
#' @examples
create_p_plots_area_grid <- function(IVs, path, series, filters, measurement, plot_column, line_size = 1, title, base_size, axis_size, colors) {
  p_plots <- list()
  #  colors <- pal_npg()(length(series))[rank(series_vec)][-1]
  colors <- colors
  if (length(series) == 1) colors <- palette_pander(length(filters) - 1)[rank(filters[-1])]
  if (length(series) >= 2) {
    for (i in 2:length(series)) {
      p_plots[[i - 1]] <- create_p_area_plot(list(IVs[[series[1]]], IVs[[series[i]]]), series[i], plot_column, c(filters[1], filters[i]), "P-Value", paste("P-Plot: ", series[[1]], measurement[[1]], "-", series[[i]], measurement[[i]]))
      p_plots[[i - 1]] <- change_plot_theme(p_plots[[i - 1]], theme_chris_IV(base_size, axis_size))
      # p_plots[[i-1]] <- add_horizontal_line_to_plot( p_plots[[i-1]], 0.05)
      p_plots[[i - 1]] <- p_plot_axes(p_plots[[i - 1]], name = "", labels = T)
      p_plots[[i - 1]] <- p_plots[[i - 1]] + theme(plot.title = element_blank(), axis.title.x = element_blank(), axis.text.x = element_text())
      p_plots[[i - 1]] <- p_plots[[i - 1]] + scale_fill_manual(breaks = series[-1][i - 1], values = colors[i]) + scale_color_manual(breaks = series[-1][i - 1], values = colors[i])
      #     p_plots[[i - 1]] <- p_plots[[i - 1]] + theme(axis.ticks.x = element_blank(), axis.title.y = element_blank())
      # p_plots[[i-1]] <- p_plots[[i - 1]] + draw_figure_label(letters[i-1],"top.left")
      # p_plots[[i-1]] <- zoom_plot(p_plots[[i-1]], 0.00001, 0.051)
    }
  }
  if (length(series) == 1) {
    for (i in 2:length(filter)) {
      p_plots[[i - 1]] <- create_p_area_plot(list(IVs[[series[1]]], IVs[[series[1]]]), series[1], plot_column, c(filters[1], filters[i]), "P-Value", paste("P-Plot: ", series[[1]], measurement[[1]], "-", series[[1]], measurement[[i]]))
      p_plots[[i - 1]] <- change_plot_theme(p_plots[[i - 1]], theme_chris_IV())
      # p_plots[[i-1]] <- add_horizontal_line_to_plot( p_plots[[i-1]], 0.05)
      p_plots[[i - 1]] <- p_plot_axes(p_plots[[i - 1]], name = "", labels = FALSE)
      p_plots[[i - 1]] <- p_plots[[i - 1]] + theme(plot.title = element_blank(), axis.title.x = element_blank())
      p_plots[[i - 1]] <- p_plots[[i - 1]] + scale_fill_manual(breaks = filter[i - 1], values = colors[i - 1]) + scale_color_manual(breaks = filter[i - 1], values = colors[i - 1])
      #       p_plots[[i - 1]] <- p_plots[[i - 1]] + theme(axis.ticks.x = element_blank())
    }
  }
  return(p_plots)
}



#' Title
#'
#' @param IVs
#' @param path
#' @param series
#' @param filters
#' @param plot_column
#' @param line_size
#' @param title
#'
#' @return
#' @export
#'
#' @examples
create_p_plots_grid <- function(IVs, path, series, filters, plot_column, line_size = 1, title) {
  p_plots <- list()
  for (i in 2:length(series)) {
    p_plots[[i - 1]] <- create_p_plot(list(IVs[[series[1]]], IVs[[series[i]]]), series[i], plot_column, c(filters[1], filters[i]), "P-Value", paste("P-Plot: ", series[[1]], measurement[[1]], "-", series[[i]], measurement[[i]]))
    p_plots[[i - 1]] <- change_plot_theme(p_plots[[i - 1]], theme_chris_IV())
    p_plots[[i - 1]] <- add_horizontal_line_to_plot(p_plots[[i - 1]], 0.05)
    p_plots[[i - 1]] <- p_plot_axes(p_plots[[i - 1]])
    p_plots[[i - 1]] <- p_plots[[i - 1]] + theme(title = element_blank())
    p_plots[[i - 1]] <- p_plots[[i - 1]] + scale_color_manual(values = "black")
    p_plots[[i - 1]] <- zoom_plot(p_plots[[i - 1]], 0.0001, 0.051)
  }
  return(p_plots)
}



#' Title
#'
#' @param IVs
#' @param path
#' @param series
#' @param measurement
#' @param filters
#' @param plot_column
#' @param line_size
#' @param title
#'
#' @return
#' @export
#'
#' @examples
create_p_plots <- function(IVs, path, series, measurement = "", filters, plot_column, line_size = 1, title) {
  p_plot <- create_p_plot(list(IVs[[series[1]]], IVs[[series[2]]]), paste0(series[[2]], measurement[[2]]), plot_column, c(filters[1], filters[2]), "P-Value", paste("P-Plot: ", title))
  if (length(series) > 2) {
    for (i in 3:length(series)) {
      p_plot <- add_to_p_plot(p_plot, list(IVs[[series[1]]], IVs[[series[i]]]), plot_column, c(filters[1], filters[i]), paste0(series[[i]], measurement[[i]]))
    }
  }
  p_plot <- change_plot_color_fill_manuel(p_plot, paste0(series[-1], measurement[-1]), pal_npg()(length(series))[-which(sort(series) == series[1])], 1)
  p_plot <- change_plot_theme(p_plot, theme_chris_IV())
  p_plot <- add_horizontal_line_to_plot(p_plot, 0.05)
  p_plot <- p_plot_axes(p_plot)

  save_actual_plot(paste0("PPlot_Activated
                            "), path, p_plot)
  return(p_plot)
}


#' Title
#'
#' @param summary_list
#' @param peak_list
#' @param title
#' @param y_lab
#' @param path
#'
#' @return
#' @export
#'
#' @examples
create_ratio_plots <- function(summary_list, peak_list, title, y_lab, lwd = 1) {
  ratio_plot <- create_ratio_plot(summary_list, peak_list, y_lab = y_lab, title = title, lwd = lwd)
  ratio_plot <- change_boxplot_color_fill_manuel(ratio_plot, create_boxplot_colors(rep(list(2), length(summary_list)), pal_npg, TRUE), 1)
  ratio_plot <- change_plot_theme(ratio_plot, theme_chris_boxplot())
  ratio_plot <- add_horizontal_line_to_plot(ratio_plot, 0)
  ratio_plot <- change_plot_manually(ratio_plot, theme(legend.position = "none"))
  # save_actual_plot(paste0("RatioPlot"), path, ratio_plot )
  return(ratio_plot)
}

#' Title
#'
#' @param IVs
#' @param path
#' @param series
#' @param measurement
#' @param filter
#' @param plot_columns
#' @param type
#' @param line_size
#' @param base_size
#' @param axis_size
#'
#' @return
#' @export
#'
#' @examples
create_matplot_plots <- function(IVs, path, series, measurement = "", filter, plot_columns, type, line_size, base_size, axis_size) {
  plot_storage <- list()
  for (plot_column in plot_columns) {
    plot_list <- list(create_matplot(IVs[[series[1]]], plot_column, filter[1], paste0(series[1], measurement[1]), plot_column, paste0(plot_column, ": ", type), line_size = line_size))
    if (length(series) == 1) {
      if (length(filter) > 1) {
        for (i in 2:length(filter)) {
          plot_list <- c(plot_list, list(add_to_matplot(tail(plot_list, 1)[[1]], IVs[[series[1]]], plot_column, filter[i], paste0(series[1], measurement[i]), line_size = line_size)))
        }
      } else {
        plot_list <- c(plot_list, list(add_to_matplot(tail(plot_list, 1)[[1]], IVs[[series[1]]], plot_column, filter[1], paste0(series[1], measurement[1]), line_size = line_size)))
      }
    } else {
      for (i in 2:length(series)) {
        plot_list <- c(plot_list, list(add_to_matplot(tail(plot_list, 1)[[1]], IVs[[series[i]]], plot_column, filter[i], paste0(series[i], measurement[i]), line_size = line_size)))
      }
    }
    # if(length(series) >= 2) finished_plot <- change_plot_color_fill_manuel(tail(plot_list,1)[[1]],paste0(filter,measurement), c("#000000",pal_npg()(length(filter)-1)),1)
    # if(length(series) == 1) finished_plot <- change_plot_color_fill_manuel(tail(plot_list,1)[[1]],paste0(filter,measurement), c("#000000",pal_npg()(length(filter)-1)),1)
    if (length(series) >= 2) finished_plot <- change_plot_color_fill_manuel(tail(plot_list, 1)[[1]], factor(paste0(series, measurement), levels = paste0(series, measurement)), c("#000000", palette_pander(length(series) - 1)), 1)
    if (length(series) == 1) finished_plot <- change_plot_color_fill_manuel(tail(plot_list, 1)[[1]], factor(paste0(series, measurement), levels = paste0(series, measurement)), c("#000000", palette_pander(length(filter) - 1)), 1)
    finished_plot <- change_plot_theme(finished_plot, theme_chris_IV(base_size, axis_size))
    finished_plot <- IV_plot_axes(finished_plot, linesize = 1, name = "")
    # save_actual_plot(paste0("Medianplot_",type,"_",plot_column), path, finished_plot)
    plot_storage <- c(plot_storage, list(finished_plot))
  }
  return(plot_storage)
}


#' Title
#'
#' @param IVs
#' @param path
#' @param series
#' @param measurement
#' @param filter
#' @param plot_columns
#' @param type
#' @param line_size
#' @param base_size
#' @param axis_size
#' @param sdMAD
#'
#' @return
#' @export
#'
#' @examples
create_median_plots <- function(IVs, path, series, measurement = "", filter, plot_columns, type, line_size = 1.3, base_size, axis_size, sdMAD) {
  plot_storage <- list()
  for (plot_column in plot_columns) {
    plot_list <- list(create_median_plot(IVs[[series[1]]], plot_column, filter[1], paste0(series[1], measurement[1]), plot_column, paste0(plot_column, ": ", type), line_size = line_size, sdMAD))
    if (length(series) == 1) {
      if (length(filter) > 1) {
        for (i in 2:length(filter)) {
          plot_list <- c(plot_list, list(add_to_median_plot(tail(plot_list, 1)[[1]], IVs[[series[1]]], plot_column, filter[i], factor(paste0(series[1], measurement[i])), line_size = line_size, sdMAD)))
        }
      } else {
        plot_list <- c(plot_list, list(add_to_median_plot(tail(plot_list, 1)[[1]], IVs[[series[1]]], plot_column, filter[1], factor(paste0(series[1], measurement[1])), line_size = line_size, sdMAD)))
      }
    } else {
      for (i in 2:length(series)) {
        plot_list <- c(plot_list, list(add_to_median_plot(tail(plot_list, 1)[[1]], IVs[[series[i]]], plot_column, filter[i], factor(paste0(series[i], measurement[i])), line_size = line_size, sdMAD)))
      }
    }
    # if(length(series) >= 2) finished_plot <- change_plot_color_fill_manuel(tail(plot_list,1)[[1]],paste0(filter,measurement), c("#000000",pal_npg()(length(filter)-1)),1)
    # if(length(series) == 1) finished_plot <- change_plot_color_fill_manuel(tail(plot_list,1)[[1]],paste0(filter,measurement), c("#000000",pal_npg()(length(filter)-1)),1)
    if (length(series) >= 2) finished_plot <- change_plot_color_fill_manuel(tail(plot_list, 1)[[1]], factor(paste0(series, measurement), levels = paste0(series, measurement)), c("#000000", palette_pander(length(series) - 1)), 1)
    if (length(series) == 1) finished_plot <- change_plot_color_fill_manuel(tail(plot_list, 1)[[1]], factor(paste0(series, measurement), levels = paste0(series, measurement)), c("#000000", palette_pander(length(filter) - 1)), 1)
    finished_plot <- change_plot_theme(finished_plot, theme_chris_IV(base_size, axis_size))
    finished_plot <- IV_plot_axes(finished_plot, line_size, name = "")
    # save_actual_plot(paste0("Medianplot_",type,"_",plot_column), path, finished_plot)
    plot_storage <- c(plot_storage, list(finished_plot))
  }
  return(plot_storage)
}
