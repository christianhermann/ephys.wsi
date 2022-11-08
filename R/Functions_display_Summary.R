####Functions to display summary data

#' prepares data for a boxplot; column-list should be in the same format as the output of peak_addition
#'
#' @param summary_list
#' @param column_list
#'
#' @return
#' @export
#'
#' @examples
get_boxplot_data <- function(summary_list, column_list) {
  summary_data <-
    map2(column_list, summary_list, function(cols, sum) {
      map(unlist(cols), function(col, sum) {
        sum[[col]]
      }, sum)
    })
  unlist_cols <- map(column_list, unlist)
  data_envir$boxplot_cols <- unlist_cols
  summary_data <-
    map2(summary_data, unlist_cols, function(a, b) {
      names(a) <- b
      a
    })

  summary_data_min <-
    map(summary_data, function(summary_data) {
      return(summary_data[seq(1, length(summary_data), 2)])
    })
  summary_data_max <-
    map(summary_data, function(summary_data) {
      return(summary_data[seq(2, length(summary_data), 2)])
    })

  summary_data_min <-
    map2(summary_data_min, names(summary_data_min), function(x, y) {
      x <- c(x, list(series = (rep(
        y, length(x[[1]])
      ))))
    })
  summary_data_max <-
    map2(summary_data_max, names(summary_data_max), function(x, y) {
      x <- c(x, list(series = (rep(
        y, length(x[[1]])
      ))))
    })

  summary_data_df_min <- bind_rows(summary_data_min)
  summary_data_df_max <- bind_rows(summary_data_max)

  summary_data_df_min$series <- factor(
    summary_data_df_min$series,
    levels = unique(summary_data_df_min$series),
    ordered = T
  )

  summary_data_df_max$series <- factor(
    summary_data_df_max$series,
    levels = unique(summary_data_df_max$series),
    ordered = T
  )


  # if (length(grep("Bef", names(summary_data_df_min))) > 0) {
  summary_data_long_min <-
    gather(
      summary_data_df_min,
      "col",
      "value",
      -series,
      factor_key = T,
      na.rm = T
    )
  summary_data_long_max <-
    gather(
      summary_data_df_max,
      "col",
      "value",
      -series,
      factor_key = T,
      na.rm = T
    )
  # } else{
  #   summary_data_long_min <-
  #     pivot_longer (summary_data_df_min,-series,
  #                   "col",
  #                   values_drop_na = TRUE)
  #
  #   summary_data_long_max <-
  #     pivot_longer (summary_data_df_max,-series,
  #                   "col",
  #                   values_drop_na = TRUE)
  # }
  summary_data_long_min[["col"]] <-
    str_replace(summary_data_long_min[["col"]], "Imin", "")
  summary_data_long_max[["col"]] <-
    str_replace(summary_data_long_max[["col"]], "Imax", "")

  summary_data_long_min[["col"]] <-
    paste0(summary_data_long_min[["series"]], ": ", summary_data_long_min[["col"]])
  summary_data_long_max[["col"]] <-
    paste0(summary_data_long_max[["series"]], ": ", summary_data_long_max[["col"]])

  if (length(column_list[[1]]) > 1) {
    summary_data_long_minAct <- list()
    if (length(summary_data_long_min$series) %% length(column_list[[1]]) == 0) {
      summary_data_long_minBef <-
        summary_data_long_min[1:(length(summary_data_long_min$series) / length(column_list[[1]])), ]
      summary_data_long_minBef <-
        summary_data_long_minBef[order(summary_data_long_minBef$series), ]
      for (i in 2:length(column_list[[1]])) {
        summary_data_long_minAct[[i - 1]] <-
          summary_data_long_min[((
            length(summary_data_long_min$series) / length(column_list[[1]])
          ) * (i - 1) + 1):(length(summary_data_long_min$series) / length(column_list[[1]]) *
            i), ]
        summary_data_long_minAct[[i - 1]] <-
          summary_data_long_minAct[[i - 1]][order(summary_data_long_minAct[[i - 1]]$series), ]
      }
      summary_data_long_min <-
        bind_rows(summary_data_long_minBef, summary_data_long_minAct)
    }


    summary_data_long_maxAct <- list()
    if (length(summary_data_long_max$series) %% length(column_list[[1]]) == 0) {
      summary_data_long_maxBef <-
        summary_data_long_max[1:(length(summary_data_long_max$series) / length(column_list[[1]])), ]
      summary_data_long_maxBef <-
        summary_data_long_maxBef[order(summary_data_long_maxBef$series), ]
      for (i in 2:length(column_list[[1]])) {
        summary_data_long_maxAct[[i - 1]] <-
          summary_data_long_max[((
            length(summary_data_long_max$series) / length(column_list[[1]])
          ) * (i - 1) + 1):(length(summary_data_long_max$series) / length(column_list[[1]]) *
            i), ]
        summary_data_long_maxAct[[i - 1]] <-
          summary_data_long_maxAct[[i - 1]][order(summary_data_long_maxAct[[i - 1]]$series), ]
      }
      summary_data_long_max <-
        bind_rows(summary_data_long_maxBef, summary_data_long_maxAct)
    }
  } else {
    # summary_data_long_min <-
    #     pivot_longer (summary_data_df_min,-series,
    #                   "col",
    #                   values_drop_na = TRUE)
    #
    #   summary_data_long_max <-
    #     pivot_longer (summary_data_df_max,-series,
    #                   "col",
    #                   values_drop_na = TRUE)
  }


  summary_data_long_min[["col"]] <-
    factor(summary_data_long_min[["col"]],
      levels = unique(summary_data_long_min[["col"]], ordered = T)
    )
  summary_data_long_max[["col"]] <-
    factor(summary_data_long_max[["col"]],
      levels = unique(summary_data_long_max[["col"]], ordered = T)
    )


  return(list(summary_data_long_min, summary_data_long_max))
}


#' prepares data for a boxplot; peak_list should be in the same format as the output of peak_addition
#'
#'
#' @param summary_list
#' @param peak_list
#'
#' @return
#' @export
#'
#' @examples
get_ratio_plot_data <- function(summary_list, peak_list) {
  summary_data <-
    map2(peak_list, summary_list, function(peaks, sum) {
      map(unlist(peaks), function(peak, sum) {
        sum[[paste0("Ratio_", peak)]]
      }, sum)
    })
  unlist_peaks <- map(peak_list, unlist)
  data_envir$ratioplot_cols <- unlist_peaks
  summary_data <-
    map2(summary_data, unlist_peaks, function(a, b) {
      names(a) <- b
      a
    })

  summary_data <-
    map2(summary_data, names(summary_data), function(x, y) {
      x <- c(x, list(series = (rep(
        y, length(x[[1]])
      ))))
    })

  summary_data_df <- bind_rows(summary_data)

  summary_data_df$series <- factor(summary_data_df$series,
    levels = unique(summary_data_df$series)
  )

  summary_data_long <-
    pivot_longer(summary_data_df, -series,
      "col",
      values_drop_na = TRUE
    )

  summary_data_long[["col"]] <-
    str_replace(summary_data_long[["col"]], "Imin", "")

  summary_data_long[["col"]] <-
    paste0(summary_data_long[["series"]], ": ", summary_data_long[["col"]])

  summary_data_long[["col"]] <-
    factor(summary_data_long[["col"]], levels = unique(summary_data_long[["col"]]))

  return(summary_data_long)
}



#' calculates and formats p_values in the right format to plot
#'
#' @param plot_data
#' @param summary_list
#' @param column_list
#'
#' @return
#' @export
#'
#' @examples
get_p_data <-
  function(plot_data,
           summary_list,
           column_list,
           Ratio = F,
           paired = F,
           ...) {
    if (Ratio == T) {
      plot_data_max <- unique(plot_data[, 1:2])
      plot_data_max <- plot_data_max[order(plot_data_max$series), ]


      p_values <-
        get_summary_p_values_U_test(summary_list, column_list, paired)

      p_values_max <- pmap(p_values, function(...) {
        c(...)
      })

      p_data_max <- cbind(plot_data_max, value = unlist(p_values_max))

      return(list(p_data_max))
    }
    plot_data_min <- unique(plot_data[[1]][, 1:2])
    plot_data_max <- unique(plot_data[[2]][, 1:2])
    plot_data_min <- plot_data_min[order(plot_data_min$series), ]
    plot_data_max <- plot_data_max[order(plot_data_max$series), ]


    p_values <-
      get_summary_p_values_U_test(summary_list, column_list, paired)
    p_values_min <- p_values[seq(1, length(p_values), 2)]
    p_values_max <- p_values[seq(2, length(p_values), 2)]
    p_values_min <- pmap(p_values_min, function(...) {
      c(...)
    })
    p_values_max <- pmap(p_values_max, function(...) {
      c(...)
    })
    p_values_min_vec <- unlist(p_values_min)
    p_values_max_vec <- unlist(p_values_max)

    p_data_min <- cbind(plot_data_min, value = p_values_min_vec)
    p_data_max <- cbind(plot_data_max, value = p_values_max_vec)

    return(list(p_data_min, p_data_max))
  }


#' Creates the baseplot for a boxplot
#'
#' @param y_lab
#' @param title
#' @param ratio
#'
#' @return
#' @export
#'
#' @examples
boxplot_base <-
  function(y_lab = "",
           title = "",
           ratio = F) {
    p <-
      ggplot(mapping = aes(x = series, y = value, colour = col)) +
      xlab("Series") +
      ylab(y_lab) +
      ggtitle(title)
    if (ratio == T) {
      return(p)
    }
    p <-
      add_horizontal_line_to_plot(p, 0, col = "black", size = 0.5)
    return(p)
  }

#' Creates a boxplot geom
#'
#' @param plot_data
#'
#' @return
#' @export
#'
#' @examples
create_boxplot_geom <- function(plot_data, lwd = 1) {
  geom_box <-
    geom_boxplot(
      data = plot_data,
      aes(x = series, y = value, colour = col),
      position = position_dodge2(),
      outlier.shape = NA,
      lwd = lwd
    )
  return(geom_box)
}

#' Creates Errorbars/Whiskers for Boxplots
#'
#' @param plot_data
#'
#' @return
#' @export
#'
#' @examples
create_errorbar_geom <- function(plot_data, lwd) {
  geom_error <- stat_boxplot(
    data = plot_data,
    aes(x = series, y = value, colour = col),
    geom = "errorbar",
    position = position_dodge(0.75),
    width = 0.3,
    lwd = lwd
  )
  return(geom_error)
}

#' Creates jitterplot geom
#'
#' @param plot_data
#' @param ... additional values for position_jitterdodge
#' @param alphaV
#'
#' @return
#' @export
#'
#' @examples
create_jitter_geom <- function(plot_data, alphaV, jittersize, ...) {
  geom_jitter <-
    geom_jitter(
      data = plot_data,
      aes(x = series, y = value, colour = col),
      position = position_jitterdodge(...),
      alpha = alphaV,
      size = jittersize
    )
  return(geom_jitter)
}

#' Creates a violon plot geom
#'
#' @param plot_data
#' @param alphaV
#'
#' @return
#' @export
#'
#' @examples
create_violin_geom <- function(plot_data, alphaV) {
  geom_violin <-
    geom_violin(
      data = plot_data,
      aes(x = series, y = value, colour = col),
      position = position_dodge(0.75),
      alpha = alphaV
    )
  return(geom_violin)
}

#' Creates a pallete of colors which can be used for boxplots
#'
#' @param pallete
#' @param col_numbers
#' @param double_colors logical if colors are going to be substetted by every second element.
#' @param a_scale
#'
#' @return
#' @export
#'
#' @examples create_boxplot_colors(list(3, 5), pal_aaas)
create_boxplot_colors <-
  function(col_numbers,
           pallete,
           double_colors,
           a_scale = 10) {
    plot_colors <- c()
    for (i in 1:length(col_numbers)) {
      for (j in 1:col_numbers[[i]]) {
        plot_colors <-
          c(plot_colors, (pallete(alpha = 1 - (j / a_scale))(i)[[i]]))
      }
    }
    if (!double_colors) {
      return(plot_colors)
    }
    colors_one <- plot_colors[seq(1, length(plot_colors), 2)]
    colors_two <- plot_colors[seq(2, length(plot_colors), 2)]
    return(c(colors_one, colors_two))
  }


#' Creates a geom containing the amount of observations for a boxplot
#'
#' @param plot_data
#' @param type
#' @param scale
#'
#' @return
#' @export
#'
#' @examples
create_observation_number_geom <-
  function(plot_data,
           type,
           scale = 1.5,
           size,
           placement_type = "absolute",
           scale_relative = 0.2) {
    if (placement_type == "absolute") {
      if (type == "min") {
        y <- min(plot_data["value"])
      }
      if (type == "max") {
        y <- max(plot_data["value"])
      }

      n_fun <- function(x) {
        return(data.frame(
          y = y * scale_relative,
          label = paste0( # "n = ",
            length(x)
          )
        ))
      }
    }

    if (placement_type == "relative") {
      if (type == "min") {
        y <-
          summarise(
            group_by(plot_data, series),
            maxVal = (min(value)),
            name = unique(series)
          )
        y2 <-
          summarise(
            group_by(plot_data, col),
            origMaxVal = (min(value)),
            name = unique(series)
          )
        y <-
          y %>% slice(rep(1:n(), times = length(unique(
            plot_data$col
          )) / length(unique(
            plot_data$series
          ))))
        y <- cbind(y, y2)
        step_y <- min(y$maxVal) * scale_relative
      }
      if (type == "max") {
        y <-
          summarise(
            group_by(plot_data, series),
            maxVal = (max(value)),
            name = unique(series)
          )
        y2 <-
          summarise(
            group_by(plot_data, col),
            origMaxVal = (max(value)),
            nameOrig = unique(series)
          )
        y <- y[order(y2$nameOrig), ]

        y <-
          y %>% slice(rep(1:n(), times = length(unique(
            plot_data$col
          )) / length(unique(
            plot_data$series
          ))))
        y <- cbind(y, y2)
        step_y <- max(y$maxVal) * scale_relative
      }

      n_fun <- function(x) {
        return(data.frame(
          y = y$maxVal[which(y$origMaxVal == max(x))] + step_y,
          label = paste0( # "n = ",
            length(x)
          )
        ))
      }
    }

    observation_number_geom <- stat_summary(
      data = plot_data,
      aes(x = series, y = value, colour = col),
      fun.data = n_fun,
      geom = "text",
      size = size
      #    position = position_dodge2(width = 0.5)
    )
    return(observation_number_geom)
  }


#' Creates a geom containing the median +sd for a boxplot
#'
#' @param plot_data
#' @param type
#' @param scale
#' @param size
#' @param placement_type
#' @param scale_relative
#'
#' @return
#' @export
#'
#' @examples
create_median_number_geom <-
  function(plot_data,
           type,
           scale = 1.5,
           size,
           placement_type = "absolute",
           scale_relative = 0.2) {
    size <- size * 0.8
    scale_relative <- scale_relative * 1.2
    if (placement_type == "absolute") {
      if (type == "min") {
        y <- min(plot_data["value"])
      }
      if (type == "max") {
        y <- max(plot_data["value"])
      }

      n_fun <- function(x) {
        return(data.frame(
          y = y * scale_relative,
          label = paste0(round(median(x), 1), "\n+-", round(sd(x), 1))
        ))
      }
    }

    if (placement_type == "relative") {
      if (type == "min") {
        y <-
          summarise(
            group_by(plot_data, series),
            maxVal = (min(value)),
            name = unique(series)
          )
        y2 <-
          summarise(
            group_by(plot_data, col),
            origMaxVal = (min(value)),
            name = unique(series)
          )
        y <-
          y %>% slice(rep(1:n(), times = length(unique(
            plot_data$col
          )) / length(unique(
            plot_data$series
          ))))
        y <- cbind(y, y2)
        step_y <- min(y$maxVal) * scale_relative
      }
      if (type == "max") {
        y <-
          summarise(
            group_by(plot_data, series),
            maxVal = (max(value)),
            name = unique(series)
          )
        y2 <-
          summarise(
            group_by(plot_data, col),
            origMaxVal = (max(value)),
            nameOrig = unique(series)
          )
        y <- y[order(y2$nameOrig), ]

        y <-
          y %>% slice(rep(1:n(), times = length(unique(
            plot_data$col
          )) / length(unique(
            plot_data$series
          ))))
        y <- cbind(y, y2)
        step_y <- max(y$maxVal) * scale_relative
      }

      n_fun <- function(x) {
        return(data.frame(
          y = y$maxVal[which(y$origMaxVal == max(x))] + step_y,
          label = paste0(round(median(x), 1), "\n+-", round(sd(x), 1))
        ))
      }
    }

    median_number_geom <- stat_summary(
      data = plot_data,
      aes(x = series, y = value, colour = col),
      fun.data = n_fun,
      geom = "text",
      size = size,
      position = position_dodge2(width = 0.75)
    )
    return(median_number_geom)
  }


#' Creates a geom containing signficance values
#'
#' @param plot_data
#' @param summary_list
#' @param column_list
#' @param type "min" or "max"
#' @param p_decimal numbers of decimals after the .
#' @param scale multiplier for the y values where the vlaues will be printed
#'
#' @return
#' @export
#'
#' @examples
create_p_value_geom <-
  function(plot_data,
           summary_list,
           column_list,
           type,
           p_decimal = 4,
           scale = 1.3,
           size,
           Ratio = F,
           placement_type = "absolute",
           scale_relative_sig,
           scale_relative_sig_min,
           distance_p_value_stars,
           ...) {
    if (Ratio == F) {
      if (type == "min") {
        y <-
          min(plot_data[[1]]["value"] * scale_relative_sig_min * distance_p_value_stars)
      }
      if (type == "max") {
        y <- max(plot_data[[2]]["value"] * scale_relative_sig * distance_p_value_stars)
      }
    } else {
      y <- max(plot_data["value"] * scale_relative_sig * distance_p_value_stars)
    }

    p_value_data <-
      get_p_data(plot_data, summary_list, column_list, Ratio = Ratio, ...)

    p_value_data <-
      map(p_value_data, function(p, p_decimal) {
        p[["value"]] <-
          round(p[["value"]], p_decimal)
        return(p)
      }, p_decimal)
    p_value_data <-
      map(p_value_data, function(p) {
        p[["value"]][which(p[["value"]] == 1)] <- ""
        return(p)
      })

    p_value_data <-
      map(p_value_data, function(p) {
        p[["value"]][which(p[["value"]] == 0)] <- "<0.001"
        return(p)
      })


    if (type == "min") {
      index <- 1
      func <- min
      scale_relative <- scale_relative_sig_min
      distance_p_value <- distance_p_value_stars
    }
    if (type == "max") {
      index <- 2
      func <- max
      scale_relative <- scale_relative_sig
      distance_p_value <- distance_p_value_stars * 1.2
    }
    if (Ratio == T) {
      index <- 1
      func <- max

      if (placement_type == "relative") {
        y <-
          summarise(
            group_by(plot_data, col),
            maxVal = (func(value)),
            name = unique(series)
          )
        step_y <- func(y$maxVal) * scale_relative_sig
        y <- y$maxVal + distance_p_value * step_y
      }
    } else {
      if (placement_type == "relative") {
        y <-
          summarise(
            group_by(plot_data[[index]], col),
            maxVal = (func(value)),
            name = unique(series)
          )

        y <- y[order(y$name, y$col), ]

        step_y <- func(y$maxVal) * scale_relative
        y <- y$maxVal + distance_p_value_stars * step_y
      }
    }


    p_value_data[[index]]$y <- y



    p_value_geom <-
      geom_text(
        data = p_value_data[[index]],
        aes(
          x = series,
          y = y,
          colour = col,
          label = value
        ),
        position = position_dodge2(width = 0.75),
        size = size
      )
    return(p_value_geom)
  }


#' calculate kruskal wallis test p values
#'
#' @param summary_list
#' @param column_list
#'
#' @return
#' @export
#'
#' @examples
get_kruskal_values <- function(summary_list, column_list) {
  columns <- pmap(column_list, function(...) {
    return(list(...))
  })
  columns <- map(columns, data.table::transpose)


  values <- map_depth(columns, 2, function(cols, summary) {
    map2(cols, summary, function(x, y) unlist(y[x]))
  }, summary_list)


  kruskalValues <- map(flatten(values), function(x) kruskal.test(x)$p.value)
  names(kruskalValues) <- map_chr(flatten(columns), function(x) x[1])

  kruskalValues <- map(kruskalValues, round, 4)

  return(kruskalValues)
}

#' Title
#'
#' @param plot_data
#' @param type
#' @param p_decimal
#' @param scale
#' @param size
#' @param Ratio
#' @param placement_type
#' @param scale_relative_sig
#' @param scale_relative_sig_min
#' @param distance_p_value_stars
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
create_paired_p_value_geom <- function(plot_data,
                                       type,
                                       p_decimal = 4,
                                       scale = 1.3,
                                       size,
                                       Ratio = F,
                                       placement_type = "absolute",
                                       scale_relative_sig,
                                       scale_relative_sig_min,
                                       distance_p_value_stars,
                                       ...) {
  if (Ratio == F) {
    if (type == "min") {
      y <-
        min(plot_data[[1]]["value"] * scale_relative_sig_min * distance_p_value_stars)
    }
    if (type == "max") {
      y <- max(plot_data[[2]]["value"] * scale_relative_sig * distance_p_value_stars)
    }
  } else {
    y <- max(plot_data["value"] * scale_relative_sig * distance_p_value_stars)
  }

  if (type == "min") {
    index <- 1
    func <- min
    scale_relative <- scale_relative_sig_min
    distance_p_value <- distance_p_value_stars
  }
  if (type == "max") {
    index <- 2
    func <- max
    scale_relative <- scale_relative_sig
    distance_p_value <- distance_p_value_stars * 1.2
  }
  if (Ratio == T) {
    index <- 1
    func <- max

    if (placement_type == "relative") {
      y <-
        summarise(
          group_by(plot_data, col),
          maxVal = (func(value)),
          name = unique(series)
        )
      step_y <- func(y$maxVal) * scale_relative_sig
      y <- y$maxVal + distance_p_value * step_y
    }
  } else {
    if (placement_type == "relative") {
      y <-
        summarise(
          group_by(plot_data[[index]], col),
          maxVal = (func(value)),
          name = unique(series)
        )

      y <- y[order(y$name, y$col), ]

      step_y <- func(y$maxVal) * scale_relative
      y <- y$maxVal + distance_p_value_stars * step_y
    }
  }

  if (Ratio == T) {
    plot_data$col <- word(plot_data$col, -1)
    stat.test <- plot_data %>%
      group_by(series) %>%
      rstatix::wilcox_test(value ~ col, paired = T, p.adjust.method = "none") %>%
      rstatix::add_xy_position(x = "series", dodge = 0.8)

    stat.test$signif <- stars.pval(stat.test$p)
    stat.test$y.position <- (y / stat.test$y.position[1]) * stat.test$y.position

    p_value_geom <- stat_pvalue_manual(
      stat.test,
      label = "p", tip.length = 0.01,
      bracket.nudge.y = -2, size = size
    )
    # stat_compare_means(data = plot_data, aes(x = series, y = value, color = col, label = pvalue(as.numeric(..p.format..), accuracy = 1/10^p_decimal)),label.y = y, paired = T, size = size)
    return(p_value_geom)
  }



  if (type == "min") {
    plot_data <- plot_data[[1]]
    plot_data$col <- word(plot_data$col, -1)
    stat.test <- plot_data %>%
      group_by(series) %>%
      rstatix::wilcox_test(value ~ col, paired = T, p.adjust.method = "none") %>%
      rstatix::add_xy_position(x = "series", dodge = 0.8)
    stat.test$y.position <- abs((y / stat.test$y.position[1]) * stat.test$y.position) * -1.1
    stat.test$signif <- stars.pval(stat.test$p)
  }

  if (type == "max") {
    plot_data <- plot_data[[2]]
    plot_data$col <- word(plot_data$col, -1)
    stat.test <- plot_data %>%
      group_by(series) %>%
      rstatix::wilcox_test(value ~ col, paired = T, p.adjust.method = "none") %>%
      rstatix::add_xy_position(x = "series", dodge = 0.8)
    stat.test$y.position <- (y / stat.test$y.position[1]) * stat.test$y.position
    stat.test$signif <- stars.pval(stat.test$p)
  }

  p_value_geom <- stat_pvalue_manual(
    stat.test,
    label = "p", tip.length = 0.01,
    bracket.nudge.y = -2, size = size
  )





  return(p_value_geom)
}

#' creates a geom containing significance stars for a paired test
#'
#' @param plot_data
#' @param type
#' @param p_decimal
#' @param scale
#' @param size
#' @param Ratio
#' @param placement_type
#' @param scale_relative_sig
#' @param scale_relative_sig_min
#' @param distance_p_value_stars
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
create_paired_p_stars_geom <- function(plot_data,
                                       type,
                                       p_decimal = 4,
                                       scale = 1.3,
                                       size,
                                       Ratio = F,
                                       placement_type = "absolute",
                                       scale_relative_sig,
                                       scale_relative_sig_min,
                                       distance_p_value_stars,
                                       ...) {
  if (Ratio == F) {
    if (type == "min") {
      y <-
        min(plot_data[[1]]["value"] * scale_relative_sig_min)
    }
    if (type == "max") {
      y <- max(plot_data[[2]]["value"] * scale_relative_sig)
    }
  } else {
    y <- max(plot_data["value"] * scale_relative_sig)
  }

  if (type == "min") {
    index <- 1
    func <- min
    scale_relative <- scale_relative_sig_min
  }
  if (type == "max") {
    index <- 2
    func <- max
    scale_relative <- scale_relative_sig
  }
  if (Ratio == T) {
    index <- 1
    func <- max

    if (placement_type == "relative") {
      y <-
        summarise(
          group_by(plot_data, col),
          maxVal = (func(value)),
          name = unique(series)
        )
      step_y <- func(y$maxVal) * scale_relative_sig
      y <- y$maxVal * step_y
    }
  } else {
    if (placement_type == "relative") {
      y <-
        summarise(
          group_by(plot_data[[index]], col),
          maxVal = (func(value)),
          name = unique(series)
        )

      y <- y[order(y$name, y$col), ]

      step_y <- func(y$maxVal) * scale_relative
      y <- y$maxVal + step_y
    }
  }


  if (Ratio == T) {
    plot_data$col <- word(plot_data$col, -1)
    stat.test <- plot_data %>%
      group_by(series) %>%
      rstatix::wilcox_test(value ~ col, paired = T, p.adjust.method = "none") %>%
      rstatix::add_xy_position(x = "series", dodge = 0.8)

    stat.test$y.position <- (y / stat.test$y.position[1]) * stat.test$y.position
    stat.test$signif <- stars.pval(stat.test$p)

    p_value_geom <- stat_pvalue_manual(
      stat.test,
      label = "signif", tip.length = 0.01, hide.ns = TRUE,
      bracket.nudge.y = -2, size = size
    )
    # stat_compare_means(data = plot_data, aes(x = series, y = value, color = col, label = pvalue(as.numeric(..p.format..), accuracy = 1/10^p_decimal)),label.y = y, paired = T, size = size)
    return(p_value_geom)
  }



  if (type == "min") {
    plot_data <- plot_data[[1]]
    plot_data$col <- word(plot_data$col, -1)
    stat.test <- plot_data %>%
      group_by(series) %>%
      rstatix::wilcox_test(value ~ col, paired = T, p.adjust.method = "none") %>%
      rstatix::add_xy_position(x = "series", dodge = 0.8)
    stat.test$y.position <- abs((y / stat.test$y.position[1]) * stat.test$y.position) * -1.1
    stat.test$signif <- stars.pval(stat.test$p)
  }

  if (type == "max") {
    plot_data <- plot_data[[2]]
    plot_data$col <- word(plot_data$col, -1)
    stat.test <- plot_data %>%
      group_by(series) %>%
      rstatix::wilcox_test(value ~ col, paired = T, p.adjust.method = "none") %>%
      rstatix::add_xy_position(x = "series", dodge = 0.8)
    stat.test$y.position <- (y / stat.test$y.position[1]) * stat.test$y.position
    stat.test$signif <- stars.pval(stat.test$p)
  }

  p_value_geom <- stat_pvalue_manual(
    stat.test,
    label = "signif", tip.length = 0.01, hide.ns = TRUE,
    bracket.nudge.y = -2, size = size
  )
}


#' creates a geom containing significance stars
#'
#' @param plot_data
#' @param summary_list
#' @param column_list
#' @param type "min" or "max"
#' @param scale multiplier for the y values where the vlaues will be printed
#'
#' @return
#' @export
#'
#' @examples
create_p_stars_geom <-
  function(plot_data,
           summary_list,
           column_list,
           type,
           scale = 1.1,
           size,
           Ratio = F,
           placement_type = "absolute",
           scale_relative_sig = 0.2,
           scale_relative_sig_min = 2,
           ...) {
    if (Ratio == F) {
      if (type == "min") {
        y <- min(plot_data[[1]]["value"] * scale_relative_sig_min)
      }
      if (type == "max") {
        y <- max(plot_data[[2]]["value"] * scale_relative_sig)
      }
    } else {
      y <- max(plot_data["value"] * scale)
    }


    p_value_data <-
      get_p_data(plot_data, summary_list, column_list, Ratio = Ratio, ...)

    p_value_data <-
      map(p_value_data, function(p) {
        p[["value"]][which(p[["value"]] == 1)] <- NA
        return(p)
      })

    p_stars_data <-
      map(p_value_data, function(p) {
        p[["value"]] <- gtools::stars.pval(p[["value"]])
        return(p)
      })

    if (type == "min") {
      index <- 1
      func <- min
      scale_relative <- scale_relative_sig_min
    }
    if (type == "max") {
      index <- 2
      func <- max
      scale_relative <- scale_relative_sig
    }
    if (Ratio == T) {
      index <- 1
      func <- max

      if (placement_type == "relative") {
        y <-
          summarise(
            group_by(plot_data, col),
            maxVal = (func(value)),
            name = unique(series)
          )
        step_y <- func(y$maxVal) * scale_relative_sig
        y <- y$maxVal + step_y
      }
    } else {
      if (placement_type == "relative") {
        y <-
          summarise(
            group_by(plot_data[[index]], col),
            maxVal = (func(value)),
            name = unique(series)
          )
        y <- y[order(y$name, y$col), ]

        step_y <- func(y$maxVal) * scale_relative
        y <- y$maxVal + step_y
      }
    }


    p_stars_data[[index]]$y <- y
    p_stars_geom <-
      geom_text(
        data = p_stars_data[[index]],
        aes(
          x = series,
          y = y,
          colour = col,
          label = value
        ),
        position = position_dodge2(width = 0.75),
        size = size * 1.5
      )


    return(p_stars_geom)
  }
