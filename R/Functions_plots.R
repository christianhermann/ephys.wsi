#' Creates a matplot showing all IVs of a measurement at once
#'
#' @param IV_list A List containing tibbles with IVs
#' @param column The column shown in the plot
#' @param filter The Pharmacon shown in the plot
#' @param measurement Name of the measurement for color and legend
#' @param y_lab Name for the y-Axis
#' @param title Title of the measurement
#' @param line_size line size
#'
#' @return
#' @export
#'
#' @examples create_matplot(IV_list = IV_list$hTRPC6, column = "normalized_slopeConductance",
#' filter = "OAG", measurement = "TRPC6 OAG", ylab = "Slope", title  ="Comparision C6 C5")
create_matplot <-
  function(IV_list,
           column,
           filter,
           measurement,
           y_lab = "",
           title = "",
           line_size = 1) {
    check_list(IV_list)
    check_string(column)
    check_string(filter)
    check_string(measurement)
    column_tbl <- tbl_list_to_column_tbl(IV_list, column)
    filtered_tbl <- filter_tbl_columns(column_tbl, filter)
    plot_data <- get_matplot_data(filtered_tbl)
    plot_base <- matplot_base(y_lab, title)
    geom <-
      create_matplot_geom_line(plot_data, measurement, line_size)
    gg_plot <- combine_plot_with_geom(plot_base, geom)
    # matplot_envir$counter <- 1
    return(gg_plot)
  }

#' Adds a new plot to an old matplot
#'
#' @param old_plot A ggplot
#' @param IV_list A List containing tibbles with IVs
#' @param column The column shown in the plot
#' @param filter The Pharmacon shown in the plot
#' @param measurement Name of the measurement for color and legend
#' @param line_size
#'
#' @return
#' @export
#'
#' @examples add_to_matplot(old_plot = ggplot1, IV_list = IV_list$hTRPC5, column = "normalized_slopeConductance",
#' filter = "EA", measurement = "TRPC5 EA")
add_to_matplot <-
  function(old_plot,
           IV_list,
           column,
           filter,
           measurement,
           line_size = 1) {
    check_list(IV_list)
    check_string(column)
    check_string(filter)
    check_string(measurement)
    column_tbl <- tbl_list_to_column_tbl(IV_list, column)
    filtered_tbl <- filter_tbl_columns(column_tbl, filter)
    plot_data <- get_matplot_data(filtered_tbl)
    geom <-
      create_matplot_geom_line(plot_data, measurement, line_size)
    gg_plot <- combine_plot_with_geom(old_plot, geom)
    return(gg_plot)
  }


#' Creates a median plot showing the median IV of an measurement and the SD
#'
#' @param IV_list A List containing tibbles with IVs
#' @param column The column shown in the plot
#' @param filter The Pharmacon shown in the plot
#' @param measurement Name of the measurement for color and legend
#' @param y_lab Name for the y-Axis
#' @param title Title of the measurement
#'
#' @return
#' @export
#'
#' @examples create_median_plot(IV_list = IV_list$hTRPC6, column = "normalized_slopeConductance",
#' filter = "OAG", measurement = "TRPC6 OAG", ylab = "Slope", title  ="Comparision C6 C5")
create_median_plot <-
  function(IV_list,
           column,
           filter,
           measurement,
           y_lab = "",
           title = "",
           line_size = 2,
           sdMAD,
           sdMAD_shadow_dir)
  {
    check_list(IV_list)
    check_string(column)
    check_string(filter)
    check_string(measurement)
    column_tbl <- tbl_list_to_column_tbl(IV_list, column)
    filtered_tbl <- filter_tbl_columns(column_tbl, filter)
    plot_data <- get_median_plot_data(filtered_tbl, sdMAD = sdMAD)
    plot_base <- medianplot_base(y_lab, title)
    geom_median <-
      create_median_plot_geom_line(plot_data,
                                   measurement,
                                   size = line_size,
                                   show_legend = T)
    geom_SD <-
      create_median_plot_geom_ribbon(plot_data, measurement, sdMAD_shadow_dir, show_legend = T)
    gg_plot <- combine_plot_with_geom(plot_base, geom_median)
    gg_plot <- combine_plot_with_geom(gg_plot, geom_SD)
    return(gg_plot)
  }



#' Adds a new plot to an old median plot
#'
#' @param old_plot A ggplot
#' @param IV_list A List containing tibbles with IVs
#' @param column The column shown in the plot
#' @param filter The Pharmacon shown in the plot
#' @param measurement Name of the measurement for color and legend
#' @param line_size
#' @param sdMAD
#' @param sdMAD_shadow_dir
#' @param show_legend
#'
#' @return
#' @export
#'
#' @examples add_to_median_plot(old_plot = ggplot1, IV_list = IV_list$hTRPC5,
#' column = "normalized_slopeConductance",
#' filter = "EA", measurement = "TRPC5 EA")
add_to_median_plot <-
  function(old_plot,
           IV_list,
           column,
           filter,
           measurement,
           line_size = 2,
           sdMAD,
           sdMAD_shadow_dir,
           show_legend = F)
  {
    check_list(IV_list)
    check_string(column)
    check_string(filter)
    check_string(measurement)

    column_tbl <- tbl_list_to_column_tbl(IV_list, column)
    filtered_tbl <- filter_tbl_columns(column_tbl, filter)
    plot_data <- get_median_plot_data(filtered_tbl, sdMAD = sdMAD)
    geom_median <-
      create_median_plot_geom_line(plot_data, measurement, size = line_size, show_legend)
    geom_SD <-
      create_median_plot_geom_ribbon(plot_data, measurement, sdMAD_shadow_dir, show_legend)
    gg_plot <- combine_plot_with_geom(old_plot, geom_median)
    gg_plot <- combine_plot_with_geom(gg_plot, geom_SD)
    return(gg_plot)
  }


#' Creates a plot showing a single IV
#'
#' @param IV_tbl
#' @param column
#' @param measurement
#' @param y_lab
#' @param title
#'
#' @return
#' @export
#'
#' @examples create_single_plot(IV_tbl =
#' IV_list$hTRPC6$hTRPC6_pIRES2_EGFP_519_B1P16_OAG_100(3)_Bef,
#' measurement = "TRPC6 OAG 100(3)", ylab = "IV", title  ="Comparision C6 C5")
create_single_plot <-
  function(IV_tbl,
           column,
           measurement,
           y_lab = "",
           title = "",
           linesize = 1)
  {
    check_tibble(IV_tbl)
    check_string(measurement)
    IV_tbl <- pull(IV_tbl, column)
    plot_data <- get_single_plot_data(IV_tbl)
    plot_base <- singleplot_base(y_lab, title)
    geom_single <-
      create_single_plot_geom_line(plot_data, measurement, size = linesize)

    gg_plot <- combine_plot_with_geom(plot_base, geom_single)
    return(gg_plot)
  }

#' Adds a new single Plot to an old plot
#'
#' @param old_plot
#' @param IV_tbl
#' @param column
#' @param measurement
#' @param linesize
#'
#' @return
#' @export
#'
#' @examples add_to_single_plot(old_plot = ggplot1,
#' IV_tbl = IV_list$mTRPC6$mTRPC6_WT_IRES_EGFP_OAG(100µM)_1(3)_OAG,
#' measurement = "mTRPC6 1 3 OAG")

add_to_single_plot <-
  function(old_plot,
           IV_tbl,
           column,
           measurement,
           linesize)
  {
    check_tibble(IV_tbl)
    check_string(measurement)
    IV_tbl <- pull(IV_tbl, column)
    plot_data <- get_single_plot_data(IV_tbl)
    geom_single <-
      create_single_plot_geom_line(plot_data, measurement, size = linesize)

    gg_plot <- combine_plot_with_geom(old_plot, geom_single)
    return(gg_plot)
  }




#' Calculates the P-Values of the columns of two series/list and plots it
#'
#' @param tbl_list
#' @param measurement
#' @param column
#' @param filter
#' @param y_lab
#' @param title
#'
#' @return
#' @export
#'
#' @examples create_p_plot(IV_list,"Test", "CurrentDensity[pA/pF]", c("OAG","EngA"))
create_p_plot <-
  function(tbl_list,
           measurement,
           column,
           filters,
           y_lab = "",
           title = "")
  {
    check_list(tbl_list)
    check_string(column)
    check_string(filters)
    check_string(measurement)


    column_tbl <- map(tbl_list, tbl_list_to_column_tbl, column)
    filtered_tbl <- map2(column_tbl, filters, filter_tbl_columns)
    # names_tbl <- names(filtered_tbl)
    names(filtered_tbl) <- c("tbl1", "tbl2")
    p_vec <- invoke(U_test_tbl, filtered_tbl)

    plot_data <- get_p_plot_data(p_vec)
    plot_base <- p_plot_base(y_lab, title)
    geom_p <-
      create_p_plot_geom_line(plot_data, measurement)

    gg_plot <- combine_plot_with_geom(plot_base, geom_p)
    gg_plot <- change_y_axis_to_log(gg_plot)
    return(gg_plot)
  }

#' Adds a p_value plot to an old plot
#'
#' @param old_plot
#' @param IV_list
#' @param column
#' @param filters
#' @param measurement
#'
#' @return
#' @export
#'
#' @examples add_to_p_plot(oldplot,,IV_list,"Test", "CurrentDensity[pA/pF]", c("OAG","EngA"))
add_to_p_plot <-
  function(old_plot,
           IV_list,
           column,
           filters,
           measurement) {
    check_list(IV_list)
    check_string(column)
    check_string(filter)
    check_string(measurement)
    column_tbl <- map(IV_list, tbl_list_to_column_tbl, column)
    filtered_tbl <- map2(column_tbl, filters, filter_tbl_columns)
    # names_tbl <- names(filtered_tbl)
    # names(filtered_tbl) <- c("tbl1","tbl2")
    p_vec <- invoke(U_test_tbl, filtered_tbl)

    plot_data <- get_p_plot_data(p_vec)
    geom_p <-
      create_p_plot_geom_line(plot_data, measurement)
    gg_plot <- combine_plot_with_geom(old_plot, geom_p)
    return(gg_plot)
  }


#' Calculates the P-Values of the columns of two series/list and plots it as an area plot
#'
#' @param tbl_list
#' @param measurement
#' @param column
#' @param filter
#' @param y_lab
#' @param title
#'
#' @return
#' @export
#'
#' @examples create_p_plot(IV_list,"Test", "CurrentDensity[pA/pF]", c("OAG","EngA"))
create_p_area_plot <-
  function(tbl_list,
           measurement,
           column,
           filters,
           y_lab = "",
           title = "",
           kruskal = F,
           paired = F,
           ...)
  {    check_list(tbl_list)

    check_string(column)
    check_string(filters)
    check_string(measurement)


    column_tbl <- map(tbl_list, tbl_list_to_column_tbl, column)
    filtered_tbl <- map2(column_tbl, filters, filter_tbl_columns)
    # names_tbl <- names(filtered_tbl)
    names(filtered_tbl) <- c("tbl1", "tbl2")

    if (kruskal == T)
      p_vec <- kruskal_test_tbl(filtered_tbl)
    if (kruskal == F)
      p_vec <- invoke(U_test_tbl, filtered_tbl, paired = paired)

    plot_data <- get_p_plot_data(p_vec)
    plot_base <- p_plot_base(y_lab, title)
    geom_p <-
      create_p_plot_geom_area(plot_data, measurement)

    gg_plot <- combine_plot_with_geom(plot_base, geom_p)
    gg_plot <- change_y_axis_to_log(gg_plot)
    return(gg_plot)
  }




#' Creates a boxplot out of a list of summarys and columns
#'
#' @param summary_list
#' @param column_list Same format as the output of add_peaks
#' @param y_lab
#' @param title
#' @param jitter set true if a jitter plot should be included
#' @param violin set true if a violin plot should be inculded
#' @param alphaV transparency of the jitter and violin plots
#' @param errorbar
#' @param observation  set true if the amount of observations should be included
#' @param p_value  set true if the p_values should be included (u-Test)
#' @param significance set true if significance stars should be included
#'
#' @return
#' @export
#'
#' @examples create_boxplot(data_storage_envir$summary_list, data_storage_envir$column_list)
create_boxplot <- function(summary_list,
                           column_list,
                           errorbar = T,
                           jitter = T,
                           violin = F,
                           observation = T,
                           p_value = T,
                           significance = T,
                           showMedian = F,
                           alphaV = 0.5,
                           y_lab = "",
                           title = "",
                           lwd = 1,
                           size = 1,
                           jittersize = 1,
                           placement_type = "absolute",
                           scale_relative = 1.2,
                           scale_relative_sig = 0.2,
                           scale_relative_sig_min = 0.6,
                           distance_p_value_stars = 1.2,
                           kruskal = F,
                           compIntern = F,

                           ...) {
  check_list(summary_list)
  check_list(column_list)
  plot_data <- get_boxplot_data(summary_list, column_list)
  plot_base <- boxplot_base(y_lab, title)

  if (errorbar == T) {
    geom_error_min <- create_errorbar_geom(plot_data[[1]], lwd)
    geom_error_max <- create_errorbar_geom(plot_data[[2]], lwd)
    gg_plot <- combine_plot_with_geom(plot_base, geom_error_min)
    gg_plot <- combine_plot_with_geom(gg_plot, geom_error_max)
  }

  geom_box_min <-
    create_boxplot_geom(plot_data[[1]], lwd)
  geom_box_max <-
    create_boxplot_geom(plot_data[[2]], lwd)

  if (errorbar == TRUE)
    gg_plot <- combine_plot_with_geom(gg_plot, geom_box_min)
  if (errorbar == FALSE)
    gg_plot <- combine_plot_with_geom(plot_base, geom_box_min)
  gg_plot <- combine_plot_with_geom(gg_plot, geom_box_max)



  if (jitter == T) {
    geom_jitter_min <-
      create_jitter_geom(plot_data[[1]], alphaV, jitter.width = 0.45, jittersize)
    geom_jitter_max <-
      create_jitter_geom(plot_data[[2]], alphaV, jitter.width = 0.45, jittersize)
    gg_plot <- combine_plot_with_geom(gg_plot, geom_jitter_min)
    gg_plot <- combine_plot_with_geom(gg_plot, geom_jitter_max)
  }
  if (violin == T) {
    geom_violin_min <- create_violin_geom(plot_data[[1]], alphaV)
    geom_violin_max <- create_violin_geom(plot_data[[2]], alphaV)
    gg_plot <- combine_plot_with_geom(gg_plot, geom_violin_min)
    gg_plot <- combine_plot_with_geom(gg_plot, geom_violin_max)
  }
  if (observation == T) {
    #   geom_obs_max <- create_observation_number_geom(plot_data[[2]], "max",size = size,placement_type = placement_type, scale_relative = scale_relative)
    geom_obs_max <-
      create_observation_number_geom(plot_data[[2]],
                                     "max",
                                     size = size,
                                     scale_relative = scale_relative)

    gg_plot <- combine_plot_with_geom(gg_plot, geom_obs_max)
  }

  if (showMedian == T) {
    geom_median_max <-
      create_median_number_geom(plot_data[[2]],
                                     "max",
                                     size = size,
                                     scale_relative = scale_relative)
    geom_median_min <-
      create_median_number_geom(plot_data[[1]],
                                     "min",
                                     size = size,
                                     scale_relative = scale_relative)

    gg_plot <- combine_plot_with_geom(gg_plot, geom_median_max)
    gg_plot <- combine_plot_with_geom(gg_plot, geom_median_min)

  }

  if (p_value == T) {
    if (kruskal == F && compIntern == F) {
      geom_p_value_min <-
        create_p_value_geom(
          plot_data,
          summary_list,
          column_list,
          "min",
          size = size,
          placement_type = placement_type,
          scale_relative_sig = scale_relative_sig,
          scale_relative_sig_min = scale_relative_sig_min,
          distance_p_value_stars = distance_p_value_stars,
          ...
        )
      geom_p_value_max <-
        create_p_value_geom(
          plot_data,
          summary_list,
          column_list,
          "max",
          size = size,
          placement_type = placement_type,
          scale_relative_sig = scale_relative_sig,
          scale_relative_sig_min = scale_relative_sig_min,
          distance_p_value_stars = distance_p_value_stars,
          ...
        )
      gg_plot <- combine_plot_with_geom(gg_plot, geom_p_value_min)
      gg_plot <- combine_plot_with_geom(gg_plot, geom_p_value_max)
    }


    if (kruskal == T) {
      kruskal_values <- get_kruskal_values(summary_list, column_list)
      k <- 1
      j <- 1
      for (i in 1:length(kruskal_values))
      {
        gg_plot <-
          gg_plot + annotate(
            "text",
            c(0.8, 1.2, 1.8, 2.2, 2.8, 3.2)[j],
            y = k * max(
              plot_data[[2]]["value"] * scale_relative_sig * distance_p_value_stars
            ),
            label = kruskal_values[[i]]
          )
        k <- k * -1
        if (i %% 2 == 0) {
          j <- j + 1
        }
      }
    }

    if (compIntern == T) {


      geom_p_value_min <- create_paired_p_value_geom(plot_data,"min", size = size, placement_type = placement_type,
                                                     scale_relative_sig = scale_relative_sig,
                                                     scale_relative_sig_min = scale_relative_sig_min,
                                                     distance_p_value_stars = distance_p_value_stars,
                                                     ...)
      geom_p_value_max <- create_paired_p_value_geom(plot_data,"max", size = size, placement_type = placement_type,
                                                     scale_relative_sig = scale_relative_sig,
                                                     scale_relative_sig_min = scale_relative_sig_min,
                                                     distance_p_value_stars = distance_p_value_stars,
                                                     ...)

      gg_plot <- combine_plot_with_geom(gg_plot, geom_p_value_min)
      gg_plot <- combine_plot_with_geom(gg_plot, geom_p_value_max)
    }
  }


  if (significance == T) {
    if (kruskal == F && compIntern == F) {
      geom_p_stars_min <-
        create_p_stars_geom(
          plot_data,
          summary_list,
          column_list,
          "min",
          size = size,
          placement_type = placement_type,
          scale_relative_sig = scale_relative_sig,
          scale_relative_sig_min = scale_relative_sig_min,
          ...
        )
      geom_p_stars_max <-
        create_p_stars_geom(
          plot_data,
          summary_list,
          column_list,
          "max",
          size = size,
          placement_type = placement_type,
          scale_relative_sig = scale_relative_sig,
          scale_relative_sig_min = scale_relative_sig_min,
          ...
        )
      gg_plot <- combine_plot_with_geom(gg_plot, geom_p_stars_min)
      gg_plot <- combine_plot_with_geom(gg_plot, geom_p_stars_max)
    }
    if (kruskal == T) {
      kruskal_values <- get_kruskal_values(summary_list, column_list)
      kruskal_values <- map_chr(kruskal_values, gtools::stars.pval)

      k <- 1
      j <- 1
      for (i in 1:length(kruskal_values))
      {
        gg_plot <-
          gg_plot + annotate(
            "text",
            c(0.8, 1.2, 1.8, 2.2, 2.8, 3.2)[j],
            y = k * max(
              plot_data[[2]]["value"] * scale_relative_sig * distance_p_value_stars
            ),
            label = kruskal_values[[i]]
          )
        k <- k * -1
        if (i %% 2 == 0) {
          j <- j + 1
        }
      }
    }
    if (compIntern == T) {


      geom_p_stars_min <- create_paired_p_stars_geom(plot_data,"min", size = size, placement_type = placement_type,
                                                     scale_relative_sig = scale_relative_sig,
                                                     scale_relative_sig_min = scale_relative_sig_min,
                                                     distance_p_value_stars = distance_p_value_stars,
                                                     ...)
      geom_p_stars_max <- create_paired_p_stars_geom(plot_data,"max", size = size, placement_type = placement_type,
                                                     scale_relative_sig = scale_relative_sig,
                                                     scale_relative_sig_min = scale_relative_sig_min,
                                                     distance_p_value_stars = distance_p_value_stars,
                                                     ...)

      gg_plot <- combine_plot_with_geom(gg_plot, geom_p_stars_min)
      gg_plot <- combine_plot_with_geom(gg_plot, geom_p_stars_max)
    }
  }


  return(gg_plot)
}


#' Creates a boxplot displaying the summary Ratios
#'
#' @param summary_list
#' @param peak_list format, same as from data_storage_envir$peak_list
#' @param jitter
#' @param alphaV
#' @param y_lab
#' @param title
#' @param observation
#' @param lwd
#' @param jittersize
#' @param p_value
#' @param significance
#' @param size
#' @param placement_type
#' @param scale_relative
#' @param scale_relative_sig
#' @param scale_relative_sig_min
#' @param distance_p_value_stars
#' @param kruskal
#' @param compIntern
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
create_ratio_plot <- function(summary_list,
                              peak_list,
                              jitter = T,
                              observation = T,
                              alphaV = 0.5,
                              y_lab = "",
                              title = "",
                              lwd = 1,
                              jittersize = 1,
                              p_value = T,
                              significance = T,
                              showMedian = F,
                              size = 1,
                              placement_type = "absolute",
                              scale_relative = 1.2,
                              scale_relative_sig = 0.1,
                              scale_relative_sig_min = 0.5,
                              distance_p_value_stars = 1.2,
                              kruskal = F,
                              compIntern = F,
                              ...) {
  check_list(summary_list)
  check_list(peak_list)
  plot_data <- get_ratio_plot_data(summary_list, peak_list)
  plot_base <- boxplot_base(y_lab, title, ratio = T)

  geom_error <- create_errorbar_geom(plot_data, lwd)
  gg_plot <- combine_plot_with_geom(plot_base, geom_error)

  geom_box <- create_boxplot_geom(plot_data, lwd)
  gg_plot <- combine_plot_with_geom(gg_plot, geom_box)

  if (jitter == T) {
    geom_jitter <-
      create_jitter_geom(plot_data,
                         alphaV,
                         jitter.width = 0.45,
                         jittersize = jittersize)
    gg_plot <- combine_plot_with_geom(gg_plot, geom_jitter)


  }
  if (observation == T) {
    geom_obs <-
      create_observation_number_geom(plot_data,
                                     "max",
                                     size = size,
                                     scale_relative = scale_relative)
    gg_plot <- combine_plot_with_geom(gg_plot, geom_obs)
  }

  if (showMedian == T) {
    #   geom_obs_max <- create_observation_number_geom(plot_data[[2]], "max",size = size,placement_type = placement_type, scale_relative = scale_relative)
    geom_median_max <-
      create_median_number_geom(plot_data,
                                     "max",
                                     size = size,
                                     scale_relative = scale_relative)
    # geom_median_min <-
    #   create_observation_number_geom(plot_data,
    #                                  "min",
    #                                  size = size,
    #                                  scale_relative = scale_relative)

    gg_plot <- combine_plot_with_geom(gg_plot, geom_median_max)
  }

  if (p_value == T) {
    if (kruskal == F && compIntern == F) {
      geom_p_value_max <-
        create_p_value_geom(
          plot_data,
          summary_list,
          map(peak_list, function(peak)
            paste0("Ratio_", unlist(peak))),
          "max",
          size = size,
          Ratio = T,
          scale_relative_sig = scale_relative_sig,
          scale_relative_sig_min = scale_relative_sig_min,
          distance_p_value_stars = distance_p_value_stars,
          ...
        )
      gg_plot <- combine_plot_with_geom(gg_plot, geom_p_value_max)
    }
    if (kruskal == T)
    {
      kruskal_values <- get_kruskal_values(summary_list, map(peak_list, function(peak)
        paste0("Ratio_", unlist(peak))))
      for (i in 1:length(kruskal_values))
      {
        gg_plot <-
          gg_plot + annotate(
            "text",
            c(0.8, 1.2, 1.8, 2.2, 2.8, 3.2)[i],
            y =   max(
              plot_data["value"] * scale_relative_sig * distance_p_value_stars
            ),
            label = kruskal_values[[i]]
          )
      }

    }
    if (compIntern == T) {
      geom_p_value_max <- create_paired_p_value_geom(plot_data,"max", size = size, placement_type = placement_type,
                                                     scale_relative_sig = scale_relative_sig,
                                                     scale_relative_sig_min = scale_relative_sig_min,
                                                     distance_p_value_stars = distance_p_value_stars,
                                                     Ratio = T,
                                                     ...)

      gg_plot <- combine_plot_with_geom(gg_plot, geom_p_value_max)
    }


  }
  if (significance == T) {
    if (kruskal == F && compIntern == F) {

      geom_p_stars_max <-
        create_p_stars_geom(
          plot_data,
          summary_list,
          map(peak_list, function(peak)
            paste0("Ratio_", unlist(peak))),
          "max",
          size = size,
          Ratio = T,
          placement_type = placement_type,
          scale_relative = scale_relative_sig,
          scale_relative_sig_min = scale_relative_sig_min,
          ...
        )
      gg_plot <- combine_plot_with_geom(gg_plot, geom_p_stars_max)
    }

    if (kruskal == T)
    {
      kruskal_values <- get_kruskal_values(summary_list,  map(peak_list, function(peak)
        paste0("Ratio_", unlist(peak))))
      kruskal_values <- map_chr(kruskal_values, gtools::stars.pval)


      for (i in 1:length(kruskal_values))
      {
        gg_plot <-
          gg_plot + annotate(
            "text",
            c(0.8, 1.2, 1.8, 2.2, 2.8, 3.2)[i],
            y =   max(
              plot_data["value"] * scale_relative_sig * distance_p_value_stars
            )[1],
            label = kruskal_values[[i]]
          )
      }

    }

    if (compIntern == T) {
      geom_p_stars_max <- create_paired_p_stars_geom(plot_data,"max", size = size, placement_type = placement_type,
                                                     scale_relative_sig = scale_relative_sig,
                                                     scale_relative_sig_min = scale_relative_sig_min,
                                                     distance_p_value_stars = distance_p_value_stars, Ratio = T,
                                                     ...)

      gg_plot <- combine_plot_with_geom(gg_plot, geom_p_stars_max)

    }
  }
  return(gg_plot)
}

#' Wrapper for ggsave
#'
#' @param filename
#' @param path
#' @param device
#' @param width
#' @param heigth
#' @param dpi
#'
#' @return
#' @export
#'
#' @examples
save_actual_plot <-
  function(filename,
           path,
           plot = last_plot(),
           device = "png",
           width = 8,
           height = 5,
           dpi = 600) {
    ggsave(
      filename = paste0(gsub("[^[:alnum:]=\\.]", "", filename), ".", device),
      path = path,
      plot = plot,
      device = device,
      width = width,
      height = height,
      dpi = dpi
    )
    print(paste0(path, filename, " saved."))
  }
