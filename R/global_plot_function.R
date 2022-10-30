

#' Title
#'
#' @param plot_type
#' @param IV_list
#' @param summary_list
#' @param column_list
#' @param peak_list
#' @param series_vector
#' @param measurement_name
#' @param display_N_legend
#' @param SD_MAD
#' @param SD_MAD_shadow_direction
#' @param used_theme
#' @param used_axis_function
#' @param used_font
#' @param used_fontsize
#' @param linesize
#' @param used_colors
#' @param save_dir
#' @param save_name
#' @param save_width
#' @param save_height
#' @param save_unit
#' @param legend_seperate
#' @param ...
#' @param ylab
#' @param display_N_legend_divisor
#' @param spacer_plot
#' @param size
#' @param jittersize
#' @param n_spacer
#' @param ratio_Median_P_plot
#' @param overrideYlim
#' @param splitted
#' @param customYaxis
#' @param splitPotential
#'
#' @return
#' @export
#'
#' @examples
create_plot <-
  function(plot_type = NULL,
           IV_list = NULL,
           summary_list = NULL ,
           column_list = NULL ,
           peak_list = NULL ,
           series_vector = NULL ,
           ylab = NULL,
           measurement_name = NULL,
           display_N_legend = NULL,
           display_N_legend_divisor = NULL,
           SD_MAD = NULL,
           SD_MAD_shadow_direction = NULL,
           used_theme = NULL ,
           used_axis_function = NULL ,
           used_font = NULL ,
           used_fontsize = NULL ,
           linesize = NULL ,
           used_colors = NULL,
           save_dir = NULL ,
           save_name = NULL ,
           save_width = NULL ,
           save_height = NULL ,
           save_unit = "mm",
           legend_seperate = FALSE,
           spacer_plot = FALSE,
           size = NULL,
           jittersize = NULL,
           n_spacer = 0,
           ratio_Median_P_plot = NULL,
           overrideYlim = NULL,
           customYaxis = FALSE,
           splitted = FALSE,
           splitPotential = 0,
           ...) {
    if (!is.null(display_N_legend)) {
      if (display_N_legend == TRUE && !invalid(measurement_name))
        measurement_name <-
          paste0(
            measurement_name,
            " (n=",
            map_dbl(series_vector, function(x, y)
              length(y[[x]]) / display_N_legend_divisor, IV_list),
            ")"
          )

      if (display_N_legend == TRUE && invalid(measurement_name))
        measurement_name <-
          paste0(
            unlist(series_vector),
            " (n=",
            map_dbl(series_vector, function(x, y)
              length(y[[x]]) / display_N_legend_divisor, IV_list),
            ")"
          )
    }

    try(splitPotential <- data_storage_envir$splitPotential)
    if(n_spacer > 0) spacer_plot = T


    #####Boxplot#####
    if (plot_type == "Boxplot") {
      finished_plot <- box_plot(
        summary_list = summary_list,
        column_list = column_list,
        peak_list = peak_list,
        series_vector = series_vector,
        ylab = ylab,
        measurement_name = measurement_name,
        display_N_legend = display_N_legend,
        display_N_legend_divisor = 2,
        linesize = linesize,
        used_fontsize = used_fontsize,
        used_font = used_font,
        used_colors = used_colors,
        used_axis_function = used_axis_function,
        used_theme = used_theme,
        legend_seperate = legend_seperate,
        spacer_plot = spacer_plot,
        jittersize = jittersize,
        size = size,
        ...
      )
    }

    #####Ratio#####
    if (plot_type == "RatioPlot") {
      finished_plot <- ratio_plot(
        summary_list = summary_list,
        column_list = column_list,
        peak_list = peak_list,
        series_vector = series_vector,
        ylab = ylab,
        measurement_name = measurement_name,
        display_N_legend = display_N_legend,
        display_N_legend_divisor = 2,
        linesize = linesize,
        used_fontsize = used_fontsize,
        used_font = used_font,
        used_colors = used_colors,
        used_axis_function = used_axis_function,
        used_theme = used_theme,
        legend_seperate = legend_seperate,
        spacer_plot = spacer_plot,
        jittersize = jittersize,
        size = size,
        ...
      )
    }

    #####Singe IV Plot#####
    if (plot_type == "SingleIV") {
      if (splitted == TRUE)
      {
        switch(
          column_list,
          "normalized_slopeConductance" = {
            column_list <-
              c(
                "normalized_slopeConductance_Inward",
                "normalized_slopeConductance_Outward"
              )
          },
          "normalized_CurrentDensity" = {
            column_list <-
              c("normalized_CurrentDensity_Inward",
                "normalized_CurrentDensity_Outward")
          },
          "fitted_normalized_CurrentDensity" = {
            column_list <-
              c(
                "fitted_normalized_CurrentDensity_Inward",
                "fitted_normalized_CurrentDensity_Outward"
              )
          },
          "CurrentDensity[pA/pF]" = {
            column_list <-
              c("CurrentDensity[pA/pF]",
                "CurrentDensity[pA/pF]")
          },
          {

          }
        )


        finished_plotInward <-

          single_plot(
            IV_list = IV_list,
            column_list = column_list[1],
            measurement_name = measurement_name,
            ylab = ylab,
            used_theme = used_theme,
            used_axis_function = used_axis_function,
            used_font =  used_font,
            used_fontsize = used_fontsize,
            linesize = linesize,
            used_colors = used_colors,
            overrideYlim = overrideYlim,
            xlims =  c(settings_envir$ramp_data[1] * 1.04 , splitPotential),
            xend1 = settings_envir$ramp_data[1] * 1.04 ,
            xends2 = c(settings_envir$ramp_data[1], splitPotential)
          ) +  theme(legend.position = "none")

        finished_plotOutward <-

          single_plot(
            IV_list = IV_list,
            column_list = column_list[2],
            measurement_name = measurement_name,
            ylab = ylab,
            used_theme = used_theme,
            used_axis_function = used_axis_function,
            used_font =  used_font,
            used_fontsize = used_fontsize,
            linesize = linesize,
            used_colors = used_colors,
            overrideYlim = overrideYlim,
            xlims =  c(splitPotential - abs((
              settings_envir$ramp_data[2] - settings_envir$ramp_data[2] * 1.04
            )
            ), settings_envir$ramp_data[2]),
            xend1 = settings_envir$ramp_data[2] - settings_envir$ramp_data[2] * 1.04,
            xends2 = c(splitPotential, settings_envir$ramp_data[2])
          )

        finished_plot <-
          finished_plotInward + finished_plotOutward

        legend = grid.arrange(get_legend(
          finished_plotOutward + theme(
            legend.box.margin = margin(0, 0, 0, 1, "cm"),
            legend.title = element_blank())))

        finished_plot_woLegend <-
          finished_plotInward  + finished_plotOutward + theme(legend.position = "none")

      }
      if (splitted == FALSE) {
        finished_plot <-
          (
            single_plot(
              IV_list = IV_list,
              column_list = column_list,
              measurement_name = measurement_name,
              ylab = ylab,
              used_theme = used_theme,
              used_axis_function = used_axis_function,
              used_font =  used_font,
              used_fontsize = used_fontsize,
              linesize = linesize,
              used_colors = used_colors,
              overrideYlim = overrideYlim,
              xlims =  c(
                settings_envir$ramp_data[1] * 1.04,
                settings_envir$ramp_data[2]
              ),
              xend1 =  settings_envir$ramp_data[1] * 1.04,
              xends2 = c(settings_envir$ramp_data[1], settings_envir$ramp_data[2])
            )
          )
        legend = grid.arrange(get_legend(
          finished_plot + theme(
            legend.box.margin = margin(0, 0, 0, 1, "cm"),
            legend.title = element_blank())))

        finished_plot_woLegend <-
          finished_plot  + theme(legend.position = "none")
      }
    }

    #####Matplot#####
    if (plot_type == "Matplot") {
      if (splitted == TRUE)
      {
        switch(
          column_list,
          "normalized_slopeConductance" = {
            column_list <-
              c(
                "normalized_slopeConductance_Inward",
                "normalized_slopeConductance_Outward"
              )
          },
          "normalized_CurrentDensity" = {
            column_list <-
              c("normalized_CurrentDensity_Inward",
                "normalized_CurrentDensity_Outward")
          },
          "fitted_normalized_CurrentDensity" = {
            column_list <-
              c(
                "fitted_normalized_CurrentDensity_Inward",
                "fitted_normalized_CurrentDensity_Outward"
              )
          },
          "CurrentDensity[pA/pF]" = {
            column_list <-
              c("CurrentDensity[pA/pF]",
                "CurrentDensity[pA/pF]")
          },
          {

          }
        )

        finished_plotInward <-

          mat_plot(
            IV_list = IV_list,
            column_list = column_list[1],
            ylab = ylab,
            peak_list = peak_list,
            series_vector = series_vector,
            measurement_name = measurement_name,
            display_N_legend = display_N_legend,
            display_N_legend_divisor = 2,
            linesize = linesize,
            used_fontsize = used_fontsize,
            used_font = used_font,
            SD_MAD = SD_MAD,
            SD_MAD_shadow_direction = SD_MAD_shadow_direction,
            used_colors = used_colors,
            used_axis_function = used_axis_function,
            used_theme = used_theme,
            overrideYlim = overrideYlim,
            xlims =  c(settings_envir$ramp_data[1] * 1.04 , splitPotential),
            xend1 = settings_envir$ramp_data[1] * 1.04 ,
            xends2 = c(settings_envir$ramp_data[1], splitPotential)
          ) +  theme(legend.position = "none")



        finished_plotOutward <-

          mat_plot(
            IV_list = IV_list,
            column_list = column_list[2],
            ylab = ylab,
            peak_list = peak_list,
            series_vector = series_vector,
            measurement_name = measurement_name,
            display_N_legend = display_N_legend,
            display_N_legend_divisor = 2,
            linesize = linesize,
            used_fontsize = used_fontsize,
            used_font = used_font,
            SD_MAD = SD_MAD,
            SD_MAD_shadow_direction = SD_MAD_shadow_direction,
            used_colors = used_colors,
            used_axis_function = used_axis_function,
            used_theme = used_theme,
            overrideYlim = overrideYlim,
            xlims =  c(splitPotential - abs((
              settings_envir$ramp_data[2] - settings_envir$ramp_data[2] * 1.04
            )
            ), settings_envir$ramp_data[2]),
            xend1 = settings_envir$ramp_data[2] - settings_envir$ramp_data[2] * 1.04,
            xends2 = c(splitPotential, settings_envir$ramp_data[2])
          )

        finished_plot <-
          finished_plotInward + finished_plotOutward

        legend = grid.arrange(get_legend(
          finished_plotOutward + theme(
            legend.box.margin = margin(0, 0, 0, 1, "cm"),
            legend.title = element_blank())))

        finished_plot_woLegend <-
          finished_plotInward  + finished_plotOutward + theme(legend.position = "none")
      }
      if (splitted == FALSE) {
        finished_plot <-
          mat_plot(
            IV_list = IV_list,
            column_list = column_list,
            ylab = ylab,
            peak_list = peak_list,
            series_vector = series_vector,
            measurement_name = measurement_name,
            display_N_legend = display_N_legend,
            display_N_legend_divisor = 2,
            linesize = linesize,
            used_fontsize = used_fontsize,
            used_font = used_font,
            used_colors = used_colors,
            used_axis_function = used_axis_function,
            used_theme = used_theme,
            legend_seperate = legend_seperate,
            spacer_plot = spacer_plot,
            overrideYlim = overrideYlim,
            xlims =  c(
              settings_envir$ramp_data[1] * 1.04,
              settings_envir$ramp_data[2]
            ),
            xend1 =  settings_envir$ramp_data[1] * 1.04,
            xends2 = c(settings_envir$ramp_data[1], settings_envir$ramp_data[2])
          )

        legend = grid.arrange(get_legend(
          finished_plot + theme(
            legend.box.margin = margin(0, 0, 0, 1, "cm"),
            legend.title = element_blank())))

        finished_plot_woLegend <-
          finished_plot  + theme(legend.position = "none")
      }
    }

    #####Median#####
    if (plot_type == "MedianTraces") {
      if (splitted == TRUE)
      {
        switch(
          column_list,
          "normalized_slopeConductance" = {
            column_list <-
              c(
                "normalized_slopeConductance_Inward",
                "normalized_slopeConductance_Outward"
              )
          },
          "normalized_CurrentDensity" = {
            column_list <-
              c("normalized_CurrentDensity_Inward",
                "normalized_CurrentDensity_Outward")
          },
          "fitted_normalized_CurrentDensity" = {
            column_list <-
              c(
                "fitted_normalized_CurrentDensity_Inward",
                "fitted_normalized_CurrentDensity_Outward"
              )
          },
          "CurrentDensity[pA/pF]" = {
            column_list <-
              c("CurrentDensity[pA/pF]",
                "CurrentDensity[pA/pF]")
          },
          {

          }
        )

        finished_plotInward <-
          median_plot(
            IV_list = IV_list,
            column_list = column_list[1],
            ylab = ylab,
            peak_list = peak_list,
            series_vector = series_vector,
            measurement_name = measurement_name,
            display_N_legend = display_N_legend,
            display_N_legend_divisor = 2,
            linesize = linesize,
            used_fontsize = used_fontsize,
            used_font = used_font,
            SD_MAD = SD_MAD,
            SD_MAD_shadow_direction = SD_MAD_shadow_direction,
            used_colors = used_colors,
            used_axis_function = used_axis_function,
            used_theme = used_theme,
            overrideYlim = overrideYlim,
            xlims =  c(settings_envir$ramp_data[1] * 1.04 , splitPotential),
            xend1 = settings_envir$ramp_data[1] * 1.04 ,
            xends2 = c(settings_envir$ramp_data[1], splitPotential)
          ) +  theme(legend.position = "none")

        finished_plotOutward <-
          median_plot(
            IV_list = IV_list,
            column_list = column_list[2],
            ylab = ylab,
            peak_list = peak_list,
            series_vector = series_vector,
            measurement_name = measurement_name,
            display_N_legend = display_N_legend,
            display_N_legend_divisor = 2,
            linesize = linesize,
            used_fontsize = used_fontsize,
            used_font = used_font,
            SD_MAD = SD_MAD,
            SD_MAD_shadow_direction = SD_MAD_shadow_direction,
            used_colors = used_colors,
            used_axis_function = used_axis_function,
            used_theme = used_theme,
            overrideYlim = overrideYlim,
            xlims =  c(splitPotential - abs(
              settings_envir$ramp_data[2] - settings_envir$ramp_data[2] * 1.04
            )
            , settings_envir$ramp_data[2]),
            xend1 = settings_envir$ramp_data[2] - settings_envir$ramp_data[2] * 1.04,
            xends2 = c(splitPotential, settings_envir$ramp_data[2])
          )

        finished_plot <-
          finished_plotInward + finished_plotOutward

        legend = grid.arrange(get_legend(
          finished_plotOutward + theme(
            legend.box.margin = margin(0, 0, 0, 1, "cm"),
            legend.title = element_blank())))

        finished_plot_woLegend <-
          finished_plotInward  + finished_plotOutward + theme(legend.position = "none")

      }
      if (splitted == FALSE) {
        finished_plot <-
          median_plot(
            IV_list = IV_list,
            column_list = column_list,
            ylab = ylab,
            peak_list = peak_list,
            series_vector = series_vector,
            measurement_name = measurement_name,
            display_N_legend = display_N_legend,
            display_N_legend_divisor = 2,
            linesize = linesize,
            used_fontsize = used_fontsize,
            used_font = used_font,
            SD_MAD = SD_MAD,
            SD_MAD_shadow_direction = SD_MAD_shadow_direction,
            used_colors = used_colors,
            used_axis_function = used_axis_function,
            used_theme = used_theme,
            overrideYlim = overrideYlim,
            xlims =  c(
              settings_envir$ramp_data[1] * 1.04,
              settings_envir$ramp_data[2]
            ),
            xend1 =  settings_envir$ramp_data[1] * 1.04,
            xends2 = c(settings_envir$ramp_data[1], settings_envir$ramp_data[2]),
            ...
          )

        legend = grid.arrange(get_legend(
          finished_plot + theme(
            legend.box.margin = margin(0, 0, 0, 1, "cm"),
            legend.title = element_blank())))

        finished_plot_woLegend <-
          finished_plot  + theme(legend.position = "none")



      }

    }


    #####P_Plot#####
    if (plot_type == "P-plot") {
      n_spacer <- length(series_vector) + n_spacer

      if (splitted == TRUE)
      {
        switch(
          column_list,
          "normalized_slopeConductance" = {
            column_list <-
              c(
                "normalized_slopeConductance_Inward",
                "normalized_slopeConductance_Outward"
              )
          },
          "normalized_CurrentDensity" = {
            column_list <-
              c("normalized_CurrentDensity_Inward",
                "normalized_CurrentDensity_Outward")
          },
          "fitted_normalized_CurrentDensity" = {
            column_list <-
              c(
                "fitted_normalized_CurrentDensity_Inward",
                "fitted_normalized_CurrentDensity_Outward"
              )
          },
          "CurrentDensity[pA/pF]" = {
            column_list <-
              c("CurrentDensity[pA/pF]",
                "CurrentDensity[pA/pF]")
          },
          {

          }
        )



        finished_plot_Inward <-
          p_plot(
            IV_list = IV_list,
            column_list = column_list[1],
            ylab = ylab,
            peak_list = peak_list,
            series_vector = series_vector,
            measurement_name = measurement_name,
            display_N_legend = display_N_legend,
            display_N_legend_divisor = 2,
            linesize = linesize,
            used_fontsize = used_fontsize,
            used_font = used_font,
            SD_MAD = SD_MAD,
            SD_MAD_shadow_direction = SD_MAD_shadow_direction,
            used_colors = used_colors,
            used_axis_function = used_axis_function,
            used_theme = used_theme,
            spacer_plot = spacer_plot,
            n_spacer = n_spacer,
            ratio_Median_P_plot = ratio_Median_P_plot,
            overrideYlim = overrideYlim,
            xlims =  c(settings_envir$ramp_data[1] * 1.04 , splitPotential),
            xend1 = settings_envir$ramp_data[1] * 1.04 ,
            xends2 = c(settings_envir$ramp_data[1], splitPotential),
            ...
          ) +  theme(legend.position = "none")
        finished_plot_Outward <-
          p_plot(
            IV_list = IV_list,
            column_list = column_list[2],
            ylab = ylab,
            peak_list = peak_list,
            series_vector = series_vector,
            measurement_name = measurement_name,
            display_N_legend = display_N_legend,
            display_N_legend_divisor = 2,
            linesize = linesize,
            used_fontsize = used_fontsize,
            used_font = used_font,
            SD_MAD = SD_MAD,
            SD_MAD_shadow_direction = SD_MAD_shadow_direction,
            used_colors = used_colors,
            used_axis_function = used_axis_function,
            used_theme = used_theme,
            spacer_plot = spacer_plot,
            n_spacer = n_spacer,
            overrideYlim = overrideYlim,
            ratio_Median_P_plot = ratio_Median_P_plot,
            xlims =  c(splitPotential - abs(
              settings_envir$ramp_data[2] - settings_envir$ramp_data[2] * 1.04

            ), settings_envir$ramp_data[2]),
            xend1 = settings_envir$ramp_data[2] - settings_envir$ramp_data[2] * 1.04,
            xends2 = c(splitPotential, settings_envir$ramp_data[2]),
            ...
          )

        legend = grid.arrange(get_legend(
          finished_plot_Inward[[1]] + theme(
            legend.box.margin = margin(0, 0, 0, 1, "cm"),
            legend.title = element_blank())))

        finished_plot_Inward_woLegend <- finished_plot_Inward
        finished_plot_Inward_woLegend[[1]] <- finished_plot_Inward_woLegend[[1]] + theme(legend.position = "none")

        finished_plot_Outward_woLegend <- finished_plot_Outward
        finished_plot_Outward_woLegend[[1]] <- finished_plot_Outward_woLegend[[1]] + theme(legend.position = "none")

        finished_plot_woLegend <-
          finished_plot_Inward_woLegend | finished_plot_Outward_woLegend

        finished_plot <-
          finished_plot_Inward_woLegend | finished_plot_Outward
      }

      if (splitted == FALSE)
      {
        finished_plot <-
          p_plot(
            IV_list = IV_list,
            column_list = column_list,
            ylab = ylab,
            peak_list = peak_list,
            series_vector = series_vector,
            measurement_name = measurement_name,
            display_N_legend = display_N_legend,
            display_N_legend_divisor = 2,
            linesize = linesize,
            used_fontsize = used_fontsize,
            used_font = used_font,
            SD_MAD = SD_MAD,
            SD_MAD_shadow_direction = SD_MAD_shadow_direction,
            used_colors = used_colors,
            used_axis_function = used_axis_function,
            used_theme = used_theme,
            spacer_plot = spacer_plot,
            n_spacer = n_spacer,
            overrideYlim = overrideYlim,
            ratio_Median_P_plot = ratio_Median_P_plot,
            xlims =  c(
              settings_envir$ramp_data[1] * 1.04,
              settings_envir$ramp_data[2]
            ),
            xend1 =  settings_envir$ramp_data[1] * 1.04,
            xends2 = c(settings_envir$ramp_data[1], settings_envir$ramp_data[2]),
            ...
          )

        legend = grid.arrange(get_legend(
          finished_plot[[1]] + theme(
            legend.box.margin = margin(0, 0, 0, 1, "cm"),
            legend.title = element_blank())))

        finished_plot_woLegend <-
          finished_plot

        finished_plot_woLegend[[1]] <- finished_plot_woLegend[[1]] + theme(legend.position = "none")

      }
    }


    #####Complete_plot######
    if (plot_type == "Complete") {

      n_spacer <- length(series_vector) + n_spacer

      if (splitted == FALSE) {

        matplot <- mat_plot(
          IV_list = IV_list,
          column_list = column_list[[1]],
          ylab = ylab[1],
          peak_list = peak_list,
          series_vector = series_vector,
          measurement_name = measurement_name,
          display_N_legend = display_N_legend,
          display_N_legend_divisor = 2,
          linesize = linesize,
          used_fontsize = used_fontsize,
          used_font = used_font,
          used_colors = used_colors,
          used_axis_function = used_axis_function[[1]],
          used_theme = used_theme[[1]],
          legend_seperate = legend_seperate,
          spacer_plot = spacer_plot,
          overrideYlim = overrideYlim[[1]],
          xlims =  c(settings_envir$ramp_data[1] * 1.04, settings_envir$ramp_data[2]),
          xend1 =  settings_envir$ramp_data[1] * 1.04,
          xends2 = c(settings_envir$ramp_data[1], settings_envir$ramp_data[2])
        )



        pplot <-  p_plot(
          IV_list = IV_list,
          column_list = column_list[[2]],
          ylab = ylab[2],
          peak_list = peak_list,
          series_vector = series_vector,
          measurement_name = measurement_name,
          display_N_legend = display_N_legend,
          display_N_legend_divisor = 2,
          linesize = linesize,
          used_fontsize = used_fontsize,
          used_font = used_font,
          SD_MAD = SD_MAD,
          SD_MAD_shadow_direction = SD_MAD_shadow_direction,
          used_colors = used_colors,
          used_axis_function = used_axis_function,
          used_theme = used_theme,
          spacer_plot = spacer_plot,
          n_spacer = n_spacer,
          overrideYlim = overrideYlim[[2]],
          ratio_Median_P_plot = ratio_Median_P_plot,
          xlims =  c(settings_envir$ramp_data[1] * 1.04, settings_envir$ramp_data[2]),
          xend1 =  settings_envir$ramp_data[1] * 1.04,
          xends2 = c(settings_envir$ramp_data[1], settings_envir$ramp_data[2]),
          ...
        )

        medianplot <- pplot[[1]]

        pplot <- pplot[[2]]

        matplot <- matplot + theme(axis.title.x = element_blank(),
                                   plot.margin = unit(c(0.4, 0.5, 0.2, 0), "cm"))
        medianplot <- medianplot + theme(axis.title.x = element_blank(),
                                         plot.margin = unit(c(0.2, 0.5, 0.2, 0), "cm"))

        legend = grid.arrange(get_legend(
          matplot + theme(
            legend.box.margin = margin(0, 0, 0, 1, "cm"),
            legend.title = element_blank())),
          get_legend(
            medianplot + theme(
              legend.box.margin = margin(0, 0, 0, 1, "cm"),
              legend.title = element_blank()
            )))

        finished_plot_woLegend <- wrap_plots(matplot + theme(legend.position = "none"),
                                             medianplot + theme(legend.position = "none"),
                                             pplot,
                                             heights = c(0.5,0.5, ratio_Median_P_plot))


        finished_plot <- wrap_plots(matplot, medianplot, pplot, heights = c(0.5,0.5, ratio_Median_P_plot))

      }

      if (splitted == TRUE)
      {
        matplot <- mat_plot(
          IV_list = IV_list,
          column_list = column_list[[1]],
          ylab = ylab[1],
          peak_list = peak_list,
          series_vector = series_vector,
          measurement_name = measurement_name,
          display_N_legend = display_N_legend,
          display_N_legend_divisor = 2,
          linesize = linesize,
          used_fontsize = used_fontsize,
          used_font = used_font,
          used_colors = used_colors,
          used_axis_function = used_axis_function[[1]],
          used_theme = used_theme[[1]],
          legend_seperate = legend_seperate,
          spacer_plot = spacer_plot,
          overrideYlim = overrideYlim[[1]],
          xlims =  c(settings_envir$ramp_data[1] * 1.04, settings_envir$ramp_data[2]),
          xend1 =  settings_envir$ramp_data[1] * 1.04,
          xends2 = c(settings_envir$ramp_data[1], settings_envir$ramp_data[2])
        )

        column_list <- map(column_list[-1], function(column_list) {
          switch(
            column_list,
            "normalized_slopeConductance" = {
              column_list <-
                c(
                  "normalized_slopeConductance_Inward",
                  "normalized_slopeConductance_Outward"
                )
            },
            "normalized_CurrentDensity" = {
              column_list <-
                c(
                  "normalized_CurrentDensity_Inward",
                  "normalized_CurrentDensity_Outward"
                )
            },
            "fitted_normalized_CurrentDensity" = {
              column_list <-
                c(
                  "fitted_normalized_CurrentDensity_Inward",
                  "fitted_normalized_CurrentDensity_Outward"
                )
            },
            "CurrentDensity[pA/pF]" = {
              column_list <-
                c("CurrentDensity[pA/pF]_Inward",
                  "CurrentDensity[pA/pF]_Outward")
            },
            {

            }
          )
          return(column_list)
        })

        matplotInward2 <- mat_plot(
          IV_list = IV_list,
          column_list = column_list[[1]][1],
          ylab = ylab[2],
          peak_list = peak_list,
          series_vector = series_vector,
          measurement_name = measurement_name,
          display_N_legend = display_N_legend,
          display_N_legend_divisor = 2,
          linesize = linesize,
          used_fontsize = used_fontsize,
          used_font = used_font,
          used_colors = used_colors,
          used_axis_function = used_axis_function[[1]],
          used_theme = used_theme[[1]],
          legend_seperate = legend_seperate,
          spacer_plot = spacer_plot,
          overrideYlim = overrideYlim[[2]],
          xlims =  c(settings_envir$ramp_data[1] * 1.04 , splitPotential),
          xend1 = settings_envir$ramp_data[1] * 1.04 ,
          xends2 = c(settings_envir$ramp_data[1], splitPotential)
        ) +  theme(legend.position = "none")

        matplotOutward2 <- mat_plot(
          IV_list = IV_list,
          column_list = column_list[[1]][2],
          ylab = ylab[2],
          peak_list = peak_list,
          series_vector = series_vector,
          measurement_name = measurement_name,
          display_N_legend = display_N_legend,
          display_N_legend_divisor = 2,
          linesize = linesize,
          used_fontsize = used_fontsize,
          used_font = used_font,
          used_colors = used_colors,
          used_axis_function = used_axis_function[[1]],
          used_theme = used_theme[[1]],
          legend_seperate = legend_seperate,
          spacer_plot = spacer_plot,
          overrideYlim = overrideYlim[[2]],
          xlims =  c(splitPotential - abs((settings_envir$ramp_data[2] - settings_envir$ramp_data[2] * 1.04)), settings_envir$ramp_data[2]),
          xend1 = settings_envir$ramp_data[2] - settings_envir$ramp_data[2] * 1.04,
          xends2 = c(splitPotential, settings_envir$ramp_data[2])
        )

        pplotInward <-  p_plot(
          IV_list = IV_list,
          column_list = column_list[[2]][1],
          ylab = ylab[3],
          peak_list = peak_list,
          series_vector = series_vector,
          measurement_name = measurement_name,
          display_N_legend = display_N_legend,
          display_N_legend_divisor = 2,
          linesize = linesize,
          used_fontsize = used_fontsize,
          used_font = used_font,
          SD_MAD = SD_MAD,
          SD_MAD_shadow_direction = SD_MAD_shadow_direction,
          used_colors = used_colors,
          used_axis_function = used_axis_function,
          used_theme = used_theme,
          spacer_plot = spacer_plot,
          n_spacer = n_spacer,
          overrideYlim = overrideYlim[[3]],
          ratio_Median_P_plot = ratio_Median_P_plot,
          xlims =  c(settings_envir$ramp_data[1] * 1.04 , splitPotential),
          xend1 = settings_envir$ramp_data[1] * 1.04 ,
          xends2 = c(settings_envir$ramp_data[1], splitPotential),
          ...
        )

        pplotOutward <-  p_plot(
          IV_list = IV_list,
          column_list = column_list[[2]],
          ylab = ylab[3],
          peak_list = peak_list,
          series_vector = series_vector,
          measurement_name = measurement_name,
          display_N_legend = display_N_legend,
          display_N_legend_divisor = 2,
          linesize = linesize,
          used_fontsize = used_fontsize,
          used_font = used_font,
          SD_MAD = SD_MAD,
          SD_MAD_shadow_direction = SD_MAD_shadow_direction,
          used_colors = used_colors,
          used_axis_function = used_axis_function,
          used_theme = used_theme,
          spacer_plot = spacer_plot,
          n_spacer = n_spacer,
          overrideYlim = overrideYlim[[3]],
          ratio_Median_P_plot = ratio_Median_P_plot,
          xlims =  c(splitPotential - abs((settings_envir$ramp_data[2] - settings_envir$ramp_data[2] * 1.04)), settings_envir$ramp_data[2]),
          xend1 = settings_envir$ramp_data[2] - settings_envir$ramp_data[2] * 1.04,
          xends2 = c(splitPotential, settings_envir$ramp_data[2]),
          ...
        )


        medianplotInward <- pplotInward [[1]]
        medianplotOutward <- pplotOutward [[1]]

        pplotInward  <- pplotInward [[2]]
        pplotOutward  <- pplotOutward [[2]]



        matplot<- matplot + theme(axis.title.x = element_blank(),
                                  plot.margin = unit(c(0.4, 0.5, 0.2, 0), "cm"))
        matplotInward2 <- matplotInward2 + theme(axis.title.x = element_blank(),
                                                 plot.margin = unit(c(0.4, 0.5, 0.2, 0), "cm"))
        medianplotInward <- medianplotInward + theme(axis.title.x = element_blank(),
                                                     plot.margin = unit(c(0.2, 0.5, 0.2, 0), "cm"))
        matplotOutward2 <- matplotOutward2 + theme(axis.title.x = element_blank(),
                                                   plot.margin = unit(c(0.4, 0.5, 0.2, 0), "cm"))
        medianplotOutward <- medianplotOutward + theme(axis.title.x = element_blank(),
                                                       plot.margin = unit(c(0.2, 0.5, 0.2, 0), "cm"))

        layout <- "
          AAHH
          BBEE
          CCFF
          DDGG
          "

        legend = grid.arrange(get_legend(
          matplot + theme(
            legend.box.margin = margin(0, 0, 0, 1, "cm"),
            legend.title = element_blank())),
          get_legend(
            medianplotInward + theme(
              legend.box.margin = margin(0, 0, 0, 1, "cm"),
              legend.title = element_blank()
            )))




        finished_plot <- wrap_plots(A = matplot + theme(legend.position = "none"), H = legend,
                                    B = matplotInward2 + theme(legend.position = "none") , E = matplotOutward2 + theme(legend.position = "none"),
                                    C = medianplotInward + theme(legend.position = "none"), F = medianplotOutward + theme(legend.position = "none"),
                                    D = pplotInward , G = pplotOutward ,
                                    design = layout,  heights = c(0.33,0.33,0.33,ratio_Median_P_plot), guides = "keep")

        finished_plot_woLegend <- wrap_plots(A = matplot + theme(legend.position = "none"), H = plot_spacer(),
                                             B = matplotInward2 + theme(legend.position = "none") , E = matplotOutward2 + theme(legend.position = "none"),
                                             C = medianplotInward + theme(legend.position = "none"), F = medianplotOutward + theme(legend.position = "none"),
                                             D = pplotInward , G = pplotOutward ,
                                             design = layout,  heights = c(0.33,0.33,0.33,ratio_Median_P_plot), guides = "keep")

      }
}
    #####Save#####
    if (!is.null(save_dir)) {
      if (!is.null(legend_seperate))
        if (legend_seperate == TRUE) {
          if (plot_type == "P-plot") {
            legend <-
              get_legend(
                finished_plot[[1]] + theme(
                  legend.box.margin = margin(0, 0, 0, 0, "cm"),
                  legend.title = element_blank()
                )
              )
            finished_plot[[1]] <-
              finished_plot[[1]]  + theme(legend.position = "none")
          }
          if (plot_type == "Complete") {
            finished_plot[[1]] <-
              finished_plot[[1]]  + theme(legend.position = "none")
            legend <-
              get_legend(
                finished_plot[[2]] + theme(
                  legend.box.margin = margin(0, 0, 0, 0, "cm"),
                  legend.title = element_blank()
                )
              )
            finished_plot[[2]] <-
              finished_plot[[2]]  + theme(legend.position = "none")
          }
          else {
            legend <-
              get_legend(
                finished_plot + theme(
                  legend.box.margin = margin(0, 0, 0, 0, "cm"),
                  legend.title = element_blank()
                )
              )
            finished_plot <-
              finished_plot  + theme(legend.position = "none")
          }
          ggsave(
            paste0("legend_", save_name),
            legend,
            path = save_dir,
            width = save_width,
            height = save_height,
            units = "mm",
            limitsize = F
          )

        }
      ggsave(
        save_name,
        finished_plot ,
        path = save_dir,
        width = save_width,
        height = save_height,
        units = "mm",
        limitsize = F
      )
      print(paste0(save_dir, fixed("/"), save_name, " succesfully saved"))
    }

    if(exists("finished_plot_woLegend")) {
    return(list(finished_plot, finished_plot_woLegend, legend))}

    return(list(finished_plot))

  }







box_plot <-
  function(IV_list = NULL,
           summary_list = NULL ,
           column_list = NULL ,
           peak_list = NULL ,
           series_vector = NULL ,
           ylab = NULL,
           measurement_name = NULL,
           display_N_legend = NULL,
           display_N_legend_divisor = NULL,
           used_theme = NULL ,
           used_axis_function = NULL ,
           used_font = NULL ,
           used_fontsize = NULL ,
           linesize = NULL ,
           used_colors = NULL,
           legend_seperate = FALSE,
           spacer_plot = FALSE,
           size = NULL,
           jittersize = NULL,
           ...) {
    boxplot <-
      create_boxplot(
        summary_list,
        column_list,
        y_lab = "",
        title = "",
        lwd = linesize,
        size = size,
        jittersize = jittersize,
        ...
      )
    if (!is.null(used_fontsize))
      if (!is.null(used_font)) {
        boxplot <-
          change_plot_theme(boxplot,
                            used_theme(used_fontsize, used_fontsize, used_font))
      } else {
        boxplot <-
          change_plot_theme(boxplot, used_theme(used_fontsize, used_fontsize))
      }
    else {
      if (!is.null(used_theme))
        boxplot <-  change_plot_theme(boxplot, used_theme())

    }

    if (!is.null(used_colors))
      boxplot <-
        change_boxplot_color_fill_manuel(boxplot, rep(used_colors[1:length(summary_list)], length(column_list[[1]])), 1)


    finished_plot <- boxplot
    detach("package:latex2exp", unload = TRUE)
    library(latex2exp)
    if (!is.null(ylab))
      finished_plot <- finished_plot + ylab(ylab)
    return(finished_plot)
  }

ratio_plot <-
  function(IV_list = NULL,
           summary_list = NULL ,
           column_list = NULL ,
           peak_list = NULL ,
           series_vector = NULL ,
           ylab = NULL,
           measurement_name = NULL,
           display_N_legend = NULL,
           display_N_legend_divisor = NULL,
           used_theme = NULL ,
           used_axis_function = NULL ,
           used_font = NULL ,
           used_fontsize = NULL ,
           linesize = NULL ,
           used_colors = NULL,
           legend_seperate = FALSE,
           spacer_plot = FALSE,
           size = NULL,
           jittersize = NULL,
           ...) {
    ratioplot <-
      create_ratio_plot(
        summary_list,
        peak_list,
        y_lab = "",
        title = "",
        lwd = linesize,
        size = size,
        jittersize = jittersize,
        ...
      )
    if (!is.null(used_fontsize))
      if (!is.null(used_font)) {
        ratioplot <-
          change_plot_theme(ratioplot,
                            used_theme(used_fontsize, used_fontsize , used_font))
      } else {
        ratioplot <-
          change_plot_theme(ratioplot, used_theme(used_fontsize, used_fontsize))
      }
    else {
      if (!is.null(used_theme))
        ratioplot <-  change_plot_theme(boxplot, used_theme())

    }

    if (!is.null(used_colors))
      ratioplot <-
        change_boxplot_color_fill_manuel(ratioplot,  rep(used_colors[1:length(summary_list)], each = length(peak_list[[1]][[1]])), 1)
    finished_plot <- ratioplot
    detach("package:latex2exp", unload = TRUE)
    library(latex2exp)
    if (!is.null(ylab))
      finished_plot <- finished_plot + ylab(ylab)
    return(finished_plot)
  }


single_plot <-
  function(IV_list = NULL,
           column_list = NULL ,
           measurement_name = NULL,
           ylab = NULL,
           display_N_legend = NULL,
           display_N_legend_divisor = NULL,
           used_theme = NULL ,
           used_axis_function = NULL ,
           used_font = NULL ,
           used_fontsize = NULL ,
           linesize = NULL ,
           used_colors = NULL,
           save_dir = NULL ,
           legend_seperate = FALSE,
           spacer_plot = FALSE,
           size = NULL,
           jittersize = NULL,
           overrideYlim = NULL,
           ...) {
    if (length(measurement_name) != length(IV_list))
      measurement_name <- names(IV_list)


    single_plot <-
      create_single_plot(IV_list[[1]], column_list,  first(measurement_name), linesize = linesize)
    if (length(IV_list) > 1) {
      for (i in 2:length(measurement_name)) {
        single_plot <-
          add_to_single_plot(single_plot,
                             IV_list[[i]],
                             column_list,
                             measurement_name[[i]],
                             linesize = linesize)
      }
    }


    if (!is.null(used_fontsize))
      if (!is.null(used_font)) {
        single_plot <-
          change_plot_theme(single_plot,
                            used_theme(used_fontsize, used_fontsize , used_font))
      } else {
        single_plot <-
          change_plot_theme(single_plot, used_theme(used_fontsize, used_fontsize))
      }
    else {
      if (!is.null(used_theme))
        single_plot <-
          change_plot_theme(single_plot, used_theme())

    }

    if (!is.null(used_colors))
      single_plot <-
      change_plot_color_fill_manuel(single_plot , measurement_name, used_colors[1:length(measurement_name)], 1)

    if (!is.null(used_axis_function))
      single_plot <-
      used_axis_function(single_plot, linesize, overrideYlim = overrideYlim, ...)

    finished_plot <- single_plot

    detach("package:latex2exp", unload = TRUE)
    library(latex2exp)
    if (!is.null(ylab))
      finished_plot <- finished_plot + ylab(ylab)
    return(finished_plot)


  }


mat_plot <-
  function(IV_list = NULL,
           summary_list = NULL ,
           column_list = NULL ,
           peak_list = NULL ,
           series_vector = NULL ,
           measurement_name = NULL,
           ylab = NULL,
           display_N_legend = NULL,
           display_N_legend_divisor = NULL,
           used_theme = NULL ,
           used_axis_function = NULL ,
           used_font = NULL ,
           used_fontsize = NULL ,
           linesize = NULL ,
           used_colors = NULL,
           save_dir = NULL ,
           legend_seperate = FALSE,
           spacer_plot = FALSE,
           size = NULL,
           jittersize = NULL,
           overrideYlim = NULL,
           xlims = c(-104, 100),
           xend1 = -104,
           xends2 = c(-100, 100),
           ...) {
    if (is.null(measurement_name) &&
        !is.null(series_vector))
      measurement_name <- series_vector

    if (!is.null(series_vector))
    {
      mat_plot <-
        create_matplot(IV_list[[series_vector[[1]]]],
                       column_list,
                       peak_list[[1]],
                       measurement_name[[1]],
                       "",
                       "",
                       linesize)
      if (length(series_vector) > 1) {
        for (i in 2:length(series_vector)) {
          mat_plot <-
            add_to_matplot(mat_plot,
                           IV_list[[series_vector[[i]]]],
                           column_list,
                           peak_list[[i]],
                           measurement_name[[i]],
                           linesize)

        }
      }
    }

    if (is.null(series_vector))
      mat_plot <-
        create_matplot(IV_list,
                       column_list,
                       peak_list,
                       measurement_name,
                       "",
                       "",
                       linesize)


    if (!is.null(used_fontsize))
      if (!is.null(used_font)) {
        mat_plot <-
          change_plot_theme(mat_plot,
                            used_theme(used_fontsize, used_fontsize , used_font))
      } else {
        mat_plot <-
          change_plot_theme(mat_plot, used_theme(used_fontsize, used_fontsize))
      }
    else {
      if (!is.null(used_theme))
        mat_plot <-  change_plot_theme(mat_plot, used_theme())

    }

    if (!is.null(used_colors))
      mat_plot <-
        change_plot_color_fill_manuel(mat_plot , measurement_name, used_colors[1:length(measurement_name)], 1)

    if (!is.null(used_axis_function))
      mat_plot <-
        used_axis_function(
          mat_plot,
          linesize,
          overrideYlim = overrideYlim,
          xlims = xlims,
          xend1 = xend1,
          xends2 = xends2
        )

    finished_plot <- mat_plot
    detach("package:latex2exp", unload = TRUE)
    library(latex2exp)
    if (!is.null(ylab))
      finished_plot <- finished_plot + ylab(ylab)
    return(finished_plot)
  }


median_plot <-
  function(IV_list = NULL,
           summary_list = NULL ,
           column_list = NULL ,
           peak_list = NULL ,
           series_vector = NULL ,
           measurement_name = NULL,
           ylab = NULL,
           display_N_legend = NULL,
           display_N_legend_divisor = NULL,
           SD_MAD = NULL,
           SD_MAD_shadow_direction = NULL,
           used_theme = NULL ,
           used_axis_function = NULL ,
           used_font = NULL ,
           used_fontsize = NULL ,
           linesize = NULL ,
           used_colors = NULL,
           save_dir = NULL ,
           legend_seperate = FALSE,
           spacer_plot = FALSE,
           size = NULL,
           jittersize = NULL,
           overrideYlim = NULL,
           xlims = c(-104, 100),
           xend1 = -104,
           xends2 = c(-100, 100),
           ...) {
    if (is.null(measurement_name) &&
        !is.null(series_vector))
      measurement_name <- series_vector

    if (length(measurement_name) < length(series_vector))
      measurement_name <- series_vector


    if (!is.null(series_vector))
    {
      median_plot <-
        create_median_plot(
          IV_list[[series_vector[[1]]]],
          column_list,
          peak_list[[1]],
          measurement_name[[1]],
          "",
          "",
          linesize,
          SD_MAD,
          SD_MAD_shadow_direction[[1]]
        )
      if (length(series_vector) > 1) {
        for (i in 2:length(series_vector)) {
          median_plot <-
            add_to_median_plot(
              median_plot,
              IV_list[[series_vector[[i]]]],
              column_list,
              peak_list[[i]],
              measurement_name[[i]],
              linesize,
              SD_MAD,
              SD_MAD_shadow_direction[[i]]
            )

        }
      }
    }

    if (is.null(series_vector))
      median_plot <-
        create_median_plot(
          IV_list,
          column_list,
          peak_list,
          measurement_name,
          "",
          "",
          linesize,
          SD_MAD,
          SD_MAD_shadow_direction
        )


    if (!is.null(used_fontsize))
      if (!is.null(used_font)) {
        median_plot <-
          change_plot_theme(median_plot,
                            used_theme(used_fontsize, used_fontsize , used_font))
      } else {
        median_plot <-
          change_plot_theme(median_plot, used_theme(used_fontsize, used_fontsize))
      }
    else {
      if (!is.null(used_theme))
        median_plot <-
          change_plot_theme(median_plot, used_theme())

    }

    if (!is.null(used_colors))
      median_plot <-
        change_plot_color_fill_manuel(median_plot , measurement_name, used_colors[1:length(measurement_name)], 1)

    if (!is.null(used_axis_function))
      median_plot <-
        used_axis_function(
          median_plot,
          linesize,
          overrideYlim = overrideYlim,
          xlims = xlims,
          xend1 = xend1,
          xends2 = xends2
        )


    finished_plot <- median_plot
    detach("package:latex2exp", unload = TRUE)
    library(latex2exp)
    if (!is.null(ylab))
      finished_plot <- finished_plot + ylab(ylab)
    return(finished_plot)
  }

p_plot <-
  function(IV_list = NULL,
           summary_list = NULL ,
           column_list = NULL ,
           peak_list = NULL ,
           series_vector = NULL ,
           measurement_name = NULL,
           ylab = NULL,
           display_N_legend = NULL,
           display_N_legend_divisor = NULL,
           SD_MAD = NULL,
           SD_MAD_shadow_direction = NULL,
           used_theme = NULL ,
           used_axis_function = NULL ,
           used_font = NULL ,
           used_fontsize = NULL ,
           linesize = NULL ,
           used_colors = NULL,
           save_dir = NULL ,
           legend_seperate = FALSE,
           spacer_plot = FALSE,
           size = NULL,
           overrideYlim = NULL,
           jittersize = NULL,
           kruskal = F,
           paired = F,
           n_spacer = 0,
           ratio_Median_P_plot = 2.2,
           xlims = c(-104, 100),
           xend1 = -104,
           xends2 = c(-100, 100),
           ...) {
    #####Medianplot of P-Plot#####
    if (is.null(measurement_name) &&
        !is.null(series_vector))
      measurement_name <- series_vector


    median_plot <-
      create_median_plot(
        IV_list[[series_vector[[1]]]],
        column_list,
        peak_list[[1]],
        measurement_name[[1]],
        "",
        "",
        linesize,
        SD_MAD,
        SD_MAD_shadow_direction[[1]]
      )
    if (length(series_vector) > 1) {
      for (i in 2:length(series_vector)) {
        median_plot <-
          add_to_median_plot(
            median_plot,
            IV_list[[series_vector[[i]]]],
            column_list,
            peak_list[[i]],
            measurement_name[[i]],
            linesize,
            SD_MAD,
            SD_MAD_shadow_direction[[i]]
          )

      }
    }

    if (!is.null(used_fontsize))
      if (!is.null(used_font)) {
        median_plot <-
          change_plot_theme(median_plot,
                            used_theme[[1]](used_fontsize, used_fontsize , used_font))
      } else {
        median_plot <-
          change_plot_theme(median_plot, used_theme[[1]](used_fontsize, used_fontsize))
      }
    else {
      if (!is.null(used_theme[[1]]))
        median_plot <-
          change_plot_theme(median_plot, used_theme[[1]]())

    }

    if (!is.null(used_colors))
      median_plot <-
      change_plot_color_fill_manuel(median_plot , measurement_name, used_colors[1:length(measurement_name)], 1)

    if (!is.null(used_axis_function))
      median_plot <-
      used_axis_function[[1]](
        median_plot,
        linesize,
        overrideYlim = overrideYlim,
        xlims = xlims,
        xend1 = xend1,
        xends2 = xends2
      )

    detach("package:latex2exp", unload = TRUE)
    library(latex2exp)

    median_plot <-
      median_plot + theme(axis.title.x = element_blank())

    if (!is.null(ylab))
      median_plot <- median_plot + ylab(ylab)

    #####P-Plot Part#####
    if (kruskal == T) {
      p_plot <-
        create_p_area_plot(
          IV_list,
          "Kruskal",
          column_list,
          peak_list,
          "P-Value",
          "",
          kruskal = kruskal,
          paired = paired
        )


      if (!is.null(used_fontsize))
        if (!is.null(used_font)) {
          p_plot <-
            change_plot_theme(p_plot,
                              used_theme[[2]](used_fontsize, used_fontsize , used_font))
        } else {
          p_plot <-
            change_plot_theme(p_plot, used_theme[[2]](used_fontsize, used_fontsize))
        }
      else {
        if (!is.null(p_plot))
          p_plot <-
            change_plot_theme(p_plot, used_theme[[2]]())

      }
      if (!is.null(used_colors))
        p_plot <-
          p_plot + scale_fill_manual(
            breaks = "Kruskal",
            values = used_colors[1],
            aesthetics = c("colour", "fill")
          )
      if (!is.null(used_axis_function))
        p_plot <-
          used_axis_function[[2]](p_plot,
                                  xlims = xlims,
                                  xend1 = xend1,
                                  xends2 = xends2)



      p_plot <-
        p_plot + theme(axis.title.x = element_text()) + scale_x_continuous(
          limits = xlims,
          minor_breaks = seq(xends2[1], xends2[2], 10),
          expand = c(0, 0)
        )

      if (spacer_plot == T) {
        spacer_plots <- rep(list(p_plot), 6 - length(series_vector))

        if (6 - length(series_vector) > 0)
          p_plot <-
            wrap_plots(c(p_plot, spacer_plots), ncol = 1)
      }
      else
        p_plot <- wrap_plots(p_plot, ncol = 1)

      comp_plot <-
        median_plot  / p_plot + plot_layout(ncol = 1,
                                            heights = c(1, ratio_Median_P_plot))

      finished_plot <- comp_plot




      return(finished_plot)
    }


    p_plot <- list()
    for (i in 2:length(series_vector)) {
      p_plot[[i - 1]] <-
        create_p_area_plot(
          list(IV_list[[series_vector[[1]]]], IV_list[[series_vector[[i]]]]),
          measurement_name[i],
          column_list,
          c(peak_list[1], peak_list[i]),
          "P-Value",
          "",
          ...
        )

      if (!is.null(used_fontsize))
        if (!is.null(used_font)) {
          p_plot[[i - 1]] <-
            change_plot_theme(p_plot[[i - 1]],
                              used_theme[[2]](used_fontsize, used_fontsize , used_font))
        } else {
          p_plot[[i - 1]] <-
            change_plot_theme(p_plot[[i - 1]], used_theme[[2]](used_fontsize, used_fontsize))
        }
      else {
        if (!is.null(p_plot[[i - 1]]))
          p_plot[[i - 1]] <-
            change_plot_theme(p_plot[[i - 1]], used_theme[[2]]())

      }
      if (!is.null(used_colors))
        p_plot[[i - 1]] <-
          p_plot[[i - 1]] + scale_fill_manual(
            breaks = measurement_name[i],
            values = used_colors[i],
            aesthetics = c("colour", "fill")
          )
      if (!is.null(used_axis_function))
        p_plot[[i - 1]] <-
          used_axis_function[[2]](p_plot[[i - 1]],
                                  xlims = xlims,
                                  xend1 = xend1,
                                  xends2 = xends2)


    }
    p_plot[[length(p_plot)]]  <-
      p_plot[[length(p_plot)]] + theme(axis.title.x = element_text()) + scale_x_continuous(
        name = "Potential (mV)",
        limits = xlims,
        minor_breaks = seq(xends2[1], xends2[2], 10),
        expand = c(0, 0)
      )


    if (spacer_plot == T) {
      spacer_plots <-
        rep(list(p_plot[[1]]), n_spacer - length(series_vector))
      if (n_spacer - length(series_vector) > 0)
        p_plot <- wrap_plots(c(p_plot, spacer_plots), ncol = 1)
    }
    else
      p_plot <- wrap_plots(p_plot, ncol = 1)

    comp_plot <-
      median_plot  / p_plot + plot_layout(ncol = 1,
                                          heights = c(1, ratio_Median_P_plot))

    finished_plot <- comp_plot





    return(finished_plot)
  }

complete_plot <-
  function(IV_list = NULL,
           summary_list = NULL ,
           column_list = NULL ,
           peak_list = NULL ,
           series_vector = NULL ,
           ylab = NULL,
           measurement_name = NULL,
           display_N_legend = NULL,
           display_N_legend_divisor = NULL,
           SD_MAD = NULL,
           SD_MAD_shadow_direction = NULL,
           used_theme = NULL ,
           used_axis_function = NULL ,
           used_font = NULL ,
           used_fontsize = NULL ,
           linesize = NULL ,
           used_colors = NULL,
           save_dir = NULL ,
           legend_seperate = FALSE,
           spacer_plot = FALSE,
           size = NULL,
           jittersize = NULL,
           n_spacer,
           ratio_Median_P_plot = 2,
           kruskal = FALSE,
           xlims = c(-104, 100),
           xend1 = -104,
           xends2 = c(-100, 100),
           customYaxis = FALSE,
           overrideYlim = NULL,
           ...) {
    # #####Medianplot of P-Plot#####
    # if (is.null(measurement_name) &&
    #     !is.null(series_vector))
    #   measurement_name <- series_vector
    #
    # median_plot <-
    #   create_matplot(IV_list[[series_vector[[1]]]],
    #                  column_list[1],
    #                  peak_list[[1]],
    #                  measurement_name[[1]],
    #                  "",
    #                  "",
    #                  linesize)
    # if (length(series_vector) > 1) {
    #   for (i in 2:length(series_vector)) {
    #     median_plot <-
    #       add_to_matplot(median_plot,
    #                      IV_list[[series_vector[[i]]]],
    #                      column_list[1],
    #                      peak_list[[i]],
    #                      measurement_name[[i]],
    #                      linesize)
    #
    #   }
    # }
    #
    # if (!is.null(used_fontsize))
    #   if (!is.null(used_font)) {
    #     median_plot <-
    #       change_plot_theme(median_plot,
    #                         used_theme[[1]](used_fontsize, used_fontsize , used_font))
    #   } else {
    #     median_plot <-
    #       change_plot_theme(median_plot, used_theme[[1]](used_fontsize, used_fontsize))
    #   }
    # else {
    #   if (!is.null(used_theme[[1]]))
    #     median_plot <-
    #       change_plot_theme(median_plot, used_theme[[1]]())
    #
    # }
    #
    # if (!is.null(used_colors))
    #   median_plot <-
    #   change_plot_color_fill_manuel(median_plot , measurement_name, used_colors[1:length(measurement_name)], 1)
    #
    # if (!is.null(used_axis_function))
    #   median_plot <-
    #   used_axis_function[[1]](
    #     median_plot,
    #     linesize,
    #     overrideYlim = NULL,
    #     xlims = xlims,
    #     xend1 = xend1,
    #     xends2 = xends2
    #   )
    #
    # median_plotP1 <-
    #   median_plot + theme(axis.title.x = element_blank(),
    #                       plot.margin = unit(c(0.2, 0.5, 0, 0), "cm"))
    #
    # detach("package:latex2exp", unload = TRUE)
    # library(latex2exp)
    # if (!is.null(ylab))
    #   median_plotP1 <- median_plotP1 + ylab(ylab[[1]])
    #
    #
    #
    #
    #
    # median_plot <-
    #   create_median_plot(
    #     IV_list[[series_vector[[1]]]],
    #     column_list[2],
    #     peak_list[[1]],
    #     measurement_name[[1]],
    #     "",
    #     "",
    #     linesize,
    #     SD_MAD,
    #     SD_MAD_shadow_direction[[1]]
    #   )
    # if (length(series_vector) > 1) {
    #   for (i in 2:length(series_vector)) {
    #     median_plot <-
    #       add_to_median_plot(
    #         median_plot,
    #         IV_list[[series_vector[[i]]]],
    #         column_list[2],
    #         peak_list[[i]],
    #         measurement_name[[i]],
    #         linesize,
    #         SD_MAD,
    #         SD_MAD_shadow_direction[[i]]
    #       )
    #
    #   }
    # }
    #
    # if (!is.null(used_fontsize))
    #   if (!is.null(used_font)) {
    #     median_plot <-
    #       change_plot_theme(median_plot,
    #                         used_theme[[1]](used_fontsize, used_fontsize , used_font))
    #   } else {
    #     median_plot <-
    #       change_plot_theme(median_plot, used_theme[[1]](used_fontsize, used_fontsize))
    #   }
    # else {
    #   if (!is.null(used_theme[[1]]))
    #     median_plot <-
    #       change_plot_theme(median_plot, used_theme[[1]]())
    #
    # }
    #
    # if (!is.null(used_colors))
    #   median_plot <-
    #   change_plot_color_fill_manuel(median_plot , measurement_name, used_colors[1:length(measurement_name)], 1)
    #
    # if (!is.null(used_axis_function))
    #   median_plot <-
    #   used_axis_function[[1]](
    #     median_plot,
    #     linesize,
    #     overrideYlim = overrideYlim,
    #     xlims = xlims,
    #     xend1 = xend1,
    #     xends2 = xends2
    #   )
    #
    # detach("package:latex2exp", unload = TRUE)
    # library(latex2exp)
    # median_plotP2 <-
    #   median_plot + theme(axis.title.x = element_blank(),
    #                       plot.margin = unit(c(0, 0.5, 0.2, 0), "cm"))
    #
    # if (!is.null(ylab))
    #   median_plotP2 <- median_plotP2 + ylab(ylab[[2]])
    # #####P-Plot Part#####
    #
    #
    # if (kruskal == T) {
    #   p_plot <-
    #     create_p_area_plot(
    #       IV_list,
    #       "Kruskal",
    #       column_list,
    #       peak_list,
    #       "\\textit{P} value",
    #       "",
    #       kruskal = kruskal,
    #       paired = paired
    #     )
    #
    #
    #   if (!is.null(used_fontsize))
    #     if (!is.null(used_font)) {
    #       p_plot <-
    #         change_plot_theme(p_plot,
    #                           used_theme[[2]](used_fontsize, used_fontsize , used_font))
    #     } else {
    #       p_plot <-
    #         change_plot_theme(p_plot, used_theme[[2]](used_fontsize, used_fontsize))
    #     }
    #   else {
    #     if (!is.null(p_plot))
    #       p_plot <-
    #         change_plot_theme(p_plot, used_theme[[2]]())
    #
    #   }
    #   if (!is.null(used_colors))
    #     p_plot <-
    #       p_plot + scale_fill_manual(
    #         breaks = "Kruskal",
    #         values = used_colors[1],
    #         aesthetics = c("colour", "fill")
    #       )
    #   if (!is.null(used_axis_function))
    #     p_plot <-
    #       used_axis_function[[2]](p_plot,
    #                               xlims = xlims,
    #                               xend1 = xend1,
    #                               xends2 = xends2)
    #
    #
    #
    #   p_plot <-
    #     p_plot + theme(axis.title.x = element_text()) + scale_x_continuous(
    #       name = "Potential (mV)",
    #       limits = xlims,
    #       minor_breaks = seq(xends2[1], xends2[2], 10),
    #       expand = c(0, 0)
    #     ) + theme(axis.title.x = element_text(margin  = margin(t = 1.5)))
    #
    #
    #
    #
    #
    #   if (spacer_plot == T) {
    #     spacer_plots <-
    #       rep(list(p_plot + theme(axis.title.x = element_blank())), n_spacer - 1)
    #
    #     if (n_spacer - 1 > 0)
    #       p_plot <-
    #         wrap_plots(c(list(p_plot), spacer_plots), ncol = 1)
    #   }
    #   else
    #     p_plot <- wrap_plots(p_plot, ncol = 1)
    #
    # }
    # else
    # {
    #   p_plot <- list()
    #   for (i in 2:length(series_vector)) {
    #     p_plot[[i - 1]] <-
    #       create_p_area_plot(
    #         list(IV_list[[series_vector[[1]]]], IV_list[[series_vector[[i]]]]),
    #         measurement_name[i],
    #         column_list[2],
    #         c(peak_list[1], peak_list[i]),
    #         "\\textit{P} value",
    #         "",
    #         ...
    #       )
    #
    #     if (!is.null(used_fontsize))
    #       if (!is.null(used_font)) {
    #         p_plot[[i - 1]] <-
    #           change_plot_theme(p_plot[[i - 1]],
    #                             used_theme[[2]](used_fontsize, used_fontsize , used_font))
    #       } else {
    #         p_plot[[i - 1]] <-
    #           change_plot_theme(p_plot[[i - 1]], used_theme[[2]](used_fontsize, used_fontsize))
    #       }
    #     else {
    #       if (!is.null(p_plot[[i - 1]]))
    #         p_plot[[i - 1]] <-
    #           change_plot_theme(p_plot[[i - 1]], used_theme[[2]]())
    #
    #     }
    #     if (!is.null(used_colors))
    #       p_plot[[i - 1]] <-
    #         p_plot[[i - 1]] + scale_fill_manual(
    #           breaks = measurement_name[i],
    #           values = used_colors[i],
    #           aesthetics = c("colour", "fill")
    #         )
    #     if (!is.null(used_axis_function))
    #       p_plot[[i - 1]] <-
    #         used_axis_function[[2]](p_plot[[i - 1]],
    #                                 xlims = xlims,
    #                                 xend1 = xend1,
    #                                 xends2 = xends2)
    #
    #
    #   }
    #   p_plot[[length(p_plot)]]  <-
    #     p_plot[[length(p_plot)]] + scale_x_continuous(
    #       name = "Potential (mV)",
    #       limits = xlims,
    #       minor_breaks = seq(xends2[1], xends2[2], 10),
    #       expand = c(0, 0)
    #     ) + theme(axis.title.x = element_text(margin  = margin(t = 1.5)))
    #
    #
    #   if (spacer_plot == T) {
    #     spacer_plots <-
    #       rep(list(p_plot[[1]] + theme(axis.title.x = element_blank())),
    #           n_spacer - length(series_vector))
    #     if (n_spacer - length(series_vector) > 0)
    #       p_plot <-
    #         wrap_plots(c(p_plot, spacer_plots), ncol = 1)
    #   }
    #   else
    #     p_plot <- wrap_plots(p_plot, ncol = 1)
    # }
    #
    # comp_plot <-
    #   median_plotP1 / median_plotP2  / p_plot + plot_layout(ncol = 1,
    #                                                         heights = c(1, 1, ratio_Median_P_plot))
    #
    # finished_plot <- comp_plot

    #return(finished_plot)


  }
