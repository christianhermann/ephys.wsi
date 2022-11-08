####Functions to import and process IVs measured with an HEKA EPC10####


#' Imports measurement(containing IVs and a Summarytable)
#'
#' @param quickImport
#'
#' @return
#' @export
#'
#' @examples
import_Data <- function(quickImport = F) {
  data_list <- import_excel_Data("summary_Data")

  walk(data_list[[1]], function(x) {
    if (!is_tibble(x)) {
      stop("The summaries
           couldnt be read into a tibble.")
    }
  })

  IV_list <- list()
  IV_dir <- list()
  IV_names <- list()
  IV_peaks <- list()
  if (quickImport == T) {
    sheet_names <- unlist(data_list["sheet_names"])
    IV_dir <- as.list(paste0(getwd(), "/ASC files/", sheet_names))
    IV_dir <-
      map(IV_dir, function(x) {
        list.files(
          x,
          pattern = "*.asc",
          recursive  = T,
          full.names = T,
          include.dirs = T
        )
      })
    names(IV_dir) <- sheet_names
    for (i in unlist(data_list["sheet_names"])) {
      print(i)
      IV_names[[i]] <-
        str_remove(word(IV_dir[[i]], -1, sep = fixed("/")), ".asc")

      #    IV_length <- map(IV_dir[[i]], read_asc_Length)

      IV_skip <- map(IV_dir[[i]], find_header_row, "Index")

      IV_list[[i]] <-
        map2(IV_dir[[i]], IV_skip, function(dir, length) {
          import_asc_Data(dir, length - 1)
        })
      names(IV_list[[i]]) <- IV_names[[i]]

      IV_peaks[[i]] <- unique(flatten_chr(map(
        IV_names[[i]],
        word,
        start = -1,
        sep = "_"
      )))

      walk(IV_list[[i]], function(x) {
        if (!is_tibble(x)) {
          stop(
            "IVs couldnt be read into a tibble.
          Most likely something is wrong with the IV
          formatting."
          )
        }
      })

      print(paste0(i, " done"))
    }
  }
  if (quickImport == F) {
    for (i in unlist(data_list["sheet_names"])) {
      IV_dir[[i]] <-
        gfile(
          paste0("Import IVs for ", i),
          type = "open",
          multi = T,
          filter = list(
            "ASC files" = list(patterns = c("*asc", "*.ASC")),
            "All files" = list(patterns = c("*"))
          )
        )
      IV_names[[i]] <-
        str_remove(word(IV_dir[[i]], -1, sep = "\\\\"), ".asc")

      #    IV_length <- map(IV_dir[[i]], read_asc_Length)

      IV_skip <- map(IV_dir[[i]], find_header_row, "Index")

      IV_list[[i]] <-
        map2(IV_dir[[i]], IV_skip, function(dir, length) {
          import_asc_Data(dir, length - 1)
        })
      names(IV_list[[i]]) <- IV_names[[i]]

      IV_peaks[[i]] <- unique(flatten_chr(map(
        IV_names[[i]],
        word,
        start = -1,
        sep = "_"
      )))

      walk(IV_list[[i]], function(x) {
        if (!is_tibble(x)) {
          stop(
            "IVs couldnt be read into a tibble.
          Most likely something is wrong with the IV
          formatting."
          )
        }
      })
    }
  }

  return(
    list(
      summary_list = data_list[[1]],
      sheet_names = data_list[[2]],
      IV_list = IV_list,
      IV_names = IV_names,
      IV_peaks = IV_peaks
    )
  )
}






#' Prepares/Cleanup measurement data for later use. Renames bad columns,
#' creates an ID Column in the summary, fixes names, trims the IVs
#' Exports Currents to Summary if wanted
#'
#' @param summary_list
#' @param IV_list
#' @param IV_names
#' @param exportCurrent
#' @param markers
#'
#' @return
#' @export
#'
#' @examples
prepare_data <-
  function(summary_list,
           IV_list,
           IV_names,
           IV_peaks,
           exportCurrent = F,
           markers = NULL) {
    testList(summary_list, any.missing = FALSE)
    testList(IV_list, any.missing = FALSE)
    testList(IV_names, any.missing = FALSE)

    summary_list <-
      map(summary_list, rename_column, "Messung", "Name")
    summary_list <- map(summary_list, order_data, "Name")
    IV_list <- map(IV_list, order_data)
    IV_names <- map(IV_names, order_data)

    summary_list <- map(summary_list, create_id, "Name")

    IV_list <-
      map_depth(IV_list, 2, function(x) {
        colnames(x) <- str_replace_all(colnames(x), '"', "")
        return(x)
      })
    IV_list <-
      map_depth(IV_list, 2, rename_column, "I-mon[A]", "CurrentIn[A]")
    IV_list <-
      map_depth(IV_list, 2, rename_column, "Imon[A]", "CurrentIn[A]")
    IV_list <-
      map_depth(IV_list,
        2,
        rename_column,
        old_name = "Stimulus",
        new_name = "Potential[V]"
      )
    IV_list <-
      map2(IV_list, IV_names, function(ivs, names) {
        map2(ivs, names, select_column_manual, search_name = "CurrentIn[A]")
      })

    settings_envir$markers <- markers
    settings_envir$exportCurrent <- exportCurrent
    data_storage_envir$origExample <- IV_list[[1]][[1]]


    if (exportCurrent == T) {
      splitted_IV_list <-
        map2(IV_list, IV_names, split_list_by_pharmacon)
      meanCurrents <-
        pmap(
          list(splitted_IV_list, IV_names, summary_list), function(IV_list,
                                                                   IV_names,
                                                                   summary_list,
                                                                   markers) {
            map_depth(
              IV_list, 2, function(IVs, markers) {
                currents <-
                  c(
                    mean(pull(IVs[markers[1]:markers[2], 3])) * 10^12,
                    mean(pull(IVs[markers[3]:markers[4], 3])) * 10^12
                  )
              },
              markers
            )
          },
          markers
        )

      meanCurs <- list()
      for (i in 1:length(meanCurrents)) {
        meanCurs[[i]] <- pmap(meanCurrents[[i]], rbind)
      }
      names(meanCurs) <- names(meanCurrents)


      meanCursNames <-
        map(meanCurs, function(meanCurs) {
          word(names(meanCurs), 1, -2, "_")
        })
      summary_list <-
        pmap(list(meanCurs, meanCursNames, summary_list), function(curValue, curName, summary) {
          map2_dfr(curValue, curName, function(curValue, curName, summary) {
            for (i in 1:length(rownames(curValue))) {
              # summary[which(summary[, 1] ==  curName), ((3 * i) + 1):((3 * i) + 2)] <-
              #   as.list(curValue[i, ])
              newColumns <- as.list(curValue[i, ])
              newColumnsNames <- c(paste0("Imin", rownames(curValue))[i], paste0("Imax", rownames(curValue))[i])
              summary <- add_column(summary[which(summary[, 1] == curName), ], !!newColumnsNames[1] := newColumns[[1]], !!newColumnsNames[2] := newColumns[[2]], .after = ((3 * i)))
            }
            return(summary)
          }, summary)
        })
    }


    IV_list <-
      map_depth(
        IV_list,
        2,
        trim_measurement,
        from = min(settings_envir$ramp_data),
        to = max(settings_envir$ramp_data)
      )

    IV_list <-
      map_depth(IV_list, 2, trim_tibble, settings_envir$asc_columns)


    walk2(IV_list, summary_list, function(IV_list, summary_list) {
      if (length(IV_list) %/% nrow(summary_list["Name"]) %% 1 != 0) {
        stop(
          "IVs and summary are not in sync. The amount of IVs is not a multiple of the amount of measurements."
        )
      }
    })
    return(list(
      summary_list = summary_list,
      IV_list = IV_list,
      IV_names = IV_names
    ))
  }



#' Detect the offset of a measurement and create a column with the offset and one offset corrected
#'
#' @param IV_list
#'
#' @return
#' @export
#'
#' @examples
detect_offset <- function(IV_list) {
  IV_offset <-
    map_depth(IV_list, 2, function(IV) {
      pull(IV[IV["Potential[V]"] == 0, ]["CurrentIn[A]"])
    })
  IV_list <-
    map2(IV_list, IV_offset, function(IV, IV_offset) {
      map2(IV, IV_offset, function(iv, IV_offset) {
        add_column(
          iv,
          Offset = IV_offset,
          "CurrentIn[A]_OffsetCor" = pull(iv["CurrentIn[A]"]) - IV_offset
        )
      })
    })

  walk(IV_offset, function(IV_offset) {
    walk(IV_offset, function(IV_offset) {
      if (anyNA(IV_offset)) {
        stop(
          "Offset couldn't be calculated. Check CurrentIn or Potential for NAs."
        )
      }
    })
  })

  data_storage_envir$IV_offset <- IV_offset

  return(IV_list)
}



#' Uses the summary table Cm and the CurrentIn to calculate the current Desitiy
#'
#' @param IV_list
#' @param IV_names
#' @param summary_list
#' @param IV_offset
#' @param splitFit
#' @param splitPotential
#'
#' @return
#' @export
#'
#' @examples
calculate_current_density <-
  function(IV_list,
           IV_names,
           summary_list,
           IV_offset,
           splitFit,
           splitPotential) {
    check_list(IV_list)
    check_list(IV_names)
    check_list(summary_list)
    check_number(IV_offset)


    IV_list <- IV_list %>%
      map_depth(2, function(iv) {
        add_column(iv, "CurrentDensity[pA/pF]" = pull(iv["CurrentIn[A]_OffsetCor"]) * 10^12)
      })


    IV_list <-
      pmap(list(IV_list, IV_names, summary_list), function(IV, names, summary) {
        map2(IV, names, function(iv, name, summary) {
          print(name)
          iv["CurrentDensity[pA/pF]"] <-
            iv["CurrentDensity[pA/pF]"] / pull(summary[which(summary[, 1] == word(name, 1, -2, sep = "_")), "Cm"])
          iv
        }, summary)
      })


    if (splitFit == TRUE) {
      splitX <-
        which(pull(IV_list[[1]][[1]], "Potential[V]") == splitPotential)



      Inward <- map_depth(IV_list, 2, function(iv) {
        pull(iv["CurrentDensity[pA/pF]"])[which(iv["Potential[V]"] == min(settings_envir$ramp_data)):splitX]
      })
      Outward <- map_depth(IV_list, 2, function(iv) {
        pull(iv["CurrentDensity[pA/pF]"])[splitX:which(iv["Potential[V]"] == max(settings_envir$ramp_data))]
      })



      Inward <-
        map_depth(
          Inward,
          2,
          function(Inward, splitX) {
            c(Inward, rep(NA, splitX - 1))
          }, splitX
        )

      Outward <-
        map_depth(
          Outward,
          2,
          function(Outward, splitX) {
            c(rep(NA, splitX - 1), Outward)
          }, splitX
        )


      IV_list <-
        pmap(list(Inward, Outward, IV_list), function(Inward,
                                                      Outward,
                                                      IV_list) {
          pmap(list(Inward, Outward, IV_list), function(`CurrentDensity[pA/pF]_Inward`,
                                                        `CurrentDensity[pA/pF]_Outward`,
                                                        IV_list) {
            cbind(
              IV_list,
              `CurrentDensity[pA/pF]_Inward`,
              `CurrentDensity[pA/pF]_Outward`
            )
          })
        })
    }



    walk(IV_list, function(IV_list) {
      walk(IV_list, function(IV) {
        if (anyNA(IV["CurrentDensity[pA/pF]"])) {
          stop(
            "Current Density couldn't be calculated. Most likely the filenames and the names in the summary aren't identical"
          )
        }
      })
    })

    return(IV_list)
  }

#' Title
#'
#' @param IV_list
#' @param forceThroughRevPot
#' @param revPotential
#' @param splitFit
#' @param splitPotential
#' @param spar
#'
#' @return
#' @export
#'
#' @examples
smooth_IVs <-
  function(IV_list,
           forceThroughRevPot,
           revPotential,
           splitFit,
           splitPotential,
           spar = 0.5) {
    check_list(IV_list)


    IV_fit <-
      map_depth(
        IV_list,
        2,
        fit_smoothing_spline_tbl,
        "Potential[V]",
        "CurrentDensity[pA/pF]",
        spar,
        forceThroughRevPot,
        revPotential,
        splitFit,
        splitPotential,
        "smoothed_CurrentDensity"
      )


    IV_list <-
      map2(IV_fit, IV_list, function(IV_fit, IV_list) {
        map2(IV_fit, IV_list, function(smoothed_CurrentDensity,
                                       IV_list) {
          cbind(IV_list, smoothed_CurrentDensity)
        })
      })

    return(IV_list)
  }




#' Calculates the normalized current density
#'
#' @param IV_list
#' @param splitFit
#' @param splitPotential
#'
#' @return
#' @export
#'
#' @examples
calculate_normed_current_density <-
  function(IV_list, splitFit, splitPotential) {
    check_list(IV_list)


    if (splitFit == TRUE) {
      data_storage_envir$splitPotential <- splitPotential


      normalized_IV_in <- map_depth(
        IV_list,
        2,
        normalize_data,
        name = "Potential[V]",
        norm_name = "smoothed_CurrentDensity",
        from = min(settings_envir$ramp_data),
        to = splitPotential,
        scale = 100
      )

      normalized_IV_out <- map_depth(
        IV_list,
        2,
        normalize_data,
        name = "Potential[V]",
        norm_name = "smoothed_CurrentDensity",
        from = splitPotential,
        to = max(settings_envir$ramp_data),
        scale = 100
      )


      # IV_list_in <- map_depth(IV_list,2, trim_measurement_which, min(settings_envir$ramp_data), 0, name = "Potential[V]")
      #
      # IV_list_in <-
      #   map2(normalized_IV_in, IV_list_in, function(normalized_IV_in, IV_list_in)
      #     map2(normalized_IV_in, IV_list_in, function(normalized_CurrentDensity, IV_list_in)
      #       cbind(IV_list_in,"normalized_CurrentDensity" = normalized_CurrentDensity - 100)))
      #
      # IV_list_out <- map_depth(IV_list,2, trim_measurement_which, 0, max(settings_envir$ramp_data),name = "Potential[V]")
      #
      # IV_list_out <-
      #   map2(normalized_IV_out, IV_list_out, function(normalized_IV_out, IV_list_out)
      #     map2(normalized_IV_out, IV_list_out, function(normalized_CurrentDensity, IV_list_out)
      #       cbind(IV_list_out, normalized_CurrentDensity)))



      normalized_IV <-
        map2(normalized_IV_in, normalized_IV_out, function(n_in, n_out) {
          map2(n_in, n_out, function(n_in, n_out) {
            c((n_in[-length(n_in)]) - 100, n_out)
          })
        })


      splitX <-
        which(pull(IV_list[[1]][[1]], "Potential[V]") == splitPotential)



      normalized_IV_in <-
        map_depth(
          normalized_IV_in,
          2,
          function(normIV, splitX) {
            c(normIV, rep(NA, splitX - 1))
          }, splitX
        )

      normalized_IV_out <-
        map_depth(
          normalized_IV_out,
          2,
          function(normIV, splitX) {
            c(rep(NA, splitX - 1), normIV)
          }, splitX
        )



      IV_list <-
        pmap(list(
          normalized_IV,
          normalized_IV_in,
          normalized_IV_out,
          IV_list
        ), function(normalized_IV,
                    normalized_IV_in,
                    normalized_IV_out,
                    IV_list) {
          pmap(list(
            normalized_IV,
            normalized_IV_in,
            normalized_IV_out,
            IV_list
          ), function(normalized_CurrentDensity,
                      normalized_CurrentDensity_Inward,
                      normalized_CurrentDensity_Outward,
                      IV_list) {
            cbind(
              IV_list,
              normalized_CurrentDensity,
              "normalized_CurrentDensity_Inward" = normalized_CurrentDensity_Inward - 100,
              normalized_CurrentDensity_Outward
            )
          })
        })
    }

    if (splitFit == FALSE) {
      normalized_IV_in <- map_depth(
        IV_list,
        2,
        normalize_data,
        name = "Potential[V]",
        norm_name = "smoothed_CurrentDensity",
        from = min(settings_envir$ramp_data),
        to = 0,
        scale = 100
      )

      normalized_IV_out <- map_depth(
        IV_list,
        2,
        normalize_data,
        name = "Potential[V]",
        norm_name = "smoothed_CurrentDensity",
        from = 0,
        to = max(settings_envir$ramp_data),
        scale = 100
      )


      IV_list_in <-
        map_depth(IV_list,
          2,
          trim_measurement_which,
          min(settings_envir$ramp_data),
          0,
          name = "Potential[V]"
        )

      IV_list_in <-
        map2(normalized_IV_in, IV_list_in, function(normalized_IV_in, IV_list_in) {
          map2(normalized_IV_in, IV_list_in, function(normalized_CurrentDensity,
                                                      IV_list_in) {
            cbind(IV_list_in, "normalized_CurrentDensity" = normalized_CurrentDensity - 100)
          })
        })

      IV_list_out <-
        map_depth(IV_list,
          2,
          trim_measurement_which,
          0,
          max(settings_envir$ramp_data),
          name = "Potential[V]"
        )

      IV_list_out <-
        map2(normalized_IV_out, IV_list_out, function(normalized_IV_out, IV_list_out) {
          map2(normalized_IV_out, IV_list_out, function(normalized_CurrentDensity,
                                                        IV_list_out) {
            cbind(IV_list_out, normalized_CurrentDensity)
          })
        })





      normalized_IV <-
        map2(normalized_IV_in, normalized_IV_out, function(n_in, n_out) {
          map2(n_in, n_out, function(n_in, n_out) {
            c((n_in[-length(n_in)]) - 100, n_out)
          })
        })

      IV_list <-
        map2(normalized_IV, IV_list, function(normalized_IV, IV_list) {
          map2(normalized_IV, IV_list, function(normalized_CurrentDensity, IV_list) {
            cbind(IV_list, normalized_CurrentDensity)
          })
        })
    }
    return(IV_list)
  }



#' Fits a cubic smoothing spline to the IV. (noise reduction)
#'
#' @param IV_list
#' @param forceThroughRevPot
#' @param revPotential
#' @param spar
#' @param splitFit
#' @param splitPotential
#'
#' @return
#' @export
#'
#' @examples
calculate_model <-
  function(IV_list,
           forceThroughRevPot,
           revPotential,
           splitFit,
           splitPotential,
           spar = 0.5) {
    check_list(IV_list)


    IV_fit <-
      map_depth(
        IV_list,
        2,
        fit_smoothing_spline_tbl,
        "Potential[V]",
        "normalized_CurrentDensity",
        spar,
        forceThroughRevPot,
        revPotential,
        splitFit,
        splitPotential,
        "fitted_normalized_CurrentDensity"
      )


    IV_list <-
      map2(IV_fit, IV_list, function(IV_fit, IV_list) {
        map2(IV_fit, IV_list, function(fitted_normalized_CurrentDensity,
                                       IV_list) {
          cbind(IV_list, fitted_normalized_CurrentDensity)
        })
      })

    return(IV_list)
  }
# calculate_model <- function(IV_list, IV_list_in, IV_list_out)
# {
#   IV_fit_in <-
#     map_depth(IV_list_in,
#               2,
#               create_glm_mode_tbl,
#               "Potential[V]",
#               "normalized_CurrentDensity",
#               5)
#
#   IV_fit_out <-
#     map_depth(IV_list_out,
#               2,
#               create_glm_mode_tbl,
#               "Potential[V]",
#               "normalized_CurrentDensity",
#               5)
#
#
#   IV_fit <-
#     map2(IV_fit_in, IV_fit_out, function(n_in , n_out)
#       map2(n_in, n_out, function(n_in, n_out)
#         c((n_in[-length(n_in)]), n_out)))
#
#
#   IV_list <-
#     map2(IV_fit, IV_list, function(IV_fit, IV_list)
#       map2(IV_fit, IV_list, function(fitted_normalized_CurrentDensity,
#                                      IV_list)
#         cbind(IV_list, fitted_normalized_CurrentDensity)))
#
#   return(IV_list)
#
# }

#' calculates the slope conductyvity for a tibble which has a
#' column names "fitted_normalized_currentDensity
#'
#' @param IV_list
#' @param splitFit
#'
#' @return
#' @export
#'
#' @examples
calculate_slope_conductivity <- function(IV_list, splitFit) {
  check_list(IV_list)

  slope <-
    map_depth(
      IV_list,
      2,
      calculate_slope_tibble,
      "normalized_CurrentDensity",
      "Potential[V]"
    )

  if (splitFit == FALSE) {
    IV_list <-
      map2(slope, IV_list, function(slope, IV_list) {
        map2(slope, IV_list, function(normalized_slopeConductance,
                                      IV_list) {
          cbind(IV_list, normalized_slopeConductance)
        })
      })
  }

  if (splitFit == TRUE) {
    slopeInward <-
      map_depth(
        IV_list,
        2,
        calculate_slope_tibble,
        "normalized_CurrentDensity_Inward",
        "Potential[V]"
      )

    slopeOutward <-
      map_depth(
        IV_list,
        2,
        calculate_slope_tibble,
        "normalized_CurrentDensity_Outward",
        "Potential[V]"
      )

    slope <-
      pmap(list(slope, slopeInward, slopeOutward), function(slope, Inward, Outward) {
        pmap(list(slope, Inward, Outward), function(slope, Inward, Outward) {
          data.frame(
            normalized_slopeConductance = slope,
            normalized_slopeConductance_Inward = Inward,
            normalized_slopeConductance_Outward = Outward
          )
        })
      })

    IV_list <-
      map2(slope, IV_list, function(slope, IV_list) {
        map2(slope, IV_list, function(normalized_slopeConductance,
                                      IV_list) {
          cbind(IV_list, normalized_slopeConductance)
        })
      })
  }



  return(IV_list)
}

#' detects outliers in an IV via grubbs. For every point in an IV a grubbs test will be done covering all measurements
#'
#' @param IV_list
#' @param IV_names
#' @param outlier_column
#' @param outlier_treshhold
#'
#' @return a list containing the IV_list without the outlier measurements and a tibble showing the number of outliers found for each measurement
#' @export
#'
#' @examples
detect_IV_outlier <-
  function(IV_list,
           IV_names,
           outlier_column,
           outlier_treshhold,
           bef = FALSE) {
    check_list(IV_list)
    check_list(IV_names)
    check_string(outlier_column)
    check_numeric(outlier_treshhold)

    splitted_IV_list <-
      map2(IV_list, IV_names, split_list_by_pharmacon)
    splitted_IV_names <-
      map2(IV_names, IV_names, split_list_by_pharmacon)
    splitted_IV_tbl <-
      map_depth(splitted_IV_list, 2, tbl_list_to_column_tbl, outlier_column)
    splitted_outlier_vec <-
      map_depth(splitted_IV_tbl, 2, function(tbl) {
        apply(tbl, 1, grubbs.flag_results)
      })
    splitted_outlier_vec <-
      map_depth(splitted_outlier_vec, 2, function(row) {
        rowSums(row, na.rm = T)
      })
    # splitted_outlier_names <- map2(splitted_outlier_vec,splitted_IV_names, function(outliers, names,outlier_treshhold) map2(outliers, names, function(outlier, name,outlier_treshhold) word(name[outlier > outlier_treshhold],1,-2,sep="_"),outlier_treshhold),outlier_treshhold)
    # splitted_outlier_names <- p<- map2(splitted_outlier_names,splitted_IV_names, function(outlier_names, iv_names)!str_detect(iv_names,gsub("\\)", "\\\\)",gsub("\\(", "\\\\(",outlier_names))))
    names_tibble <- map(splitted_IV_names, bind_cols)

    outlier_tibble <- map(splitted_outlier_vec, bind_cols)
    # outlier_vec <- map(outlier_tibble,rowSums)
    if (bef == TRUE) {
      outlier_tibble <-
        map(outlier_tibble, function(tbl) {
          mutate(tbl, is_outlier = rowSums(tbl) >= outlier_treshhold * length(tbl))
        })
    }
    if (bef == FALSE) {
      outlier_tibble <-
        map(outlier_tibble, function(tbl) {
          mutate(tbl, is_outlier = rowSums(tbl[-which(colnames(tbl) == "Bef")]) >= outlier_treshhold * length(tbl[-which(colnames(tbl) == "Bef")]))
        })
    }
    splitted_IV_list <-
      map2(splitted_IV_list, outlier_tibble, function(iv_list, outlier) {
        map(iv_list, function(x) {
          x[!outlier["is_outlier"]]
        })
      })
    splitted_outlier_IV_list <-
      map2(splitted_IV_list, outlier_tibble, function(iv_list, outlier) {
        map(iv_list, function(x) {
          x[pull(outlier, "is_outlier")]
        })
      })
    outlier_list <- map(splitted_outlier_IV_list, flatten)
    IV_list <- map(splitted_IV_list, flatten)
    outlier_tbl <- map2(names_tibble, outlier_tibble, bind_cols)
    return(
      list(
        outlier_tibble = outlier_tbl,
        outlier_corrected_IV_list = IV_list,
        outlier_IV_list = outlier_list
      )
    )
  }
#' Saves an IV list to a directory
#'
#' @param IV_list
#' @param IV_names
#'
#' @return
#' @export
#'
#' @examples
save_IV_list <- function(IV_list, IV_names, sheet_names) {
  check_list(IV_list)
  check_list(IV_names)

  directory <- gfile(
    "Save IVs",
    type = "selectdir",
    multi = T,
    filter = list(
      "ASC files" = list(patterns = c("*asc", "*.ASC")),
      "All files" = list(patterns = c("*"))
    )
  )

  walk(sheet_names, function(sheet_name, directory) {
    check_and_create_folder(directory, sheet_name)
  }, directory = directory)
  IV_locations <-
    map2(IV_names, sheet_names, function(names, sheet_name) {
      map(names, function(name, directory, sheet_name) {
        paste0(file.path(directory, sheet_name, name), ".asc")
      }, directory, sheet_name)
    })
  walk2(IV_list, IV_locations, function(iv_list, iv_locations) {
    walk2(iv_list, iv_locations, save_IV_ASC)
  })
}


#' Used to change the unit of the stimulus/potential for a whole measurement
#'
#' @param IV_list
#' @param old_unit
#' @param new_unit
#' @param unit_factor factor from the old to new unit
#'
#' @return
#' @export
#'
#' @examples change_potential_unit(data_storage_envir$IV_list, "V", "mV", 1000)
change_potential_unit <-
  function(IV_list, old_unit, new_unit, unit_factor) {
    IV_list <-
      map_depth(IV_list, 2, function(IV_list, old_unit, unit_factor) {
        IV_list[[paste0("Potential[", old_unit, "]")]] <-
          IV_list[[paste0("Potential[", old_unit, "]")]] * unit_factor
        return(IV_list)
      }, old_unit, unit_factor)
    IV_list <-
      map_depth(IV_list, 2, function(IV_list, old_unit, new_unit) {
        colnames(IV_list)[colnames(IV_list) == paste0("Potential[", old_unit, "]")] <-
          paste0("Potential[", new_unit, "]")
        return(IV_list)
      }, old_unit, new_unit)
    settings_envir$voltage_unit <- new_unit
    settings_envir$ramp_data <-
      settings_envir$ramp_data * unit_factor
    settings_envir$step <- settings_envir$step * unit_factor
    return(IV_list)
  }


#' Exports a value of out an IV column and puts it into long format
#'
#' @param IV_list
#' @param IV_names
#' @param column
#' @param peak_list
#'
#' @return
#' @export
#'
#' @examples
export_IV_value <-
  function(IV_list,
           IV_names,
           column,
           position,
           peak_list,
           range) {
    # column_data <- map_depth(IV_list, 2, function(IV, col)
    #   pull(IV[col])[which(position == pull(IV, paste0("Potential[",settings_envir$voltage_unit,"]")))], column)

    # IV_list <- map(IV_list, function(IVs) return(IVs[!str_detect(names(IVs), "Bef")]))

    #  IV_list <- map(IV_list, function(list) list[order(names(list))])

    if (any(unlist(flatten(
      map2(IV_list, IV_names, function(iv, name) {
        names(iv) == name
      })
    )) == FALSE)) {
      stop("Error: IVs and Names arent aligned")
    }

    column_data <- map_depth(IV_list, 2, function(IV, col) {
      mean(pull(IV[col])[c(
        which(position == pull(
          IV, paste0("Potential[", settings_envir$voltage_unit, "]")
        )) - range,
        which(position == pull(
          IV, paste0("Potential[", settings_envir$voltage_unit, "]")
        )) + range
      )])
    }, column)

    column_data <- map(column_data, flatten_dbl)

    column_data_splitted_list <-
      map2(column_data, IV_names, split_list_by_pharmacon)

    column_data_peaks <-
      map2(column_data_splitted_list, peak_list, function(x, y) {
        lst(!!y := x[[y]])
      })

    column_data_peaks <-
      map2(column_data_peaks, names(column_data_peaks), function(x, y) {
        x <- c(x, list(series = (rep(
          y, length(x[[1]])
        ))))
      })

    column_data_df <- bind_rows(column_data_peaks)



    column_data_df$series <- factor(column_data_df$series,
      levels = unique(column_data_df$series)
    )

    column_data_long <-
      gather(
        column_data_df,
        col,
        value,
        colnames(column_data_df)[-which(colnames(column_data_df) == "series")],
        na.rm = T,
        factor_key = T
      )


    column_data_long[["col"]] <-
      paste0(
        column_data_long[["series"]],
        ": ",
        column_data_long[["col"]],
        ": ",
        column,
        ": ",
        position
      )

    column_data_long[["col"]] <-
      factor(column_data_long[["col"]], levels = unique(column_data_long[["col"]]))


    return(column_data_long)
  }

#' Title
#'
#' @param IV
#' @param summary
#' @param outlier_column
#'
#' @return
#' @export
#'
#' @examples
deselect_IV_outlier <- function(IV, summary, outlier_column) {
  return(IV[!word(names(IV), 1, -2, "_") %in% summary$Name[summary[[outlier_column]]]])
}


#' Runs through all process steps
#'
#' @return
#' @export
#'
#' @examples
process_IVs <-
  function(spar,
           quickImport,
           forceThroughRevPot,
           revPotential,
           splitFit,
           splitPotential,
           exportCurrent,
           markers,
           ...) {
    data_list <- import_Data(quickImport)
    list2env(data_list, data_envir)
    data_list_prepared <-
      prepare_data(
        data_envir$summary_list,
        data_envir$IV_list,
        data_envir$IV_names,
        IV_peaks,
        exportCurrent,
        markers
      )
    list2env(data_list_prepared, data_envir)
    data_list_offset <- detect_offset(data_envir$IV_list)
    data_envir$IV_list <- data_list_offset
    data_list_CD <-
      calculate_current_density(
        data_envir$IV_list,
        data_envir$IV_names,
        data_envir$summary_list,
        data_storage_envir$IV_offset,
        splitFit,
        splitPotential
      )
    data_envir$IV_list <- data_list_CD

    data_list_model <-
      smooth_IVs(
        data_envir$IV_list,
        forceThroughRevPot,
        revPotential,
        splitFit,
        splitPotential,
        spar
      )

    data_envir$IV_list <- data_list_model


    data_list_nCD <-
      calculate_normed_current_density(data_envir$IV_list, splitFit, splitPotential)
    data_envir$IV_list <- data_list_nCD

    data_list_slope <-
      calculate_slope_conductivity(data_envir$IV_list, splitFit)
    data_envir$IV_list <- data_list_slope
  }


#' Runs through all process steps
#'
#' @return
#' @export
#'
#' @examples
process_IVs_OLD090422 <-
  function(spar,
           quickImport,
           forceThroughRevPot,
           revPotential,
           splitFit,
           splitPotential,
           exportCurrent,
           markers,
           ...) {
    data_list <- import_Data(quickImport)
    list2env(data_list, data_envir)
    data_list_prepared <-
      prepare_data(
        data_envir$summary_list,
        data_envir$IV_list,
        data_envir$IV_names,
        IV_peaks,
        exportCurrent,
        markers
      )
    list2env(data_list_prepared, data_envir)
    data_list_offset <- detect_offset(data_envir$IV_list)
    data_envir$IV_list <- data_list_offset
    data_list_CD <-
      calculate_current_density(
        data_envir$IV_list,
        data_envir$IV_names,
        data_envir$summary_list,
        data_storage_envir$IV_offset,
        splitFit,
        splitPotential
      )
    data_envir$IV_list <- data_list_CD
    data_list_nCD <-
      calculate_normed_current_density(data_envir$IV_list, splitFit, splitPotential)
    data_envir$IV_list <- data_list_nCD
    data_list_model <-
      calculate_model(
        data_envir$IV_list,
        forceThroughRevPot,
        revPotential,
        splitFit,
        splitPotential,
        spar
      )
    data_envir$IV_list <- data_list_model
    data_list_slope <-
      calculate_slope_conductivity(data_envir$IV_list, splitFit)
    data_envir$IV_list <- data_list_slope
  }


#' Creates an IV_list; Run through all existing process steps
#'
#' @return
#' @export
#'
#' @examples
process_new_IVs <-
  function(spar = 0.8,
           quickImport = FALSE,
           forceThroughRevPot = FALSE,
           revPotential = 0,
           splitFit = FALSE,
           splitPotential = 0,
           exportCurrent = F,
           markers = NULL,
           ...) {
    #  list2env(list(...), environment())
    process_IVs(
      spar,
      quickImport,
      forceThroughRevPot,
      revPotential,
      splitFit,
      splitPotential,
      exportCurrent,
      markers,
      ...
    )
    data_storage_envir$IV_list <- data_envir$IV_list
    data_storage_envir$summary_list <- data_envir$summary_list
    data_storage_envir$IV_names <- data_envir$IV_names
    data_storage_envir$sheet_names <- data_envir$sheet_names
    data_storage_envir$IV_peaks <- data_envir$IV_peaks
  }
#' Add a new Measurement; Run through all existing process steps; Add new list to the current existing list
#'
#' @param spar
#'
#' @return
#' @export
#'
#' @examples
add_new_processed_IV <- function(spar = 0.8) {
  process_IVs(spar)
  data_storage_envir$IV_list <-
    c(data_storage_envir$IV_list, data_envir$IV_list)
  data_storage_envir$summary_list <-
    c(data_storage_envir$summary_list, data_envir$summary_list)
  data_storage_envir$IV_names <-
    c(data_storage_envir$IV_names, data_envir$IV_names)
  data_storage_envir$sheet_names <-
    c(data_storage_envir$sheet_names, data_envir$sheet_names)
  data_storage_envir$IV_peaks <-
    c(data_storage_envir$IV_peaks, data_envir$IV_peaks)
}

#' Adds a new Measurement to an existing one; Runs through all existing process steps, merging the new IVlist with the existing one
#'
#' @return
#' @export
#'
#' @examples
expand_processed_IV <- function(spar = 0.8) {
  process_IVs(spar)
  data_storage_envir$IV_list <-
    list_merge(data_storage_envir$IV_list, data_envir$IV_list)
  data_storage_envir$summary_list <-
    list_merge(data_storage_envir$summary_list, data_envir$summary_list)
  data_storage_envir$IV_names <-
    list_merge(data_storage_envir$IV_names, data_envir$IV_names)
  data_storage_envir$sheet_names <-
    list_merge(data_storage_envir$sheet_names, data_envir$sheet_names)
  data_storage_envir$IV_peaks <-
    list_merge(data_storage_envir$IV_peaks, data_envir$IV_peaks)
}
