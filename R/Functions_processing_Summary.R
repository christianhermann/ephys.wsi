####Functions to import and process excel summaries for experiments measured with an HEKA EPC10####


#' Imports a summary
#'
#' @return
#' @export
#'
#' @examples
import_summary <- function() {
  summary_data <- import_excel_Data("summary_Data")
  return(list(summary_list = data_list[[1]], sheet_names = data_list[[2]]))
}
#' Extract the IV-Offset out of an IV
#'
#' @param IV_list
#'
#' @return
#' @export
#'
#' @examples
export_IV_offset <- function(IV_list) {
  check_list(IV_list)
  offset <- map_depth(IV_list, 2, function(IV) {
    pull(IV["Offset"])[1]
  })
  offset <- map(offset, flatten_dbl)
  return(offset)
}


#' Export the offset from a IV_list to a summary_list and changes the unit to pA
#'
#' @param offset_list
#' @param summary_list
#' @param IV_names
#' @param peak_list
#'
#' @return
#' @export
#'
#' @examples
export_offset_to_summary <-
  function(offset_list,
           summary_list,
           IV_names,
           peak_list) {
    check_list(offset_list)
    check_list(summary_list)
    check_list(IV_names)
    check_list(peak_list)

    offset_list <- map(data_storage_envir$IV_offset, function(x) x * 10^12)
    Offset_splitted_list <-
      map2(offset_list, IV_names, split_list_by_pharmacon)

    Offset_splitted_list <- map2(Offset_splitted_list, peak_list, function(offset_list, peak) offset_list[order(unlist(peak))])

    splitted_summary_list <-
      pmap(list(Offset_splitted_list, summary_list, peak_list), function(Offset_splitted_list,
                                                                         summary,
                                                                         peak_list) {
        map2(Offset_splitted_list, unlist(peak_list), function(Offset_splitted_list, peak, summary) { #        map2(Offset_splitted_list[order(unlist(peak_list))], unlist(peak_list), function(Offset_splitted_list, peak, summary)
          add_column(
            summary, !!paste0("Offset", peak) := as.numeric(Offset_splitted_list)
          )
        }, summary)
      })

    summary_list <- join_splitted_list(splitted_summary_list)

    return(summary_list)
  }


#' Wrapper for openPeakSetup with some preparations
#'
#' @return
#' @export
#'
#' @examples
prepare_peak_setup <- function(IV_names) {
  data_envir$peak_list <- NULL
  data_envir$peak_suggestion <-
    map(
      map_depth(
        IV_names,
        1,
        word,
        start = -1,
        sep = "_"

    ), unique)
  data_envir$column_list <- list()
  openPeakSetup("filler")

  # gtkMain()
}


#' calculates the current densitiy in the summarys via the Cm and user picked columns(openpeakSetup)
#'
#' @param summary_list
#' @param column_list
#'
#' @return
#' @export
#'
#' @examples
calculate_summary_current_density <-
  function(summary_list, column_list) {
    check_list(summary_list)
    check_list(column_list)
    splitted_summary_list <-
      map2(column_list, summary_list, function(column, summary) {
        map(unlist(column), function(col, sum) {
          add_column(sum, !!paste0("CD_", col) := pull(sum, col) / pull(sum, "Cm"))
        }, summary)
      })

    summary_list <- join_splitted_list(splitted_summary_list)

    return(summary_list)
  }


#' calculates the corrected current densitiy in the summarys via the Cm and user picked columns(openpeakSetup) and the offset
#'
#' @param summary_list
#' @param column_list
#' @param peak_list
#'
#' @return
#' @export
#'
#' @examples
calculate_corrected_summary_current_density <-
  function(summary_list, column_list, peak_list) {
    check_list(summary_list)
    check_list(column_list)
    check_list(peak_list)

    splitted_summary_list <-
      pmap(list(column_list, summary_list, peak_list), function(column, summary, peak) {
        map2(unlist(column), rep(unlist(peak), each = 2), function(col, peak, sum) {
          add_column(
            sum,
            !!paste0("CD_Corrected_", col) := (pull(sum, col) - pull(sum, paste0("Offset", peak))) / pull(sum, "Cm")
          )
        }, summary)
      })

    summary_list <- join_splitted_list(splitted_summary_list)

    return(summary_list)
  }

#' calculates the ratios between the different Imin and Imax
#'
#' @param summary_list
#' @param column_list
#' @param peak_list
#'
#' @return
#' @export
#'
#' @examples
calculate_ratio <- function(summary_list, column_list, peak_list) {
  check_list(summary_list)
  check_list(column_list)
  splitted_summary_list <-
    pmap(list(column_list, summary_list, peak_list), function(column, summary, peak) {
      map2(column, unlist(peak), function(col, peak, sum) {
        add_column(sum, !!paste0("Ratio_", peak) := abs(pull(sum, paste0("CD_Corrected_", col[2])) / pull(sum, paste0("CD_Corrected_", col[1]))))
      }, summary)
    })

  summary_list <- join_splitted_list(splitted_summary_list)

  return(summary_list)
}


#' Detects the outliers in the different columns via two sided grubbs
#'
#' @param summary_list
#' @param column_list
#'
#' @return
#' @export
#'
#' @examples
detect_summary_outlier <- function(summary_list, column_list, peak_list) {
  column_list <- map(column_list, function(x) x[!str_detect(x, "Bef")])
  outlier_list <- map2(summary_list, column_list, function(summary, columns) map(unlist(columns), function(column, summary) detect_grubbs_outlier(summary, paste0("CD_Corrected_", column)), summary))
  outlier_list_iterative <- pmap(list(summary_list, column_list, outlier_list), function(summary, columns, outlier) map(unlist(columns), function(column, summary, outlier) double_iterative_grubbs_outlier(summary, paste0("CD_Corrected_", column), outlier), summary, outlier))
  outlier_list_iterative_new <- NA
  while (!identical(outlier_list_iterative, outlier_list_iterative_new)) {
    if (!is.na(outlier_list_iterative_new)) outlier_list_iterative <- outlier_list_iterative_new
    outlier_list_iterative_new <- pmap(list(summary_list, column_list, outlier_list_iterative), function(summary, columns, outlier) map(unlist(columns), function(column, summary, outlier) double_iterative_grubbs_outlier(summary, paste0("CD_Corrected_", column), outlier), summary, outlier))
  }

  outlier_list <- map2(outlier_list_iterative_new, column_list, function(outliers, columns) {
    names(outliers) <- paste0("Outlier_CD_Corrected_", unlist(columns))
    return(outliers)
  })

  peak_list <- map_depth(peak_list, 2, function(x) x[!str_detect(x, "Bef")])

  outlier_list_Ratio <- map2(summary_list, peak_list, function(summary, peaks) map(unlist(peaks), function(peak, summary) detect_grubbs_outlier(summary, paste0("Ratio_", peak)), summary))

  outlier_list_Ratio_iterative <- pmap(list(summary_list, peak_list, outlier_list), function(summary, peaks, outlier) map(unlist(peaks), function(peak, summary, outlier) double_iterative_grubbs_outlier(summary, paste0("Ratio_", peak), outlier), summary, outlier))
  outlier_list_Ratio_iterative_new <- NA
  while (!identical(outlier_list_Ratio_iterative, outlier_list_Ratio_iterative_new)) {
    if (!is.na(outlier_list_Ratio_iterative_new)) outlier_list_Ratio_iterative <- outlier_list_Ratio_iterative_new
    outlier_list_Ratio_iterative_new <- pmap(list(summary_list, peak_list, outlier_list_Ratio_iterative), function(summary, peaks, outlier) map(unlist(peaks), function(peak, summary, outlier) double_iterative_grubbs_outlier(summary, paste0("Ratio_", peak), outlier), summary, outlier))
  }


  outlier_list_Ratio <- map2(outlier_list_Ratio_iterative_new, peak_list, function(outliers, peaks) {
    names(outliers) <- paste0("Outlier_Ratio_", unlist(peaks))
    return(outliers)
  })

  outlier_list <- map(outlier_list, bind_cols)
  outlier_list_Ratio <- map(outlier_list_Ratio, bind_cols)

  outlier_list <- map(outlier_list, function(outliers) add_column(outliers, Outlier_CD = rowSums(outliers) > 0))
  # outlier_list_Ratio <- map(outlier_list_Ratio, function(outliers) add_column(outliers, Outlier_Ratio =  rowSums(outliers) > 0 ))
  outlier_list_Ratio <- map(outlier_list_Ratio, function(outliers) add_column(outliers, Outlier_Ratio = rowSums(outliers) > 0))


  outlier_list <- map2(outlier_list, outlier_list_Ratio, cbind)
  outlier_list <- map(outlier_list, function(outliers) add_column(outliers, Outlier = rowSums(outliers) > 0))

  summary_list <- map2(summary_list, outlier_list, bind_cols)
  return(summary_list)
}


#' Trims the statistic tibble down to a handy useable size
#'
#' @param summary_list
#' @param column_list
#' @param peak_list
#'
#' @return
#' @export
#'
#' @examples
trim_summary_statistic_columns <- function(summary_list, column_list, peak_list) {
  CD_columns <- map(column_list, function(cols) paste0("CD_Corrected_", unlist(cols)))
  ratio_columns <- map(peak_list, function(peaks) paste0("Ratio_", unlist(peaks)))
  column_trim_list <- map2(ratio_columns, CD_columns, function(ratio, CD, gen_cols) c(gen_cols, ratio, CD), settings_envir$summary_statistic_columns)
  summary_list_statistic <- map2(summary_list, column_trim_list, trim_tibble)
  return(summary_list_statistic)
}

#' Returns a summary with or without outliers
#'
#' @param summary
#' @param outliers TRUE = Without Outliers
#'
#' @return
#' @export
#'
#' @examples
deselect_outlier <- function(summary, outlier_column, outliers = TRUE) {
  summary <- summary[summary[[outlier_column]] != outliers, ]
  return(summary)
}

#' exports values from a summary to long format
#'
#' @param summary_list
#' @param column_list
#'
#' @return
#' @export
#'
#' @examples
export_summary_value_long <- function(summary_list, column_list) {
  summary_data <-
    map2(column_list, summary_list, function(columns, sum) {
      map(unlist(columns), function(column, sum) {
        sum[[column]]
      }, sum)
    })
  unlist_peaks <- map(column_list, unlist)
  data_envir$clustering_cols <- unlist_peaks
  summary_data <-
    map2(summary_data, unlist_peaks, function(a, b) {
      names(a) <- b
      a
    })
  names(summary_data) <- names(summary_list)
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
    gather(
      summary_data_df,
      col,
      value,
      colnames(summary_data_df)[-which(colnames(summary_data_df) == "series")],
      na.rm = T,
      factor_key = T
    )

  summary_data_long[["col"]] <-
    paste0(summary_data_long[["series"]], ": ", summary_data_long[["col"]])

  summary_data_long[["col"]] <-
    factor(summary_data_long[["col"]], levels = unique(summary_data_long[["col"]]))

  return(summary_data_long)
}


#' Runs through all process steps with a summary
#'
#' @return
#' @export
#'
#' @examples
process_summary <- function() {
  data_storage_envir$IV_offset <- export_IV_offset(data_storage_envir$IV_list)
  prepare_peak_setup(data_storage_envir$IV_names)
  data_envir$summary_list <- export_offset_to_summary(data_storage_envir$IV_offset, data_storage_envir$summary_list, data_storage_envir$IV_names, data_storage_envir$peak_list)
  data_envir$summary_list <- calculate_summary_current_density(data_envir$summary_list, data_storage_envir$column_list)
  data_envir$summary_list <- calculate_corrected_summary_current_density(data_envir$summary_list, data_storage_envir$column_list, data_storage_envir$peak_list)
  data_envir$summary_list <- calculate_ratio(data_envir$summary_list, data_storage_envir$column_list, data_storage_envir$peak_list)
  data_envir$summary_list <- detect_summary_outlier(data_envir$summary_list, data_storage_envir$column_list, data_storage_envir$peak_list)
  data_envir$summary_statistic_list <- trim_summary_statistic_columns(data_envir$summary_list, data_storage_envir$column_list, data_storage_envir$peak_list)
}

#' Imports a summary and runs all processing steps
#'
#' @return
#' @export
#'
#' @examples
process_new_summary <- function() {
  data_list <- import_summary()
  list2env(data_list, data_envir)
  process_summary()
  data_storage_envir$summary_list <- data_envir$summary_list
  data_storage_envir$summary_statistic_list <- data_envir$summary_statistic_list
}

#' Add a new Summary; Run through all existing process steps; Add new list to the current existing list
#'
#' @return
#' @export
#'
#' @examples
add_new_processed_summary <- function() {
  data_list <- import_summary()
  list2env(data_list, data_envir)
  process_summary()
  data_storage_envir$summary_list <- c(data_storage_envir$summary_list, data_envir$summary_list)
  data_storage_envir$summary_statistic_list <- c(data_storage_envir$summary_list, data_envir$summary_statistic_list)
}

#' Adds a new summary to an existing one; Runs through all existing process steps, merging the new summary list with the existing one
#'
#' @return
#' @export
#'
#' @examples
expand_processed_summary <- function() {
  data_list <- import_summary()
  list2env(data_list, data_envir)
  process_summary()
  data_storage_envir$summary_list <- list_merge(data_storage_envir$summary_list, data_envir$summary_list)
  data_storage_envir$summary_statistic_list <- list_merge(data_storage_envir$summary_list, data_envir$summary_statistic_list)
}

#' Processes an already imported summary
#'
#' @return
#' @export
#'
#' @examples
process_existing_summary <- function() {
  process_summary()
  data_storage_envir$summary_list <- data_envir$summary_list
  data_storage_envir$summary_statistic_list <- data_envir$summary_statistic_list
}
