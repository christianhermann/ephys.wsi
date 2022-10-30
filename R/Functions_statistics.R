#' Wrapper for wilcox.test
#'
#' @param vec1
#' @param vec2
#' @param paired
#'
#' @return p_value of the test
#' @export
#'
#' @examples
U_test_vectors <- function(vec1, vec2, paired = F) {
  if (anyNA(vec1) || anyNA(vec2)) {
    return(NA)
  }
  if (all(unique(vec1 == 0)) & all(unique(vec2 == 0))) {
    return(1)
  }
  if (identical(vec1, vec2)) {
    return(1)
  }
  if (length(vec1) != length(vec2) && paired == TRUE) stop("Sample size needs to be identical for a paired test!")
  wilcox_result <- wilcox.test(vec1, vec2, paired = paired)
  if (paired == T) print(paste0("Paired = ", paired))
  p_value <- wilcox_result[["p.value"]]
  return(p_value)
}

#' Wrapper for U_test_vectors. Iterates through 2 tbl and calculates the p_value for each vector
#'
#' @param paired
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
U_test_tbl <- function(paired, ...) {
  check_tibble(..1)
  check_tibble(..2)
  t_df1 <- data.frame(t(..1))
  t_df2 <- data.frame(t(..2))
  if (paired == F) {
    p_values <- map2_dbl(t_df1, t_df2, U_test_vectors, F)
    print(paste0("Paired = F"))
  } else {
    tryCatch(
      {
        p_values <- map2_dbl(t_df1, t_df2, U_test_vectors, paired)
        print(paste0("Paired = ", paired))
      },
      error = function(e) {
        return("Sample size needs to be identical for a paired test!")
      }
    )
  }
  return(p_values)
}

#' Wrapper for U_test_tbl, uses a list of tbls and iterates pair wise through each
#'
#' @param tbl_list
#'
#' @return
#' @export
#'
#' @examples
U_test_pairwise_tbl <- function(tbl_list) {
  check_list(tbl_list)
  p_values <-
    map(tbl_list, function(x, l) {
      map(l, function(y) {
        U_test_tbl(x, y)
      })
    }, tbl_list)
  return(p_values)
}


#' Wrapper for kruskal.test
#'
#' @param vec_list
#'
#' @return p_value of the test
#' @export
#'
#' @examples
kruskal_test_vectors <- function(vec_list) {
  test_vec <- map_lgl(vec_list, anyNA)
  if (sum(test_vec) > 0) {
    return(NA)
  }
  kruskal_result <- kruskal.test(vec_list)
  p_value <- kruskal_result[["p.value"]]
  return(p_value)
}

#' Wrapper for kruskal_test_vectors
#'
#' @param tbl_list
#'
#' @return
#' @export
#'
#' @examples
kruskal_test_tbl <- function(tbl_list) {
  t_df_list <- map(tbl_list, function(tbl) {
    data.frame(t(tbl))
  })
  p_values <-
    pmap_dbl(t_df_list, function(...) {
      kruskal_test_vectors(list(...))
    })
  return(p_values)
}


#' Wrapper for u_test_tbl, uses two measurements and two columns
#'
#' @param IV_list
#' @param column
#'
#' @return
#' @export
#'
#' @examples
get_p_values_U_test <- function(IV_list, columns) {
  check_list(IV_list)
  IV_tbl <- map2(IV_list, columns, tbl_list_to_column_tbl)
  return(do.call(IV_tbl, U_test_tbl))
}


#' Gets the p_values out of a list of summarys and their corresponding columns; Columns format is the same as the output from open_peak_selection
#'
#' @param summary
#' @param columns
#'
#' @return
#' @export
#'
#' @examples columns <- list(list(c(colMin, colMax)), list(c(colMin1, colMax2)))
get_summary_p_values_U_test <- function(summary, columns, paired = F) {
  columns <- pmap(columns, function(...) {
    return(list(...))
  })
  columns <- map(columns, data.table::transpose)
  p_value_list <-
    map_depth(columns, 2, function(cols, sum, paired) {
      map2(sum, cols, function(s, x, s_o, x_o, paired) {
        U_test_vectors(unlist(s_o[x_o]), unlist(s[x]), paired)
      }, sum[[1]], cols[[1]], paired)
    }, summary, paired)
  p_value_list <- flatten(p_value_list)
  flat_cols <-
    flatten(map_depth(columns, 2, function(columns) {
      paste0(columns, collapse = "")
    }))
  names(p_value_list) <- flat_cols
  return(p_value_list)
}

#' Wrapper function for U_test vecors
#'
#' @param summary_list
#' @param column_vector
#'
#' @return dataframe containing p values
#' @export
#'
#' @examples
get_pairwise_vector_summary_p_values_U_test <-
  function(column_vector, summary_list) {
    p_list <-
      map2(column_vector, summary_list, function(d1_map, sum_map, d1_orig, sum_orig) {
        map2(d1_orig, sum_orig, function(d1_orig_map,
                                         sum_orig_map,
                                         sum_map,
                                         d1_map) {
          U_test_vectors(unlist(sum_map[d1_map]), unlist(sum_orig_map[d1_orig_map]))
        }, sum_map, d1_map)
      }, column_vector, summary_list)
    p_list <- map(p_list, unlist)
    p_matrix <- do.call(rbind, p_list)
    p_names <- paste0(names(summary_list), "_", column_vector)
    rownames(p_matrix) <- p_names
    colnames(p_matrix) <- p_names
    p_df <- data.frame(p_matrix)
    return(p_df)
  }

#' Creates a list of dataframes containing the pvalues of pairwise u tests
#'
#' @param summary_list
#' @param column_list
#'
#' @return
#' @export
#'
#' @examples
get_pairwise_summary_p_values_U_test <-
  function(summary_list, column_list) {
    column_list <- pmap(column_list, function(...) {
      return(list(...))
    })
    column_list <- map(column_list, data.table::transpose)
    p_value_list <-
      map_depth(
        column_list,
        2,
        get_pairwise_vector_summary_p_values_U_test,
        summary_list
      )
    return(p_value_list)
  }
