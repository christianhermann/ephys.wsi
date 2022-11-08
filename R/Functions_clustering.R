####Functions which can be used to cluster data using DIANA and Kmeans Clustering####


#' Prepares data for clustering
#'
#' @param summary_list
#' @param column_list same format as output of peak_addition list("rTRPC4ß1" = list(c("Ratio_EA")), "hTRPC6 - WT" = list(c("Ratio_OAG")))
#' @param IV_list
#' @param IV_cols list of columns list(col, col, col)
#' @param IV_names
#' @param position  list of positions of the used column (-0.06 for -60mV)
#' @param peak_list list of used peaks  list("EA","OAG")
#' @param range
#'
#' @return
#' @export
#'
#' @examples
get_clustering_data <-
  function(summary_list,
           column_list,
           IV_list,
           IV_cols,
           IV_names,
           position,
           peak_list,
           range) {
    if (!is.null(summary_list)) {
      summary_data_long_list <-
        map(map_depth(pmap(flatten(
          map_depth(column_list, 2, as.list)
        ), list), 2, list), function(columns, summary_list) {
          export_summary_value_long(summary_list, columns)
        }, summary_list)
      summary_data_long <- bind_cols(summary_data_long_list)
      summary_data_long[["col"]] <-
        factor(summary_data_long[["col"]], levels = unique(summary_data_long[["col"]]))
    }
    if (!is.null(IV_list)) {
      iv_data_long_list <-
        map2(IV_cols, position, function(col, pos) {
          export_IV_value(IV_list, IV_names, col, pos, peak_list, range)
        })

      iv_data_long <- bind_cols(iv_data_long_list)
      names(iv_data_long)[2] <- "col"
      iv_data_long[["col"]] <-
        factor(iv_data_long[["col"]], levels = unique(iv_data_long[["col"]]))

      if (!is.null(summary_list)) {
        cluster_data <- bind_cols(summary_data_long, iv_data_long)
      }
      if (is.null(summary_list)) {
        cluster_data <- iv_data_long
      }
    }
    if (is.null(IV_list)) {
      cluster_data <- summary_data_long
    }


    return(cluster_data)
  }

#' Wrapper for diana
#'
#' @param cluster_data
#'
#' @return
#' @export
#'
#' @examples
cluster_diana <-
  function(cluster_data,
           metric = "euclidian",
           stop_k = 2) {
    diana_dat <-
      cluster_data[, str_detect(names(cluster_data), "value")]
    rownames(diana_dat) <-
      make.unique(as.character(cluster_data$series))
    diana <-
      diana(diana_dat,
        metric = metric,
        stand = T,
        stop.at.k = stop_k
      )
    return(diana)
  }

#' Wrapper for K-Means
#'
#' @param cluster_data
#' @param k
#'
#' @return
#' @export
#'
#' @examples
cluster_kmeans <- function(cluster_data, k, scale = F) {
  cluster_dat <-
    cluster_data[, str_detect(names(cluster_data), "value")]
  rownames(cluster_dat) <-
    make.unique(as.character(cluster_data$series))
  kmeans <- kmeans(cluster_dat, centers = k, nstart = 25)
  if (scale == T) {
    kmeans <- kmeans(scale(cluster_dat), centers = k, nstart = 25)
  }


  return(kmeans)
}

#' Does DIANA Clustering
#'
#' @param summary_list
#' @param column_list same format as output of peak_addition: list("rTRPC4ß1" = list(c("Ratio_EA")), "hTRPC6 - WT" = list(c("Ratio_OAG")))
#' @param IV_list
#' @param IV_cols list of columns list(col, col, col)
#' @param IV_names
#' @param position  list of positions of the used column (-0.06 for -60mV)
#' @param peak_list list of used peaks  list("EA","OAG")
#'
#' @return
#' @export
#'
#' @examples
do_diana_cluster <- function(summary_list,
                             column_list,
                             IV_list,
                             IV_names,
                             IV_cols,
                             position,
                             peak_list,
                             range) {
  cluster_data <- get_clustering_data(
    summary_list,
    column_list,
    IV_list,
    IV_cols,
    IV_names,
    position,
    peak_list,
    range
  )

  diana <- cluster_diana(cluster_data)

  # ggdendro <- ggdendrogram(diana)
  return(diana)
}

#' Does K-MeanClustering
#'
#' @param summary_list
#' @param column_list same format as output of peak_addition: list("rTRPC4ß1" = list(c("Ratio_EA")), "hTRPC6 - WT" = list(c("Ratio_OAG")))
#' @param IV_list
#' @param IV_names
#' @param IV_cols list of columns list(col, col, col)
#' @param position
#' @param peak_list list of positions of the used column (-0.06 for -0.06V)
#' @param cluster list of used peaks  list("EA","OAG")
#' @param range range for taking the mean of of peaklist
#'
#' @return
#' @export
#'
#' @examples
do_kmean_cluster <- function(summary_list,
                             column_list,
                             IV_list,
                             IV_names,
                             IV_cols,
                             position,
                             peak_list,
                             cluster,
                             range,
                             scale) {
  cluster_data <- get_clustering_data(
    summary_list,
    column_list,
    IV_list,
    IV_cols,
    IV_names,
    position,
    peak_list,
    range
  )

  kmeans <- cluster_kmeans(cluster_data, cluster, scale)
  return(list(kmeans, cluster_data))
}


#' Prepares data and uses factoextra function eclust for elegant clustering
#'
#' @param summary_list
#' @param column_list
#' @param IV_list
#' @param IV_cols
#' @param IV_names
#' @param position
#' @param peak_list
#' @param range
#' @param scale
#' @param clustertype
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
do_clustering <- function(summary_list,
                          column_list,
                          IV_list,
                          IV_names,
                          IV_cols,
                          position,
                          peak_list,
                          range,
                          scale,
                          clustertype,
                          ...) {
  cluster_data <- get_clustering_data(
    summary_list,
    column_list,
    IV_list,
    IV_cols,
    IV_names,
    position,
    peak_list,
    range
  )

  cluster_dat <-
    cluster_data[, str_detect(names(cluster_data), "value")]
  rownames(cluster_dat) <-
    make.unique(as.character(cluster_data$series...1))


  if (clustertype == "3d") {
    cluster_dat$series <- as.factor(sub("\\..*", "", rownames(cluster_dat)))
    # plot <- plot_ly(
    #   x=cluster_dat$value, y=cluster_dat$value1, z=cluster_dat$value2,
    #   color = cluster_dat$series)
    #
    return(list(plot, cluster_dat))
  }


  if (scale == T) cluster_dat <- scale(cluster_dat)

  cluster_fin <- eclust(cluster_dat, clustertype, ...)
  return(list(cluster_fin, cluster_data))
}



#' Combines IV Outlier with Summary Outliers
#'
#' @param outlier_tbl outlier_tbl returned from detect_IV_outlier
#' @param IV_list
#' @param IV_names
#' @param summary_list
#'
#' @return list containing an iv_list without outliers, summary_list with outlier column, list of the outliers
#' @export
#'
#' @examples
get_complete_outlier <-
  function(outlier_tbl,
           IV_list,
           IV_names,
           summary_list,
           outlier) {
    splitted_IV_list <- map2(IV_list, IV_names, split_list_by_pharmacon)

    if (is.null(outlier) == F) {
      summary_outlier <-
        map(summary_list, function(x) {
          return(x[[outlier]])
        })
    } else {
      summary_outlier <-
        map(summary_list, function(x) {
          return(x$Name == T)
        })
    }

    IV_outlier <- map(outlier_tbl, function(x) {
      return(x$is_outlier)
    })

    combined_outlier <-
      map2(summary_outlier, IV_outlier, function(sum, out) {
        return(as.logical(rowSums(cbind(
          sum, out
        ))))
      })

    splitted_IV_list <-
      map2(splitted_IV_list, combined_outlier, function(iv_list, outlier) {
        map(iv_list, function(x) {
          x[!outlier]
        })
      })

    IV_list <- map(splitted_IV_list, flatten)

    IV_names <-
      map2(IV_names, combined_outlier, function(names, outlier) {
        return(names[!outlier])
      })

    summary_list <-
      map2(summary_list, combined_outlier, function(summary_list, Outlier) {
        summary_list[["Outlier"]] <- NULL
        cbind(summary_list, Outlier)
      })

    return(list(
      IV_list = IV_list,
      summary_list = summary_list,
      outlier_vec = combined_outlier
    ))
  }
