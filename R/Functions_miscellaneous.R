#' Wrapper function to check if packages exists and if not to unstall them from the minicran ==> Outdated since checkpoint
#'
#' @param pkg
#'
#' @return
#' @export
#'
#' @examples
check_packages <- function(pkg, libpath) {
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) {
    # install.packages(new.pkg, dependencies = TRUE,repos = "http://cran.us.r-project.org")
    if (dir.exists("U://Installationsprogramme//R//miniCRAN")) {
      install.packages(new.pkg, dependencies = TRUE, repos = paste0("file:///", "U://Installationsprogramme//R//miniCRAN"), lib = libpath)
    }
  }
  if (!dir.exists("U://Installationsprogramme//R//miniCRAN")) {
    print("Choose miniCRAN directory: most likely in U://Installationsprogramme//R//miniCRAN")
    install.packages(new.pkg, dependencies = TRUE, repos = paste0("file:///", choose.dir()), lib = libpath)
  }
  sapply(pkg, require, character.only = TRUE)
}


#' Imports an excel table containing all its sheets
#'
#' @param name name of the list which will be returned
#'
#' @return List containing a list with the sheets and the sheetnames:
#'
#' @export
#'
#' @examples
import_excel_Data <- function(name) {
  dir_summary <-
    gfile(
      "Import Summary",
      type = "open",
      filter = list(
        "Excel sheets" = list(patterns = c("*.xlsx", "*.xls")),
        "All files" = list(patterns = c("*"))
      )
    )
  sheet_names <- getSheetNames(dir_summary)

  excel_data <-
    map(sheet_names, function(sheet) {
      excel_data <- read.xlsx(dir_summary, sheet = sheet)
    })

  excel_data <-
    map(excel_data, function(x) {
      colnames(x) <- make.unique(colnames(x))
      x
    })

  excel_data <- map(excel_data, tibble::as.tibble)

  sheet_names <-
    map_chr(sheet_names, function(sheet) {
      if (str_ends(sheet, " ")) {
        return(stri_replace_last_fixed(sheet, " ", ""))
      }
      return(sheet)
    })
  names(excel_data) <- sheet_names

  return_list <- list()
  return_list[[name]] <- excel_data
  return_list[["sheet_names"]] <- sheet_names
  return(return_list)
}


#' Imports an .asc file
#'
#' @param directory A filepath leading to an .asc file
#' @param skips An integer specifieng the length of the header
#'
#' @return a tibble containing the .asc file
#' @export
#'
#' @examples data <- import_asc_Data("filepath.asc", skips = 5)
import_asc_Data <- function(directory, skips) {
  return(vroom(directory, ",", TRUE, cols(.default = col_double()), skip = skips, .name_repair = make.unique))
}


#' Gets the row length of an .asc file
#'
#' @param directory A filepath leading to an .asc file
#'
#' @return numeric with the row length of the .asc file
#' @export
#'
#' @examples read_asc_Length("filepath.asc")
read_asc_Length <- function(directory) {
  tmp <- read_file(directory)
  tmp <- str_replace(tmp, "\n\r", "")
  return(str_count(tmp, "\n"))
}

#' Reads iterative through an .asc file until a string containing the header_column is found.
#'
#' @param directory A filepath leading to an .asc file
#' @param header_column name of a column in the header
#'
#' @return
#' @export
#'
#' @examples
find_header_row <- function(directory, header_column) {
  i <- 0
  header_row <- NULL
  while (length(header_row) < 1) {
    line <- readLines(directory, i)
    header_row <- grep(header_column, line)
    i <- i + 1
  }
  return(header_row)
}

#' Renames a column
#'
#' @param tbl a tibble
#' @param old_name the name of the column
#' @param new_name the new name of the column
#'
#' @return the tibble with the renamed column
#' @export
#'
#' @examples tbl <- tibble("Test" = 3, "Random" = 2)
#' rename_column(tbl, "Test", "NewName")
rename_column <- function(tbl, old_name, new_name) {
  if (check_column_exist(tbl, old_name)) {
    return(rename(tbl, !!new_name := old_name))
  }
  return(tbl)
}


#' checks if a column exists
#'
#' @param tbl a tibble
#' @param name the name of the column to be checked
#'
#' @return TRUE or FALSE
#' @export
#'
#' @examples
check_column_exist <- function(tbl, name) {
  return(name %in% names(tbl))
}

#' takes a subset of a tibble
#'
#' @param tbl a tibble
#' @param columns vector containing all columns which shall be subsettet
#'
#' @return the subsetted tibble
#' @export
#'
#' @examples
trim_tibble <- function(tbl, columns) {
  return(tbl %>% select(columns))
}


#' Sorts a given Input
#'
#' @param data a list, vector or tibble
#' @param column the name of the colum used to sort
#'
#' @return
#' @export
#'
#' @examples
order_data <- function(data, column = NULL) {
  switch(class(data)[1],
    tbl_df = {
      data <- arrange(data, pull(data[column]))
    },
    character = {
      data <- str_sort(data)
    },
    list = {
      data <- data[order(names(data))]
    }
  )
  return(data)
}

#' Creates a unique ID out if the measurements name
#'
#' @param tbl a tibble
#' @param column the column with the names
#'
#' @return a vector contaning the ids
#' @export
#'
#' @examples
create_id <- function(tbl, column) {
  return(tbl %>% mutate(ID = word(pull(tbl, column), -1, sep = "_")))
}


#' Trims a Measurment from a specified Potential to a specified potential
#'
#' @param tbl
#' @param from
#' @param to
#'
#' @return
#' @export
#'
#' @examples trim_measurement(tbl, -0.99, 0.99)
trim_measurement <- function(tbl, from, to) {
  tbl <-
    tbl[c((which.min(tbl["Potential[V]"] == from) - 1):which.max(tbl["Potential[V]"] == to)), ]
  return(tbl)
}



#' Trims a Measurment from a specified column between two values
#'
#' @param tbl
#' @param from
#' @param to
#' @param name
#'
#' @return
#' @export
#'
#' @examples
trim_measurement_which <- function(tbl, from, to, name) {
  tbl <- tbl[c((which(tbl[name] == from)):which(tbl[name] == to)), ]
  return(tbl)
}

#' Checks if the wanted column is in an IV. If not a GUI will be opened where the User can select the column
#'
#' @param tbl a tibble containing the IV
#' @param iv_name the name of the IV
#' @param search_name the name of the column
#'
#' @return a tbl which contains the renamed columns
#' @export
#'
#' @examples
select_column_manual <- function(tbl, iv_name, search_name) {
  if (!check_column_exist(tbl, search_name)) {
    openColumnCorrection(window, search_name, iv_name, colnames(tbl))
    # gtkMain()

    colnames(tbl)[which(colnames(tbl) == paste0(gui_interaction_envir$correctColumn))] <-
      search_name
    return(tbl)
  }
  return(tbl)
}


#' normalizes a column of a tibble
#'
#' @param tbl
#' @param name the name of the column which will be used to select the data (Inward and Outward Current)
#' @param norm_name the name of the column which will be normalized
#' @param from
#' @param to
#'
#' @return
#' @export
#'
#' @examples
normalize_data <- function(tbl, name, norm_name, from, to, scale) {
  tbl <- tbl[c((which(tbl[name] == from)):which(tbl[name] == to)), ]
  norm_data <-
    ((pull(tbl[norm_name]) - first(pull(tbl[norm_name]))) / (last(pull(tbl[norm_name])) - first(pull(tbl[norm_name])))) * scale
  return(norm_data)
}


#' normalizes a given vector
#'
#' @param vec
#'
#' @return
#' @export
#'
#' @examples
normalize_vector_data <- function(vec) {
  return(norm_vec = (vec - min(vec)) / (max(vec) - min(vec)))
}


compare_normed_data <- function(normed_data, original_data) {
  diff_norm_data <- diff(normed_data)
  diff_original_data <- diff(original_data)
}


get_model_RMSE <- function(model) {
  original <- model$y
  fit <- model$fitted.values
  return(sqrt(mean((fit - original)^2)))
}
check_model <- function(model) {
  return(rsq(model))
}

create_model_rsq_mean <- function(rsq_list) {
  rsq_list <- flatten(rsq_list)
  rsq_vector <- flatten_dbl(rsq_list)
  return(mean(rsq_vector))
}

#' A wrapper for create glm model which uses a tibble and two column names to fit a model
#'
#' @param tbl
#' @param x_name
#' @param y_name
#' @param poly
#'
#' @return
#' @export
#'
#' @examples
create_glm_mode_tbl <- function(tbl, x_name, y_name, poly) {
  fit <- create_glm_model(pull(tbl, y_name), pull(tbl, x_name), poly)
  return(as.numeric(fit$fitted.values))
}

#' Wrapper for GLM
#'
#' @param v_DataY
#' @param v_DataX
#' @param poly
#'
#' @return
#' @export
#'
#' @examples
create_glm_model <- function(v_DataY, v_DataX, poly) {
  model <-
    glm(v_DataY ~ poly(v_DataX, poly),
      family = gaussian
    )
  return(model)
}


#' Wrapper for GAM
#'
#' @param v_DataY
#' @param v_DataX
#' @param k
#'
#' @return
#' @export
#'
#' @examples
create_gam_model <- function(v_DataY, v_DataX, k) {
  model <-
    gam(v_DataY ~ s(v_DataX, bs = "cr", k = k),
      gamma = 1,
      method = "REML"
    )
  return(model)
}

#' A wrapper for create glm model which uses a tibble and two column names to fit a model
#'
#' @param tbl
#' @param x_name
#' @param y_name
#' @param k
#'
#' @return
#' @export
#'
#' @examples
create_gam_model_tbl <- function(tbl, x_name, y_name, k) {
  check_tibble(tbl)
  if (!x_name %in% colnames(tbl)) {
    stop(paste0(x_name, "not found in tibble"))
  }
  if (!y_name %in% colnames(tbl)) {
    stop(paste0(x_name, "not found in tibble"))
  }
  check_number(k)
  fit <- create_gam_model(pull(tbl, y_name), pull(tbl, x_name), k)
  return(as.numeric(fit$fitted.values))
}


gam_model_test <- function(tbl, k_start, k_fin) {
  means <- c()
  RMSEs <- c()
  for (i in k_start:k_fin) {
    model <-
      create_gam_model(tbl$normalized_CurrentDensity, tbl$"Potential[V]", i)
    #   gam.check(model)
    rsq_m <- check_model(model)
    # rsq_ <- create_model_rsq_mean(rsq_m)
    RMSE <- get_model_RMSE(model)
    means <- c(means, rsq_m)
    RMSEs <- c(RMSEs, RMSE)
    # png(paste0("mC6_Bef_Outward", i, ".png"))
    plot(
      tbl$"Potential[V]",
      tbl$normalized_CurrentDensity,
      type = "l",
      main = paste0(
        "Degree: ",
        i,
        ": R² =",
        round(rsq_m, 6),
        " RMSE  = ",
        round(RMSE, 6)
      ),
      ylab = "normalized Current Density",
      xlab = "Potential [V]",
      lwd = 2,
      cex.axis = 1.5,
      cex.main = 1.5,
      cex.lab = 1.5
    )
    lines(
      tbl$"Potential[V]",
      model$fitted.values,
      type = "l",
      col = "green",
      lwd = 2
    )
    legend("topleft", c("normed IV", "model"), fill = c("black", "green"))
    # dev.off()
  }
  return(c(means, RMSEs))
}





glm_model_test <- function(tbl, pstart, pfinish) {
  means <- c()
  RMSEs <- c()
  for (i in pstart:pfinish) {
    model <-
      create_glm_model(tbl$normalized_CurrentDensity, tbl$"Potential[V]", i)
    rsq_m <- check_model(model)
    # rsq_ <- create_model_rsq_mean(rsq_m)
    RMSE <- get_model_RMSE(model)
    means <- c(means, rsq_m)
    RMSEs <- c(RMSEs, RMSE)
    png(paste0("mC6_Bef_Outward", i, ".png"))
    plot(
      tbl$"Potential[V]",
      tbl$normalized_CurrentDensity,
      type = "l",
      main = paste0(
        "Degree: ",
        i,
        ": R² =",
        round(rsq_m, 6),
        " RMSE  = ",
        round(RMSE, 6)
      ),
      ylab = "normalized Current Density",
      xlab = "Potential [V]",
      lwd = 2,
      cex.axis = 1.5,
      cex.main = 1.5,
      cex.lab = 1.5
    )
    lines(
      tbl$"Potential[V]",
      model$fitted.values,
      type = "l",
      col = "green",
      lwd = 2
    )
    legend("topleft", c("normed IV", "model"), fill = c("black", "green"))
    dev.off()
  }
  return(c(means, RMSEs))
}

glm_check_plots <- function(IV_list, k_start, k_fin) {
  vals <- map_depth(IV_list, 1, gam_model_test, k_start, k_fin)
  flatVals <- flatten(vals)
  bindVals <- bind_rows(flatVals)
  means <- rowMeans(bindVals)
  plot(
    means[5:8],
    type = "b",
    lty = 1,
    ylab = "RMSE",
    xlab = "Degree",
    main = paste0(
      "Vergleich: RMSE GL Model; ",
      ncol(bindVals),
      " Messungen TRPC5/6"
    ),
    cex = 1,
    lwd = 2,
    cex.axis = 1.5,
    cex.main = 1.5,
    cex.lab = 1.5
  )
  plot(
    means[1:4],
    type = "b",
    lty = 1,
    ylab = "R²",
    xlab = "Degree",
    main = paste0("Vergleich: R² GL Model; ", ncol(bindVals), " Messungen TRPC5/6"),
    cex = 1,
    lwd = 2,
    cex.axis = 1.5,
    cex.main = 1.5,
    cex.lab = 1.5
  )
}


rsq_ <- function(x, y) {
  cor(x, y)^2
}

rmse <- function(fit, original) {
  sqrt(mean((fit - original)^2))
}

#' Wrapper for smooth.spline
#'
#' @param v_DataY
#' @param v_DataX
#' @param spar
#'
#' @return
#' @export
#'
#' @examples
fit_smoothing_spline <- function(v_DataY, v_DataX, spar) {
  fit <-
    smooth.spline(v_DataX, v_DataY, spar = spar)
  return(fit)
}

#' fits a weighted smoothing spline
#'
#' @param v_DataY
#' @param v_DataX
#' @param weights
#' @param spar
#'
#' @return
#' @export
#'
#' @examples
fit_weighted_smoothing_spline <- function(v_DataY, v_DataX, weights, spar) {
  fit <-
    smooth.spline(v_DataX, v_DataY, w = weights, spar = spar)
  return(fit)
}

fit_splitted_smoothing_spline <- function(v_DataY, v_DataX, splitX, spar) {
  fit_inward <-
    smooth.spline(v_DataX[1:splitX], v_DataY[1:splitX], spar = spar)
  fit_outward <-
    smooth.spline(v_DataX[splitX:length(v_DataX)], v_DataY[splitX:length(v_DataX)], spar = spar)

  return(list(inward = fit_inward, outward = fit_outward))
}

#' wrapper for cobs
#'
#' @param v_DataY
#' @param v_DataX
#' @param reversePot
#'
#' @return
#' @export
#'
#' @examples
fit_cobs <- function(v_DataY, v_DataX, reversePot) {
  fit <-
    cobs(v_DataX, v_DataY, nknots = 40, lambda = 0.1, pointwise = rbind(c(0, reversePot, 0)))
}

#' Wrapper for fit_smoothing_spline
#'
#' @param tbl
#' @param x_name
#' @param y_name
#' @param spar
#'
#' @return
#' @export
#'
#' @examples
fit_smoothing_spline_tbl <- function(tbl, x_name, y_name, spar, weighted, reversePot, splitFit = F, splitPotential = 0, newColName) {
  check_tibble(tbl)

  if (!x_name %in% colnames(tbl)) {
    stop(paste0(x_name, "not found in tibble"))
  }
  if (!y_name %in% colnames(tbl)) {
    stop(paste0(x_name, "not found in tibble"))
  }
  check_number(spar)
  if (weighted == FALSE) {
    fit <-
      fit_smoothing_spline(pull(tbl, y_name), pull(tbl, x_name), spar)
    fitCont <- fit
    fit <- as.numeric(fit$y)
  }

  if (weighted == TRUE && splitFit == FALSE) {
    weights <- rep(0.1, length(pull(tbl, x_name)))
    weights[which(pull(tbl, "Potential[V]") == reversePot)] <- 1000
    fit <- fit_weighted_smoothing_spline(pull(tbl, y_name), pull(tbl, x_name), weights, spar)
    return(as.numeric(fit$y))
  }
  if (weighted == FALSE && splitFit == TRUE) {
    splitX <- which(pull(tbl, "Potential[V]") == splitPotential)
    wfit <-
      fit_splitted_smoothing_spline(pull(tbl, y_name), pull(tbl, x_name), splitX, spar)
    wfit$inward$y[(splitX + 1):length(pull(tbl, "Potential[V]"))] <- NA
    wfit$outward$y <- c(rep(NA, splitX - 1), wfit$outward$y)

    fit <- data.frame(fitted_normalized_CurrentDensity = as.numeric(fitCont$y), fitted_normalized_CurrentDensity_Inward = wfit$inward$y, fitted_normalized_CurrentDensity_Outward = wfit$outward$y)
    colnames(fit) <- c(newColName, paste(newColName, c("Inward", "Outward"), sep = "_"))
  }

  return(fit)
}

#' Wrapper for fit cobs
#'
#' @param tbl
#' @param x_name
#' @param y_name
#' @param reversePot
#'
#' @return
#' @export
#'
#' @examples
fit_cobs_smoothing_spline_tbl <- function(tbl, x_name, y_name, reversePot) {
  check_tibble(tbl)
  if (!x_name %in% colnames(tbl)) {
    stop(paste0(x_name, "not found in tibble"))
  }
  if (!y_name %in% colnames(tbl)) {
    stop(paste0(x_name, "not found in tibble"))
  }
  check_number(reversePot)
  fit <-
    fit_cobs(pull(tbl, y_name), pull(tbl, x_name), pull(tbl, x_name)[which(pull(tbl, "Potential[V]") == reversePot)])

  fit <- smooth.spline(v_DataX, data, weights, spar = 0.4)$y
  return(as.numeric(fit$fitted))
}

#
# smoothing_spline_fit_test <-
#   function(tbl, spar_start, spar_finish, spar_step) {
#     means <- c()
#     RMSEs <- c()
#     ftests <- c()
#     for (i in (seq(spar_start, spar_finish, spar_step))) {
#       fit <-
#         fit_smoothing_spline(tbl$normalized_CurrentDensity, tbl$"Potential[V]", i)
#       # fit <-
#       #   fit_smoothing_spline(fit$y, fit$x, 0.7)
#       rsq__m <- rsq_(fit$y, tbl$normalized_CurrentDensity)
#       # rsq__ <- create_model_rsq__mean(rsq__m)
#       RMSE <- rmse(fit$y, tbl$normalized_CurrentDensity)
#       means <- c(means, rsq__m)
#       RMSEs <- c(RMSEs, RMSE)
#       ftest <-
#         as.numeric(var.test(fit$y, tbl$normalized_CurrentDensity)[[1]])
#       ftests <- c(ftests, ftest)
#       #   png(paste0("mC6_Bef_Outward", i, ".png"))
#       plot(
#         tbl$"Potential[V]",
#         tbl$normalized_CurrentDensity,
#         type = "l",
#         main = paste0(
#           "Degree: ",
#           i,
#           ": R² =",
#           round(rsq__m, 6),
#           " RMSE  = ",
#           round(RMSE, 6)
#         ),
#         ylab = "normalized Current Density",
#         xlab = "Potential [V]",
#         lwd = 2,
#         cex.axis = 1.5,
#         cex.main = 1.5,
#         cex.lab = 1.5
#       )
#       lines(
#         tbl$"Potential[V]",
#         fit$y,
#         type = "l",
#         col = "green",
#         lwd = 2
#       )
#       legend("topleft", c("normed IV", "model"), fill = c("black", "green"))
#       #    dev.off()
#     }
#     return(c(means, RMSEs, ftests))
#   }
#
#
# smoothing_spline_check_plots <-
#   function(IV_list, spar_start, spar_fin, spar_step) {
#     vals <-
#       map_depth(
#         IV_list,
#         2,
#         smoothing_spline_fit_test,
#         spar_start,
#         spar_fin,
#         spar_step
#       )
#     flatVals <- flatten(vals)
#     bindVals <- bind_rows(flatVals)
#     means <- rowMeans(bindVals)
#
#     plot(
#       seq(spar_start, spar_fin, spar_step),
#       means[23:33],
#       type = "b",
#       lty = 1,
#       ylab = "P ftest",
#       xlab = "spar",
#       main = paste0(
#         "Vergleich: FTest smoothing-spline; ",
#         ncol(bindVals),
#         " Messungen TRPC 4/5/6"
#       ),
#       cex = 1,
#       lwd = 2,
#       cex.axis = 1.5,
#       cex.main = 1.5,
#       cex.lab = 1.5
#     )
#     plot(
#       seq(spar_start, spar_fin, spar_step),
#       means[12:22],
#       type = "b",
#       lty = 1,
#       ylab = "RMSE",
#       xlab = "spar",
#       main = paste0(
#         "Vergleich: RMSE smoothing-spline; ",
#         ncol(bindVals),
#         " Messungen TRPC 4/5/6"
#       ),
#       cex = 1,
#       lwd = 2,
#       cex.axis = 1.5,
#       cex.main = 1.5,
#       cex.lab = 1.5
#     )
#     plot(
#       seq(spar_start, spar_fin, spar_step),
#       means[1:11],
#       type = "b",
#       lty = 1,
#       ylab = "R²",
#       xlab = "spar",
#       main = paste0(
#         "Vergleich: R² smoothing-spline; ",
#         ncol(bindVals),
#         " Messungen TRPC5/6"
#       ),
#       cex = 1,
#       lwd = 2,
#       cex.axis = 1.5,
#       cex.main = 1.5,
#       cex.lab = 1.5
#     )
#   }
#
#
#
#
#
#
# fit_supsmu <- function(v_DataY, v_DataX, span) {
#   fit <-
#     supsmu(v_DataX, v_DataY, span = span)
#   return(fit)
# }
#
#
# fit_supsmu_tbl <- function(tbl, x_name, y_name, span) {
#   check_tibble(tbl)
#   if (!x_name %in% colnames(tbl)) {
#     stop(paste0(x_name, "not found in tibble"))
#   }
#   if (!y_name %in% colnames(tbl)) {
#     stop(paste0(x_name, "not found in tibble"))
#   }
#   check_number(span)
#   fit <- supsmu(pull(tbl, y_name), pull(tbl, x_name), span)
#   return(as.numeric(fit$y))
# }
#
#
#
# supsmu_fit_test <-
#   function(tbl, span_start, span_finish, span_step) {
#     means <- c()
#     RMSEs <- c()
#     ftests <- c()
#     for (i in (seq(span_start, span_finish, span_step))) {
#       fit <-
#         fit_supsmu(tbl$normalized_CurrentDensity, tbl$"Potential[V]", i)
#       rsq__m <- rsq_(fit$y, tbl$normalized_CurrentDensity)
#       # rsq__ <- create_model_rsq__mean(rsq__m)
#       RMSE <- rmse(fit$y, tbl$normalized_CurrentDensity)
#       means <- c(means, rsq__m)
#       RMSEs <- c(RMSEs, RMSE)
#       ftest <-
#         as.numeric(var.test(fit$y, tbl$normalized_CurrentDensity)[3])
#       ftests <- c(ftests, ftest)
#       #   png(paste0("mC6_Bef_Outward", i, ".png"))
#       plot(
#         tbl$"Potential[V]",
#         tbl$normalized_CurrentDensity,
#         type = "l",
#         main = paste0(
#           "Degree: ",
#           i,
#           ": R² =",
#           round(rsq__m, 6),
#           " RMSE  = ",
#           round(RMSE, 6)
#         ),
#         ylab = "normalized Current Density",
#         xlab = "Potential [V]",
#         lwd = 2,
#         cex.axis = 1.5,
#         cex.main = 1.5,
#         cex.lab = 1.5
#       )
#       lines(
#         tbl$"Potential[V]",
#         fit$y,
#         type = "l",
#         col = "green",
#         lwd = 2
#       )
#       legend("topleft", c("normed IV", "model"), fill = c("black", "green"))
#       # dev.off()
#     }
#     return(c(means, RMSEs, ftests))
#   }
#
#
# supsmu_check_plots <-
#   function(IV_list, spar_start, spar_fin, spar_step) {
#     vals <-
#       map_depth(IV_list, 1, supsmu_fit_test, spar_start, spar_fin, spar_step)
#     flatVals <- flatten(vals)
#     bindVals <- bind_rows(flatVals)
#     means <- rowMeans(bindVals)
#
#     plot(
#       seq(spar_start, spar_fin, spar_step),
#       means[43:63],
#       type = "b",
#       lty = 1,
#       ylab = "P ftest",
#       xlab = "spar",
#       main = paste0(
#         "Vergleich: FTest smoothing-spline; ",
#         ncol(bindVals),
#         " Messungen TRPC 4/5/6"
#       ),
#       cex = 1,
#       lwd = 2,
#       cex.axis = 1.5,
#       cex.main = 1.5,
#       cex.lab = 1.5
#     )
#     plot(
#       seq(spar_start, spar_fin, spar_step),
#       means[22:42],
#       type = "b",
#       lty = 1,
#       ylab = "RMSE",
#       xlab = "spar",
#       main = paste0(
#         "Vergleich: RMSE smoothing-spline; ",
#         ncol(bindVals),
#         " Messungen TRPC 4/5/6"
#       ),
#       cex = 1,
#       lwd = 2,
#       cex.axis = 1.5,
#       cex.main = 1.5,
#       cex.lab = 1.5
#     )
#     plot(
#       seq(spar_start, spar_fin, spar_step),
#       means[1:21],
#       type = "b",
#       lty = 1,
#       ylab = "R²",
#       xlab = "spar",
#       main = paste0(
#         "Vergleich: R² smoothing-spline; ",
#         ncol(bindVals),
#         " Messungen TRPC5/6"
#       ),
#       cex = 1,
#       lwd = 2,
#       cex.axis = 1.5,
#       cex.main = 1.5,
#       cex.lab = 1.5
#     )
#   }
#


#' Calculates the slope of points given in two vectors (x&y)
#' The mean of the surrounding two slopes will be used as slope
#'
#' @param v_DataX
#' @param v_DataY
#'
#' @return
#' @export
#'
#' @examples
calculate_slope <- function(v_DataX, v_DataY) {
  check_numeric(v_DataX)
  check_numeric(v_DataY)
  slope <- diff(v_DataX, lag = 2) / diff(v_DataY, lag = 2)
  slope <- c(NA, slope, NA)
  return(slope)
}

#' Wrapper for calculate_slope() which is intended for the use with tibbles
#'
#' @param tbl
#' @param x_name
#' @param y_name
#'
#' @return
#' @export
#'
#' @examples
calculate_slope_tibble <- function(tbl, x_name, y_name) {
  checkTibble(tbl)
  slope <- calculate_slope(pull(tbl, x_name), pull(tbl, y_name))
  return(slope)
}


#' Saves a tbl as .asc file
#'
#' @param tbl
#' @param directory
#'
#' @return
#' @export
#'
#' @examples
save_IV_ASC <- function(tbl, IV_location) {
  check_path_for_output(IV_location)
  write_csv(tbl, path = IV_location)
}

#' Checks if a folder in a given directory exists. If not the folder is created
#'
#' @param directory
#' @param folder
#'
#' @return
#' @export
#'
#' @examples
check_and_create_folder <- function(directory, folder) {
  ifelse(!dir.exists(file.path(paste(
    directory, folder,
    sep = "/"
  ))), dir.create(file.path(paste(
    directory, folder,
    sep = "/"
  ))), FALSE)
}


#' Extracts a column out of tibbles in a list and binds them in a tibble
#'
#' @param tbl_list
#' @param column
#'
#' @return
#' @export
#'
#' @examples
tbl_list_to_column_tbl <- function(tbl_list, column) {
  check_list(tbl_list)
  check_string(column)
  tbl_list <- map(tbl_list, function(x) {
    pull(x[column])
  })
  column_tbl <- bind_cols(tbl_list)
  return(column_tbl)
}

#' Filters the columns enden with the filter_name out of a tibble
#'
#' @param tbl
#' @param filter_name
#'
#' @return
#' @export
#'
#' @examples
filter_tbl_columns <- function(tbl, filter_name) {
  check_tibble(tbl)
  check_string(filter_name)
  tbl_select <- select(tbl, ends_with(filter_name))
  return(tbl_select)
}


#' Extracts the last word seperated by a "_"
#'
#' @param names_list
#'
#' @return
#' @export
#'
#' @examples
create_IV_pharmacon_list <- function(names_list) {
  check_list(names_list)
  return(word(names_list, -1, sep = "_"))
}


#' Splits a list containing IVs by the different endings in new lists
#'
#' @param data_list
#' @param names_list
#'
#' @return
#' @export
#'
#' @examples list(check_OAG, check_EA) -> list(list(check_OAG), list(check_EA))
split_list_by_pharmacon <- function(data_list, names_list) {
  check_list(data_list)
  check_list(names_list)
  IV_pharmacons <- create_IV_pharmacon_list(names_list)

  unique_IV_pharmacons <- unique(IV_pharmacons)
  splitted_list <-
    map(unique_IV_pharmacons, function(u_IV_pharmacon,
                                       IV_pharmacons,
                                       data_list) {
      data_list[which(IV_pharmacons == u_IV_pharmacon)]
    }, IV_pharmacons, data_list)
  names(splitted_list) <- unique_IV_pharmacons
  return(splitted_list)
}

#' Joins a splitted list back together through a left join
#'
#' @param splitted_list
#'
#' @return
#' @export
#'
#' @examples
join_splitted_list <- function(splitted_list) {
  check_list(splitted_list)
  return(map(splitted_list, reduce, left_join))
}


#' Tests a vector for outliers through a two.sided grubbs test. flags index with TRUE if an outlier is found
#'
#' @param vec
#' @param outlier_vec
#'
#' @return
#' @export
#'
#' @examples
grubbs.flag <- function(vec, outlier_vec = NA) {
  check_numeric(vec)
  if (anyNA(vec)) {
    return(tibble(vec = vec, Outlier = NA))
  }
  outliers <- NULL
  test <- vec
  if (!any(is.na(outlier_vec))) {
    test <- vec[!outlier_vec]
    outliers <- vec[outlier_vec]
  }

  if (length(test) < 3) {
    warning("Grubb's test requires > 2 input values")
    return(tibble(vec = vec, Outlier = outlier_vec))
  }

  grubbs.result <- grubbs.test(test, two.sided = T)
  pv <- grubbs.result$p.value

  while (pv < 0.05) {
    outliers <- c(outliers, outlier(test))
    test <- vec[!vec %in% outliers]
    if (length(test) < 3) {
      warning("All but two values flagged as outliers")
      break
    }
    grubbs.result <- grubbs.test(test, two.sided = T)
    pv <- grubbs.result$p.value
  }
  return(tibble(vec = vec, Outlier = (vec %in% outliers)))
}

#' Wrapper for grubbs.flag
#'
#' @param tbl
#' @param column_name
#'
#' @return
#' @export
#'
#' @examples
detect_grubbs_outlier <- function(tbl, column_name) {
  check_tibble(tbl)
  check_string(column_name)
  vec_column <- pull(tbl[column_name])
  outlier_tbl <- grubbs.flag(vec_column)
  return(pull(outlier_tbl, "Outlier"))
}

#' Checks for outliers using a double iterative grubbs test
#'
#' @param tbl
#' @param column_name
#' @param outlier_list
#'
#' @return
#' @export
#'
#' @examples
double_iterative_grubbs_outlier <- function(tbl, column_name, outlier_list) {
  check_tibble(tbl)
  check_string(column_name)
  column_name <- unlist(column_name)
  outlier_df <- data.frame(t(matrix(unlist(outlier_list), nrow = length(outlier_list), byrow = T)))
  colnames(outlier_df) <- column_name
  outlier <- rowSums(outlier_df) >= 1
  vec_column <- pull(tbl[column_name])
  # if(length(which(outlier == FALSE)) < 3) return(unlist(outlier_list))
  outlier_tbl <- grubbs.flag(vec_column, outlier)
  return(pull(outlier_tbl, "Outlier"))
}



#' Wrapper for grubbs.flag
#'
#' @param vec
#'
#' @return
#' @export
#'
#' @examples
grubbs.flag_results <- function(vec) {
  outlier_tbl <- grubbs.flag(vec)
  return(pull(outlier_tbl, "Outlier"))
}


#' Saves a list containing tibbles as an xlsx file
#'
#' @param summary_list
#' @param sheet_names
#'
#' @return
#' @export
#'
#' @examples
save_tbl_list_as_excel <- function(summary_list, sheet_names) {
  check_list(summary_list)
  check_character(sheet_names)
  directory <- gfile("Save Directory", type = "save")
  wb <- createWorkbook()
  map2(summary_list, sheet_names, function(summary, sheet, directory) {
    sheet <- addWorksheet(wb, sheet)
    writeData(wb, sheet, summary)
    saveWorkbook(wb, file.path(paste0(directory, ".xlsx")), overwrite = TRUE)
  }, directory)
}


#' Filters a list
#'
#' @param IV_list
#' @param filter
#'
#' @return
#' @export
#'
#' @examples
filter_IV_list <- function(IV_list, filter) {
  IV_list[str_detect(word(names(IV_list), -1, sep = "_"), "filter")]
}


#' Add invisible Subscript and Superscript ° for alignment
#'
#' @param label
#'
#' @return
#' @export
#'
#' @examples
add_phantom_supersubscript <- function(label) {
  paste0("$\\phantom{^|} $", label, "$\\phantom{_|}$")
}
