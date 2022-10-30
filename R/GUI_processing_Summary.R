#' User Input for the Peak Setup:
#' peak_list and column_list will be created by user input
#'
#' @param win
#'
#' @return
#' @export
#'
#' @examples
openPeakSetup <- function(win) {
  gui_envir$peak_setup <-
    gwindow(
      "Peak-Setup",
      visible = T,
      width = 350,
      height = 30,
      parent = win
    )

  overGroup <-
    ggroup(container = gui_envir$peak_setup, horizontal = F)
  main_group <- ggroup(container = overGroup, horizontal = T)
  gui_envir$peak_choose_box <-
    gcombobox(data_storage_envir$sheet_names,
      container = main_group,
      expand = T
    )

  buttonAdd <-
    gbutton(
      "Add Peak",
      container = main_group,
      expand = T,
      handler = function(h, ...) {
        peak_addition()
      }
    )
  buttonSelectColumns <-
    gbutton(
      "Select Columns",
      container = main_group,
      expand = T,
      handler = function(h, ...) {
        open_peak_selection()
      }
    )

  buttonRepeat <-
    gbutton(
      "Repeat for all",
      container = overGroup,
      handler = function(h, ...) {
        data_envir$peak_list <- map(data_storage_envir$sheet_names, function(name, peak_list) {
          data_envir$peak_list[[name]] <-
            peak_list
        }, peak_list = flatten(data_envir$peak_list))
        names(data_envir$peak_list) <- data_storage_envir$sheet_names

        data_envir$column_list <- map(data_storage_envir$sheet_names, function(name, column_list) {
          data_envir$column_list[[name]] <-
            column_list
        }, column_list = flatten(data_envir$column_list))
        names(data_envir$column_list) <- data_storage_envir$sheet_names
      }
    )

  buttonDone <-
    gbutton(
      "Done",
      container = overGroup,
      handler = function(h, ...) {
        data_storage_envir$peak_list <- data_envir$peak_list
        data_storage_envir$column_list <- data_envir$column_list
        gui_envir$waitForAction <- FALSE
        dispose(gui_envir$peak_setup)
      }
    )

  addHandlerUnrealize(
    gui_envir$peak_setup,
    handler = function(h, ...) {
      val <- gconfirm("Really close window", parent = h$obj)
      if (as.logical(val)) {
        #  gtkMainQuit()
        dispose(gui_envir$peak_setup)
      } # destroy
      else {
        return(TRUE)
      } # don't destroy
    }
  )
  tkwait.window(gui_envir$peak_setup$widget)
}


#' User input to select the corresponding columns to each peak
#'
#' @return
#' @export
#'
#' @examples
open_peak_selection <- function() {
  gui_envir$peak_selection <-
    gwindow(
      paste0("Peak Column Selection: ", svalue(gui_envir$peak_choose_box)),
      visible = T,
      width = 405,
      height = 80
    )
  overGroup <-
    ggroup(cont = gui_envir$peak_selection, horizontal = F, expand = T)

  groupList <- list()
  frameList <- list()
  minList <- list()
  maxList <- list()
  minCbList <- list()
  maxCbList <- list()
  column_list <- list()
  i <- 1

  for (x in unlist(data_envir$peak_list[[svalue(gui_envir$peak_choose_box)]])) {
    print(x)
    groupList <- c(groupList, ggroup(cont = overGroup, expand = T))
    frameList <-
      c(frameList, gframe(x, container = groupList[[i]], expand = T))

    minList <- c(minList, glabel("Min:", cont = frameList[[i]]))
    minCbList <-
      c(
        minCbList,
        gcombobox(
          colnames(data_storage_envir$summary_list[[svalue(gui_envir$peak_choose_box)]]),
          selected = 1,
          expand = T,
          cont = frameList[[i]]
        )
      )
    maxList <- c(maxList, glabel("Max:", cont = frameList[[i]]))
    maxCbList <-
      c(
        maxCbList,
        gcombobox(
          colnames(data_storage_envir$summary_list[[svalue(gui_envir$peak_choose_box)]]),
          selected = 1,
          expand = T,
          cont = frameList[[i]]
        )
      )
    print(x)
    column_list <- c(column_list, x)
    # print(columnList)
    # names(columnList)[[i]] <- x
    i <- i + 1
  }
  buttonDone <-
    gbutton(
      "Done",
      container = overGroup,
      handler = function(h, ...) {
        for (x in 1:length(unlist(data_envir$peak_list[[svalue(gui_envir$peak_choose_box)]]))) {
          column_list[[x]] <-
            c(svalue(minCbList[[x]]), svalue(maxCbList[[x]]))
        }
        if (svalue(gui_envir$peak_choose_box) %in% names(data_envir$column_list)) {
          data_envir$column_list[svalue(gui_envir$peak_choose_box)] <-
            NULL
        }
        data_envir$column_list[[svalue(gui_envir$peak_choose_box)]] <- column_list
        dispose(gui_envir$peak_selection)
      }
    )
}

#' User input for the used peaks/pharmacons
#'
#' @return
#' @export
#'
#' @examples
peak_addition <- function() {
  gui_envir$choose_box_suggestion <-
    data_envir$peak_suggestion[svalue(gui_envir$peak_choose_box)]
  peaks <- ginput(
    paste0(
      "Please enter the used peaks!\n
      Series: ",
      svalue(gui_envir$peak_choose_box),
      "\n
      Seperate peaks with ;"
    ),
    str_c(unlist(gui_envir$choose_box_suggestion), collapse = ";")
  )
  peak_list <- strsplit(peaks, ";")
  if (svalue(gui_envir$peak_choose_box) %in% names(data_envir$peak_list)) {
    data_envir$peak_list[svalue(gui_envir$peak_choose_box)] <- NULL
  }
  if (exists("peak_list", data_envir)) {
    data_envir$peak_list[[svalue(gui_envir$peak_choose_box)]] <- peak_list
  } else {
    data_envir$peak_list[[svalue(gui_envir$peak_choose_box)]] <- peak_list
  }

  names(data_envir$peak_list)[length(data_envir$peak_list)] <-
    svalue(gui_envir$peak_choose_box)
}
