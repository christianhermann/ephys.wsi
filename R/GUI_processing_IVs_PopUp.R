#' Opens a dialog to chose the right column for the currentin
#'
#' @param win
#' @param search_name
#' @param iv_name
#' @param col_names
#'
#' @return
#' @export
#'
#' @examples
openColumnCorrection <-
  function(win, search_name, iv_name, col_names) {
    gui_envir$columnCorrection <-
      gwindow(
        "Column-Correction",
        visible = T,
        width = 400,
        height = 30
      )
    g <-
      ggroup(cont = gui_envir$columnCorrection, horizontal = FALSE)
    label <-
      glabel(
        paste0(
          "No ",
          search_name,
          " found in: \n",
          iv_name,
          "\nPlease choose the column which shows the ",
          search_name,
          ":"
        ),
        cont = g
      )
    col_namesCombobox <-
      gcombobox(col_names, selected = 1, cont = g)
    button <-
      gbutton(
        "Select Column",
        cont = g,
        handler = function(h, ...) {
          gui_interaction_envir$correctColumn <-
            svalue(col_namesCombobox)
          dispose(gui_envir$columnCorrection)
          # gtkMainQuit()
          gui_envir$waitForAction <- TRUE
        }
      )

    addHandlerUnrealize(
      gui_envir$columnCorrection,
      handler = function(h, ...) {
        val <- gconfirm("Really close window", parent = h$obj)
        if (as.logical(val)) {
          # gtkMainQuit()
          gui_envir$waitForAction <- TRUE

          return(FALSE)
        } # destroy
        else {
          return(TRUE)
        } # don't destroy
      }
    )
    tkwait.window(gui_envir$columnCorrection$widget)
  }
