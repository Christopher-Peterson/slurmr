# utility functions

last <- function(x) {
  x[length(x)]
}

#' Prints a status while running an expression
#' @param .code an expression to run
#' @param .status the status to print
#' @return the value of `.code`
#' @export
cat_status = function(.code, .status) {
  cat(.status, "...", sep = "")
  .done = force(.code)
  cat(" Done!\n")
  .done
}
