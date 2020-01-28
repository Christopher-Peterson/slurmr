# This contains functions helpful for parsing job setup config files


#' Checks to make sure that global variables are defined
#' @param ... unquoted global parameters
#' @return invisible
#' @importFrom rlang ensyms list2 has_name list2 is_list
#' @export
require_globals = function(...) {
  globals = ensyms(...) %>%
    vapply(as.character, "chr")
  globals_exist = globals %>%
    vapply(exists, TRUE, envir = globalenv())
  if(!all(globals_exist)) {
    stop(glue("Globals not defined: {glbls}",
              glbls = glue_collapse(globals[!globals_exist], ", ")),
         call. = FALSE)
  }
  invisible()
}
#' Ensure that global variables have (only) the right names
#' @param names required names
#' @param ... global variables to check
#' @importFrom purrr map_chr map_lgl
#' @export
require_names = function(names, ...) {
  arg_nms = ensyms(...) %>% map_chr(rlang::as_string)
  args = list2(...)
  arg_lens = vapply(args, length, 1L) == length(names)
  names_present = map_lgl(args, ~all(has_name(.x, names)))
  valid = arg_lens & names_present
  if(all(valid)) return(invisible(NULL))
  # Throws an error below
  bad_args = glue_collapse(arg_nms[!valid], sep = ", ")
  req_names = glue_collapse(names, sep = ", ")
  stop(glue("Globals have invalid names:
            Required names: {req_names}
            Invalid globals: {bad_args}"), call. = FALSE)
}


#' unnests arguments to be saved in a database
#' This is helpful for environmental & popsize args that hav multiple populations
#' @param .data a tibble of arguments
#' @return an unnested data frame, with an extra sub_pop column
#' @export
unnest_args = function(.data) {
  # expand list_cols in a reasonable way, so that
  list_cols = vapply(.data, is_list, TRUE)
  # nesting_index used to be sub_pop (if that's a problem)
  if(!any(list_cols)) return(.data %>% mutate(nesting_index = 1))
  nested_cols = names(.data)[list_cols]
  .data %>% mutate(nesting_index = 1:n()) %>%
    tidyr::unchop(!!nested_cols)
}