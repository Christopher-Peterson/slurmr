# This is used to turn a configured setup list into a dataframe of parameter combinations

#' Get all combinations of each named argument
#'
#' Pass a formula or list of formulas to delay evaluation until after
#' after combinations have been made.
#' @param ... arguments to get combinations of; splicable with !!!
#' @return a tibble of all argument combinations
#' @export
#' @importFrom purrr pmap_dfr reduce2 modify_if cross pmap_dfr
#' @importFrom dplyr bind_rows mutate n
arg_combinations <- function(...) {
  # browser()
  combo_list <- rlang::list2(...) %>%
    modify_if(rlang::is_list, encapsulate_elements) %>%
    modify_if(rlang::is_formula, ~ list(list(.x)))
  combo_list %>%
    cross() %>%
    bind_rows() %>%
    eval_formula_args()
}
# convert wrap each list element in its own list; this helps
# with making list columns in tibbles
encapsulate_elements <- function(lst) {
  lapply(lst, list)
}

# Returns TRUE if it's a list of formulas
is_formula_list <- function(x) {
  rlang::is_list(x) && rlang::is_formula(x[[1]])
}

eval_formula_args <- function(combos) {
  # Parse formula specifications in the arg combos
  non_formulas <- rlang::names2(combos)[
    !vapply(combos, is_formula_list, TRUE)
  ]
  if (length(non_formulas) == ncol(combos)) {
    return(combos)
  }
  # Nest non-formulas so the formulae can be easily mapped over
  combo_nested <- tidyr::nest(combos, data = !!non_formulas)
  pmap_dfr(combo_nested, function(..., data) {
    formula_args <- list(...)
    # Evaluate formulae in order
    reduce2(formula_args, rlang::names2(formula_args),
      function(.dat, .form, .name) {
        expr <- rlang::f_rhs(.form)
        .dat %>% mutate(!!.name := !!expr)
      },
      .init = data
    )
  })
  # The return value should be a single data frame with the same
  # names as combos, but everything evaluated
}
