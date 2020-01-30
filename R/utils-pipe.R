#' Pipe operator
#'
#' See `magrittr::[\%>\%][magrittr::pipe]` for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL

#' Replace missing values
#'
#' See `rlang::[\%|\%][rlang::op-na-default]` for details.
#' @name op-na-default
#' @keywords internal
#' @rdname op-na-default
#' @export
#' @usage x \%|\% value_if_x_is_na
`%|%` = rlang::`%|%`
