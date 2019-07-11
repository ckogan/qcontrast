#' Generate a template for specifying contrasts from a model
#'
#' @param model A model from which terms may be extracted with the \code{terms} function.
#' @export
contrast_template <- function(model) {
  labs <- mod %>%
    terms %>%
    attr("term.labels")
  x <- rep(0, length(labs))
  names(x) <- labs
  dput(x)
}

#' Create vector identifying term coefficients
#'
#' @param term_label
#' @export
identify_terms <- function(model, terms) {
  mm <- model.matrix(mod)
  assi <- attr(mm, "assign")
  lab <- attr(terms(mod), "term.labels")
  num <- which(lab %in% terms)
  as.integer(assi %in% num)
}

#' Create vector for averaging term coefficients
#'
#' @param term_label
#' @export
average_terms <- function(model, terms) {
  idterms <- identify_term(model, terms)
  idterms / (1L + sum(idterms))
}

#' Create vector for averaging effects
#'
#' @param model
#' @param terms
#' @export
average_effects <- function(model, terms) {
  idterms <- identify_terms(model, terms)
  idterms / sum(idterms)
}

#' Create a contrast matrix
#'
#' @param template
#' @export
qcontrast <- function(template) {

}
