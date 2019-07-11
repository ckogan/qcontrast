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
  mm <- model.matrix(model)
  assi <- attr(mm, "assign")
  lab <- attr(terms(mod), "term.labels")
  num <- which(lab %in% terms)
  as.integer(assi %in% num)
}

#' Create matrix identifying term levels
#'
#' @export
identify_levels <- function(model, terms) {
  mm <- model.matrix(model)
  assi <- attr(mm, "assign")
  lab <- attr(terms(model), "term.labels")
  num <- which(lab %in% terms)
  ix <- which(assi %in% num)
  mat <- matrix(0, nrow = length(ix) + 1L, ncol = length(assi))
  for(i in seq_along(ix)) {
    mat[i+1L, ix[i]] <- 1
  }
  mat
}

#' Create matrix of ordered effects for factor
#'
#' @export
ordered_effects <- function(model, terms) {
  mat <- identify_levels(model, terms)
  eff <- matrix(0, nrow = nrow(mat)-1L, ncol = ncol(mat))
  for(i in seq_len(nrow(eff))) {
    eff[i,] <- mat[i+1,] - mat[i,]
  }
  eff
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
