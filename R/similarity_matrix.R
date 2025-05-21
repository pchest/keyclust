#' Algorithm designed to create a cosine similarity matrix from a fitted word embedding model
#'
#' @description This function takes a fitted word embedding model and computes the cosine similarity between
#' each word.
#' @param x A word embedding matrix
#' @param words A vector of words or the name of a column that corresponds to the word dimension of the fitted word embeddings
#' @param max_terms The maximum number of embedding terms that will be included in output similarity matrix.
#' Assumes that embedding input is ordered by word frequency.
#' @keywords keyclust
#' @import data.table
#' @examples
#' \dontrun{
#' # Create a set of keywords using a pre-defined set of seeds
#' simmat <- similarity_matrix(wordemb_FasttextEng_sample, words = "words")
#' }
#' @export
similarity_matrix <- function(x,
                              words = NULL,
                              max_terms = 25000) {
    UseMethod("similarity_matrix")
}

#' @export
similarity_matrix.matrix <- function(x,
                                     words = NULL,
                                     max_terms = 25000){
    if(is.null(words)) stop("Provide words column name or vector")
    if(dim(x)[1] > max_terms) x <- x[1:max_terms, ]
    if(!is.numeric(x)) stop("Matrix is not numeric")
    y <- t(x) %*% x
    out <- y / (t(sqrt(diag(y))) %*% sqrt(diag(y)))
    if(!is.null(words)) {dimnames(out) <- list(words, words)}
    return(out)
}

#' @export
similarity_matrix.data.table <- function(x,
                                         words = NULL,
                                         max_terms = 25000){
    col_keep <- NULL
    if(is.null(words)) stop("Provide words column name or vector")
    if(dim(x)[1] > max_terms) x <- x[1:max_terms, ]
    if(length(words) == 1) {
      w <- x[[words]]
      col_keep <- which(!(names(x) %in% words))
      x <- x[ , .SD, .SDcols = col_keep, drop = FALSE]
      x <- as.matrix(x)
      words <- w
      }
    if(!is.numeric(x)) stop("Matrix is not numeric")
    out <- sim_fun(x)
    if(!is.null(words)) {dimnames(out) <- list(words, words)}
    return(out)
}


#' @export
similarity_matrix.data.frame <- function(x,
                                         words = NULL,
                                         max_terms = 25000){
    col_keep <- NULL
    if(is.null(words)) stop("Provide words column name or vector")
    if(dim(x)[1] > max_terms) x <- x[1:max_terms, ]
    if(length(words) == 1) {
      w <- x[[words]]
      col_keep <- which(!(names(x) %in% words))
      x <- x[ , col_keep]
      x <- as.matrix(x)
      words <- w
      }
    if(!is.numeric(x)) stop("Matrix is not numeric")
    out <- sim_fun(x)
    if(!is.null(words)) {dimnames(out) <- list(words, words)}
    return(out)
}



## Helper functions

sim_fun <- function(x) {
  y <- x %*% t(x)
  out <- y / (sqrt(diag(y)) %*% t(sqrt(diag(y))))
  return(out)
}
