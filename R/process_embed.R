#' A tool designed to reduce redundant terms in a fitted embedding model
#'
#' @description Takes a fitted embedding model as an input. Allows users to combine
#' embeddings by the case, stem, or lemma of associated terms.
#' @param x A fitted word embedding model in the data frame format
#' @param words The name of a column that corresponds to the word dimension of the fitted word embeddings
#' @param punct Removes punctuation
#' @param tolower Combines terms that differ by case
#' @param lemmatize Combines terms that share a common lemma. Uses the lexicon package by default.
#' @param stem Combines terms that share a common stem. *Note:* Stemming should not be used in conjunction with lemmatize.
#' @keywords keyclust
#' @import textstem
#' @import data.table
#' @export
#' @return A data frame with the same columns as the input, but with redundant terms combined.
process_embed <- function(x, words = NULL, punct = TRUE, tolower = TRUE, lemmatize = TRUE, stem = FALSE) {
    UseMethod("process_embed")
}

#' @export
process_embed.data.frame <- function(x, words = NULL, punct = TRUE, tolower = TRUE, lemmatize = TRUE, stem = FALSE) {
  if(!(words %in% names(x))) {stop("Term column name not found.\\n")}
  if(tolower) {x[[words]] <- tolower(x[[words]])}
  if(lemmatize) {x[[words]] <- lemmatize_words(x[[words]])}
  if(stem) {x[[words]] <- stem_words(x[[words]])}
  numeric_logical_cols <- sapply(x, is.numeric) | sapply(x, is.logical)
  x <- as.data.table(x)
  if(punct) {
    punct_rows <- grepl(pattern = "[[:punct:]]", x[[words]])
    x <- x[!punct_rows, ]
    }
  x <- x[, lapply(.SD, mean), by = eval(words), .SDcols = numeric_logical_cols]
  x <- x[, lapply(.SD, mean), by = eval(words), .SDcols = numeric_logical_cols]
  x <- as.data.frame(x)
  return(x)
}
