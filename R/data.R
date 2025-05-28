#' Sample from the pre-trained English fastText model
#'
#' @description This is a data frame containing the 2,000 most frequently occurring
#' terms from Facebook's English-language fastText word embeddings model.
#'
#' @format A 2000 row and 301 column data frame. The row represents the word embedding
#' term, while the numeric columns represent the word embedding dimension. The character
#' column gives the terms associated with each word vector.
#'
#' @examples
#' data(wordemb_FasttextEng_sample)
#' head(wordemb_FasttextEng_sample)
#'
#' @references P. Bojanowski*, E. Grave*, A. Joulin, T. Mikolov,
#' Enriching Word Vectors with Subword Information
#' (\href{https://arxiv.org/abs/1607.04606}{arxiv})
#'
#' @keywords data
"wordemb_FasttextEng_sample"
