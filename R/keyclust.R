#' Algorithm designed to efficiently extract keywords from a cosine similarity matrix
#'
#' @description This function takes a cosine similarity matrix derived from a word embedding model,
#' along with a set of seed words and outputs a semantically-related set of keywords of
#' a length and cosimilarity determined by the user
#' @param sim_mat A cosine similarity matrix produced by cosine.
#' @param seed_words A set of user-provided seed words that best represent the target concept.
#' @param sim_thresh Minimum cosine similarity a candidate word must have to the existing set of keywords
#'   for it to be added.
#' @param max_n The maximum size of the output set of keywords.
#' @param dictionary An optional dictionary that maps metadata, such as definitions, to keywords.
#' @param exclude A vector of words that the user does not want included in the final keyword set.
#' @param verbose If true, keyclust will produce live updates as it adds keywords.
#' @keywords keyclust
#' @import data.table
#' @examples
#' \donttest{
#' # Create a set of keywords using a pre-defined set of seeds
#' seeds <- c("October", "November")
#' # Create a cosine similarity matrix from a word embedding model
#' simmat_FasttextEng_sample <- wordemb_FasttextEng_sample |>
#'     process_embed(words='words') |>
#'     similarity_matrix(words = "words")
#' # Use keyclust to generate a set of keywords
#' months <- keyclust(simmat_FasttextEng_sample, seed_words = seeds, max_n = 8)
#' }
#' @export
keyclust <- function(sim_mat,
                     seed_words,
                     sim_thresh = 0.25,
                     max_n = 50,
                     dictionary = NULL,
                     exclude = NULL,
                     verbose = TRUE) {
    UseMethod("keyclust")
}

#' @export
keyclust.data.frame <- function(sim_mat,
                            seed_words,
                            sim_thresh = 0.25,
                            max_n = 50,
                            dictionary = NULL,
                            exclude = NULL,
                            verbose = TRUE){
  row_names <- col_names <- mean_sim <- X <- Group_similarity <- NULL
  sim_mat <- as.data.table(sim_mat)
  if(verbose) {cat('Initializing with', paste(seed_words, collapse = ", "), '\n')}
  in_mat <- seed_words %in% names(sim_mat)
  missing_words <- seed_words[!in_mat]
  if(verbose){
   for(i in missing_words){
    cat(i, "not found\n")
  }
  if(length(missing_words) == length(seed_words)) {
    stop('All seeds were missing\n')
  }
  }
  seed_words <- seed_words[in_mat]
  sim_mat$row_names <- setdiff(names(sim_mat), "row_names")
  if(!is.null(exclude)) sim_mat[!(row_names %in% exclude), ][, (exclude) := NULL]
  col_names <- c("row_names", seed_words)
  while(length(seed_words) < max_n){
    sub <- sim_mat[, .SD, .SDcols = col_names
                    ][, mean_sim := rowMeans(.SD), .SD = seed_words
                    ][!(row_names %in% seed_words), ] |>
        setorder(-mean_sim)
    if(sub$mean_sim[1] < sim_thresh) break
    seed_words <- c(seed_words, sub$row_names[1])
    if(verbose) cat("Added", sub$row_names[1],"\n")
    col_names <- c("row_names", seed_words)
  }
  dict <- data.table(row_names = seed_words, X = 1:length(seed_words))
  sim_mat <- sim_mat[, .SD, .SDcols = col_names][row_names %in% seed_words, ] |>
    merge(dict, by = "row_names") |>
    setorder(X)
  sim_mat[, X := NULL]
  out_mat <- as.matrix(sim_mat[, -1])
  rownames(out_mat) <- colnames(out_mat)
  rownames(out_mat) <- sim_mat$row_names
  sim_group <- rowMeans(out_mat)
  word_df <- data.frame(Term = seed_words, Group_similarity = sim_group)
  rownames(word_df) <- NULL
  if(!is.null(dictionary)) word_df <- merge(word_df, dictionary, by = "Term", all.x = TRUE)
  word_df <- word_df |>
    setorder(-Group_similarity)
  rownames(word_df) <- NULL
  out <- list(Concept_lex = word_df, Cosim_mat = out_mat)
  class(out) <- c("keyclust", "list")
  return(out)
}

#' @export
keyclust.matrix <- function(sim_mat,
                            seed_words,
                            sim_thresh = 0.25,
                            max_n = 50,
                            dictionary = NULL,
                            exclude = NULL,
                            verbose = TRUE){
  row_names <- col_names <- mean_sim <- X <- Group_similarity <- NULL
  sim_mat <- as.data.table(sim_mat)
  if(verbose) {cat('Initializing with', paste(seed_words, collapse = ", "), '\n')}
  in_mat <- seed_words %in% names(sim_mat)
  missing_words <- seed_words[!in_mat]
  if(verbose){
   for(i in missing_words){
    cat(i, "not found\n")
  }
  if(length(missing_words) == length(seed_words)) {
    stop('All seeds were missing\n')
  }
  }
  seed_words <- seed_words[in_mat]
  sim_mat$row_names <- setdiff(names(sim_mat), "row_names")
  #sim_mat[, row_names := setdiff(names(sim_mat), "row_names")]
  if(!is.null(exclude)) sim_mat[!(row_names %in% exclude), ][, (exclude) := NULL]
  col_names <- c("row_names", seed_words)
  while(length(seed_words) < max_n){
    sub <- sim_mat[, .SD, .SDcols = col_names
                    ][, mean_sim := rowMeans(.SD), .SD = seed_words
                    ][!(row_names %in% seed_words), ] |>
        setorder(-mean_sim)
    if(sub$mean_sim[1] < sim_thresh) break
    seed_words <- c(seed_words, sub$row_names[1])
    if(verbose) cat("Added", sub$row_names[1],"\n")
    col_names <- c("row_names", seed_words)
  }
  dict <- data.table(row_names = seed_words, X = 1:length(seed_words))
  sim_mat <- sim_mat[, .SD, .SDcols = col_names][row_names %in% seed_words, ] |>
    merge(dict, by = "row_names") |>
    setorder(X)
  sim_mat[, X := NULL]
  out_mat <- as.matrix(sim_mat[, -1])
  rownames(out_mat) <- colnames(out_mat)
  rownames(out_mat) <- sim_mat$row_names
  sim_group <- rowMeans(out_mat)
  word_df <- data.frame(Term = seed_words, Group_similarity = sim_group)
  rownames(word_df) <- NULL
  if(!is.null(dictionary)) word_df <- merge(word_df, dictionary, by = "Term", all.x = TRUE)
  word_df <- word_df |>
    setorder(-Group_similarity)
  rownames(word_df) <- NULL
  out <- list(Concept_lex = word_df, Cosim_mat = out_mat)
  class(out) <- c("keyclust", "list")
  return(out)
}

#' Returns terms generated by keyclust
#'
#' @description A function that returns the terms and their cosine cosimilarities
#' produced by [keyclust()]
#' @param x output from [keyclust()]
#' @param ... additional arguments not used
#' @keywords keyclust
#' @method terms keyclust
#' @export
#' @rdname terms
terms.keyclust <- function(x, ...) {
    return(x$Concept_lex)
}

#' Prints terms generated by keyclust
#' @param x output from [keyclust()]
#' @param ... additional arguments not used
#' @keywords keyclust
#' @method print keyclust
#' @importFrom utils head
#' @export
#' @rdname print
print.keyclust <- function(x, ...) {
    cat("keyclust produced", nrow(x$Concept_lex), "keywords\n\n")
    print(head(x$Concept_lex))
}


