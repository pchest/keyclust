## ----warn=FALSE, message=FALSE------------------------------------------------
rm(list = ls())

library(R.utils)
library(keyclust)
library(data.table)
library(knitr)

embedding_rows <- 20000

## -----------------------------------------------------------------------------
options(timeout = 400)

# For English language embeddings produced by the FastText Model
if (!file.exists("cc.en.300.vec")) {
  download.file("https://dl.fbaipublicfiles.com/fasttext/vectors-crawl/cc.en.300.vec.gz",  destfile = "cc.en.300.vec.gz")
  gunzip("cc.en.300.vec.gz")
}

# For English language embeddings produced by the GloVe Model

if (!file.exists("glove.6B.300d.txt")) {
  download.file("https://nlp.stanford.edu/data/glove.6B.zip", destfile = "glove6B.zip")
  unzip("glove6B.zip")
}

# For Chinese language embeddings produced by the FastText Model
if (!file.exists("cc.zh.300.vec")) {
  download.file("https://dl.fbaipublicfiles.com/fasttext/vectors-crawl/cc.zh.300.vec.gz",  destfile = "cc.zh.300.vec.gz")
  gunzip("cc.zh.300.vec.gz")
}

## -----------------------------------------------------------------------------
ft_embed <- fread("cc.en.300.vec", nrows = embedding_rows, skip = 1, quote = "")
names(ft_embed) <- c("words", paste0("V", 1:300)) # Default column names are not great

## -----------------------------------------------------------------------------
dim(ft_embed)

## -----------------------------------------------------------------------------
head(ft_embed[, 1:10]) |> 
  kable()

## -----------------------------------------------------------------------------
cat(ft_embed$words[1:100])

## -----------------------------------------------------------------------------
ft_embed_processed <- process_embed(ft_embed, words = "words", punct = TRUE, tolower = TRUE, lemmatize = TRUE)

## -----------------------------------------------------------------------------
dim(ft_embed_processed)

## -----------------------------------------------------------------------------
cat(ft_embed_processed$words[1:100])

## -----------------------------------------------------------------------------
ft_embed_sim_mat <- similarity_matrix(ft_embed_processed, words = "words")

## -----------------------------------------------------------------------------
str(ft_embed_sim_mat)

## -----------------------------------------------------------------------------
peace <- c("peace", "harmony", "tranquility")

## -----------------------------------------------------------------------------
keyclust_peace_output <- keyclust(sim_mat = ft_embed_sim_mat,
                                  seed_words = peace,
                                  max_n = 20)

## -----------------------------------------------------------------------------
keyclust_peace_output |>
  terms() |>
  kable()

## -----------------------------------------------------------------------------
democracy <- c("election", "vote", "campaign", "candidate")

## -----------------------------------------------------------------------------
keyclust_democracy_output <- keyclust(sim_mat = ft_embed_sim_mat,
                                      seed_words = democracy,
                                      max_n = 20)

## -----------------------------------------------------------------------------
keyclust_democracy_output |>
  terms() |>
  kable()

## -----------------------------------------------------------------------------
rm(list = c("ft_embed", "ft_embed_processed"))
gc()

## -----------------------------------------------------------------------------
gl_embed <- fread("glove.6B.300d.txt", nrows = embedding_rows, header = TRUE, quote = "")
names(gl_embed) <- c("words", paste0("V", 1:300)) # Default column names are not great

## -----------------------------------------------------------------------------
dim(gl_embed)

## -----------------------------------------------------------------------------
keyclust_peace_output <-
  gl_embed |>
  process_embed(words = "words") |>
  similarity_matrix(words = "words") |>
  keyclust(seed_words = peace, max_n = 20)

## -----------------------------------------------------------------------------
keyclust_peace_output |>
  terms() |>
  kable()

## -----------------------------------------------------------------------------
rm(list = "gl_embed")
gc()

## -----------------------------------------------------------------------------
ft_cn_embed <- fread("cc.zh.300.vec", nrows = embedding_rows, skip = 1, quote = "")
names(ft_cn_embed) <- c("words", paste0("V", 1:300)) # Default column names are not great

## -----------------------------------------------------------------------------
head(ft_cn_embed[, 1:10]) |>
  kable()

## -----------------------------------------------------------------------------
peace_cn <- c("和平", # he2ping2, peace
              "平安", # ping2an1, safe
              "和谐") # he2xie2, harmonious

## -----------------------------------------------------------------------------
keyclust_peace_output <-
  ft_cn_embed |>
  process_embed(words = "words") |>
  similarity_matrix(words = "words") |>
  keyclust(seed_words = peace_cn,
           #max_n = 20,
           sim_thresh = 0.4)

## -----------------------------------------------------------------------------
keyclust_peace_output |>
  terms() |>
  kable()

