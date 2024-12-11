# keyclust
This package is designed to enable researchers to quickly and efficiently generate customized sets of keywords. It is currently in the early alpha stage of development.

## Installation

```{r}
install.packages("devtools") # If not already installed

devtools::install_github("pchest/keyclust")
```

## Usage

### Creating a cosimilarity matrix from a pre-fitted word embeddings model

```{r}
library(keyclust)

simmat <- wordemb_FasttextEng_sample |>
    process_embed(words = "words") |>
    similarity_matrix(words = "words")
```

### Extracting a semantically-related set of keywords from a cosimilarity matrix

```{r}
seed_months <- c("october", "november")

out_months <- keyclust(simmat, seed_words = seed_months, max_n = 10)

out_months |>
  terms() |>
  head(n = 10)
```
