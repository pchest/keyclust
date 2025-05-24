
## About

An R package designed to enable researchers to quickly and efficiently
generate customized sets of keywords, created by [Patrick
Chester](patrickjchester.com).

For more information about the methods underlying this package see
[Chester (2025)](https://doi.org/10.17605/OSF.IO/5B7RQ).

## Installation

Install the package from CRAN:

``` r
install.packages("keyclust")
```

Or install the development version from GitHub:

``` r
install.packages("devtools") # If not already installed

devtools::install_github("pchest/keyclust")
```

## Usage

### Creating a cosimilarity matrix from a pre-fitted word embeddings model

``` r
library(keyclust)

simmat <- wordemb_FasttextEng_sample |>
    process_embed(words = "words") |>
    similarity_matrix(words = "words")
```

### Extracting a semantically-related set of keywords from a cosimilarity matrix

``` r
seed_months <- c("october", "november")

out_months <- keyclust(simmat, seed_words = seed_months, max_n = 10)
```

    ## Initializing with october, november 
    ## Added september 
    ## Added august 
    ## Added february 
    ## Added january 
    ## Added december 
    ## Added march 
    ## Added april 
    ## Added june

``` r
out_months |>
  terms() |>
  head(n = 10)
```

    ##         Term Group_similarity
    ## 1    october        0.9764174
    ## 2   february        0.9727986
    ## 3  september        0.9715995
    ## 4    january        0.9695413
    ## 5   november        0.9693111
    ## 6     august        0.9677877
    ## 7      march        0.9673854
    ## 8      april        0.9653796
    ## 9       june        0.9625061
    ## 10  december        0.9609312
