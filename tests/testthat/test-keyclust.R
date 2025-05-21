context("test keyclust")

test_that("keyclust works", {
    #skip_on_cran()

    simmat_FasttextEng_sample <- wordemb_FasttextEng_sample |>
      process_embed(words='words') |>
      similarity_matrix(words = "words")

    seed_words = c("september", "october", "november")

    ## Test 1: keyclust produces sensible output


    r1 <- keyclust(sim_mat = simmat_FasttextEng_sample, seed_words = seed_words, max_n = 8, sim_thresh = .4)

    r1_terms <- terms(r1)

    expect_true(sum(r1_terms$Term %in% tolower(month.name)) == nrow(r1_terms))

    ## Test 2: max_n

    expect_true(nrow(r1_terms) == 8)

    ## Test 3: cosimilarity matrix

    cm <- cosimilarity_matrix(r1)

    expect_true(dim(cm)[1] == nrow(r1_terms) & dim(cm)[2] == nrow(r1_terms))

    ## Test 4: sim_thresh

    r2 <- keyclust(sim_mat = simmat_FasttextEng_sample, seed_words = seed_words, max_n = 50, sim_thresh = .7)

    expect_true(min(terms(r2)$Group_similarity) >= 0.4)

})
