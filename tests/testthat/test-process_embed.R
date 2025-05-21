context("test keyclust")

test_that("process_embed works", {
    #skip_on_cran()

    wordemb_FasttextEng_pe <- wordemb_FasttextEng_sample |>
        process_embed(words = "words")

    ## Test 1: Number of tokens has gone down
    expect_true(dim(wordemb_FasttextEng_pe)[1] < dim(wordemb_FasttextEng_sample)[1])

    ## Test 2: no duplicate entries
    expect_true(dim(wordemb_FasttextEng_pe)[1] == length(unique(wordemb_FasttextEng_pe$words)))

    # Test 3: Compatible with similarity_matrix function
    sm <- similarity_matrix(wordemb_FasttextEng_pe, words = "words")

    expect_true(dim(sm)[1] == nrow(wordemb_FasttextEng_pe) & dim(sm)[2] == nrow(wordemb_FasttextEng_pe))

    # Test 5: Make sure that it works with keyclust
    seed_words <- c("october", "november")

    r1 <- keyclust(sim_mat = sm, seed_words = seed_words, max_n = 11)

    expect_true(sum(tolower(month.name) %in% r1$Concept_lex$Term) == 11)

})
