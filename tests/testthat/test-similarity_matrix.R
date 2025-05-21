context("test keyclust")

test_that("keyclust works", {
    #skip_on_cran()

    seed_words = c("October", "November", "December")

    ## Test 1: keyclust produces sensible output

    s1 <- similarity_matrix(wordemb_FasttextEng_sample, words = "words")

    # Test 1: Is square matrix
    expect_true(dim(s1)[1] == dim(s1)[1])

    #Test 2: Square matrix is equal in size to input
    expect_true(dim(s1)[1] == nrow(wordemb_FasttextEng_sample))

    # Test 3: Diagonals are equal to 1
    expect_true(sum(diag(s1)) == nrow(wordemb_FasttextEng_sample))

    # Test 4: Values are in expected range
    expect_true(max(s1) < 1.000001 & min(s1) > -1.000001)

    # Test 5: Make sure that it works with keyclust
    r1 <- keyclust(sim_mat = s1, seed_words = seed_words, max_n = 12)

    expect_true(sum(month.name %in% r1$Concept_lex$Term) == 12)

})
