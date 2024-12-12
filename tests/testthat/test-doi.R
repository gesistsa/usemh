test_that("doi #8", {
    input_data <- "Relative Negative Sentiment Bias (Sweeney & Najafian, 2019) <doi:10.18653/v1/P19-1162>, and Embedding Coherence Test (Dev & Phillips, 2019) <arXiv:1901.07656>."
    expect_true(stringr::str_detect(.fix_dois(input_data), stringr::fixed("https://doi.org/10.18653/v1/P19-1162")))
})

test_that("doi #12", {
    input_data <- "Relative Negative Sentiment Bias (Sweeney & Najafian, 2019) <doi:10.18653/v1/P19-1162>, and Embedding Coherence Test (Dev & Phillips, 2019) <arXiv:1901.07656>."
    expect_true(stringr::str_detect(.fix_dois(input_data), stringr::fixed("[doi:10.18653/v1/P19-1162]")))
})
