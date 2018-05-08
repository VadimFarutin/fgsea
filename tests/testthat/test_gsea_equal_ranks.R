context("GSEA equal ranks maintaining")

test_that("calcGseaStats returns correct leadingEdge for positive enrichment score", {
    statsWithTies <- c(5, 1, 1, 1, 1, 1, -1, -1, -1, -5)
    sample1 <- c(1, 2, 3, 4, 5, 7, 8)
    sample2 <- c(1, 3, 4, 5, 6, 7, 8)

    expect_equal(statsWithTies[sample1], statsWithTies[sample2])

    res1 <- calcGseaStat(statsWithTies, selectedStats = sample1, returnLeadingEdge = TRUE)
    res2 <- calcGseaStat(statsWithTies, selectedStats = sample2, returnLeadingEdge = TRUE)

    expect_equal(res1$res, res2$res)
    expect_equal(c(1, 2, 3, 4, 5), res1$leadingEdge)
    expect_equal(c(1, 3, 4, 5, 6), res2$leadingEdge)
})

test_that("calcGseaStats returns correct leadingEdge for negative enrichment score", {
    statsWithTies <- c(5, 1, 1, 1, 1, -1, -1, -1, -5)
    sample1 <- c(6, 8)
    sample2 <- c(6, 7)

    expect_equal(statsWithTies[sample1], statsWithTies[sample2])

    res1 <- calcGseaStat(statsWithTies, selectedStats = sample1, returnLeadingEdge = TRUE)
    res2 <- calcGseaStat(statsWithTies, selectedStats = sample2, returnLeadingEdge = TRUE)

    expect_equal(res1$res, res2$res)
    expect_equal(c(8, 6), res1$leadingEdge)
    expect_equal(c(7, 6), res2$leadingEdge)
})

test_that("calcGseaStats supports stats with equal ranks and positive enrichment score", {
    statsWithTies <- c(5, 1, 1, 1, 1, -1, -1, -1, -5)
    sample1 <- c(2, 3, 6, 7)
    sample2 <- c(4, 5, 6, 7)

    expect_equal(statsWithTies[sample1], statsWithTies[sample2])
    expect_equal(calcGseaStat(statsWithTies, selectedStats = sample1),
                 calcGseaStat(statsWithTies, selectedStats = sample2))
})

test_that("calcGseaStats supports stats with equal ranks and negative enrichment score", {
    statsWithTies <- c(5, 1, 1, 1, 1, -1, -1, -1, -5)
    sample1 <- c(6, 8)
    sample2 <- c(6, 7)

    expect_equal(statsWithTies[sample1], statsWithTies[sample2])
    expect_equal(calcGseaStat(statsWithTies, selectedStats = sample1),
                 calcGseaStat(statsWithTies, selectedStats = sample2))
})

test_that("calcGseaStats returns correct new selectedStats", {
    statsWithTies <- c(5, 1, 1, 1, 1, -1, -1, -1, -5)
    sample1 <- c(1, 2, 4, 6, 7)
    correctSelectedStats <- c(1, 5, 8)

    res <- calcGseaStat(statsWithTies, selectedStats = sample1, returnAllExtremes = TRUE)

    expect_equal(res$selectedStats, correctSelectedStats)
})
