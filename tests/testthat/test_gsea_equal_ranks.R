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

test_that("plotEnrichment supports stats with equal ranks", {
    statsWithTies <- c(5, 1, 1, 1, 1, -1, -1, -1, -5)
    sample1 <- c(2, 3, 6, 7)
    sample2 <- c(4, 5, 6, 7)

    expect_equal(statsWithTies[sample1], statsWithTies[sample2])

    p1 <- plotEnrichment(as.character(sample1), setNames(statsWithTies, as.character(seq_along(statsWithTies))))
    p2 <- plotEnrichment(as.character(sample2), setNames(statsWithTies, as.character(seq_along(statsWithTies))))

    expect_equal(p1, p2)

    # Comparing with old version

    data("examplePathways")
    data("exampleRanks")

    p <- plotEnrichment(examplePathways[["5991130_Programmed_Cell_Death"]], exampleRanks)

    oldPlotPath <- paste(getwd(), "/old.svg", sep="")
    newPlotPath <- paste(getwd(), "/new.svg", sep="")
    ggsave(newPlotPath, p, width = 8, height = 6)

    oldPlotFile <- file(oldPlotPath, "rb")
    oldPlotInfo <- file.info(oldPlotPath)
    oldSize <- oldPlotInfo$size

    newPlotFile <- file(newPlotPath, "rb")
    newPlotInfo <- file.info(newPlotPath)
    newSize <- newPlotInfo$size

    oldData <- readBin(oldPlotFile, integer(), size = 1, n = oldSize)
    newData <- readBin(newPlotFile, integer(), size = 1, n = newSize)

    close(oldPlotFile)
    close(newPlotFile)
    file.remove(newPlotPath)

    expect_equal(oldData, newData)
})

test_that("calcGseaStatsCumulative supports equal ranks", {
    stats <- c(5, 1, 1, 1, 1, -1, -1, -1, -5)
    sample1 <- c(3, 6, 2, 7, 9, 5, 4, 1)

    groupInfo <- distinguishGroups(stats, 1e-15)

    ess <- calcGseaStatCumulative(stats, sample1, gseaParam = 1,
                                  geneToGroup = groupInfo$geneToGroup,
                                  groupEnds = groupInfo$groupEnds)

    for (i in seq_along(sample1)) {
        expect_equal(ess[i], calcGseaStat(stats, sample1[seq_len(i)]))
    }
})
