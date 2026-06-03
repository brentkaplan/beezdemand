# Empirical demand measures must not depend on the row order of the input.
# Purchase-task data is conceptually ordered by price; BP0 (first price with
# zero consumption) and the Pmaxe tie-break are price-ordered concepts, so they
# must be computed against price order, not the incidental row order of the data
# frame. Regression guard for the v0.3.0 audit.

test_that("empirical measures are invariant to input row order", {
  base <- data.frame(id = "s1", x = c(1, 2, 3, 4, 5), y = c(10, 8, 5, 0, 0))
  m_asc <- get_empirical_measures(base)$measures
  set.seed(7)
  m_shuf <- get_empirical_measures(base[sample(nrow(base)), ])$measures
  m_desc <- get_empirical_measures(base[order(-base$x), ])$measures

  for (col in c("Intensity", "BP0", "BP1", "Omaxe", "Pmaxe")) {
    expect_equal(m_shuf[[col]], m_asc[[col]], info = col)
    expect_equal(m_desc[[col]], m_asc[[col]], info = col)
  }
  # Correct values for this series, independent of order:
  expect_equal(m_asc$Intensity, 10) # consumption at lowest price
  expect_equal(m_asc$BP0, 4) # first price with zero consumption
  expect_equal(m_asc$BP1, 3) # last price with non-zero consumption
})

test_that("empirical Pmaxe tie-break (highest price among expenditure ties) is order-invariant", {
  # expenditure x*y = 8, 8, 8, 0 -> tie at x = 1, 2, 4; convention picks the
  # highest price (4). Must hold regardless of input order.
  base <- data.frame(id = "s1", x = c(1, 2, 4, 8), y = c(8, 4, 2, 0))
  m_asc <- get_empirical_measures(base)$measures
  m_desc <- get_empirical_measures(base[order(-base$x), ])$measures
  expect_equal(m_asc$Pmaxe, 4)
  expect_equal(m_desc$Pmaxe, m_asc$Pmaxe)
  expect_equal(m_desc$Omaxe, m_asc$Omaxe)
})
