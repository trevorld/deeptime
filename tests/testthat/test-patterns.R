test_that("scale_fill_geopattern works", {
  # invalid code
  vals <- c("101", "313", "603", "999")
  gg <- ggplot(mpg, aes(factor(cyl), fill = vals[factor(cyl)])) +
    geom_bar() +
    scale_fill_geopattern()
  expect_error(print(gg))

  skip_if(R.Version()$os != "mingw32") # only test the rest on Windows
  vals <- c("101", "313", "603", "733")
  gg <- ggplot(mpg, aes(factor(cyl), fill = vals[factor(cyl)])) +
    geom_bar() +
    scale_fill_geopattern()
  expect_doppelganger_deeptime("scale_fill_geopattern", gg, patterns = TRUE)

  # custom labels
  gg <- ggplot(mpg, aes(factor(cyl), fill = vals[factor(cyl)])) +
    geom_bar() +
    scale_fill_geopattern(labels = c("101" = "test"))
  expect_doppelganger_deeptime("scale_fill_geopattern_labels", gg, patterns = TRUE)

  # custom limits
  gg <- ggplot(mpg, aes(factor(cyl), fill = vals[factor(cyl)])) +
    geom_bar() +
    scale_fill_geopattern(limits = c("101", "313"))
  expect_doppelganger_deeptime("scale_fill_geopattern_limits", gg, patterns = TRUE)

  # test with NA values
  vals <- c("101", "313", "603", NA)
  gg <- ggplot(mpg, aes(factor(cyl), fill = vals[factor(cyl)])) +
    geom_bar() +
    scale_fill_geopattern()
  expect_doppelganger_deeptime("scale_fill_geopattern_NA", gg, patterns = TRUE)

  # test with custom NA value
  gg <- ggplot(mpg, aes(factor(cyl), fill = vals[factor(cyl)])) +
    geom_bar() +
    scale_fill_geopattern(na.value = geo_pattern("701"))
  expect_doppelganger_deeptime("scale_fill_geopattern_NA2", gg, patterns = TRUE)
})

test_that("geo_grob works", {
  grob <- geo_grob("101")
  expect_true(grid::is.grob(grob))

  expect_error(geo_grob(999))
  expect_error(geo_grob("test"))

  expect_doppelganger_deeptime("geo_grob", {
    grid.newpage()
    grid.draw(grob)
  })
})

test_that("geo_pattern works", {
  patt <- geo_pattern("101")
  expect_true(is(patt, "GridPattern"))

  expect_error(geo_pattern(999))
  expect_error(geo_pattern("test"))

  pattern1 <- geo_pattern(code = "313-K")
  pattern2 <- geo_pattern(code = "607")

  expect_doppelganger_deeptime("geo_pattern1", {
    grid.newpage()
    grid.draw(rectGrob(gp = gpar(fill = pattern1)))
  }, patterns = TRUE)

  expect_doppelganger_deeptime("geo_pattern2", {
    grid.newpage()
    grid.draw(rectGrob(gp = gpar(fill = pattern2)))
  }, patterns = TRUE)
})

test_that("grid.pattern_geo works", {
  x <- 0.5 + 0.5 * cos(seq(2 * pi / 4, by = 2 * pi / 6, length.out = 6))
  y <- 0.5 + 0.5 * sin(seq(2 * pi / 4, by = 2 * pi / 6, length.out = 6))

  expect_doppelganger_deeptime("grid.pattern_geo", {
    grid.newpage()
    grid.pattern_geo(params = list(pattern_type = "633", pattern_scale = 4),
                     boundary_df = data.frame(x, y, id = 1))
  }, patterns = TRUE)
})

test_that("ggpattern works", {
  skip_if_not_installed("ggpattern")
  skip_if_not_installed("gridpattern", "1.5.1")
  skip_if("geo" %in% gridpattern::conflicting_patterns()$name,
          "a conflicting 'geo' pattern is registered")
  df <- data.frame(trt = c("a", "b", "c"), outcome = c(2.3, 1.9, 3.2))
  gg <- ggplot(df, aes(trt, outcome)) +
    geom_col_pattern(aes(color = trt, pattern_type = trt), pattern = 'geo',
                     pattern_color = "black", fill = "white",
                     pattern_fill = "white") +
    scale_pattern_type_manual(values = c("101", "313", "999")) +
    scale_color_viridis_d() +
    theme(legend.key.size = unit(1.5, 'cm'))
  expect_error(print(gg))

  skip_if(R.Version()$os != "mingw32") # only test the rest on Windows
  gg <- ggplot(df, aes(trt, outcome)) +
    geom_col_pattern(aes(color = trt, pattern_type = trt), pattern = 'geo',
                     pattern_color = "black", fill = "white",
                     pattern_fill = "white") +
    scale_pattern_type_manual(values = c("101", "313", "634")) +
    scale_color_viridis_d() +
    theme(legend.key.size = unit(1.5, 'cm'))
  expect_doppelganger_deeptime("ggpattern", gg, patterns = TRUE)
})

test_that("geo pattern is registered with gridpattern", {
  skip_if_not_installed("gridpattern", "1.5.1")
  expect_true("geo" %in% gridpattern::registered_patterns()$name)
  expect_true(gridpattern::has_pattern("deeptime::geo"))

  # the bare name "geo" is ambiguous if another "geo" pattern is registered
  skip_if("geo" %in% gridpattern::conflicting_patterns()$name,
          "a conflicting 'geo' pattern is registered")

  x <- 0.5 + 0.5 * cos(seq(2 * pi / 4, by = 2 * pi / 6, length.out = 6))
  y <- 0.5 + 0.5 * sin(seq(2 * pi / 4, by = 2 * pi / 6, length.out = 6))

  # registered defaults apply when drawn through gridpattern
  expect_doppelganger_deeptime("grid.pattern_geo_registered", {
    grid.newpage()
    gridpattern::grid.pattern("geo", x = x, y = y)
  }, patterns = TRUE)

  # explicit parameters and the `gp` fill (background) and col are respected
  expect_doppelganger_deeptime("grid.pattern_geo_registered_gp", {
    grid.newpage()
    gridpattern::grid.pattern("geo", x = x, y = y, type = "633", scale = 4,
                              gp = gpar(fill = "lightblue", col = "darkblue"))
  }, patterns = TRUE)

  skip_if_not_installed("ggpattern")
  # `pattern_type` defaults to "101" when not specified in ggpattern
  df <- data.frame(trt = c("a", "b", "c"), outcome = c(2.3, 1.9, 3.2))
  gg <- ggplot(df, aes(trt, outcome)) +
    geom_col_pattern(pattern = "geo")
  expect_no_error(ggplotGrob(gg))
  # draw on a throwaway device, then restore the previously active device
  old_dev <- dev.cur()
  pdf(NULL)
  withr::defer({
    dev.off()
    if (old_dev > 1L) dev.set(old_dev)
  })
  expect_no_error(print(gg))
})

test_that("fgdc_dict works", {
  dict <- fgdc_dict()
  expect_true(is.function(dict))
  vals <- c("603", "626", "720", "733")
  expect_equal(dict(vals), c("Crossbedded gravel or conglomerate", "Chalk",
                             "Banded igneous rock", "Ore"))
  dict <- fgdc_dict(clean = FALSE, wrap = 10)
  expect_equal(dict("601"), "Gravel or\nconglomerate\n(1st\noption)")
})
