.onLoad <- function(...) {
  # coord_geo_polar theme elements
  ggplot2::register_theme_elements(
    deeptime.scale.background = ggplot2::element_rect(fill = NA, color = NA),
    deeptime.axis.line.r = ggplot2::element_line(),
    deeptime.axis.text.r = ggplot2::element_text(size = 3.5, vjust = -2,
                                                 hjust = -0.5),
    deeptime.axis.ticks.r = ggplot2::element_line(),
    deeptime.axis.ticks.length.r = grid::unit(1.5, "points"),
    element_tree = list(
      deeptime.scale.background = ggplot2::el_def("element_rect", "rect"),
      deeptime.axis.line.r = ggplot2::el_def("element_line", "line",
                                             "axis.line"),
      deeptime.axis.text.r = ggplot2::el_def("element_text", "text",
                                             "axis.text"),
      deeptime.axis.ticks.r = ggplot2::el_def("element_line", "line",
                                              "axis.ticks"),
      deeptime.axis.ticks.length.r = ggplot2::el_def("unit",
                                                     "axis.ticks.length")
    )
  )

  # register the "geo" pattern for ggpattern/gridpattern
  rlang::on_package_load("gridpattern", register_geo_pattern())
}

# Registers the "geo" pattern with gridpattern (owner "deeptime", qualified
# name "deeptime::geo") so it can be used with ggpattern and
# gridpattern::grid.pattern(). gridpattern is only in Suggests, so check that
# the installed version has `register_pattern()` (>= 1.5.1) and otherwise fall
# back to the legacy option (without clobbering any other registered patterns).
register_geo_pattern <- function() {
  if (exists("register_pattern", envir = asNamespace("gridpattern"),
             inherits = FALSE)) {
    gridpattern::register_pattern(
      "geo",
      grid.pattern_geo,
      defaults = list(
        pattern_type = "101",
        pattern_colour = function(params, gp) {
          params$pattern_color %||% gp$col %||% "black"
        },
        pattern_fill = NA,
        pattern_scale = 2,
        fill = function(params, gp) gp$fill %||% "white"
      ),
      unprefixed = "fill"
    )
  } else {
    funcs <- getOption("ggpattern_geometry_funcs", list())
    funcs[["geo"]] <- grid.pattern_geo
    options(ggpattern_geometry_funcs = funcs)
  }
}
