resample_pvalue <- function(pts, rect, cell = 700, n = 500, seed = 28092025) {
  bb <- sf::st_bbox(rect)
  grid <- sf::st_make_grid(rect, cellsize = c(cell, cell),
                           what = "polygons", square = TRUE,
                           offset = c(bb["xmin"], bb["ymin"])) |>
    sf::st_as_sf()
  counts <- lengths(sf::st_intersects(grid, pts))
  lambda <- mean(counts)
  probs  <- c(dpois(0:4, lambda), 1 - ppois(4, lambda))
  if (!is.null(seed)) set.seed(seed)
  samp <- sample(counts, n, replace = TRUE)
  obs  <- tabulate(pmin(samp, 5) + 1, nbins = 6)
  exp  <- n * probs
  chisq <- sum((obs - exp)^2 / exp)
  pchisq(chisq, df = 5, lower.tail = FALSE)
}
