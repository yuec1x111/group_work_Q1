library(sf)

cell <- 700
grid <- st_make_grid(rect, cellsize = c(cell, cell), what = "polygons", square = TRUE)
grid <- st_as_sf(grid)

hits_per_cell <- lengths(st_intersects(grid, pts_proj))

lambda <- mean(hits_per_cell)
p_cat <- c(dpois(0:4, lambda), 1 - ppois(4, lambda))
names(p_cat) <- c("0","1","2","3","4",">4")

set.seed(28092025)
n <- 500
samp <- sample(hits_per_cell, size = n, replace = TRUE)

tab6 <- function(v) {
  t <- tabulate(pmin(v, 5) + 1, nbins = 6)
  names(t) <- c("0","1","2","3","4",">4")
  t
}
obs <- tab6(samp)
exp <- n * p_cat

chisq <- sum((obs - exp)^2 / exp)
df <- 5
pval <- pchisq(chisq, df, lower.tail = FALSE)

out <- data.frame(
  `V-1s in square (k)` = c(names(obs), "Total"),
  `Expected (Poisson)` = c(round(exp, 2), sum(exp)),
  `Observed`           = c(as.integer(obs), sum(obs)),
  check.names = FALSE
)

print(out, row.names = FALSE)
cat(sprintf("\nlambda = %.3f, chi^2 = %.2f, df = %d, p = %.4f\n",
            lambda, chisq, df, pval))

