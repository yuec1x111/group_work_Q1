# settings
cell_sides <- c("0.25 km^2 (500 m)" = 500,
                "0.49 km^2 (700 m)" = 700,
                "0.81 km^2 (900 m)" = 900)
sample_sizes <- c("n=500" = 500, "n=1000" = 1000, "n=5000" = 5000)

# table of p-values
set.seed(28092025)
pvals <- matrix(NA_real_,
                nrow = length(cell_sides),
                ncol = length(sample_sizes),
                dimnames = list(names(cell_sides), names(sample_sizes)))

for (i in seq_along(cell_sides)) {
  for (j in seq_along(sample_sizes)) {
    pvals[i, j] <- resample_pvalue(
      pts  = pts_proj,
      rect = rect,
      cell = cell_sides[i],
      n    = sample_sizes[j]
    )
  }
}

# print nicely
pval_tbl <- as.data.frame(pvals)
pval_tbl$`Area (rows)` <- rownames(pval_tbl)
pval_tbl <- pval_tbl[, c("Area (rows)", colnames(pvals))]

if (requireNamespace("knitr", quietly = TRUE)) {
  knitr::kable(
    pval_tbl,
    align = "lrrr",
    caption = "P-values for cell sizes (rows) × resample sizes (columns); seed = 28092025"
  )
} else {
  print(pval_tbl, row.names = FALSE)
}
