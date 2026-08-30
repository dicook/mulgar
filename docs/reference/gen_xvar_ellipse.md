# Ellipse matching data center and variance

This function generates points on the surface of an ellipse with the
same center and variance-covariance of the provided data.

## Usage

``` r
gen_xvar_ellipse(x, n = 100, nstd = 1)
```

## Arguments

- x:

  multivariate data set.

- n:

  number of points to generate

- nstd:

  scale factor for size of ellipse, in terms of number of standard
  deviations

## Value

matrix of size n x p

## Details

This is useful for checking the equal variance-covariance assumption
from linear discriminant analysis.

## Examples

``` r
data(aflw)
aflw_vc <- gen_xvar_ellipse(aflw[,c("goals","behinds",
                               "kicks","disposals")], n=500)
require(ggplot2)
ggplot(aflw_vc, aes(x=goals, y=behinds)) + geom_point() +
  theme(aspect.ratio=1)

if (interactive()) {
  require(tourr)
  animate_slice(aflw_vc, rescale=TRUE, v_rel=0.02)
  aflw_all <- rbind(aflw_vc, aflw[,c("goals","behinds",
                               "kicks","disposals")])
  clrs <- c(rep("orange", 500), rep("black", nrow(aflw)))
  animate_xy(aflw_all, col=clrs)
}
```
