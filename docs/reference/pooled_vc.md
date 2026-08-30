# Compute pooled variance-covariance matrix

This function computes the group variance-covariance matrices, and
produces a weighted average. It is useful for examining the linear
discriminant analysis model.

## Usage

``` r
pooled_vc(x, cl, prior = rep(1/length(unique(cl)), length(unique(cl))))
```

## Arguments

- x:

  multivariate data set, matrix.

- cl:

  class variable

- prior:

  prior probability for each class, must sum to 1, default all equal

## Value

matrix

## Examples

``` r
data(clusters)
pooled_vc(clusters[,1:5], clusters$cl)
#>             x1           x2          x3           x4           x5
#> x1  0.99999333 -0.011761538  0.03090888  0.039326651  0.020667963
#> x2 -0.01176154  0.065410181 -0.01087632  0.001238853  0.007879633
#> x3  0.03090888 -0.010876321  0.48526224 -0.012003440  0.022479828
#> x4  0.03932665  0.001238853 -0.01200344  0.178607342 -0.012844319
#> x5  0.02066796  0.007879633  0.02247983 -0.012844319  0.513469244
```
