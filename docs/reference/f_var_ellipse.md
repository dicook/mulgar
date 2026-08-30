# Ellipse matching data center and variance

This function generates points on the surface of an ellipse with the
same center and variance-covariance of the provided data.

## Usage

``` r
f_var_ellipse(x, n = 100)
```

## Arguments

- x:

  multivariate data set.

- n:

  number of points to generate

## Value

matrix of size n x p

## Details

This is useful for checking the equal variance-covariance assumption
from linear discriminant analysis.

## Examples

``` r
cat("make an example\n")
#> make an example
```
