# Generate a sample from a multivariate normal

This function generates a sample of size n from a multivariate normal
distribution

## Usage

``` r
f_gen_mvn(n = 100, p = 5, mn = rep(0, p), vc = diag(rep(1, p)))
```

## Arguments

- n:

  number of points to generate

- p:

  dimension

- mn:

  mean of the distribution, a vector of length equal to the dimension of
  vc

- vc:

  symmetric square matrix describing the variance-covariance matrix
  which defines the shape of the ellipse.

## Value

matrix of size n x p

## Examples

``` r
require(ggplot2)
#> Loading required package: ggplot2
require(tibble)
#> Loading required package: tibble
d <- f_gen_mvn(n=100, p=2, mn = c(1,1), vc = matrix(c(4, 2, 2, 6), ncol=2, byrow=T))
d <- as_tibble(d)
ggplot(d, aes(x = V1, y = V2)) + geom_point()
```
