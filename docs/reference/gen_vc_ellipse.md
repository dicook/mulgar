# Generate points on the surface of an ellipse

This function generates points by transforming points on the surface of
a sphere.

## Usage

``` r
gen_vc_ellipse(vc, xm = rep(0, ncol(vc)), n = 500)
```

## Arguments

- vc:

  symmetric square matrix describing the variance-covariance matrix
  which defines the shape of the ellipse.

- xm:

  center of the ellipse, a vector of length equal to the dimension of vc

- n:

  number of points to generate

## Value

matrix of size n x p

## Examples

``` r
require(ggplot2)
require(tibble)
ell2d <- gen_vc_ellipse(vc = matrix(c(4, 2, 2, 6),
                        ncol=2, byrow=TRUE),
                        xm = c(1,1))
ell2d <- as_tibble(ell2d)
ggplot(ell2d, aes(x = V1, y = V2)) + geom_point() +
  theme(aspect.ratio=1)
```
