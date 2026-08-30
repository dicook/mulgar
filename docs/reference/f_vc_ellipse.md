# Generate points on the surface of an ellipse

This function generates points by transforming points on the surface of
a sphere.

## Usage

``` r
f_vc_ellipse(vc, xm, n = 500)
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
ell2d <- f_vc_ellipse(vc = matrix(c(4, 2, 2, 6), ncol=2, byrow=T), xm = c(1,1))
#> Error in sphere.hollow(p, n): could not find function "sphere.hollow"
ell2d <- as_tibble(ell2d)
#> Error in as_tibble(ell2d): object 'ell2d' not found
ggplot(ell2d, aes(x = V1, y = V2)) + geom_point()
#> Error in ggplot(ell2d, aes(x = V1, y = V2)): object 'ell2d' not found
```
