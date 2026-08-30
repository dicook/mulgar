# Normalise a vector to have length 1

Returns the normalised vector, where the sum of squares is equal to 1

## Usage

``` r
norm_vec(x)
```

## Arguments

- x:

  numeric vector

## Value

numeric vector

## Examples

``` r
x <- rnorm(5)
norm_vec(x)
#> [1] -0.595680329 -0.708343079  0.009961467 -0.312049764  0.214337916
```
