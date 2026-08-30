# Non-linear relationship in 5D

This data is simulated to use for testing. It has three dimensions of
variability and two of noise. It is created from a 2 factor non-linear
model. All variables are associated.

## Format

A dataset with 100 rows and 5 columns

## Details

- x1, x2, x3, x4, x5:

  five numeric variables

## See also

plane, box

## Examples

``` r

plane_nonlin_pca <- prcomp(plane_nonlin)
ggscree(plane_nonlin_pca)
```
