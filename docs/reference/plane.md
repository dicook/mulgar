# 2D plane in 5D

This data is simulated to use for testing. It has two dimensions of
variability and three of noise. It is created from a 2 factor model,
where all variables are related.

## Format

A data set with 100 rows and 5 columns

## Details

- x1, x2, x3, x4, x5:

  five numeric variables

## See also

box

## Examples

``` r

plane_pca <- prcomp(plane)
ggscree(plane_pca)
```
