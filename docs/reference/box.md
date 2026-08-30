# 3D plane in 5D

This data is simulated to use for testing. It has three dimensions of
variability and two of noise. It is created from a 3 factor model. All
variables are linearly associated.

## Format

A dataset with 200 rows and 5 columns

## Details

- x1, x2, x3, x4, x5:

  five numeric variables

## See also

plane

## Examples

``` r

box_pca <- prcomp(box)
ggscree(box_pca)
```
