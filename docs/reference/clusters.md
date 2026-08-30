# Three clusters in 5D

This data is simulated to use for testing. It has three elliptical
clusters in mostly variables 2 and 4. They are not equidistant.

## Format

A dataset with 300 rows and 6 columns

## Details

- x1, x2, x3, x4, x5:

  five numeric variables

- cl:

  class variable

## See also

simple_clusters

## Examples

``` r

clusters_pca <- prcomp(clusters[,1:5])
ggscree(clusters_pca)
```
