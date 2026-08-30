# Four unusually shaped clusters in 4D

This data is simulated to use for testing. It has two small spherical
clusters, and a curve cluster and a sine wave cluster.

## Format

A dataset with 300 rows and 6 columns

## Details

- x1, x2, x3, x4:

  five numeric variables

## See also

clusters

## Examples

``` r

require(ggplot2)
ggplot(clusters_nonlin, aes(x=x1, y=x2)) +
  geom_point() +
  theme(aspect.ratio=1)
```
