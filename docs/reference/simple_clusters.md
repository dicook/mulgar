# Two clusters in 2D

This data is simulated to use for testing. It has two spherical
clusters, and two variables.

## Format

A dataset with 137 rows and 3 columns

## Details

- x1, x2:

  two numeric variables

- cl:

  class variable

## See also

clusters

## Examples

``` r

require(ggplot2)
ggplot(simple_clusters, aes(x=x1, y=x2)) +
  geom_point() + theme(aspect.ratio=1)
```
