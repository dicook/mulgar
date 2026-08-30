# Produces an mclust summary plot with ggplot

Takes data returned by `mclustBIC()`, converts to a tibble for plotting.

## Usage

``` r
ggmcbic(mc, cl = 1:nrow(mc), top = ncol(mc))
```

## Arguments

- mc:

  mclustBIC object

- cl:

  subset of clusters to show

- top:

  number to indicate how many models to show, default "all"

## Value

mc_bic a ggplot object

## Examples

``` r
require(mclust)
#> Loading required package: mclust
#> Warning: package ‘mclust’ was built under R version 4.6.1
#> Package 'mclust' version 6.1.3
#> Type 'citation("mclust")' for citing this R package in publications.
#> 
#> Attaching package: ‘mclust’
#> The following object is masked from ‘package:dplyr’:
#> 
#>     count
data(clusters)
clusters_BIC <- mclustBIC(clusters[,1:5], G=2:6)
ggmcbic(clusters_BIC)
#> Warning: This manual palette can handle a maximum of 13 values. You have supplied 14
#> Warning: Removed 5 rows containing missing values or values outside the scale range
#> (`geom_line()`).

ggmcbic(clusters_BIC, top=4)


data(simple_clusters)
clusters_BIC <- mclustBIC(simple_clusters[,1:2])
ggmcbic(clusters_BIC, cl=2:5, top=3)
#> Warning: Removed 1 row containing missing values or values outside the scale range
#> (`geom_line()`).
```
