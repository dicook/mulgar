# Computes the ellipses of an mclust model

Takes data returned by
[`Mclust()`](https://mclust-org.github.io/mclust/reference/Mclust.html),
extracts parameter estimates, and computes points on ellipses.

## Usage

``` r
mc_ellipse(mc, npts = 100)
```

## Arguments

- mc:

  Mclust object

- npts:

  Number of points to simulate for each cluster, default 100

## Value

mc_ellipses data frame

## Examples

``` r
require(mclust)
data(simple_clusters)
clusters_mc <- Mclust(simple_clusters[,1:2],
                     G=2,
                     modelname="EEI")
mce <- mc_ellipse(clusters_mc, npts=400)
require(ggplot2)
sc <- simple_clusters
sc$cl <- factor(clusters_mc$classification)
ggplot() +
  geom_point(data=sc, aes(x=x1, y=x2, colour=cl)) +
  geom_point(data=mce$ell, aes(x=x1, y=x2, colour=cl), shape=4) +
  geom_point(data=mce$mn, aes(x=x1, y=x2, colour=cl), shape=3) +
  theme(aspect.ratio=1, legend.position="none")
```
