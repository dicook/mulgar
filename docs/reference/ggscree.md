# This function produces a simple scree plot

Takes a PCA object returned by
[`prcomp()`](https://rdrr.io/r/stats/prcomp.html), extracts the standard
deviations of the principal components (PC), and plots these against the
PC number. The guidance line assumes that all of the variables have been
standardised prior to PCA.

## Usage

``` r
ggscree(pc, q = 2, guide = TRUE, cumulative = FALSE)
```

## Arguments

- pc:

  PCA object

- q:

  number of principal components to show, default 2 (you should change)

- guide:

  logical whether to compute and add a typical value of the variance, if
  the data was full-dimensional

- cumulative:

  logical whether to draw cumulative variance

## Value

scree a ggplot object

## Examples

``` r
data(aflw)
aflw_std <- apply(aflw[,7:35], 2, function(x)
                    (x-mean(x, na.rm=TRUE))/
                     sd(x, na.rm=TRUE))
aflw_pca <- prcomp(aflw_std[,c("goals","behinds",
                               "kicks","disposals")])
ggscree(aflw_pca, q=3)
```
