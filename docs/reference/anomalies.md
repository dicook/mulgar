# Data sets with anomalies

Simulated data with anomalies

## Usage

``` r
anomaly1

anomaly2

anomaly3

anomaly4

anomaly5
```

## Format

A datasets with anomalies

## Source

Created by Di Cook.

## Details

- x1, x2, x3, x4:

  numeric variables

## Examples

``` r
require(GGally)
#> Loading required package: GGally
#> Loading required package: ggplot2
data(anomaly1)
data(anomaly2)
ggscatmat(anomaly1)

ggscatmat(anomaly2)
```
