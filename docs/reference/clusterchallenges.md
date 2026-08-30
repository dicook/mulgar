# Cluster challenge data sets

Simulated data with different structures

## Usage

``` r
c1

c2

c3

c4

c5

c6

c7
```

## Format

A datasets with differing number of rows and columns

## Source

Created by Di Cook.

## Details

- x1, x2, ... :

  numeric variables

## Examples

``` r
require(ggplot2)
data(c1)
ggplot(c1, aes(x=x1, y=x2)) +
  geom_point() + theme(aspect.ratio=1)
```
