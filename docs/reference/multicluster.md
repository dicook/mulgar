# Multiple clusters of different sizes, shapes and distance from each other

This data is originally from
http://ifs.tuwien.ac.at/dm/download/multiChallenge-matrix.txt, and
provided as a challenge for non-linear dimension reduction.It was used
as an example in Lee, Laa, Cook (2023)
https://doi.org/10.52933/jdssv.v2i3.

## Format

A dataset with 400 rows and 11 columns

## Details

- group:

  cluster label

- x1, ... x10:

  numeric variables

## See also

clusters

## Examples

``` r

require(ggplot2)
ggplot(multicluster, aes(x=x1, y=x2)) +
  geom_point() + theme(aspect.ratio=1)
```
