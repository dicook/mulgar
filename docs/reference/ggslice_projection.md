# Generate slice display

Generate slice display

## Usage

``` r
ggslice_projection(data, h, proj, center = NULL, col = NULL)
```

## Arguments

- data:

  data frame containing only variables used for the display

- h:

  slice thickness

- proj:

  projection matrix from p to 2 dimensions

- center:

  center point vector used for anchoring the slice, if NULL the mean of
  the data is used

- col:

  grouping vector mapped to color in the display

## Value

ggplot2 object showing the sliced data

## See also

ggslice

## Examples

``` r
d <- geozoo::sphere.hollow(4, 1000)$points
ggslice_projection(d, 0.3, tourr::basis_random(4))

ggslice_projection(d, 0.3, tourr::basis_random(4),
                   center = c(0.4, 0.4, 0.4, 0.4))
```
