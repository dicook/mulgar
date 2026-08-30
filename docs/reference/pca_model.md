# Create wire frame of PCA model

This function takes the PCA and produces a wire frame of the PCA to
examine with the data in a tour. The purpose is to see how well the
variance is explained. The model will be centered at the mean, and
extend 3 SDs towards the edge of the data, which is assuming that the
data is standardised.

## Usage

``` r
pca_model(pc, d = 2, s = 1)
```

## Arguments

- pc:

  PCA object

- d:

  number of dimensions to use, default=2

- s:

  scale model, default=1

## Value

a list of points and edges

## Examples

``` r
data(plane)
plane_pca <- prcomp(plane)
plane_m <- pca_model(plane_pca)
plane_m_d <- rbind(plane_m$points, plane)
if (interactive()) {
  require(tourr)
  animate_xy(plane_m_d, edges=plane_m$edges, axes="bottomleft")
}
```
