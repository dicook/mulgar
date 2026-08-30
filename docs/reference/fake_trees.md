# A high-dimensional tree data structure with 10 branching points.

A high-dimensional tree data structure with 10 branching points.

## Usage

``` r
fake_trees
```

## Format

An object of class `data.frame` with 3000 rows and 101 columns.

## Source

[PHATE](https://github.com/KrishnaswamyLab/PHATE/blob/master/Python/phate/tree.py)

## Details

Data are obtained from diffusion limited aggregation tree simulation in
the `phate` python and `phateR` packages, but reconstructed as a wide
data.frame rather than a list.

There are 3000 rows and 101 columns, the first 100 columns are labelled
dim1 - dim100, and are numeric, while the final column is a factor
representing the branch id.
