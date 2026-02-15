# Lambert W

Ben Bolker's port of Lambert W from GNU Scientific Library (GPLV3)

## Usage

``` r
lambertW(z, b = 0, maxiter = 10, eps = .Machine$double.eps, min.imag = 1e-09)
```

## Arguments

- z:

  input value

- b:

  branch, set to principal by default

- maxiter:

  Halley iteration count

- eps:

  error precision

- min.imag:

  minimum for imaginary solution

## Value

numeric

## Details

Ben Bolker's port of Lambert W from GNU Scientific Library

## Author

Benjamin Bolker (port)
