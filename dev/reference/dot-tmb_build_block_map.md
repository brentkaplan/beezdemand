# Build the block-structure metadata vectors for the Phase-2 TMB template

Emits the integer vectors that tell the C++ template how to slice the
parameter vector into per-block covariance components and how to map
subject-level random effects through each block's Cholesky.

## Usage

``` r
.tmb_build_block_map(re_parsed)
```

## Arguments

- re_parsed:

  Output of `.normalize_re_input()`.

## Value

A list of integer scalars and integer vectors describing the block
structure (see Conventions above).

## Details

Conventions:

- `block_types[b] = 0` for pdDiag, `1` for pdSymm.

- `block_q0_offset[b]` / `block_alpha_offset[b]` are 0-indexed starting
  columns within `Z_q0` / `Z_alpha` respectively.

- `n_logsigma` is `sum(block_q0_dim + block_alpha_dim)` – one
  standard-deviation parameter per RE column across all blocks.

- `n_rho` is `sum(over pdSymm blocks: block_dim*(block_dim-1)/2)` where
  `block_dim = block_q0_dim + block_alpha_dim`. pdDiag blocks contribute
  0 free correlations.
