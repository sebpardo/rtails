# rtails: Simulate heavy-tailed deviations

## Installation

``` r
install.packages("remotes")
remotes::install_github("sebpardo/rtails")
```

## Introduction to `rtails`

The `rtails` package provides functions to easily obtain
heavy-tailed multiplicative random deviates (i.e., positive only, centered around 1).
While its primary aim is to simulate heavy-tailed recruitment deviations, these
functions could also be used for other applications as well.

The heavy-tailed random deviates are generated using four distributions: 

- Normal mixture (i.e. normal with sporadic wider normal draws): `rnorm_tails()`
- Pareto: `rpareto_tails()`
- Student-t: `rst_tails()`
- Generalized Extreme Variance: `rgev_tails()`

Where possible, functions are analytically bias-corrected around a mean of 1 
by default. However, this correction is not always applicable
(e.g. Student-t and GEV). We therefore also allow all functions to 
be bias corrected using the drawn sample mean (`sample_bias_correct`). 
All functions also take a `seed` argument so that different parameterizations 
of each distribution using a given seed can be more directly comparable.

We also provide the `mutate_tails()` function that converts an existing set
of normal random deviates into deviates with a Student-t or Pareto distribution, 
and allows for bootstrapping.

Lastly, the `plot_tails()` function allows for easy visualization of the 
generated random deviates.

See the vignette for more details.
