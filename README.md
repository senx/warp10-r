
<!-- README.md is generated from README.Rmd. Please edit that file -->

# warp10r

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

R client for executing WarpScript on a Warp 10 instance.

## Fork

The original package \[<https://github.com/senx/warp10-r>\] has been
forked to make the package more concilient with current developments of
R packages :

  - Package has been moved to the root of the git repository
  - Dependancies have been added to the DESCRIPTION
  - Construct a warp 10 script with helpers function and send them to
    Warp 10 database

## Installation

``` r
remotes::install_github("centreon/warp10-r")
```

## First steps

### Hello World

``` r
library(warp10r)
library(magrittr)

# Create a connection
con <- wrp_connect(endpoint = "https://warp.senx.io/api/v0/exec")

# set_script store a script in the connection object and print the script as it is.
set_script(con, "'Hello World' NOW")

# Execute the script
wrp_exec(con)
#>  Status: 200
#> [1] "1571219200995892" "Hello World"
```

### Example with Geo Time Series

``` r
library(tibble)

df1 <- tibble(ds = 1:10, y = rnorm(10))
df2 <- tibble(ds = 2:11, y = rnorm(10))

con %>% 
  clear_script() %>% 
  wrp_new_gts() %>% 
  wrp_rename("randGTS") %>% 
  wrp_add_value_df(df1, tick = ds, value = y) %>% 
  wrp_new_gts() %>% 
  wrp_add_value_df(df2, tick = ds, value = y) %>% 
  wrp_rename("nogeoTS") %>% 
  wrp_exec()
#>  Status: 200
#> # A GTS object: 20 x 3
#>    timestamp            value class  
#>  * <dttm>               <dbl> <chr>  
#>  1 1970-01-01 00:00:00 -0.701 nogeoTS
#>  2 1970-01-01 00:00:00  0.356 nogeoTS
#>  3 1970-01-01 00:00:00 -0.428 nogeoTS
#>  4 1970-01-01 00:00:00 -0.216 nogeoTS
#>  5 1970-01-01 00:00:00  0.251 nogeoTS
#>  6 1970-01-01 00:00:00 -0.513 nogeoTS
#>  7 1970-01-01 00:00:00 -0.706 nogeoTS
#>  8 1970-01-01 00:00:00 -0.391 nogeoTS
#>  9 1970-01-01 00:00:00 -1.42  nogeoTS
#> 10 1970-01-01 00:00:00 -0.869 nogeoTS
#> 11 1970-01-01 00:00:00 -1.64  randGTS
#> 12 1970-01-01 00:00:00  1.35  randGTS
#> 13 1970-01-01 00:00:00  0.380 randGTS
#> 14 1970-01-01 00:00:00 -1.97  randGTS
#> 15 1970-01-01 00:00:00  0.117 randGTS
#> 16 1970-01-01 00:00:00  0.589 randGTS
#> 17 1970-01-01 00:00:00 -0.384 randGTS
#> 18 1970-01-01 00:00:00  0.727 randGTS
#> 19 1970-01-01 00:00:00  0.241 randGTS
#> 20 1970-01-01 00:00:00  0.983 randGTS
```
