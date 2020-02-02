
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

# frankenply <img src='man/figures/logo.png' align="right" height="139" />

> “Learn from me, if not by my precepts, at least by my example, how
> dangerous is the acquirement of knowledge, and how much happier that
> man is who believes his native town to be his world, than he who
> aspires to become greater than his nature will allow.” ― Mary Shelley,
> Frankenstein

Install with:

``` r
remotes::install_github("moodymudskipper/frankenply")
```

It can be seen as a replacement to apply functions:

``` r
library(frankenply, warn.conflicts = FALSE)
nrow(?~~list(iris, cars))
#> [1] 150  50
```

And is especially intuitive for rowwise operations, let’s see with
sample data :

``` r
library(dplyr, warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE)
#> Warning: package 'dplyr' was built under R version 3.6.2
data <- 
  mtcars %>%
  group_by(vs) %>%
  summarize(disp_hp = list(tibble(disp, hp)),
            drat_wt = list(tibble(drat, wt)))
data
#> # A tibble: 2 x 3
#>      vs disp_hp           drat_wt          
#>   <dbl> <list>            <list>           
#> 1     0 <tibble [18 x 2]> <tibble [18 x 2]>
#> 2     1 <tibble [14 x 2]> <tibble [14 x 2]>
```

Then unleash Franky:

``` r
data %>% 
  transmute( 
    nrow_disp_hp    = nrow(?~~disp_hp),  
    disp_hp_head    = head(?~disp_hp, 1), 
    disp_hp_drat_wt = cbind(?~disp_hp, ?~drat_wt)
    )
#> # A tibble: 2 x 3
#>   nrow_disp_hp disp_hp_head     disp_hp_drat_wt  
#>          <int> <list>           <list>           
#> 1           18 <tibble [1 x 2]> <df[,4] [18 x 4]>
#> 2           14 <tibble [1 x 2]> <df[,4] [14 x 4]>
```

`?~~` on first `transmute()` instruction wraps `mapply()`, so outputs
atomic columns whenever possible, integer here.

`?~~` on other `transmute()` instructions wraps `Map()`, so outputs list
columns.

It’s a monster because :

  - It modifies the call of the function that calls `?`, which is a
    monstruous thing to do.
  - It’s full of problems, won’t work for primitives and some other
    functions (`lm()` and `dplyr::bind_cols()` break it for instance).
  - To solve these issues I might need to be greater than my nature will
    allow.
