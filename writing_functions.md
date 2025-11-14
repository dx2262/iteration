writing_functions
================

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   4.0.0     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.4     ✔ tidyr     1.3.1
    ## ✔ purrr     1.0.4     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(readxl)
library(rvest)
```

    ## 
    ## Attaching package: 'rvest'
    ## 
    ## The following object is masked from 'package:readr':
    ## 
    ##     guess_encoding

## Start small!!

Everyone loves z scores.

``` r
x_vec <- rnorm(20, mean = 10, sd = 3.5)

(x_vec - mean(x_vec)) / sd(x_vec)
```

    ##  [1]  1.83424873 -0.36796092  0.64639490 -0.37474279 -0.85792246  0.35618353
    ##  [7]  0.20878586  0.12525968 -0.54724603  0.62881425  0.97052007 -1.82896512
    ## [13] -0.34225535 -0.22137072 -0.77506225  0.20398517 -0.65534019  2.39967486
    ## [19]  0.02554783 -1.42854906

Write a function to compute z scores.

``` r
z_scores <- function(x) {
  
  if (!is.numeric(x)) {
    stop("The input x should be numeric.")
  }
  
  if (length(x) < 5) {
    stop("Only compute z scores when the input has 5 or more numbers.")
  }
  
  z = (x - mean(x)) / sd(x)
  
  z
}
```

Let’s try our function….

``` r
z_scores(x = x_vec)
```

    ##  [1]  1.83424873 -0.36796092  0.64639490 -0.37474279 -0.85792246  0.35618353
    ##  [7]  0.20878586  0.12525968 -0.54724603  0.62881425  0.97052007 -1.82896512
    ## [13] -0.34225535 -0.22137072 -0.77506225  0.20398517 -0.65534019  2.39967486
    ## [19]  0.02554783 -1.42854906

``` r
num_vec <- rnorm(123, mean = 14, sd = 0.4)

z_scores(x = num_vec)
```

    ##   [1]  1.57573874  0.97850189 -1.82110522  2.52313409 -0.35525903 -0.02963890
    ##   [7] -0.90071922  0.09123988  0.70636033  0.48108088  0.21415597  0.17927139
    ##  [13] -0.39700766 -0.09402497 -0.06461264  1.54339635 -1.26441304  1.07968102
    ##  [19]  2.29252908  0.45477210 -1.49162302  0.63154650  0.68344233  0.83523635
    ##  [25] -1.15365949 -1.18628602  0.10599584  0.89495776  1.01874471 -1.85619678
    ##  [31]  1.19667140 -0.10171543 -0.62375692 -0.72666111 -0.34974454 -0.62164232
    ##  [37]  1.41324447  0.27107140  0.78794727  1.55303570 -1.07854761 -2.89141401
    ##  [43]  1.78286476  1.11823286  0.04681255 -0.47035366  0.24834047  0.13700001
    ##  [49]  0.49017353  0.75178176  1.04925936 -0.23275918  1.02881238 -0.51326236
    ##  [55] -0.62063875 -0.35787413 -0.37091101 -0.21712666  0.45513397 -0.90544554
    ##  [61]  1.47098801  0.34181704 -0.45484639 -0.14387725 -0.23973580 -1.90893704
    ##  [67] -1.04534379 -1.12952138 -0.06800072 -0.54354438  0.84381785  0.99760762
    ##  [73]  1.44105379 -0.25624619 -0.07616739 -0.33443353 -1.48240937  0.17730563
    ##  [79]  0.51953753 -0.91462650 -0.47278038 -1.50173398 -0.45627407 -0.55856559
    ##  [85] -1.67674992 -0.77887010  0.26569000  0.59303365  1.53790594  0.31784200
    ##  [91] -0.77350684  0.19732509 -0.41918697  0.62106752  0.42157984  0.92155208
    ##  [97]  0.59469561 -1.56001140 -0.38024079  1.47052446  0.19029509  0.68306781
    ## [103]  2.10854052  0.69285283 -0.08350916  1.00160008 -0.58222642 -0.88898431
    ## [109] -2.17102357 -1.09411312  0.98386192 -1.70360593  0.56144683 -1.12941604
    ## [115] -1.33496854  0.56088623 -0.65354107 -0.05140445  0.70384930 -0.05652085
    ## [121] -1.56569351  0.20002754  0.14707705

Let’s break our function…

``` r
z_scores(3)
```

    ## Error in z_scores(3): Only compute z scores when the input has 5 or more numbers.

``` r
z_scores("my name is jeff")
```

    ## Error in z_scores("my name is jeff"): The input x should be numeric.

## Let’s compute some stuff….

``` r
mean_and_sd <- function(x) {
  
  
  if (!is.numeric(x)) {
    stop("The input x should be numeric.")
  }
  
  if (length(x) < 5) {
    stop("Only compute z scores when the input has 5 or more numbers.")
  }
  
  mean_x = mean(x, na.rm = TRUE)
  sd_x = sd(x, na.rm = TRUE)
  
  c(mean_x, sd_x)
  
  tibble(
    mean = mean_x,
    sd = sd_x
  )
  
  
}

mean_and_sd(x = x_vec)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  10.2  4.21

## Make up data..

Let’s *simulate* some data..

``` r
sim_df <- 
  tibble(
    x = rnorm(n = 30, mean = 3, sd = 2)
  )

sim_df %>% 
  summarize(
    mu_hat = mean(x),
    sigma_hat = sd(x)
  )
```

    ## # A tibble: 1 × 2
    ##   mu_hat sigma_hat
    ##    <dbl>     <dbl>
    ## 1   2.46      2.65

Write a function to do simulations.

We wrote it in a code chunk last time, now it is being sourced.

``` r
source("source/sim_mean_sd.R")
```

Let’s run this function.

``` r
sim_mean_sd()
```

    ## # A tibble: 1 × 2
    ##   mu_hat sigma_hat
    ##    <dbl>     <dbl>
    ## 1   3.95      2.04

Import the LotR data.

``` r
fellowship_ring <- 
  read_excel("data/LotR_Words.xlsx", range = "B3:D6") %>% 
  mutate(movie = "Fellowship of the Ring")

two_towers <- 
  read_excel("data/LotR_Words.xlsx", range = "F3:H6") %>% 
  mutate(movie = "Two Towers")

return_of_the_king <- 
  read_excel("data/LotR_Words.xlsx", range = "J3:L6") %>% 
  mutate(movie = "Return of the King")

lotr_df <- 
  bind_rows(fellowship_ring, two_towers, return_of_the_king)
```

Turn this into a function.

``` r
lotr_import <- function(cell_range, movie_title) {
  
  df = read_excel("data/LotR_Words.xlsx", range = cell_range) %>% 
  mutate(movie = movie_title)
  
  df
  
}
```

``` r
fellowship <- lotr_import(cell_range = "B3:D6", movie_title = "Fellowship")
two_towers <- lotr_import(cell_range = "F3:H6", movie_title = "Two Towers")
return_of_the_king <- lotr_import(cell_range = "J3:L6", movie_title = "Return of the King")

bind_rows(fellowship, two_towers, return_of_the_king)
```

    ## # A tibble: 9 × 4
    ##   Race   Female  Male movie             
    ##   <chr>   <dbl> <dbl> <chr>             
    ## 1 Elf      1229   971 Fellowship        
    ## 2 Hobbit     14  3644 Fellowship        
    ## 3 Man         0  1995 Fellowship        
    ## 4 Elf       331   513 Two Towers        
    ## 5 Hobbit      0  2463 Two Towers        
    ## 6 Man       401  3589 Two Towers        
    ## 7 Elf       183   510 Return of the King
    ## 8 Hobbit      2  2673 Return of the King
    ## 9 Man       268  2459 Return of the King
