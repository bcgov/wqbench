# check ssdtools functions directly

    Code
      output
    Output
      # A tibble: 4 x 15
        dist    proportion   est    se   lcl   ucl    wt level est_method ci_method   
        <chr>        <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <chr>      <chr>       
      1 average       0.01 0.533 0.236 0.386  1.10     1  0.95 multi      weighted_sa~
      2 average       0.05 0.869 0.222 0.676  1.36     1  0.95 multi      weighted_sa~
      3 average       0.1  1.07  0.224 0.855  1.54     1  0.95 multi      weighted_sa~
      4 average       0.2  1.35  0.238 1.12   1.81     1  0.95 multi      weighted_sa~
      # i 5 more variables: boot_method <chr>, nboot <dbl>, pboot <dbl>,
      #   dists <list>, samples <list>

---

    Code
      output
    Output
      # A tibble: 5 x 15
        dist    proportion   est    se   lcl   ucl    wt level est_method ci_method 
        <chr>        <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <chr>      <chr>     
      1 gamma         0.05 0.951 0.336 0.623  1.91 0.237  0.95 cdf        percentile
      2 lgumbel       0.05 1.03  0.239 0.707  1.75 0.136  0.95 cdf        percentile
      3 llogis        0.05 0.972 0.342 0.479  1.85 0.176  0.95 cdf        percentile
      4 lnorm         0.05 1.00  0.296 0.680  1.88 0.219  0.95 cdf        percentile
      5 weibull       0.05 0.834 0.403 0.354  1.75 0.233  0.95 cdf        percentile
      # i 5 more variables: boot_method <chr>, nboot <dbl>, pboot <dbl>,
      #   dists <list>, samples <list>

