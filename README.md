README
================
Chenyi Lin
2/25/2021

``` r
library(tidyverse)
```

    ## ─ Attaching packages ──────────────────── tidyverse 1.3.0 ─

    ## ✓ ggplot2 3.3.2     ✓ purrr   0.3.4
    ## ✓ tibble  3.0.4     ✓ dplyr   1.0.4
    ## ✓ tidyr   1.1.2     ✓ stringr 1.4.0
    ## ✓ readr   1.4.0     ✓ forcats 0.5.1

    ## ─ Conflicts ───────────────────── tidyverse_conflicts() ─
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(nycflights13)
```

``` r
flights %>% group_by(tailnum)%>%summarise(arr_delay_mean=mean(arr_delay,na.rm = TRUE)) %>%left_join(planes) %>% select(tailnum:year)
```

    ## Joining, by = "tailnum"

    ## # A tibble: 4,044 x 3
    ##    tailnum arr_delay_mean  year
    ##    <chr>            <dbl> <int>
    ##  1 D942DN          31.5      NA
    ##  2 N0EGMQ           9.98     NA
    ##  3 N10156          12.7    2004
    ##  4 N102UW           2.94   1998
    ##  5 N103US          -6.93   1999
    ##  6 N104UW           1.80   1999
    ##  7 N10575          20.7    2002
    ##  8 N105UW          -0.267  1999
    ##  9 N107US          -5.73   1999
    ## 10 N108UW          -1.25   1999
    ## # … with 4,034 more rows

``` r
##do not want na
```

``` r
flights %>% group_by(origin, month, day)%>%
  summarise(deps = n())%>%
  pivot_wider(names_from = origin,values_from = deps)
```

    ## `summarise()` has grouped output by 'origin', 'month'. You can override using the `.groups` argument.

    ## # A tibble: 365 x 5
    ## # Groups:   month [12]
    ##    month   day   EWR   JFK   LGA
    ##    <int> <int> <int> <int> <int>
    ##  1     1     1   305   297   240
    ##  2     1     2   350   321   272
    ##  3     1     3   336   318   260
    ##  4     1     4   339   318   258
    ##  5     1     5   238   302   180
    ##  6     1     6   301   307   224
    ##  7     1     7   342   307   284
    ##  8     1     8   334   288   277
    ##  9     1     9   336   288   278
    ## 10     1    10   344   306   282
    ## # … with 355 more rows

``` r
flights%>%
  mutate(date =as.Date(time_hour))%>%
  group_by(origin,date)%>%
  summarise(deps = n())%>%
  pivot_wider(names_from = origin,values_from = deps)
```

    ## `summarise()` has grouped output by 'origin'. You can override using the `.groups` argument.

    ## # A tibble: 366 x 4
    ##    date         EWR   JFK   LGA
    ##    <date>     <int> <int> <int>
    ##  1 2013-01-01   255   236   218
    ##  2 2013-01-02   351   319   260
    ##  3 2013-01-03   336   320   261
    ##  4 2013-01-04   340   319   258
    ##  5 2013-01-05   262   303   203
    ##  6 2013-01-06   272   309   203
    ##  7 2013-01-07   348   307   277
    ##  8 2013-01-08   336   291   276
    ##  9 2013-01-09   336   289   279
    ## 10 2013-01-10   341   302   282
    ## # … with 356 more rows

``` r
flights%>%
  mutate(date =as.Date(time_hour))%>%
  group_by(date,dest)%>%
  summarise(deps = n())%>%
  pivot_wider(names_from = dest,values_from = deps)
```

    ## `summarise()` has grouped output by 'date'. You can override using the `.groups` argument.

    ## # A tibble: 366 x 106
    ## # Groups:   date [366]
    ##    date         ALB   ATL   AUS   AVL   BDL   BNA   BOS   BQN   BTV   BUF   BUR
    ##    <date>     <int> <int> <int> <int> <int> <int> <int> <int> <int> <int> <int>
    ##  1 2013-01-01     2    38     4     1     1     8    20     1     5    12     2
    ##  2 2013-01-02     3    48     7     1     2    11    28     3     9    15     2
    ##  3 2013-01-03     2    50     7    NA     2    11    27     3     6    14     2
    ##  4 2013-01-04     2    48     6    NA     1    11    30     3     7    13     2
    ##  5 2013-01-05     2    36     4    NA     2    10    23     3     8    15     2
    ##  6 2013-01-06     2    40     5    NA     1    11    26     3     7    12     2
    ##  7 2013-01-07     2    49     6    NA     1    14    44     3     7    14     1
    ##  8 2013-01-08     2    47     5    NA     1    14    48     3     7    14     1
    ##  9 2013-01-09     2    47     6    NA     1    14    49     3     7    13     1
    ## 10 2013-01-10     2    48     6    NA     1    14    48     3     8    13     1
    ## # … with 356 more rows, and 94 more variables: BWI <int>, CAK <int>,
    ## #   CHS <int>, CLE <int>, CLT <int>, CMH <int>, CRW <int>, CVG <int>,
    ## #   DAY <int>, DCA <int>, DEN <int>, DFW <int>, DTW <int>, EGE <int>,
    ## #   FLL <int>, GRR <int>, GSO <int>, GSP <int>, HNL <int>, HOU <int>,
    ## #   IAD <int>, IAH <int>, IND <int>, JAC <int>, JAX <int>, LAS <int>,
    ## #   LAX <int>, LGB <int>, MCI <int>, MCO <int>, MDW <int>, MEM <int>,
    ## #   MHT <int>, MIA <int>, MKE <int>, MSN <int>, MSP <int>, MSY <int>,
    ## #   MYR <int>, OAK <int>, OMA <int>, ORD <int>, ORF <int>, PBI <int>,
    ## #   PDX <int>, PHL <int>, PHX <int>, PIT <int>, PWM <int>, RDU <int>,
    ## #   RIC <int>, ROC <int>, RSW <int>, SAN <int>, SAT <int>, SAV <int>,
    ## #   SDF <int>, SEA <int>, SFO <int>, SJC <int>, SJU <int>, SLC <int>,
    ## #   SMF <int>, SNA <int>, SRQ <int>, STL <int>, STT <int>, SYR <int>,
    ## #   TPA <int>, XNA <int>, DSM <int>, OKC <int>, PSE <int>, PVD <int>,
    ## #   TUL <int>, TYS <int>, BHM <int>, CAE <int>, BZN <int>, EYW <int>,
    ## #   HDN <int>, MTJ <int>, PSP <int>, BGR <int>, CHO <int>, ABQ <int>,
    ## #   ACK <int>, MVY <int>, TVC <int>, ANC <int>, LGA <int>, SBN <int>,
    ## #   ILM <int>, LEX <int>

\#\#Na means 0. because there was no fllights on that day.
