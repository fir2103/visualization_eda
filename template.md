Visualization Part I
================

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
    ## ✔ ggplot2 3.3.6      ✔ purrr   0.3.4 
    ## ✔ tibble  3.1.8      ✔ dplyr   1.0.10
    ## ✔ tidyr   1.2.0      ✔ stringr 1.4.1 
    ## ✔ readr   2.1.2      ✔ forcats 0.5.2 
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
weather_df = 
  rnoaa::meteo_pull_monitors(
    c("USW00094728", "USC00519397", "USS0023B17S"),
    var = c("PRCP", "TMIN", "TMAX"), 
    date_min = "2017-01-01",
    date_max = "2017-12-31") %>%
  mutate(
    name = recode(
      id, 
      USW00094728 = "CentralPark_NY", 
      USC00519397 = "Waikiki_HA",
      USS0023B17S = "Waterhole_WA"),
    tmin = tmin / 10,
    tmax = tmax / 10) %>%
  select(name, id, everything())
```

    ## Registered S3 method overwritten by 'hoardr':
    ##   method           from
    ##   print.cache_info httr

    ## using cached file: ~/Library/Caches/R/noaa_ghcnd/USW00094728.dly

    ## date created (size, mb): 2022-10-05 19:32:51 (8.408)

    ## file min/max dates: 1869-01-01 / 2022-10-31

    ## using cached file: ~/Library/Caches/R/noaa_ghcnd/USC00519397.dly

    ## date created (size, mb): 2022-10-05 19:32:56 (1.699)

    ## file min/max dates: 1965-01-01 / 2020-03-31

    ## using cached file: ~/Library/Caches/R/noaa_ghcnd/USS0023B17S.dly

    ## date created (size, mb): 2022-10-05 19:32:57 (0.951)

    ## file min/max dates: 1999-09-01 / 2022-10-31

``` r
## Registered S3 method overwritten by 'hoardr':
##   method           from
##   print.cache_info httr
## using cached file: ~/Library/Caches/R/noaa_ghcnd/USW00094728.dly
## date created (size, mb): 2022-04-04 04:13:35 (7.647)
## file min/max dates: 1869-01-01 / 2022-04-30
## using cached file: ~/Library/Caches/R/noaa_ghcnd/USC00519397.dly
## date created (size, mb): 2022-04-04 04:13:39 (1.697)
## file min/max dates: 1965-01-01 / 2020-02-29
## using cached file: ~/Library/Caches/R/noaa_ghcnd/USS0023B17S.dly
## date created (size, mb): 2022-04-04 04:13:42 (0.93)
## file min/max dates: 1999-09-01 / 2022-03-31

weather_df
```

    ## # A tibble: 1,095 × 6
    ##    name           id          date        prcp  tmax  tmin
    ##    <chr>          <chr>       <date>     <dbl> <dbl> <dbl>
    ##  1 CentralPark_NY USW00094728 2017-01-01     0   8.9   4.4
    ##  2 CentralPark_NY USW00094728 2017-01-02    53   5     2.8
    ##  3 CentralPark_NY USW00094728 2017-01-03   147   6.1   3.9
    ##  4 CentralPark_NY USW00094728 2017-01-04     0  11.1   1.1
    ##  5 CentralPark_NY USW00094728 2017-01-05     0   1.1  -2.7
    ##  6 CentralPark_NY USW00094728 2017-01-06    13   0.6  -3.8
    ##  7 CentralPark_NY USW00094728 2017-01-07    81  -3.2  -6.6
    ##  8 CentralPark_NY USW00094728 2017-01-08     0  -3.8  -8.8
    ##  9 CentralPark_NY USW00094728 2017-01-09     0  -4.9  -9.9
    ## 10 CentralPark_NY USW00094728 2017-01-10     0   7.8  -6  
    ## # … with 1,085 more rows

``` r
## # A tibble: 1,095 × 6
##   name           id          date        prcp  tmax  tmin
##   <chr>          <chr>       <date>     <dbl> <dbl> <dbl>
## 1 CentralPark_NY USW00094728 2017-01-01     0   8.9   4.4
## 2 CentralPark_NY USW00094728 2017-01-02    53   5     2.8
## 3 CentralPark_NY USW00094728 2017-01-03   147   6.1   3.9
## 4 CentralPark_NY USW00094728 2017-01-04     0  11.1   1.1
## 5 CentralPark_NY USW00094728 2017-01-05     0   1.1  -2.7
## # … with 1,090 more rows
```

## Scatterplots!!

Create my first scatterplot ever

``` r
ggplot(weather_df, aes(x = tmin, y = tmax)) +
  geom_point()
```

    ## Warning: Removed 15 rows containing missing values (geom_point).

![](template_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

New approach, same plot

``` r
weather_df %>% 
  ggplot(aes(x = tmin, y = tmax)) + 
  geom_point()
```

    ## Warning: Removed 15 rows containing missing values (geom_point).

![](template_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

Save and edit a plot object.

``` r
weather_plot <- 
  weather_df %>% 
  ggplot(aes(x = tmin, y = tmax))

weather_plot + geom_point()
```

    ## Warning: Removed 15 rows containing missing values (geom_point).

![](template_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

## Advanced scatterplot

Start with the same one and make it fancy!

``` r
weather_df %>% 
  ggplot(aes(x = tmin, y = tmax, color = name)) + 
  geom_point() + 
  geom_smooth(se = FALSE)
```

    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

    ## Warning: Removed 15 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 15 rows containing missing values (geom_point).

![](template_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

What about the `aes` placement?

``` r
weather_df %>% 
  ggplot(aes(x = tmin, y = tmax)) + 
  geom_point(aes(color = name)) + #up till here, no difference between the two
  geom_smooth(se = FALSE) #here - the placement of aes(color=name) makes a difference
```

    ## `geom_smooth()` using method = 'gam' and formula 'y ~ s(x, bs = "cs")'

    ## Warning: Removed 15 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 15 rows containing missing values (geom_point).

![](template_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

Let’s facet some things..

``` r
weather_df %>% 
  ggplot(aes(x = tmin, y = tmax, color = name)) + 
  geom_point(alpha = 0.2, size = 2) + 
  geom_smooth(se = FALSE) + 
  facet_grid(. ~ name) #this creates columns. facet_grid(name ~ .) creates rows
```

    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

    ## Warning: Removed 15 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 15 rows containing missing values (geom_point).

![](template_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

Let’s combine some elements and try new plot

``` r
weather_df %>%
  ggplot(aes(x = date, y = tmax, color = name)) + 
  geom_point(aes(size = prcp), alpha =.5) +
  geom_smooth(se = FALSE) +
  facet_grid(. ~ name)
```

    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

    ## Warning: Removed 3 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 3 rows containing missing values (geom_point).

![](template_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

## Small notes

How many geoms have to exist?

You can have whatever geoms you want.

``` r
weather_df %>%
  ggplot(aes(x = tmin, y = tmax, color = name)) + 
  geom_smooth(se = FALSE)
```

    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

    ## Warning: Removed 15 rows containing non-finite values (stat_smooth).

![](template_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

You can use a neat geom!

``` r
weather_df %>% 
  ggplot(aes(x = tmin, y = tmax)) + 
  geom_hex()
```

    ## Warning: Removed 15 rows containing non-finite values (stat_binhex).

![](template_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

## Univariate plots

``` r
weather_df %>%
  ggplot(aes(x = tmin)) +
  geom_histogram()
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 15 rows containing non-finite values (stat_bin).

![](template_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

Can we add color..

``` r
weather_df %>%
  ggplot(aes(x = tmin, fill = name)) + 
  geom_histogram(position = 'dodge')
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 15 rows containing non-finite values (stat_bin).

![](template_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

``` r
weather_df %>% 
  ggplot(aes(x = tmin, fill = name)) + 
  geom_histogram() + 
  facet_grid(. ~ name)
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 15 rows containing non-finite values (stat_bin).

![](template_files/figure-gfm/unnamed-chunk-13-2.png)<!-- --> Let’s try
a new geometry

``` r
weather_df %>% 
  ggplot(aes(x = tmin, fill = name)) + 
  geom_density(alpha = 0.3, adjust = 0.5)
```

    ## Warning: Removed 15 rows containing non-finite values (stat_density).

![](template_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

What about box plots?

``` r
weather_df %>% 
  ggplot(aes(x = name, y = tmin)) + 
  geom_boxplot()
```

    ## Warning: Removed 15 rows containing non-finite values (stat_boxplot).

![](template_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

Trendy blots

``` r
weather_df %>% 
  ggplot(aes(x = name, y = tmin, fill = name)) + 
  geom_violin(alpha = 0.5) + 
  stat_summary()
```

    ## Warning: Removed 15 rows containing non-finite values (stat_ydensity).

    ## Warning: Removed 15 rows containing non-finite values (stat_summary).

    ## No summary function supplied, defaulting to `mean_se()`

![](template_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

Ridge plots – the most popular plot of 2017

``` r
weather_df %>% 
  ggplot(aes(x = tmin, y = name, fill = name)) + 
  geom_density_ridges()
```

    ## Picking joint bandwidth of 1.67

    ## Warning: Removed 15 rows containing non-finite values (stat_density_ridges).

![](template_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

## Save and Embed

``` r
weather_plot <- 
weather_df %>% 
  ggplot(aes(x = tmin, y= tmax, color = name)) + 
  geom_point(alpha = 0.5)

ggsave("./results/weather_plot.pdf", weather_plot, width = 8, height = 5)
```

    ## Warning: Removed 15 rows containing missing values (geom_point).

What about embedding..

``` r
weather_plot
```

    ## Warning: Removed 15 rows containing missing values (geom_point).

![](template_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->

Embed at different size

``` r
weather_plot
```

    ## Warning: Removed 15 rows containing missing values (geom_point).

![](template_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->
