case_study
================
Farizah Rob
2022-10-13

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
    ## ✔ ggplot2 3.3.6      ✔ purrr   0.3.4 
    ## ✔ tibble  3.1.8      ✔ dplyr   1.0.10
    ## ✔ tidyr   1.2.0      ✔ stringr 1.4.1 
    ## ✔ readr   2.1.2      ✔ forcats 0.5.2 
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
knitr::opts_chunk$set(
  fig.width = 6, 
  fig.asp = .6, 
  out.width = "90%"
)

theme_set(theme_minimal() + theme(legend.position="bottom"))

options(
  ggplot2.continuous.colour = "viridis", 
  ggplot2.continuous.fill = "viridis"
)

scale_color_discrete <- scale_color_viridis_d
scale_fill_discrete <- scale_fill_viridis_d
```

### Get data

``` r
library(p8105.datasets)

data(nyc_airbnb) #17 variables, 40753 variables

nyc_airbnb <- 
  nyc_airbnb %>% 
  rename(borough = neighbourhood_group) %>%
  mutate(stars = review_scores_location / 2)
```

## Brainstorm ideas

-   what variables are relevant to higher reviews?
-   what are the popular places to rent an airbnb?
-   what portion of hosts/locations are private rooms vs entire
    apartment vs shared room? (room_type)
-   what makes an airbnb more expensive?

``` r
nyc_airbnb %>%
  ggplot(aes(x = borough, y = review_scores_location)) + geom_point() #does not tell us much
```

    ## Warning: Removed 10037 rows containing missing values (geom_point).

<img src="case_study_files/figure-gfm/unnamed-chunk-3-1.png" width="90%" />

``` r
nyc_airbnb %>%
  group_by(borough) %>%
  summarize(avg_review = mean(stars, na.rm=TRUE)) %>%
  knitr::kable()
```

| borough       | avg_review |
|:--------------|-----------:|
| Bronx         |   4.444444 |
| Brooklyn      |   4.645007 |
| Manhattan     |   4.785744 |
| Queens        |   4.651240 |
| Staten Island |   4.618280 |

``` r
#Most number in Williamsburg, Bed-Stuy, Harlem, Bushwick
nyc_airbnb %>%
  group_by(neighbourhood, borough) %>%
  distinct() %>%
  summarize(n_obs = n()) %>%
  filter(n_obs > 100) %>%
  arrange(n_obs)
```

    ## `summarise()` has grouped output by 'neighbourhood'. You can override using the
    ## `.groups` argument.

    ## # A tibble: 54 × 3
    ## # Groups:   neighbourhood [54]
    ##    neighbourhood    borough   n_obs
    ##    <chr>            <chr>     <int>
    ##  1 Woodside         Queens      113
    ##  2 East New York    Brooklyn    118
    ##  3 Elmhurst         Queens      125
    ##  4 Sheepshead Bay   Brooklyn    127
    ##  5 Windsor Terrace  Brooklyn    128
    ##  6 Brooklyn Heights Brooklyn    129
    ##  7 Kensington       Brooklyn    137
    ##  8 Boerum Hill      Brooklyn    153
    ##  9 Jackson Heights  Queens      154
    ## 10 Tribeca          Manhattan   156
    ## # … with 44 more rows

``` r
#Bronx has lowest average review score but all are comparable
# Hunts Point has lowest average review score amongst all neighborhoods in Bronx

nyc_airbnb %>%
  filter(borough=="Bronx") %>%
  group_by(neighbourhood) %>%
  summarize(avg_review = mean(stars, na.rm = TRUE)) %>%
  knitr::kable()
```

| neighbourhood      | avg_review |
|:-------------------|-----------:|
| Allerton           |   4.452381 |
| Baychester         |   4.250000 |
| Belmont            |   4.250000 |
| Bronxdale          |   4.450000 |
| Castle Hill        |   4.500000 |
| City Island        |   5.000000 |
| Claremont Village  |   4.416667 |
| Clason Point       |   4.441177 |
| Co-op City         |   4.666667 |
| Concourse          |   4.300000 |
| Concourse Village  |   4.550000 |
| Country Club       |        NaN |
| East Morrisania    |   4.333333 |
| Eastchester        |   4.541667 |
| Edenwald           |   5.000000 |
| Fieldston          |   4.750000 |
| Fordham            |   4.406250 |
| Highbridge         |   4.384615 |
| Hunts Point        |   3.750000 |
| Kingsbridge        |   4.532258 |
| Longwood           |   4.270833 |
| Melrose            |   4.333333 |
| Morris Heights     |   3.950000 |
| Morris Park        |   5.000000 |
| Morrisania         |   3.750000 |
| Mott Haven         |   4.325000 |
| Mount Eden         |   4.375000 |
| Mount Hope         |   4.166667 |
| North Riverdale    |   4.750000 |
| Norwood            |   4.454546 |
| Olinville          |   4.000000 |
| Parkchester        |   4.575000 |
| Pelham Bay         |   4.750000 |
| Pelham Gardens     |   4.916667 |
| Port Morris        |   4.315790 |
| Riverdale          |   4.700000 |
| Schuylerville      |   4.625000 |
| Soundview          |   4.700000 |
| Spuyten Duyvil     |   4.750000 |
| Throgs Neck        |   4.800000 |
| Tremont            |   4.250000 |
| Unionport          |   4.833333 |
| University Heights |   4.318182 |
| Van Nest           |   4.428571 |
| Wakefield          |   4.583333 |
| West Farms         |   4.000000 |
| Westchester Square |   4.000000 |
| Williamsbridge     |   4.315790 |
| Woodlawn           |   4.700000 |

``` r
#Looking at Brooklyn next
# Brownsville has lowest average review amongst Brooklyn neighborhoods
nyc_airbnb %>%
  filter(borough=="Brooklyn") %>%
  group_by(neighbourhood) %>%
  summarize(avg_review = mean(stars, na.rm = TRUE)) %>%
  arrange(avg_review) %>%
  knitr::kable()
```

| neighbourhood             | avg_review |
|:--------------------------|-----------:|
| Brownsville               |   3.909091 |
| East New York             |   4.177419 |
| Cypress Hills             |   4.188525 |
| Borough Park              |   4.213235 |
| East Flatbush             |   4.263547 |
| Flatlands                 |   4.375000 |
| Canarsie                  |   4.424528 |
| Bedford-Stuyvesant        |   4.437528 |
| Mill Basin                |   4.500000 |
| Sea Gate                  |   4.500000 |
| Crown Heights             |   4.507968 |
| Midwood                   |   4.508621 |
| Flatbush                  |   4.527344 |
| Prospect-Lefferts Gardens |   4.532020 |
| Bushwick                  |   4.534657 |
| Brighton Beach            |   4.580645 |
| Bath Beach                |   4.583333 |
| Navy Yard                 |   4.590909 |
| Sunset Park               |   4.595238 |
| Kensington                |   4.606482 |
| Bensonhurst               |   4.609375 |
| Red Hook                  |   4.620370 |
| Dyker Heights             |   4.642857 |
| Downtown Brooklyn         |   4.664474 |
| Manhattan Beach           |   4.666667 |
| Columbia St               |   4.673913 |
| Coney Island              |   4.678571 |
| Gravesend                 |   4.678571 |
| Sheepshead Bay            |   4.693182 |
| Gowanus                   |   4.734615 |
| Vinegar Hill              |   4.738095 |
| Clinton Hill              |   4.755125 |
| Bay Ridge                 |   4.769231 |
| Fort Hamilton             |   4.791667 |
| Greenpoint                |   4.808599 |
| Williamsburg              |   4.809618 |
| Windsor Terrace           |   4.831731 |
| Fort Greene               |   4.838235 |
| Carroll Gardens           |   4.854651 |
| South Slope               |   4.870192 |
| Prospect Heights          |   4.892857 |
| DUMBO                     |   4.895833 |
| Boerum Hill               |   4.906504 |
| Park Slope                |   4.944602 |
| Brooklyn Heights          |   4.953608 |
| Cobble Hill               |   4.986301 |
| Bergen Beach              |   5.000000 |
| Gerritsen Beach           |        NaN |

``` r
#Manhattan
#East Harlem followed by Inwood has lowest review amongst Manhattan neighborhoods
nyc_airbnb %>%
  filter(borough=="Manhattan") %>%
  group_by(neighbourhood) %>%
  summarize(avg_review = mean(stars, na.rm = TRUE)) %>%
  arrange(avg_review) %>%
  knitr::kable()
```

| neighbourhood       | avg_review |
|:--------------------|-----------:|
| East Harlem         |   4.420762 |
| Inwood              |   4.457576 |
| Marble Hill         |   4.500000 |
| Washington Heights  |   4.501615 |
| Two Bridges         |   4.512821 |
| Harlem              |   4.552956 |
| Chinatown           |   4.644689 |
| Roosevelt Island    |   4.702381 |
| Lower East Side     |   4.759603 |
| Civic Center        |   4.800000 |
| Morningside Heights |   4.808765 |
| Stuyvesant Town     |   4.809524 |
| Little Italy        |   4.818182 |
| East Village        |   4.843984 |
| Upper East Side     |   4.854446 |
| Murray Hill         |   4.864641 |
| Kips Bay            |   4.867412 |
| Tribeca             |   4.884615 |
| Upper West Side     |   4.890299 |
| Financial District  |   4.894309 |
| Hell’s Kitchen      |   4.895731 |
| Gramercy            |   4.902128 |
| Midtown             |   4.907434 |
| Flatiron District   |   4.921875 |
| Theater District    |   4.925000 |
| Battery Park City   |   4.926471 |
| Nolita              |   4.929167 |
| Chelsea             |   4.937947 |
| NoHo                |   4.958333 |
| West Village        |   4.966780 |
| SoHo                |   4.969858 |
| Greenwich Village   |   4.972973 |

``` r
#total private rooms, shared room, entire apartment
#Entire home/apartment and Private room (similar numbers), only 1190 shared room 

nyc_airbnb %>%
  group_by(room_type) %>%
  summarize(count = n())
```

    ## # A tibble: 3 × 2
    ##   room_type       count
    ##   <chr>           <int>
    ## 1 Entire home/apt 19937
    ## 2 Private room    19626
    ## 3 Shared room      1190

Let’s look at price..

``` r
# you can also use summarize without grouping first
nyc_airbnb %>%
  group_by(borough, room_type) %>%
  summarize(
    mean_price = mean(price, na.rm = TRUE)
  ) %>% 
  pivot_wider(
    names_from = "room_type", 
    values_from = "mean_price"
  ) 
```

    ## `summarise()` has grouped output by 'borough'. You can override using the
    ## `.groups` argument.

    ## # A tibble: 5 × 4
    ## # Groups:   borough [5]
    ##   borough       `Entire home/apt` `Private room` `Shared room`
    ##   <chr>                     <dbl>          <dbl>         <dbl>
    ## 1 Bronx                      125.           65.5          57.5
    ## 2 Brooklyn                   175.           76.7          59.6
    ## 3 Manhattan                  238.          107.           84.7
    ## 4 Queens                     140.           70.6          49.1
    ## 5 Staten Island              207.           65.4          25

Look at price distributions

``` r
nyc_airbnb %>%
  filter(borough=="Manhattan", 
         room_type=="Entire home/apt", 
         price < 1000) %>%
  ggplot(aes(x = price)) + geom_histogram()
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

<img src="case_study_files/figure-gfm/unnamed-chunk-5-1.png" width="90%" />

``` r
nyc_airbnb %>%
  filter(borough == "Manhattan", 
         price < 1000) %>%
  ggplot(aes(x = price)) + geom_histogram() + facet_grid(. ~ room_type)
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

<img src="case_study_files/figure-gfm/unnamed-chunk-5-2.png" width="90%" />

Price vs.. rating?

``` r
nyc_airbnb %>%
  filter(borough == "Manhattan") %>%
  ggplot(aes(x = stars, y = price)) + geom_point() + facet_grid(. ~ room_type)
```

    ## Warning: Removed 4671 rows containing missing values (geom_point).

<img src="case_study_files/figure-gfm/unnamed-chunk-6-1.png" width="90%" />

Somewhat fancy plot

``` r
nyc_airbnb %>%
  filter(borough == "Manhattan") %>%
  group_by(neighbourhood) %>%
  summarize(mean_price = mean(price, na.rm = TRUE)) %>%
  arrange(desc(mean_price))
```

    ## # A tibble: 32 × 2
    ##    neighbourhood     mean_price
    ##    <chr>                  <dbl>
    ##  1 Tribeca                 353.
    ##  2 Flatiron District       319.
    ##  3 NoHo                    302.
    ##  4 Greenwich Village       258.
    ##  5 SoHo                    256.
    ##  6 Midtown                 251.
    ##  7 West Village            239.
    ##  8 Chelsea                 233.
    ##  9 Theater District        232.
    ## 10 Battery Park City       221.
    ## # … with 22 more rows

``` r
nyc_airbnb %>%
  filter(borough == "Manhattan", 
         price <= 1000, 
         room_type == "Entire home/apt") %>%
  mutate(neighbourhood = fct_reorder(neighbourhood, price)) %>%
  ggplot(aes(x = neighbourhood, y = price)) + geom_boxplot() + coord_flip()
```

<img src="case_study_files/figure-gfm/unnamed-chunk-8-1.png" width="90%" />

Price vs location

``` r
nyc_airbnb %>%
  filter(borough == "Manhattan", 
         price <= 1000, 
         room_type == "Entire home/apt") %>%
  sample_n(1000) %>%
  ggplot(aes(x = lat, y = long, color = price)) + geom_point(alpha = 0.5)
```

<img src="case_study_files/figure-gfm/unnamed-chunk-9-1.png" width="90%" />

``` r
#northern manhattan is on the right top, southern manhattan is left bottom
```
