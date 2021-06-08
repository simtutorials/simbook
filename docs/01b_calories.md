# Calorie Placement Re-Simulation"


```r
knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
library(faux)
library(afex)
library(emmeans)
faux_options(plot = FALSE)

set.seed(8675309)
```

## Data Source

We will be replicating some of the re-analyses in Francis & Thunell's (2020) Meta-Psychology paper: Excess success in "Don't count calorie labeling out: Calorie counts on the left side of menu items lead to lower calorie food choices".

They ran power analyses for all 6 studies in Dallas, Liu, and Ubel's (2019) study showing that people order food with significantly fewer calories when the calorie count was placed to the left of the item than to the right (or having no calorie label). They then used these power estimates to calculate the probability of all 6 out of 6 studies being significant, given the observed power of each study.

* [Re-analysis](https://doi.org/10.15626/MP.2019.2266)
* [Re-analysis code](https://osf.io/xrdhj/)
* [Original paper](https://doi.org/10.1002/jcpy.1053)

Table 1 of the re-analysis paper provides all of the parameters we will need. 

## Reanalyses

### Study 2

We'll start with S2 because the analysis is very straightforward. It's a between-subjects design, where 143 subjects saw calorie placement on the left and their mean calories ordered were 1249.83 (SD = 449.07), while 132 subjects saw calorie placement on the right and their mean calories ordered were 1362.31 (SD = 447.35).

Let's first simulate a single data table with these parameters and set up our analysis.


```r
s2 <- sim_design(
  between = list(placement = c("left", "right")),
  mu = c(left = 1249.83, right = 1362.31),
  sd = c(left = 449.07, right = 447.35),
  n = c(left = 143, right = 132),
  dv = "calories"
)
```

Wrap the analysis in a function using the `tidy()` function from {broom} to get the results in a tidy table. Check that it works by running it on the single data set above.


```r
s2_analyse <- function(data) {
  t.test(calories ~ placement, data = data) %>%
    broom::tidy()
}

s2_analyse(s2)
```

```
## # A tibble: 1 x 10
##   estimate estimate1 estimate2 statistic p.value parameter conf.low conf.high
##      <dbl>     <dbl>     <dbl>     <dbl>   <dbl>     <dbl>    <dbl>     <dbl>
## 1    -80.4     1266.     1347.     -1.50   0.134      258.    -186.      24.9
## # … with 2 more variables: method <chr>, alternative <chr>
```


Now, simulate the data 1000 times.


```r
s2 <- sim_design(
  between = list(placement = c("left", "right")),
  mu = c(left = 1249.83, right = 1362.31),
  sd = c(left = 449.07, right = 447.35),
  n = c(left = 143, right = 132),
  dv = "calories",
  rep = 1000
)
```

Run the analysis on each data set.


```r
s2_sim <- s2 %>%
  mutate(analysis = map(data, s2_analyse)) %>%
  select(-data) %>%
  unnest(analysis)

head(s2_sim)
```

```
## # A tibble: 6 x 11
##     rep estimate estimate1 estimate2 statistic  p.value parameter conf.low
##   <int>    <dbl>     <dbl>     <dbl>     <dbl>    <dbl>     <dbl>    <dbl>
## 1     1   -161.      1204.     1365.     -2.74 0.00659       254.    -276.
## 2     2   -186.      1203.     1388.     -3.42 0.000732      266.    -293.
## 3     3   -150.      1225.     1375.     -2.78 0.00577       272.    -256.
## 4     4   -130.      1275.     1405.     -2.39 0.0176        272.    -237.
## 5     5    -93.9     1273.     1367.     -1.62 0.106         269.    -208.
## 6     6   -116.      1235.     1350.     -1.99 0.0472        272.    -230.
## # … with 3 more variables: conf.high <dbl>, method <chr>, alternative <chr>
```

Summarise the `p.value` column to get power.


```r
s2_power <- s2_sim %>%
  mutate(sig = p.value < .05) %>%
  summarise(power = mean(sig)) %>%
  pull(power)
```

Compare this value (0.523) with the value in the paper (0.5426).

### Study 1

Study 1 is a little more complicated because the design includes a "no label" condition, so the decision rule for supporting the hypothesis is more complicated.

The data simulation is relatively straightforward, though.


```r
s1 <- sim_design(
  between = list(placement = c("left", "right", "none")),
  mu = c(654.53, 865.41, 914.34),
  sd = c(390.45, 517.26, 560.94),
  n = c(45, 54, 50),
  dv = "calories"
)
```

Set up the analysis. Here, we really just care about three p=values, so we'll just return those. We can use a function from the {emmeans} package to check the two pairwise comparisons.


```r
afex::set_sum_contrasts() # avoids annoying afex message on each run
```

```
## setting contr.sum globally: options(contrasts=c('contr.sum', 'contr.poly'))
```

```r
afex_options(include_aov = TRUE) # we need aov for lsmeans

s1_analyse <- function(data) {
  # main effect of placement
  a <- afex::aov_ez(
    id = "id",
    dv = "calories",
    between = "placement",
    data = data
  )
  
  # contrasts
  e <- emmeans(a, "placement")
  c1 <- list(lr = c(-0.5, 0.5, 0),
             ln = c(-0.5, 0, 0.5))
  b <- contrast(e, c1, adjust = "holm") %>%
    broom::tidy()
  
  data.frame(
    p_all = a$anova_table$`Pr(>F)`[[1]],
    p_1 = b$adj.p.value[[1]],
    p_2 = b$adj.p.value[[2]]
  )
}

s1_analyse(s1)
```

```
##         p_all        p_1         p_2
## 1 0.002915484 0.01000732 0.002055471
```

Let's just replicate this 100 times so the simulation doesn't take too long to run at first. We can always increase it later after we've run some sense checks.


```r
s1 <- sim_design(
  between = list(placement = c("left", "right", "none")),
  mu = c(654.53, 865.41, 914.34),
  sd = c(390.45, 517.26, 560.94),
  n = c(45, 54, 50),
  dv = "calories",
  rep = 100
)
```

Run the analysis on each data set.


```r
s1_sim <- s1 %>%
  mutate(analysis = map(data, s1_analyse)) %>%
  select(-data) %>%
  unnest(analysis)

head(s1_sim)
```

```
## # A tibble: 6 x 4
##     rep   p_all    p_1     p_2
##   <int>   <dbl>  <dbl>   <dbl>
## 1     1 0.0286  0.0161 0.0999 
## 2     2 0.281   0.447  0.225  
## 3     3 0.0187  0.0128 0.0380 
## 4     4 0.00301 0.570  0.00336
## 5     5 0.0103  0.0511 0.00546
## 6     6 0.0333  0.0922 0.0197
```

Calculating power is a little trickier here, as all three p-values need to be significant here to support the hypothesis.


```r
s1_power <- s1_sim %>%
  mutate(sig = (p_all < .05) & 
               (p_1 < .05) & 
               (p_2 < .05)  ) %>%
  summarise(power = mean(sig)) %>%
  pull(power)
```

Compare this value (0.5) with the value in the paper (0.4582).

### Study 3

Now you can use the pattern from Study 1 to analyse the data for Study 3. We'll start with the repeated data set.


```r
s3 <- sim_design(
  between = list(placement = c("left", "right", "none")),
  mu = c(1428.24, 1308.66, 1436.79),
  sd = c(377.02, 420.14, 378.47),
  n = c(85, 86, 81),
  dv = "calories",
  rep = 100
)
```

These data were collected in the Hebrew language, which reads right to left, so the paired contrasts will be different.


```r
s3_analyse <- function(data) {
  # main effect of placement
  a <- afex::aov_ez(
    id = "id",
    dv = "calories",
    between = "placement",
    data = data
  )
  
  # contrasts (reversed)
  e <- emmeans(a, "placement")
  c1 <- list(rl = c(0.5, -0.5, 0),
             ln = c(0, -0.5, 0.5))
  b <- contrast(e, c1, adjust = "holm") %>%
    broom::tidy()
  
  data.frame(
    p_all = a$anova_table$`Pr(>F)`[[1]],
    p_1 = b$adj.p.value[[1]],
    p_2 = b$adj.p.value[[2]]
  )
}
```

Run the analysis on each data set.


```r
s3_sim <- s3 %>%
  mutate(analysis = map(data, s3_analyse)) %>%
  select(-data) %>%
  unnest(analysis)

head(s3_sim)
```

```
## # A tibble: 6 x 4
##     rep   p_all      p_1    p_2
##   <int>   <dbl>    <dbl>  <dbl>
## 1     1 0.0765  0.0477   0.210 
## 2     2 0.00103 0.000566 0.252 
## 3     3 0.201   0.219    0.219 
## 4     4 0.0117  0.00831  0.488 
## 5     5 0.583   0.701    0.701 
## 6     6 0.0320  0.0390   0.0305
```


```r
s3_power <- s3_sim %>%
  mutate(sig = (p_all < .05) & 
               (p_1 < .05) & 
               (p_2 < .05)  ) %>%
  summarise(power = mean(sig)) %>%
  pull(power)
```

Compare this value (0.33) with the value in the paper (0.3626).


### Study S1

Now you can use the pattern from Study 2 to analyse the data for Study S1. You can even reuse the analysis function `s2_analyse`!


```r
ss1 <- sim_design(
  between = list(placement = c("left", "right")),
  mu = c(left = 185.94, right = 215.73),
  sd = c(left = 93.92, right = 95.33),
  n = c(left = 99, right = 77),
  dv = "calories",
  rep = 1000
)
```


```r
ss1_sim <- ss1 %>%
  mutate(analysis = map(data, s2_analyse)) %>%
  select(-data) %>%
  unnest(analysis)
```



```r
ss1_power <- ss1_sim %>%
  mutate(sig = p.value < .05) %>%
  summarise(power = mean(sig)) %>%
  pull(power)
```


### Study S2

Now you can use the pattern from Study 1 to analyse the data for Study S2. You can even reuse the analysis function `s1_analyse`!


```r
ss2 <- sim_design(
  between = list(placement = c("left", "right", "none")),
  mu = c(1182.15, 1302.23, 1373.74),
  sd = c(477.60, 434.41, 475.77),
  n = c(139, 141, 151),
  dv = "calories",
  rep = 100
)
```


```r
ss2_sim <- ss2 %>%
  mutate(analysis = map(data, s1_analyse)) %>%
  select(-data) %>%
  unnest(analysis)
```


```r
ss2_power <- ss2_sim %>%
  mutate(sig = (p_all < .05) & 
               (p_1 < .05) & 
               (p_2 < .05)  ) %>%
  summarise(power = mean(sig)) %>%
  pull(power)
```

### Study S3

Now you can use the pattern from Study 1 to analyse the data for Study S3.


```r
ss3 <- sim_design(
  between = list(placement = c("left", "right", "none")),
  mu = c(1302.03, 1373.15, 1404.35),
  sd = c(480.02, 442.49, 422.03),
  n = c(336, 337, 333),
  dv = "calories",
  rep = 100
)
```


```r
ss3_sim <- ss3 %>%
  mutate(analysis = map(data, s1_analyse)) %>%
  select(-data) %>%
  unnest(analysis)
```


```r
ss3_power <- ss3_sim %>%
  mutate(sig = (p_all < .05) & 
               (p_1 < .05) & 
               (p_2 < .05)  ) %>%
  summarise(power = mean(sig)) %>%
  pull(power)
```

## Conclusion

Now that you've calculated power for each of the 6 studies, just multiply the 6 power values together to get the probability that all 6 studies will be significant.



```r
power_table <- tribble(
  ~study, ~power_ft, ~ power_my,
  "1", 0.4582, s1_power,
  "2", 0.5426, s2_power,
  "3", 0.3626, s3_power,
  "S1", 0.5358, ss1_power,
  "S2", 0.5667, ss2_power,
  "S3", 0.4953, ss3_power
)

power_table
```

```
## # A tibble: 6 x 3
##   study power_ft power_my
##   <chr>    <dbl>    <dbl>
## 1 1        0.458    0.5  
## 2 2        0.543    0.523
## 3 3        0.363    0.33 
## 4 S1       0.536    0.534
## 5 S2       0.567    0.59 
## 6 S3       0.495    0.37
```

The `reduce()` function from {purrr} applies a function sequentially over a vector, so can give up the product of all the values in the power columns.


```r
prob_ft <- purrr::reduce(power_table$power_ft, `*`)
prob_my <- purrr::reduce(power_table$power_my, `*`)
```

The Francis & Thunell paper showed a 0.0135577 probability of getting 6 of 6 studies significant. Our re-simulation showed a 0.0100596 probability.

