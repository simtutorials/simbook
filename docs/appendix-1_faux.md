# Faux Exercises


```r
library(tidyverse)
library(faux)
library(afex)
library(emmeans)
faux_options(plot = FALSE)

set.seed(8675309)
```

## Short Exercises

### Multivariate normal

Sample 40 values of three variables named `J`, `K` and `L` from a population with means of 10, 20 and 30, and SDs of 5. `J` and `K` are correlated 0.5, `J` and `L` are correlated 0.25, and `K` and `L` are not correlated.


```r
ex1 <- rnorm_multi(n = 40, mu = c(J = 10, K = 20, L = 30),
                   sd = 5, r = c(0.5, 0.25, 0))

get_params(ex1)
```

<div class="kable-table">

|  n|var |    J|     K|     L|  mean|   sd|
|--:|:---|----:|-----:|-----:|-----:|----:|
| 40|J   | 1.00|  0.52|  0.12|  9.33| 4.44|
| 40|K   | 0.52|  1.00| -0.20| 19.46| 4.62|
| 40|L   | 0.12| -0.20|  1.00| 28.61| 4.57|

</div>

### From existing data

Using the data from the built-in dataset `attitude`, simulate a new set of 20 observations drawn from a population with the same means, SDs and correlations for each column as the original data.


```r
dat_r <- cor(attitude)
dat_mu <- summarise_all(attitude, mean) %>% t()
dat_sd <- summarise_all(attitude, sd) %>% t()

ex2 <- rnorm_multi(20, mu = dat_mu, sd = dat_sd,r = dat_r)

get_params(ex2)
```

<div class="kable-table">

|  n|var        | rating| complaints| privileges| learning| raises| critical| advance|  mean|    sd|
|--:|:----------|------:|----------:|----------:|--------:|------:|--------:|-------:|-----:|-----:|
| 20|rating     |   1.00|       0.68|       0.39|     0.38|   0.37|     0.26|    0.06| 62.73|  8.19|
| 20|complaints |   0.68|       1.00|       0.50|     0.18|   0.51|     0.34|   -0.05| 64.82|  9.84|
| 20|privileges |   0.39|       0.50|       1.00|     0.29|   0.34|    -0.03|    0.10| 50.06| 10.68|
| 20|learning   |   0.38|       0.18|       0.29|     1.00|   0.59|     0.01|    0.64| 55.90| 11.33|
| 20|raises     |   0.37|       0.51|       0.34|     0.59|   1.00|     0.30|    0.60| 63.06| 10.82|
| 20|critical   |   0.26|       0.34|      -0.03|     0.01|   0.30|     1.00|   -0.19| 76.67| 10.22|
| 20|advance    |   0.06|      -0.05|       0.10|     0.64|   0.60|    -0.19|    1.00| 43.89| 12.71|

</div>


### 2b

Create a dataset with a between-subject factor of "pet" having two levels, "cat", and "dog". The DV is "happiness" score. There are 20 cat-owners with a mean happiness score of 10 (SD = 3) and there are 30 dog-owners with a mean happiness score of 11 (SD = 3).


```r
dat2b <- sim_design(
  between = list(pet = c("cat", "dog")),
  dv = "happiness",
  n = list(cat = 20, dog = 30),
  mu = list(cat = 10, dog = 11),
  sd = 3
)

get_params(dat2b, between = "pet")
```

<div class="kable-table">

|pet |  n|  mean|   sd|
|:---|--:|-----:|----:|
|cat | 20| 10.21| 2.95|
|dog | 30| 11.17| 2.69|

</div>

### 3w

Create a dataset of 20 observations with 1 within-subject variable ("condition") having 3 levels ("A", "B", "C") with means of 10, 20 and 30 and SD of 5. The correlations between each level have r = 0.4. The dataset should look like this:

| id | condition | score |
|:---|:----------|------:|
|S01 | A         |  9.17 |
|... | ...       |  ...  |
|S20 | A         | 11.57 |
|S01 | B         | 18.44 |
|... | ...       |  ...  |
|S20 | B         | 20.04 |
|S01 | C         | 35.11 |
|... | ...       |  ...  |
|S20 | C         | 29.16 |


```r
dat3w <- sim_design(
  within = list(condition = c("A", "B", "C")),
  n = 20,
  mu = c(10, 20, 30),
  sd = 5,
  r = .4,
  dv = "score",
  long = TRUE
)

get_params(dat3w)
```

<div class="kable-table">

|  n|var |    A|    B|    C|  mean|   sd|
|--:|:---|----:|----:|----:|-----:|----:|
| 20|A   | 1.00| 0.47| 0.43|  8.59| 4.70|
| 20|B   | 0.47| 1.00| 0.21| 20.65| 5.97|
| 20|C   | 0.43| 0.21| 1.00| 31.00| 3.29|

</div>

### 2w*2w

Create a dataset with 50 observations of 2 within-subject variables ("A" and "B") each having 2 levels. The mean for all cells is 10 and the SD is 2. The dataset should have 20 subjects. The correlations look like this:

|       | A1_B1 | A1_B2 | A2_B1 | A2_B2 |
|:------|------:|------:|------:|------:|
| A1_B1 | 1.0   | 0.5   | 0.5   | 0.2   |
| A1_B2 | 0.5   | 1.0   | 0.2   | 0.5   |
| A2_B1 | 0.5   | 0.2   | 1.0   | 0.5   |
| A2_B2 | 0.2   | 0.5   | 0.5   | 1.0   |



```r
dat2w2w <- sim_design(
  within = c(2,2),
  n = 50, 
  mu = 10,
  sd = 2,
  r = c(.5, .5, .2, 
            .2, .5, 
                .5)
)

get_params(dat2w2w)
```

<div class="kable-table">

|  n|var   | A1_B1| A1_B2| A2_B1| A2_B2|  mean|   sd|
|--:|:-----|-----:|-----:|-----:|-----:|-----:|----:|
| 50|A1_B1 |  1.00|  0.41|  0.45|  0.13| 10.67| 2.03|
| 50|A1_B2 |  0.41|  1.00| -0.08|  0.37| 10.00| 2.15|
| 50|A2_B1 |  0.45| -0.08|  1.00|  0.40| 10.44| 2.16|
| 50|A2_B2 |  0.13|  0.37|  0.40|  1.00|  9.95| 2.11|

</div>

### 2w*3b

Create a dataset with a between-subject factor of "pet" having 3 levels ("cat", "dog", and "ferret") and a within-subject factor of "time" having 2 levels ("pre" and "post"). The N in each group should be 10. Means are:

* cats: pre = 10, post = 12
* dogs: pre = 14, post = 16
* ferrets: pre = 18, post = 20

SDs are all 5 and within-cell correlations are all 0.25.


```r
mu <- data.frame(
  cat = c(10, 12),
  dog = c(14, 16),
  ferret = c(18, 20)
)

dat2w3b <- sim_design(
  within = list(time = c("pre", "post")),
  between = list(pet = c("cat", "dog", "ferret")),
  n = 10,
  mu = mu,
  sd = 5,
  r = 0.25
)

get_params(dat2w3b)
```

<div class="kable-table">

|pet    |  n|var  |  pre| post|  mean|   sd|
|:------|--:|:----|----:|----:|-----:|----:|
|cat    | 10|pre  | 1.00| 0.49|  8.68| 4.19|
|cat    | 10|post | 0.49| 1.00| 10.47| 6.35|
|dog    | 10|pre  | 1.00| 0.04| 13.60| 5.16|
|dog    | 10|post | 0.04| 1.00| 15.52| 4.52|
|ferret | 10|pre  | 1.00| 0.08| 15.17| 4.73|
|ferret | 10|post | 0.08| 1.00| 17.66| 5.88|

</div>

### Replications

Create 5 datasets with a 2b*2b design, 30 participants in each cell. Each cell's mean should be 0, except A1_B1, which should be 0.5. The SD should be 1. Make the resulting data in long format.


```r
dat2b2b <- sim_design(
  between = c(2,2),
  n = 30,
  mu = c(0.5, 0, 0, 0),
  rep = 5,
  long = TRUE
)
```

### Power 

Simulate 100 datasets like the one above and use `lm()` or `afex::aov_ez()` to look at the interaction between A and B. What is the power of this design?


```r
# simulate 100 datasets
dat2b2b_100 <- sim_design(
  between = c(2, 2),
  n = 30,
  mu = c(0.5, 0, 0, 0),
  rep = 100,
  long = TRUE
)

# linear model version
analyse_lm <- function(data) {
  lm(y ~ A*B, data = data) %>% 
    broom::tidy() %>%
    mutate(term = factor(term, levels = term))
}

ana_lm <- dat2b2b_100 %>%
  mutate(analysis = map(data, analyse_lm)) %>%
  select(-data) %>%
  unnest(analysis)

ana_lm %>%
  group_by(term) %>%
  summarise(power = mean(p.value < .05),
            .groups = "drop")
```

<div class="kable-table">

|term        | power|
|:-----------|-----:|
|(Intercept) |  0.77|
|AA2         |  0.52|
|BB2         |  0.49|
|AA2:BB2     |  0.33|

</div>


```r
# anova version
afex::set_sum_contrasts() # avoids annoying afex message
```

```
## setting contr.sum globally: options(contrasts=c('contr.sum', 'contr.poly'))
```

```r
afex_options(include_aov = FALSE) # runs faster
afex_options(es_aov = "pes") # changes effect size measure to partial eta squared

analyse_aov <- function(data) {
  a <- afex::aov_ez(id = "id",
              dv = "y",
              between = c("A", "B"),
              data = data)
  
  as_tibble(a$anova_table, rownames = "term") %>%
    mutate(term = factor(term, levels = term)) %>% 
    rename(p.value = `Pr(>F)`) 
}

ana_aov <- dat2b2b_100 %>%
  mutate(analysis = map(data, analyse_aov)) %>%
  select(-data) %>%
  unnest(analysis)

ana_aov %>%
  group_by(term) %>%
  summarise(power = mean(p.value < .05),
            .groups = "drop")
```

<div class="kable-table">

|term | power|
|:----|-----:|
|A    |  0.23|
|B    |  0.21|
|A:B  |  0.33|

</div>



## Calorie Placement Re-Simulation

### Data Source

We will be replicating some of the re-analyses in Francis & Thunell's (2020) Meta-Psychology paper: Excess success in "Don't count calorie labeling out: Calorie counts on the left side of menu items lead to lower calorie food choices".

They ran power analyses for all 6 studies in Dallas, Liu, and Ubel's (2019) study showing that people order food with significantly fewer calories when the calorie count was placed to the left of the item than to the right (or having no calorie label). They then used these power estimates to calculate the probability of all 6 out of 6 studies being significant, given the observed power of each study.

* [Re-analysis](https://doi.org/10.15626/MP.2019.2266)
* [Re-analysis code](https://osf.io/xrdhj/)
* [Original paper](https://doi.org/10.1002/jcpy.1053)

Table 1 of the re-analysis paper provides all of the parameters we will need. 

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

<div class="kable-table">

|  estimate| estimate1| estimate2| statistic|   p.value| parameter|  conf.low| conf.high|method                  |alternative |
|---------:|---------:|---------:|---------:|---------:|---------:|---------:|---------:|:-----------------------|:-----------|
| -134.2927|  1182.723|  1317.016| -2.376637| 0.0181663|  270.9239| -245.5381| -23.04728|Welch Two Sample t-test |two.sided   |

</div>


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

<div class="kable-table">

| rep|   estimate| estimate1| estimate2| statistic|   p.value| parameter|  conf.low|  conf.high|method                  |alternative |
|---:|----------:|---------:|---------:|---------:|---------:|---------:|---------:|----------:|:-----------------------|:-----------|
|   1|  -83.51288|  1268.786|  1352.299| -1.774903| 0.0770381|  270.5825| -176.1476|   9.121849|Welch Two Sample t-test |two.sided   |
|   2| -114.97521|  1263.464|  1378.439| -2.181833| 0.0299779|  271.9809| -218.7203| -11.230099|Welch Two Sample t-test |two.sided   |
|   3|  -85.44596|  1255.420|  1340.866| -1.573587| 0.1167434|  272.4557| -192.3471|  21.455179|Welch Two Sample t-test |two.sided   |
|   4| -116.93164|  1218.279|  1335.211| -2.074587| 0.0389873|  265.7669| -227.9081|  -5.955226|Welch Two Sample t-test |two.sided   |
|   5| -139.76904|  1247.966|  1387.735| -2.575934| 0.0105253|  271.9315| -246.5912| -32.946843|Welch Two Sample t-test |two.sided   |
|   6|  -72.69624|  1295.543|  1368.239| -1.311121| 0.1910297|  247.9852| -181.9011|  36.508633|Welch Two Sample t-test |two.sided   |

</div>

Summarise the `p.value` column to get power.


```r
s2_power <- s2_sim %>%
  mutate(sig = p.value < .05) %>%
  summarise(power = mean(sig)) %>%
  pull(power)
```

Compare this value (0.539) with the value in the paper (0.5426).

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

<div class="kable-table">

|     p_all|       p_1|       p_2|
|---------:|---------:|---------:|
| 0.0418833| 0.1474684| 0.0239774|

</div>

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

<div class="kable-table">

| rep|     p_all|       p_1|       p_2|
|---:|---------:|---------:|---------:|
|   1| 0.0034317| 0.0086018| 0.0027807|
|   2| 0.2595953| 0.2328832| 0.2328832|
|   3| 0.0004448| 0.0001797| 0.0506550|
|   4| 0.0676437| 0.0931488| 0.0487033|
|   5| 0.0169225| 0.0235099| 0.0235099|
|   6| 0.0650196| 0.1093398| 0.0435229|

</div>

Calculating power is a little trickier here, as all three p-values need to be significant here to support the hypothesis.


```r
s1_power <- s1_sim %>%
  mutate(sig = (p_all < .05) & 
               (p_1 < .05) & 
               (p_2 < .05)  ) %>%
  summarise(power = mean(sig)) %>%
  pull(power)
```

Compare this value (0.42) with the value in the paper (0.4582).

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

<div class="kable-table">

| rep|     p_all|       p_1|       p_2|
|---:|---------:|---------:|---------:|
|   1| 0.0262845| 0.0166075| 0.0750064|
|   2| 0.0549322| 0.0659798| 0.0659798|
|   3| 0.0291340| 0.1503155| 0.0159775|
|   4| 0.0145949| 0.0073671| 0.1366318|
|   5| 0.0122501| 0.0499609| 0.0073854|
|   6| 0.0350654| 0.0277263| 0.0561030|

</div>


```r
s3_power <- s3_sim %>%
  mutate(sig = (p_all < .05) & 
               (p_1 < .05) & 
               (p_2 < .05)  ) %>%
  summarise(power = mean(sig)) %>%
  pull(power)
```

Compare this value (0.31) with the value in the paper (0.3626).


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

### Conclusion

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

<div class="kable-table">

|study | power_ft| power_my|
|:-----|--------:|--------:|
|1     |   0.4582|    0.420|
|2     |   0.5426|    0.539|
|3     |   0.3626|    0.310|
|S1    |   0.5358|    0.547|
|S2    |   0.5667|    0.510|
|S3    |   0.4953|    0.420|

</div>

The `reduce()` function from {purrr} applies a function sequentially over a vector, so can give up the product of all the values in the power columns.


```r
prob_ft <- purrr::reduce(power_table$power_ft, `*`)
prob_my <- purrr::reduce(power_table$power_my, `*`)
```

The Francis & Thunell paper showed a 0.01 probability of getting 6 of 6 studies significant. Our re-simulation showed a 0.01 probability.

