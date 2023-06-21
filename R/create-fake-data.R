# Create fake data
# https://www.r-bloggers.com/2021/05/how-to-generate-correlated-data-in-r/

set.seed(123)

library(dplyr)
library(GGally)
library(MASS)
library(tibble)

set.seed(5)
# create the variance covariance matrix
sigma <- rbind(c(1,-0.8,-0.7), c(-0.8, 1, 0.9), c(-0.7, 0.9, 1))
# create the mean vector
mu <- c(230, 100, 83)

# generate the multivariate normal distribution
dat <- as.data.frame(mvrnorm(n = 50, mu = mu, Sigma = sigma))

dat <-
  dat %>%
  mutate(MyNoisyBinary = ifelse(
    V1 > median(V1),
    sample(c(0, 1), n(), replace = TRUE, p = c(0.6, 0.4)) ,
    sample(c(0, 1), n(), replace = TRUE, p = c(0.75, 0.25))
  )) |>
  mutate(material =
           case_when(MyNoisyBinary == 1 ~ "Liquid",
                     TRUE ~ "Tungsten")) |>
  mutate(MyNoisyBinary2 = ifelse(
    V1 > median(V1),
    sample(c(0, 1), n(), replace = TRUE, p = c(0.6, 0.4)) ,
    sample(c(0, 1), n(), replace = TRUE, p = c(0.75, 0.25))
  )) |>
  mutate(diameter =
           case_when(MyNoisyBinary2 == 1 ~ "118",
                     TRUE ~ "156")) |>
  mutate(MyNoisyBinary3 = ifelse(
    V1 > median(V1),
    sample(c(0, 1), n(), replace = TRUE, p = c(0.6, 0.4)) ,
    sample(c(0, 1), n(), replace = TRUE, p = c(0.75, 0.25))
  )) |>
  mutate(dimples =
           case_when(MyNoisyBinary3 == 1 ~ "392",
                     TRUE ~ "422"))  |>
  mutate(MyNoisyBinary4 = ifelse(
    V1 > median(V1),
    sample(c(0, 1), n(), replace = TRUE, p = c(0.6, 0.4)) ,
    sample(c(0, 1), n(), replace = TRUE, p = c(0.75, 0.25))
  )) |>
  mutate(thickness =
           case_when(MyNoisyBinary4 == 1 ~ "0.03",
                     TRUE ~ "0.0"))

dat <-
  dat %>%
  add_column(runif = runif(nrow(.), -40, 40)) |>
  mutate(iron = V1)

ggpairs(dat)

golf_dat <-
  dat |>
  dplyr::select(iron, material, dimples, diameter, thickness)

readr::write_csv(golf_dat, "data/processed/golf_dat.csv")