library(covidseir)
library(dplyr)
library(ggplot2)

cases <- c(
  0, 0, 1, 3, 1, 8, 0, 6, 5, 0, 7, 7, 18, 9, 22, 38, 53, 45, 40,
  77, 76, 48, 67, 78, 42, 66, 67, 92, 16, 70, 43, 53, 55, 53, 29,
  26, 37, 25, 45, 34, 40, 35
)

## create m
# Example fixed sample fractions:
s1 <- c(rep(0.1, 13), rep(0.2, length(cases) - 13))
# Only using 150 iterations and 1 chain for a quick example:
m <- fit_seir(
  cases,
  iter = 150,
  chains = 1,
  samp_frac_fixed = s1
)
print(m)

## create p and states (which is a wide form of p)

p <- project_seir(m, return_states = TRUE)
states <- p %>%
  select(-variable_num) %>%
  tidyr::pivot_wider(names_from = variable, values_from = value)
states

## now explore how to make Rt from this

# functional form
Rt <- function(m, states, lag = 30) {
  k1 <- m$pars["k1"]
  lag <- 30
  lag_avail <- states$time - min(states$time)
  inds <- which(lag_avail >= lag)
  dt <- states$time[2] - states$time[1]
  numback <- lag / (dt) - 1
  denominator <- vector(length = nrow(states))
  E1tot <- states$E1 + states$E1d
  for (i in inds) {
    pastinds <- i - (1:numback)
    pastE1 <- E1tot[pastinds]
    s <- seq(from = 0, by = dt, length.out = length(pastinds))
    integrand <- k1 * pastE1 * exp(-k1 * s)
    trapezoid_weights <- rep(1, length(integrand))
    trapezoid_weights[1] <- 0.5
    trapezoid_weights[length(trapezoid_weights)] <- 0.5
    denominator[i] <- dt * sum(integrand * trapezoid_weights)
  }
  denominator[denominator == 0] <- NA
  tibble::tibble(Rt = E1tot / denominator)
}

# test this function
# testrt <- Rt(m,states,30)

states$Rt <- dplyr::group_by(states, .iteration) %>%
  dplyr::group_split() %>%
  purrr::map_dfr(Rt, m = m) %>%
  dplyr::pull(Rt)

ggplot(data = states, aes(x = time, y = testrt, group = .iteration)) +
  geom_line(alpha = 0.1) +
  geom_vline(xintercept = m$post$start_decline, alpha = 0.1, colour = "red") +
  geom_vline(xintercept = m$post$end_decline, alpha = 0.1, colour = "blue") +
  geom_hline(yintercept = m$post$R0, col = "darkgreen", alpha = 0.1)
