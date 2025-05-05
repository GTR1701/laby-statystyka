# Zadanie 1: Rozkład normalny
mean_X <- 170
sd_X <- sqrt(144)

# i) P(X < 164)
p1 <- pnorm(164, mean = mean_X, sd = sd_X)

# ii) P(X > 188)
p2 <- 1 - pnorm(188, mean = mean_X, sd = sd_X)

# iii) P(158 < X < 185)
p3 <- pnorm(185, mean = mean_X, sd = sd_X) - pnorm(158, mean = mean_X, sd = sd_X)

# iv) Wzrost k, taki że 15% populacji ma wzrost większy od k
k <- qnorm(0.85, mean = mean_X, sd = sd_X)

# Zadanie 2: Generowanie liczb z rozkładu normalnego standardowego
set.seed(123)
U1 <- runif(10000)
U2 <- runif(10000)
Z <- cos(2 * pi * U1) * sqrt(-2 * log(U2))

# Histogram dla Z
hist(Z,
  breaks = 50, probability = TRUE, col = "lightblue",
  main = "Histogram Z z nałożoną gęstością N(0,1)",
  xlab = "Z"
)
curve(dnorm(x, mean = 0, sd = 1), col = "red", lwd = 2, add = TRUE)

# Estymator jądrowy gęstości dla Z
library(ggplot2)
library(MASS)
density_Z <- density(Z)
ggplot(data.frame(x = density_Z$x, y = density_Z$y), aes(x, y)) +
  geom_line(color = "blue") +
  stat_function(fun = dnorm, color = "red") +
  ggtitle("Estymator jądrowy gęstości Z vs gęstość N(0,1)")

# iii) Y = 100 + 15Z
Y <- 100 + 15 * Z

# Histogram dla Y
hist(Y,
  breaks = 50, probability = TRUE, col = "lightgreen",
  main = "Histogram Y z nałożoną gęstością N(100,15)",
  xlab = "Y"
)
curve(dnorm(x, mean = 100, sd = 15), col = "red", lwd = 2, add = TRUE)

density_Y <- density(Y)
ggplot(data.frame(x = density_Y$x, y = density_Y$y), aes(x, y)) +
  geom_line(color = "blue") +
  stat_function(fun = function(x) dnorm(x, mean = 100, sd = 15), color = "red") +
  ggtitle("Estymator jądrowy gęstości Y vs gęstość N(100,15)")

# Zadanie 3: Rozkład normalny standardowy
set.seed(123)
n <- 2000
X <- rnorm(n, mean = mean_X, sd = sd_X)
Z <- (X - mean_X) / sd_X

# Histogram dla Z
hist(Z,
  breaks = 50, probability = TRUE, col = "lightcoral",
  main = "Histogram Z (n = 2000) z nałożoną gęstością N(0,1)",
  xlab = "Z"
)
curve(dnorm(x, mean = 0, sd = 1), col = "red", lwd = 2, add = TRUE)

density_Z <- density(Z)
ggplot(data.frame(x = density_Z$x, y = density_Z$y), aes(x, y)) +
  geom_line(color = "blue") +
  stat_function(fun = dnorm, color = "red") +
  ggtitle("Estymator jądrowy gęstości Z vs gęstość N(0,1)")

# Powtórzenie dla n = 500 i n = 100
for (n in c(500, 100)) {
  X <- rnorm(n, mean = mean_X, sd = sd_X)
  Z <- (X - mean_X) / sd_X

  # Histogram dla Z
  hist(Z,
    breaks = 50, probability = TRUE, col = "lightpink",
    main = paste("Histogram Z (n =", n, ") z nałożoną gęstością N(0,1)"),
    xlab = "Z"
  )
  curve(dnorm(x, mean = 0, sd = 1), col = "red", lwd = 2, add = TRUE)

  density_Z <- density(Z)
  ggplot(data.frame(x = density_Z$x, y = density_Z$y), aes(x, y)) +
    geom_line(color = "blue") +
    stat_function(fun = dnorm, color = "red") +
    ggtitle(paste("Estymator jądrowy gęstości Z (n =", n, ") vs gęstość N(0,1)"))
}

# Zadanie 4: Centralne twierdzenie graniczne
set.seed(123)
lambda <- 0.5
n_values <- c(1, 20, 200)
for (n in n_values) {
  S_n <- replicate(1000, sum(rexp(n, rate = lambda)))
  Z_n <- (S_n - n / lambda) / sqrt(n / lambda^2)

  # Histogram dla Z_n
  hist(Z_n,
    breaks = 50, probability = TRUE, col = "lightyellow",
    main = paste("Histogram Z_n (n =", n, ") z nałożoną gęstością N(0,1)"),
    xlab = "Z_n"
  )
  curve(dnorm(x, mean = 0, sd = 1), col = "red", lwd = 2, add = TRUE)

  density_Z_n <- density(Z_n)
  ggplot(data.frame(x = density_Z_n$x, y = density_Z_n$y), aes(x, y)) +
    geom_line(color = "blue") +
    stat_function(fun = dnorm, color = "red") +
    ggtitle(paste("Estymator jądrowy Z_n (n =", n, ") vs gęstość N(0,1)"))
}

# Zadanie 5: Rozkład dwumianowy
set.seed(1234)
binomial_cases <- list(
  list(n = 30, p = 0.5),
  list(n = 1000, p = 0.5),
  list(n = 30, p = 0.05),
  list(n = 1000, p = 0.05)
)

for (case in binomial_cases) {
  n <- case$n
  p <- case$p
  realizations <- rbinom(10000, size = n, prob = p)
  mean_bin <- n * p
  sd_bin <- sqrt(n * p * (1 - p))
  x_vals <- 0:(2 * n * p)
  rel_freq <- table(factor(realizations, levels = x_vals)) / length(realizations)

  # Histogram dla rozkładu dwumianowego
  bar_centers <- barplot(rel_freq,
    names.arg = x_vals, col = "lightblue", main = paste("Histogram Bin(n =", n, ", p =", p, ")"),
    xlab = "x", ylab = "Relatywna frekwencja"
  )
  lines(bar_centers, dnorm(x_vals, mean = mean_bin, sd = sd_bin), col = "red", lwd = 2)
}
