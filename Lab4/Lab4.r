# Zmienna X ma rozkład jednostajny na przedziale [4; 12]
# i) P(X < 7)
p1 <- punif(7, min = 4, max = 12)
print(paste("P(X < 7):", p1))

# ii) P(5 < X < 11)
p2 <- punif(11, min = 4, max = 12) - punif(5, min = 4, max = 12)
print(paste("P(5 < X < 11):", p2))

# iii) P(X > 10)
p3 <- 1 - punif(10, min = 4, max = 12)
print(paste("P(X > 10):", p3))

# iv) Wyznaczyć x taki, że P(X > x) = 0.6
x <- qunif(0.4, min = 4, max = 12)
print(paste("x taki, że P(X > x) = 0.6:", x))

# Telefony przychodzą do pewnej centrali losowo z stałą intensywnością 4 na minutę
lambda <- 4
# T jest czasem między dwoma telefonami, czyli ma rozkład wykładniczy z parametrem lambda

# P(T > 30s)
p4 <- pexp(30 / 60, rate = lambda, lower.tail = FALSE)
print(paste("P(T > 30s):", p4))

# P(T < 20s)
p5 <- pexp(20 / 60, rate = lambda)
print(paste("P(T < 20s):", p5))

# P(40s < T < 80s)
p6 <- pexp(80 / 60, rate = lambda) - pexp(40 / 60, rate = lambda)
print(paste("P(40s < T < 80s):", p6))

# Wyznaczyć czas t taki, że P(T > t) = 0.2
t <- qexp(0.2, rate = lambda, lower.tail = FALSE)
print(paste("t taki, że P(T > t) = 0.2:", t))

# Narysować wykres gęstości zmiennej T na przedziale 0 ≤ t ≤ 3
t_values <- seq(0, 3, by = 0.01)
density_T <- dexp(t_values, rate = lambda)
plot(t_values, density_T, type = "l", main = "Gęstość zmiennej T", xlab = "t", ylab = "g(t)")

# Czas do pierwszej usterki ma rozkład wykładniczy z parametrem intensywności 1 / 3
lambda2 <- 1 / 3

# P(T > 2 lata)
p7 <- pexp(2, rate = lambda2, lower.tail = FALSE)
print(paste("P(T > 2 lata):", p7))

# P(T < 4 lata)
p8 <- pexp(4, rate = lambda2)
print(paste("P(T < 4 lata):", p8))

# P(3 < T < 5 lat)
p9 <- pexp(5, rate = lambda2) - pexp(3, rate = lambda2)
print(paste("P(3 < T < 5 lat):", p9))

# Wyznaczyć czas t taki, że P(T < t) = 0.4
t2 <- qexp(0.4, rate = lambda2)
print(paste("t taki, że P(T < t) = 0.4:", t2))

# Wzrost studentów X ma rozkład normalny z wartością oczekiwaną 170cm i odchyleniem standardowym 12cm
mean_X <- 170
sd_X <- 12

# P(X > 180)
p10 <- pnorm(180, mean = mean_X, sd = sd_X, lower.tail = FALSE)
print(paste("P(X > 180):", p10))

# P(X < 165)
p11 <- pnorm(165, mean = mean_X, sd = sd_X)
print(paste("P(X < 165):", p11))

# P(155 < X < 190)
p12 <- pnorm(190, mean = mean_X, sd = sd_X) - pnorm(155, mean = mean_X, sd = sd_X)
print(paste("P(155 < X < 190):", p12))

# Narysować wykres gęstości zmiennej X na przedziale 130 ≤ x ≤ 210
x_values <- seq(130, 210, by = 0.01)
density_X <- dnorm(x_values, mean = mean_X, sd = sd_X)
# plot(x_values, density_X, type = "l", main = "Gęstość zmiennej X", xlab = "x", ylab = "g(x)")