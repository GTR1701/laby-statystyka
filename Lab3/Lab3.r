# Zad1 Rzucono monetą 6 razy. Niech X będzie liczbą reszek.
# i) P(X = 5)

p11 <- dbinom(5, size = 6, prob = 0.5)
print(paste("P(X = 5):", p11))

# ii) P(X >= 3)
p12 <- sum(dbinom(3:6, size = 6, prob = 0.5))
print(paste("P(X ≥ 3):", p12))

# iii) P(2 <= X <= 4)
p13 <- sum(dbinom(2:4, size = 6, prob = 0.5))
print(paste("P(2 ≤ X ≤ 4):", p13))

# iv) Narysować wykres rozkładu zmiennej X (funkcja: plot, type=”h”)
x <- 0:6
y <- dbinom(x, size = 6, prob = 0.5)
# plot(x, y, type = "h", main = "Rozkład zmiennej X", xlab = "Liczba reszek", ylab = "Prawdopodobieństwo")

# Zad 2
# P(X = 5)
tygodnie <- 2
p21 <- dpois(5, lambda = 3 * tygodnie)
print(paste("P(X = 5):", p21))

# P(X ≥ 4)

p22 <- sum(dpois(4:(3 * tygodnie), lambda = 3 * tygodnie))
print(paste("P(X ≥ 4):", p22))

# P(3 ≤ X ≤ 5)

p23 <- sum(dpois(3:5, lambda = 3 * tygodnie))
print(paste("P(3 ≤ X ≤ 5):", p23))

# (Tylko na komputerze) Narysować wykres rozkładu zmiennej X dla 0≤x≤30.

x <- 0:30
y <- dpois(x, lambda = 3 * tygodnie)
# plot(x, y, type = "h", main = "Rozkład zmiennej X", xlab = "Liczba sprzedanych samochodów", ylab = "Prawdopodobieństwo")

# Zad 3
# Wyznaczyć E(X), Var(X)

x <- c(1, 2, 3, 4)
p <- c(0.2, 0.4, 0.3, 0.1)

EX <- sum(x * p)
print(paste("E(X):", EX))

EX2 <- sum(x^2 * p)
VarX <- EX2 - EX^2

print(paste("Var(X):", VarX))

# Zad 4
# P(X = 27)

p41 <- dbinom(27, size = 180, prob = 1/6)
print(paste("P(X = 27):", p41))

# P(X ≥ 32)

p42 <- sum(dbinom(32:180, size = 180, prob = 1/6))
print(paste("P(X ≥ 32):", p42))

# P(X < 29)

p43 <- sum(dbinom(0:28, size = 180, prob = 1/6))
print(paste("P(X < 29):", p43))

# P(25 ≤ X ≤ 33)

p44 <- sum(dbinom(25:33, size = 180, prob = 1/6))
print(paste("P(25 ≤ X ≤ 33):", p44))

# Zad 5
# i)	P(X = 16)

p51 <- dpois(16, lambda = 3.5 * 5)
print(paste("P(X = 16):", p51))

# ii)	P(X ≥ 20)

p52 <- sum(dpois(20:100, lambda = 3.5 * 5))
print(paste("P(X ≥ 20):", p52))

# iii)	P(X < 12)

p53 <- sum(dpois(0:11, lambda = 3.5 * 5))
print(paste("P(X < 12):", p53))

# iv)	P(14 ≤ X < 22) 

p54 <- sum(dpois(14:21, lambda = 3.5 * 5))
print(paste("P(14 ≤ X < 22):", p54))

# Zad 6 
# a) Niech X~Binom(100;0,02). Narysować wykres rozkładu zmiennej X.

x <- 0:10
y <- dbinom(x, size = 100, prob = 0.02)
plot(x, y, type = "h", main = "Rozkład zmiennej X", xlab = "Liczba sukcesów", ylab = "Prawdopodobieństwo")

# b) Niech Y~Poisson(2). Nałożyć wykres rozkładu zmiennej Y na wykres rozkładu zmiennej X

y <- 0:10
y <- dpois(y, lambda = 2)
lines(y, col = "red")
