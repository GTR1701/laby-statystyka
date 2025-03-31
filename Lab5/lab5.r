# 1. Generowanie realizacji z rozkładów
# a) 5000 realizacji z rozkładu jednostajnego na [0,1]
uniform_samples <- runif(5000, min = 0, max = 1)

# b) 3000 realizacji z rozkładu normalnego (średnia 100, odchylenie standardowe 15)
normal_samples <- rnorm(3000, mean = 100, sd = 15)

# Estymator gęstości rozkładu
# i) Histogram
# hist(uniform_samples, main = "Histogram - Rozkład jednostajny", xlab = "Wartości", col = "lightblue")
# hist(normal_samples, main = "Histogram - Rozkład normalny", xlab = "Wartości", col = "lightgreen")

# ii) Estymator jądrowy
density_uniform <- density(uniform_samples)
density_normal <- density(normal_samples)

# plot(density_uniform, main = "Estymator jądrowy - Rozkład jednostajny", col = "blue")
# plot(density_normal, main = "Estymator jądrowy - Rozkład normalny", col = "green")

# 2. Symulacja 600 rzutów kostką
# a) Przekształcenie realizacji z rozkładu jednostajnego na [0,1]
dice_rolls <- floor(runif(600, min = 1, max = 7)) # Losowanie liczb od 1 do 6

# Średnia i wariancja
mean_dice <- mean(dice_rolls)
var_dice <- var(dice_rolls)

# Porównanie z wartością oczekiwaną i wariancją
expected_mean <- 3.5
expected_variance <- 35 / 12

# Rozkład częstości
freq_table <- table(dice_rolls)
freq_df <- as.data.frame(freq_table)

# Wyświetlenie ramki danych i wariancji częstości
print(freq_df)
freq_variance <- var(freq_table)

# b) Symulacja 600 rzutów kostką za pomocą "sample"
dice_rolls_sample <- sample(1:6, size = 600, replace = TRUE)

# 3. Generowanie 1000 liczb z rozkładu dyskretnego
# k: 0, 1, 2, 3; P(X=k): 0.15, 0.25, 0.5, 0.1
k_values <- c(0, 1, 2, 3)
probabilities <- c(0.15, 0.25, 0.5, 0.1)
discrete_samples <- sample(k_values, size = 1000, replace = TRUE, prob = probabilities)

# 4. Rozkład Bin(10, 0.3)
binomial_samples <- rbinom(100, size = 10, prob = 0.3)

# 5. Rozkład Geom(0.4)
geometric_samples <- rgeom(50, prob = 0.4)

# 6. Rozkład o gęstości g(x) = 0.5x dla 0 ≤ x ≤ 2
# a) Metoda odwracania dystrybuanty
inverse_transform <- function() {
  u <- runif(1)
  sqrt(2 * u)
}

inverse_samples <- replicate(200, inverse_transform())

# b) Metoda przyjęcia i odrzucenia
accept_reject <- function() {
  repeat {
    x <- runif(1, min = 0, max = 2)
    y <- runif(1, min = 0, max = 1)
    if (y <= 0.5 * x) {
      return(x)
    }
  }
}

accept_reject_samples <- replicate(200, accept_reject())

# Wykresy dla obu metod
# hist(inverse_samples, main = "Metoda odwracania dystrybuanty", col = "lightblue")
# hist(accept_reject_samples, main = "Metoda przyjęcia i odrzucenia", col = "lightgreen")

# 1. Generowanie realizacji z rozkładów
cat("1. Generowanie realizacji z rozkładów:\n")
cat("a) 5000 realizacji z rozkładu jednostajnego na [0,1]:\n")
cat("Pierwsze 10 wartości:\n", head(uniform_samples, 10), "\n\n")

cat("b) 3000 realizacji z rozkładu normalnego (średnia 100, odchylenie standardowe 15):\n")
cat("Pierwsze 10 wartości:\n", head(normal_samples, 10), "\n\n")

# 2. Symulacja 600 rzutów kostką
cat("2. Symulacja 600 rzutów kostką:\n")
cat("a) Przekształcenie realizacji z rozkładu jednostajnego na [0,1]:\n")
cat("Średnia z próby:", mean_dice, "\n")
cat("Wariancja z próby:", var_dice, "\n")
cat("Wartość oczekiwana:", expected_mean, "\n")
cat("Wariancja teoretyczna:", expected_variance, "\n\n")

cat("Rozkład częstości wyników:\n")
print(freq_table)
cat("\nRamka danych z rozkładem częstości:\n")
print(freq_df)
cat("\nWariancja częstości wyników:", freq_variance, "\n\n")

cat("b) Symulacja 600 rzutów kostką za pomocą 'sample':\n")
cat("Pierwsze 10 wyników:\n", head(dice_rolls_sample, 10), "\n\n")

# 3. Generowanie 1000 liczb z rozkładu dyskretnego
cat("3. Generowanie 1000 liczb z rozkładu dyskretnego:\n")
cat("Pierwsze 10 wartości:\n", head(discrete_samples, 10), "\n\n")

# 4. Rozkład Bin(10, 0.3)
cat("4. Rozkład Bin(10, 0.3):\n")
cat("Pierwsze 10 wartości:\n", head(binomial_samples, 10), "\n\n")

# 5. Rozkład Geom(0.4)
cat("5. Rozkład Geom(0.4):\n")
cat("Pierwsze 10 wartości:\n", head(geometric_samples, 10), "\n\n")

# 6. Rozkład o gęstości g(x) = 0.5x dla 0 ≤ x ≤ 2
cat("6. Rozkład o gęstości g(x) = 0.5x dla 0 ≤ x ≤ 2:\n")
cat("a) Metoda odwracania dystrybuanty:\n")
cat("Pierwsze 10 wartości:\n", head(inverse_samples, 10), "\n\n")

cat("b) Metoda przyjęcia i odrzucenia:\n")
cat("Pierwsze 10 wartości:\n", head(accept_reject_samples, 10), "\n\n")