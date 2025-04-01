# 1. Generowanie realizacji z rozkładów
# a) 5000 realizacji z rozkładu jednostajnego na [0,1]
jednostajne_probki <- runif(5000, min = 0, max = 1)

# b) 3000 realizacji z rozkładu normalnego (średnia 100, odchylenie standardowe 15)
normalne_probki <- rnorm(3000, mean = 100, sd = 15)

# Estymator gęstości rozkładu
# i) Histogram
# hist(jednostajne_probki, main = "Histogram - Rozkład jednostajny", xlab = "Wartości", col = "lightblue")
# hist(normalne_probki, main = "Histogram - Rozkład normalny", xlab = "Wartości", col = "lightgreen")

# ii) Estymator jądrowy
gestosc_jednostajny <- density(jednostajne_probki)
gestosc_normalny <- density(normalne_probki)

# plot(gestosc_jednostajny, main = "Estymator jądrowy - Rozkład jednostajny", col = "blue")
# plot(gestosc_normalny, main = "Estymator jądrowy - Rozkład normalny", col = "green")

# 2. Symulacja 600 rzutów kostką
# a) Przekształcenie realizacji z rozkładu jednostajnego na [0,1]
rzuty_kostka <- floor(runif(600, min = 1, max = 7)) # Losowanie liczb od 1 do 6

# Średnia i wariancja
srednia_kostka <- mean(rzuty_kostka)
wariancja_kostka <- var(rzuty_kostka)

# Porównanie z wartością oczekiwaną i wariancją
oczekiwana_srednia <- 3.5
oczekiwana_wariancja <- 35 / 12

# Rozkład częstości
czestosc_tabela <- table(rzuty_kostka)
czestosc_df <- as.data.frame(czestosc_tabela)

# Wyświetlenie ramki danych i wariancji częstości
print(czestosc_df)
wariancja_czestosc <- var(czestosc_tabela)

# b) Symulacja 600 rzutów kostką za pomocą "sample"
rzuty_kostka_sample <- sample(1:6, size = 600, replace = TRUE)

# 3. Generowanie 1000 liczb z rozkładu dyskretnego
# k: 0, 1, 2, 3; P(X=k): 0.15, 0.25, 0.5, 0.1
k_wartosci <- c(0, 1, 2, 3)
prawdopodobienstwa <- c(0.15, 0.25, 0.5, 0.1)
dyskretne_probki <- sample(k_wartosci, size = 1000, replace = TRUE, prob = prawdopodobienstwa)

# 4. Rozkład Bin(10, 0.3)
dwumianowe_probki <- rbinom(100, size = 10, prob = 0.3)

# 5. Rozkład Geom(0.4)
geometryczne_probki <- rgeom(50, prob = 0.4)

# 6. Rozkład o gęstości g(x) = 0.5x dla 0 ≤ x ≤ 2
# a) Metoda odwracania dystrybuanty
odwrotna_dystrybuanta <- function() {
  u <- runif(1)
  sqrt(2 * u)
}

odwrotne_probki <- replicate(200, odwrotna_dystrybuanta())

# b) Metoda przyjęcia i odrzucenia
przyjecie_odrzucenie <- function() {
  repeat {
    x <- runif(1, min = 0, max = 2)
    y <- runif(1, min = 0, max = 1)
    if (y <= 0.5 * x) {
      return(x)
    }
  }
}

przyjecie_probki <- replicate(200, przyjecie_odrzucenie())

# Wykresy dla obu metod
# hist(odwrotne_probki, main = "Metoda odwracania dystrybuanty", col = "lightblue")
# hist(przyjecie_probki, main = "Metoda przyjęcia i odrzucenia", col = "lightgreen")

# 1. Generowanie realizacji z rozkładów
cat("1. Generowanie realizacji z rozkładów:\n")
cat("a) 5000 realizacji z rozkładu jednostajnego na [0,1]:\n")
cat("Pierwsze 10 wartości:\n", head(jednostajne_probki, 10), "\n\n")

cat("b) 3000 realizacji z rozkładu normalnego (średnia 100, odchylenie standardowe 15):\n")
cat("Pierwsze 10 wartości:\n", head(normalne_probki, 10), "\n\n")

# 2. Symulacja 600 rzutów kostką
cat("2. Symulacja 600 rzutów kostką:\n")
cat("a) Przekształcenie realizacji z rozkładu jednostajnego na [0,1]:\n")
cat("Średnia z próby:", srednia_kostka, "\n")
cat("Wariancja z próby:", wariancja_kostka, "\n")
cat("Wartość oczekiwana:", oczekiwana_srednia, "\n")
cat("Wariancja teoretyczna:", oczekiwana_wariancja, "\n\n")

cat("Rozkład częstości wyników:\n")
print(czestosc_tabela)
cat("\nRamka danych z rozkładem częstości:\n")
print(czestosc_df)
cat("\nWariancja częstości wyników:", wariancja_czestosc, "\n\n")

cat("b) Symulacja 600 rzutów kostką za pomocą 'sample':\n")
cat("Pierwsze 10 wyników:\n", head(rzuty_kostka_sample, 10), "\n\n")

# 3. Generowanie 1000 liczb z rozkładu dyskretnego
cat("3. Generowanie 1000 liczb z rozkładu dyskretnego:\n")
cat("Pierwsze 10 wartości:\n", head(dyskretne_probki, 10), "\n\n")

# 4. Rozkład Bin(10, 0.3)
cat("4. Rozkład Bin(10, 0.3):\n")
cat("Pierwsze 10 wartości:\n", head(dwumianowe_probki, 10), "\n\n")

# 5. Rozkład Geom(0.4)
cat("5. Rozkład Geom(0.4):\n")
cat("Pierwsze 10 wartości:\n", head(geometryczne_probki, 10), "\n\n")

# 6. Rozkład o gęstości g(x) = 0.5x dla 0 ≤ x ≤ 2
cat("6. Rozkład o gęstości g(x) = 0.5x dla 0 ≤ x ≤ 2:\n")
cat("a) Metoda odwracania dystrybuanty:\n")
cat("Pierwsze 10 wartości:\n", head(odwrotne_probki, 10), "\n\n")

cat("b) Metoda przyjęcia i odrzucenia:\n")
cat("Pierwsze 10 wartości:\n", head(przyjecie_probki, 10), "\n\n")
