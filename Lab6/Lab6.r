# Definicja macierzy prawdopodobieństw dla pierwszego rozkładu
macierz_prawdopodobienstw <- matrix(c(1 / 8, 1 / 4, 1 / 8, 1 / 6, 1 / 6, 1 / 6), nrow = 2, byrow = TRUE)

# Wyznaczenie rozkładów brzegowych
brzegowy_X <- rowSums(macierz_prawdopodobienstw) # Rozkład brzegowy X
brzegowy_Y <- colSums(macierz_prawdopodobienstw) # Rozkład brzegowy Y
print("Rozkład brzegowy X:")
print(brzegowy_X)
print("Rozkład brzegowy Y:")
print(brzegowy_Y)

print("Macierz prawdopodobieństw:")
print(macierz_prawdopodobienstw)
# Wyznaczenie współczynnika korelacji ρ(X,Y)
wartosci_X <- seq_len(nrow(macierz_prawdopodobienstw))
wartosci_Y <- seq_len(ncol(macierz_prawdopodobienstw))
wektor_prawdopodobienstw <- as.vector(macierz_prawdopodobienstw)
wektor_X <- rep(wartosci_X, each = length(wartosci_Y))
wektor_Y <- rep(wartosci_Y, times = length(wartosci_X))

# Obliczenia
oczekiwane_XY <- sum(wektor_X * wektor_Y * wektor_prawdopodobienstw)
oczekiwane_X <- sum(wartosci_X * rowSums(macierz_prawdopodobienstw))
oczekiwane_Y <- sum(wartosci_Y * colSums(macierz_prawdopodobienstw))
wariancja_X <- sum((wartosci_X^2) * rowSums(macierz_prawdopodobienstw)) - oczekiwane_X^2
wariancja_Y <- sum((wartosci_Y^2) * colSums(macierz_prawdopodobienstw)) - oczekiwane_Y^2

# Współczynnik korelacji
korelacja <- (oczekiwane_XY - oczekiwane_X * oczekiwane_Y) / sqrt(wariancja_X * wariancja_Y)
print("Współczynnik korelacji ρ(X, Y):")
print(korelacja)

# Wyznaczenie rozkładów warunkowych P(Y=y|X=x)
warunkowy_Y_dla_X0 <- macierz_prawdopodobienstw[1, ] / brzegowy_X[1]
warunkowy_Y_dla_X1 <- macierz_prawdopodobienstw[2, ] / brzegowy_X[2]
print("Rozkład warunkowy P(Y|X=0):")
print(warunkowy_Y_dla_X0)
print("Rozkład warunkowy P(Y|X=1):")
print(warunkowy_Y_dla_X1)

# Sprawdzenie niezależności zmiennych X i Y
sprawdzenie_niezaleznosci <- all.equal(macierz_prawdopodobienstw, outer(brzegowy_X, brzegowy_Y))
print("Czy zmienne X i Y są niezależne?")
print(sprawdzenie_niezaleznosci)

# Symulacja 1000 par realizacji metodą skumulowanych prawdopodobieństw
set.seed(123) # Ustawienie ziarna dla powtarzalności
skumulowane_prawdopodobienstwo <- cumsum(wektor_prawdopodobienstw)
losowe_wartosci <- runif(1000)
symulowane_indeksy <- findInterval(losowe_wartosci, skumulowane_prawdopodobienstwo) + 1
symulowane_X <- wektor_X[symulowane_indeksy]
symulowane_Y <- wektor_Y[symulowane_indeksy]

# Oszacowanie współczynnika korelacji na podstawie danych
korelacja_pearson <- cor(symulowane_X, symulowane_Y, method = "pearson")
korelacja_spearman <- cor(symulowane_X, symulowane_Y, method = "spearman")
korelacja_kendall <- cor(symulowane_X, symulowane_Y, method = "kendall")
print("Współczynniki korelacji na podstawie danych symulowanych:")
print(paste("Pearson:", korelacja_pearson))
print(paste("Spearman:", korelacja_spearman))
print(paste("Kendall:", korelacja_kendall))

# Tablica rozdzielcza relatywnych czestotliwosci
tablica_czestotliwosci <- table(symulowane_X, symulowane_Y) / 1000
print("Tablica rozdzielcza relatywnych czestotliwosci dla danych symulowanych:")
print(tablica_czestotliwosci)

# Drugi rozkład (oceny z Algebry i Analizy)
macierz_ocen <- matrix(
  c(
    0.05, 0.03, 0.02, 0, 0, 0,
    0.05, 0.07, 0.05, 0.03, 0, 0,
    0.03, 0.05, 0.06, 0.04, 0.02, 0,
    0.01, 0.04, 0.06, 0.06, 0.02, 0.01,
    0, 0.02, 0.05, 0.08, 0.04, 0.01,
    0, 0.01, 0.01, 0.02, 0.03, 0.03
  ),
  nrow = 6, byrow = TRUE
)


# Analogiczne kroki jak dla pierwszego rozkładu
brzegowy_ocen_X <- rowSums(macierz_ocen)
brzegowy_ocen_Y <- colSums(macierz_ocen)
print("Rozkład brzegowy ocen (X):")
print(brzegowy_ocen_X)
print("Rozkład brzegowy ocen (Y):")
print(brzegowy_ocen_Y)



print("Macierz prawdopodobieństw:")
print(macierz_ocen)
# Wyznaczenie współczynnika korelacji ρ(X,Y)
wartosci_X <- seq_len(nrow(macierz_ocen))
wartosci_Y <- seq_len(ncol(macierz_ocen))
wektor_ocen <- as.vector(macierz_ocen)
wektor_X <- rep(wartosci_X, each = length(wartosci_Y))
wektor_Y <- rep(wartosci_Y, times = length(wartosci_X))

# Obliczenia
oczekiwane_XY <- sum(wektor_X * wektor_Y * wektor_ocen)
oczekiwane_X <- sum(wartosci_X * rowSums(macierz_ocen))
oczekiwane_Y <- sum(wartosci_Y * colSums(macierz_ocen))
wariancja_X <- sum((wartosci_X^2) * rowSums(macierz_ocen)) - oczekiwane_X^2
wariancja_Y <- sum((wartosci_Y^2) * colSums(macierz_ocen)) - oczekiwane_Y^2

korelacja <- (oczekiwane_XY - oczekiwane_X * oczekiwane_Y) / sqrt(wariancja_X * wariancja_Y)
print("Współczynnik korelacji ρ(X, Y):")
print(korelacja)

# Symulacja 1000 par realizacji dla drugiego rozkładu
set.seed(123)
wartosci_ocen_X <- c(2, 3, 3.5, 4, 4.5, 5)
wartosci_ocen_Y <- c(2, 3, 3.5, 4, 4.5, 5)
wektor_prawdopodobienstw_ocen <- as.vector(macierz_ocen)
skumulowane_prawdopodobienstwo_ocen <- cumsum(wektor_prawdopodobienstw_ocen)
losowe_wartosci_ocen <- runif(1000)
symulowane_indeksy_ocen <- findInterval(losowe_wartosci_ocen, skumulowane_prawdopodobienstwo_ocen) + 1
symulowane_oceny_X <- rep(wartosci_ocen_X, each = length(wartosci_ocen_Y))[symulowane_indeksy_ocen]
symulowane_oceny_Y <- rep(wartosci_ocen_Y, times = length(wartosci_ocen_X))[symulowane_indeksy_ocen]

# Tablica rozdzielcza relatywnych czestotliwosci dla drugiego rozkładu
tablica_czestotliwosci_ocen <- table(symulowane_oceny_X, symulowane_oceny_Y) / 1000
print("Tablica rozdzielcza relatywnych czestotliwosci dla ocen:")
print(tablica_czestotliwosci_ocen)

# Współczynniki korelacji dla drugiego rozkładu
korelacja_pearson_ocen <- cor(symulowane_oceny_X, symulowane_oceny_Y, method = "pearson")
korelacja_spearman_ocen <- cor(symulowane_oceny_X, symulowane_oceny_Y, method = "spearman")
korelacja_kendall_ocen <- cor(symulowane_oceny_X, symulowane_oceny_Y, method = "kendall")
print("Współczynniki korelacji dla ocen:")
print(paste("Pearson:", korelacja_pearson_ocen))
print(paste("Spearman:", korelacja_spearman_ocen))
print(paste("Kendall:", korelacja_kendall_ocen))
