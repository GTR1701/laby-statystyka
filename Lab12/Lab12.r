# Dane
polacy <- c(68, 80, 74, 62)
brytyjczycy <- c(85, 67, 79, 73)
chinczycy <- c(60, 66, 57)

waga <- c(polacy, brytyjczycy, chinczycy)
kraj <- factor(c(
  rep("Polacy", length(polacy)),
  rep("Brytyjczycy", length(brytyjczycy)),
  rep("Chinczycy", length(chinczycy))
))
dane <- data.frame(waga, kraj)

# Liczba grup i liczba wszystkich obserwacji
k <- length(unique(kraj))
n <- length(waga)

# Średnie
srednia_globalna <- mean(waga)
srednie_grup <- tapply(waga, kraj, mean)

# Suma kwadratów między grupami (MG)
MG <- sum(tapply(waga, kraj, function(x) length(x) * (mean(x) - srednia_globalna)^2))

# Suma kwadratów wewnątrz grup (WG)
WG <- sum(tapply(waga, kraj, function(x) sum((x - mean(x))^2)))

# Suma kwadratów całkowita (C)
C <- sum((waga - srednia_globalna)^2)

# Stopnie swobody
df_MG <- k - 1
df_WG <- n - k
df_C <- n - 1

# Średnie kwadratów
MS_MG <- MG / df_MG
MS_WG <- WG / df_WG
MS_C <- C / df_C

# Wyniki w formie tabeli
tablica <- data.frame(
  "Suma kwadratów" = c(MG, WG, C),
  "Stopnie swobody" = c(df_MG, df_WG, df_C),
  "Średnia kwadratów" = c(MS_MG, MS_WG, MS_C),
  row.names = c("MG", "WG", "C")
)
print(tablica)

F_stat <- MS_MG / MS_WG
cat("Statystyka F =", F_stat, "\n")

alpha <- 0.05
F_critical <- qf(1 - alpha, df_MG, df_WG)
cat("Wartość krytyczna F dla alfa=0.05:", F_critical, "\n")

if (F_stat > F_critical) {
  cat("Odrzucamy H0: waga zależy od narodowości.\n")
} else {
  cat("Brak podstaw do odrzucenia H0: waga nie zależy od narodowości.\n")
}

model <- aov(waga ~ kraj, data = dane)
wynik <- summary(model)
print(wynik)

# Interpretacja wyniku testu ANOVA
p_value <- wynik[[1]][["Pr(>F)"]][1]

if (p_value < 0.05) {
  cat("p-value =", p_value, "\n")
  cat("Odrzucamy hipotezę zerową: waga zależy od narodowości.\n")
} else {
  cat("p-value =", p_value, "\n")
  cat("Brak podstaw do odrzucenia hipotezy zerowej: waga nie zależy od narodowości.\n")
}

# 2a) Analiza wariancji: czy metraż zależy od dzielnicy?
mieszkania <- read.csv("Lab11/mieszkania.csv", header = TRUE, sep = ";", stringsAsFactors = TRUE)
model_metraz <- aov(Metraz ~ Dzielnica, data = mieszkania)
wynik_metraz <- summary(model_metraz)
print(wynik_metraz)

# Interpretacja wyniku testu ANOVA dla metrażu
p_value_metraz <- wynik_metraz[[1]][["Pr(>F)"]][1]
if (p_value_metraz < 0.05) {
  cat("p-value =", p_value_metraz, "\n")
  cat("Odrzucamy hipotezę zerową: metraż zależy od dzielnicy.\n")
} else {
  cat("p-value =", p_value_metraz, "\n")
  cat("Brak podstaw do odrzucenia hipotezy zerowej: metraż nie zależy od dzielnicy.\n")
}

# 2b) Porównanie parami - test post-hoc Tukeya
tukey <- TukeyHSD(model_metraz)
print(tukey)

# Średni metraż w każdej dzielnicy
srednie_metraz <- aggregate(Metraz ~ Dzielnica, data = mieszkania, mean)
print(srednie_metraz)

# 3a) Kategoryzacja mieszkań według liczby pokoi
mieszkania$kategoria_pokoje <- cut(
  mieszkania$Pokoje,
  breaks = c(-Inf, 1, 2, 3, Inf),
  labels = c("1-pokojowe", "2-pokojowe", "3-pokojowe", "wielopokojowe"),
  right = TRUE
)

# 3b) Cena za m2
mieszkania$cena_m2 <- mieszkania$Cena / mieszkania$Metraz

# Analiza wariancji: czy cena za m2 zależy od liczby pokoi (kategorii)
model_cena <- aov(cena_m2 ~ kategoria_pokoje, data = mieszkania)
wynik_cena <- summary(model_cena)
print(wynik_cena)

# Interpretacja wyniku testu ANOVA
p_value_cena <- wynik_cena[[1]][["Pr(>F)"]][1]
if (p_value_cena < 0.05) {
  cat("p-value =", p_value_cena, "\n")
  cat("Odrzucamy hipotezę zerową: cena za m2 zależy od liczby pokoi.\n")
} else {
  cat("p-value =", p_value_cena, "\n")
  cat("Brak podstaw do odrzucenia hipotezy zerowej: cena za m2 nie zależy od liczby pokoi.\n")
}

# 3c) Porównanie parami - test post-hoc Tukeya
tukey_cena <- TukeyHSD(model_cena)
print(tukey_cena)

# Średnia cena za m2 w każdej kategorii
srednie_cena_m2 <- aggregate(cena_m2 ~ kategoria_pokoje, data = mieszkania, mean)
print(srednie_cena_m2)
