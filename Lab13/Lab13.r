# --- Zadanie: Korelacje dla wyników egzaminów ---
dane <- read.csv(file.path(getwd(), "Lab11/mieszkania.csv"), header = TRUE, sep = ";", stringsAsFactors = FALSE)

analiza <- c(28, 26, 23, 18, 14, 12)
algebra <- c(25, 27, 20, 24, 16, 13)

# i) Korelacja Pearsona
wynik_pearson <- cor.test(analiza, algebra, method = "pearson")
print(wynik_pearson)
cat("Interpretacja: ")
if (wynik_pearson$p.value < 0.05) {
  cat("Odrzucamy hipotezę o braku korelacji liniowej między wynikami z analizy i algebry.\n")
} else {
  cat("Brak podstaw do odrzucenia hipotezy o braku korelacji liniowej między wynikami z analizy i algebry.\n")
}

# ii) Korelacja Spearmana
wynik_spearman <- cor.test(analiza, algebra, method = "spearman")
print(wynik_spearman)
cat("Interpretacja: ")
if (wynik_spearman$p.value < 0.05) {
  cat("Odrzucamy hipotezę o braku monotonicznej korelacji rangowej między wynikami z analizy i algebry.\n")
} else {
  cat("Brak podstaw do odrzucenia hipotezy o braku monotonicznej korelacji rangowej między wynikami z analizy i algebry.\n")
}

# iii) Korelacja Kendalla
wynik_kendall <- cor.test(analiza, algebra, method = "kendall")
print(wynik_kendall)
cat("Interpretacja: ")
if (wynik_kendall$p.value < 0.05) {
  cat("Odrzucamy hipotezę o braku korelacji rangowej (Kendalla) między wynikami z analizy i algebry.\n")
} else {
  cat("Brak podstaw do odrzucenia hipotezy o braku korelacji rangowej (Kendalla) między wynikami z analizy i algebry.\n")
}

# --- Zadanie: Symulacja 100 par (X, Y) ---
set.seed(123)
X <- rnorm(100)
Y <- rnorm(100)

# Korelacje i testy
wynik_pearson_XY <- cor.test(X, Y, method = "pearson")
print(wynik_pearson_XY)
cat("Interpretacja: ")
if (wynik_pearson_XY$p.value < 0.05) {
  cat("Odrzucamy hipotezę o braku korelacji liniowej między X i Y.\n")
} else {
  cat("Brak podstaw do odrzucenia hipotezy o braku korelacji liniowej między X i Y.\n")
}

wynik_spearman_XY <- cor.test(X, Y, method = "spearman")
print(wynik_spearman_XY)
cat("Interpretacja: ")
if (wynik_spearman_XY$p.value < 0.05) {
  cat("Odrzucamy hipotezę o braku monotonicznej korelacji rangowej między X i Y.\n")
} else {
  cat("Brak podstaw do odrzucenia hipotezy o braku monotonicznej korelacji rangowej między X i Y.\n")
}

wynik_kendall_XY <- cor.test(X, Y, method = "kendall")
print(wynik_kendall_XY)
cat("Interpretacja: ")
if (wynik_kendall_XY$p.value < 0.05) {
  cat("Odrzucamy hipotezę o braku korelacji rangowej (Kendalla) między X i Y.\n")
} else {
  cat("Brak podstaw do odrzucenia hipotezy o braku korelacji rangowej (Kendalla) między X i Y.\n")
}

# Permutacyjny test korelacji
wynik_perm_pearson <- perm_test(X, Y, "pearson")
print(wynik_perm_pearson)
cat("Interpretacja: ")
if (wynik_perm_pearson$p.value < 0.05) {
  cat("Odrzucamy hipotezę o braku korelacji liniowej (test permutacyjny).\n")
} else {
  cat("Brak podstaw do odrzucenia hipotezy o braku korelacji liniowej (test permutacyjny).\n")
}

wynik_perm_spearman <- perm_test(X, Y, "spearman")
print(wynik_perm_spearman)
cat("Interpretacja: ")
if (wynik_perm_spearman$p.value < 0.05) {
  cat("Odrzucamy hipotezę o braku korelacji rangowej (Spearman, test permutacyjny).\n")
} else {
  cat("Brak podstaw do odrzucenia hipotezy o braku korelacji rangowej (Spearman, test permutacyjny).\n")
}

wynik_perm_kendall <- perm_test(X, Y, "kendall")
print(wynik_perm_kendall)
cat("Interpretacja: ")
if (wynik_perm_kendall$p.value < 0.05) {
  cat("Odrzucamy hipotezę o braku korelacji rangowej (Kendalla, test permutacyjny).\n")
} else {
  cat("Brak podstaw do odrzucenia hipotezy o braku korelacji rangowej (Kendalla, test permutacyjny).\n")
}

# Bootstrap dla przedziału ufności
wynik_boot_p <- boot.ci(boot_p, type = "perc")
print(wynik_boot_p)
cat("Interpretacja: ")
if (wynik_boot_p$percent[4] < 0 & wynik_boot_p$percent[5] > 0) {
  cat("Przedział ufności dla współczynnika korelacji Pearsona zawiera 0 – brak podstaw do stwierdzenia istotnej korelacji.\n")
} else {
  cat("Przedział ufności dla współczynnika korelacji Pearsona nie zawiera 0 – współczynnik korelacji jest istotnie różny od zera.\n")
}

wynik_boot_s <- boot.ci(boot_s, type = "perc")
print(wynik_boot_s)
cat("Interpretacja: ")
if (wynik_boot_s$percent[4] < 0 & wynik_boot_s$percent[5] > 0) {
  cat("Przedział ufności dla współczynnika korelacji Spearmana zawiera 0 – brak podstaw do stwierdzenia istotnej korelacji.\n")
} else {
  cat("Przedział ufności dla współczynnika korelacji Spearmana nie zawiera 0 – współczynnik korelacji jest istotnie różny od zera.\n")
}

wynik_boot_k <- boot.ci(boot_k, type = "perc")
print(wynik_boot_k)
cat("Interpretacja: ")
if (wynik_boot_k$percent[4] < 0 & wynik_boot_k$percent[5] > 0) {
  cat("Przedział ufności dla współczynnika korelacji Kendalla zawiera 0 – brak podstaw do stwierdzenia istotnej korelacji.\n")
} else {
  cat("Przedział ufności dla współczynnika korelacji Kendalla nie zawiera 0 – współczynnik korelacji jest istotnie różny od zera.\n")
}

# --- Zadanie: Korelacja X i V ---
V <- 0.2 * X + sqrt(0.96) * Y
wynik_pearson_XV <- cor.test(X, V, method = "pearson")
print(wynik_pearson_XV)
cat("Interpretacja: ")
if (wynik_pearson_XV$p.value < 0.05) {
  cat("Odrzucamy hipotezę o braku korelacji liniowej między X i V.\n")
} else {
  cat("Brak podstaw do odrzucenia hipotezy o braku korelacji liniowej między X i V.\n")
}

wynik_spearman_XV <- cor.test(X, V, method = "spearman")
print(wynik_spearman_XV)
cat("Interpretacja: ")
if (wynik_spearman_XV$p.value < 0.05) {
  cat("Odrzucamy hipotezę o braku korelacji rangowej (Spearman) między X i V.\n")
} else {
  cat("Brak podstaw do odrzucenia hipotezy o braku korelacji rangowej (Spearman) między X i V.\n")
}

wynik_kendall_XV <- cor.test(X, V, method = "kendall")
print(wynik_kendall_XV)
cat("Interpretacja: ")
if (wynik_kendall_XV$p.value < 0.05) {
  cat("Odrzucamy hipotezę o braku korelacji rangowej (Kendalla) między X i V.\n")
} else {
  cat("Brak podstaw do odrzucenia hipotezy o braku korelacji rangowej (Kendalla) między X i V.\n")
}

# --- Zadanie: Analiza danych z mieszkania.csv ---
wynik_pearson_mtx <- cor(dane[, c("cena", "metraz", "cena_m2", "pokoje")], method = "pearson")
print(wynik_pearson_mtx)
cat("Interpretacja: Wysoka korelacja między ceną a metrażem oznacza, że większe mieszkania są droższe. Korelacja cena_m2 a metraż zwykle ujemna.\n")

wynik_spearman_mtx <- cor(dane[, c("cena", "metraz", "cena_m2", "pokoje")], method = "spearman")
print(wynik_spearman_mtx)
cat("Interpretacja: Korelacje rangowe pokazują monotoniczne zależności między zmiennymi.\n")

wynik_kendall_mtx <- cor(dane[, c("cena", "metraz", "cena_m2", "pokoje")], method = "kendall")
print(wynik_kendall_mtx)
cat("Interpretacja: Korelacje Kendalla potwierdzają zgodność rang między zmiennymi.\n")

wynik_pearson_cm2 <- cor.test(dane$cena_m2, dane$metraz, method = "pearson")
print(wynik_pearson_cm2)
cat("Interpretacja: ")
if (wynik_pearson_cm2$p.value < 0.05) {
  cat("Odrzucamy hipotezę o braku korelacji liniowej między ceną za m2 a metrażem.\n")
} else {
  cat("Brak podstaw do odrzucenia hipotezy o braku korelacji liniowej między ceną za m2 a metrażem.\n")
}

wynik_spearman_cm2 <- cor.test(dane$cena_m2, dane$metraz, method = "spearman")
print(wynik_spearman_cm2)
cat("Interpretacja: ")
if (wynik_spearman_cm2$p.value < 0.05) {
  cat("Odrzucamy hipotezę o braku korelacji rangowej (Spearman) między ceną za m2 a metrażem.\n")
} else {
  cat("Brak podstaw do odrzucenia hipotezy o braku korelacji rangowej (Spearman) między ceną za m2 a metrażem.\n")
}

wynik_kendall_cm2 <- cor.test(dane$cena_m2, dane$metraz, method = "kendall")
print(wynik_kendall_cm2)
cat("Interpretacja: ")
if (wynik_kendall_cm2$p.value < 0.05) {
  cat("Odrzucamy hipotezę o braku korelacji rangowej (Kendalla) między ceną za m2 a metrażem.\n")
} else {
  cat("Brak podstaw do odrzucenia hipotezy o braku korelacji rangowej (Kendalla) między ceną za m2 a metrażem.\n")
}

# --- Zadanie: Wzrost i waga z rozkładu normalnego ---
set.seed(123)
rho <- 0.7
X <- rnorm(100)
Y <- rnorm(100)
V <- rho * X + sqrt(1 - rho^2) * Y
H <- 170 + 12 * X
W <- 65 + 10 * V

# i) Średnie i odchylenia standardowe
print(mean(H))
print(sd(H))
print(mean(W))
print(sd(W))

# ii) Korelacje
print(cor(H, W, method = "pearson"))
print(cor(H, W, method = "spearman"))
print(cor(H, W, method = "kendall"))

# iii) Estymatory gęstości
plot(density(H), main = "Gęstość H (wzrost)")
plot(density(W), main = "Gęstość W (waga)")

# iv) Test normalności
print(shapiro.test(H))
cat("Interpretacja: ")
if (shapiro.test(H)$p.value < 0.05) {
  cat("Odrzucamy hipotezę o normalności rozkładu wzrostu.\n")
} else {
  cat("Brak podstaw do odrzucenia hipotezy o normalności rozkładu wzrostu.\n")
}
print(shapiro.test(W))
cat("Interpretacja: ")
if (shapiro.test(W)$p.value < 0.05) {
  cat("Odrzucamy hipotezę o normalności rozkładu wagi.\n")
} else {
  cat("Brak podstaw do odrzucenia hipotezy o normalności rozkładu wagi.\n")
}

# v) Wykres rozrzutu
plot(H, W, main = "Rozrzut H i W")
