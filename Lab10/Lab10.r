# Zadanie 1a
# 385 z 1000 osób ma wyższe wykształcenie. Testujemy H0: p = 0.4

# i) Test Z
liczba_osob <- 1000
liczba_wykszt <- 385
proporcja_hipotetyczna <- 0.4
proporcja_obserwowana <- liczba_wykszt / liczba_osob
se_proporcji <- sqrt(proporcja_hipotetyczna * (1 - proporcja_hipotetyczna) / liczba_osob)
statystyka_z <- (proporcja_obserwowana - proporcja_hipotetyczna) / se_proporcji
p_wartosc_z <- 2 * pnorm(-abs(statystyka_z))
cat("Test Z: z =", statystyka_z, ", p-value =", p_wartosc_z, "\n")

# ii) prop.test
wynik_prop_test <- prop.test(liczba_wykszt, liczba_osob, p = proporcja_hipotetyczna, correct = FALSE)
print(wynik_prop_test)

# Zadanie 1b
# Kobiety: 220/520, Mężczyźni: 165/480
kobiety_wykszt <- 220
kobiety_liczba <- 520
mezczyzni_wykszt <- 165
mezczyzni_liczba <- 480
proporcja_kobiet <- kobiety_wykszt / kobiety_liczba
proporcja_mezczyzn <- mezczyzni_wykszt / mezczyzni_liczba
proporcja_laczna <- (kobiety_wykszt + mezczyzni_wykszt) / (kobiety_liczba + mezczyzni_liczba)
se_roznicy <- sqrt(proporcja_laczna * (1 - proporcja_laczna) * (1 / kobiety_liczba + 1 / mezczyzni_liczba))
statystyka_z2 <- (proporcja_kobiet - proporcja_mezczyzn) / se_roznicy
p_wartosc_z2 <- 2 * pnorm(-abs(statystyka_z2))
cat("Test Z dla dwóch proporcji: z =", statystyka_z2, ", p-value =", p_wartosc_z2, "\n")

# ii) prop.test dla dwóch proporcji
wynik_prop_test2 <- prop.test(c(kobiety_wykszt, mezczyzni_wykszt), c(kobiety_liczba, mezczyzni_liczba), correct = FALSE)
print(wynik_prop_test2)

# Zadanie 1c
# Średni Wzrost kobiet: 166cm, wariancja 100cm2, n1=520
# Średni Wzrost mężczyzn: 174cm, wariancja 121cm2, n2=480

srednia_kobiet <- 166
wariancja_kobiet <- 100
liczba_kobiet <- 520
srednia_mezczyzn <- 174
wariancja_mezczyzn <- 121
liczba_mezczyzn <- 480
se_srednich <- sqrt(wariancja_kobiet / liczba_kobiet + wariancja_mezczyzn / liczba_mezczyzn)
statystyka_z3 <- (srednia_kobiet - srednia_mezczyzn) / se_srednich
p_wartosc_z3 <- 2 * pnorm(-abs(statystyka_z3))
cat("Test Z dla średnich: z =", statystyka_z3, ", p-value =", p_wartosc_z3, "\n")

# Zadanie 2
dane_waga <- read.csv(file.path(getwd(), "Lab10/waga1.csv"), header = TRUE, sep = ";")
kobiety_maska <- dane_waga$plec == "1"
liczba_osob <- nrow(dane_waga)
liczba_kobiet <- sum(kobiety_maska)
proporcja_hipotetyczna <- 0.5
proporcja_obserwowana <- liczba_kobiet / liczba_osob
se_proporcji <- sqrt(proporcja_hipotetyczna * (1 - proporcja_hipotetyczna) / liczba_osob)
statystyka_z <- (proporcja_obserwowana - proporcja_hipotetyczna) / se_proporcji
p_wartosc_z4 <- 2 * pnorm(-abs(statystyka_z))
cat("Zadanie 2a: Test Z: z =", statystyka_z, ", p-value =", p_wartosc_z4, "\n")
cat("Zadanie 2b: prop.test:\n")
print(prop.test(liczba_kobiet, liczba_osob, p = proporcja_hipotetyczna, correct = FALSE))

# Zadanie 3
waga_kobiet <- dane_waga$Waga_po[dane_waga$plec == "1"]
waga_mezczyzn <- dane_waga$Waga_po[dane_waga$plec == "0"]
srednia_kobiet <- mean(waga_kobiet)
srednia_mezczyzn <- mean(waga_mezczyzn)
wariancja_kobiet <- var(waga_kobiet)
wariancja_mezczyzn <- var(waga_mezczyzn)
liczba_kobiet <- length(waga_kobiet)
liczba_mezczyzn <- length(waga_mezczyzn)
se_srednich <- sqrt(wariancja_kobiet / liczba_kobiet + wariancja_mezczyzn / liczba_mezczyzn)
statystyka_z <- (srednia_kobiet - srednia_mezczyzn) / se_srednich
p_wartosc_z5 <- 2 * pnorm(-abs(statystyka_z))
cat("Zadanie 3a: Test Z: z =", statystyka_z, ", p-value =", p_wartosc_z5, "\n")
cat("Zadanie 3b: t.test:\n")
print(t.test(waga_kobiet, waga_mezczyzn))

# Zadanie 4
kobiety_powyzej_70 <- sum(dane_waga$plec == "1" & dane_waga$Waga_po > 70)
mezczyzni_powyzej_70 <- sum(dane_waga$plec == "0" & dane_waga$Waga_po > 70)
liczba_kobiet <- sum(dane_waga$plec == "1")
liczba_mezczyzn <- sum(dane_waga$plec == "0")
proporcja_kobiet <- kobiety_powyzej_70 / liczba_kobiet
proporcja_mezczyzn <- mezczyzni_powyzej_70 / liczba_mezczyzn
proporcja_laczna <- (kobiety_powyzej_70 + mezczyzni_powyzej_70) / (liczba_kobiet + liczba_mezczyzn)
se_roznicy <- sqrt(proporcja_laczna * (1 - proporcja_laczna) * (1 / liczba_kobiet + 1 / liczba_mezczyzn))
statystyka_z <- (proporcja_kobiet - proporcja_mezczyzn) / se_roznicy
p_wartosc_z6 <- 2 * pnorm(-abs(statystyka_z))
cat("Zadanie 4a: Test Z: z =", statystyka_z, ", p-value =", p_wartosc_z6, "\n")
cat("Zadanie 4b: t.test (na proporcjach):\n")
print(t.test(
  as.numeric(dane_waga$plec == "1" & dane_waga$Waga_po > 70),
  as.numeric(dane_waga$plec == "0" & dane_waga$Waga_po > 70)
))

# Bootstrap dla różnicy proporcji
set.seed(123)
liczba_bootstrap <- 10000
roznice_proporcji <- numeric(liczba_bootstrap)
for (i in 1:liczba_bootstrap) {
  probka_kobiet <- sample(dane_waga$Waga_po[dane_waga$plec == "1"], liczba_kobiet, replace = TRUE)
  probka_mezczyzn <- sample(dane_waga$Waga_po[dane_waga$plec == "0"], liczba_mezczyzn, replace = TRUE)
  prop_kobiet <- mean(probka_kobiet > 70)
  prop_mezczyzn <- mean(probka_mezczyzn > 70)
  roznice_proporcji[i] <- prop_kobiet - prop_mezczyzn
}
przedzial_ufnosci <- quantile(roznice_proporcji, c(0.025, 0.975))
cat("Zadanie 4c: 95% CI dla różnicy proporcji (bootstrap):", przedzial_ufnosci, "\n")

# Zadanie 5
# H0: mean(M) - mean(K) = 5
roznica_obserwowana <- srednia_mezczyzn - srednia_kobiet
se_srednich <- sqrt(wariancja_kobiet / liczba_kobiet + wariancja_mezczyzn / liczba_mezczyzn)
statystyka_z <- (roznica_obserwowana - 5) / se_srednich
p_wartosc_z7 <- 2 * pnorm(-abs(statystyka_z))
cat("Zadanie 5: Test Z: z =", statystyka_z, ", p-value =", p_wartosc_z7, "\n")

# Zadanie 6
przyrost_wagi <- dane_waga$Waga_po - dane_waga$Waga_przed
liczba_osob <- length(przyrost_wagi)
liczba_przyrost <- sum(przyrost_wagi > 0)
proporcja_hipotetyczna <- 0.8
proporcja_obserwowana <- liczba_przyrost / liczba_osob
se_proporcji <- sqrt(proporcja_hipotetyczna * (1 - proporcja_hipotetyczna) / liczba_osob)
statystyka_z <- (proporcja_obserwowana - proporcja_hipotetyczna) / se_proporcji
p_wartosc_z8 <- 2 * pnorm(-abs(statystyka_z))
cat("Zadanie 6: Test Z: z =", statystyka_z, ", p-value =", p_wartosc_z8, "\n")

# Zadanie 7a
kobiety_powyzej_170 <- sum(dane_waga$plec == "1" & dane_waga$Wzrost > 170)
mezczyzni_powyzej_170 <- sum(dane_waga$plec == "0" & dane_waga$Wzrost > 170)
liczba_kobiet <- sum(dane_waga$plec == "1")
liczba_mezczyzn <- sum(dane_waga$plec == "0")
proporcja_kobiet <- kobiety_powyzej_170 / liczba_kobiet
proporcja_mezczyzn <- mezczyzni_powyzej_170 / liczba_mezczyzn
proporcja_laczna <- (kobiety_powyzej_170 + mezczyzni_powyzej_170) / (liczba_kobiet + liczba_mezczyzn)
se_roznicy <- sqrt(proporcja_laczna * (1 - proporcja_laczna) * (1 / liczba_kobiet + 1 / liczba_mezczyzn))
statystyka_z <- (proporcja_kobiet - proporcja_mezczyzn) / se_roznicy
p_wartosc_z9 <- 2 * pnorm(-abs(statystyka_z))
cat("Zadanie 7a: Test Z: z =", statystyka_z, ", p-value =", p_wartosc_z9, "\n")

# Zadanie 7b: Bootstrap 98% CI
set.seed(123)
liczba_bootstrap <- 10000
roznice_proporcji <- numeric(liczba_bootstrap)
for (i in 1:liczba_bootstrap) {
  probka_kobiet <- sample(dane_waga$Wzrost[dane_waga$plec == "1"], liczba_kobiet, replace = TRUE)
  probka_mezczyzn <- sample(dane_waga$Wzrost[dane_waga$plec == "0"], liczba_mezczyzn, replace = TRUE)
  prop_kobiet <- mean(probka_kobiet > 170)
  prop_mezczyzn <- mean(probka_mezczyzn > 170)
  roznice_proporcji[i] <- prop_kobiet - prop_mezczyzn
}
przedzial_ufnosci <- quantile(roznice_proporcji, c(0.01, 0.99))
cat("Zadanie 7b: 98% CI dla różnicy proporcji (bootstrap):", przedzial_ufnosci, "\n")

# --- Analiza wyników hipotez ---

cat("\n--- Analiza wyników hipotez ---\n")

# Zadanie 1a
cat("Zadanie 1a:\n")
if (p_wartosc_z < 0.05) {
  cat("Odrzucamy hipotezę zerową: odsetek osób z wyższym wykształceniem różni się od 40%.\n")
} else {
  cat("Brak podstaw do odrzucenia hipotezy zerowej: odsetek osób z wyższym wykształceniem nie różni się istotnie od 40%.\n")
}

# Zadanie 1b
cat("\nZadanie 1b:\n")
if (p_wartosc_z2 < 0.05) {
  cat("Odrzucamy hipotezę zerową: proporcje osób z wyższym wykształceniem różnią się istotnie między kobietami a mężczyznami.\n")
} else {
  cat("Brak podstaw do odrzucenia hipotezy zerowej: nie ma istotnej różnicy między proporcjami kobiet i mężczyzn z wyższym wykształceniem.\n")
}

# Zadanie 1c
cat("\nZadanie 1c:\n")
if (p_wartosc_z3 < 0.05) {
  cat("Odrzucamy hipotezę zerową: średni wzrost kobiet i mężczyzn różni się istotnie.\n")
} else {
  cat("Brak podstaw do odrzucenia hipotezy zerowej: brak istotnej różnicy średniego wzrostu.\n")
}

# Zadanie 2
cat("\nZadanie 2:\n")
if (p_wartosc_z4 < 0.05) {
  cat("Odrzucamy hipotezę zerową: udział kobiet w próbie różni się od 50%.\n")
} else {
  cat("Brak podstaw do odrzucenia hipotezy zerowej: udział kobiet nie różni się istotnie od 50%.\n")
}

# Zadanie 3
cat("\nZadanie 3:\n")
if (p_wartosc_z5 < 0.05) {
  cat("Odrzucamy hipotezę zerową: średnia waga kobiet i mężczyzn różni się istotnie.\n")
} else {
  cat("Brak podstaw do odrzucenia hipotezy zerowej: brak istotnej różnicy średniej wagi.\n")
}

# Zadanie 4
cat("\nZadanie 4:\n")
if (p_wartosc_z6 < 0.05) {
  cat("Odrzucamy hipotezę zerową: odsetek osób powyżej 70kg różni się istotnie między kobietami a mężczyznami.\n")
} else {
  cat("Brak podstaw do odrzucenia hipotezy zerowej: brak istotnej różnicy odsetka osób powyżej 70kg.\n")
}

# Zadanie 5
cat("\nZadanie 5:\n")
if (p_wartosc_z7 < 0.05) {
  cat("Odrzucamy hipotezę zerową: różnica średnich wag mężczyzn i kobiet nie wynosi 5kg.\n")
} else {
  cat("Brak podstaw do odrzucenia hipotezy zerowej: różnica średnich wag może wynosić 5kg.\n")
}

# Zadanie 6
cat("\nZadanie 6:\n")
if (p_wartosc_z8 < 0.05) {
  cat("Odrzucamy hipotezę zerową: odsetek osób z przyrostem wagi różni się od 80%.\n")
} else {
  cat("Brak podstaw do odrzucenia hipotezy zerowej: odsetek osób z przyrostem wagi nie różni się istotnie od 80%.\n")
}

# Zadanie 7a
cat("\nZadanie 7a:\n")
if (p_wartosc_z9 < 0.05) {
  cat("Odrzucamy hipotezę zerową: odsetek osób powyżej 170cm różni się istotnie między kobietami a mężczyznami.\n")
} else {
  cat("Brak podstaw do odrzucenia hipotezy zerowej: brak istotnej różnicy odsetka osób powyżej 170cm.\n")
}
