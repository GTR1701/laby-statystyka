# Wyniki rzutów kostką
wyniki <- c(171, 200, 168, 213, 226, 222)

# a) Oczekiwane frekwencje (kostka symetryczna)
n <- sum(wyniki)
oczekiwane <- rep(n / 6, 6)
print(oczekiwane)

# b) Statystyka testowa chi-kwadrat
statystyka <- sum((wyniki - oczekiwane)^2 / oczekiwane)
print(statystyka)

# c) Wartość p
p_value <- pchisq(statystyka, df = 5, lower.tail = FALSE)
print(p_value)

# d) Wniosek
if (p_value < 0.05) {
  print("Odrzucamy hipotezę o symetryczności kostki (istotna różnica).")
} else {
  print("Brak podstaw do odrzucenia hipotezy o symetryczności kostki.")
}

# e) Test chi-kwadrat wbudowany
test <- chisq.test(wyniki, p = rep(1 / 6, 6))
print(test)

# 2. Tablica wykształcenie vs płeć
tablica <- matrix(c(200, 300, 150, 350), nrow = 2, byrow = TRUE)
colnames(tablica) <- c("Wyższe", "Średnie")
rownames(tablica) <- c("Kobiety", "Mężczyźni")
print(tablica)

# a) Oczekiwane frekwencje
rzedy <- rowSums(tablica)
kolumny <- colSums(tablica)
n <- sum(tablica)
oczekiwane <- outer(rzedy, kolumny) / n
print(oczekiwane)

# b) Statystyka testowa chi-kwadrat (Pearsona)
statystyka <- sum((tablica - oczekiwane)^2 / oczekiwane)
print(statystyka)

# c) Wartość p
df <- (nrow(tablica) - 1) * (ncol(tablica) - 1)
p_value <- pchisq(statystyka, df = df, lower.tail = FALSE)
print(p_value)

# d) Wniosek
if (p_value < 0.05) {
  print("Odrzucamy hipotezę o niezależności (istnieje zależność między płcią a wykształceniem).")
} else {
  print("Brak podstaw do odrzucenia hipotezy o niezależności.")
}

# e) Test chi-kwadrat wbudowany
test_chi <- chisq.test(tablica, correct = FALSE)
print(test_chi)

# f) Test Fishera
test_fisher <- fisher.test(tablica)
print(test_fisher)

# 3. Import danych z pliku mieszkania.csv
# Zakładam, że plik mieszkania.csv znajduje się w folderze Lab11
mieszkania <- read.csv("Lab11/mieszkania.csv", header = TRUE, sep = ";", stringsAsFactors = FALSE)
print(head(mieszkania))

# a) Tabela rozdzielcza liczby pokoi w zależności od dzielnicy
tabela_pokoje <- table(mieszkania$Pokoje, mieszkania$Dzielnica)
print(tabela_pokoje)

# b) Nowa zmienna: pokoje_4plus
mieszkania$pokoje_4plus <- ifelse(mieszkania$Pokoje >= 4, 4, mieszkania$Pokoje)
tabela_pokoje_4plus <- table(mieszkania$pokoje_4plus, mieszkania$Dzielnica)
print(tabela_pokoje_4plus)

# c) Test niezależności liczby pokoi od dzielnicy
test_chi_pokoje <- chisq.test(tabela_pokoje_4plus)
print(test_chi_pokoje)

# 4. Analiza ceny za m2 względem dzielnicy

# a) Zmienna: czy cena za m2 > 6000 zł
mieszkania$cena_m2 <- mieszkania$Cena / mieszkania$Metraz
mieszkania$cena_m2_6000 <- ifelse(mieszkania$cena_m2 > 6000, "powyzej_6000", "ponizej_6000")
print(table(mieszkania$cena_m2_6000))

# b) Tabela rozdzielcza: cena_m2_6000 vs dzielnica
tabela_cena_dzielnica <- table(mieszkania$cena_m2_6000, mieszkania$Dzielnica)
print(tabela_cena_dzielnica)

# Test niezależności: czy udział mieszkań z ceną >6000zł zależy od dzielnicy
test_chi_cena <- chisq.test(tabela_cena_dzielnica)
print(test_chi_cena)

# 5. Testowanie normalności i estymacja gęstości

# a) Test normalności dla ceny za m2
shapiro_test_cena <- shapiro.test(mieszkania$cena_m2)
print(shapiro_test_cena)

# Wykres estymatora gęstości dla ceny za m2
plot(density(mieszkania$cena_m2), main = "Estymator gęstości: cena za m2", xlab = "Cena za m2")

# b) Test normalności dla metrażu mieszkań w Śródmieściu
metraz_srodmiescie <- mieszkania$Metraz[mieszkania$Dzielnica == "Srodmiescie"]
shapiro_test_metraz <- shapiro.test(metraz_srodmiescie)
print(shapiro_test_metraz)

# Wykres estymatora gęstości dla metrażu w Śródmieściu
plot(density(metraz_srodmiescie), main = "Estymator gęstości: metraż w Śródmieściu", xlab = "Metraż")

# 6. Centralne twierdzenie graniczne i testy rozkładów

# a) Próbka z rozkładu wykładniczego Exp(1), wartość oczekiwana = 1
set.seed(123)
proba_exp <- rexp(1000, rate = 1)

# b) Testy dla próbki wykładniczej
# i) Test normalności (H0: rozkład normalny N(1,1))
shapiro_test_exp_norm <- shapiro.test(proba_exp)
print(shapiro_test_exp_norm)

# Porównanie z rozkładem normalnym N(1,1) - wykres
hist(proba_exp, probability = TRUE, breaks = 30, main = "Exp(1) vs N(1,1)")
curve(dnorm(x, mean = 1, sd = 1), col = "blue", add = TRUE)

# ii) Test zgodności z rozkładem wykładniczym Exp(1) (test Kolmogorova-Smirnova)
ks_test_exp <- ks.test(proba_exp, "pexp", rate = 1)
print(ks_test_exp)

# c) Próbka z rozkładu Gamma(k=100, scale=1)
proba_gamma <- rgamma(1000, shape = 100, scale = 1)

# d) Testy dla próbki gamma
# i) Test normalności (H0: N(100,10))
shapiro_test_gamma_norm <- shapiro.test(proba_gamma)
print(shapiro_test_gamma_norm)

# Porównanie z rozkładem normalnym N(100,10) - wykres
hist(proba_gamma, probability = TRUE, breaks = 30, main = "Gamma(100,1) vs N(100,10)")
curve(dnorm(x, mean = 100, sd = 10), col = "blue", add = TRUE)

# ii) Test zgodności z rozkładem Gamma(100,1) (test Kolmogorova-Smirnova)
ks_test_gamma <- ks.test(proba_gamma, "pgamma", shape = 100, scale = 1)
print(ks_test_gamma)
