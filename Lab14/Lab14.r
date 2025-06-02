# --- Zadanie 1 ---
dane <- read.csv(file.path(getwd(), "Lab11/mieszkania.csv"), header = TRUE, sep = ";", stringsAsFactors = FALSE)
dane$cena_m2 <- dane$cena / dane$metraz

# Indykatory dzielnic (Krzyki jako baza)
dane$dzielSt <- as.integer(dane$dzielnica == "Stare Miasto")
dane$dzielSr <- as.integer(dane$dzielnica == "Srodmiescie")
dane$dzielP <- as.integer(dane$dzielnica == "Psie Pole")
dane$dzielF <- as.integer(dane$dzielnica == "Fabryczna")
# Piętro_1: 0 do 4-tego, 1 powyżej
dane$Pietro_1 <- as.integer(dane$pietro > 4)

# a) Model regresji krokowej (wstecznej) dla ceny za m2
model_full <- lm(cena_m2 ~ metraz + Pietro_1 + dzielSt + dzielSr + dzielP + dzielF, data = dane)
model_step <- step(model_full, direction = "backward", trace = 0)
summary(model_step)

# b) Predykcje
new1 <- data.frame(metraz = 80, Pietro_1 = 1, dzielSt = 0, dzielSr = 0, dzielP = 0, dzielF = 1)
new2 <- data.frame(metraz = 65, Pietro_1 = 0, dzielSt = 0, dzielSr = 0, dzielP = 0, dzielF = 0)
cat("80m2, 10 piętro, Fabryczna:", predict(model_step, new1), "\n")
cat("65m2, 3 piętro, Krzyki:", predict(model_step, new2), "\n")

# c) Reszty
reszty <- resid(model_step)
# d) Test normalności reszt
print(shapiro.test(reszty))

# --- Zadanie 2 ---
model_full2 <- lm(cena ~ metraz + Pietro_1 + dzielSt + dzielSr + dzielP + dzielF, data = dane)
model_step2 <- step(model_full2, direction = "backward", trace = 0)
summary(model_step2)
cat("Predykcja (80m2, 10 piętro, Fabryczna):", predict(model_step2, new1), "\n")
cat("Predykcja (65m2, 3 piętro, Krzyki):", predict(model_step2, new2), "\n")
reszty2 <- resid(model_step2)
print(shapiro.test(reszty2))

# --- Zadanie 3 ---
bakt <- read.csv(file.path(getwd(), "Lab14/bakteria.csv"), header = TRUE, sep = ";", stringsAsFactors = FALSE)
print(head(bakt)) # sprawdź dane!
bakt$czas <- as.numeric(bakt$czas)
bakt$masa <- as.numeric(bakt$masa)
bakt <- na.omit(bakt) # usuń wiersze z NA
print(head(bakt)) # sprawdź po konwersji

if (nrow(bakt) == 0) {
  stop("Brak poprawnych danych po konwersji! Sprawdź plik bakteria.csv.")
}

# a) Wykres rozrzutu
plot(bakt$czas, bakt$masa, main = "Masa bakterii vs czas", xlab = "Czas", ylab = "Masa bakterii")
# b) Regresja liniowa masa ~ czas
model_bakt <- lm(masa ~ czas, data = bakt)
abline(model_bakt, col = "red")
summary(model_bakt)
# c) Regresja log(masa) ~ czas
model_log <- lm(log(masa) ~ czas, data = bakt)
summary(model_log)
# d) Regresja wykładnicza: masa = exp(a + b*czas)
bakt$masa_hat <- exp(predict(model_log, newdata = bakt))
lines(bakt$czas, bakt$masa_hat, col = "blue", lwd = 2)
