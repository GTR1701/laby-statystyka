# Wczytanie danych
dane <- read.csv(file.path(getwd(), "/Lab8/waga1.csv"), header = TRUE, sep = ";")

# Zadanie 1
# Dane
liczba_studentow <- 100
srednie_IQ <- 109
wariancja_IQ <- 225
odch_std_IQ <- sqrt(wariancja_IQ)
proporcja_powyzej_115 <- 30 / liczba_studentow

# a) Przedział ufności dla proporcji
alfa_95 <- 0.05
alfa_99 <- 0.01
z_95 <- qnorm(1 - alfa_95 / 2)
z_99 <- qnorm(1 - alfa_99 / 2)

blad_std_proporcji <- sqrt(proporcja_powyzej_115 * (1 - proporcja_powyzej_115) / liczba_studentow)
przedzial_ufnosci_95_proporcja <- c(proporcja_powyzej_115 - z_95 * blad_std_proporcji, proporcja_powyzej_115 + z_95 * blad_std_proporcji)
przedzial_ufnosci_99_proporcja <- c(proporcja_powyzej_115 - z_99 * blad_std_proporcji, proporcja_powyzej_115 + z_99 * blad_std_proporcji)

# b) Przedział ufności dla średniego IQ (rozkład normalny)
blad_std_sredniej <- odch_std_IQ / sqrt(liczba_studentow)
przedzial_ufnosci_95_srednia_norm <- c(srednie_IQ - z_95 * blad_std_sredniej, srednie_IQ + z_95 * blad_std_sredniej)
przedzial_ufnosci_99_srednia_norm <- c(srednie_IQ - z_99 * blad_std_sredniej, srednie_IQ + z_99 * blad_std_sredniej)

# c) Przedział ufności dla średniego IQ (rozkład Studenta)
t_95 <- qt(1 - alfa_95 / 2, df = liczba_studentow - 1)
t_99 <- qt(1 - alfa_99 / 2, df = liczba_studentow - 1)
przedzial_ufnosci_95_srednia_t <- c(srednie_IQ - t_95 * blad_std_sredniej, srednie_IQ + t_95 * blad_std_sredniej)
przedzial_ufnosci_99_srednia_t <- c(srednie_IQ - t_99 * blad_std_sredniej, srednie_IQ + t_99 * blad_std_sredniej)

# Zadanie 2
# a) Przedział ufności dla średniego wzrostu (rozkład normalny)
sredni_wzrost <- mean(dane$Wzrost)
odch_std_wzrost <- sd(dane$Wzrost)
liczba_obserwacji <- nrow(dane)
blad_std_wzrost <- odch_std_wzrost / sqrt(liczba_obserwacji)
z_90 <- qnorm(1 - 0.10 / 2)
przedzial_ufnosci_90_wzrost_norm <- c(sredni_wzrost - z_90 * blad_std_wzrost, sredni_wzrost + z_90 * blad_std_wzrost)

# b) Przedział ufności dla średniego wzrostu (rozkład Studenta)
t_90 <- qt(1 - 0.10 / 2, df = liczba_obserwacji - 1)
przedzial_ufnosci_90_wzrost_t <- c(sredni_wzrost - t_90 * blad_std_wzrost, sredni_wzrost + t_90 * blad_std_wzrost)

# c) Przedział ufności dla średniego wzrostu (bootstrap)
library(boot)
bootstrap_srednia <- function(dane, indeksy) {
  d <- dane[indeksy]
  return(mean(d))
}
wyniki_bootstrap <- boot(dane$Wzrost, statistic = bootstrap_srednia, R = 1000)
przedzial_ufnosci_90_wzrost_boot <- boot.ci(wyniki_bootstrap, type = "perc", conf = 0.90)$percent[4:5]

# Zadanie 3
# Analogiczne do zadania 2, ale dla studentek
dane_studentki <- subset(dane, plec == 1)
sredni_wzrost_studentki <- mean(dane_studentki$Wzrost)
odch_std_wzrost_studentki <- sd(dane_studentki$Wzrost)
liczba_obserwacji_studentki <- nrow(dane_studentki)
blad_std_wzrost_studentki <- odch_std_wzrost_studentki / sqrt(liczba_obserwacji_studentki)

# a) Normalny
z_98 <- qnorm(1 - 0.02 / 2)
przedzial_ufnosci_98_wzrost_studentki_norm <- c(sredni_wzrost_studentki - z_98 * blad_std_wzrost_studentki, sredni_wzrost_studentki + z_98 * blad_std_wzrost_studentki)

# b) Student
t_98 <- qt(1 - 0.02 / 2, df = liczba_obserwacji_studentki - 1)
przedzial_ufnosci_98_wzrost_studentki_t <- c(sredni_wzrost_studentki - t_98 * blad_std_wzrost_studentki, sredni_wzrost_studentki + t_98 * blad_std_wzrost_studentki)

# c) Bootstrap
wyniki_bootstrap_studentki <- boot(dane_studentki$Wzrost, statistic = bootstrap_srednia, R = 1000)
przedzial_ufnosci_98_wzrost_studentki_boot <- boot.ci(wyniki_bootstrap_studentki, type = "perc", conf = 0.98)$percent[4:5]

# Zadanie 4
# Proporcja studentów o wzroście > 168 cm
proporcja_wzrost_168 <- mean(dane$Wzrost > 168)
blad_std_proporcja_wzrost <- sqrt(proporcja_wzrost_168 * (1 - proporcja_wzrost_168) / liczba_obserwacji)

# a) Normalny
z_94 <- qnorm(1 - 0.06 / 2)
przedzial_ufnosci_94_proporcja_wzrost_norm <- c(proporcja_wzrost_168 - z_94 * blad_std_proporcja_wzrost, proporcja_wzrost_168 + z_94 * blad_std_proporcja_wzrost)

# b) Bootstrap
bootstrap_proporcja <- function(dane, indeksy) {
  d <- dane[indeksy]
  return(mean(d > 168))
}
wyniki_bootstrap_wzrost <- boot(dane$Wzrost, statistic = bootstrap_proporcja, R = 1000)
przedzial_ufnosci_94_proporcja_wzrost_boot <- boot.ci(wyniki_bootstrap_wzrost, type = "perc", conf = 0.94)$percent[4:5]

# Zadanie 5
# Analogiczne do zadania 4, ale dla studentek
proporcja_wzrost_168_studentki <- mean(dane_studentki$Wzrost > 168)
blad_std_proporcja_wzrost_studentki <- sqrt(proporcja_wzrost_168_studentki * (1 - proporcja_wzrost_168_studentki) / liczba_obserwacji_studentki)

# a) Normalny
z_96 <- qnorm(1 - 0.04 / 2)
przedzial_ufnosci_96_proporcja_wzrost_studentki_norm <- c(proporcja_wzrost_168_studentki - z_96 * blad_std_proporcja_wzrost_studentki, proporcja_wzrost_168_studentki + z_96 * blad_std_proporcja_wzrost_studentki)

# b) Bootstrap
wyniki_bootstrap_wzrost_studentki <- boot(dane_studentki$Wzrost, statistic = bootstrap_proporcja, R = 1000)
przedzial_ufnosci_96_proporcja_wzrost_studentki_boot <- boot.ci(wyniki_bootstrap_wzrost_studentki, type = "perc", conf = 0.96)$percent[4:5]

# Wyświetlenie wyników
list(
  zad1 = list(przedzial_ufnosci_95_proporcja, przedzial_ufnosci_99_proporcja, przedzial_ufnosci_95_srednia_norm, przedzial_ufnosci_99_srednia_norm, przedzial_ufnosci_95_srednia_t, przedzial_ufnosci_99_srednia_t),
  zad2 = list(przedzial_ufnosci_90_wzrost_norm, przedzial_ufnosci_90_wzrost_t, przedzial_ufnosci_90_wzrost_boot),
  zad3 = list(przedzial_ufnosci_98_wzrost_studentki_norm, przedzial_ufnosci_98_wzrost_studentki_t, przedzial_ufnosci_98_wzrost_studentki_boot),
  zad4 = list(przedzial_ufnosci_94_proporcja_wzrost_norm, przedzial_ufnosci_94_proporcja_wzrost_boot),
  zad5 = list(przedzial_ufnosci_96_proporcja_wzrost_studentki_norm, przedzial_ufnosci_96_proporcja_wzrost_studentki_boot)
)

# Wyświetlenie wyników w konsoli
cat("Zadanie 1:\n")
cat("a) Przedział ufności dla proporcji (95%):", przedzial_ufnosci_95_proporcja, "\n")
cat("a) Przedział ufności dla proporcji (99%):", przedzial_ufnosci_99_proporcja, "\n")
cat("b) Przedział ufności dla średniego IQ (95%, normalny):", przedzial_ufnosci_95_srednia_norm, "\n")
cat("b) Przedział ufności dla średniego IQ (99%, normalny):", przedzial_ufnosci_99_srednia_norm, "\n")
cat("c) Przedział ufności dla średniego IQ (95%, Student):", przedzial_ufnosci_95_srednia_t, "\n")
cat("c) Przedział ufności dla średniego IQ (99%, Student):", przedzial_ufnosci_99_srednia_t, "\n\n")

cat("Zadanie 2:\n")
cat("a) Przedział ufności dla średniego wzrostu (90%, normalny):", przedzial_ufnosci_90_wzrost_norm, "\n")
cat("b) Przedział ufności dla średniego wzrostu (90%, Student):", przedzial_ufnosci_90_wzrost_t, "\n")
cat("c) Przedział ufności dla średniego wzrostu (90%, bootstrap):", przedzial_ufnosci_90_wzrost_boot, "\n\n")

cat("Zadanie 3:\n")
cat("a) Przedział ufności dla średniego wzrostu studentek (98%, normalny):", przedzial_ufnosci_98_wzrost_studentki_norm, "\n")
cat("b) Przedział ufności dla średniego wzrostu studentek (98%, Student):", przedzial_ufnosci_98_wzrost_studentki_t, "\n")
cat("c) Przedział ufności dla średniego wzrostu studentek (98%, bootstrap):", przedzial_ufnosci_98_wzrost_studentki_boot, "\n\n")

cat("Zadanie 4:\n")
cat("a) Przedział ufności dla proporcji wzrostu > 168 cm (94%, normalny):", przedzial_ufnosci_94_proporcja_wzrost_norm, "\n")
cat("b) Przedział ufności dla proporcji wzrostu > 168 cm (94%, bootstrap):", przedzial_ufnosci_94_proporcja_wzrost_boot, "\n\n")

cat("Zadanie 5:\n")
cat("a) Przedział ufności dla proporcji wzrostu studentek > 168 cm (96%, normalny):", przedzial_ufnosci_96_proporcja_wzrost_studentki_norm, "\n")
cat("b) Przedział ufności dla proporcji wzrostu studentek > 168 cm (96%, bootstrap):", przedzial_ufnosci_96_proporcja_wzrost_studentki_boot, "\n")