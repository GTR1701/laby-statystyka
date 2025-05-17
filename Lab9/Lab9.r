# Wczytanie danych
waga1 <- read.csv(file.path(getwd(), "Lab9/waga1.csv"), header = TRUE, sep = ";")

# Zadanie 2: Test Z dla średniego IQ
n <- 100
mean_sample <- 109
var_sample <- 225
mean_h0 <- 105
sd_sample <- sqrt(var_sample)

# Obliczenie statystyki testowej Z
z_stat <- (mean_sample - mean_h0) / (sd_sample / sqrt(n))
p_value_z <- 2 * (1 - pnorm(abs(z_stat)))

cat("Test Z dla średniego IQ:\n")
cat("Statystyka Z:", z_stat, "\n")
cat("Wartość p:", p_value_z, "\n\n")

# Zadanie 2b: Test t dla średniego IQ
t_stat <- (mean_sample - mean_h0) / (sd_sample / sqrt(n))
df <- n - 1
p_value_t <- 2 * (1 - pt(abs(t_stat), df))

cat("Test t dla średniego IQ:\n")
cat("Statystyka t:", t_stat, "\n")
cat("Wartość p:", p_value_t, "\n\n")

interpretuj_test_t <- function(p_value, alpha = 0.05) {
  if (p_value > alpha) {
    cat("Nie odrzucamy hipotezy zerowej na poziomie istotności", alpha, ":\n")
    cat("Nie mamy dowodów przeciwko H_0.\n")
  } else if (p_value > 0.01) {
    cat("Odrzucamy hipotezę zerową na poziomie istotności", alpha, ":\n")
    cat("Mamy dowody przeciwko H_0.\n")
  } else if (p_value > 0.001) {
    cat("Odrzucamy hipotezę zerową na poziomie istotności 1%:\n")
    cat("Mamy mocne dowody przeciwko H_0.\n")
  } else {
    cat("Odrzucamy hipotezę zerową na poziomie istotności 0.1%:\n")
    cat("Mamy bardzo mocne dowody przeciwko H_0.\n")
  }
}

# Zadanie 3: Test Z dla średniego wzrostu studentów
mean_h0_height <- 168
# Zakładamy, że dane o wzroście są w kolumnie "Wzrost"
mean_sample_height <- mean(waga1$Wzrost)
sd_sample_height <- sd(waga1$Wzrost)
n_height <- length(waga1$Wzrost)

z_stat_height <- (mean_sample_height - mean_h0_height) / (sd_sample_height / sqrt(n_height))
p_value_z_height <- 2 * (1 - pnorm(abs(z_stat_height)))

cat("Test Z dla średniego wzrostu studentów:\n")
cat("Statystyka Z:", z_stat_height, "\n")
cat("Wartość p:", p_value_z_height, "\n\n")

# Test t dla średniego wzrostu studentów
t_test_height <- t.test(waga1$Wzrost, mu = mean_h0_height)

cat("Test t dla średniego wzrostu studentów:\n")

# Interpretacja wyniku testu t
interpretuj_test_t(t_test_height$p.value)

# Obliczenie przyrostu wagi
waga1$Przyrost_wagi <- waga1$Waga_po - waga1$Waga_przed

# Zadanie 4: Testy dla średniego przyrostu wagi o 2kg (wszyscy studenci)
mean_h0_weight_all <- 2
mean_sample_weight_all <- mean(waga1$Przyrost_wagi)
sd_sample_weight_all <- sd(waga1$Przyrost_wagi)
n_weight_all <- length(waga1$Przyrost_wagi)

# Test Z
z_stat_weight_all <- (mean_sample_weight_all - mean_h0_weight_all) / (sd_sample_weight_all / sqrt(n_weight_all))
p_value_z_weight_all <- 2 * (1 - pnorm(abs(z_stat_weight_all)))

cat("Test Z dla średniego przyrostu wagi (wszyscy studenci):\n")
cat("Statystyka Z:", z_stat_weight_all, "\n")
cat("Wartość p:", p_value_z_weight_all, "\n")
cat("Porównanie z wartością krytyczną (1.96 dla alfa=0.05):", ifelse(abs(z_stat_weight_all) > 1.96, "Odrzucamy H0", "Nie odrzucamy H0"), "\n\n")

# Test t
t_stat_weight_all <- (mean_sample_weight_all - mean_h0_weight_all) / (sd_sample_weight_all / sqrt(n_weight_all))
df_weight_all <- n_weight_all - 1
p_value_t_weight_all <- 2 * (1 - pt(abs(t_stat_weight_all), df_weight_all))

cat("Test t dla średniego przyrostu wagi (wszyscy studenci):\n")
cat("Statystyka t:", t_stat_weight_all, "\n")
cat("Wartość p:", p_value_t_weight_all, "\n")
cat("Porównanie z wartością krytyczną (dla alfa=0.05):", ifelse(abs(t_stat_weight_all) > qt(0.975, df_weight_all), "Odrzucamy H0", "Nie odrzucamy H0"), "\n\n")

# Test t za pomocą funkcji t.test
t_test_weight_all <- t.test(waga1$Przyrost_wagi, mu = mean_h0_weight_all)

cat("Test t (t.test) dla średniego przyrostu wagi (wszyscy studenci):\n")
interpretuj_test_t(t_test_weight_all$p.value)

# Zadanie 5: Testy dla średniego przyrostu wagi o 4kg (studenci męscy)
waga_male <- subset(waga1, plec == "0")
mean_h0_weight_male <- 4
mean_sample_weight_male <- mean(waga_male$Przyrost_wagi)
sd_sample_weight_male <- sd(waga_male$Przyrost_wagi)
n_weight_male <- length(waga_male$Przyrost_wagi)

# Test Z
z_stat_weight_male <- (mean_sample_weight_male - mean_h0_weight_male) / (sd_sample_weight_male / sqrt(n_weight_male))
p_value_z_weight_male <- 2 * (1 - pnorm(abs(z_stat_weight_male)))

cat("Test Z dla średniego przyrostu wagi (studenci męscy):\n")
cat("Statystyka Z:", z_stat_weight_male, "\n")
cat("Wartość p:", p_value_z_weight_male, "\n")
cat("Porównanie z wartością krytyczną (1.96 dla alfa=0.05):", ifelse(abs(z_stat_weight_male) > 1.96, "Odrzucamy H0", "Nie odrzucamy H0"), "\n\n")

# Test t
t_stat_weight_male <- (mean_sample_weight_male - mean_h0_weight_male) / (sd_sample_weight_male / sqrt(n_weight_male))
df_weight_male <- n_weight_male - 1
p_value_t_weight_male <- 2 * (1 - pt(abs(t_stat_weight_male), df_weight_male))

cat("Test t dla średniego przyrostu wagi (studenci męscy):\n")
cat("Statystyka t:", t_stat_weight_male, "\n")
cat("Wartość p:", p_value_t_weight_male, "\n")
cat("Porównanie z wartością krytyczną (dla alfa=0.05):", ifelse(abs(t_stat_weight_male) > qt(0.975, df_weight_male), "Odrzucamy H0", "Nie odrzucamy H0"), "\n\n")

# Test t za pomocą funkcji t.test
t_test_weight_male <- t.test(waga_male$Przyrost_wagi, mu = mean_h0_weight_male)

cat("Test t (t.test) dla średniego przyrostu wagi (studenci męscy):\n")
interpretuj_test_t(t_test_weight_male$p.value)