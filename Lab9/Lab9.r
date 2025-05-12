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
print(t_test_height)

# Interpretacja wyniku testu t
interpretuj_test_t(t_test_height$p.value)

# Analogicznie należy postąpić dla pozostałych zadań, zmieniając odpowiednie wartości hipotez i dane wejściowe.
