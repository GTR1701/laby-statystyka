# Zadanie 1a
# 385 z 1000 osób ma wyższe wykształcenie. Testujemy H0: p = 0.4

# i) Test Z
n <- 1000
x <- 385
p0 <- 0.4
phat <- x/n
se <- sqrt(p0*(1-p0)/n)
z <- (phat - p0)/se
p_value_z <- 2*pnorm(-abs(z))
cat("Test Z: z =", z, ", p-value =", p_value_z, "\n")

# ii) prop.test
prop_test <- prop.test(x, n, p = p0, correct = FALSE)
print(prop_test)

# Zadanie 1b
# Kobiety: 220/520, Mężczyźni: 165/480
x1 <- 220; n1 <- 520
x2 <- 165; n2 <- 480
p1 <- x1/n1
p2 <- x2/n2
p_pool <- (x1 + x2)/(n1 + n2)
se2 <- sqrt(p_pool*(1-p_pool)*(1/n1 + 1/n2))
z2 <- (p1 - p2)/se2
p_value_z2 <- 2*pnorm(-abs(z2))
cat("Test Z dla dwóch proporcji: z =", z2, ", p-value =", p_value_z2, "\n")

# ii) prop.test dla dwóch proporcji
prop_test2 <- prop.test(c(x1, x2), c(n1, n2), correct = FALSE)
print(prop_test2)

# Zadanie 1c
# Średni Wzrost kobiet: 166cm, wariancja 100cm2, n1=520
# Średni Wzrost mężczyzn: 174cm, wariancja 121cm2, n2=480

mean1 <- 166; var1 <- 100; n1 <- 520
mean2 <- 174; var2 <- 121; n2 <- 480
se3 <- sqrt(var1/n1 + var2/n2)
z3 <- (mean1 - mean2)/se3
p_value_z3 <- 2*pnorm(-abs(z3))
cat("Test Z dla średnich: z =", z3, ", p-value =", p_value_z3, "\n")

# Zadanie 2
waga <- read.csv(file.path(getwd(), "Lab10/waga1.csv"), header = TRUE, sep = ";")
kobiety <- waga$plec == "1"
n <- nrow(waga)
x <- sum(kobiety)
p0 <- 0.5
phat <- x/n
se <- sqrt(p0*(1-p0)/n)
z <- (phat - p0)/se
p_value_z <- 2*pnorm(-abs(z))
cat("Zadanie 2a: Test Z: z =", z, ", p-value =", p_value_z, "\n")
cat("Zadanie 2b: prop.test:\n")
print(prop.test(x, n, p = p0, correct = FALSE))

# Zadanie 3
waga_k <- waga$Waga_po[waga$plec == "1"]
waga_m <- waga$Waga_po[waga$plec == "0"]
mean_k <- mean(waga_k)
mean_m <- mean(waga_m)
var_k <- var(waga_k)
var_m <- var(waga_m)
n_k <- length(waga_k)
n_m <- length(waga_m)
se <- sqrt(var_k/n_k + var_m/n_m)
z <- (mean_k - mean_m)/se
p_value_z <- 2*pnorm(-abs(z))
cat("Zadanie 3a: Test Z: z =", z, ", p-value =", p_value_z, "\n")
cat("Zadanie 3b: t.test:\n")
print(t.test(waga_k, waga_m))

# Zadanie 4
kobiety_70 <- sum(waga$plec == "1" & waga$Waga_po > 70)
mezczyzni_70 <- sum(waga$plec == "0" & waga$Waga_po > 70)
n_k <- sum(waga$plec == "1")
n_m <- sum(waga$plec == "0")
p1 <- kobiety_70/n_k
p2 <- mezczyzni_70/n_m
p_pool <- (kobiety_70 + mezczyzni_70)/(n_k + n_m)
se <- sqrt(p_pool*(1-p_pool)*(1/n_k + 1/n_m))
z <- (p1 - p2)/se
p_value_z <- 2*pnorm(-abs(z))
cat("Zadanie 4a: Test Z: z =", z, ", p-value =", p_value_z, "\n")
cat("Zadanie 4b: t.test (na proporcjach):\n")
print(t.test(
  as.numeric(waga$plec == "1" & waga$Waga_po > 70),
  as.numeric(waga$plec == "0" & waga$Waga_po > 70)
))

# Bootstrap dla różnicy proporcji
set.seed(123)
B <- 10000
diffs <- numeric(B)
for (i in 1:B) {
  samp_k <- sample(waga$Waga_po[waga$plec == "1"], n_k, replace = TRUE)
  samp_m <- sample(waga$Waga_po[waga$plec == "0"], n_m, replace = TRUE)
  prop_k <- mean(samp_k > 70)
  prop_m <- mean(samp_m > 70)
  diffs[i] <- prop_k - prop_m
}
ci <- quantile(diffs, c(0.025, 0.975))
cat("Zadanie 4c: 95% CI dla różnicy proporcji (bootstrap):", ci, "\n")

# Zadanie 5
# H0: mean(M) - mean(K) = 5
diff_obs <- mean_m - mean_k
se <- sqrt(var_k/n_k + var_m/n_m)
z <- (diff_obs - 5)/se
p_value_z <- 2*pnorm(-abs(z))
cat("Zadanie 5: Test Z: z =", z, ", p-value =", p_value_z, "\n")

# Zadanie 6
przyrost <- waga$Waga_po - waga$Waga_przed
n <- length(przyrost)
x <- sum(przyrost > 0)
p0 <- 0.8
phat <- x/n
se <- sqrt(p0*(1-p0)/n)
z <- (phat - p0)/se
p_value_z <- 2*pnorm(-abs(z))
cat("Zadanie 6: Test Z: z =", z, ", p-value =", p_value_z, "\n")

# Zadanie 7a
kobiety_170 <- sum(waga$plec == "1" & waga$Wzrost > 170)
mezczyzni_170 <- sum(waga$plec == "0" & waga$Wzrost > 170)
n_k <- sum(waga$plec == "1")
n_m <- sum(waga$plec == "0")
p1 <- kobiety_170/n_k
p2 <- mezczyzni_170/n_m
p_pool <- (kobiety_170 + mezczyzni_170)/(n_k + n_m)
se <- sqrt(p_pool*(1-p_pool)*(1/n_k + 1/n_m))
z <- (p1 - p2)/se
p_value_z <- 2*pnorm(-abs(z))
cat("Zadanie 7a: Test Z: z =", z, ", p-value =", p_value_z, "\n")

# Zadanie 7b: Bootstrap 98% CI
set.seed(123)
B <- 10000
diffs <- numeric(B)
for (i in 1:B) {
  samp_k <- sample(waga$Wzrost[waga$plec == "1"], n_k, replace = TRUE)
  samp_m <- sample(waga$Wzrost[waga$plec == "0"], n_m, replace = TRUE)
  prop_k <- mean(samp_k > 170)
  prop_m <- mean(samp_m > 170)
  diffs[i] <- prop_k - prop_m
}
ci <- quantile(diffs, c(0.01, 0.99))
cat("Zadanie 7b: 98% CI dla różnicy proporcji (bootstrap):", ci, "\n")