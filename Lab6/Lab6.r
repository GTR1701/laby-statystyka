# Definicja macierzy prawdopodobieństw dla pierwszego rozkładu
joint_prob <- matrix(c(1 / 8, 1 / 4, 1 / 8, 1 / 6, 1 / 6, 1 / 6), nrow = 2, byrow = TRUE)
rownames(joint_prob) <- c("X=0", "X=1")
colnames(joint_prob) <- c("Y=0", "Y=1", "Y=2")

# Wyznaczenie rozkładów brzegowych
marginal_X <- rowSums(joint_prob) # Rozkład brzegowy X
marginal_Y <- colSums(joint_prob) # Rozkład brzegowy Y
print("Rozkład brzegowy X:")
print(marginal_X)
print("Rozkład brzegowy Y:")
print(marginal_Y)

# Wyznaczenie współczynnika korelacji ρ(X,Y)
x_values <- c(0, 1)
y_values <- c(0, 1, 2)
prob_vector <- as.vector(joint_prob)
x_vector <- rep(x_values, each = length(y_values))
y_vector <- rep(y_values, times = length(x_values))
expected_XY <- sum(x_vector * y_vector * prob_vector)
expected_X <- sum(x_values * marginal_X)
expected_Y <- sum(y_values * marginal_Y)
var_X <- sum((x_values ^ 2) * marginal_X) - expected_X ^ 2
var_Y <- sum((y_values ^ 2) * marginal_Y) - expected_Y ^ 2
correlation <- (expected_XY - expected_X * expected_Y) / sqrt(var_X * var_Y)
print("Współczynnik korelacji ρ(X, Y):")
print(correlation)

# Wyznaczenie rozkładów warunkowych P(Y=y|X=x)
conditional_Y_given_X0 <- joint_prob[1,] / marginal_X[1]
conditional_Y_given_X1 <- joint_prob[2,] / marginal_X[2]
print("Rozkład warunkowy P(Y|X=0):")
print(conditional_Y_given_X0)
print("Rozkład warunkowy P(Y|X=1):")
print(conditional_Y_given_X1)

# Sprawdzenie niezależności zmiennych X i Y
independence_check <- all.equal(joint_prob, outer(marginal_X, marginal_Y))
print("Czy zmienne X i Y są niezależne?")
print(independence_check)

# Symulacja 1000 par realizacji metodą skumulowanych prawdopodobieństw
set.seed(123) # Ustawienie ziarna dla powtarzalności
cumulative_prob <- cumsum(prob_vector)
random_values <- runif(1000)
simulated_indices <- findInterval(random_values, cumulative_prob) + 1
simulated_X <- x_vector[simulated_indices]
simulated_Y <- y_vector[simulated_indices]

# Oszacowanie współczynnika korelacji na podstawie danych
pearson_corr <- cor(simulated_X, simulated_Y, method = "pearson")
spearman_corr <- cor(simulated_X, simulated_Y, method = "spearman")
kendall_corr <- cor(simulated_X, simulated_Y, method = "kendall")
print("Współczynniki korelacji na podstawie danych symulowanych:")
print(paste("Pearson:", pearson_corr))
print(paste("Spearman:", spearman_corr))
print(paste("Kendall:", kendall_corr))

# Tablica rozdzielcza relatywnych frekwencji
freq_table <- table(simulated_X, simulated_Y) / 1000
print("Tablica rozdzielcza relatywnych frekwencji dla danych symulowanych:")
print(freq_table)

# Drugi rozkład (oceny z Algebry i Analizy)
grades_joint_prob <- matrix(
  c(0.05, 0.03, 0.02, 0, 0, 0,
    0.05, 0.07, 0.05, 0.03, 0, 0,
    0.03, 0.05, 0.06, 0.04, 0.02, 0,
    0.01, 0.04, 0.06, 0.06, 0.02, 0.01,
    0, 0.02, 0.05, 0.08, 0.04, 0.01,
    0, 0.01, 0.01, 0.02, 0.03, 0.03),
  nrow = 6, byrow = TRUE
)
rownames(grades_joint_prob) <- c("X=2", "X=3", "X=3.5", "X=4", "X=4.5", "X=5")
colnames(grades_joint_prob) <- c("Y=2", "Y=3", "Y=3.5", "Y=4", "Y=4.5", "Y=5")

# Analogiczne kroki jak dla pierwszego rozkładu
grades_marginal_X <- rowSums(grades_joint_prob)
grades_marginal_Y <- colSums(grades_joint_prob)
print("Rozkład brzegowy ocen (X):")
print(grades_marginal_X)
print("Rozkład brzegowy ocen (Y):")
print(grades_marginal_Y)

# Symulacja 1000 par realizacji dla drugiego rozkładu
set.seed(123)
x_values_grades <- c(2, 3, 3.5, 4, 4.5, 5)
y_values_grades <- c(2, 3, 3.5, 4, 4.5, 5)
prob_vector_grades <- as.vector(grades_joint_prob)
cumulative_prob_grades <- cumsum(prob_vector_grades)
random_values_grades <- runif(1000)
simulated_indices_grades <- findInterval(random_values_grades, cumulative_prob_grades) + 1
simulated_X_grades <- rep(x_values_grades, each = length(y_values_grades))[simulated_indices_grades]
simulated_Y_grades <- rep(y_values_grades, times = length(x_values_grades))[simulated_indices_grades]

# Tablica rozdzielcza relatywnych frekwencji dla drugiego rozkładu
freq_table_grades <- table(simulated_X_grades, simulated_Y_grades) / 1000
print("Tablica rozdzielcza relatywnych frekwencji dla ocen:")
print(freq_table_grades)

# Współczynniki korelacji dla drugiego rozkładu
pearson_corr_grades <- cor(simulated_X_grades, simulated_Y_grades, method = "pearson")
spearman_corr_grades <- cor(simulated_X_grades, simulated_Y_grades, method = "spearman")
kendall_corr_grades <- cor(simulated_X_grades, simulated_Y_grades, method = "kendall")
print("Współczynniki korelacji dla ocen:")
print(paste("Pearson:", pearson_corr_grades))
print(paste("Spearman:", spearman_corr_grades))
print(paste("Kendall:", kendall_corr_grades))