# Zad 1 Wektory
# Zad 1.1

a <- c(1, 4, 6, 13, -10, 8)
b <- 1:101
c <- c(4, 4, 4, 7, 7, 7, 9, 9, 9)
d <- c("czy", "to", "jest", "wektor", NA)
e <- c("czy", "to", "jest", "wektor", "NA")
f <- c(4, 7, 9)
for(i in 1:5) {
    f <- c(f, 4, 7 ,9)
}

print(a)
print(b)
print(c)
print(d)
print(e)
print(f)

# Zad 1.2

# Długość wektora
print("Długość wektora")
print(length(a))
print(length(b))
print(length(c))
print(length(d))
print(length(e))
print(length(f))

# Typ danych
print("Typ danych")
print(class(a))
print(class(b))
print(class(c))
print(class(d))
print(class(e))
print(class(f))

# Element najmniejszy
print("Element najmniejszy")
print(min(a))
print(min(b))
print(min(c))
print(min(d))
print(min(e))
print(min(f))

# Element największy
print("Element największy")
print(max(a))
print(max(b))
print(max(c))
print(max(d))
print(max(e))
print(max(f))

# Suma elementów
print("Suma elementów")
if(!is.character(a)) print(sum(a))
if(!is.character(b)) print(sum(b))
if(!is.character(c)) print(sum(c))
if(!is.character(d)) print(sum(d))
if(!is.character(e)) print(sum(e))
if(!is.character(f)) print(sum(f))

# Zad 1.3

# Sortowanie wektorów
print("Sortowanie wektorów")
print(sort(d))
print(sort(e))

# Zad 1.4

# a) a+f
print("a+f")
print(a+f)

# b) a*f
print("a*f")
print(a*f)

# c) a+c
print("a+c")
print(a+c)

# d) a+10
print("a+10")
print(a+10)

# e) 15a
print("15a")
print(15*a)

# f) 26-ty element wektora b
print("26-ty element wektora b")
print(b[26])

# g) 6-ty do 10-tego elementu (włącznie) wektora f
print("6-ty do 10-tego elementu (włącznie) wektora f")
print(f[6:10])

# Zad 1.5

print("Które elementy w wektorze b są większe niż 50? Ile ich jest?")
print(b[b > 50])
print(length(b[b > 50]))

# Zad 2 Macierze
# Zad 2.1

A <- matrix(c(3,4,1,5,2,3), nrow=2, ncol=3)
B <- cbind(c(-1,3,-5), c(2,-4,6))
C <- rbind(c(7,3), c(2,1))
D <- matrix(c(1,3,5,2,5,7,4,7,11), nrow=3, ncol=3)

# Zad 2.2

# a) A+B
print("A+B")
if(all(dim(A) == dim(B))) print(A+B) else print("Nie można dodać macierzy A i B")

# b) A+B^T
print("A+B^T")
print(A+t(B))

# c) AB
print("AB")
if(all(ncol(A) == nrow(B))) print(A%*%B) else print("Nie można pomnożyć macierzy A i B")

# d) A*A
print("A*A")
if(all(ncol(A) == ncol(B))) print(A%*%A) else print("Nie można pomnożyć macierzy A i A")

# e) D^-1
print("D^-1")
print(solve(D))

# f) DD^-1
print("DD^-1")
print(D%*%solve(D))

# Zad 2.3

# a) CX = A
print("CX = A")
print("X=")
print(solve(C)%*%A)

# b) XD = A
print("XD = A")
print("X=")
print(A%*%solve(D))

# Zad 3

# Zad 3.1
