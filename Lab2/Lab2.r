# Zad 1 Data frame

# a) Tworzenie data frame
print("Tworzenie data frame")
dataFrame <- data.frame(
    "Imie" = c("Krzysztof", "Maria", "Henryk", "Anna"),
    "Plec" = c("m", "k", "m", "k"),
    "Analiza" = c(3.5, 4.5, 5.0, 4.5),
    "Algebra" = c(4.0, 5.0, 4.0, 3.5)
)
print(dataFrame)

# b) 
print("Wyświetlenie pierwszych dwóch wierszy")
row <- dataFrame[1:2, ]
print(row)

# c)
print("Opis struktury ramy")
str(dataFrame)

# d)
print("Średnia ocena z analizy")
print(mean(dataFrame$Analiza))


#e)

print("Nowa kolumna z średnią oceną")
dataFrame$Srednia <- (dataFrame$Analiza + dataFrame$Algebra) / 2

print(dataFrame)

# f)

print("Wyniki wszystkich kobiet")
kobiety <- dataFrame[dataFrame$Plec == "k", ]
print(kobiety)

print("Srednia ocen kobiet")
print(mean(kobiety$Srednia))
# g)

print("Wyniki studentów z oceną co najmniej 4.5")
student <- dataFrame[dataFrame$Analiza >= 4.5 | dataFrame$Algebra >= 4.5, ]
print(student)

# h)

print("Ilość osób z oceną co najmniej 4.5 z analizy")
print(nrow(dataFrame[dataFrame$Analiza >= 4.5, ]))

# Zad 2

# 2. a)

print("Wczytanie danych z pliku")
waga1 <- read.csv(file.path(getwd(), "/Lab2/waga1.csv"), header = TRUE, sep = ";")

wagaDF <- data.frame(waga1)

# b)

print("Pierwsze pięc wierszy")
print(wagaDF[1:5, ])

# c)
print("Opis struktury ramy")
str(wagaDF)

# d)

print("Średni wzrost przed studiami")
mean(wagaDF$Wzrost)

print("Średnia waga przed studiami")
mean(wagaDF$Waga_przed)

# e)

print("Nowa kolumna z wskaźnikiem wagi")
wagaDF$Wskaźnik <- wagaDF$Waga_przed / (wagaDF$Wzrost / 100)^2

print(wagaDF)

# f)

print("Kobiety z wskaźnikiem wagi powyżej 25")
kobiety <- wagaDF[wagaDF$plec == "1" & wagaDF$Wskaźnik > 25, ]
print(kobiety)

mezczyzni20 <- wagaDF[wagaDF$plec == "0" & wagaDF$Wskaźnik < 20, ]
print(mezczyzni20)

# g)

print("Mężczyźni")
mezczyzni <- wagaDF[wagaDF$plec == "0", ]
print(mezczyzni)

# h)

print("Osoby wyższe od 175cm")
print(nrow(wagaDF[wagaDF$Wzrost > 175, ]))

# Zad 3

# 3.

print("Wczytanie danych z pliku mieszkania.csv")
mieszkania <- read.csv(file.path(getwd(), "/Lab2/mieszkania.csv"), header = TRUE, sep = ";")

# b)

print("Pierwsze sześć wierszy")
print(mieszkania[1:6,])

# c)

print("Opis struktury ramy")
str(mieszkania)

# d)

print("Średni metraż")
mean(mieszkania$Metraz)

print("Średnia cena")
mean(mieszkania$Cena)

# e)

print("Nowa kolumna z ceną za m2")
mieszkania$Cena_m2 <- mieszkania$Cena / mieszkania$Metraz

# f)

print("Oferty na Psim Polu o cenie poniżej 400 000PLN")
psiePole <- mieszkania[mieszkania$Dzielnica == "Psie Pole" & mieszkania$Cena < 400000, ]

print(psiePole)

# g)

print("Oferty w Śródmieściu o metrażu powyżej 60m2")
srodmiescie <- mieszkania[mieszkania$Dzielnica == "Srodmiescie" & mieszkania$Metraz > 60, ]

print(srodmiescie)

# h)

print("Mieszkania o metrażu większym niż 60m2 oraz o cenie poniżej 350 000PLN")
print(nrow(mieszkania[mieszkania$Metraz > 60 & mieszkania$Cena < 350000, ]))

srodmiesciePietro <- mieszkania[mieszkania$Dzielnica == "Srodmiescie" & mieszkania$Pietro < 2 & mieszkania$Cena_m2 < 5000, ]
print(srodmiesciePietro)