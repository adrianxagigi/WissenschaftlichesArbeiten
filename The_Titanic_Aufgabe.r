# Lese den Titanic-Datensatz ein
titanic <- read.csv("titanic.csv")

# Installation des stringr Packages
install.packages("stringr")
install.packages("dplyr")
install.packages("tidyverse")


library(tidyverse)
library(stringr)
library(forcats)
library(dplyr)

# Mein List von Namen
names <- titanic$Name

#Pattern von Anrede 
pattern <- "\\b[A-Za-z]+\\."

# str_extract Funktion um Anrede zu finden
titles <- str_extract(names, pattern)

# Unique Funktion, um die einzigartigen Anrede
unique_titles <- unique(titles)

# Zeigen Sie einzigartige Anrede
print(unique_titles)

# Ordinale Variablen als Factors

#Survived Yes = 1, No = 0

titanic$Survived <- as.factor(titanic$Survived)
titanic$Survived

#Sex: Geschlecht (male/female)
titanic$Sex = as.factor(titanic$Sex)

#Embarked: Zustiegshafen (C = Cherbourg; Q = Queenstown; S = Southampton)
titanic$Embarked = as.factor(titanic$Embarked)

# Umkehrung der Faktorslevel
# Klasse des Reisenden (ordinal mit 1 > 2 > 3)

titanic$Pclass <- factor(titanic$Pclass, ordered = TRUE)  # Create ordered factor
titanic$Pclass <- factor(titanic$Pclass, levels = rev(levels(titanic$Pclass)))  # Reverse levels



# Titel zum Datenframe hinzufügen
titanic$Title <- unlist(titles)

# Berechnung des mean für jeden Titel
average_age_by_title <- aggregate(Age ~ Title, titanic, mean, na.rm = TRUE)

# Ersetzen der fehlenden Alter durch das mean für jeden Titel
titanic$Age <- ifelse(is.na(titanic$Age), 
                      ave(titanic$Age, titanic$Title, FUN = function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x)), 
                      titanic$Age)

#Extrahiert aus der Variable „Cabin“ 
kabine_nummer <- as.numeric(str_extract(titanic$Cabin, "\\d+"))

# Neue Variable "Bord"
Bord <- ifelse(kabine_nummer %% 2 == 0, "Backbord", "Steuerbord")

# Neue Variable "Deck"
Deck <- str_extract(titanic$Cabin, "^\\D+")

# Ersetzt fehlende Werte mit NA
Deck[is.na(Deck)] <- NA
Bord[is.na(kabine_nummer)] <- NA

# Fügt neue Variablen zum Datensatz
titanic <- cbind(titanic, Bord , Deck)

#Entfernt Variablen „PassengerID“, "Name", "Cabin"
neue_titanic <- select(titanic, -PassengerId, -Name, -Ticket, -Cabin)

save(neue_titanic, file = "neue_titanic.RData")


