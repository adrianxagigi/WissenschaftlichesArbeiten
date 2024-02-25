#load("neue_titanic.RData")

source("Funktionen_R_Skript_1.R")
library(ggplot2)
library(magrittr)
library(dplyr)

# 1. Ueberlebensrate nach Klasse

# Deskriptive Statistiken für Überlebensrate nach Klasse
survival_stats <- calculate_bivariate_categorical(neue_titanic,"Survived" ,"Pclass")  
survival_stats

survival_numbers <- survival_stats$Kreuztabelle[2, ]
Non_survival_numbers <- survival_stats$Kreuztabelle[1, ]

class_names <- rownames(survival_numbers)
class_names2 <-rownames(Non_survival_numbers)

# Visualisierung der Ueberlebensrate nach Klasse mit Balkendiagramm
barplot(survival_numbers, 
        names.arg = class_names,
        main = "Überlebensrate nach Klasse",
        xlab = "Klasse",
        ylab = "Anzahl der Überlebenden",
        col = "skyblue")

# Visualisierung der Ueberlebensrate nach Klasse mit Balkendiagramm
barplot(Non_survival_numbers, 
        names.arg = class_names2,
        main = "Ueberlebensrate nach Klasse",
        xlab = "Klasse",
        ylab = "Anzahl der Nichtüberlebenden",
        col = "green")


# Chi-Quadrat-Test für den Zusammenhang zwischen Ueberlebensrate und Klasse
chi_sq_result <- calculate_bivariate_categorical(neue_titanic, "Survived", "Pclass")
print(chi_sq_result$Chi_Quadrat_Test)


# 2. Ticketspreise nach Klasse

# Deskriptive Statistiken fuer Ticketpreise nach Klasse

fare_stats <- deskriptive_statistiken(neue_titanic)
print(fare_stats)

# Visualisierung der Ticketpreise nach Klasse mit Boxplot
ggplot(neue_titanic, aes(x = as.factor(Pclass), y = Fare)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(title = "Ticketpreise nach Klasse", x = "Klasse", y = "Ticketpreis") 


# T-test fuer die Ticketpreise in verschiedenen Klassen
compare_means_ttest("Fare", "Pclass", neue_titanic)


# 3. Ueberlebensrate nach Geschlecht und Alter

# Ueberlebensrate nach Geschlecht
survival_by_sex <- calculate_descriptive_categorical(neue_titanic, "Sex")

# Bivariate Analyse: Ueberlebensrate nach Geschlecht und Alter
survival_by_sex_and_age <- calculate_bivariate_categorical(neue_titanic, "Sex", "Age")

# Ergebnisse ausgeben:
# Daten vorbereiten
data_for_plot <- neue_titanic %>%
  group_by(Sex, Age, Survived) %>%
  summarise(count = n()) %>%
  mutate(Percentage = count / sum(count) * 100)

# Balkendiagramm erstellen
ggplot(data_for_plot, aes(x = Age, y = Percentage, fill = as.factor(Survived))) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(. ~ Sex) +
  labs(title = "Ueberlebensrate nach Geschlecht und Alter",
       x = "Alter",
       y = "Prozentsatz") +
  scale_fill_manual(values = c("red", "skyblue"), name = "Survived") +
  theme_minimal()


# Visualisierung des Überlebensrate in Abhängigkeit vom Alter und Ticket
plot(neue_titanic$Age, neue_titanic$Survived, main = 
       "Überlebensrate in Abhängigkeit vom Alter", xlab = "Alter", 
     ylab = "Überlebt (1) oder nicht (0)", 
     col = ifelse(neue_titanic$Survived == 1, "blue", "red"))
plot(neue_titanic$Fare, neue_titanic$Survived, 
     main = "Überlebensrate in Abhängigkeit vom Ticketpreis", 
     xlab = "Ticketpreis", ylab = "Überlebt (1) oder nicht (0)",
     col = ifelse(neue_titanic$Survived == 1, "blue", "red"))

# Ueberlebensrate nach Alter, Klasse und Geschlecht
plot_age_survival(neue_titanic)

# 4. Ueberlebensrate nach Position auf dem Schiff (Steuerbord oder Backbord)

# Deskriptive Statistiken für Überlebensrate nach Steuerbord und Backbord
survival_stats_position <- calculate_bivariate_categorical(neue_titanic, "Survived", "Bord")
print(survival_stats_position)

# Datenvorbereitung
nicht_ueberlebt = survival_stats_position$Kreuztabelle[1, ]
ueberlebt = survival_stats_position$Kreuztabelle[2, ]

# Balkendiagramm erstellen
barplot(
  cbind(nicht_ueberlebt, ueberlebt),
  beside = TRUE,
  main = "Überlebensrate nach Steuerbord und Backbord",
  xlab = "Überlebensstatus",
  ylab = "Anzahl",
  names.arg = c("Steuerbord", "Backbord"),
  col = c("skyblue", "green", "grey"),
  legend = TRUE,
  args.legend = list(x = "topright", bty = "n", legend = c("Steuerbord", "Backbord", "Kein Info"))
)
