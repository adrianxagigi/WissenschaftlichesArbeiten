#load("neue_titanic.RData")

source("Funktionen-R-Skript 1.R")

# 1. Ueberlebensrate nach Klasse

# Deskriptive Statistiken für Überlebensrate nach Klasse
survival_stats <- calculate_descriptive_categorical(neue_titanic, "Pclass")

# Visualisierung der Ueberlebensrate nach Klasse mit Balkendiagramm
barplot(survival_stats$Relative_Haeufigkeiten, names.arg = survival_stats$Kategorie,
        main = "Überlebensrate nach Klasse",
        xlab = "Klasse",
        ylab = "Relative Häufigkeit",
        col = "skyblue")

# Chi-Quadrat-Test für den Zusammenhang zwischen Ueberlebensrate und Klasse
chi_sq_result <- calculate_bivariate_categorical(neue_titanic, "Survived", "Pclass")
print(chi_sq_result$Chi_Quadrat_Test)


# 2. Ticketspreise nach Klasse

# Deskriptive Statistiken fuer Ticketpreise nach Klasse

fare_stats <- deskriptive_statistiken(data.frame(neue_titanic$Fare))
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
