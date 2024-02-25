# Installiere das corrplot Paket, falls es noch nicht installiert ist
if (!requireNamespace("corrplot", quietly = TRUE)) {
  install.packages("corrplot")
}

# Lade erforderliche Pakete
library(ggplot2)
library(magrittr)
library(dplyr)
library(corrplot)

# Öffne eine PDF-Datei zum Speichern der Grafiken
pdf("Grafiken_und_Ergebnisse.pdf")

# Lade Daten und Funktionen
load("neue_titanic.RData")
source("Funktionen_R_Skript_1.R")

# Plotte die Grafiken
# 1) Überlebensrate nach Klasse
survival_stats <- calculate_bivariate_categorical(neue_titanic,"Survived" ,"Pclass")  
survival_numbers <- survival_stats$Kreuztabelle[2, ]
Non_survival_numbers <- survival_stats$Kreuztabelle[1, ]
class_names <- rownames(survival_numbers)
class_names2 <-rownames(Non_survival_numbers)
barplot(survival_numbers, 
        names.arg = class_names,
        main = "Überlebensrate nach Klasse",
        xlab = "Klasse",
        ylab = "Anzahl der Überlebenden",
        col = "skyblue")
barplot(Non_survival_numbers, 
        names.arg = class_names2,
        main = "Ueberlebensrate nach Klasse",
        xlab = "Klasse",
        ylab = "Anzahl der Nichtüberlebenden",
        col = "green")

# Chi-Quadrat-Test für den Zusammenhang zwischen Überlebensrate und Klasse
chi_sq_result <- calculate_bivariate_categorical(neue_titanic, "Survived", "Pclass")
print(chi_sq_result$Chi_Quadrat_Test)

# 2) Ticketpreise nach Klasse
fare_stats <- deskriptive_statistiken(neue_titanic)
print(fare_stats)
ggplot(neue_titanic, aes(x = as.factor(Pclass), y = Fare)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(title = "Ticketpreise nach Klasse", x = "Klasse", y = "Ticketpreis") 

# 3) T-test für die Überlebenden nach Ticketpreis
Survived_Factor = as.factor(neue_titanic$Survived)
compute_descriptive_stats(neue_titanic$Fare, Survived_Factor)

# 4) Überlebensrate nach Geschlecht und Alter
survival_by_sex <- calculate_descriptive_categorical(neue_titanic, "Sex")
survival_by_sex_and_age <- calculate_bivariate_categorical(neue_titanic, "Sex", "Age")
data_for_plot <- neue_titanic %>%
  group_by(Sex, Age, Survived) %>%
  summarise(count = n()) %>%
  mutate(Percentage = count / sum(count) * 100)
ggplot(data_for_plot, aes(x = Age, y = Percentage, fill = as.factor(Survived))) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(. ~ Sex) +
  labs(title = "Überlebensrate nach Geschlecht und Alter", x = "Alter", y = "Prozentsatz") +
  scale_fill_manual(values = c("red", "skyblue"), name = "Survived") +
  theme_minimal()

# 5) Überlebensrate in Abhängigkeit vom Alter und Ticket
plot(neue_titanic$Age, neue_titanic$Survived, main = 
       "Überlebensrate in Abhängigkeit vom Alter", xlab = "Alter", 
     ylab = "Überlebt (1) oder nicht (0)", 
     col = ifelse(neue_titanic$Survived == 1, "blue", "red"))
plot(neue_titanic$Fare, neue_titanic$Survived, 
     main = "Überlebensrate in Abhängigkeit vom Ticketpreis", 
     xlab = "Ticketpreis", ylab = "Überlebt (1) oder nicht (0)",
     col = ifelse(neue_titanic$Survived == 1, "blue", "red"))

# 6) Überlebensrate nach Alter, Klasse und Geschlecht
plot_age_survival(neue_titanic)

# 7) Überlebensrate nach Position auf dem Schiff (Steuerbord oder Backbord)
survival_stats_position <- calculate_bivariate_categorical(neue_titanic, "Survived", "Bord")
nicht_ueberlebt = survival_stats_position$Kreuztabelle[1, ]
ueberlebt = survival_stats_position$Kreuztabelle[2, ]
barplot(
  cbind(nicht_ueberlebt, ueberlebt),
  beside = TRUE,
  main = "Überlebensrate nach Steuerbord und Backbord",
  xlab = "Überlebensstatus",
  ylab = "Anzahl",
  names.arg = c("Überlebt", "Nicht überlebt"),
  col = c("skyblue", "green", "grey"),
  legend = TRUE,
  args.legend = list(x = "topright", bty = "n", legend = c("Steuerbord", "Backbord", "Kein Info"))
)

# 8) Korrelationsmatrix für numerische Variablen
korrelation <- cor(neue_titanic[, sapply(neue_titanic, is.numeric)], use = "complete.obs")
corrplot(korrelation, method = "circle")

# Schließe die PDF-Datei
dev.off()