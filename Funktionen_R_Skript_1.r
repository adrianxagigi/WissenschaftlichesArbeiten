load("neue_titanic.RData")
install.packages("magrittr")
install.packages("tidyverse")

library(tidyverse)
library(ggplot2)
source("Funktionen_R_Skript_2.r")

#i)

# Funktion zur Berechnung der 5-Werte Zusammenfassung metrischer Variablen

deskriptive_statistiken <- function(df) {

  numerische_variablen <- sapply(df, is.numeric)
  df_numerisch <- df[, numerische_variablen]

  # Pruefen ob die variablen numerisch sind

  for (var in colnames(df_numerisch)) {
    check_numeric(df_numerisch, var)
  }
  
  statistiken <- data.frame(
    'Minimum' = apply(df_numerisch, 2, min, na.rm = TRUE),
    '1. Quartil' = apply(df_numerisch, 2, quantile, probs = 0.25, na.rm = TRUE),
    'Median' = apply(df_numerisch, 2, median, na.rm = TRUE),
    'Mittelwert' = apply(df_numerisch, 2, mean, na.rm = TRUE),
    '3. Quartil' = apply(df_numerisch, 2, quantile, probs = 0.75, na.rm = TRUE),
    'Maximum' = apply(df_numerisch, 2, max, na.rm = TRUE)
  )
  
  return(statistiken)
}

# Anwendung der Funktion auf den Datensatz
statistiken <- deskriptive_statistiken(neue_titanic)
print(statistiken)


# ii. Funktion für kategoriale Variablen, die Absolute und Relative Häufigkeiten, und Modus zurückgibt.
calculate_descriptive_categorical <- function(neue_titanic, cat_variable) {
  
  # Ueberprüfen, ob die Variable im Datenrahmen ist
  if (!cat_variable %in% names(neue_titanic)) {
    stop("Die angegebene kategoriale Variable ist im Datenrahmen nicht vorhanden.")
  }
  
  # Extrahiere die Spalte mit der kategorialen Variable
  variable_values <- neue_titanic[[cat_variable]]
  
  # Absolute Haeufigkeiten
  frequencies <- table(variable_values)
  
  # Relative Haeufigkeiten
  relative_frequencies <- prop.table(frequencies)
  
  # Modus
  mode_value <- as.character(names(frequencies)[which.max(frequencies)])
  
  # Zusammenfassung der Ergebnisse
  result_summary <- data.frame(
    Variable = cat_variable,
    Kategorie = as.character(names(frequencies)),
    Absolute_Haeufigkeiten = as.vector(frequencies),
    Relative_Haeufigkeiten = as.vector(relative_frequencies),
    Modus = mode_value
  )
  
  # Rueckgabe der Ergebnisse
  return(result_summary)
}

#Beispiel:
calculate_descriptive_categorical(neue_titanic, cat_variable = "Title")

#iii)

# Funktion für bivariate Statistiken zwischen zwei kategorialen Variablen
## Die Funktion berechnet die Beziehung zwischen zwei Variablen durch Berechnung einer Chi-squared Test
calculate_bivariate_categorical <- function(data_frame, var1, var2) {
  
  # Überprüfen, ob die Variablen im Datenrahmen vorhanden sind
  if (!(var1 %in% names(data_frame)) | !(var2 %in% names(data_frame))) {
    stop("Eine oder beide angegebenen Variablen sind im Datenrahmen nicht vorhanden.")
  }
  
  # Kreuztabelle erstellen
  cross_tab <- table(data_frame[[var1]], data_frame[[var2]], useNA = "always")
  
  # Chi-Quadrat-Test durchführen
  chi_sq_test <- chisq.test(cross_tab)
  
  # Zusammenfassung der Ergebnisse
  result_summary <- list(
    Kreuztabelle = cross_tab,
    Chi_Quadrat_Test = chi_sq_test
  )
  
  # Rückgabe der Ergebnisse
  return(result_summary)
}

# Beispielaufruf der Funktion:
# calculate_bivariate_categorical(neue_titanic, "Variable1", "Variable2")

#iv)deskriptive bivariate Statistiken für den Zusammengang zwischen 
#einer metrischen und einer dichotomen Variablen 

compute_descriptive_stats <- function(metric_var, dichotomous_var) {
  if (!is.numeric(metric_var)) {
    stop("Die metrische Variable muss numerisch sein.")
  }
  if (!is.factor(dichotomous_var)) {
    stop("Die dichotome Variable muss ein Faktor sein.")
  }
  
  # Korrelation berechnen
  correlation <- cor.test(metric_var, as.numeric(dichotomous_var))
  
  # T-Test durchführen
  t_test <- t.test(metric_var ~ dichotomous_var)
  
  # Deskriptive Statistiken
  mean_group1 <- mean(metric_var[dichotomous_var == levels(dichotomous_var)[1]])
  mean_group2 <- mean(metric_var[dichotomous_var == levels(dichotomous_var)[2]])
  sd_group1 <- sd(metric_var[dichotomous_var == levels(dichotomous_var)[1]])
  sd_group2 <- sd(metric_var[dichotomous_var == levels(dichotomous_var)[2]])
  
  # Ergebnisse ausgeben
  cat("Korrelation zwischen den Variablen:\n")
  print(correlation)
  cat("\n\n")
  cat("T-Test zwischen den Gruppen:\n")
  print(t_test)
  cat("\n\n")
  cat("Deskriptive Statistiken für Gruppe 1:\n")
  cat("Mittelwert:", mean_group1, "\n")
  cat("Standardabweichung:", sd_group1, "\n\n")
  cat("Deskriptive Statistiken für Gruppe 2:\n")
  cat("Mittelwert:", mean_group2, "\n")
  cat("Standardabweichung:", sd_group2, "\n")
}


#v)

# Define the function
#Eine Grafik, die der Alter, Geschlecht, Passagierklasse und Überlebensrate beschreibt.
plot_age_survival <- function(data) {
  data %>%
    ggplot(aes(x = Age, fill = factor(Survived))) +
    geom_histogram(binwidth = 15) +
    facet_wrap(~Sex + Pclass) +
    theme_test() +
    theme(
      plot.title = element_text(family = "Times New Roman", hjust = 0.5),
      axis.text = element_text(family = "Times New Roman", face = "bold"),
      axis.title = element_text(family = "Times New Roman", face = "bold"),
      legend.title = element_blank(),
      legend.text = element_text(family = "Times New Roman")
    ) +
    labs(title = "Survival rates Age, Sex and Passenger class")
}


plot_age_survival(neue_titanic)


#iv) Weitere Funktionen 

# Korrelationsmatrix für numerische Variablen
korrelation <- cor(neue_titanic[, sapply(neue_titanic, is.numeric)], use = "complete.obs")
print(korrelation)


# Boxplot für die Variable 'Alter'
boxplot(neue_titanic$Age, main = "Boxplot des Alters", ylab = "Age")
