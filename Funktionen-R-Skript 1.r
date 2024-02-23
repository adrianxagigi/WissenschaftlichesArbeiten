load("neue_titanic.RData")
install.packages("magrittr")
install.packages("tidyverse")

library(tidyverse)
library(ggplot2)

#i)

# Funktion zur Berechnung deskriptiver Statistiken
deskriptive_statistiken <- function(df) {
  numerische_variablen <- sapply(df, is.numeric)
  df_numerisch <- df[, numerische_variablen]
  
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


# ii. Funktion fuer kategoriale Variablen
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
calculate_bivariate_categorical <- function(data_frame, var1, var2) {
  
  # Überprüfen, ob die Variablen im Datenrahmen vorhanden sind
  if (!(var1 %in% names(data_frame)) | !(var2 %in% names(data_frame))) {
    stop("Eine oder beide angegebenen Variablen sind im Datenrahmen nicht vorhanden.")
  }
  
  # Kreuztabelle erstellen
  cross_tab <- table(data_frame[[var1]], data_frame[[var2]])
  
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

#iv)

#iv. T-test fuer die Variablen Survived und Pclass

compare_means_ttest <- function(metric_variable, dich_variable, data) {
  #Subset the two groups
  survived <- data[data[[dich_variable]] == 0, metric_variable]
  deceased <- data[data[[dich_variable]] == 1, metric_variable]
  
  #t-test
  t_test_result <- t.test(survived, deceased)
  
  #Print results
  cat("T-Test Results:\n")
  cat("-------------\n")
  cat("Survived Mean:", mean(survived), "\n")
  cat("Dead Mean:", mean(deceased), "\n")
  cat("\n")
  print(t_test_result)
}

#Example:
compare_means_ttest("Pclass", "Survived", neue_titanic)

#v)

# Define the function
plot_age_survival <- function(data) {
  data %>%
    ggplot(aes(x = Age, fill = Survived)) +
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