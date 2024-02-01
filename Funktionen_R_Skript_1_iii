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
