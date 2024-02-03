#Funktionen-R-Skript 1

load("neue_titanic.RData")
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

