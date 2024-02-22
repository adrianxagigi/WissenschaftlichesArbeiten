# Funktion zur Behandlung von fehlenden Werten durch Mittelwert-Ersatz
handle_missing_values <- function(data) {
  for (col in colnames(data)) {
    if (anyNA(data[[col]])) {  # Überprüfen, ob die Spalte fehlende Werte enthält
      mean_value <- mean(data[[col]], na.rm = TRUE)  # Berechnung des Mittelwerts ohne NA
      data[[col]][is.na(data[[col]])] <- mean_value  # Ersetzen der fehlenden Werte durch den Mittelwert
    }
  }
  return(data)
}
