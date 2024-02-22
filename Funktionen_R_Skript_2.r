# Hilfsfunktion zur Überprüfung, ob eine Variable numerisch ist
check_numeric <- function(df, var) {
  if (!is.numeric(df[[var]])) {
    stop(paste("Die Variable", var, "ist nicht numerisch. Bitte wählen Sie eine numerische Variable."))
  }
}
