#Funktionen-R-Skript 1

load("neue_titanic.RData")
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

