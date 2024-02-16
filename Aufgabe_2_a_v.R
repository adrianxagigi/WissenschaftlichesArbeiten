
install.packages("magrittr")
install.packages("tidyverse")

library(tidyverse)
library(ggplot2)

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