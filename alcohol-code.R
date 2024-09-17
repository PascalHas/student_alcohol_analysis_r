# Complete R Code of alcohol-consumption.qmd
# Author: Pascal Hasenkamp
# Date: 17th September 2024

library(tidyverse)
library(tidymodels)
library(tidyverse) 
library(tidymodels)
library(explore) 
library(rpart.plot)
library(corrplot)
library(GGally)
library(tidyr)
library(dplyr)
library(ggplot2)

d_mat <- read.csv("dataset/student-mat.csv", header = TRUE, fileEncoding = "UTF-8")
d_por <- read.csv("dataset/student-por.csv", header = TRUE, fileEncoding = "UTF-8")

# Anzeigen der Spaltennamen in Datensatz Mathe
print(colnames(d_mat))
# Anzeigen der Spaltennamen in Datensatz Portugiesisch
print(colnames(d_por))

common_columns <- c("school", "sex", "age", "address", "famsize", "Pstatus", 
                    "Medu", "Fedu", "Mjob", "Fjob", "reason", "nursery", "internet")

# Vorhanden in beiden Datensätzen?
if (!all(common_columns %in% colnames(d_mat)) | !all(common_columns %in% colnames(d_por))) {
  stop("Nicht alle angegebenen Spalten sind in beiden Datensätzen vorhanden.")
}

untidy_student_data <- merge(d_mat, d_por, by = common_columns)

student_data <- untidy_student_data %>%
  pivot_longer(
    cols = ends_with(c(".x", ".y")), 
    names_to = c(".value", "course"),
    names_sep = "\\."
  ) %>%
  
  mutate(course = ifelse(course == "x", "Mathematik", "Portugiesisch"))

describe_tbl(student_data)

# only uncomment, if you want to save the data externally
# write.csv(student_data, file = "dataset/student_data.csv", row.names = FALSE)

knitr::kable(head(student_data), caption = "Das Spaltenformat des eingelesenen Studentendatensatzes")
glimpse(student_data)
summary(student_data)

numeric_vars <- student_data %>% select(where(is.numeric))

numeric_vars %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Wert") %>%
  ggplot(aes(x = Variable, y = Wert)) +
  geom_boxplot() +
  coord_flip() +  # bessere Lesbarkeit durch links/rechts Darstellung
  theme_minimal() +
  labs(title = "Boxplots zur Identifizierung von Ausreißern in numerischen Variablen")

summary(numeric_vars)

remove_outliers <- function(data) {
  numeric_vars <- data %>% select(where(is.numeric))
  
  for (var in names(numeric_vars)) {
    Q1 <- quantile(numeric_vars[[var]], 0.25, na.rm = TRUE)
    Q3 <- quantile(numeric_vars[[var]], 0.75, na.rm = TRUE)
    IQR <- Q3 - Q1
    
    # Berechnung der oberen und unteren Grenzen für die Ausreißer
    lower_bound <- Q1 - 1.5 * IQR
    upper_bound <- Q3 + 1.5 * IQR
    
    # Filtern der Zeilen, die innerhalb der Grenzen liegen
    data <- data %>% filter((!!sym(var) >= lower_bound) & (!!sym(var) <= upper_bound))
  }
  
  return(data)
}

student_data_cleaned <- remove_outliers(student_data)
summary(student_data_cleaned)

ggplot(student_data_cleaned, aes(x = G1)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Verteilung der Noten in der ersten Periode (G1)", x = "Note", y = "Häufigkeit")

ggplot(student_data_cleaned, aes(x = G2)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Verteilung der Noten in der zweiten Periode (G2)", x = "Note", y = "Häufigkeit")

ggplot(student_data_cleaned, aes(x = G3)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Verteilung der Abschlussnoten (G3)", x = "Note", y = "Häufigkeit")

ggplot(student_data_cleaned, aes(x = address, y = G3, fill = address)) +
  geom_boxplot() +
  labs(title = "Einfluss des Wohnorts auf die Abschlussnote", x = "Wohnort (U = städtisch, R = ländlich)", y = "Abschlussnote (G3)")

ggplot(student_data_cleaned, aes(x = factor(Medu), y = G3, fill = factor(Medu))) +
  geom_boxplot() +
  labs(title = "Einfluss des Bildungsniveaus der Mutter auf die Abschlussnote", x = "Bildungsniveau der Mutter", y = "Abschlussnote (G3)")

ggplot(student_data_cleaned, aes(x = factor(Fedu), y = G3, fill = factor(Fedu))) +
  geom_boxplot() +
  labs(title = "Einfluss des Bildungsniveaus des Vaters auf die Abschlussnote", x = "Bildungsniveau des Vaters", y = "Abschlussnote (G3)")

ggplot(student_data_cleaned, aes(x = internet, y = G3, fill = internet)) +
  geom_boxplot() +
  labs(title = "Einfluss von Internetzugang auf die Abschlussnote", x = "Internet zu Hause (Ja/Nein)", y = "Abschlussnote (G3)")



ggplot(student_data_cleaned, aes(x = absences, y = G3)) +
  geom_point(alpha = 0.6, color = "darkgrey", size = 2) +
  geom_smooth(formula = y ~ x,method = "lm", se = TRUE, color = "red", linetype = "dashed") + 
  labs(title = "Zusammenhang zwischen Abwesenheiten und Abschlussnote",
       x = "Anzahl der Abwesenheiten (absences)",
       y = "Abschlussnote (G3)") +
  theme_minimal()

sd_check <- sapply(student_data_cleaned %>% select(where(is.numeric)), sd, na.rm = TRUE)
constant_vars <- names(sd_check[sd_check == 0])
print(constant_vars)

student_data_cleaned_non_constant <- student_data_cleaned %>% select(-failures)
cor_matrix <- cor(student_data_cleaned_non_constant %>% select(where(is.numeric)), use = "complete.obs")
par(mar = c(1, 1, 4, 1))
corrplot(cor_matrix, method = "color", type = "upper", tl.cex = 0.7)


