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
library(parsnip)
library(yardstick)
library(caret)
library(ranger)
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

# only uncomment, if you want to safe the data locally
#write.csv(student_data, file = "dataset/student_data.csv", row.names = FALSE)

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
corrplot(cor_matrix, method = "color", type = "upper", tl.cex = 0.7)

avg_grade_by_family <- aggregate(G3 ~ Pstatus, data = student_data, FUN = mean)
print(avg_grade_by_family)

ggplot(student_data_cleaned, aes(x = Pstatus, y = G3, fill = Pstatus)) +
  geom_boxplot() +
  labs(title = "Einfluss des Familienstatus auf die Abschlussnote",
       x = "Familienstatus (T = zusammen, A = getrennt)",
       y = "Abschlussnote (G3)")

correlation_failures_grade <- cor(student_data$failures, student_data$G3)

ggplot(student_data, aes(x = failures, y = G3)) +
  geom_point() +
  geom_smooth(formula = y ~ x,method = "lm", se = FALSE, color = "red") +
  labs(title = "Zusammenhang zwischen Fehlversuchen und Abschlussnote", x = "Anzahl der Fehlversuche", y = "Abschlussnote")

avg_grade_by_sex <- aggregate(G3 ~ sex, data = student_data_cleaned, FUN = mean)
print(avg_grade_by_sex)

ggplot(student_data_cleaned, aes(x = sex, y = G3, fill = sex)) +
  geom_boxplot() +
  labs(title = "Einfluss des Geschlechts auf die Abschlussnote",
       x = "Geschlecht (M = männlich, F = weiblich)",
       y = "Abschlussnote (G3)")

avg_grade_by_paid <- aggregate(G3 ~ paid, data = student_data_cleaned, FUN = mean)

ggplot(student_data_cleaned, aes(x = paid, y = G3, fill = paid)) +
  geom_boxplot() +
  labs(title = "Einfluss von Nachhilfe auf die Abschlussnote",
       x = "Nachhilfe (yes = ja, no = nein)",
       y = "Abschlussnote (G3)")

set.seed(123)
index <- createDataPartition(student_data_cleaned$G3, p = 0.8, list = FALSE)
train <- student_data_cleaned[index, ]
test <- student_data_cleaned[-index, ]

cols_to_scale <- train %>% select(where(is.numeric)) %>% colnames()
train_scale <- train %>%
  mutate(across(all_of(cols_to_scale), ~ scale(.) %>% as.vector))
test_scale <- test %>%
  mutate(across(all_of(cols_to_scale), ~ scale(.) %>% as.vector))
summary(train_scale)

linear_model <- lm(G3 ~ studytime + famrel + health, data = train_scale)

summary(linear_model)

predictions_linear <- predict(linear_model, newdata = test_scale)

mse_linear <- mean((predictions_linear - test_scale$G3)^2)

print(paste("MSE für Lineare Regression:", mse_linear))

linear_model_new <- lm(G3 ~ studytime + famrel + health + age + absences + goout, data = train_scale)
predictions_linear_new <- predict(linear_model_new, newdata = test_scale)
mse_linear_new <- mean((predictions_linear_new - test_scale$G3)^2)
print(paste("MSE für Lineare Regression mit zusätzlichen Variablen:", mse_linear_new))

v_actual <- test_scale$G3
v_predicted <- predictions_linear_new

# Berechnung der Residuen
v_residuals <- v_actual - v_predicted

# Histogramm der Residuen
hist(v_residuals, breaks = 10, col = "skyblue", 
     xlab = "Residuen (Fehler)", 
     main = "Verteilung der Vorhersagefehler (Residuen)",
     border = "white")

set.seed(123)
cv_splits <- vfold_cv(train_scale, v = 10)
linear_spec <- linear_reg() %>%
  set_engine("lm")

cv_results_linear <- fit_resamples(
  linear_spec,
  G3 ~ studytime + famrel + health + age + absences + goout,
  resamples = cv_splits,
  metrics = metric_set(rmse, rsq)
)

collect_metrics(cv_results_linear)

decision_tree_model <- decision_tree(cost_complexity = 0.01, 
                                     tree_depth = 10, 
                                     min_n = 5) %>%
  set_engine("rpart") %>%
  set_mode("regression")

train_data_tree <- train_scale %>%
  select(G3, studytime, famrel, health, age, absences, goout)

tree_fit <- decision_tree_model %>%
  fit(G3 ~ studytime + famrel + health + age + absences + goout, 
      data = train_data_tree)

predictions_tree <- predict(tree_fit, new_data = test_scale) %>%
  pull(.pred)

mse_tree <- mean((predictions_tree - test_scale$G3)^2)
print(paste("MSE für Entscheidungsbaum:", mse_tree))

rpart.plot(tree_fit$fit, main = "Entscheidungsbaum für G3", roundint = FALSE)

decision_tree_spec <- decision_tree(cost_complexity = 0.01, 
                                    tree_depth = 10, 
                                    min_n = 5) %>%
  set_engine("rpart") %>%
  set_mode("regression")
cv_results_tree <- fit_resamples(
  decision_tree_spec,
  G3 ~ studytime + famrel + health + age + absences + goout,
  resamples = cv_splits,
  metrics = metric_set(rmse, rsq)
)

collect_metrics(cv_results_tree)

rand_forest_model <- rand_forest(mtry = sqrt(ncol(train_scale) - 1), 
                                 trees = 1000, 
                                 min_n = 5) %>% 
  set_engine("ranger") %>%  
  set_mode("regression")  

train_data_forest <- train_scale %>%
  select(G3, studytime, famrel, health, age, absences, goout)
rf_fit <- rand_forest_model %>%
  fit(G3 ~ studytime + famrel + health + age + absences + goout, 
      data = train_data_forest)

predictions_rf <- predict(rf_fit, new_data = test_scale) %>%
  pull(.pred)

mse_rf <- mean((predictions_rf - test_scale$G3)^2)
print(paste("MSE für Random Forest:", mse_rf))

rf_model_ranger <- ranger(G3 ~ studytime + famrel + health + age + absences + goout, 
                          data = train_scale, 
                          importance = "impurity",
                          num.trees = 1000)
importance_rf <- rf_model_ranger$variable.importance
importance_percent <- importance_rf / sum(importance_rf) * 100
importance_df <- data.frame(Feature = names(importance_percent), Importance = importance_percent)

ggplot(importance_df, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_col(fill = "skyblue") +
  ggtitle("Feature-Wichtigkeit im Random Forest (ranger)") +
  xlab("Feature") + 
  ylab("Wichtigkeit") +
  theme_minimal()

rf_spec <- rand_forest(mtry = sqrt(ncol(train_scale) - 1), trees = 1000, min_n = 5) %>%
  set_engine("ranger") %>%
  set_mode("regression")
cv_results_rf <- fit_resamples(
  rf_spec,
  G3 ~ studytime + famrel + health + age + absences + goout,
  resamples = cv_splits,
  metrics = metric_set(rmse, rsq)
)

collect_metrics(cv_results_rf)

# End of Code