# Einlesen der beiden Studentendatensätze mit read.csv2 für das korrekte Trennzeichen
d_mat <- read.csv("dataset/student-mat.csv", header = TRUE, fileEncoding = "UTF-8")
d_por <- read.csv("dataset/student-por.csv", header = TRUE, fileEncoding = "UTF-8")

# Anzeigen der Spaltennamen in beiden Datensätzen
cat("Spaltennamen von student-mat.csv:\n")
print(colnames(d_mat))

cat("\nSpaltennamen von student-por.csv:\n")
print(colnames(d_por))

# Sicherstellen, dass die gemeinsamen Spalten korrekt angegeben sind
common_columns <- c("school", "sex", "age", "address", "famsize", "Pstatus", 
                    "Medu", "Fedu", "Mjob", "Fjob", "reason", "nursery", "internet")

# Überprüfen, ob alle Spalten in beiden Datensätzen vorhanden sind
if (!all(common_columns %in% colnames(d_mat)) | !all(common_columns %in% colnames(d_por))) {
  stop("Nicht alle angegebenen Spalten sind in beiden Datensätzen vorhanden.")
}

# Zusammenführen der Datensätze anhand der bereinigten gemeinsamen Spalten
student_data <- merge(d_mat, d_por, by = common_columns)

# Anzeigen der ersten Zeilen des zusammengeführten Datensatzes
knitr::kable(head(student_data), caption = "Das Spaltenformat des eingelesenen Studentendatensatzes")

#############
### NEUER CODE
#############

d_mat <- read.csv("dataset/student-mat.csv", header = TRUE, fileEncoding = "UTF-8")
d_por <- read.csv("dataset/student-por.csv", header = TRUE, fileEncoding = "UTF-8")
# Anzeigen der Spaltennamen in beiden Datensätzen
print(colnames(d_mat))
print(colnames(d_por))

# Korrekt angegeben?
common_columns <- c("school", "sex", "age", "address", "famsize", "Pstatus", 
                    "Medu", "Fedu", "Mjob", "Fjob", "reason", "nursery", "internet")

# Vorhanden?
if (!all(common_columns %in% colnames(d_mat)) | !all(common_columns %in% colnames(d_por))) {
  stop("Nicht alle angegebenen Spalten sind in beiden Datensätzen vorhanden.")
}
untidy_student_data <- merge(d_mat, d_por, by = common_columns)


#student_data
knitr::kable(head(untidy_student_data), caption = "Das Spaltenformat des eingelesenen Studentendatensatzes")




# Installieren und Laden der erforderlichen Bibliothek
#install.packages("tidyr")
library(tidyr)
library(dplyr)

# Umwandeln in Long-Format
student_data <- untidy_student_data %>%
  # Sammeln der kursbezogenen Spalten in ein Long-Format
  pivot_longer(
    cols = ends_with(c(".x", ".y")), 
    names_to = c(".value", "course"),
    names_sep = "\\."
  ) %>%
  # Kursnamen angeben
  mutate(course = ifelse(course == "x", "Mathematik", "Portugiesisch"))

# Überprüfen des neuen "tidy" Datensatzes
knitr::kable(head(student_data), caption = "Kombinierter 'tidy' Studentendatensatz")

# Exportieren des Datensatzes 'student_data' als CSV-Datei auf dem Desktop
#write.csv(student_data, file = "dataset/student_data.csv", row.names = FALSE)

str(student_data)

glimpse(student_data)

summary(student_data)