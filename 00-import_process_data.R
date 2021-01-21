# Import CSV
library(readr)
umfrage_final_modified <-
  read_delim(
    "Survey/umfrage_final_modified.csv",
    ";",
    escape_double = FALSE,
    col_types = cols(
      `Datum Abgeschickt` = col_datetime(format = "%d.%m.%y %H:%M"),
      `Datum gestartet` = col_datetime(format = "%d.%m.%y %H:%M"),
      `Datum letzte Aktivität` = col_datetime(format = "%d.%m.%y %H:%M")
    ),
    trim_ws = TRUE
  )

# Import Dataframe
df <- umfrage_final_modified

rm ## TRANSFORM RANKINGS
# Hard Skills
new_cols <-
  c(
    "Kennntisse von Computer-Hardware",
    "Kenntnisse von Cloud Computing",
    "Kenntnisse von Cybersecurity",
    "Kenntnisse von Datenbanken",
    "Kenntnisse von Data-Engineering / Datenanalyse ",
    "Kenntnisse von ERP Systemen (SAP etc.)",
    "Kenntnisse von Graphical User Interface Design",
    "Kenntnisse von Netzwerktechnik",
    "Kenntnisse von Betriebssystemen",
    "Kenntnisse von Programmiersprachen",
    "Kenntnisse von Softwareentwicklung (generell)",
    "Kenntnisse von Web-Development"
  )

# Generate new columns
for (col in 1:length(new_cols)) {
  df <-
    df %>% add_column(!!(new_cols[col]) := NA, .before = "Haben wir in unserer Auflistung noch wichtige Hard Skills vergessen?")
}

# Find first and last column
s <- which(colnames(df) == new_cols[1])
e <- which(colnames(df) == new_cols[length(new_cols)])

# Convert to numeric
df[, c(s:e)] <- sapply(df[, c(s:e)], as.numeric)

ranking_name <- "Hard Skills"

# Transform
for (col in 1:length(new_cols)) {
  for (i in 1:nrow(df)) {
    for (j in 1:5) {
      if (!is.na(df[i, paste(ranking_name, " [Rank ", j, "]", sep = "")]) &&
          df[i, paste(ranking_name, " [Rank ", j, "]", sep = "")] == new_cols[col]) {
        df[i, new_cols[col]] = j
      }
    }
  }
}

# Soft Skills
new_cols <-
  c(
    "Anpassungsfähigkeit (zB. Umgang mit Veränderungen)",
    "Innnovationsfähigkeit",
    "Kommunikations-/Präsentationsfähigkeiten",
    "Kreativität",
    "Zwischenmenschliche Fähigkeiten",
    "Wissen über agile Methodiken (oder ähnliches)",
    "Führungsqualitäten",
    "Management- & Organisationsfähigkeiten",
    "Problemlösungsfähigkeiten",
    "Teamfähigkeit",
    "Motivation zur ständigen Weiterbildung",
    "Kundenumgang"
  )

for (col in 1:length(new_cols)) {
  df <-
    df %>% add_column(!!(new_cols[col]) := NA, .before = "Haben wir in unserer Auflistung noch wichtige Soft Skills vergessen?")
}

s <- which(colnames(df) == new_cols[1])
e <- which(colnames(df) == new_cols[length(new_cols)])

df[, c(s:e)] <- sapply(df[, c(s:e)], as.numeric)

ranking_name <- "Soft Skills"

for (col in 1:length(new_cols)) {
  for (i in 1:nrow(df)) {
    for (j in 1:5) {
      if (!is.na(df[i, paste(ranking_name, " [Rank ", j, "]", sep = "")]) &&
          df[i, paste(ranking_name, " [Rank ", j, "]", sep = "")] == new_cols[col]) {
        df[i, new_cols[col]] = j
      }
    }
  }
}

# Arbeitgeber Faktoren
new_cols <-
  c(
    "Autonomie bei der Arbeit",
    "Unternehmenskultur (zB. Casual Fridays)",
    "Image des Unternehmens",
    "Flexible Arbeitsgestaltung (zB. Flexible/r Arbeitszeit, -ort etc.)",
    "Gute Aufstiegsmöglichkeiten Im Unternehmen",
    "Hohes Gehalt",
    "Beschäftigungssicherheit",
    "Sinnhaftigkeit der Arbeit (zB. etwas mit der Arbeit bewirken)",
    "Ausbildungsmöglichkeiten (Training, Fortbildungen etc.)",
    "Arbeitsumgebung (bsp: Arbeit im Team)",
    "Arbeiten an interessanten/herausfordernden Aufgaben",
    "Zusätzliche Leistungen (Versicherung, Bonus, etc.)"
  )

for (col in 1:length(new_cols)) {
  df <-
    df %>% add_column(!!(new_cols[col]) := NA, .before = "Haben wir in unserer Auflistung noch wichtige Faktoren für die Arbeitgeberauswahl vergessen?")
}

s <- which(colnames(df) == new_cols[1])
e <- which(colnames(df) == new_cols[length(new_cols)])

df[, c(s:e)] <- sapply(df[, c(s:e)], as.numeric)

ranking_name <- "Arbeitgeber"

for (col in 1:length(new_cols)) {
  for (i in 1:nrow(df)) {
    for (j in 1:5) {
      if (!is.na(df[i, paste(ranking_name, " [Rank ", j, "]", sep = "")]) &&
          df[i, paste(ranking_name, " [Rank ", j, "]", sep = "")] == new_cols[col]) {
        df[i, new_cols[col]] = j
      }
    }
  }
}

# Give Columns a more friendly name
names(df)[names(df) == 'Autonomie bei der Arbeit'] <- 'Autonomie bei der Arbeit'
names(df)[names(df) == 'Unternehmenskultur (zB. Casual Fridays)'] <- 'Unternehmenskultur'
names(df)[names(df) == 'Image des Unternehmens'] <- 'Unternehmensimage'
names(df)[names(df) == 'Flexible Arbeitsgestaltung (zB. Flexible/r Arbeitszeit, -ort etc.)'] <- 'Flexible Arbeitsgestaltung'
names(df)[names(df) == 'Gute Aufstiegsmöglichkeiten Im Unternehmen'] <- 'Gute Aufstiegsmöglichkeiten'
names(df)[names(df) == 'Hohes Gehalt'] <- 'Hohes Gehalt'
names(df)[names(df) == 'Beschäftigungssicherheit'] <- 'Beschäftigungssicherheit'
names(df)[names(df) == 'Sinnhaftigkeit der Arbeit (zB. etwas mit der Arbeit bewirken)'] <- 'Sinnhaftigkeit der Arbeit'
names(df)[names(df) == 'Ausbildungsmöglichkeiten (Training, Fortbildungen etc.)'] <- 'Ausbildungsmöglichkeiten'
names(df)[names(df) == 'Arbeitsumgebung (bsp: Arbeit im Team)'] <- 'Arbeitsumgebung'
names(df)[names(df) == 'Arbeiten an interessanten/herausfordernden Aufgaben'] <- 'Interessante/herausfordernden Aufgaben'
names(df)[names(df) == 'Zusätzliche Leistungen (Versicherung, Bonus, etc.)'] <- 'Zusätzliche Leistungen'

# Clean up 
rm(col, e, i, j, s, new_cols, ranking_name)