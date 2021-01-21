sexTable <- table(df %>% select("Geschlecht"))
studAb_M <- table(df %>% filter(df[["Geschlecht"]] == "Männlich") %>% select("Studienabschnitt"))
studAb_W <- table(df %>% filter(df[["Geschlecht"]] == "Weiblich") %>% select("Studienabschnitt"))
studGa_M <- table(df %>% filter(df[["Geschlecht"]] == "Männlich") %>% select("Studiengang"))
studGa_W <- table(df %>% filter(df[["Geschlecht"]] == "Weiblich") %>% select("Studiengang"))
sem_M <- table(df %>% filter(df[["Geschlecht"]] == "Männlich") %>% select("Semester"))
sem_W <- table(df %>% filter(df[["Geschlecht"]] == "Weiblich") %>% select("Semester"))

table <- data.frame(Kategorie = "Geschlecht", var = "", M = sexTable[1], W = sexTable[2])

table <- table %>% add_row(Kategorie = "Studienabschnitt", var = "Bachelor-Abschluss", M = studAb_M["Bachelor-Abschluss"], W = studAb_W["Bachelor-Abschluss"])
table <- table %>% add_row(Kategorie = "Studienabschnitt", var = "Master-Abschluss", M = studAb_M["Master- Abschluss"], W = studAb_W["Master- Abschluss"])
table <- table %>% add_row(Kategorie = "Studienabschnitt", var = "Doktorat", M = studAb_M["Doktorat"], W = studAb_W["Doktorat"])

studGa <- c("Data Engineering/Data Science (oder ähnliches)", "Finance and Information Management (FIM)", "Informatik/Computerwissenschaften", "Informatik: Games Engineering", "Lehramt: Naturwissenschaftliche Bildung/Berufliche Bildung", "Mathematik (oder ähnliches)", "Medieninformatik", "Physik (oder ähnliches)", "Robotics, Cognition, Intelligence", "Sonstiges", "Technische Redaktion und Kommunikation", "Technology & Management (oder ähnliches)", "Wirtschaftsinformatik")
for(i in 1:length(studGa)){
  if(studGa[i] != "Mathematik (oder ähnliches)") {
    table <- table %>% add_row(Kategorie = "Studiengang", var = studGa[i], M = studGa_M[studGa[i]], W = studGa_W[studGa[i]])
  } else {
    table <- table %>% add_row(Kategorie = "Studiengang", var = studGa[i], M = studGa_M["Mathematik"], W = studGa_W[studGa[i]])
  }
}

table <- table %>% add_row(Kategorie = "Semester", var = "1.-2. Semester", M = (sem_M["1"] + sem_M["2"]), W = (sem_W["1"] + sem_W["2"]))
table <- table %>% add_row(Kategorie = "Semester", var = "3.-4. Semester", M = (sem_M["3"] + sem_M["4"]), W = (sem_W["3"] + sem_W["4"]))
table <- table %>% add_row(Kategorie = "Semester", var = "5.-6. Semester", M = (sem_M["5"] + sem_M["6"]), W = (sem_W["5"]))
table <- table %>% add_row(Kategorie = "Semester", var = "6.-... Semester", M = sem_M[">6"], W = sem_W[">6"])


row.names(table) <- NULL
reactable(
  table,
  pagination = FALSE,
  groupBy = "Kategorie",
  outlined = TRUE,
  theme = reactableTheme(
    borderColor = "#dfe2e5",
    stripedColor = "#f6f8fa",
    highlightColor = "#f0f5f9",
    cellPadding = "8px 12px",
    style = list(fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif"),
    searchInputStyle = list(width = "100%")
  ),
  columnGroups = list(
    colGroup(name = "Geschlecht", columns = c("M", "W"))
  )
)