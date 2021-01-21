# First and last question
s <- which(colnames(df) == "Karriere [Der Prozess der Beaufsichtigung, Beeinflussung, Führung und Kontrolle von Menschen ist...]") # First column
e <- which(colnames(df) == "Karriereplanung [Ich wollte schon immer ein eigenes Unternehmen gründen und aufbauen.]") # Last column

nColumns <- (e-s+1)

# Group individual columns by Factor
mapping <- data.frame(c(13, 17, 25), c(10, 11, 20), c(8, 9, 22), c(1, 2, 3), c(15, 19, 24), c(14, 18, NA), c(6, 7, 21), c(4, 5, NA), c(12, 16, 23))
desc_de <- c("Unternehmerische Kreativität", "Technische/funktionale Kompetenz", "Serviceorientierung/Sinnstiftung", "General-Management-Kompetenz", "Lebensstil", "Sicherheit-Geografisch", "Autonomie/Unabhängigkeit", "Sicherheit-Arbeitsplatzsicherheit", "Reine Herausforderung")
desc_en <- c("Entrepreneurship", "Technical", "Service", "Managerial", "Lifestyle", "Security-Geographic", "Autonomy", "Security-Job tenure", "Pure challange")
desc <- desc_en

grouped_list_m <- vector("list", length(desc))
grouped_list_w <- vector("list", length(desc))
for(i in 1:length(desc)) {
  if (!is.na(mapping[3, i])) {
    grouped_list_m[[i]] <- (df %>% filter(df[["Geschlecht"]] == "Männlich") %>% select(mapping[1, i]+s-1) 
                            + df %>% filter(df[["Geschlecht"]] == "Männlich") %>% select(mapping[2, i]+s-1) 
                            + df %>% filter(df[["Geschlecht"]] == "Männlich") %>% select(mapping[3, i]+s-1))/3
    grouped_list_w[[i]] <- (df %>% filter(df[["Geschlecht"]] == "Weiblich") %>% select(mapping[1, i]+s-1) 
                            + df %>% filter(df[["Geschlecht"]] == "Weiblich") %>% select(mapping[2, i]+s-1) 
                            + df %>% filter(df[["Geschlecht"]] == "Weiblich") %>% select(mapping[3, i]+s-1))/3
    # print("3")
  } else {
    grouped_list_m[[i]] <- (df %>% filter(df[["Geschlecht"]] == "Männlich") %>% select(mapping[1, i]+s-1) 
                            + df %>% filter(df[["Geschlecht"]] == "Männlich") %>% select(mapping[2, i]+s-1))/2
    grouped_list_w[[i]] <- (df %>% filter(df[["Geschlecht"]] == "Weiblich") %>% select(mapping[1, i]+s-1) 
                            + df %>% filter(df[["Geschlecht"]] == "Weiblich") %>% select(mapping[2, i]+s-1))/2
    # print("2")
  }
}

# Min., 1st Qu., Median, Mean, 3rd Qu., Max., NA's   

# M
sum <- str_split(summary(grouped_list_m[[1]]), ":")
frame <- data.frame(c(sum[[1]][2], sum[[2]][2], sum[[3]][2], sum[[4]][2], sum[[5]][2], sum[[6]][2], sum[[7]][2]))
# W
sum <- str_split(summary(grouped_list_w[[1]]), ":")
frame[2] = c(sum[[1]][2], sum[[2]][2], sum[[3]][2], sum[[4]][2], sum[[5]][2], sum[[6]][2], sum[[7]][2])

for(i in 2:length(desc)) {
  # M
  sum <- str_split(summary(grouped_list_m[[i]]), ":")
  frame[i*2-1] = c(sum[[1]][2], sum[[2]][2], sum[[3]][2], sum[[4]][2], sum[[5]][2], sum[[6]][2], sum[[7]][2])
  # W
  sum <- str_split(summary(grouped_list_w[[i]]), ":")
  frame[i*2] = c(sum[[1]][2], sum[[2]][2], sum[[3]][2], sum[[4]][2], sum[[5]][2], sum[[6]][2], sum[[7]][2])
}

# Export tables as CSV
write.table(frame, file = "CSV_exports/CarAnch_Summary.csv", row.names = FALSE, col.names = FALSE, sep=";")

# Clean up 
rm(frame, grouped_list_m, grouped_list_w, mapping, sum, desc, desc_de, desc_en, e, i, nColumns, s)