# REPLACE HERE!!!
s <- which(colnames(df) == "Autonomie bei der Arbeit") # First column
e <- which(colnames(df) == "Zusätzliche Leistungen") # Last column

nColumns <- (e-s+1)

# Min., 1st Qu., Median, Mean, 3rd Qu., Max., NA's   

# M
sum <- str_split(summary(df %>% filter(df[["Geschlecht"]] == "Männlich") %>% select(s)), ":")
frame <- data.frame(c(sum[[1]][2], sum[[2]][2], sum[[3]][2], sum[[4]][2], sum[[5]][2], sum[[6]][2], sum[[7]][2]))
# W
sum <- str_split(summary(df %>% filter(df[["Geschlecht"]] == "Weiblich") %>% select(s)), ":")
frame[2] = c(sum[[1]][2], sum[[2]][2], sum[[3]][2], sum[[4]][2], sum[[5]][2], sum[[6]][2], sum[[7]][2])

for(i in 2:nColumns) {
  # M
  sum <- str_split(summary(df %>% filter(df[["Geschlecht"]] == "Männlich") %>% select(s+i-1)), ":")
  frame[i*2-1] = c(sum[[1]][2], sum[[2]][2], sum[[3]][2], sum[[4]][2], sum[[5]][2], sum[[6]][2], sum[[7]][2])
  # W
  sum <- str_split(summary(df %>% filter(df[["Geschlecht"]] == "Weiblich") %>% select(s+i-1)), ":")
  frame[i*2] = c(sum[[1]][2], sum[[2]][2], sum[[3]][2], sum[[4]][2], sum[[5]][2], sum[[6]][2], sum[[7]][2])
}

# Export tables as CSV
write.table(frame, file = "CSV_exports/Prio_Ranking_Summary.csv", row.names = FALSE, col.names = FALSE, sep=";")

# Clean up 
rm(frame, sum, e, i, nColumns, s)