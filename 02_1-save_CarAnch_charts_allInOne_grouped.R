# First and last question
s <- which(colnames(df) == "Karriere [Der Prozess der Beaufsichtigung, Beeinflussung, Führung und Kontrolle von Menschen ist...]") # First column
e <- which(colnames(df) == "Karriereplanung [Ich wollte schon immer ein eigenes Unternehmen gründen und aufbauen.]") # Last column

exclude <- which(colnames(df) == "Karriere [In meinem Fachgebiet zu bleiben im Gegensatz zur Beförderung außerhalb meines Fachgebietes ist...]") # Exclude column with wrong translation 

# Maxmimum answer value (5 for Karriereorientierung 1 and 7 for Karriereorientierung 2)
maxValue <- 5

nColumns <- (e-s+1)
df_list <- vector("list", nColumns)

# Function to convert column to vector
rcolumnToVector <- function(rcolumn) {
  tmp = rep(0, maxValue)
  for(j in 1:maxValue) {
    if(!is.na(rcolumn[j])) {
      tmp[j] = rcolumn[j]
    }
  }
  return(tmp)
}

for(colNum in 1:nColumns) {
  # can be replaced with more friedly name 
  tmp_df <- data.frame(rank = c(1:maxValue), m = rep(0, maxValue), w = rep(0, maxValue))
  if ((s+colNum-1) != -1) { # replace -1 with exclude
    tmp_df$m <- rcolumnToVector(prop.table(table(df %>% filter(df[["Geschlecht"]] == "Männlich") %>% select(s+colNum-1))))
    tmp_df$w <- rcolumnToVector(prop.table(table(df %>% filter(df[["Geschlecht"]] == "Weiblich") %>% select(s+colNum-1))))
  } else {
    tmp_df$m <- rcolumnToVector(prop.table(table(df %>% filter(df[["Start-Sprache"]] == "de" & df[["Geschlecht"]] == "Männlich") %>% select(s+colNum-1))))
    tmp_df$w <- rcolumnToVector(prop.table(table(df %>% filter(df[["Start-Sprache"]] == "de" & df[["Geschlecht"]] == "Weiblich") %>% select(s+colNum-1))))
  }
  

  df_list[[colNum]] <- tmp_df
}

# Group individual tables by Factor
mapping <- data.frame(c(13, 17, 25), c(10, 11, 20), c(8, 9, 22), c(1, 2, 3), c(15, 19, 24), c(14, 18, NA), c(6, 7, 21), c(4, 5, NA), c(12, 16, 23))
desc_de <- c("Unternehmerische Kreativität", "Fachkompetenz", "Sinnstiftung", "Führungskompetenz", "Lifestyle", "Geografische Sicherheit", "Autonomie", "Arbeitsplatzsicherheit", "Herausforderung")
desc_en <- c("Entrepreneurship", "Technical", "Service", "Managerial", "Lifestyle", "Security-Geographic", "Autonomy", "Security-Job tenure", "Pure challange")
desc <- desc_de

grouped_list <- vector("list", length(desc))
for(i in 1:length(desc)) {
  if (!is.na(mapping[3, i])) {
    grouped_list[[i]] <- (df_list[[mapping[1, i]]] + df_list[[mapping[2, i]]] + df_list[[mapping[3, i]]])/3
  } else {
    grouped_list[[i]] <- (df_list[[mapping[1, i]]] + df_list[[mapping[2, i]]])/2
  }
  
  # Export tables as CSV
  write.table(round(grouped_list[[i]], 4), file = paste("CSV_exports/CarAnch_", i, "-", str_replace_all(desc[i], "/", "_"), ".csv", sep=""), row.names = FALSE, sep=";")
}

# Graph settings
smoothingAmount = 7.5/10
colorM = "#31aad9"
colorW = "#b83578"
areaAlpha = 0.4
lineSize = 1.5
indWidth = 500
indHeight = 450
col = 3;

plist <- vector("list", length(desc)) # List of all graphs

# Function to wrap sentences to fit width
wrap_sentence <- function(string, width) {
  words <- unlist(strsplit(string, " "))
  fullsentence <- ""
  checklen <- ""
  for(i in 1:length(words)) {
    checklen <- paste(checklen, words[i])
    if(nchar(checklen)>(width+1)) {
      fullsentence <- paste0(fullsentence, "\n")
      checklen <- ""
    }
    fullsentence <- paste(fullsentence, words[i])
  }
  fullsentence <- sub("^\\s", "", fullsentence)
  fullsentence <- gsub("\n ", "\n", fullsentence)
  return(fullsentence)
}

for(p in 1:length(desc)) {
  plist[[p]] <- ggplot(grouped_list[[p]], aes(x=rank)) +
    stat_smooth(geom = 'area', method = 'loess', span = smoothingAmount, aes(y = m), fill=colorM, alpha=areaAlpha) +
    stat_smooth(geom = 'line', method = 'loess', span = smoothingAmount, aes(y = m), color=colorM, size=lineSize) +
    stat_smooth(geom = 'area', method = 'loess', span = smoothingAmount, aes(y = w), fill=colorW, alpha=areaAlpha) +
    stat_smooth(geom = 'line', method = 'loess', span = smoothingAmount, aes(y = w), color=colorW, size=lineSize) +
    scale_x_continuous(labels=c("Nicht wichtig", "Mäßig wichtig", "Etwas wichtig", "Sehr wichtig", "Von zentraler\nBedeutung")) +
    labs(x = "Relevanz", y = "Anteil") +
    ggtitle(wrap_sentence(desc[p], 60)) +
    theme_minimal() +
    theme(plot.margin = margin(1, 1, 0.8, 0.5, "cm"), 
          axis.text=element_text(size=15),
          axis.title = element_text(size = 18),
          plot.title = element_text(hjust = 0.5, vjust = 6, size = 20, face = "bold"))
  
  print(paste("Generating graph", p))
}

png(file = "Generated_Charts/CarAnch_Charts_Overview.png",width=(col*indWidth), height=(ceiling(length(desc)/col)*indHeight), pointsize = 22)

ggarrange(plotlist = plist, ncol = col, nrow = ceiling(length(desc)/col))

dev.off()

# Clean up 
rm(excludes, df_list, plist, tmp_df, areaAlpha, col, colNum, colorM, colorW, e, indHeight, indWidth, lineSize, maxValue, nColumns, p, s, smoothingAmount, desc, desc_de, desc_en, i, grouped_list, mapping)