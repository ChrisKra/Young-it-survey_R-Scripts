# First and last question
s <- which(colnames(df) == "Karriere [Der Prozess der Beaufsichtigung, Beeinflussung, Führung und Kontrolle von Menschen ist...]") # First column
e <- which(colnames(df) == "Karriereplanung [Ich wollte schon immer ein eigenes Unternehmen gründen und aufbauen.]") # Last column

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
  
  tmp_df$m <- rcolumnToVector(prop.table(table(df %>% filter(df[["Geschlecht"]] == "Männlich") %>% select(s+colNum-1))))
  tmp_df$w <- rcolumnToVector(prop.table(table(df %>% filter(df[["Geschlecht"]] == "Weiblich") %>% select(s+colNum-1))))

  df_list[[colNum]] <- tmp_df
}

# Graph settings
smoothingAmount = 7.5/10
colorM = "#31aad9"
colorW = "#b83578"
areaAlpha = 0.4
lineSize = 1.5
indWidth = 500
indHeight = 500
col = 3;

plist <- vector("list", nColumns) # List of all graphs

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

for(p in 1:nColumns) {
  plist[[p]] <- ggplot(df_list[[p]], aes(x=rank)) +
    stat_smooth(geom = 'area', method = 'loess', span = smoothingAmount, aes(y = m), fill=colorM, alpha=areaAlpha) +
    stat_smooth(geom = 'line', method = 'loess', span = smoothingAmount, aes(y = m), color=colorM, size=lineSize) +
    stat_smooth(geom = 'area', method = 'loess', span = smoothingAmount, aes(y = w), fill=colorW, alpha=areaAlpha) +
    stat_smooth(geom = 'line', method = 'loess', span = smoothingAmount, aes(y = w), color=colorW, size=lineSize) +
    scale_x_continuous(labels=c("Nicht wichtig", "Mäßig wichtig", "Etwas wichtig", "Sehr wichtig", "Von zentraler\nBedeutung")) +
    labs(x = "Relevanz", y = "Anteil") +
    ggtitle(wrap_sentence(colnames(df)[s+p-1], 60)) +
    theme_minimal() +
    theme(plot.margin = margin(2.5, 1.5, 1.5, 1, "cm"), 
          plot.title = element_text(hjust = 0.5, vjust = 6, size = 15, face = "bold"))
  
  print(paste("Generating graph", p))
}

png(file = "Generated_Charts/CarAnch_Charts_Overview.png",width=(col*indWidth), height=(ceiling(nColumns/col)*indHeight), pointsize = 22)

ggarrange(plotlist = plist, ncol = col, nrow = ceiling(nColumns/col))

dev.off()

# Clean up 
rm(df_list, plist, tmp_df, areaAlpha, col, colNum, colorM, colorW, e, indHeight, indWidth, lineSize, maxValue, nColumns, p, s, smoothingAmount)