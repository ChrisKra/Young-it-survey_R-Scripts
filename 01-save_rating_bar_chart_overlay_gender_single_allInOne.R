# usage: 
# 1. replace first and last column name 
# 2. replace output name for df

# Alpha function for colors
add.alpha <- function(col, alpha=1){
  if(missing(col))
    stop("Please provide a vector of colours.")
  apply(sapply(col, col2rgb)/255, 2, 
        function(x) 
          rgb(x[1], x[2], x[3], alpha=alpha))  
}

# REPLACE HERE!!!
s <- which(colnames(df) == "Autonomie bei der Arbeit") # First column
e <- which(colnames(df) == "Zusätzliche Leistungen") # Last column

# List of dataframes 
df_list <- vector("list", 5)

for(rank in 1:5) {
  # function to convert column to vector
  rcolumnToVector <- function(rcolumnM, rcolumnW) {
    tmp = c(0, 0)
    # Fill M Value 
    if(!is.na(rcolumnM[toString(rank)])) {
      tmp[1] = rcolumnM[toString(rank)]*(6-rank) # Weigh in rank
    }
    # Fill W Value view 
    if(!is.na(rcolumnW[toString(rank)])) {
      tmp[2] = rcolumnW[toString(rank)]*(6-rank) # Weigh in rank
    }
    return(tmp)
  }
  
  if(rank == 1) {
    # convert first column and create df
    tmp_df <- data.frame(rcolumnToVector(table(df %>% filter(df[["Geschlecht"]] == "Männlich") %>% select(s)), table(df %>% filter(df[["Geschlecht"]] == "Weiblich") %>% select(s)))) # create df with first column
    names(tmp_df)[1] <- colnames(df)[s] # rename column
    
    # append all other columns
    for(i in 2:(e-s+1)){
      tmp_df[i] = rcolumnToVector(table(df %>% filter(df[["Geschlecht"]] == "Männlich") %>% select(s+i-1)), table(df %>% filter(df[["Geschlecht"]] == "Weiblich") %>% select(s+i-1)))
      names(tmp_df)[i] <- colnames(df)[s+i-1]
    }
  } else {
    # append all other columns
    for(i in 1:(e-s+1)){
      tmp <- rcolumnToVector(table(df %>% filter(df[["Geschlecht"]] == "Männlich") %>% select(s+i-1)), table(df %>% filter(df[["Geschlecht"]] == "Weiblich") %>% select(s+i-1)))
      tmp_df[i] <- tmp_df[i] + tmp # Add to existing column
    }
  }
  
  # name rows 
  row.names(tmp_df) <- c("Männlich", "Weiblich")
}

# convert table to percent 
sums <- rowSums(tmp_df);
for(i in 1:2) {
  for(j in 1:ncol(tmp_df)) {
    tmp_df[i, j] = (tmp_df[i, j]/sums[i])
  }
}

# Export tables as CSV
write.table(round(tmp_df, 4), file = "CSV_exports/Prio_Single_Split.csv", row.names = FALSE, col.names = FALSE, sep=";")


# Add maximum row
# tmp_df <- rbind(rep(max(tmp_df),(e-s+1)) , rep(0,(e-s+1)) , tmp_df)

# generate graph

# transpose
tmp_df <- t(tmp_df)

# prepare for ggplot
tmp_df <- melt(tmp_df,"Row.names")
colnames(tmp_df)[2] <- "Geschlecht"
colorM = "#31aad9"
colorW = "#b83578"

png(file="Generated_Charts/Prio_Bar_Chart_single.png",width=1400, height=700)

par(oma=c(2,2,2,2))

print(
  ggplot(tmp_df, aes(x = Row.names, y = value)) +
    geom_bar(aes(fill = Geschlecht), stat = "identity", position = "dodge", alpha = 0.9) +
    labs(x = "Charakteristik", y = "Gewichtete Verteilung") +
    theme_minimal() +
    scale_fill_manual(values=c(colorM,colorW)) + 
    ggtitle("Prioritäten kombiniert") + 
    theme(plot.margin = margin(2, 2, 1, 2, "cm"),
          text = element_text(size=20),
          axis.text.x = element_text(angle = -20, hjust = .1), 
          axis.title.y = element_text(vjust = 4),
          plot.title = element_text(hjust = 0.5, vjust=4, face="bold"))
)

dev.off()

# Debugging
# view(tmp_df)
# rowSums(tmp_df)
# arbeitgeber_df <- tmp_df # REPLACE HERE!!!

rm(df_list, tmp_df, tmp, colorM, colorW, s, e, i, j, rank, sums)