# usage: 
# 1. replace first and last column name 
# 2. replace output name for df

# FILTER: One of Männlich, Weiblich, all
sex = "Weiblich"

# function to convert column to vector
rcolumnToVector <- function(rcolumn) {
  tmp = c(0,0,0,0,0)
  for(j in 1:5) {
    if(!is.na(rcolumn[toString(j)])) {
      tmp[j] = rcolumn[toString(j)]
    }
  }
  return(tmp)
}

# REPLACE HERE!!!
s <- which(colnames(df) == "Autonomie bei der Arbeit") # First column
e <- which(colnames(df) == "Zusätzliche Leistungen") # Last column

# convert first column and create df
if(sex != "all") {
  tmp_df <- data.frame(rcolumnToVector(table(df %>% filter(df[["Geschlecht"]] == sex) %>% select(s)))) # create df with first column
} else {
  tmp_df <- data.frame(rcolumnToVector(table(df %>% select(s)))) # create df with first column
}
names(tmp_df)[1] <- colnames(df)[s] # rename column

# append all other columns
for(i in 2:(e-s+1)){
  if(sex != "all") {
    tmp_df[i] = rcolumnToVector(table(df %>% filter(df[["Geschlecht"]] == sex) %>% select(s+i-1)))
  } else {
    tmp_df[i] = rcolumnToVector(table(df %>% select(s+i-1)))
  }
  names(tmp_df)[i] <- colnames(df)[s+i-1]
}

# convert table to percent 
sums <- rowSums(tmp_df);
for(i in 1:5) {
  for(j in 1:ncol(tmp_df)) {
    tmp_df[i, j] = (tmp_df[i, j]/sums[i])
  }
}

# find maximum, add 2 lines to the dataframe: the max and min of each topic 
tmp_df <- rbind(rep(max(tmp_df),(e-s+1)) , rep(0,(e-s+1)) , tmp_df)

colScheme <- c("#08212b", "#186481", "#28a6d7", "#7ecae7", "#d4edf7")
if (sex == "Weiblich") {
  colScheme <- c("#280b1a", "#77224d", "#c63981", "#dd88b3", "#f4d7e6")
} else if (sex == "all") {
  colScheme <-rev(brewer.pal(n = 5, name = 'Greys'))
}

# generate graph
radarchart(
  tmp_df,
  title=paste("Rating:", sex),
  pcol = colScheme,
  plwd = 3,
  cglcol = "grey",
  cglty = 1,
  axislabcol = "grey",
  caxislabels = seq(0, 20, 5),
  cglwd = 0.8,
  vlcex = 0.8
)
legend(
  x = 1.6,
  y = 1.3,
  legend = paste("Priorität ", (as.numeric(rownames(tmp_df[-c(1, 2), ])) - 2)),
  bty = "n",
  pch = 20 ,
  col = colScheme,
  text.col = "grey",
  cex = 1.2,
  pt.cex = 3
)

# Debugging
# view(tmp_df)
# rowSums(tmp_df)
# arbeitgeber_df <- tmp_df # REPLACE HERE!!!

rm(tmp_df, s, e, i, j, sums, sex)