# usage: 
# 1. replace first and last column name 
# 2. replace output name for df

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
tmp_df <- data.frame(rcolumnToVector(table(df %>% select(s)))) # create df with first column
names(tmp_df)[1] <- colnames(df)[s] # rename column

# append all other columns
for(i in 2:(e-s+1)){
  tmp_df[i] = rcolumnToVector(table(df %>% select(s+i-1)))
  names(tmp_df)[i] <- colnames(df)[s+i-1]
}

# find maximum, add 2 lines to the dataframe: the max and min of each topic 
tmp_df <- rbind(rep(max(tmp_df),(e-s+1)) , rep(0,(e-s+1)) , tmp_df)

# generate graph
radarchart(
  tmp_df,
  pcol = brewer.pal(n = 5, name = 'Spectral'),
  cglcol = "grey",
  cglty = 1,
  axislabcol = "grey",
  caxislabels = seq(0, 20, 5),
  cglwd = 0.8,
  vlcex = 0.8
)
legend(
  x = 2,
  y = 1.4,
  legend = (as.numeric(rownames(tmp_df[-c(1, 2), ])) - 2),
  bty = "n",
  pch = 20 ,
  col = brewer.pal(n = 5, name = 'Spectral'),
  text.col = "grey",
  cex = 1.2,
  pt.cex = 3
)


# arbeitgeber_df <- tmp_df # REPLACE HERE!!!

rm(tmp_df, s, e, i)