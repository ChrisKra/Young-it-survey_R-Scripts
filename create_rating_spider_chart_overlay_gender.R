# usage: 
# 1. replace first and last column name 
# 2. replace output name for df

# FILTER
rank = 5

# function to convert column to vector
rcolumnToVector <- function(rcolumnM, rcolumnW) {
  tmp = c(0, 0)
  # Fill M Value 
  if(!is.na(rcolumnM[toString(rank)])) {
    tmp[1] = rcolumnM[toString(rank)]
  }
  # Fill W Value 
  if(!is.na(rcolumnW[toString(rank)])) {
    tmp[2] = rcolumnW[toString(rank)]
  }
  return(tmp)
}

# REPLACE HERE!!!
s <- which(colnames(df) == "Autonomie bei der Arbeit") # First column
e <- which(colnames(df) == "Zusätzliche Leistungen") # Last column

# convert first column and create df
tmp_df <- data.frame(rcolumnToVector(table(df %>% filter(df[["Geschlecht"]] == "Männlich") %>% select(s)), table(df %>% filter(df[["Geschlecht"]] == "Weiblich") %>% select(s)))) # create df with first column
names(tmp_df)[1] <- colnames(df)[s] # rename column

# append all other columns
for(i in 2:(e-s+1)){
  tmp_df[i] = rcolumnToVector(table(df %>% filter(df[["Geschlecht"]] == "Männlich") %>% select(s+i-1)), table(df %>% filter(df[["Geschlecht"]] == "Weiblich") %>% select(s+i-1)))
  names(tmp_df)[i] <- colnames(df)[s+i-1]
}

# name rows 
row.names(tmp_df) <- c("Männlich", "Weiblich")

# convert table to percent 
sums <- rowSums(tmp_df);
for(i in 1:2) {
  for(j in 1:ncol(tmp_df)) {
    tmp_df[i, j] = (tmp_df[i, j]/sums[i])
  }
}

# Alpha function for colors
add.alpha <- function(col, alpha=1){
  if(missing(col))
    stop("Please provide a vector of colours.")
  apply(sapply(col, col2rgb)/255, 2, 
        function(x) 
          rgb(x[1], x[2], x[3], alpha=alpha))  
}

# find maximum, add 2 rows to the dataframe: the max and min of each topic 
tmp_df <- rbind(rep(max(tmp_df),(e-s+1)) , rep(0,(e-s+1)) , tmp_df)

# generate graph
radarchart(
  tmp_df,
  title=paste("Priorität ", rank),
  pcol = c(col="#31aad9", col="#b83578"),
  pfcol = add.alpha(c(col="#31aad9", col="#b83578"), 0.8),
  plwd = 3,
  plty=1,
  cglcol = "grey",
  cglty = 1,
  axislabcol = "grey",
  caxislabels = seq(0, 20, 5),
  cglwd = 0.8,
  vlcex = 0.8
)
legend(
  x = 2,
  y = 1.3,
  legend = rownames(tmp_df[-c(1, 2), ]),
  bty = "n",
  pch = 20 ,
  col = c(col="#31aad9", col="#b83578"),
  text.col = "grey",
  cex = 1.2,
  pt.cex = 3
)

# Debugging
# view(tmp_df)
rowSums(tmp_df)
# arbeitgeber_df <- tmp_df # REPLACE HERE!!!

rm(tmp_df, s, e, i, j, rank, sums)