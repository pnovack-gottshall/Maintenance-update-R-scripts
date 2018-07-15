## Identify taxa placed inconsistently within higher taxa.

rm(list = ls())
setwd("C:/Users/pnovack-gottshall/Desktop/Databases/Maintenance & update R scripts")

# Input file with following columns (Phylum, Subphylum, Class, Subclass, Order,
# Suborder, Superfamily, Family, Subfamily), making sure headers are included
x <- read.csv(file = "HigherTaxa.csv", header = TRUE)
head(x)
attach(x)

# Subfamilies
tax <- levels(Subfamily)
for(i in 1:length(tax)) {
  if(i==1) cat("Subfamilies:\n")
  wh <- which(Subfamily==tax[i])
  if(length(wh)==1L) next
  higher <- unique(x[wh, 1:6])
  if(nrow(higher)==1) next
  cat(as.character(tax[i]), "\n")
}
# Ignore "UNCERTAIN," which is inherently allowed to be polyphyletic

# Families
tax <- levels(Family)
for(i in 1:length(tax)) {
  if(i==1) cat("Families:\n")
  wh <- which(Family==tax[i])
  if(length(wh)==1L) next
  higher <- unique(x[wh, 1:6])
  if(nrow(higher)==1) next
  cat(as.character(tax[i]), "\n")
}
# Ignore "UNCERTAIN," which is inherently allowed to be polyphyletic

# Superfamilies
tax <- levels(Superfamily)
for(i in 1:length(tax)) {
  if(i==1) cat("Superfamilies:\n")
  wh <- which(Superfamily==tax[i])
  if(length(wh)==1L) next
  higher <- unique(x[wh, 1:5])
  if(nrow(higher)==1) next
  cat(as.character(tax[i]), "\n")
}

# Suborders
tax <- levels(Suborder)
for(i in 1:length(tax)) {
  if(i==1) cat("Suborders:\n")
  wh <- which(Suborder==tax[i])
  if(length(wh)==1L) next
  higher <- unique(x[wh, 1:4])
  if(nrow(higher)==1) next
  cat(as.character(tax[i]), "\n")
}

# Orders
tax <- levels(Order)
for(i in 1:length(tax)) {
  if(i==1) cat("Orders:\n")
  wh <- which(Order==tax[i])
  if(length(wh)==1L) next
  higher <- unique(x[wh, 1:3])
  if(nrow(higher)==1) next
  cat(as.character(tax[i]), "\n")
}

# Subclass
tax <- levels(Subclass)
for(i in 1:length(tax)) {
  if(i==1) cat("Subclasses:\n")
  wh <- which(Subclass==tax[i])
  if(length(wh)==1L) next
  higher <- unique(x[wh, 1:2])
  if(nrow(higher)==1) next
  cat(as.character(tax[i]), "\n")
}

# Class
tax <- levels(Class)
for(i in 1:length(tax)) {
  if(i==1) cat("Classes:\n")
  wh <- which(Class==tax[i])
  if(length(wh)==1L) next
  higher <- unique(x[wh, 1])
  if(length(higher)==1L) next
  cat(as.character(tax[i]), "\n")
}

# Subphylum
tax <- levels(Subphylum)
for(i in 1:length(tax)) {
  if(i==1) cat("Subphyla:\n")
  wh <- which(Subphylum==tax[i])
  if(length(wh)==1L) next
  higher <- unique(x[wh, 1])
  if(length(higher)==1L) next
  cat(as.character(tax[i]), "\n")
}

