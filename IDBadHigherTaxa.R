## Code to identify differences in taxonomy used by my database and that in the
## PBDB.

rm(list = ls())
setwd("C:/Users/pnovack-gottshall/Desktop/Databases/Maintenance & update R scripts")

## Run relevant code in SelectCols.R for IDBadHigherTaxa.R to obtain following
## output.

# Input file with following columns (Phylum, Subphylum, Class, Subclass, Order,
# Suborder, Superfamily, Family, Subfamily), making sure headers are included
# x <- read.csv(file = "HigherTaxa.csv", header = TRUE)
x <- read.csv(file = "HigherTaxa_PBDB.csv", header = TRUE)
head(x)
attach(x)


## (1) Identify superfamilies with same root that differ only in suffix (-acea
##     vs. -oidea)
tax <- levels(Superfamily)
t.acea <- grep("acea$", tax, value = TRUE)
root.acea <- sub("acea$", "", t.acea)
t.oidea <- grep("oidea$", tax, value = TRUE)
root.oidea <- sub("oidea$", "", t.oidea)
same.root <- if (length(root.acea) > length(root.oidea))
  root.acea[root.acea %in% root.oidea] else
  root.oidea[root.oidea %in% root.acea]
same.root


## (2) Identify taxa given different ranks in my taxonomy and in the PBDB.
# Known exceptions (see FormatPBDB_Cluster.R for additional details):
#    a. Ignore UNCERTAINS.
#    b. Accept Tommotiida as both class and order given taxonomic
#       ambiguity.
#    c. Change order Cephalodiscida to Cephalodiscoidea (in class 
#       Cephalodiscida), per WoRMS.
#    d. Bdelloidea is valid as both a rotifer class and a xiphosuran 
#       superfamily.
#    d. Articulata is valid as both a crinoid subclass and a bryozoan suborder.
#    e. Nautiloidea and Orthoceratoidea are both valid as both a cephalopod 
#       subclass and superfamily.
#    f. Stolonifera is valid as both a bryozoan suborder and a cnidarian order.

for (rank in 1:8) {
  for (subrank in (rank + 1):9) {
    same <- which(levels(x[[subrank]]) %in% levels(x[[rank]]))
    if(length(same) > 0L) {
      cat("match in", colnames(x)[rank], "and", colnames(x)[subrank], ":\n")
      cat(levels(x[[subrank]])[same], "\n")
      cat("\n")
    }
  }
}


## (3) Identify taxa placed inconsistently within higher taxa.
# Ignore "UNCERTAIN," which is inherently allowed to be polyphyletic.
# Ignore Class Tommotiida, which is known to be either paraphyletic or
#      polyphyletic.

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

# Class
tax <- levels(Class)
for(i in 1:length(tax)) {
  if(i==1) cat("Classes:\n")
  wh <- which(Class==tax[i])
  if(length(wh)==1L) next
  higher <- unique(x[wh, 2])
  if(length(higher)==1L) next
  cat(as.character(tax[i]), "\n")
}

# Subclass
tax <- levels(Subclass)
for(i in 1:length(tax)) {
  if(i==1) cat("Subclasses:\n")
  wh <- which(Subclass==tax[i])
  if(length(wh)==1L) next
  higher <- unique(x[wh, 1:3])
  if(nrow(higher)==1) next
  cat(as.character(tax[i]), "\n")
}

# Orders
tax <- levels(Order)
for(i in 1:length(tax)) {
  if(i==1) cat("Orders:\n")
  wh <- which(Order==tax[i])
  if(length(wh)==1L) next
  higher <- unique(x[wh, 1:4])
  if(nrow(higher)==1) next
  cat(as.character(tax[i]), "\n")
}

# Below orders, more efficient to work upwards from subfamily up ranks (because
# can correct multiple ranks at same time).

# Subfamilies
tax <- levels(Subfamily)
for(i in 1:length(tax)) {
  if(i==1) cat("Subfamilies:\n")
  wh <- which(Subfamily==tax[i])
  if(length(wh)==1L) next
  higher <- unique(x[wh, 1:8])
  if(nrow(higher)==1) next
  cat(as.character(tax[i]), "\n")
}

# Families
tax <- levels(Family)
for(i in 1:length(tax)) {
  if(i==1) cat("Families:\n")
  wh <- which(Family==tax[i])
  if(length(wh)==1L) next
  higher <- unique(x[wh, 1:7])
  if(nrow(higher)==1) next
  cat(as.character(tax[i]), "\n")
}

# Superfamilies
tax <- levels(Superfamily)
for(i in 1:length(tax)) {
  if(i==1) cat("Superfamilies:\n")
  wh <- which(Superfamily==tax[i])
  if(length(wh)==1L) next
  higher <- unique(x[wh, 1:6])
  if(nrow(higher)==1) next
  cat(as.character(tax[i]), "\n")
}

# Suborders
tax <- levels(Suborder)
for(i in 1:length(tax)) {
  if(i==1) cat("Suborders:\n")
  wh <- which(Suborder==tax[i])
  if(length(wh)==1L) next
  higher <- unique(x[wh, 1:5])
  if(nrow(higher)==1) next
  cat(as.character(tax[i]), "\n")
}
