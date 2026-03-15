## Code to identify differences in taxonomy used by my database and that in the
## PBDB.

rm(list = ls())
setwd("C:/Users/pnovack-gottshall/OneDrive - Benedictine University/Desktop/Databases/Maintenance & update R scripts")

## Run relevant code in SelectCols.R for IDBadHigherTaxa.R to obtain following
## output.

# Input file with following columns (Phylum, Subphylum, Superclass, Class,
# Subclass, Infraclass, Superorder, Order, Suborder, Infraorder, Section,
# Subsection, Superfamily, Family, Subfamily, Tribe), making sure headers are
# included
x <- read.csv(file = "HigherTaxa.csv", header = TRUE)
#x <- read.csv(file = "HigherTaxa_PBDB.csv", header = TRUE)
head(x)
attach(x)


## (1) Identify superfamilies with same root that differ only in suffix (-acea
##     vs. -oidea)
tax <- unique(Superfamily)
t.acea <- grep("acea$", tax, value = TRUE)
root.acea <- sub("acea$", "", t.acea)
t.oidea <- grep("oidea$", tax, value = TRUE)
root.oidea <- sub("oidea$", "", t.oidea)
same.root <- if (length(root.acea) > length(root.oidea))
  root.acea[root.acea %in% root.oidea] else
  root.oidea[root.oidea %in% root.acea]
same.root


## (2) Identify taxa given different ranks in my taxonomy and in the PBDB.

for (rank in 1:(ncol(x) - 1)) {
  for (subrank in (rank + 1):ncol(x)) {
    same <- which(unique(x[[subrank]]) %in% unique(x[[rank]]))
    if (length(same) > 0L) {
      cat("match in", colnames(x)[rank], "and", colnames(x)[subrank], ":\n")
      cat(unique(x[[subrank]])[same], "\n")
      cat("\n")
    }
  }
}
# Known exceptions (see FormatPBDB.R for additional details):
#    a. Ignore UNCERTAINS and blanks
#    b. Accept Hyolitha, Kimberellomorpha, Petalonamae, and Tentaculita as both 
#       phylum and class given taxonomic ambiguity.
#    c. Accept Holocephali as both class and subclass.
#    d. Accept Cladistia as both class and infraclass.
#    e. Accept Nektaspidida, Tuzoida, Tommotiida, and Parablastoidea as both
#       class and order.
#    f. Accept Chondrostei and Actinistia as both subclass and infraclass.
#    g. Accept Aglaspidida,  Paleoloricata, Homoctenida, and Tentaculitida as 
#       both subclass and order.
#    h. Accept Orthoceratoidea as both subclass and superfamily.
#    i. Accept Cladoselachimorpha as both infraclass and superorder.
#    j. Accept Cocculiniformia as both superorder and order.
#    k. Accept Palaeostomata as both superorder (of many bryozoan orders) and 
#       suborder (of cyclostomes)
#    l. Accept Stolonifera (for bryozoan and cnidarian), Phymosomatoida, and 
#       Orchocladina as both order and suborder.
#    m. Accept Pilosa as both order and infraorder.
#    n. Accept Cambridioidea as both order and superfamily.
#    o. Accept Lepadoidea and Cassidulina as both suborder and superfamily.


## (3) Identify taxa placed inconsistently within higher taxa.

# Ignore "UNCERTAIN" and "", which are inherently allowed to be polyphyletic.

# Subphylum
tax <- unique(Subphylum)
wh.ignore <- which(tax %in% c("UNCERTAIN", ""))
tax <- tax[-wh.ignore]
for(i in 1:length(tax)) {
  if (i == 1)
    cat("Subphyla:\n")
  wh <- which(Subphylum == tax[i])
  if (length(wh) == 1L)
    next
  higher <- unique(x[wh, 1])
  if (length(higher) == 1L)
    next
  cat(as.character(tax[i]), ": ", as.character(higher), "\n")
}

# Superclass
tax <- unique(Superclass)
wh.ignore <- which(tax %in% c("UNCERTAIN", ""))
tax <- tax[-wh.ignore]
for (i in 1:length(tax)) {
  if (i == 1)
    cat("Superclasses:\n")
  wh <- which(Superclass == tax[i])
  if (length(wh) == 1L)
    next
  higher <- unique(x[wh, 1:2])
  if (nrow(higher) == 1L)
    next
  cat(as.character(tax[i]), ": ", as.character(higher), "\n")
}

# Class
tax <- unique(Class)
wh.ignore <- which(tax %in% c("UNCERTAIN", ""))
tax <- tax[-wh.ignore]
for (i in 1:length(tax)) {
  if (i == 1)
    cat("Classes:\n")
  wh <- which(Class == tax[i])
  if (length(wh) == 1L)
    next
  higher <- unique(x[wh, 1:3])
  if (nrow(higher) == 1L)
    next
  cat(as.character(tax[i]), ": ", as.character(higher), "\n\n")
}
# Ignore class Tommotiida, which is known to be either paraphyletic or
#      polyphyletic (and generally considered as various stem brachiopods).

# Subclass
tax <- unique(Subclass)
wh.ignore <- which(tax %in% c("UNCERTAIN", ""))
tax <- tax[-wh.ignore]
for (i in 1:length(tax)) {
  if (i == 1)
    cat("Subclasses:\n")
  wh <- which(Subclass == tax[i])
  if (length(wh) == 1L)
    next
  higher <- unique(x[wh, 1:4])
  if (nrow(higher) == 1)
    next
  cat(as.character(tax[i]), ": ", as.character(higher), "\n\n")
}

# Infraclass
tax <- unique(Infraclass)
wh.ignore <- which(tax %in% c("UNCERTAIN", ""))
tax <- tax[-wh.ignore]
for (i in 1:length(tax)) {
  if (i == 1)
    cat("Infraclasses:\n")
  wh <- which(Infraclass == tax[i])
  if (length(wh) == 1L)
    next
  higher <- unique(x[wh, 1:5])
  if (nrow(higher) == 1)
    next
  cat(as.character(tax[i]), ": ", as.character(higher), "\n\n")
}

# Superorder
tax <- unique(Superorder)
wh.ignore <- which(tax %in% c("UNCERTAIN", ""))
tax <- tax[-wh.ignore]
for (i in 1:length(tax)) {
  if (i == 1)
    cat("Superorders:\n")
  wh <- which(Superorder == tax[i])
  if (length(wh) == 1L)
    next
  higher <- unique(x[wh, 1:6])
  if (nrow(higher) == 1)
    next
  cat(as.character(tax[i]), ": ", as.character(higher), "\n\n")
}

# Order
tax <- unique(Order)
wh.ignore <- which(tax %in% c("UNCERTAIN", ""))
tax <- tax[-wh.ignore]
for (i in 1:length(tax)) {
  if (i == 1)
    cat("Orders:\n")
  wh <- which(Order == tax[i])
  if (length(wh) == 1L)
    next
  higher <- unique(x[wh, 1:7])
  if (nrow(higher) == 1)
    next
  cat(as.character(tax[i]), ": ", as.character(higher), "\n\n")
}
# Ignore order Tommotiida, which is known to be either paraphyletic or
#      polyphyletic (and generally considered as various stem brachiopods).

# Suborder
tax <- unique(Suborder)
wh.ignore <- which(tax %in% c("UNCERTAIN", ""))
tax <- tax[-wh.ignore]
for (i in 1:length(tax)) {
  if (i == 1)
    cat("Suborders:\n")
  wh <- which(Suborder == tax[i])
  if (length(wh) == 1L)
    next
  higher <- unique(x[wh, 1:8])
  if (nrow(higher) == 1)
    next
  cat(as.character(tax[i]), ": ", as.character(higher), "\n\n")
}

# Infraorder
tax <- unique(Infraorder)
wh.ignore <- which(tax %in% c("UNCERTAIN", ""))
tax <- tax[-wh.ignore]
for (i in 1:length(tax)) {
  if (i == 1)
    cat("Infraorders:\n")
  wh <- which(Infraorder == tax[i])
  if (length(wh) == 1L)
    next
  higher <- unique(x[wh, 1:8])
  if (nrow(higher) == 1)
    next
  cat(as.character(tax[i]), ": ", as.character(higher), "\n\n")
}

# Section
tax <- unique(Section)
wh.ignore <- which(tax %in% c("UNCERTAIN", ""))
tax <- tax[-wh.ignore]
for (i in 1:length(tax)) {
  if (i == 1)
    cat("Sections:\n")
  wh <- which(Section == tax[i])
  if (length(wh) == 1L)
    next
  higher <- unique(x[wh, 1:8])
  if (nrow(higher) == 1)
    next
  cat(as.character(tax[i]), ": ", as.character(higher), "\n\n")
}


# Subsection
tax <- unique(Subsection)
wh.ignore <- which(tax %in% c("UNCERTAIN", ""))
tax <- tax[-wh.ignore]
for (i in 1:length(tax)) {
  if (i == 1)
    cat("Subsections:\n")
  wh <- which(Subsection == tax[i])
  if (length(wh) == 1L)
    next
  higher <- unique(x[wh, 1:8])
  if (nrow(higher) == 1)
    next
  cat(as.character(tax[i]), ": ", as.character(higher), "\n\n")
}

# Superfamily
tax <- unique(Superfamily)
wh.ignore <- which(tax %in% c("UNCERTAIN", ""))
tax <- tax[-wh.ignore]
for (i in 1:length(tax)) {
  if (i == 1)
    cat("Superfamilies:\n")
  wh <- which(Superfamily == tax[i])
  if (length(wh) == 1L)
    next
  higher <- unique(x[wh, 1:8])
  if (nrow(higher) == 1)
    next
  cat(as.character(tax[i]), ": ", as.character(higher), "\n\n")
}

# Family
tax <- unique(Family)
wh.ignore <- which(tax %in% c("UNCERTAIN", ""))
tax <- tax[-wh.ignore]
for (i in 1:length(tax)) {
  if (i == 1)
    cat("Families:\n")
  wh <- which(Family == tax[i])
  if (length(wh) == 1L)
    next
  higher <- unique(x[wh, 1:8])
  if (nrow(higher) == 1)
    next
  cat(as.character(tax[i]), ": ", as.character(higher), "\n\n")
}

# Subfamily
tax <- unique(Subfamily)
wh.ignore <- which(tax %in% c("UNCERTAIN", ""))
tax <- tax[-wh.ignore]
for (i in 1:length(tax)) {
  if (i == 1)
    cat("Subfamilies:\n")
  wh <- which(Subfamily == tax[i])
  if (length(wh) == 1L)
    next
  higher <- unique(x[wh, 1:8])
  if (nrow(higher) == 1)
    next
  cat(as.character(tax[i]), ": ", as.character(higher), "\n\n")
}

# Tribe
tax <- unique(Tribe)
wh.ignore <- which(tax %in% c("UNCERTAIN", ""))
tax <- tax[-wh.ignore]
for (i in 1:length(tax)) {
  if (i == 1)
    cat("Tribes:\n")
  wh <- which(Tribe == tax[i])
  if (length(wh) == 1L)
    next
  higher <- unique(x[wh, 1:8])
  if (nrow(higher) == 1)
    next
  cat(as.character(tax[i]), ": ", as.character(higher), "\n\n")
}
