## Check which classes need more data collection

## IMPORT DATA ------------------------------------------------------------- 

# (1) Export following data fields as 'ClassLHCodings.tab' tab-delimited format.
# Open and add headers (easiest to do saving crap file in Excel format). DO NOT 
# SAVE AS EXCEL FORMAT, AS DOING SO IDIOSYNCRATICALLY CHANGES NAs TO 0s AND 1s!!

# Taxonomy: Class
# Proxy fields: EcologyScale

rm(list=ls())
setwd("C:/Users/pnovack-gottshall/Desktop/Databases/Maintenance & update R scripts")
input <- read.delim(file="ClassLHCodings.tab", colClasses="character")
scales <- c("Species", "Subgenus", "Genus", "Subfamily", "Family", "Superfamily", 
  "Suborder", "Order", "Subclass", "Class", "Subphylum", "Phylum", "", NA)
scales <- factor(scales, levels=scales, ordered=TRUE)
input$EcologyScale <- factor(input$EcologyScale, levels=scales, ordered=TRUE)
head(input)

cl <- sort(unique(input$Class))
cl.n <- table(input$Class)
cl.percent <- data.frame(n=cl.n, percent=NA, done=NA, remaining=NA)

for(i in 1:length(cl)){
  wh.cl <- which(input$Class==cl[i]) # which class?
  this.cl <- input[wh.cl, ]
  n.good <- length(which(this.cl$EcologyScale == "Species" | 
      this.cl$EcologyScale == "Genus"))
  cl.percent$done[i] <- n.good
  cl.percent$remaining[i] <- length(which(this.cl$EcologyScale != "Species" & 
      this.cl$EcologyScale != "Genus"))
  cl.percent$percent[i] <- round(n.good / nrow(this.cl), 2)
}

cl.percent[order(cl.percent$percent), ]
