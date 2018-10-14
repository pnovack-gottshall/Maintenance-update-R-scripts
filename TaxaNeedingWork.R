## Check which classes need more data collection

## IMPORT DATA ------------------------------------------------------------- 

# (1) Export following data fields as 'ScaleCodings.tab' tab-delimited format.
# Open and add headers (easiest to do saving crap file in Excel format). DO NOT 
# SAVE AS EXCEL FORMAT, AS DOING SO IDIOSYNCRATICALLY CHANGES NAs TO 0s AND 1s!!

# Taxonomy: Phylum, Class
# Proxy fields: EcologyScale, BodySizeScale

rm(list=ls())
setwd("C:/Users/pnovack-gottshall/Desktop/Databases/Maintenance & update R scripts")
input <- read.delim(file="ScaleCodings.tab", colClasses="character")
scales <- c("Species", "Subgenus", "Genus", "Subfamily", "Family", "Superfamily", 
  "Suborder", "Order", "Subclass", "Class", "Subphylum", "Phylum", "", NA)
scales <- factor(scales, levels=scales, ordered=TRUE)
input$EcologyScale <- factor(input$EcologyScale, levels=scales, ordered=TRUE)
input$BodySizeScale <- factor(input$BodySizeScale, levels=scales, ordered=TRUE)
head(input)

# Phylum level
ph <- sort(unique(input$Phylum))
ph.n <- table(input$Phylum)
ph.LH.percent <- data.frame(n=ph.n, percent=NA, done=NA, remaining=NA)
ph.size.percent <- data.frame(n=ph.n, percent=NA, done=NA, remaining=NA)

for(i in 1:length(ph)){
  wh.ph <- which(input$Phylum==ph[i]) # which phylum?
  this.ph <- input[wh.ph, ]
  n.LH.good <- length(which(this.ph$EcologyScale == "Species" | 
                              this.ph$EcologyScale == "Genus"))
  n.size.good <- length(which(this.ph$BodySizeScale == "Species" | 
                                this.ph$BodySizeScale == "Genus"))
  ph.LH.percent$done[i] <- n.LH.good
  ph.size.percent$done[i] <- n.size.good
  ph.LH.percent$remaining[i] <- length(which(this.ph$EcologyScale != "Species" & 
                                               this.ph$EcologyScale != "Genus"))
  ph.size.percent$remaining[i] <- length(which(this.ph$BodySizeScale != "Species" & 
                                                 this.ph$BodySizeScale != "Genus"))
  ph.LH.percent$percent[i] <- round(n.LH.good / nrow(this.ph), 2)
  ph.size.percent$percent[i] <- round(n.size.good / nrow(this.ph), 2)
}

cat("Life habits:\n"); ph.LH.percent[order(ph.LH.percent$percent, ph.LH.percent$n.Freq, ph.LH.percent$n.Var1), ]
cat("Body size:\n"); ph.size.percent[order(ph.size.percent$percent, ph.size.percent$n.Freq, ph.size.percent$n.Var1), ]


# Class level:
cl <- sort(unique(input$Class))
cl.n <- table(input$Class)
cl.LH.percent <- data.frame(n=cl.n, percent=NA, done=NA, remaining=NA)
cl.size.percent <- data.frame(n=cl.n, percent=NA, done=NA, remaining=NA)

for(i in 1:length(cl)){
  wh.cl <- which(input$Class==cl[i]) # which class?
  this.cl <- input[wh.cl, ]
  n.LH.good <- length(which(this.cl$EcologyScale == "Species" | 
                           this.cl$EcologyScale == "Genus"))
  n.size.good <- length(which(this.cl$BodySizeScale == "Species" | 
                              this.cl$BodySizeScale == "Genus"))
  cl.LH.percent$done[i] <- n.LH.good
  cl.size.percent$done[i] <- n.size.good
  cl.LH.percent$remaining[i] <- length(which(this.cl$EcologyScale != "Species" & 
                                            this.cl$EcologyScale != "Genus"))
  cl.size.percent$remaining[i] <- length(which(this.cl$BodySizeScale != "Species" & 
                                               this.cl$BodySizeScale != "Genus"))
  cl.LH.percent$percent[i] <- round(n.LH.good / nrow(this.cl), 2)
  cl.size.percent$percent[i] <- round(n.size.good / nrow(this.cl), 2)
}

cat("Life habits:\n"); cl.LH.percent[order(cl.LH.percent$percent, cl.LH.percent$n.Freq, cl.LH.percent$n.Var1), ]
cat("Body size:\n"); cl.size.percent[order(cl.size.percent$percent, cl.size.percent$n.Freq, cl.size.percent$n.Var1), ]

