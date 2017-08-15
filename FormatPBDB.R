## Format PBDB genus/subgenus occurrences into taxonomic structure of my data

# NOTE: THIS IS VERY SLOW! ~4.5 hours to process PBDB. Consider re-doing for 
# parallel (requires re-writing as a 'lapply' function). Until then: do Steve
# Holland's poor-man HPC and divide the loop into 4 and run in 4 R windows.

## Download data directly from PaleobioDB and save to working directory (will be
## > 40 MB) (DO NOT RESTRICT TO PHANEROZOIC! Restricting only includes taxa with
## fossil occurrences. Here we want ALL taxa!)

# setwd("C:/Users/pnovack-gottshall/Desktop/Databases/Maintenance & update R scripts")

rm(list=ls())
## Easier if paste link into URL and save manually 
# pbdb <- read.csv("www.paleobiodb.org/data1.2/taxa/list.csv?base_name=Metazoa&show=app&vocab=pbdb")
# If want forams too, use base_name=Metazoa,Retaria
pbdb <- read.csv("pbdb_data.csv")
head(pbdb)

# Use genera and subgenera
which.gsg <- which(pbdb$accepted_rank=="genus" | pbdb$accepted_rank=="subgenus")
n.gen <- length(unique(pbdb$accepted_name[which.gsg]))

# Prep outfile file (keep early and late ranges!)
x <- data.frame(Phylum=character(n.gen), Subphylum=character(n.gen), 
  Class=character(n.gen), Subclass=character(n.gen), Order=character(n.gen), 
  Suborder=character(n.gen), Superfamily=character(n.gen), Family=character(n.gen), 
  Subfamily=character(n.gen), Genus=sort(unique(pbdb$accepted_name[which.gsg])), 
  Subgenus=character(n.gen), Species=rep("sp.", n.gen), early_age=numeric(n.gen),
  late_age=numeric(n.gen), stringsAsFactors=FALSE)
# Make sure order of next matches column names in output file 'x'
scales <- c("phylum", "subphylum", "class", "subclass", "order", "suborder", 
  "superfamily", "family", "subfamily", "genus", "subgenus")
index <- c(seq(0, 400, by=100), seq(500, n.gen, by=500))
(start.t <- Sys.time())
# for(g in 1:17750) {
# for(g in 17751:35500) {
# for(g in 35501:53250) {
# for(g in 53251:71000) {
for(g in 1:n.gen) {
  if(g %in% index) cat(g, "out of", n.gen, "\n")
  wh <- which(pbdb$accepted_name == x$Genus[g] & (pbdb$taxon_rank == "genus" | 
      pbdb$taxon_rank == "subgenus"))[1]
  x$early_age[g] <- pbdb$firstapp_max_ma[wh]
  x$late_age[g] <- pbdb$lastapp_min_ma[wh]
  if(pbdb$accepted_rank[wh] == "subgenus") x$Subgenus[g] <- as.character(x$Genus[g])
  parent <- pbdb[which(pbdb$accepted_no == pbdb$parent_no[wh]), ][1, ]
  repeat {
    if(parent$accepted_rank %in% scales) 
      x[g, which(scales == parent$accepted_rank)] <- as.character(parent$accepted_name)
    parent <- pbdb[which(pbdb$accepted_no == parent$parent_no), ][1, ]
    if(all(is.na(parent))) break
  }
}
(Sys.time() - start.t)    

# write.csv(x, file="PBDBformatted4.csv", row.names=FALSE); alarm()
# x1 <- read.csv(file="PBDBformatted1.csv", header=TRUE, stringsAsFactors=FALSE)
# x2 <- read.csv(file="PBDBformatted2.csv", header=TRUE, stringsAsFactors=FALSE)
# x3 <- read.csv(file="PBDBformatted3.csv", header=TRUE, stringsAsFactors=FALSE)
# x4 <- read.csv(file="PBDBformatted4.csv", header=TRUE, stringsAsFactors=FALSE)
# x <- rbind(x1[1:17750, ], x2[17751:35500, ], x3[35501:53250, ], x4[53251:n.gen, ])


## Remove terrestrial and non-marine taxa, but include marine tetrapods (list
## courtesy of Bush and Bambach 2015)
non.marine <- c("Arachnida", "Insecta", "Collembola", "Palaeophreatoicidae", 
  "Limnocytheridae", "Darwinuloidea", "Cypridoidea", "Cytherideidae", 
  "Assimineidae", "Stenothyridae", "Hydrobiidae", "Ampullariidae", "Cyclophoridae", 
  "Diplommatinidae", "Maizaniidae", "Viviparidae", "Melanopsidae", "Pachychilidae", 
  "Thiaridae", "Amnicolidae", "Bithyniidae", "Lithoglyphidae", "Pomatiopsidae", 
  "Valvatidae", "Physidae", "Ancylidae", "Lymnaeidae", "Endodontidae", "Melampidae",
  "Oreohelicidae", "Pleurodontidae", "Pupillidae", "Sagdidae", "Limacidae", "Vitrinidae",
  "Milacidae", "Strobilopsidae", "Valloniidae", "Subulinidae", "Succineidae", "Zonitidae",
  "Planorbidae", "Plicatusidae", "Pragoserpulinidae", "Unionida", "Corbiculidae", 
  "Sphaeriidae", "Dreissenidae", "Amiinae", "Sinamiidae", "Cypriniformes", 
  "Lepisosteiformes", "Osmeriformes", "Osteoglossiformes", "Percidae", "Esocidae", 
  "Siluriformes", "Lepidosirenidae", "Polypteridae", "Cichlidae", "Gonorynchiformes",
  "Characiformes", "Gymnotiformes")
tetrapods <- c("Mammalia", "Reptilia")
marine.exceptions <- c("Chelonioidea", "Ophidiomorpha", "Mosasauroidea", "Thalattosauria", 
  "Sauropterygia", "Ichthyopterygia", "Mesoeucrocodylia", "Pterosauria", 
  "Hesperornithiformes", "Ichthyornithiformes", "Sphenisciformes", "Procellariiformes", 
  "Pelecaniformes", "Pelagornithidae", "Plotopteridae", "Charadriiformes", "Cetacea",
  "Sirenia", "Pinnipedia", "Desmostylia", "Ariidae", "Plotosidae")

write.csv(x, file="PBDBformatted2.csv", row.names=FALSE)
# x <- read.csv(file="PBDBformatted.csv", header=TRUE, stringsAsFactors=FALSE)

sq <- 1:nrow(x)
marine.vert.exceptions <- x[sapply(sq, function(sq) any(marine.exceptions %in% x[sq, ])), ]
# Remove the exceptions here, and add back in (in case of non-tetrapod duplicates)
marine.typical <- x[!sapply(sq, function(sq) any(c(non.marine, tetrapods, 
  marine.exceptions) %in% x[sq, ])), ]
marine.taxa <- rbind(marine.typical, marine.vert.exceptions)
table(marine.taxa$Class)
nrow(x)
nrow(marine.taxa)


# read mine (size or LH) and add PBDBformatted (BELOW mine!)
index matches
keep mine, remove pbdb matches, and merge
save

## Identify taxa placed inconsistently within higher taxa.
# Run IDBadHigherTaxa.R

# Consider extracting out non-marines and genus duplicates (multiple species for same genus) and non-fossilized at end instead?
