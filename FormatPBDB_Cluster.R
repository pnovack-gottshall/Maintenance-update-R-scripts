## Format PBDB genus/subgenus occurrences into taxonomic structure of my data,
## using a parallel computing environment

# The serial version takes ~4.5 hours to process PBDB into a formatted taxonomic
# dataframe structure. This code rewrites the algorithm as a function that can
# be implemented "in parallel."

## Download data directly from PaleobioDB and save to working directory (will be
## > 40 MB) (DO NOT RESTRICT TO PHANEROZOIC! Restricting only includes taxa with
## fossil occurrences. Here we want ALL taxa!)

setwd("C:/Users/pnovack-gottshall/Desktop/Databases/Maintenance & update R scripts")

rm(list=ls())
## Easier if paste link into URL and save manually 
# pbdb <- read.csv("www.paleobiodb.org/data1.2/taxa/list.csv?base_name=Metazoa&show=app&vocab=pbdb")
# If want forams too, use base_name=Metazoa,Retaria
pbdb <- read.csv("pbdb_data.csv")
head(pbdb)


## Function to add higher taxonomic names (phylum, class, order, etc.) for PBDB
## genus (and subgenus) names
# gen.seq = Vector (sequence) of number of genus names to process
# gen.names = Vector of PBDB genus (and subgenus) names
# pbdb = data frame of all PBDB occurrences
#
# output is a list, with each item the taxonomy for a single genus
prep.PBDB <- function(g = 1, gen.names, pbdb) {
  scales <- c("phylum", "subphylum", "class", "subclass", "order", "suborder", 
              "superfamily", "family", "subfamily", "genus", "subgenus")
  out <- data.frame(Phylum=character(1), Subphylum=character(1), 
                  Class=character(1), Subclass=character(1), 
                  Order=character(1), Suborder=character(1),
                  Superfamily=character(1), Family=character(1), 
                  Subfamily=character(1), Genus=character(1), 
                  Subgenus=character(1), Species="sp.", 
                  stringsAsFactors=FALSE)
  out$Genus <- as.character(gen.names[g])
  wh <- which(pbdb$accepted_name == out$Genus & (pbdb$taxon_rank == "genus" | 
                                                   pbdb$taxon_rank == "subgenus"))[1]
  out$early_age <- as.numeric(pbdb$firstapp_max_ma[wh])
  out$late_age <- as.numeric(pbdb$lastapp_min_ma[wh])
  if (pbdb$accepted_rank[wh] == "subgenus")
    out$Subgenus <- as.character(out$Genus)
  parent <- pbdb[which(pbdb$accepted_no == pbdb$parent_no[wh]), ][1, ]
  repeat {
    if (parent$accepted_rank %in% scales)
      out[1, which(scales == parent$accepted_rank)] <-
        as.character(parent$accepted_name)
    parent <-
      pbdb[which(pbdb$accepted_no == parent$parent_no),][1,]
    if (all(is.na(parent)))
      break
  }
  return(out)
}

## Function to unpack the list into a compact data frame
# Note that with a large PBDB object that this still takes some time to
# complete. Consider tweaking to speed up.
unpack.PBDB <- function(prep) {
  Seq <- seq(length(prep))
  out <- prep[[1]]
  for (i in 2:length(prep)) {
    out[i, ] <- prep[[i]]
  }
  return(out)
}



# Serial version (running through 1000 genera)
(start.t <- Sys.time())
# Use genera and subgenera
which.gsg <- which(pbdb$accepted_rank=="genus" | pbdb$accepted_rank=="subgenus")
n.gen <- length(unique(pbdb$accepted_name[which.gsg]))
x <- data.frame(Phylum=character(n.gen), Subphylum=character(n.gen), 
                Class=character(n.gen), Subclass=character(n.gen), Order=character(n.gen), 
                Suborder=character(n.gen), Superfamily=character(n.gen), Family=character(n.gen), 
                Subfamily=character(n.gen), Genus=sort(unique(pbdb$accepted_name[which.gsg])), 
                Subgenus=character(n.gen), Species=rep("sp.", n.gen), early_age=numeric(n.gen),
                late_age=numeric(n.gen), stringsAsFactors=FALSE)
scales <- c("phylum", "subphylum", "class", "subclass", "order", "suborder", 
            "superfamily", "family", "subfamily", "genus", "subgenus")
for(g in 1:1000) {
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

# Serial version takes 232.3 seconds for 1000 (or 4.65 hours for all)



# Re-do using lapply (only 1 CPU)
(start.t <- Sys.time())
which.gsg <- which(pbdb$accepted_rank=="genus" | pbdb$accepted_rank=="subgenus")
gen.names <- sort(unique(pbdb$accepted_name[which.gsg]))
# gen.seq <- seq_along(gen.names)
gen.seq <- 1:1000
prep <- lapply(X = gen.seq, FUN = prep.PBDB, gen.names = gen.names, pbdb = pbdb)
output <- unpack.PBDB(prep)
(Sys.time() - start.t)    
# Serial lapply version takes 41.2 seconds for 1000 (or 49.4 mins for all)


# Now compare using parallel computing:
library(snowfall)
(t.start0 <- Sys.time())
# Initialize
which.gsg <- which(pbdb$accepted_rank=="genus" | pbdb$accepted_rank=="subgenus")
gen.names <- sort(unique(pbdb$accepted_name[which.gsg]))
gen.seq <- seq_along(gen.names)
# gen.seq <- 1:1000
# Set up computer cluster
cpus <- 8					# Number of CPUs to cluster together
# sfSetMaxCPUs(cpus)			# Use if plan more than 32 CPUs
sfInit(parallel=T, cpus=cpus, slaveOutfile="initfile") # Initialize cluster
stopifnot(sfCpus()==cpus)		# Confirm set up CPUs properly
stopifnot(sfParallel()==TRUE)		# Confirm now running in parallel
sfExportAll()				# Export all libraries, files, & objects
# Execute the function
(t.start1 <- Sys.time())
prep <- NA
prep <- sfLapply(x = gen.seq, fun = prep.PBDB, gen.names = gen.names, pbdb = pbdb) # Version without load-balancing
# prep <- sfClusterApplyLB(x = gen.seq, fun = prep.PBDB, gen.names = gen.names, pbdb = pbdb) 	# Version using load-balancing
sfStop()
(Sys.time() - t.start1)
output2 <- unpack.PBDB(prep)
(Sys.time() - t.start0)
(Sys.time() - t.start1)
alarm()
write.csv(output2, file="PBDBformatted.csv", row.names=FALSE)


# Parallel lapply version takes 60.2 secs for 1000 (21 s for the sfLapply) (or 27.8 min for all)
# Parallel lapply version (w/o export time) takes 19.7 secs for 1000 (or 23.7 mins for all)
# Parallel lapplyLB version takes 642 secs for 1000 (or 12.8 hrs for all)
# Parallel lapplyLB version (w/o export time) takes 602 secs for 1000 (or 12.0 hrs for all)


# Compare output
rbind(x[1000, ], output[1000, ], output2[1000, ])



# x <- read.csv(file="PBDBformatted.csv", header=TRUE, stringsAsFactors=FALSE)
head(x)

# Remove terrestrial and non-marine taxa, but include marine tetrapods (list
# courtesy of Bush and Bambach, 2015, but modified to explicitly list three
# cetacean suborders because Cetacea listed within Order Artiodactyla in PBDB)
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
                       "Sirenia", "Pinnipedia", "Desmostylia", "Ariidae", "Plotosidae", "Archaeoceti", 
                       "Mysticeti", "Odontoceti")

sq <- 1:nrow(x)
marine.vert.exceptions <- x[sapply(sq, function(sq) any(marine.exceptions %in% x[sq, ])), ]
# Remove the exceptions here, and add back in (in case of non-tetrapod duplicates)
marine.typical <- x[!sapply(sq, function(sq) any(c(non.marine, tetrapods, 
                                                   marine.exceptions) %in% x[sq, ])), ]
marine.taxa <- rbind(marine.typical, marine.vert.exceptions)
table(marine.taxa$Class)
nrow(x)
nrow(marine.taxa)

write.csv(marine.taxa, file="PBDBformatted_NoTerr.csv", row.names=FALSE)
# x <- read.csv(file="PBDBformatted_NoTerr.csv", header=TRUE, stringsAsFactors=FALSE)


## Run following to manually combine my database and the PBDB databases

# (1) In Excel, open the "postSizes.tab" or "postLH.tab" file and re-save as 
# "PreSizes_withPBDB.tab" or "PreLH_withPBDB.tab" (MANUALLY ADDING THE ".TAB" TO
# FILE NAME TO FORCE AS TAB-DELIMITED INSTEAD OF TEXT FILE FORMAT. Open 
# "PBDBformatted_NoTerr.csv" and copy this data into the combined database. Add 
# new IDNumbers (that pick up after those in the existing database), and
# re-save. Manually delete any "NA"s in early and late ages.

# (2) Open here and run following code to remove duplicated genus entries.

rm(list=ls())
setwd("C:/Users/pnovack-gottshall/Desktop/Databases/Maintenance & update R scripts")
pre <- read.delim(file="PreSizes_withPBDB.tab", stringsAsFactors=FALSE)
head(pre)
tail(pre)
duplicate.G <- duplicated(pre$Genus)
post <- pre[!duplicate.G, ]
write.table(post, file="PreSizes_withPBDB.tab", quote=FALSE, sep="\t", row.names=FALSE)

# (3) Run code as usual in "PropogateSizes.R" or "PropogateLifeHabits.R", but
# resaving as postX_withPBDB" file name. Make sure to add new IDNumbers to the
# new PBDB entries!

# (4) Import into copy of FileMakerPro life habit database, adding the new
# entries. Use this one for running next analyses.

# (5) Before running disparity and tiering analyses, open here and remove the
# non-terrestrials (and non-fossils with Recent-only occurrences?)
