## SELECT PROPER COL HEADINGS

# FileMakerPro does not have a custom-export function that allows users to
# specify which columns to export for different export tasks. The code below
# allows users to export ALL columns (with headers) and then use the code to
# customize the columns. Much easier than doing manually in FileMakerPro!


## WORKFLOW ------------------------------------------------------------------

# (1) From Body size layout, export ALL data fields (EXCEPT EcologyNotes),
# selecting "Current Table ("Biota")" in dropdown, as 'AllCols.tab'
# tab-delimited format. DO NOT SAVE AS EXCEL FORMAT, AS DOING SO
# IDIOSYNCRATICALLY CHANGES NAs TO 0s AND 1s!! (MUST be tab-delimited, as
# several text fields contain commas!!!). Save a duplicate as an Excel Workbook
# (AllColsHeadings.xlsx) to automatically obtain column headings. (Need to
# create this colnames file EVERY TIME in case the order of columns has
# changed.)

# (2) Open file in MSWord (make sure smart quotes are off: File > Options >
# Proofing > Autocorrect Options > Autoformat As You Type > uncheck Smart
# Quotes). UNLESS THE TERMINAL QUOTATION MARKS ARE CORRECTLY AT THE END OF A
# TEXT FIELD, find and replace (a) "^t with ^t and (b) replace ^t" with ^t and
# (c) replace "" [double quotation marks] with " [single quotation marks].
# Re-save file in same format.

# (3) In Excel, add a row for headers and confirm the column headers are correct
# (and no cells are "hanging"). (If "hanging", it means there a tab was
# incorrectly deleted or there is a hidden tab within a field; remove the tab,
# re-save the file, and re-open.)


## IMPORT ALL-COLUMNS DATA FILE ---------------------------------------------------

rm(list = ls())
setwd("C:/Users/pnovack-gottshall/Desktop/Databases/Maintenance & update R scripts")
# setwd("C:/Users/pnovack-gottshall/Documents/GSA (& NSF & NAPC)/2016GSA/GSA2016 analyses")
all <- read.delim(file = "AllCols.tab", stringsAsFactors = FALSE)
# all <- read.delim(file = "AllCols_Constant_PBDB.tab", stringsAsFactors = FALSE)
# all <- read.delim(file = "AllCols_Mode_PBDB.tab", stringsAsFactors = FALSE)
head(all)
nrow(all)


## CHOOSE SELECTED COLUMN SET -----------------------------------------------------


# IDBadHigherTaxa columns:     ----------------------------------------------------
cols <- c("Phylum", "Subphylum", "Class", "Subclass", "Order", "Suborder", 
          "Superfamily", "Family", "Subfamily")
wh.cols <- match(cols, colnames(all))
selected <- all[, wh.cols]
head(selected)
if (!identical(cols, colnames(selected)))
  stop("column names are not as specified!")
write.csv(selected, file = "HigherTaxa.csv", row.names = FALSE)



# UpdateAges&DivCurve columns: ----------------------------------------------------
cols <- c("IDNumber", "Phylum", "Class", "Order", "Superfamily", "Family", "Genus", 
          "Subgenus", "Species", "max_age", "max_ma", "min_age", "min_ma")
wh.cols <- match(cols, colnames(all))
selected <- all[, wh.cols]
head(selected)
if (!identical(cols, colnames(selected)))
  stop("column names are not as specified!")
write.csv(selected, file = "occs.csv", row.names = FALSE)




# PropogateSizes columns:     ----------------------------------------------------
cols <- c("IDNumber", "Phylum", "Subphylum", "Class", "Subclass", "Order", 
          "Suborder", "Superfamily", "Family", "Subfamily", "Genus", "Subgenus", 
          "Species", "max_ma", "min_ma", "BodySizeScale", "RefGenusSize", 
          "RefSpeciesSize", "Enterer", "DateEntered_Size", "SizeChanged", 
          "History_Size", "BodyMeasureReference", "APLength", "TransverseLength", 
          "DVLength", "PhotoAP", "PhotoTransverse", "PhotoDV", "APScale", 
          "TransverseScale", "DVScale", "Est_AP", "Est_T", "Est_DV", 
          "AbsStratDistance", "Est_AbsStratDistance")
wh.cols <- match(cols, colnames(all))
selected <- all[, wh.cols]
head(selected)
if (!identical(cols, colnames(selected)))
  stop("column names are not as specified!")
# write.table(selected, file="PreSizes.tab", row.names = FALSE, sep = "\t", quote = FALSE)
# write.table(selected, file="PreSizes_Constant_withPBDB.tab", row.names = FALSE, sep = "\t", quote = FALSE)
# write.table(selected, file="PreSizes_Mode_withPBDB.tab", row.names = FALSE, sep = "\t", quote = FALSE)




# PropogateLH columns:     ----------------------------------------------------
cols <- c("IDNumber", "Phylum", "Subphylum", "Class", "Subclass", "Order", 
          "Suborder", "Superfamily", "Family", "Subfamily", "Genus", "Subgenus", 
          "Species", "EcologyScale", "RefGenusEco", "RefSpeciesEco", 
          "DateEntered_Ecology", "SizeChanged", "BodySizeScale", "History_Ecology", 
          "AboveImmediate", "AbovePrimary", "AbsFoodStratification", 
          "AbsStratification", "AmbientFeeder", "Asexual", "Attached", 
          "AttachmentFeeder", "Autotroph", "Biotic", "BulkFeeder", "Carnivore", 
          "FeedingAboveImm", "FeedingAbovePrimary", "FeedingWithinImm", 
          "FeedingWithinPrimary", "FilterDensity", "FilterFeeder", "Fluidic", 
          "FreeLiving", "HardSubstratum", "Herbivore", "Incorporeal", 
          "Insubstantial", "Lithic", "MassFeeder", "Microbivore", "Mobility", 
          "ParticleFeeder", "RaptorFeeder", "RelFoodStratification", 
          "RelStratification", "SelfSupport", "Sexual", "SoftSubstratum", 
          "SolutionFeeder", "Supported", "WithinImmediate", "WithinPrimary", 
          "Est_AboveImmediate", "Est_AbovePrimary", "Est_AbsFoodStratification", 
          "Est_AbsStratification", "Est_AmbientFeeder", "Est_Asexual", 
          "Est_Attached", "Est_AttachmentFeeder", "Est_Autotroph", "Est_Biotic", 
          "Est_BulkFeeder", "Est_Carnivore", "Est_FeedingAboveImm", 
          "Est_FeedingAbovePrimary", "Est_FeedingWithinImm", 
          "Est_FeedingWithinPrimary", "Est_FilterDensity", "Est_FilterFeeder", 
          "Est_Fluidic", "Est_FreeLiving", "Est_HardSubstratum", "Est_Herbivore", 
          "Est_Incorporeal", "Est_Insubstantial", "Est_Lithic", "Est_MassFeeder", 
          "Est_Microbivore", "Est_Mobility", "Est_ParticleFeeder", "Est_RaptorFeeder", 
          "Est_RelFoodStratification", "Est_RelStratification", "Est_SelfSupport", 
          "Est_Sexual", "Est_SoftSubstratum", "Est_SolutionFeeder", "Est_Supported",
          "Est_WithinImmediate", "Est_WithinPrimary")
wh.cols <- match(cols, colnames(all))
selected <- all[, wh.cols]
head(selected)
if (!identical(cols, colnames(selected)))
  stop("column names are not as specified!")
# write.table(selected, file="PreLH_constant.tab", row.names = FALSE, sep = "\t", quote = FALSE)
# write.table(selected, file="PreLH_mode.tab", row.names = FALSE, sep = "\t", quote = FALSE)
# write.table(selected, file="PreLH_constant_PBDB.tab", row.names = FALSE, sep = "\t", quote = FALSE)
# write.table(selected, file="PreLH_mode_PBDB.tab", row.names = FALSE, sep = "\t", quote = FALSE)
