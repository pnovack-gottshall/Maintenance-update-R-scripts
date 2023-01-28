## Quickly tally how many life habit references were used in the database. (Also
## checks any unintended insertion of tabs, double spaces, and other data-entry
## errors in the EcologyNotes field.)

# (1) Export ONLY the "IDNumber" (in case of error, allows you to know which
# entry to correct), "EcologyReference", and "EcologyNotes" fields as
# "LHRefs.tab". If want to tally body size references, do the same for
# "BodySizeReference" but do not include "EcologyNotes".

# (2) Open file in MSWord (make sure smart quotes are off: File > Options >
# Proofing > Autocorrect Options > Autoformat As You Type > uncheck Smart
# Quotes) and delete any hidden tabs (tab within a text field) and all
# problematic (i.e., double) quotation marks (replacing [UNLESS THE QUOTATION
# MARK IS CORRECTLY AT THE END OF A TEXT FIELD] "^t with ^t and replacing ^t"
# with ^t and replacing "" with "). Re-save file in same format, if any changed.

# (3) In Excel, add a row for headers and confirm the column headers are correct
# (and no cells are "hanging"). (If "hanging", it means there a tab was
# incorrectly deleted or there is a hidden tab within a field; remove the tab,
# re-save the file, and re-open.)

# (4). Open in Excel as tab-delimited and check for insertion of unintended tab
# characters. If found, remove in the original database field.

# (5). In Excel, "find" any double space mark ("_ _"). If found, replace with
# single space in the original database field.

# (6). In Excel, remove the "IDNumber" and "EcologyNotes" columns.

# (7). Open in Word, and find-and-replace ;_ with hard returns "^p", "^p with
# ^p, ^p" with ^p, and "Application of " with "". Text should be a column of
# references, one entry per line/row. Review and edit, if needed. Resave and
# open here.

# WARNING: THIS SCRIPT WILL NOT BE ABLE TO DISTINGUISH DIFFERENT REFERENCES WITH
# THE SAME CITATION FIELDS (e.g., Zamora, et al., 2012a and Zamora, et al.,
# 2012b) UNLESS THEY ARE SO IDENTIFIED IN THE DOWNLOADED DATABASE ENTRIES, SO
# PRODUCES ONLY A CONSERVATIVE ESTIMATE OF THE NUMBER OF REFERENCES. YOU WILL
# NEED TO COMPARE THE LIST TO YOUR BIBLIOGRAPHY TO IDENTIFY SUCH INVISIBLE
# REFERENCES.

rm(list = ls())
setwd("C:/Users/pnovack-gottshall/OneDrive - Benedictine University/Desktop/Databases/Maintenance & update R scripts")
# setwd("C:/Users/pnovack-gottshall/Documents/GSA (& NSF & NAPC)/2016GSA/GSA2016 analyses")
input <- read.delim(file = "LHRefs.tab", header = TRUE, colClasses = "character")
# input <- read.delim(file = "SizeRefs.tab", header = TRUE, colClasses = "character")
head(input)
str(input)

# For SizeRefs, need to grep to remove text after the year
if(ncol(input) == 2L) {
  for (r in 1:nrow(input)) {
    input[r, 2] <- strsplit(input[r, 2], ":")[[1]][1]
  }
  # To make following code easier (albeit less intuitive)
  colnames(input)[2] <- "EcologyReference"
  head(input)
}


# How many unique references?
length(unique(input$EcologyReference))

sort(table(input$EcologyReference), increasing = TRUE)
sorted.tab <- sort(table(input$EcologyReference), decreasing = TRUE)
names(sorted.tab) <- 1:length(sorted.tab)
plot(sorted.tab, xlab = "Life habit reference", 
     ylab = "No. of entries per reference", 
     main = "Distribution of life habit references used")
summary(as.vector(table(input$EcologyReference)))

# Save the list of unique references used
write.csv(sort(unique(input$EcologyReference)), file = "LHrefs.csv")
