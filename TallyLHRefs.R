## Quickly tally how many life habit references were used in the database. (Also
## checks any unintended insertion of tabs, double spaces, and other data-entry
## errors in the EcologyNotes field.)

# (1) Export ONLY the "IDNumber" (in case of error, allows you to know which entry
# to correct), "EcologyReference", and "EcologyNotes" fields as "LHRefs.tab".

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
# ^p, and ^p" with ^p. Text should be a column of references, one entry per
# line/row. Review and edit, if needed. Resave and open here.

rm(list = ls())
setwd("C:/Users/pnovack-gottshall/Desktop/Databases/Maintenance & update R scripts")
# setwd("C:/Users/pnovack-gottshall/Documents/GSA (& NSF & NAPC)/2016GSA/GSA2016 analyses")
input <- read.delim(file = "LHRefs.tab", header=TRUE, colClasses = "character")
head(input)
str(input)

# How many unique references?
length(unique(input$EcologyReference))

sort(table(input$EcologyReference))
plot(sort(table(input$EcologyReference)))
summary(as.vector(table(input$EcologyReference)))


