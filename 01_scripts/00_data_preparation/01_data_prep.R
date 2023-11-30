require(tidyverse)
require(reshape2)

load("00_data/eurovoc_sparql.Rdata")

### This part of the code relies on the CEPS EurLex dataset:
### Borrett, Camille; Laurer, Moritz, 2020, 
### "The CEPS EurLex dataset: 142.036 EU laws from 1952-2019 with full text and 22 variables", 
### https://doi.org/10.7910/DVN/0EGYWY, Harvard Dataverse, V2, UNF:6:+hH0OxeHX0vYGiIOintTyA== [fileUNF]

### We use the "all text" Version
### Link to the data set: https://dataverse.harvard.edu/file.xhtml?persistentId=doi:10.7910/DVN/0EGYWY/6SVEIN&version=2.0
### Due to the large size we do not include an automatic download

ceps_full <- read.csv("00_data/EurLex_all.csv")

ceps_full$act_raw_text <- NULL

ceps_subset<- ceps_full %>% 
  select(CELEX,
         EUROVOC,
         Date_document)

# Define maximum number of columns necessary
max_number_of_eurovoc <- max(str_count(ceps_subset$EUROVOC , ";"))

ceps_split <- colsplit(ceps_subset$EUROVOC,
         pattern = "; ",
         names = 1:max_number_of_eurovoc)

#  Add back CELEX id
ceps_split$CELEX <- ceps_subset$CELEX

# Convert to long to match on individual terms
ceps_split_long <- melt(ceps_split, id.vars = "CELEX")
ceps_split_long$variable <- NULL
colnames(ceps_split_long) <- c("CELEX", "eurovoc_label")

# Set empty rows NA
ceps_split_long$eurovoc_label[str_length(ceps_split_long$eurovoc_label) == 0] <- NA

# extract only non empty rows
ceps_split_long_no_na <- ceps_split_long[!is.na(ceps_split_long$eurovoc_label),]

# merge based on preferred terms (eurovoc_label) and alternative forms (uf)
merged_ceps_eurovoc_long_pt <- merge(ceps_split_long_no_na, eurovoc_sparql, by="eurovoc_label")
merged_ceps_eurovoc_long_uf <- merge(ceps_split_long_no_na, eurovoc_sparql,by.x="eurovoc_label" ,by.y="uf")


ceps_mapped_long <- bind_rows(merged_ceps_eurovoc_long_pt, merged_ceps_eurovoc_long_uf)

# Extract unique domain_labels per Celex
ceps_mapped_long <- ceps_mapped_long %>% 
  select(CELEX, domain_label) %>% 
  unique()

# Convert to wide
ceps_mapped_wide <- dcast(ceps_mapped_long, CELEX~domain_label)

# extract domains
eurovoc_domains <- names(ceps_mapped_wide)[grepl("[0-9]", colnames(ceps_mapped_wide))]

# fill empty values with 0
ceps_mapped_wide[is.na(ceps_mapped_wide)] <- 0
# fill not 0 values with 1 in eurovoc_domain columns
ceps_mapped_wide[eurovoc_domains][ceps_mapped_wide[eurovoc_domains]!=0] <- 1

# merge into ceps_full, keeping all observations (all.x=T)
ceps_full_mapped <- merge(ceps_full, ceps_mapped_wide, by="CELEX", all.x=T)

# save finished data set
save(ceps_full_mapped, file="00_data/ceps_full_mapped.Rdata")
rm(list = ls())

