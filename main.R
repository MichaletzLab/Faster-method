library(readxl)
library(BIEN)
library(tidyverse)

# Species selection

#set.seed(1)
no_species = 100

UBC_botanical_garden_species_list <- read_excel("UBC_botanical_garden_species_list.xlsx")

# Keep only wild provenance
wild_species_list = subset(UBC_botanical_garden_species_list, ProvenanceCode == "W")

# Remove unknown species
wild_species_list = subset(wild_species_list, Species != "sp.")
wild_species_list = subset(wild_species_list, !is.na(Species))

# Remove hybrids
wild_species_list = subset(wild_species_list, !str_detect(TaxonName, "×"))

# Remove known CAM photosynthesizers
photo_path = read.csv("photo_path_data_extracted_uncorrected.csv")
photo_path = unique(photo_path)
photo_path$GenusSpecies = photo_path$Taxon

w = merge(wild_species_list, photo_path, by = "GenusSpecies")

# Generate scientific name without subspecies
wild_species_list$GenusSpecies = paste(wild_species_list$Genus, wild_species_list$Species)

# Extract unique list of species with associated family
unique_species_list = wild_species_list %>% select(GenusSpecies, Family, Genus, Species) %>% unique()

# Randomly choose species by family
set.seed(1)
unique_species_list$Family = as.factor(unique_species_list$Family)

random_species_list = c()

for (f in unique(unique_species_list$Family)) {
  cur_fam = subset(unique_species_list, Family == f)
  n = ifelse(dim(cur_fam)[1] < 3, dim(cur_fam)[1], 3)
  
  cur_sample = sample_n(cur_fam, n)
  cur_sample$priority = 1:n
  
  random_species_list = rbind(random_species_list, cur_sample)
  
}



sampled_by_family = unique_species_list %>% group_by(Family) %>% sample_n(3)

# So it's easy to randomly select one species per family. But I need to make sure they are appropriate...
# Does this sample span economic trait values? May just have to assume yes given that we don't have the data
# Is it C3, C4, or CAM? I think we should exclude CAM
# 

# Then, subsample for validation dataset
# Does validation dataset include at least one of:
# C3, C4, needle leaf, broad leaf, scale leaf, graminoid, woody, herb
# canopy, and understory?



                          