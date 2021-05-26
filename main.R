# Species selection

#set.seed(1)
no_species = 100

UBC_botanical_garden_species_list <- read_excel("UBC_botanical_garden_species_list.xlsx")

wild_species_list = subset(UBC_botanical_garden_species_list, ProvenanceCode == "W")

unique_families = unique(wild_species_list$Family)




# I think we want things which are C3 and C4 - CAM makes life difficult

# How to get an estimat of economic traits?


library(BIEN)

