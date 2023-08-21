# Function to build database
build_database = function() {
  
  # Find all AT files
  all_files = list.files("data/methods_comparison", recursive = T, full.names = T) %>% 
    grep("\\.", ., value = T, invert = T)
  
  # Read LI-6800 files
  data_raw = lapply(all_files, read_6800)
  
  # Unpack data
  data_unpack = c()
  for (i in 1:length(data_raw)) {
    data_unpack = bind_rows(data_unpack, 
                            data.frame(data_raw[i], curveID = i) %>% select(-averaging))
  }
  
  # Now separate out the data 
  data_unpack$rep = data_unpack$filename %>% str_match("P[0-9]+") %>% str_extract("[0-9]+")

  AT_step = data_unpack[grep("SEM", data_unpack$filename),]
  AT_chamber = data_unpack[grep("ATC", data_unpack$filename),]
  AT_faster = data_unpack[grep("FAS", data_unpack$filename),] 
  
  AT_chamber$method = "chamber" # OK
  AT_step$method    = "step"    # OK
  AT_faster$method  = "faster"  # OK
  
  return(list(AT_step = AT_step, 
              AT_chamber = AT_chamber, 
              AT_faster = AT_faster) )
}

