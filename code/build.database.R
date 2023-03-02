# Function to build database
build.database = function() {
  
  # Find all AT files
  all_files = list.files("data/data_AT", recursive = T, full.names = T) %>% 
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
  data_unpack$species = NA
  data_unpack$rep = data_unpack$filename %>% str_match("r[0-9]+") %>% str_extract("[0-9]+")
  data_unpack$rep[grep("blank",data_unpack$filename)] = "blank"
  #data_unpack[grep("rasa", data_unpack$filename),]$species = "RASA"
  
  AT_step = data_unpack[grep("step", data_unpack$filename),]
  AT_chamber = data_unpack[grep("chamber", data_unpack$filename),]
  AT_faster = data_unpack[grep("faster_r", data_unpack$filename),] 

  AT_chamber$method = "chamber" # OK
  AT_step$method    = "step"    # OK
  AT_faster$method  = "faster"  # OK
  
  return(list(AT_step = AT_step, 
              AT_chamber = AT_chamber, 
              AT_faster = AT_faster) )
}
