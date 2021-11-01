build.database = function() {
  
  # Read all validation files
  all_files = list.files("data/data_validation", recursive = T, full.names = T) %>% 
    grep("\\.", ., value = T, invert = T)

  data_raw = lapply(all_files, read_6800)
  
  data_unpack = c()
  for (i in 1:length(data_raw)) {
    data_unpack = bind_rows(data_unpack, data.frame(data_raw[i], curveID = i))
  }
  
  # Now separate out the data 
  data_unpack$species = NA
  data_unpack$rep = data_unpack$filename %>% str_match("r[0-9]+") %>% str_extract("[0-9]+")
  data_unpack[grep("rasa", data_unpack$filename),]$species = "RASA"
  data_unpack[grep("tito", data_unpack$filename),]$species = "TITO"
  
  LRC = data_unpack[grep("lrc", data_unpack$filename),]
  
  AT_step = data_unpack[grep("step", data_unpack$filename),]
  AT_chamber = data_unpack[grep("chamber", data_unpack$filename),]
  AT_faster = data_unpack[grep("faster_r", data_unpack$filename),] 
  
  AT_insitu = data_unpack[grep("insitu", data_unpack$filename),]
  
  RACiR = data_unpack[grep("racir", data_unpack$filename),]
  
  AT_insitu$rep = as.character(AT_insitu$leafRep)
  
  # Things to do:
  # Add columns for species, method, rep
  # Try fitting curves and comparing parameter estimates
  # You'll probably want to do some NLMEs at some point
  # LRC safe to ignore for now, just nice to have
  # RACiR curves need a fair amount of processing - is there a package?
  
  AT_chamber$method = "chamber" # OK
  AT_step$method    = "step"    # OK
  AT_faster$method  = "faster"  # OK
  AT_insitu$method  = "insitu"  # OK

  return(list(AT_step = AT_step, 
              AT_chamber = AT_chamber, 
              AT_insitu = AT_insitu, 
              AT_faster = AT_faster, 
              RACiR = RACiR))
}
