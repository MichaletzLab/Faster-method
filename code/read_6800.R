# Function to read LI-6800 files
read_6800 <- function(filename) {
  
  raw_input = readLines(filename)                # Read in raw datafile
  
  # Look for stability criteria and chop them out because they can mess things up
  #raw_input = raw_input[-grep("Stability Definition", raw_input)]
  
  data_start = grep("\\[Data\\]", raw_input) + 2 # Find where data starts (at "[Data]" line)
  data_end = grep("\\[Header\\]", raw_input) - 1 # Find where data ends (at "[Header]" line)
  data_end = c(data_end, length(raw_input))[-1]  # Add data end at end of file
  
  data_end = data_end[!(data_end < data_start[1])]
  
  compiled_data = c() # Initiate blank holder
  
  # Loop over each data set in the file
  for (i in 1:length(data_start)) {
    trimmed = raw_input[data_start[i]:data_end[i]]      # Grab data
    
    if(!is_empty(grep("Stability Definition", trimmed))) {
      trimmed = trimmed[-grep("Stability Definition", trimmed)]# Remove stability definitions
    }
    if(!is_empty(grep("^[^\t]*\t?[^\t]*$", trimmed))) {
      trimmed = trimmed[-grep("^[^\t]*\t?[^\t]*$", trimmed)] # Remove comments, hopefully
    }
    
    trimmed = trimmed[-2]                               # Remove units line
    current_data = read.csv(text = trimmed, sep = "\t") # Convert to dataframe
    compiled_data = rbind(compiled_data, current_data)  # Merge together into one 
  }
  compiled_data$filename = filename
  return(compiled_data)
}
