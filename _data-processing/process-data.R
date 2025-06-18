# Initialize Function
makeDat = function(){
  
  # Reading CSV:
  all_data = read.csv("../_data/raw-data.csv")
  
  # Combining Data
  all_data = do.call(rbind, out_list)
  
  # Getting rid of repeat participants
  repeat_pid = names(which(table(all_data$pid)>150))
  
  tdat = all_data[all_data$pid == repeat_pid,]
  sid_rep = unique(tdat$sid)
  bad_sid = sid_rep[2]
  all_data = all_data[!all_data$sid==bad_sid,]
  
  # Adding Subject Numbers 
  all_data$sub = as.numeric(factor(all_data$pid))
  
  # Adding Trial Numbers
  all_data$trial = rep(rep(1:15, 5*2), max(all_data$sub))
  
  
  # Creating Scores for each task
  all_data$y = NA
  
  # Brentano
  ind = which(all_data$task=="br")
  all_data[ind,]$y = all_data[ind,]$resp/all_data[ind,]$parA
  ind = which(all_data$task=="br" & all_data$version == 1)
  all_data[ind,]$y = -all_data[ind,]$y
  
  # Ebbinghaus
  ind = which(all_data$task=="eb" & all_data$version == 2)
  all_data[ind,]$y = (all_data[ind,]$resp-all_data[ind,]$parA)/all_data[ind,]$parA
  ind = which(all_data$task=="eb" & all_data$version == 1)
  all_data[ind,]$y = (all_data[ind,]$parA-all_data[ind,]$resp)/all_data[ind,]$parA
  
  # Poggendorf
  ind = which(all_data$task=="pog")
  all_data[ind,]$y = all_data[ind,]$resp-all_data[ind,]$parA
  ind = which(all_data$task=="pog" & all_data$version == 2)
  all_data[ind,]$y = -all_data[ind,]$y
  
  # Panzo
  ind = which(all_data$task=="pz")
  all_data[ind,]$y = (all_data[ind,]$resp-all_data[ind,]$parA)/all_data[ind,]$parA
  ind = which(all_data$task=="pz" & all_data$version == 2)
  all_data[ind,]$y = -all_data[ind,]$y
  
  # Zoellner
  ind = which(all_data$task=="zol")
  all_data[ind,]$y = all_data[ind,]$resp
  ind = which(all_data$task=="zol" & all_data$version == 2)
  all_data[ind,]$y = -all_data[ind,]$y
  all_data$y = all_data$y
  
  # Fixing RTs
  ind = which(all_data$rt == "null")
  all_data[ind,]$rt = 8000
  all_data$rt = as.numeric(all_data$rt)
  
  # Fixing Blocks
  all_data$block = rep(rep(c(1,2),each = 75), max(all_data$sub))
  
  # Writing the Output to a CSV File
  dir = paste0("merged_data")
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
  }
  write.csv(all_data, "../_data/processed-data.csv", row.names = F)
}

# Executing the function
makeDat()