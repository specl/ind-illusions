
# Function to retrieve file names from a GitHub repository.
listFilesGithub=function(usr="PerceptionCognitionLab", repo,filter){
  header="https://api.github.com/repos/"
  trailer="/git/trees/main?recursive=1"
  request <- paste(header,usr,"/",repo,trailer,sep="")
  req <- GET(request)
  stop_for_status(req)
  content <- unlist(lapply(content(req)$tree, "[", "path"), use.names = F)
  return(content[grep(filter, content)])
}

# Function to merge data from multiple CSV files hosted on GitHub
mergeData = function(){
  
  # Package setup
  necessary_packages = c("httr")
  new_packages = necessary_packages[!(necessary_packages %in% installed.packages()[,"Package"])]
  if(length(new_packages)) install.packages(new_packages)
  library(httr)
  
  # Preparing URLs and Fetching Data
  front_url = "https://raw.githubusercontent.com/PerceptionCognitionLab/data4/main/"
  end_url_list = listFilesGithub(usr="PerceptionCognitionLab",
                                 repo="data4",
                                 filter="iBat5/iBat5_p")
  out_list = list()
  obj = numeric(length(end_url_list))
  
  # Downloading and Processing Data
  for (i in 1:length(end_url_list)){
    url = paste0(front_url, end_url_list[i])
    dat = read.csv(url)
    
    rowSelect = dat$task %in% c("br","eb","pog","pz","zol")
    out1 = dat[rowSelect, c("pid", 
                            "task", 
                            "resp", 
                            "start", 
                            "version", 
                            "parA", 
                            "rt", 
                            "time_elapsed", 
                            "sid", 
                            "block",
                            "jitterX",
                            "jitterY")]
    obj[i] = nrow(dat)
    out_list[[i]] = out1
  }
  
  # Combining Data and Saving Data:
  raw_data = do.call(rbind, out_list)
  write.csv(raw_data,"../_data/raw-data.csv")
}
mergeData()