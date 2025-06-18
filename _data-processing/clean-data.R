
cleanData = function(){
  processed_data = read.csv("../_data/processed-data.csv")
  
  # Outliers in Reaction Times
  low_rt_ound = 600
  low_rts_prop = mean(processed_data$rt < low_rt_ound)*100
  bad_obs_rts = which(processed_data$rt < low_rt_ound)
  low_too_quick = .05
  too_quick = tapply(processed_data$rt<low_rt_ound, 
                     processed_data$sub, mean)
  num_partips_too_quick = sum(too_quick > low_too_quick)
  bad_partips_rt = unique(processed_data$sub)[which(too_quick > low_too_quick)]
  bad_RTs = unique(c(which(processed_data$sub %in% bad_partips_rt), 
                     bad_obs_rts))
  dat1 = processed_data[-bad_RTs,]
  
  # Outliers in Univariate Setup
  scores = tapply(dat1$y, list(dat1$sub,dat1$task), mean)
  standardized_scores = scale(scores)
  bad_sub_panzo = as.integer(names(which(standardized_scores[,4]>2)))
  bad_sub_panzo_ind = which(dat1$sub %in% bad_sub_panzo)
  num_bad_sub_panzo = length(bad_sub_panzo)
  dat2 = dat1[-bad_sub_panzo_ind,]
  stand_error = tapply(dat2$y, list(dat2$sub, dat2$task), 
                       function(x) sd(x) / sqrt(length(x)))
  bad_zol_partip = as.integer(names(which(stand_error[,5]>.8)))
  ind = which(dat2$sub == bad_zol_partip)
  dat3 = dat2[-ind,]
  
  # Outliers in Bivariate Setup
  scores = tapply(dat3$y, 
                  list(dat3$sub, dat3$version, dat3$task), 
                  mean)
  pz = scale(scores[,,4])
  eb = scale(scores[,,2])
  s_pz = apply(pz, 1, function(x) any(x>2) && any(x < -2))
  s_eb = apply(eb, 1, function(x) any(x>1.8) && any(x < -1.7))
  bad_eb_sub = as.integer(rownames(eb)[which(s_eb)])
  bad_pz_sub = as.integer(rownames(pz)[which(s_pz)])
  ind = which(dat3$sub == bad_pz_sub)
  dat4 = dat3[-ind,]
  
  
  # Adding levels for subjects, tasks
  dat4$S = as.numeric(factor(dat4$sub))
  dat4$J = as.numeric(factor(dat4$task))

  # Saving the clean data
  write.csv(dat4, "../_data/clean_data.csv")
  
}
cleanData()
