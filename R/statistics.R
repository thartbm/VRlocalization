

getData <- function(conditions=c('hand30','hand60','pen30'), phases=c('aligned','rotated'),trialtypes=c('reach','nocursor','localization'),baseline=TRUE) {
  
  dfs <- list() 
  
  for (condition in conditions) {
    if (condition == 'hand30') { dfs[[length(dfs)+1]] <- readFiles(files=c('data/hand_30.csv'), phases=phases, trialtypes=trialtypes, baseline=baseline) }
    if (condition == 'hand60') { dfs[[length(dfs)+1]] <- readFiles(files=c('data/hand_60.csv'), phases=phases, trialtypes=trialtypes, baseline=baseline) }
    if (condition == 'pen30')  { dfs[[length(dfs)+1]] <- readFiles(files=c('data/pen_aligned.csv', 'data/pen_rotated.csv'), phases=phases, trialtypes=trialtypes, baseline=baseline) }
  }
  
  return(dfs)
  
}


readFiles <- function(files, phases, trialtypes, baseline) {
  
  df <- NA
  
  for (file in files) {
    temp <- read.csv(file, stringsAsFactors = FALSE)
    temp <- prepareData(temp, file)
    if (is.data.frame(df)) {
      df <- rbind(df, temp)
    } else {
      df <- temp
    }
  }
  
  
  print(files)
  if (baseline) {
    df <- applyBaseline(df)
  }
  
  return(df)
  
}

prepareData <- function(df, file) {
  
  if (file %in% c('data/hand_30.csv', 'data/hand_60.csv')) {
    # one way to prep the contents:
    
    # add phase:
    df$phase <- 'aligned'
    df$phase[which(df$trial_num >= 194)] <- 'rotated'
    
    # add unique participant ID:
    pre <- list('data/hand_30.csv'='h30', 'data/hand_60.csv'='h60')[[file]]
    df$unid <- sprintf('%s_%s', pre, df$ppid)
  } else {
    # longer pen things, different way to prep contents:
    
    # add phase:
    if (file == 'data/pen_aligned.csv') {df$phase <- 'aligned'}
    if (file == 'data/pen_rotated.csv') {df$phase <- 'rotated'}
    
    # add unique participant ID
    df$unid <- sprintf('p30_%s', df$ppid)
    
    # remove break or instruction "trials" / empty lines
    df <- df[which(df$type != ''),]
  }
  
  # get block numbers / trial set numbers
  df <- addBlockNumbers(df)
  
  # add trial type:
  df <- addTrialType(df)
  
  return(df)

}

addBlockNumbers <- function(df) {
  
  # get all trial numbers in a vector:
  trial_nums <- sort(unique(df$trial_num))
  
  # get block idx that skip "break" trials:
  block_idx <- ceiling(c(1:length(trial_nums)) / 4)
  
  # make a map from trial number to block index
  map <- rep(NA, max(trial_nums))
  for (idx in c(1:length(trial_nums))) {
    map[trial_nums[idx]] <- block_idx[idx]
  }
  
  # apply map to add a column of block indices:
  df$block_idx <- map[df$trial_num]
  
  return(df)
  
}

addTrialType <- function(df) {
  
  df$trialtype <- df$type
  
  df$trialtype[which(df$trialtype == 'aligned')] <- 'reach'
  df$trialtype[which(df$trialtype == 'rotated')] <- 'reach'
  
  return(df)
  
}

applyBaseline <- function(df) {
  
  print(class(df$trialtype))
  print(unique(df$trialtype))
  
}

reachANOVA <- function() {
  
  
  
  
  
}

