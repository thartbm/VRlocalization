
# download data from OSF -----

downloadCurrentData <- function() {
  
  Reach::downloadOSFdata(repository = 'vzds5',
                         filelist = list('/'=c( 'hand_30.csv',
                                                'hand_60.csv', 
                                                'pen_aligned.csv',
                                                'pen_rotated.csv',
                                                'omnibus_hand.csv')), # what is in this file?
                         folder = 'data',
                         overwrite = TRUE)

}

# pre-processing data -----



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
      # this only happens for the pen files: write special function?
      df <- combinePenData(df, temp)
      # df <- rbind(df, temp)
    } else {
      df <- temp
    }
  }
  
  if (length(files) > 1) {
    # pen data
    df <- addDeviations(df)
  } else {
    df <- df[,names(df)[which(!names(df) %in% c('X'))]]
    df$pen_present <- FALSE
    df$pen_3cm_out_angle <- NA
    df$pen_final_angle <- NA
  }
  
  if (baseline) {
    df <- applyBaseline(df)
  }
  
  # print(length(names(df)))
  
  return(df)
  
}

combinePenData <- function(aligned, rotated) {
  
  # treat this as one continues task:
  rotated$trialset_idx <- rotated$trialset_idx + max(aligned$trialset_idx)
  
  return(rbind(aligned, rotated))
  
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
    
    # add condition name:
    df$condition <- list('data/hand_30.csv'='hand30', 'data/hand_60.csv'='hand60')[[file]]
    
    # add effector:
    df$effector <- 'hand'
  } else {
    # longer pen things, different way to prep contents:
    
    # add phase:
    if (file == 'data/pen_aligned.csv') {df$phase <- 'aligned'}
    if (file == 'data/pen_rotated.csv') {df$phase <- 'rotated'}
    
    # add unique participant ID
    df$unid <- sprintf('p30_%s', df$ppid)
    
    # remove break or instruction "trials" / empty lines
    df <- df[which(df$type != ''),]
    
    # add condition name:
    df$condition <- 'pen30'
    
    # add effector
    df$effector <- 'hand'
    df$effector[which(df$pen_present)] <- 'pen'
    
    # hand-after-pen should have unique participant IDs for ANOVAs to work:
    hand_idx <- which(df$effector == 'hand')
    df$unid[hand_idx] <- sprintf('hap30_%s', df$ppid[which(df$effector == 'hand')])
    df$condition[hand_idx] <- 'hap30'
  }
  
  # get block numbers / trial set numbers
  df <- addTrialSetNumbers(df)
  
  # add trial type:
  df <- addTrialType(df)
  
  # remove practice blocks:
  df <- removePractice(df, file)
  
  return(df)
  
}

addTrialSetNumbers <- function(df) {
  
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
  df$trialset_idx <- map[df$trial_num]
  
  return(df)
  
}

addTrialType <- function(df) {
  
  df$trialtype <- df$type
  
  df$trialtype[which(df$trialtype == 'aligned')] <- 'reach'
  df$trialtype[which(df$trialtype == 'rotated')] <- 'reach'
  
  return(df)
  
}

removePractice <- function(df, file) {
  
  if (file %in% c('data/hand_30.csv', 'data/hand_60.csv', 'data/pen_aligned.csv')) {
    df <- df[which(df$block_num > 7),]
  }
  
  return(df)
  
}

addDeviations <- function(df) {
  
  for (trialtype in c('nocursor', 'localization')) {
    
    idx <- which(df$trialtype == trialtype)
    
    if (trialtype == 'nocursor') {
      for (effector in c('pen','hand')) {
        if (effector == 'hand') {
          df$deviation[idx] <- df$cursor_final_angle[idx] - df$target_angle[idx]
        }
        if (effector == 'pen') {
          df$deviation[idx] <- df$pen_final_angle[idx] - df$target_angle[idx]
        }
      }
      
    }
    
    if (trialtype == 'localization') {
      df$deviation[idx] <- df$localizing_angle[idx] - df$arc_aquired_angle[idx]
    }
    
  }
  
  return(df)
  
}

applyBaseline <- function(df) {
  
  for (condition in unique(df$condition)) {
    
    for (effector in unique(df$effector)) {
      
      for (trialtype in unique(df$trialtype[which(df$phase == 'aligned' & df$effector == effector & df$condition == condition)])) {
        
        AL_trialset_idx <- unique(df$trialset_idx[which(df$phase == 'aligned' & df$trialtype == trialtype & df$condition == condition)])
        RO_trialset_idx <- unique(df$trialset_idx[which(df$phase == 'rotated' & df$trialtype == trialtype & df$condition == condition)])
        
        targets <- unique(df$target_angle)
        
        # print(c(condition, effector, trialtype))
        # print(c(length(AL_trialset_idx), length(RO_trialset_idx)))
        
        for (pp in unique(df$unid[AL_trialset_idx])) {
          
          for (target in targets) {
            
            AL_idx <- which(df$unid == pp & df$target_angle == target & df$trialset_idx %in% AL_trialset_idx)
            RO_idx <- which(df$unid == pp & df$target_angle == target & df$trialset_idx %in% RO_trialset_idx)
            
            # if (length(AL_idx) == 0) {print(c(pp, target))}
            
            baseline_bias <- median( df$deviation[AL_idx], na.rm=TRUE )
            
            # print(c(df$condition[1], effector, trialtype, target, baseline_bias))
            
            df$deviation[RO_idx] <- df$deviation[RO_idx] - baseline_bias
            
          }
          
        }
        
      }
      
    }
    
  }
  
  return(df)
  
}
