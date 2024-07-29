source('R/data.R')

reachANOVA <- function() {
  
  dfs <- getData(baseline=TRUE)
  
  # for hand30 we used trial sets 47, 48 and 71
  # for pen30 its 95, 96 and 119
  # for hap30 its 
  
  dfs[[1]] <- dfs[[1]][which(dfs[[1]]$trialset_idx %in% c(47, 48, 71)),]
  dfs[[2]] <- dfs[[2]][which(dfs[[2]]$trialset_idx %in% c(47, 48, 71)),]
  
  dfs[[3]] <- dfs[[3]][which(dfs[[3]]$trialset_idx %in% c(95, 96, 119)),]
  
  adf <- do.call("rbind", dfs)
  
  adf$trialset_idx[which(adf$trialset_idx %in% c(47,95))] <- 1
  adf$trialset_idx[which(adf$trialset_idx %in% c(48,96))] <- 2
  adf$trialset_idx[which(adf$trialset_idx %in% c(71,119))] <- 3
  
  
  aovdf <- aggregate(deviation ~ trialset_idx + unid + condition, data=adf, FUN=mean)
  
  aovdf$condition <- as.factor(aovdf$condition)
  aovdf$unid      <- as.factor(aovdf$unid)
  
  afex::aov_ez( id = 'unid',
                dv = 'deviation',
                data = aovdf,
                between = 'condition',
                within = 'trialset_idx')
  
}

