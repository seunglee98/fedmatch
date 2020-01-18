#' match_evaluate
#'
#' @description evaluate a matched dataset
#'
#' @param matches data.frame. Merged dataset. 
#' @param data1 data.frame. First to-merge dataset.
#' @param data2 data.frame. Second to-merge dataset.
#' @param unique_key_1 character vector. Primary key of data1 that uniquely identifies each row (can be multiple fields)
#' @param unique_key_2 character vector. Primary key of data2 that uniquely identifies each row (can be multiple fields)
#' @param suffixes character vector. Mnemonics associated data1 and data2.
#' @param tier character vector. Default=NULL. The variable that defines a tier.
#' @param tier_order character vector. Default=NULL. Variable that defines the order of tiers, if needed.
#' @param aggregate_by character vector. Default='unique'. Variable aggregated to calculate statistics. If equal to 'unique', aggregation is count, if otherwise, sum.
#' @param quality_vars character vector. Variables you want to use to calculate the quality of each tier. Calculates mean. 
#'
#' @return data.frame. Table describing each tier according to aggregate_by variables and quality_vars variables.
#' 
#' @seealso merge_plus
#'
#' @export
#'
#' @examples
#' x = data.frame('id'=1:5,'name'=c('a','b','c','d','d'), 'amount' = 101:105)
#' y = data.frame('id'=6:10,'name'=c('b','c','d','e','f'), 'amount' = rep(102,5))
#' matches = merge_plus(x,y,by='name',unique_key_1='id','unique_key_2'='id', matchscore_settings = list('amount'=list('compare_type'='difference',weight=1)), evaluate=NULL)$'matches'
#' match_evaluate(matches, x, y, 'id.x', 'id.y')  
#' 
#' 
match_evaluate <- function(matches, data1, data2, unique_key_1, unique_key_2, suffixes=c('.x','.y'), 
                     tier=NULL, tier_order=NULL,
                     aggregate_by='unique',  quality_vars=NULL, dupe_ratio=FALSE){
  
  # preliminaries
  
    ## fixing unique_keys
    unique_key_1_matches = unique_key_1
    unique_key_2_matches = unique_key_2
    for(i in 1:pmax(length(unique_key_1),length(unique_key_2))){
      if(unique_key_1[i] %in% names(data2) & !(unique_key_1[i] %in% names(matches))){
        unique_key_1_matches[i] = paste0(unique_key_1[i],suffixes[1])}
      if(unique_key_2[i] %in% names(data1) & !(unique_key_2[i] %in% names(matches))){
        unique_key_2_matches[i] = paste0(unique_key_2[i],suffixes[2])}
    }
  
    ## defaults for tiers  
    if(!is.null(tier)){     
      matches[,'tier.'] = apply(matches[tier], 1, paste, collapse="_")
      tiers =c(matches[,'tier.'][!duplicated(matches[,'tier.'])],'all')
    } else {
      tiers = 'all'
    }
   
    ## putting tiers in proper order, if needed
    if(!is.null(tier_order)){
      tiers = tiers[match(c(tier_order,'all'),tiers)]
      tiers = tiers[!is.na(tiers)]}
    
    ## initializing variables
    match_evaluation = NULL
    matches[,'counted_1'] = matches[,'counted_2'] = 0
    
    
  # evaluating tiers
  for(tier in tiers){
    
    ## subsetting matches to current tier 
    if(tier !='all'){
      temp = matches[which(matches[,'tier.']==tier),] 
    } else {
      temp = matches
    }
    
    ## initializing output 
    tier_evaluation = data.frame('tier' = tier, 'matches' = nrow(temp))
    
    ## evaluating tier 
    if(nrow(temp)!=0){
      
      ### count number or amount of the aggregate_by variable in the tier
      for(j in 1:length(aggregate_by)){
        for(i in 1:2){

            #### initializing
            unique_key = get(paste('unique_key',i,'matches',sep='_'))
            counted = paste('counted',i,sep='_')                 
            
            #### counting 
            aggregated = temp[which(temp[,counted]==0),]
            aggregated = aggregated[!duplicated(aggregated[,unique_key]),]
            if(aggregate_by[j]=='unique'){
              x = nrow(aggregated)
            } else {
              x = sum(aggregated[,paste0(aggregate_by[j],suffixes[i])], na.rm=TRUE)
            }
         
            #### adding to result
            tier_evaluation=cbind(tier_evaluation, x)
            names(tier_evaluation)[ncol(tier_evaluation)] = paste(aggregate_by[j],i)
            
            #### marking as counted       
            matches[,counted][which(apply(matches[unique_key], 1, paste, collapse="_") %in% apply(aggregated[unique_key], 1, paste, collapse="_"))] = 1
        }
      }
      
      ## calculating duplicate ratio 
      if(dupe_ratio==TRUE){
        for(i in 1:2){
          tier_evaluation[,paste('duplicate ratio',i)] = mean(aggregate(apply(temp[get(paste0('unique_key_',(3 - i %% 3),'_matches'))], 1, paste, collapse="_"), list(apply(temp[get(paste0('unique_key_',i,'_matches'))], 1, paste, collapse="_")), function(x) length(unique(x)))[,2])}}
      
      ### calculate the average quality of the tier
      if(!is.null(quality_vars)){
        for(var in quality_vars){
          
          #### cleaning
          temp[,var][which(abs(temp[,var]) == Inf)]=NA
          
          #### calculating average quality for var in tier 
          x = data.frame('average'= mean(temp[,var], na.rm=TRUE))  #, 'median'= median(temp[,var], na.rm=TRUE)
          
          #### adding to result 
          names(x) = paste(var,names(x))
          tier_evaluation=cbind(tier_evaluation, x)
        }
      }
      
      ### add to match_evaluation 
      if(is.null(match_evaluation)){
        match_evaluation = tier_evaluation
      } else {
        match_evaluation=rbind(match_evaluation, tier_evaluation)
      }
    }
  }
   
  # calculating totals (note, tier order must be correct
  for(j in 1:length(aggregate_by)){
    for(i in 1:2){   
      ## initializing data
      temp = get(paste0('data',i))
      unique_key = get(paste('unique_key',i,sep='_'))
      if(aggregate_by[j] %in% c(names(temp),'unique')){
      
        ## calculating totals
        match_evaluation[,paste('Total',aggregate_by[j],i)] =  cumsum(match_evaluation[,paste(aggregate_by[j],i)])  
      
        ## caluating percentages 
        if(aggregate_by[j] == 'unique'){
          match_evaluation[,paste('pct',aggregate_by[j],i)] =  match_evaluation[,paste('Total',aggregate_by[j],i)]/nlevels(as.factor(apply(temp[unique_key], 1, paste, collapse="_")))
        } else {
          match_evaluation[,paste('pct',aggregate_by[j],i)] =  match_evaluation[,paste('Total',aggregate_by[j],i)]/sum(temp[!duplicated(apply(temp[unique_key], 1, paste, collapse="_")),aggregate_by[j]],na.rm=TRUE)
        }
      
        ### removing total if just one tier 
        #if(length(tiers)==1){
        #  match_evaluation[,paste('Total',aggregate_by[j],i)] = NULL}
      }
    }
  }
    
  
  
    
  return(match_evaluation)
}
