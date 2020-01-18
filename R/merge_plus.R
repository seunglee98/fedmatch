#' merge_plus
#'
#' @description merge two datasets, plus.
#'
#' @param data1 data.frame. First to-merge dataset.
#' @param data2 data.frame. Second to-merge dataset.
#' @param by character string. Variables to merge on (common across data 1 and data 2). See \code{merge}
#' @param by.x character string. Variable to merge on in data1. See \code{merge}
#' @param by.y character string. Variable to merge on in data2. See \code{merge}
#' @param suffixes character vector with length==2. Suffix to add to like named variables after the merge. See \code{merge}
#' @param unique_key_1 character vector. Primary key of data1 that uniquely identifies each row (can be multiple fields)
#' @param unique_key_2 character vector. Primary key of data2 that uniquely identifies each row (can be multiple fields)
#' @param check_merge logical. Checks that your unique_keys are indeed unique, and prevents merge from running if merge would result in data.frames larger than 5 million rows
#' @param match_type. string. If 'exact', match is exact, if 'fuzzy', match is fuzzy.
#' @param amatch.args. additional arguments for amatch, to be used if match_type = 'fuzzy'. Suggested defaults provided. (see amatch, method='jw')
#' @param score_settings list. score settings. See vingette matchscore
#' @param filter function or numeric. Filters a merged data1-data2 dataset. If a function, should take in
#'       a data.frame (data1 and data2 merged by name1 and name2) and spit out a trimmed verion
#'       of the data.frame (fewer rows). Think of this function as applying other conditions
#'       to matches, other than a match by name. The first argument of filter should be the data.frame.
#'       If numeric, will drop all observations with a matchscore lower than or equal to filter.
#' @param filter.args list. Arguments passed to filter, if a function
#' @param evaluate Function to evalute merge_plus output.
#' @param evaluate.args ist. Arguments passed to evaluate
#'
#' @return list with matches, filtered matches (if applicable), data1 and data2 minus matches, and match evaluation
#'
#' @seealso match_evaluate
#'
#' @export
#'
#' @examples
#' x = data.frame('id'=1:5,'name'=c('a','b','c','d','d'), 'amount' = 101:105)
#' y = data.frame('id'=6:10,'name'=c('b','c','d','e','f'), 'amount' = rep(102,5))
#' merge_plus(x,y,by='name',unique_key_1='id','unique_key_2'='id')
#' merge_plus(x,y,by='name',unique_key_1='id','unique_key_2'='id',
#'            matchscore_settings = list('amount'=list('compare_type'='difference',weight=1)),
#'            filter=.5)
#'
#'
merge_plus <- function(data1, data2, by=NULL, by.x=by, by.y=by, suffixes=c(".x",".y"),
                       check_merge = TRUE, unique_key_1, unique_key_2,
                       match_type = 'exact', amatch.args=list(method='jw', p=0.1, maxDist=0.05, matchNA=FALSE),
                       score_settings=NULL, filter=NULL, filter.args=list(),
                       evaluate = match_evaluate, evaluate.args=list()) {

  # Merge datasets data1 and data2 by name1 and name2;

    ## checking merge
    if(check_merge==TRUE){

      ### checking that unique_keys are indeed unique
      if(nrow(data1)!=nrow(data1[!duplicated(data1[,c(unique_key_1)]),])){
        stop('unique_key_1 does not uniquely identify data1')}
      if(nrow(data2)!=nrow(data2[!duplicated(data2[,c(unique_key_2)]),])){
        stop('unique_key_2 does not uniquely identify data2')}

      ### checking to make sure match won't break computer
      if(count_rows(data1[which(!is.na(data1[,by.x[1]])),], data2[which(!is.na(data2[,by.y[1]])),], by.x=by.x, by.y=by.y)>5000000){
        stop('Potential merge too large! Pick different mergeids')} }

    ## fixing unique_keys
    unique_key_1_matches = unique_key_1
    unique_key_2_matches = unique_key_2
    for(i in 1:pmax(length(unique_key_1),length(unique_key_2))){
      if(unique_key_1[i] %in% names(data2) & !(unique_key_1[i] %in% c(by, by.x, by.y))){
        unique_key_1_matches[i] = paste0(unique_key_1[i],suffixes[1])}
      if(unique_key_2[i] %in% names(data1) & !(unique_key_2[i] %in% c(by, by.x, by.y))){
        unique_key_2_matches[i] = paste0(unique_key_2[i],suffixes[2])}
    }

    if(match_type=='exact'){

      matches = merge(data1[which(!is.na(data1[,by.x[1]])),], data2[which(!is.na(data2[,by.y[1]])),], by.x=by.x, by.y=by.y, suffixes=suffixes)

    } else if (match_type=='fuzzy') {

      ### ordering data
      data1 = data1[order(data1[,by.x]),]
      data2 = data2[order(data2[,by.y]),]

      ###fuzzymatch
      matches = do.call(stringdist::amatch, c(list(data1[,by.x],data2[,by.y]), amatch.args))
      matches = merge(cbind(data1, matches), cbind(data2,  index = seq(1,length(data2[,by.y]))), by.x='matches', by.y ='index', all.x=TRUE, suffixes = suffixes)
      matches = matches [!is.na(matches[,'matches']),]
      matches[,'matches'] = NULL

    } else if (match_type=='fuzzyplus') {

      for(i in 1:2){

        ### presets
        stop = FALSE
        matches = NULL

        ### picking data
        lead = get(paste0('data',i))
        follow = get(paste0('data',((i-1.5)*-1)+1.5))

        while(stop == FALSE){

          ###fuzzymatch
          temp = do.call(stringdist::amatch, c(list(lead[,c(by.x,by.y)[i]],follow[,c(by.x,by.y)[((i-1.5)*-1)+1.5]]), amatch.args))
          temp = merge(cbind(lead, temp), cbind(follow,  index = seq(1,length(follow[,c(by.x,by.y)[((i-1.5)*-1)+1.5]]))), by.x='temp', by.y ='index', all.x=TRUE, suffixes = suffixes)
          temp = temp [!is.na(temp[,'temp']),]
          temp[,'temp'] = NULL

          if(is.null(matches)){
            matches=temp
          } else if(nrow(temp)==0){
            stop=TRUE
          } else {
            matches = rbind(matches, temp)
          }

          follow = follow[!is.element(follow[,get(paste0('unique_key_',((i-1.5)*-1)+1.5))],matches[,get(paste0('unique_key_matches_',((i-1.5)*-1)+1.5))]),]
        }

        assign(paste0('matches',i), matches)
      }

      matches = rbind(matches1, matches2)[!duplicated(rbind(matches1, matches2))]
    }

    ## checking matches
    if(nrow(matches)==0){
      message(paste('Merge returned no matches'))
      matches = NULL; matches_filter = NULL; data1_nomatch = data1; data2_nomatch = data2; match_evaluation = NULL
    } else {  # this bracket is closed after #Evaluate

    ## remove obs from data1 and data2 that are in the matched dataset
    data1_nomatch = data1[!is.element(apply(data1[unique_key_1], 1, paste, collapse="_"),apply(matches[unique_key_1_matches], 1, paste, collapse="_")),]
    data2_nomatch = data2[!is.element(apply(data2[unique_key_2], 1, paste, collapse="_"),apply(matches[unique_key_2_matches], 1, paste, collapse="_")),]


  # score
  if(!is.null(score_settings)){

    ## initializing score variable
    matches[,'matchscore']=0

    ## updating score
    for(j in 1:length(score_settings)){
      tryCatch({

        ## getting names for convienience
        variable     = names(score_settings)[j]
        for(i in 1:length(by.x)){
          if(any(variable %in% c(by.x[i],by.y[i])) & by.x[i]==by.y[i]){
            matches[,paste0(variable,suffixes[1])] = matches[,paste0(variable,suffixes[2])] = matches[,variable]}
        }
        compare_type = score_settings[[j]][['compare_type']]

        ## creating comparison variable
        if(compare_type=='in'        ){matches[,variable] = as.numeric(as.character(ifelse(stringr::str_detect(matches[,paste0(variable,suffixes[2])],matches[,paste0(variable,suffixes[1])]), 1,0)))}
        if(compare_type=='indicator' ){matches[,variable] = ifelse((matches[,paste0(variable,suffixes[1])]==matches[,paste0(variable,suffixes[2])]), 1,0)}
        if(compare_type=='substr'    ){matches[,variable] = 0; for(k in 1:max(nchar(matches[,paste0(variable,suffixes[1])]),na.rm=TRUE)){matches[,variable] = ifelse(substr(matches[,paste0(variable,suffixes[1])],1,k)==substr(matches[,paste0(variable,suffixes[2])],1,k) & nchar(matches[,paste0(variable,suffixes[1])])>=k & nchar(matches[,paste0(variable,suffixes[2])])>=k ,k,matches[,variable])}}
        if(compare_type=='difference'){matches[,variable] = pmin(abs(matches[,paste0(variable,suffixes[1])]-matches[,paste0(variable,suffixes[2])]),abs(matches[,paste0(variable,suffixes[2])]-matches[,paste0(variable,suffixes[1])]));}
        if(compare_type=='ratio'     ){matches[,variable] = pmax(matches[,paste0(variable,suffixes[2])]/matches[,paste0(variable,suffixes[1])],matches[,paste0(variable,suffixes[1])]/matches[,paste0(variable,suffixes[2])] )}
        if(compare_type=='stringdist'){matches[,variable] = stringdist::stringdist(matches[,paste0(variable,suffixes[1])],matches[,paste0(variable,suffixes[2])], method="jw",p=.02)}

        matches[,variable] = as.numeric(matches[,variable])

        ## updating score variable
        if('threshold' %in% names(score_settings)){
          thresholds = score_settings[[j]][['threshold']]
          for(i in (1:(length(thresholds)))){
            threshold = score_settings[[j]][['threshold']][length(scores)-i]
            newscore = c(score_settings[[j]][['score']],0)[[length(scores)-i]]
            oldscore = c(score_settings[[j]][['score']],0)[[length(scores)-i+1]]
            matches[,'matchscore'][which(matches[,variable] <= threshold)] = matches[,'matchscore'][which(matches[,variable] <= threshold)] + newscore - oldscore
          }
        } else {

          if(is.null(score_settings[[j]][['weight']])){
            weight = 1/length(score_settings)
          } else {
            weight = score_settings[[j]][['weight']]
          }

          if(compare_type=='in'        ){distance = matches[,variable]}
          if(compare_type=='indicator' ){distance = matches[,variable]}
          if(compare_type=='substr'    ){distance = matches[,variable]/pmin(nchar(matches[,paste0(variable,suffixes[1])]), nchar(matches[,paste0(variable,suffixes[2])]), na.rm=TRUE)}
          if(compare_type=='difference'){distance = 1/(1+(1/max(matches[,variable], na.rm=TRUE))*matches[,variable]^2)}
          if(compare_type=='ratio'     ){distance = 1/matches[,variable]}
          if(compare_type=='stringdist'){distance = 1-matches[,variable]}

          distance[is.na(distance)] = mean(distance, na.rm=TRUE)

          matches[,'matchscore'][!is.na(distance * weight)] = matches[,'matchscore'][!is.na(distance * weight)] +  (distance * weight)[!is.na(distance * weight)]
          matches[,'matchscore'][is.na(matches[,'matchscore'])] = 0
        }

      }, error=function(e){message(e); cat('\n')}, warning=function(w){message(w); cat('\n')})
    }
  }

  # Filter
  if (!is.null(filter)) {

    ## filter data
    if(is.function(filter)) {
      matches_filter = do.call(filter, c(list(matches), filter.args))}
    if(is.numeric(filter)){
      matches_filter = matches[which(matches[,'matchscore'] >= filter),]}

    ## redefine no matches to include only those in the filtered dataset
    data1_nomatch = data1[!is.element(apply(data1[unique_key_1], 1, paste, collapse="_"),apply(matches_filter[unique_key_1_matches], 1, paste, collapse="_")),]
    data2_nomatch = data2[!is.element(apply(data2[unique_key_2], 1, paste, collapse="_"),apply(matches_filter[unique_key_2_matches], 1, paste, collapse="_")),]

    ## checking results
    if(nrow(matches_filter)==0){
      message(paste('There are no matches after filtering'))
      matches_filter = NULL}

  } else { matches_filter = NULL }


  # Evaluate
  if(!is.null(evaluate)){
    match_evaluation = do.call(evaluate, c(list(matches, data1, data2, unique_key_1, unique_key_2, suffixes), evaluate.args))
    if(!is.null(filter)) {
      match_evaluation_filter = do.call(evaluate, c(list(matches_filter, data1, data2, unique_key_1_matches, unique_key_2_matches, suffixes), evaluate.args))
      match_evaluation = rbind(match_evaluation, match_evaluation_filter)
      match_evaluation[3,]=c('all',match_evaluation[1,-1]-match_evaluation[2,-1])
      match_evaluation = cbind(data.frame('match'=c('matches','matches_filter','difference'),match_evaluation))
    }
  } else {match_evaluation = NULL}

    } # closes the else from ##checking matches

  # Final sort
  if (!is.null(matches)) {
    matches = matches[,order(names(matches))] }

  # Return
  return(list("matches" = matches, 'matches_filter' = matches_filter, "data1_nomatch" = data1_nomatch, "data2_nomatch" = data2_nomatch, 'match_evaluation' = match_evaluation))
}
