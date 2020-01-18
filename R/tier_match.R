#' tier_match
#'
#' @description Constructs a tier_match by running merge_plus with different parameters sequentially on the same data, removing matched observations after each tier
#'
#' @param data1 data.frame. First to-merge dataset.
#' @param data2 data.frame. Second to-merge dataset.
#' @param suffixes see \code{merge}
#' @param check_merge logical. Checks that your unique_keys are indeed unique, and prevents merge from running if merge would result in data.frames larger than 5 million rows
#' @param unique_key_1 character vector. Primary key of data1 that uniquely identifies each row (can be multiple fields)
#' @param unique_key_2 character vector. Primary key of data2 that uniquely identifies each row (can be multiple fields)
#' @param tiers list(). tier is a list of lists, where each list holds the parameters for creating that tier. All arguments to tier_match listed after this argument can either be supplied directly to tier_match, or indirectly via tiers.
#' @param takeout string. Specifies whether to exclude matched observations from matching in subsequent tiers for 'data1', 'data2' or 'both'.
#'         of matching. If
#' @param match_type. string. If 'exact', match is exact, if 'fuzzy', match is fuzzy.
#' @param amatch.args. additional arguments for amatch, to be used if match_type = 'fuzzy'. Suggested defaults provided. (see amatch, method='jw')
#' @param clean Function to clean strings prior to match. see \code{clean_strings}.
#' @param clean.args list. Arguments passed to clean.
#' @param score_settings list. score settings. See vingette matchscore
#' @param filter function or numeric. Filters a merged data1-data2 dataset. If a function, should take in
#'       a data.frame (data1 and data2 merged by name1 and name2) and spit out a trimmed verion
#'       of the data.frame (fewer rows). Think of this function as applying other conditions
#'       to matches, other than a match by name. The first argument of filter should be the data.frame.
#'       If numeric, will drop all observations with a matchscore lower than or equal to filter.
#' @param filter.args list. Arguments passed to filter, if a function
#' @param evaluate Function to evalute merge_plus output. see \code{evaluate_match}.
#' @param evaluate.args list. Arguments passed to function specified by evaluate
#'
#' @return list with matches, data1 and data2 minus matches, and match evaluation
#'
#' @export
#'
#' @seealso merge_plus clean_strings
#'
#' @examples
#' data1 = data.frame(unique_key = 1, name = c("hello world"))
#' data2 = data.frame(unique_key = 1:3, name = c("hello world", "Hello World", "hello worlds"))
#' tier_match(data1, data2, by='name', unique_key_1='unique_key', unique_key_2='unique_key',
#'           tiers = list(a=list(clean='none'), b=list(), c=list(match_type='fuzzy')), takeout='data2')
#'
#'
tier_match <- function(data1, data2, by=NULL, by.x=by, by.y=by, suffixes=c(".x",".y"),
                       check_merge=TRUE, unique_key_1, unique_key_2,
                       tiers = list(), takeout='both',
                       match_type = 'exact', amatch.args=list(method='jw', p=0.1, maxDist=0.05, matchNA=FALSE),
                       clean = clean_strings, clean.args=list(),
                       score_settings=NULL, filter=NULL, filter.args=list(),
                       evaluate = match_evaluate, evaluate.args=list()
                      ) {

  # preliminaries
  matches = NULL
  data1_save = data1
  data2_save = data2

  # expanding tiers
  for(tier in names(tiers)){
    if(!is.null(tiers[[tier]][['sequential_words']]) ){
      subtiers = list()
      for(j in 1:nrow(tiers[[tier]][['sequential_words']])){
        subtiers[[paste(names(tiers[tier]),j,sep='.')]] = tiers[[tier]]
        subtiers[[paste(names(tiers[tier]),j,sep='.')]][['clean.args']][['common_words']] = rbind(subtiers[[paste(names(tiers[tier]),j,sep='.')]][['clean.args']][['common_words']], subtiers[[paste(names(tiers[tier]),j,sep='.')]][['sequential_words']][1:j,])
      }
      tiers = c(tiers[1:(grep(tier, names(tiers))-1)], subtiers, tiers[(grep(tier, names(tiers))+1):length(tiers)])
    }
  }


  # creating tiers
  for(i in 1:length(tiers)){
    tryCatch({

      ## parsing tier parameters

        ### name tier
        if(is.null(names(tiers)[i])){
          names(tiers)[i]=i}
        cat('Tier ',i,': ',names(tiers)[i],'\n')

        ### define tier
        tier = tiers[[i]]

        ### assign tier parameters

          #### by.x and by.y equal to by if not specifically set in tier
          for(param in c('by.x','by.y')){
            if(is.null(tier[[param]])){
              tier[[param]] = tier[['by']]}}

          #### params equal to default param if not specifically set in tier
          for(param in c('by.x','by.y','check_merge',  'match_type','amatch.args','score_settings', 'filter','filter.args','evaluate','evaluate.args', 'clean', 'clean.args')){
            tryCatch({
              if(is.null(tier[[param]])){
                if(!is.null(get(param))){
                  tier[[param]] = get(param)
                } else {
                  tier[[param]] = NULL
                }
              }
            }, error=function(e){message(e); cat('\n')}, warning=function(w){message(w); cat('\n')})
          }


      ## creating merge variables and cleaning strings according to tier parameters
      for(j in 1:2){
        variable = c('by.x','by.y')[j]
        data = get(paste0('data',j))
        data[,variable] = apply(data[,tier[[variable]], drop=FALSE], 1, paste, collapse="_")
        for(k in 1:length(tier[[variable]])){data[,variable][which(is.na(data[,tier[[variable]][k]]))]=NA}
        if(class(tier[['clean']])=='function'){data[,variable][which(!is.na(data[,variable]))] = do.call(tier[['clean']], c(list(string=data[,variable][which(!is.na(data[,variable]))]),tier[['clean.args']]))}
        assign(paste0('data',j), data)
      }

      temp = tier[['filter']]

      ## creating tier according to tier parameters
      tier = merge_plus(data1, data2, by.x='by.x', by.y='by.y', suffixes=suffixes,
                        unique_key_1=unique_key_1, unique_key_2=unique_key_2, check_merge=tier[['check_merge']],
                        match_type=tier[['match_type']],  amatch.args = tier[['amatch.args']],
                        score_settings=tier[['score_settings']],
                        filter=tier[['filter']], filter.args=tier[['filter.args']],
                        evaluate=NULL, evaluate.args=list())


      ## assigning output
      if(is.null(temp)){newmatches = tier[['matches']]} else { newmatches = tier[['matches_filter']]}
      if(!is.null(newmatches)){
        if(!('by.y' %in% names(newmatches))){
          newmatches[,'by.y'] = newmatches[,'by.x']}
        if(is.null(matches)){
          matches = cbind(tier=rep(names(tiers)[i], nrow(newmatches)), newmatches)
        } else {
          matches = rbind(matches, cbind(tier=rep(names(tiers)[i], nrow(newmatches)),newmatches))
        }
      }
      if(takeout %in% c('data2','both')){data2 = tier[['data2_nomatch']]}
      if(takeout %in% c('data1','both')){data1 = tier[['data1_nomatch']]}



    }, error=function(e){message(e); cat('\n')})
  }


  ## fixing unique_keys
  unique_key_1_matches = unique_key_1
  unique_key_2_matches = unique_key_2
  for(i in 1:pmax(length(unique_key_1),length(unique_key_2))){
    if(unique_key_1[i] %in% names(data2)){
      unique_key_1_matches[i] = paste0(unique_key_1[i],suffixes[1])}
    if(unique_key_2[i] %in% names(data1)){
      unique_key_2_matches[i] = paste0(unique_key_2[i],suffixes[2])}
  }

  ## evaluating
  if(is.null(matches)){
    match_evaluation = NULL
  } else {
    if(!is.null(evaluate)){
      match_evaluation = do.call(evaluate, c(list(matches=matches, data1=data1_save, data2=data2_save, unique_key_1=unique_key_1, unique_key_2=unique_key_2, suffixes=suffixes, tier='tier', dupe_ratio=TRUE), evaluate.args))
    } else {
      match_evaluation = NULL
    }
  }

  ## nomatch data
  if(is.null(matches)){
    data1_nomatch = data1
    data2_nomatch=data2
  } else {
    data1_nomatch = data1[!is.element(apply(data1[unique_key_1], 1, paste, collapse="_"),apply(matches[unique_key_1_matches], 1, paste, collapse="_")),]
    data2_nomatch = data2[!is.element(apply(data2[unique_key_2], 1, paste, collapse="_"),apply(matches[unique_key_2_matches], 1, paste, collapse="_")),]
  }


  # return
  return(list("matches" = matches, "data1_nomatch" = data1_nomatch, "data2_nomatch" = data2_nomatch, 'match_evaluation' = match_evaluation))

}

