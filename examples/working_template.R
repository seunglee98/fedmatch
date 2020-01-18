#File Author: 	Melanie Friedrichs, Board of Governors of the Federal Reserve 
#Project: 		fedmatch example	
#Last edited: 	073117
#Run Time:		~5 minutes
#Notes:		This is a self-contained tutorial on the functionality of this package and template for your own matching program. 
#---------------------------
# Preliminaries
#---------------------------
setwd('YOU MUST ADD SOMETHING HERE, BUT YOU SHOULDNT NEED TO CHANGE ANYTHING ELSE') #setwd -- this should be the same directory you put the fedmatch folder in 
require(devtools)  #if devtools is not already installed you must install it 
devtools::load_all('fedmatch')  #load matching package  -- may be from different file 
rm(list = ls()); cat("\014")  #clear environment and console 

#---------------------------
# Load data
#---------------------------
data1 = read.csv('fedmatch/raw_data/data1.csv',na.strings=c("","NA"), stringsAsFactors = FALSE)   
data2 = read.csv('fedmatch/raw_data/data2.csv',na.strings=c("","NA"), stringsAsFactors=FALSE, fileEncoding = 'latin1') #may need latin1 if there are funny characters 
#verified = read.csv('datapath',na.strings=c("","NA"), stringsAsFactors = FALSE)  #if you have a verified set 


#---------------------------
# Clean data (Very important and usually the hardest part)
#---------------------------
#identify primary keys and make sure data is unique ont htem
data1[,'id'] = 1:10
data1 = data1[!duplicated(data1[,c('id')]),]
#rename and reformat fields you are going to use in score_Settings
data1[,'amount'] = as.numeric(data1[,'Revenue'])*1000000000
data1[,'state'] = data1[,'State'] 
data1[,'country'] = data1[,'Country']
data1[,'SIC'] = data1[,'SIC']
data1[,'name'] = data1[,'Company']
data1[,'cleanname'] = clean_strings(data1$Company, common_words = corporate_words)
data1 = data1[,c('id','name','cleanname','country','state','SIC','amount')]

#identify primary keys and make sure data is unique ont htem
data2[,'id'] = 1:10
data2 = data2[!duplicated(data2[,c('id')]),]
#rename and reformat fields you are going to use in score_Settings
data2[,'amount'] = as.numeric(gsub(',','',data2[,'earnings']))*1000000
data2[,'state'] = data2[,'state_code']
data2[,'country'] = data2[,'country']
data2[,'SIC'] = data2[,'SIC_code']
data2[,'name'] = data2[,'Name']
data2[,'cleanname'] = clean_strings(data2$Name, common_words = corporate_words)
data2 = data2[,c('id','name','cleanname','country','state','SIC','amount')]

#---------------------------
# Create settings for matchscore
#---------------------------
##if you have a verified match do this to calculate weights using record linkage methodology 
#temp = merge(data1, verified[,c('id1','id2')], by.x='id', by.y='id1')

#verified_match = merge(temp, data2, by.x='id2', by.y='id' suffixes =c('_1','_2'))
#weights = calculate_weights(data=verified_match, variables=c('cleanname','country', 'state','SIC'), compare_types=c('stringdist','indicator','indicator','substr'), suffixes=c('_1','_2'))
#settings = weights$settings


#if no verified match available make up your weights like this. Note that "compare_type" is the distance metric, which should vary by data type. 
settings = list(
  'cleanname' =   list('compare_type'='stringdist',    'weight' = .4)
  ,'country' = list('compare_type'='indicator', 'weight' = .1)
  ,'state'   = list('compare_type'='indicator', 'weight' = .2)
  ,'SIC'     = list('compare_type'='substr',    'weight' = .3))


#---------------------------
# Figure out words to remove during string cleaning
#---------------------------
data1_words = word_frequency(data1$cleanname)
data2_words = word_frequency(data2$cleanname)

#words = rbind(data1_words[1:10,1], data2_words[1:10,1])  #takes the 10 most common words -- you should not use this line of code persay but look at the results and do what makes sense 
words = c('company','corp','inc')

#---------------------------
# Match
#---------------------------
#use mege_plus to do a simple merge (can specify fuzzy or exact, can remove words, etc )
simple = merge_plus(data1=data1, data2=data2, unique_key_1='id', unique_key_2='id',by='name',score_settings = settings , match_type='fuzzy')

#use tier match to loop over merge_plus versions 
tiered = tier_match(data1=data1, data2=data2, suffixes =c( "_1", "_2"), unique_key_1='id', unique_key_2='id',
                    tiers = list(  'exact' = list(by.x='name', by.y='name'),
                                   'scrub' = list(by.x='name', by.y='name', clean.args=list(common_words = words, stem=TRUE) ),
                                   'fuzzy' = list(by.x='name', by.y='name', match_type='fuzzy' ),                    
                                   'stateSIC' = list(by.x=c('state','SIC'), by.y=c('state','SIC'), match_type='exact', filter=.5)
                    ),
                    takeout = 'neither', score_settings = settings)


#examine results 
tiered$matches     #matches 
tiered$match_evaluation      #matching rate      
tiered$data1 #unmatched data1
tiered$data2 #unmatched data2

#if takeout = 'neither' you will want to dedupe on match id 
matches = tiered$matches[!duplicated(paste(tiered$matches$id_1,tiered$matches$id_2,sep='_')),]   

#you can also use match_evaluate to calculate percentages by some other variable, like amount (see aggregate_by argument)
match_evaluation = match_evaluate(matches=matches, data1=data1,data2=data2,unique_key_1='id', unique_key_2='id',suffixes =c( "_1", "_2"), tier='tier', aggregate_by=c('unique','amount'))

#just to show you how calculate weights works (since I didn't supply a verfied set)
weights = calculate_weights(data=matches, variables=c('cleanname','country', 'state','SIC'), compare_types=c('stringdist','indicator','indicator','substr'), suffixes=c('_1','_2'))
weights$m  #probability variable is the same if two observations are a true match 
weights$u #probability that variable is the same two observations are NOT a true match (random correctness rate)
weights$w #combination of m and u probabilities (compare to the manually chosen settings weights above)
weights$settings #list object to read into tier_match

#---------------------------
# Exporting
#---------------------------
# you will want to export both your matches and your evaluation statistics for further use
# 
# write.csv(matches,file='matches.csv',row.names=FALSE,na='')
# write.csv(matche_evaluation,file='match_evalute.csv',row.names=FALSE,na='')
