#----------------------------------------------
# Examples for the various fedmatch functions
#----------------------------------------------

rm(list=ls())

setwd("~/fedmatch") 
source("./R/word_frequency.R")
source("./R/clean_string.R")

#---------------------
# word_frequency.R
#---------------------

## example 1 with simple vector
names = c("company name 1", "company name 2", "company 3")
word_frequency(names)

## example 2 with dataframe
col1 = c(1,2,3)
df1 = cbind(col1, names)
word_frequency(df1[,2])

# more complicated example
names = c("company name 1", "company &    3 inc.", "co nm 1,, lp")
df2 = cbind(col1, names)
word_frequency(df2[,2])

#------------------------
# clean_string.R
#------------------------

# basic cleaning example
sample_str = c("Holding Co. A @ B St, 3 ))", "Company B & C 4, Inc.", "make $, inc.")
sample_clean1 = clean_strings(sample_str)

# substituting common words with a database
common_words_db = read.csv("./data/common_abbr.csv")
sample_clean2 = clean_strings(sample_str, common_words=common_words_db)

# dropping common words in a database
sample_clean3 = clean_strings(sample_str, common_words=common_words_db, remove_words=TRUE)

# substituting common words without a database
common_words_db = cbind(c("co", "inc"), c("company", "incorporate"))
sample_clean4 = clean_strings(sample_str, common_words=common_words_db)

# sunco example
str = "sunco co"
sample_clean5 = clean_strings(str, common_words = cbind(c("co"), c("company")))

# changing special characters to words
## Note that @ and & are dropped with punctuation
drop_char = cbind(c("\\$", "\\%"), c("dollar", "percent"))
sample_clean6 = clean_strings(sample_str, sp_char_words = drop_char)

# combining fundparaphenalia, corporateparaphenalia & common_words
## read databases
corp = read.csv("./data/corporateparaphernalia.csv", stringsAsFactors=F)
fund = read.csv("./data/fundparaphernalia.csv", stringsAsFactors=F)
cw = read.csv("./data/common_abbr.csv", stringsAsFactors=F)
names(cw) = c("Word", "Replace")

##combine databases
common_words_db = rbind(corp, fund, cw)

# drop everything in corp, fund, and cw
sample_clean6= clean_strings(sample_str, common_words=common_words_db, remove_words=TRUE)
