############load###############
library('data.table')
library(dplyr)
setwd('C:\\Users\\dcw10\\OneDrive\\文件\\data_mining_期末')
getwd()
lenders=fread('./csv/lenders.csv',nrows=358120)
loans=fread('./csv/loans.csv',nrows=161980)
sloan2=fread('./csv/sloan2.csv')
sloan_with_gdp=fread('./csv/sloan100.csv')
courtry_stat=fread('./kaggle上的kiva資料/country_stats.csv')
reviews=lenders$LOAN_BECAUSE[lenders$LOAN_BECAUSE!='']

colnames(sloan_with_gdp)=c(unlist(sloan_with_gdp[1,]))

#sloan_with_gdp=sloan_with_gdp[-1,]
sloan_with_gdp$LOAN_AMOUNT=sloan_with_gdp$LOAN_AMOUNT %>% as.numeric()
names(sloan_with_gdp)[35]='2015_gdp'
sloan_with_gdp$`2015_gdp`=sloan_with_gdp$`2015_gdp` %>% as.numeric()

class(sloan_with_gdp$`2015_gdp`)
##################資料分析#########################

library(ggplot2)
ggplot(data=sloan_with_gdp,aes(x=SECTOR_NAME,y=log(LOAN_AMOUNT,10),fill=STATUS))+
  geom_boxplot()



##################world visualization###################################
sloan_with_gdp %>% 
  group_by(COUNTRY_NAME) %>% 
  summarise(
    loan_num = length(LOAN_AMOUNT),
    funded_num=length(STATUS=='funded'),
    loan_avg=mean(LOAN_AMOUNT,rm='T'),
    fund_ratio=length(STATUS)/length(STATUS=='funded')
    
    )%>% 
  ungroup() -> loan_all
# light grey boundaries
l <- list(color = "grey", width = 0.5)
##merge some country stat...
loan_all=merge(loan_all,unique(sloan_with_gdp[,c('COUNTRY_NAME','2015_gdp')]),by.x='COUNTRY_NAME')
loan_all$gdp_rank=rank(-loan_all$`2015_gdp`, na.last = TRUE)
loan_all=merge(loan_all,unique(courtry_stat[,c('kiva_country_name','life_expectancy')]),by.x='COUNTRY_NAME',by.y='kiva_country_name')
loan_all=merge(loan_all,unique(courtry_stat[,c('kiva_country_name','population_below_poverty_line')]),by.x='COUNTRY_NAME',by.y='kiva_country_name')
loan_all=merge(loan_all,unique(courtry_stat[,c('kiva_country_name','mean_years_of_schooling')]),by.x='COUNTRY_NAME',by.y='kiva_country_name')
loan_all=merge(loan_all,unique(courtry_stat[,c('kiva_country_name','expected_years_of_schooling')]),by.x='COUNTRY_NAME',by.y='kiva_country_name')




loan_all$hover <- with(loan_all, 
                       paste('Country; ', COUNTRY_NAME, '<br>',
                             'Number of funded: ',funded_num, '<br>',
                             'GDP:',`2015_gdp`, '<br>',
                             'GDP_rank:',gdp_rank
                       ))
# specify map projection/options
g <- list(
  showframe = TRUE,
  showcoastlines = TRUE,
  projection = list(type = 'Mercator')
)
#install.packages("plotly")
library('plotly')
plot_geo(loan_all, locationmode = 'country names') %>%
  add_trace(
    z = ~funded_num, color = ~funded_num, colors = 'Spectral',
    text = ~hover, locations=~COUNTRY_NAME, marker = list(line = l)
  ) %>%
  colorbar(title = 'funded_num', tickprefix = '') %>%
  layout(
    title = 'Loans per country',
    geo = g
  )



#################loans per sector and activity##################
#install.packages('treemap')
library(treemap)
loans %>% group_by(SECTOR_NAME, ACTIVITY_NAME) %>% summarise(nr = length(COUNTRY_NAME)) %>% top_n(100,wt=nr) %>% ungroup() %>%
  treemap(
    index=c("SECTOR_NAME","ACTIVITY_NAME"), 
    type="value",
    vSize = "nr",  
    vColor = "nr",
    palette = "RdBu",  
    title=sprintf("Loans per sector and activity"), 
    title.legend = "Number of loans",
    fontsize.title = 14 
  )

##################Number Gender:#########################
#sloan_with_gdp$BORROWER_GENDERS[which(sloan_with_gdp$BORROWER_GENDERS !='female' & sloan_with_gdp$BORROWER_GENDERS!='male' )]='group'

sloan_with_gdp$BORROWER_GENDERS=sloan_with_gdp$BORROWER_GENDERS %>% as.character() 
sloan_with_gdp$BORROWER_GENDERS[which(sloan_with_gdp$BORROWER_GENDERS %>% is.na())]='group'
#


sloan_with_gdp %>% group_by(BORROWER_GENDERS, REPAYMENT_INTERVAL) %>% summarise(nr = length(STATUS=='funded')) %>% ungroup() %>%
  ggplot(aes(x = reorder(BORROWER_GENDERS,nr), y = nr, fill=REPAYMENT_INTERVAL)) +
  geom_bar(stat="identity", aes(fill=REPAYMENT_INTERVAL)) + 
  theme_bw(base_size = 12)  +
  labs(title="", x ="Borrower gender", y = "Number of loans (thousands)", fill="Repayment interval",
       main="Repayment interval")
#############corrplot:#####################
library(corrplot)
loan_all$loan_num=loan_all$loan_num %>% as.numeric()
loan_all$funded_num=loan_all$funded_num %>% as.numeric()
cr = cor(loan_all[sapply(loan_all, class) == "numeric"], use="pairwise.complete.obs")
corrplot(cr, method = "ellipse", tl.cex=0.7)
#############LDA###################
#devtools::install_github("cpsievert/LDAvisData")
#data(reviews, package = "LDAvisData")
library(readr)
library(dplyr)


reviews=lenders$LOAN_BECAUSE[lenders$LOAN_BECAUSE!='']
loanbecause=reviews
#loanbecause=loanbecause[-2]
as.data.frame(loanbecause ) %>% View
#reviews=reviews[-2]

# read in some stopwords:
library(tm)
stop_words <- stopwords("SMART")
stop_words
reviews <- as.list(t(reviews))
# pre-processing:
reviews <- gsub("'", "", reviews)  # remove apostrophes
reviews <- gsub("[[:punct:]]", " ", reviews)  # replace punctuation with space
reviews <- gsub("[[:cntrl:]]", " ", reviews)  # replace control characters with space
reviews <- gsub("[[:digit:]]"," ", reviews) # remove number
reviews <- gsub("^[[:space:]]+", "", reviews) # remove whitespace at beginning of documents
reviews <- gsub("[[:space:]]+$", "", reviews) # remove whitespace at end of documents

#install.packages('stringr')
library(stringr)
reviews <- tolower(str_trim(reviews))  # force to lowercase
# tokenize on space and output as a list:
doc.list <- strsplit(reviews, "[[:space:]]+")
length(reviews)
# compute the table of terms:
term.table <- table(unlist(doc.list))
term.table <- sort(term.table, decreasing = TRUE)
# remove terms that are stop words or occur fewer than 5 times:

del <- (names(term.table) %in% stop_words | term.table < 5)
term.table <- term.table[!del]
vocab <- names(term.table)


# now put the documents into the format required by the lda package:
get.terms <- function(x) {
  index <- match(x, vocab)
  index <- index[!is.na(index)]
  rbind(as.integer(index - 1), as.integer(rep(1, length(index))))
}
documents <- lapply(doc.list, get.terms)

# Compute some statistics related to the data set:
D <- length(documents)  # number of documents (2,000)
W <- length(vocab)  # number of terms in the vocab (14,568)
doc.length <- sapply(documents, function(x) sum(x[2, ]))  # number of tokens per document [312, 288, 170, 436, 291, ...]
N <- sum(doc.length)  # total number of tokens in the data (546,827)
term.frequency <- as.integer(term.table)  # frequencies of terms in the corpus [8939, 5544, 2411, 2410, 2143, ...]


# MCMC and model tuning parameters:
K <- 20
G <- 5000
alpha <- 0.02
eta <- 0.02

# Fit the model:
library(lda)
set.seed(357)
t1 <- Sys.time()
fit <- lda.collapsed.gibbs.sampler(documents = documents, K = K, vocab = vocab,
                                   num.iterations = G, alpha = alpha,
                                   eta = eta, initial = NULL, burnin = 0,
                                   compute.log.likelihood = TRUE)
t2 <- Sys.time()
t2 - t1  # about 24 minutes on laptop


theta <- t(apply(fit$document_sums + alpha, 2, function(x) x/sum(x)))
phi <- t(apply(t(fit$topics) + eta, 2, function(x) x/sum(x)))

MovieReviews <- list(phi = phi,
                     theta = theta,
                     doc.length = doc.length,
                     vocab = vocab,
                     term.frequency = term.frequency)

#install.packages('LDAvis')
library(LDAvis)
# create the JSON object to feed the visualization:
json <- createJSON(phi = MovieReviews$phi, 
                   theta = MovieReviews$theta, 
                   doc.length = MovieReviews$doc.length, 
                   vocab = MovieReviews$vocab, 
                   term.frequency = MovieReviews$term.frequency)

# install.packages('servr')
serVis(json, out.dir = 'vis', open.browser = TRUE)
getwd()
save.image(file='demo.Rdata')
load('demo.RData')
