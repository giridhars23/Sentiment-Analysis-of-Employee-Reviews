setwd('')
Sys.setenv("JAVA_HOME"="C:\\Program Files\\Java\\jre1.8.0_77")

library(tm)
library(qdap)
library(wordcloud)

abc <- read.csv("500_abc.csv")

xyz <- read.csv("500_xyz.csv")

#Removing missing values
abc_pros <- na.omit(abc$pros)
abc_cons <- na.omit(abc$cons)

xyz_pros <- na.omit(xyz$pros)
xyz_cons <- na.omit(xyz$cons)

# Collapsing all reviews
abc <- c(pros=paste(abc$pros,collapse=''),cons=paste(abc$cons,collapse=''))
xyz <- c(pros=paste(xyz$pros,collapse=''),cons=paste(xyz$cons,collapse=''))


#Sourcing Cleaning Functions.More Info inside files
source("qdap_clean.R")
source("tm_clean.R")

xyz <- qdap_clean(xyz)

xyz <- VCorpus(VectorSource(xyz))

xyz <- tm_clean(xyz)

# Alter abc_pros
abc_pros <- qdap_clean(abc_pros)

# Alter abc_cons
abc_cons <- qdap_clean(abc_cons)

# Create abc_p_corp 
abc_p_corp <- VCorpus(VectorSource(abc_pros))

# Create abc_c_corp
abc_c_corp <- VCorpus(VectorSource(abc_cons))

# Create abc_pros_corp
abc_pros_corp <- tm_clean(abc_p_corp)

# Create abc_cons_corp
abc_cons_corp <- tm_clean(abc_c_corp)


# Alter xyz_pros
xyz_pros <- qdap_clean(xyz_pros)

# Alter xyz_cons
xyz_cons <- qdap_clean(xyz_cons)

# Create xyz_p_corp 
xyz_p_corp <- VCorpus(VectorSource(xyz_pros))

# Create xyz_c_corp
xyz_c_corp <- VCorpus(VectorSource(xyz_cons))

# Create xyz_pros_corp
xyz_pros_corp <- tm_clean(xyz_p_corp)

# Create xyz_cons_corp
xyz_cons_corp <- tm_clean(xyz_c_corp)


library(RWeka)
tokenizer <- function(x) 
  NGramTokenizer(x, Weka_control(min = 2, max = 2))

xyz_tdm <- TermDocumentMatrix(xyz, control = list(tokenize=tokenizer))

abc_p_tdm <- TermDocumentMatrix(abc_pros_corp, control = list(tokenize = tokenizer))

abc_p_tdm_m <- as.matrix(abc_p_tdm)

abc_p_freq <- rowSums(abc_p_tdm_m)

wordcloud(names(abc_p_freq),abc_p_freq,max.words=25,color='blue',scale=c(4,.2))

# Create abc_c_tdm
abc_c_tdm <- TermDocumentMatrix(abc_cons_corp, control = list(tokenize = tokenizer))

# Create abc_c_tdm_m
abc_c_tdm_m <- as.matrix(abc_c_tdm)

# Create abc_c_freq
abc_c_freq <- rowSums(abc_c_tdm_m)

# Plot a wordcloud using abc_c_freq values
wordcloud(names(abc_c_freq),abc_c_freq,max.words=25,color='red',scale=c(4,.2))

# Create abc_c_tdm
abc_c_tdm <- TermDocumentMatrix(abc_cons_corp, control=list(tokenize = tokenizer))


# Create abc_c_tdm2 by removing sparse terms 
abc_c_tdm2 <- removeSparseTerms(abc_c_tdm,sparse=.993)

# Create hc as a cluster of distance values
hc <- hclust(dist(abc_c_tdm2, method="euclidean"), method="complete")

# Produce a plot of hc
plot(hc)

# Create abc_p_tdm
abc_p_tdm <-  TermDocumentMatrix(abc_pros_corp,control = list(tokenize= tokenizer))

# Create abc_p_m
abc_p_m <-  as.matrix(abc_p_tdm)

# Create abc_p_freq
abc_p_freq <- rowSums(abc_p_m)

# Create term_frequency
term_frequency <- sort(abc_p_freq, decreasing=TRUE)

# Print the 5 most common terms
term_frequency[1:5]

# Find associations with fast paced

findAssocs(abc_p_tdm , "fast paced", 0.2)

all_xyz_corpus <- xyz

# Create all_xyz_corp
all_xyz_corp <- tm_clean(all_xyz_corpus)

# Create all_tdm
all_tdm <- TermDocumentMatrix(all_xyz_corp)

# Name the columns of all_tdm
colnames(all_tdm) <- c("xyz_pros", "xyz_cons")

# Create all_m
all_m <- as.matrix(all_tdm)

# Build a comparison cloud
comparison.cloud(all_m,colors= c("#F44336", "#2196f3"), max.words=100,scale=c(2,2),title.size = 2)

abc_pros <- paste(abc_pros,collapse='')
xyz_pros <- paste(xyz_pros,collapse ='')

pros <- c(abc_pros,xyz_pros)

pros <-qdap_clean(pros)

pros <- VCorpus(VectorSource(pros))

pros <- tm_clean(pros)

all_tdm_corp <- TermDocumentMatrix(pros, control= list(tokenize = tokenizer))

all_tdm_m <- as.matrix(all_tdm_corp)

xyz_tdm_m <- all_tdm_m
  
# Create common_words
common_words <- subset(xyz_tdm_m, xyz_tdm_m[, 1] > 0 & xyz_tdm_m[, 2] > 0)

# Create difference
difference <- abs(common_words[,1]-common_words[,2])

# Add difference to common_words
common_words <- cbind(common_words,difference)

# Order the data frame from most differences to least
common_words <- common_words[order(common_words[, 3], decreasing = TRUE), ]

# Create top15_df
top25_df <- data.frame(x = common_words[1:25, 1], 
                       y = common_words[1:25, 2], 
                       labels = rownames(common_words[1:25, ]))
library(plotrix)
# Create the pyramid plot
pyramid.plot(top25_df$x, top25_df$y, 
                labels= top25_df$labels, gap = 12, 
                top.labels = c("abc", "Pro Words", "xyz"), 
                main = "Words in Common", unit = NULL)

abc_cons <- paste(abc_cons,collapse='')
xyz_cons <- paste(xyz_cons,collapse ='')

cons <- c(abc_cons,xyz_cons)

cons <- qdap_clean(cons)

cons <- VCorpus(VectorSource(cons))

cons <- tm_clean(cons)

all_tdm_corp_cons <- TermDocumentMatrix(cons, control= list(tokenize = tokenizer))

all_tdm_m_cons <- as.matrix(all_tdm_corp_cons)

# Create common_words
common_words <- subset(all_tdm_m_cons, all_tdm_m_cons[, 1] > 0 & all_tdm_m_cons[, 2] > 0)

# Create difference
difference <- abs(common_words[,1]-common_words[,2])

# Add difference to common_words
common_words <- cbind(common_words,difference)

# Order the data frame from most differences to least
common_words <- common_words[order(common_words[, 3], decreasing = TRUE), ]

# Create top15_df
top25_df <- data.frame(x = common_words[1:25, 1], 
                       y = common_words[1:25, 2], 
                       labels = rownames(common_words[1:25, ]))

# Create the pyramid plot
pyramid.plot(top25_df$x, top25_df$y, 
             labels= top25_df$labels, gap = 12, 
             top.labels = c("abc", "Con Words", "xyz"), 
             main = "Words in Common", unit = NULL) 

#####Sentiment Analysis#############

abc <- read.csv("500_abc.csv")

xyz <- read.csv("500_xyz.csv")

#Making data into a tidy format
tidy <- c(as.character(abc$pros),as.character(xyz$pros),as.character(abc$cons),as.character(xyz$cons))

tidyr <- data.frame(comp=NA,rev=NA,text=NA)
tidyr[c(1:500,1002:1501),1]="abc"
tidyr[c(501:1001,1502:2002),1]="xyz"
tidyr[1:1001,2]="pros"
tidyr[1002:2002,2]="cons"
tidyr$text <- tidy

library(tidytext)
library(dplyr)
library(ggplot2)

tidy_data <- tidyr %>%
    unnest_tokens(word,text) %>%
    anti_join(stop_words)
    
tv_sentiment <- tidy_data %>% 
  inner_join(get_sentiments("nrc"))

#Word contribution to each sentiment for company abc
tv_sentiment %>%
  filter(comp=="abc") %>%
  # Count by word and sentiment
  count(word,sentiment,sort=TRUE) %>%
  # Group by sentiment
  group_by(sentiment) %>%
  # Take the top words for each sentiment
  top_n(8,n) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x=word,y=n,fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ sentiment, scales = "free") +
  coord_flip()

#Word contribution to each sentiment for xyz
tv_sentiment %>%
  filter(comp=="xyz") %>%
  # Count by word and sentiment
  count(word,sentiment,sort=TRUE) %>%
  # Group by sentiment
  group_by(sentiment) %>%
  # Take the top 
  top_n(8,n) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x=word,y=n,fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ sentiment, scales = "free") +
  coord_flip()
#####Sentiment Analysis##########
