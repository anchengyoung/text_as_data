library(tidyverse)
library(quanteda)
library(stm)
library(topicmodels)

setwd("Documents/atNYU/Grad/2022 Spring/Text As Data/Homework/Homework_3")
vaccination <- read_csv("vaccination_all_tweets.csv", show_col_types = FALSE)

vaccination <- vaccination %>%
  mutate(date = as.Date(date, "%m/%d/%Y"),
         hashtag = ifelse(str_detect(hashtags, "(?i)PfizerBioNTech"),"PfizerBioNTech",
                          ifelse(str_detect(hashtags, "(?i)Covaxin"),"Covaxin", NA)))

vax_tweet <- vaccination %>%
  filter(date >= "2021-01-01", date <= "2021-04-30",
         hashtag %in% c("PfizerBioNTech","Covaxin"))

vax_count <- vax_tweet %>%
  group_by(date, hashtag) %>%
  summarize(count = n())

ggplot(vax_count, aes(x = date, y = count,
                      group = hashtag, color = hashtag)) +
  geom_line() + xlab("Date") + ylab("Tweet Count")

## -----------------------------------------------------------------------------------------------

vax_tweet <- vax_tweet %>%
  mutate(binary = ifelse(hashtag == "PfizerBioNTech", 1, 0))

processed <- textProcessor(vax_tweet$text, metadata=vax_tweet)
out <- prepDocuments(processed$documents, processed$vocab, processed$meta, lower.thresh = 10)

#system.time(stm_fit <- stm(out$documents, out$vocab, K = 10,
#                           prevalence = ~binary + s(date),
#                           content = ~binary, data = out$meta))

#saveRDS(stm_fit, "stm_fit.rds")
stm_fit <- readRDS("stm_fit.rds")
stm_fit$convergence$its

## Completing Iteration 79 (approx. per word bound = -5.366, relative change = 1.752e-05) 

plot(stm_fit, type = "summary")

#system.time(stm_fit_2 <- stm(out$documents, out$vocab, K = 20,
#                           prevalence = ~binary + s(date),
#                           content = ~binary, data = out$meta))

#saveRDS(stm_fit_2, "stm_fit_2.rds")
stm_fit_2 <- readRDS("stm_fit_2.rds")
stm_fit_2$convergence$its

## Completing Iteration 3 (approx. per word bound = -5.429, relative change = 1.646e-02)

plot(stm_fit_2, type = "summary")

plot(stm_fit, type="perspectives", topics = 5)
plot(stm_fit_2, type="perspectives", topics = 8)

out$meta$date <- as.numeric(out$meta$date)
prep <- estimateEffect(c(5) ~ binary + s(date) , stm_fit, meta = out$meta)
plot(prep, covariate = "binary", model = stm_fit, topics = 5, 
     method = "continuous", xaxt = "n", xlab = "Date")

prep <- estimateEffect(c(8) ~ binary + s(date) , stm_fit_2, meta = out$meta)
plot(prep, covariate = "binary", model = stm_fit_2, topics = 8, 
     method = "continuous", xaxt = "n", xlab = "Date")

plot(prep, covariate = "binary", model = stm_fit, topics = c(6,9,7,5,2),
     method = "difference", cov.value1 = 1, cov.value2 = 0)

## ------------------

vax_corpus <- corpus(vax_tweet$text)

tokenized <- tokens(vax_tweet$text, remove_punct = TRUE)
no_stop <- tokens_remove(tokenized, pattern = stopwords("en"))
stemmed <- tokens_wordstem(no_stop)
vax_dfm <- dfm(stemmed)
vax_dfm <- dfm_trim(vax_dfm, min_docfreq=10, docfreq_type="count")

system.time(stm_fit <- stm(out$documents, out$vocab, K = 10,
                           prevalence = ~binary + s(date),
                           content =~ binary, data = vax_tweet))

summary(vax_tweet)

vax_stm <- convert(vax_dfm, to = "stm", docvars = docvars(vax_corpus))

docvars(vax_corpus)

system.time(stm_fit <- stm(vax_stm$documents, vax_stm$vocab, K = 10,
                           prevalence = ~binary + s(date),
                           content =~ binary, data = docvars(vax_corpus)))

