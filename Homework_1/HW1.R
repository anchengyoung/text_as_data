library(tidyverse)
library(quanteda)
library(quanteda.corpora)
library(quanteda.textstats)
library(quanteda.textplots)

## Q1
speeches <- corpus_subset(data_corpus_inaugural, President == "Reagan")

# (a)
tokenized <- tokens(speeches, remove_punct = TRUE)
TTR_calculator <- function(tokens_item) {
  corpused <- sapply(tokens_item, paste, collapse=" ") %>% corpus()
  corpus_info <- summary(corpused)
  corpus_info %>%
    mutate(TTR = Types / Tokens) %>%
    select(Text, TTR)
}
TTR_calculator(tokenized)

# (b)
reagan_dfm <- dfm(tokenized, tolower = FALSE)
textstat_simil(reagan_dfm, margin = "documents", method = "cosine")

## Q2

# (a)
stemmed_token <- tokens_wordstem(tokenized)
TTR_calculator(stemmed_token)

stemmed_dfm <- dfm(stemmed_token, tolower = FALSE)
textstat_simil(stemmed_dfm, margin = "documents", method = "cosine")

# (b)
nostop_token <- tokens_remove(tokenized, pattern = stopwords("en"))
TTR_calculator(nostop_token)

nostop_dfm <- dfm(nostop_token, tolower = FALSE)
textstat_simil(nostop_dfm, margin = "documents", method = "cosine")

# (c)
lower_token <- tokens_tolower(tokenized)
TTR_calculator(lower_token)

lower_dfm <- dfm(lower_token, tolower = TRUE)
textstat_simil(lower_dfm, margin = "documents", method = "cosine")

# (d)
topfeatures(dfm_tfidf(reagan_dfm))
topfeatures(dfm_tfidf(stemmed_dfm))
topfeatures(dfm_tfidf(nostop_dfm))
topfeatures(dfm_tfidf(lower_dfm))

## Q3

headlines <- c(headline1 = "Nasa Mars rover: Perseverance robot all set for big test.",
               headline2 = "NASA Lands Its Perseverance Rover on Mars.")
head_token <- tokens_tolower(tokens(headlines, remove_punct = TRUE))
head_dfm <- dfm(head_token)

# (a)
euc_dist <- sqrt(sum((head_dfm[1,] - head_dfm[2,])^2))

# (b)
man_dist <- sum(abs(head_dfm[1,] - head_dfm[2,]))

# (c)
product <- sum(head_dfm[1,] * head_dfm[2,])
norm_1 <- sqrt(sum(head_dfm[1,]^2))
norm_2 <- sqrt(sum(head_dfm[2,]^2))

product / (norm_1 * norm_2)

# (d)
"The Levenshtein distance is 3"

## Q4
library(gutenbergr)
library(stylest)

n <- gutenberg_authors[,]
author_list <- c("Poe, Edgar Allan", "Twain, Mark",
                 "Shelley, Mary Wollstonecraft","Doyle, Arthur Conan")
book_list <- c(932,1064,1065,32037,74,76,86,91,84,6447,15238,18247,108,126,139,244)

prepare_dt <- function(book_list, num_lines, removePunct = TRUE){
     meta <- gutenberg_works(gutenberg_id == book_list)
     meta <- meta %>% mutate(author = unlist(str_split(author, ","))[1]
     %>% tolower(.))
     texts <- lapply(book_list, function(x) gutenberg_download(x,
     mirror="http://mirrors.xmission.com/gutenberg/") %>%
                       #select(text) %>%
                       sample_n(500, replace=TRUE) %>%
                       unlist() %>%
                       paste(., collapse = " ") %>%
                       str_replace_all(., "^ +| +$|( ) +", "\\1"))
     # remove apostrophes
     texts <- lapply(texts, function(x) gsub("‘|’", "", x))
     if(removePunct) texts <- lapply(texts, function(x)
     gsub("[^[:alpha:]]", " ", x))
     # remove all non-alpha characters
     output <- tibble(title = meta$title, author = meta$author, text =
     unlist(texts, recursive = FALSE))
}
# run function
set.seed(1984L)

# (a)


# (b)
texts_dt <- lapply(book_list, prepare_dt, num_lines = 500, removePunct = TRUE)
texts_dt <- do.call(rbind, texts_dt)
str(texts_dt)

# (c)
stopwords_en <- stopwords("en")
filter <- corpus::text_filter(drop_punct = TRUE, drop_number = TRUE, drop = stopwords_en)

vocab_terms <- stylest_select_vocab(texts_dt$text, texts_dt$author,
                                    filter = filter, smooth = 1, nfold = 5,
                                    cutoff_pcts = c(25, 50, 75, 99))
vocab_terms$cutoff_pct_best
vocab_terms$miss_pct

# (d)
vocab_subset <- stylest_terms(texts_dt$text, texts_dt$author,
                              vocab_terms$cutoff_pct_best , filter = filter)
style_model <- stylest_fit(texts_dt$text, texts_dt$author, terms = vocab_subset, filter = filter)

authors <- unique(texts_dt$author)
term_usage <- style_model$rate
lapply(authors, function(x) head(term_usage[x,][order(-term_usage[x,])], 5)) %>%
  setNames(authors)

# (e)
test <- data.frame(term_usage) %>% filter(rownames(term_usage) %in% c('twain','doyle'))

rate_ratio <- term_usage['twain',] / term_usage['doyle',]
head(rate_ratio[order(-rate_ratio)], 5)

# (f)
mys_file <- "Documents/atNYU/Grad/2022 Spring/Text As Data/Homework/mystery_excerpt.rds"
mystery_excerpt <- readRDS(mys_file)

pred <- stylest_predict(style_model, mystery_excerpt)
pred$predicted
pred$log_probs

# (g)
texts_tokens <- tokens(texts_dt$text) %>% setNames(texts_dt$title)

texts_lambda <- textstat_collocations(texts_tokens, min_count = 5) %>% arrange(desc(lambda))
head(texts_lambda, 10)

texts_count <- textstat_collocations(texts_tokens, min_count = 5) %>% arrange(desc(count))
head(texts_count, 10)

## Q5
library("sophistication")
data(data_corpus_ungd2017, package = "quanteda.corpora")

# (a)
snippetData <- snippets_make(data_corpus_ungd2017, nsentence = 1, minchar = 150, maxchar = 350)
snippetData <- snippets_clean(snippetData)
head(snippetData, 10)

# (b)
testData <- sample_n(snippetData, 1000)
snippetPairsMST <- pairs_regular_make(testData)

gold_questions <- pairs_gold_make(snippetPairsMST, n.pairs = 10)

print(gold_questions$text1)
print(gold_questions$text2)

print(gold_questions$easier_gold)

## Q6
little_women <- tokens(corpus(gutenberg_download(514)), remove_punct = TRUE)
little_dfm <- dfm(little_women)
great_gatsby <- tokens(corpus(gutenberg_download(64317)), remove_punct = TRUE)
great_dfm <- dfm(great_gatsby)

combined_dfm <- rbind(little_dfm, great_dfm)

plot(log10(1:100), log10(topfeatures(combined_dfm, 100)),
     xlab = "log10(rank)", ylab = "log10(frequency)",
     main = "Top 100 Words in Little Women & The Great Gatsby")

regression <- lm(log10(topfeatures(combined_dfm, 100)) ~ log10(1:100))
abline(regression, col = "red")

## Q7
num_tokens <- sum(lengths(little_women), lengths(great_gatsby))

M <- nfeat(combined_dfm)

k <- 44
b <- 0.4645909

k * (num_tokens)^b
M

## Q8

head(kwic(little_women, pattern = "poor*"))
head(kwic(great_gatsby, pattern = "poor*"))

head(kwic(little_women, pattern = "rich*"))
head(kwic(great_gatsby, pattern = "rich*"))

head(kwic(little_women, pattern = "wealth*", window = 4))
head(kwic(great_gatsby, pattern = "wealth*", window = 3))

head(kwic(little_women, pattern = "money*", window = 4))
head(kwic(great_gatsby, pattern = "money*", window = 4))

## Q9
library(pbapply)
data("data_corpus_ukmanifestos")
manifestos <- corpus_subset(data_corpus_ukmanifestos, Party == "Con")
sent_tokens <- unlist(tokens(manifestos, what = "sentence", include_docvars = TRUE))

yearnames <- list(unlist(names(sent_tokens)))
yearnames <- lapply(yearnames[[1]], function(x){strsplit(x, "_")[[1]][3]})
yearslist <- unlist(yearnames)

sentences_df <- tibble(text = sent_tokens, year = yearslist)
sentences_df <- sentences_df[grepl( ("[\\.\\?\\!]$"), sentences_df$text), ]

sent_corp <- corpus(sentences_df$text)
docvars(sent_corp, field = "Year") <- sentences_df$year

iters <- 10

boot_flesch <- function(party_data){
  N <- nrow(party_data)
  bootstrap_sample <- corpus_sample(corpus(c(party_data$text)), size = N, replace = TRUE)
  bootstrap_sample<- as.data.frame(as.matrix(bootstrap_sample))
  readability_results <- textstat_readability(bootstrap_sample$V1, measure = "Flesch")
  return(mean(readability_results$Flesch))
}

years = unique(yearslist)

boot_flesch_year <- pblapply(years, function(x){
  sub_data <- sentences_df %>% filter(year == x)
  output_flesch <- lapply(1:iters, function(i) boot_flesch(sub_data))
  return(unlist(output_flesch))
})

names(boot_flesch_year) <- years
year_means <- lapply(boot_flesch_year, mean) %>% unname() %>% unlist()
year_ses <- lapply(boot_flesch_year, sd) %>% unname() %>% unlist()

estimates <- data.frame(year = years,
                        mean = round(year_means, 2),
                        ses = round(year_ses, 2))
estimates

# (b)
flesch_score <- sentences_df$text %>%
  textstat_readability(measure = "Flesch") %>% 
  group_by(sentences_df$year) %>% 
  summarise(mean_flesch = round(mean(Flesch), 2)) %>% 
  setNames(c("year", "mean")) %>% arrange(year) 

estimates %>% select(-ses) %>%
  left_join(flesch_score, by = "year",
            suffix = c(".boot",".noboot")) %>%
  mutate(diff = mean.boot - mean.noboot)

ggplot(flesch_score, aes(year, mean)) +
  geom_point() + theme_bw() + 
  scale_y_continuous(breaks=seq(floor(min(flesch_score$mean)), 
                                ceiling(max(flesch_score$mean)), by = 2)) +
  xlab("Mean Flesch Score") + ylab("Year")  