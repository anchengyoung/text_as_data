library(tidyverse)
library(quanteda)
library(quanteda.textmodels)

# Q1
emails <- data.frame(sender = c("Perdue","Perdue","Ossoff","Ossoff",
                               "Ossoff","Perdue","Ossoff"),
                     content = c("immigration aliens criminals country loophole",
                                 "voter economy tax jobs security",
                                 "immigration country diversity help security",
                                 "healthcare affordable expansion unfair help",
                                 "voter relief debt jobs help",
                                 "healthcare cost socialism unfair help",
                                 "abortion choice right women help court"))
mystery <- "healthcare voter tax help jobs"

# (a)
by_sender <- emails %>%
  group_by(sender) %>%
  summarize(words = paste(content, collapse = " "),
            email_ratio = n()/length(emails$content),
            total_words = str_count(words," ")+1,
            healthcare = str_count(words,"healthcare")/total_words,
            voter = str_count(words,"voter")/total_words,
            tax = str_count(words,"tax")/total_words,
            help = str_count(words,"help")/total_words,
            jobs = str_count(words,"jobs")/total_words,
            prediction = healthcare*voter*tax*help*jobs*email_ratio)

by_sender %>% select(sender, prediction)

# (b)
all_words <- paste(emails$content, collapse = " ")
unique_words <- length(dfm(tokens(all_words)))

by_sender_sm <- emails %>%
  group_by(sender) %>%
  summarize(words = paste(content, collapse = " "),
            email_ratio = n()/length(emails$content),
            total_words = str_count(words," ")+1+unique_words,
            healthcare = (str_count(words,"healthcare")+1)/total_words,
            voter = (str_count(words,"voter")+1)/total_words,
            tax = (str_count(words,"tax")+1)/total_words,
            help = (str_count(words,"help")+1)/total_words,
            jobs = (str_count(words,"jobs")+1)/total_words,
            prediction = healthcare*voter*tax*help*jobs*email_ratio)

by_sender_sm %>% select(sender, prediction)

# Q2
setwd("Documents/atNYU/Grad/2022 Spring/Text As Data/Homework/Homework_2")
#setwd("../../../../../../..")
tripadvisor <- read_csv("tripadvisor.csv",show_col_types = FALSE)

# (a)
tripadvisor <- tripadvisor %>%
  mutate(class = ifelse(stars >= median(tripadvisor$stars),"positive","negative"))

tripadvisor %>%
  group_by(class) %>%
  summarize(proportion = n()/nrow(tripadvisor))

median(tripadvisor$stars)

# (b)
tripadvisor <- tripadvisor %>%
  mutate(anchor = ifelse(stars == 5,"positive",
                         ifelse(stars <= 2,"negative","neutral")))

tripadvisor %>%
  group_by(anchor) %>%
  summarize(proportion = n()/nrow(tripadvisor))

# Q3
pos_words <- c(read.table("positive-words.txt", header=FALSE)$V1)
neg_words <- c(read.table("negative-words.txt", header=FALSE)$V1)

# (a)
tripadvisor$text <- gsub(pattern = "'", "", tripadvisor$text) 

pos_dfm <- convert(dfm(tripadvisor$text, select = pos_words, remove_punct = TRUE),to="data.frame")
tripadvisor$pos_count <- rowSums(pos_dfm[,c(-1)])
neg_dfm <- convert(dfm(tripadvisor$text, select = neg_words, remove_punct = TRUE),to="data.frame")
tripadvisor$neg_count <- rowSums(neg_dfm[,c(-1)])

tripadvisor <- tripadvisor %>%
  mutate(sentiment = pos_count - neg_count)

ggplot(tripadvisor, aes(x=sentiment)) +
  geom_histogram(binwidth = 1) +
  xlab("Sentiment Score")

# (b)
tripadvisor <- tripadvisor %>%
  mutate(dichotomous = ifelse(sentiment > 0,"positive","negative"))

tripadvisor %>%
  group_by(dichotomous) %>%
  summarize(n()/nrow(tripadvisor))

# (c)
cmat <- table(tripadvisor$dichotomous,tripadvisor$class)
cmat
accuracy <- sum(diag(cmat))/sum(cmat)
accuracy
precision <- cmat[2,2]/sum(cmat[,2])
precision
recall <- cmat[2,2]/sum(cmat[2,])
recall
specificity <- cmat[1,1]/sum(cmat[1,])
specificity
f1_score <- 2*precision*recall/(precision+recall)
f1_score

# Q4

# (a)

# (b)
set.seed(42)
prop_train <- 0.2
ids <- 1:nrow(tripadvisor)
ids_train <- sample(ids, ceiling(prop_train*length(ids)), replace = FALSE)
train_set <- tripadvisor[ids_train,]
test_set <- tripadvisor[-ids_train,]

train_dfm <- dfm(train_set$text, stem = TRUE, remove_punct = TRUE, remove = stopwords("english"))
test_dfm <- dfm(test_set$text, stem = TRUE, remove_punct = TRUE, remove = stopwords("english"))
test_dfm <- dfm_match(test_dfm, features = featnames(train_dfm))

nb_model_sm <- textmodel_nb(train_dfm, train_set$class, smooth = 1, prior = "uniform")
predicted_class_sm <- predict(nb_model_sm, newdata = test_dfm, force=TRUE)

cmat_sm <- table(test_set$class, predicted_class_sm)
cmat_sm
nb_acc_sm <- sum(diag(cmat_sm))/sum(cmat_sm)
nb_acc_sm
nb_precision_sm <- cmat_sm[2,2]/sum(cmat_sm[,2])
nb_precision_sm
nb_recall_sm <- cmat_sm[2,2]/sum(cmat_sm[2,])
nb_recall_sm
nb_f1_sm <- 2*(nb_recall_sm*nb_precision_sm)/(nb_recall_sm + nb_precision_sm)
nb_f1_sm

# (c)
nb_model_sm_2 <- textmodel_nb(train_dfm, train_set$class, smooth = 1, prior = "docfreq")
predicted_class_2 <- predict(nb_model_sm_2, newdata = test_dfm, force=TRUE)

cmat_sm_2 <- table(test_set$class, predicted_class_2)
cmat_sm_2
nb_acc_sm_2 <- sum(diag(cmat_sm_2))/sum(cmat_sm_2)
nb_acc_sm_2
nb_precision_sm_2 <- cmat_sm_2[2,2]/sum(cmat_sm_2[,2])
nb_precision_sm_2
nb_recall_sm_2 <- cmat_sm_2[2,2]/sum(cmat_sm_2[2,])
nb_recall_sm_2
nb_f1_sm_2 <- 2*(nb_recall_sm_2*nb_precision_sm_2)/(nb_recall_sm_2 + nb_precision_sm_2)
nb_f1_sm_2

# (d)
nb_model <- textmodel_nb(train_dfm, train_set$class, smooth = 0, prior = "uniform")
predicted_class <- predict(nb_model, newdata = test_dfm, force=TRUE)

cmat <- table(test_set$class, predicted_class)
cmat
nb_acc <- sum(diag(cmat))/sum(cmat)
nb_acc
nb_precision <- cmat[2,2]/sum(cmat[,2])
nb_precision
nb_recall <- cmat[2,2]/sum(cmat[2,])
nb_recall
nb_f1 <- 2*(nb_recall*nb_precision)/(nb_recall + nb_precision)
nb_f1

# (e)

# Q5
library(readtext)
filenames <- list.files(path = "manifestos")
party <- unlist(regmatches(unlist(filenames), gregexpr("^[[:alpha:]]{3}", unlist(filenames))))
year <- unlist(regmatches(unlist(filenames), gregexpr("[[:digit:]]+", unlist(filenames))))
cons_labour_manifestos <- corpus(readtext("manifestos/*.txt"))
docvars(cons_labour_manifestos, field = c("party", "year")) <- data.frame(cbind(party, year))
cons_labour_df <- tibble(text = as.character(cons_labour_manifestos),
                         class = party,year = as.integer(year))

anchor_labor <- cons_labour_df %>% filter(class == 'Lab', year == 1945)
anchor_cons <- cons_labour_df %>% filter(class == 'Con', year == 1983)
train_set <- rbind(anchor_labor, anchor_cons)
train_dfm <- dfm(train_set$text, remove_punct = TRUE, remove = stopwords("english"))

ws_base <- textmodel_wordscores(train_dfm, y = (2*as.numeric(train_set$class == "Lab"))-1)

head(sort(ws_base$wordscores, decreasing = TRUE),10)
head(sort(ws_base$wordscores, decreasing = FALSE),10)

# (b)
test_labor <- cons_labour_df %>% filter(class == 'Lab', year != 1945)
test_cons <- cons_labour_df %>% filter(class == 'Con', year != 1983)
test_set <- rbind(test_labor, test_cons)
test_dfm <- dfm(test_set$text, remove_punct = TRUE, remove = stopwords("english"))

predict(ws_base, newdata = test_dfm, rescaling = "none", level = 0.95) 

# (c)
predict(ws_base, newdata = test_dfm, rescaling = "lbg", level = 0.95)

# Q6
library(caret)
trip_samp <- tripadvisor[1:1000,]

# (a)

# (b)
tokenized <- tokens(trip_samp$text, remove_punct = TRUE)
no_stop <- tokens_remove(tokenized, pattern = stopwords("en"))
stemmed <- tokens_wordstem(no_stop)
trip_dfm <- dfm(stemmed) %>% convert("matrix")

results <- data.frame(train_size = NA, accuracy = NA)
for (i in 1:9) {
  
  ids_train <- createDataPartition(1:nrow(trip_dfm), p = 0.1*i, list = FALSE, times = 1)
  train_x <- trip_dfm[ids_train, ] %>% as.data.frame()
  train_y <- trip_samp$class[ids_train] %>% as.factor()
  val_x <- trip_dfm[-ids_train, ] %>% as.data.frame()
  val_y <- trip_samp$class[-ids_train] %>% as.factor()
  
  trctrl <- trainControl(method = "cv", number = 5)
  svm_mod_linear <- train(x = train_x, y = train_y, 
                          method = "svmLinear", trControl = trctrl)
  svm_linear_pred <- predict(svm_mod_linear, newdata = val_x)
  svm_linear_cmat <- confusionMatrix(svm_linear_pred, val_y)
  results[i,] <- c(0.1*i, svm_linear_cmat$overall[1])
  
}

results %>% filter(accuracy == max(accuracy))

# (c)
ids_train <- createDataPartition(1:nrow(trip_dfm), p = 0.8, list = FALSE, times = 1)
train_x <- trip_dfm[ids_train, ] %>% as.data.frame()
train_y <- trip_samp$class[ids_train] %>% as.factor()
val_x <- trip_dfm[-ids_train, ] %>% as.data.frame()
val_y <- trip_samp$class[-ids_train] %>% as.factor()

trctrl <- trainControl(method = "cv", number = 5)
svm_mod_logit <- train(x = train_x, y = train_y, method = "glm",
                       trControl = trctrl, family = 'binomial')

svm_logit_pred <- predict(svm_mod_logit, newdata = val_x)
svm_logit_cmat <- confusionMatrix(svm_logit_pred, val_y)

# Q7
trip_samp <- tripadvisor[1:500,]

# (a)
tokenized <- tokens(trip_samp$text, remove_punct = TRUE)
no_stop <- tokens_remove(tokenized, pattern = stopwords("en"))
stemmed <- tokens_wordstem(no_stop)
trip_dfm <- dfm(stemmed) %>% convert("matrix")

ids_train <- createDataPartition(1:nrow(trip_dfm), p = 0.8, list = FALSE, times = 1)
train_x <- trip_dfm[ids_train, ] %>% as.data.frame()
train_y <- trip_samp$class[ids_train] %>% as.factor()
test_x <- trip_dfm[-ids_train, ] %>% as.data.frame()
test_y <- trip_samp$class[-ids_train] %>% as.factor()

# (b)
library(randomForest)
rf.base <- randomForest(x = train_x, y = train_y, importance = TRUE)
token_importance <- round(importance(rf.base, 2), 2)
head(rownames(token_importance)[order(-token_importance)],10)

# (c)
predict_test <- predict(rf.base, newdata = test_x)
cmat <- confusionMatrix(data = predict_test, reference = test_y)
cmat$table
cmat$overall[1]
precision <- cmat$byClass[5]
precision
recall <- cmat$byClass[6]
recall
f1_score <- 2*precision[[1]]*recall[[1]]/(precision[[1]]+recall[[1]])
f1_score

# (d)
mtry <- sqrt(ncol(train_x))

rf.base_1 <- randomForest(x = train_x, y = train_y, mtry = 0.5*mtry, importance = TRUE)
predict_test_1 <- predict(rf.base_1, newdata = test_x)
cmat_1 <- confusionMatrix(data = predict_test_1, reference = test_y)
cmat_1$overall[1]

rf.base_2 <- randomForest(x = train_x, y = train_y, mtry = 1.5*mtry, importance = TRUE)
predict_test_2 <- predict(rf.base_2, newdata = test_x)
cmat_2 <- confusionMatrix(data = predict_test_2, reference = test_y)
cmat_2$overall[1]

