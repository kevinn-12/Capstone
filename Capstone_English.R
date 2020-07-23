library(stringr); library(tm); library(dplyr); library(tibble); library(reshape); library(RWeka); library(LaF); library(purrr); library(tidyr)
library(data.table)

# Importing and CLeaning Data
if (! file.exists("Coursera-SwiftKey.zip")) {
        download.file("https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip", "Coursera-SwiftKey.zip", mode = "wb")
        unzip("Coursera-SwiftKey.zip")
}

files <- list.files(pattern = "en_US", recursive = T)

set.seed(133)
for (i in 1:length(files)) {
        assign(files[i], sample_lines(files[i], 500))
}

dat_all <- c(mget(unlist(files)))

rm(list = files)

dat_all <- dat_all %>%
        map(., as.data.frame) %>%
        rbindlist(., fill = T, idcol = T) %>%
        dplyr::rename(., doc_id = .id, text = `.x[[i]]`) %>%
        mutate(., doc_id = as.factor(sub(".*\\_ *(.*?) *.txt*", "\\1", doc_id)))

# Training and Test Data

training <- dat_all %>%
        sample_frac(., 0.75) %>%
        data.frame(.)

test <- dat_all %>%
        sample_frac(., 0.25) %>%
        data.frame(.)

rm(dat_all)

token <- function(x) {
        NGramTokenizer(x, Weka_control(min = 1, max = 4))
}

training <- training %>%
        as.data.frame(.) %>%
        DataframeSource(.) %>%
        VCorpus(., readerControl = list(language = "en")) %>%
        tm_map(., content_transformer(tolower)) %>%
        tm_map(., content_transformer(removeNumbers)) %>%
        tm_map(., content_transformer(stripWhitespace)) %>%
        tm_map(., content_transformer(function(.) gsub("[^\x01-\x7F]+", "", .))) %>%
        tm_map(., content_transformer(function(.) removePunctuation(., preserve_intra_word_contractions = T, preserve_intra_word_dashes = T))) %>%
        #tm_map(., removeWords, stopwords(kind = "en")) %>%
        #tm_map(., stemDocument, "en") %>% 
        tm_map(., content_transformer(PlainTextDocument)) %>%
        DocumentTermMatrix(., control = list(tokenize = token))%>%
        as.matrix(.) %>%
        as.data.frame(.) %>%
        gather(., Word, Frequency, names(.)) %>%
        group_by(., Word) %>%
        summarise(., Frequency = sum(Frequency)) %>%
        ungroup(.) %>%
        mutate(., N_gram = str_count(Word, "\\S+")) %>%
        group_by(., N_gram) %>%
        mutate(., Prop = Frequency/ sum(Frequency),
               Word_to_Predict = word(Word, -1)) %>%
        data.frame(.)

# Exploratory Analysis
top.plot <- training %>%
        group_by(., N_gram) %>%
        top_n(., 10, Prop) %>%
        ggplot(., aes(x = reorder_within(Word_to_Predict, -Prop, N_gram), y = Prop, fill = as.factor(N_gram))) + 
        geom_bar(stat="identity", position="dodge") + facet_wrap(~ N_gram, ncol = 2, scales = "free") + 
        scale_y_continuous(labels = scales::percent) + theme_bw() + scale_x_reordered() + 
        labs(title = "Top Predicted Words by N-gram", x = "Words", fill = "N-gram") + 
        coord_flip()
top.plot

# Predictive Model
NextWordPrediction <- function(input) {
        dat <- training %>%
                filter(., N_gram == str_count(input, "\\S+") + 1) %>%
                filter(grepl(paste("^", tolower(str_squish(input)), "\\b", sep = ""), Word)) %>%
                arrange(., desc(Prop))
        
        if (nrow(dat) != 0) {
                assign("training",
                       training %>%
                               mutate(Frequency = ifelse(Word == input &
                                                                 N_gram == str_count(input, "\\S+"),
                                                         Frequency + 1,
                                                         Frequency)) %>%
                               group_by(., N_gram) %>%
                               mutate(., Prop = Frequency/ sum(Frequency)) %>%
                               data.frame(.),
                       envir = .GlobalEnv)
                
                val <- dat$Word_to_Predict[1]
                ans <- paste(str_squish(input), val)
                
                return(list(ans, head(dat,5)))
                
        } else {
                for (i in 2:str_count(input, "\\S+")) {
                        input_1 <-  word(input, start = i, end =  str_count(input,"\\S+"))
                        
                        dat <- training %>%
                                filter(., N_gram == str_count(input_1, "\\S+") + 1) %>%
                                filter(grepl(paste("^", tolower(str_squish(input_1)), "\\b", sep = ""), Word)) %>%
                                arrange(., desc(Prop))
                        if (nrow(dat) != 0) {
                                val <- dat$Word_to_Predict[1]
                                ans <- paste(str_squish(input), val)
                                
                                return(list(ans, head(dat,5)))
                                
                        } else if (nrow(dat) == 0 & i == str_count(input, "\\S+")) {
                                
                                for (i in str_count(input, "\\S+"):1) {
                                        input_1 <-  word(input, start = 1, end =  i)
                                        
                                        assign("training",
                                               training %>%
                                                       add_row(., Word = tolower(input_1), Frequency = + 1, N_gram = str_count(input_1, "\\S+"), 
                                                               Word_to_Predict = word(input_1, -1)) %>%
                                                       group_by(., N_gram) %>%
                                                       mutate(., Prop = Frequency/ sum(Frequency)) %>%
                                                       data.frame(.),
                                               envir = .GlobalEnv)
                                }
                                
                                ans <- paste("Word not in dictionary. We added this to our database!")
                                
                                return(ans)
                        }
                }
        }
}

# Model Evaluation
test <- test %>%
        as.data.frame(.) %>%
        DataframeSource(.) %>%
        VCorpus(., readerControl = list(language = "en")) %>%
        tm_map(., content_transformer(tolower)) %>%
        tm_map(., content_transformer(removeNumbers)) %>%
        tm_map(., content_transformer(stripWhitespace)) %>%
        tm_map(., content_transformer(function(.) gsub("[^\x01-\x7F]+", "", .))) %>%
        tm_map(., content_transformer(function(.) removePunctuation(., preserve_intra_word_contractions = T, preserve_intra_word_dashes = T)))%>%
        tm_map(., content_transformer(PlainTextDocument)) %>%
        DocumentTermMatrix(., control = list(tokenize = token))%>%
        as.matrix(.) %>%
        as.data.frame(.) %>%
        gather(., Word, Frequency, names(.)) %>%
        group_by(., Word) %>%
        summarise(., Frequency = sum(Frequency)) %>%
        ungroup(.) %>%
        mutate(., N_gram = str_count(Word, "\\S+")) %>%
        group_by(., N_gram) %>%
        mutate(., Prop = Frequency/ sum(Frequency)) %>%
        data.frame(.)

test$test <- 1
for (i in 1:nrow(test)) {
        # print(i) run as progress bar
        word <- ifelse(test[i,3] == 1, test[i,1], word(test[i,1], start = 1, end = str_count(test[i,1],"\\S+")-1))
        test[i,5] <- NextWordPrediction(word)[1]
}

check <- test %>%
        mutate(., check = ifelse(Word == test, 1, 0)) %>%
        summarise(., Accuracy = sum(check)/nrow(.))
check