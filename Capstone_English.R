library(stringr); library(tm); library(dplyr); library(tibble); library(reshape); library(caret); library(RWeka); library(LaF)
library(jsonlite); library(purrr); library(data.table); library(tidyr); library(ggplot2); library(textcat); library(tidytext): library(qdap)

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

# Model

token <- function(x) {
        NGramTokenizer(x, Weka_control(min = 1, max = 4))
}

training <- training %>%
        as.data.frame(.) %>%
        DataframeSource(.) %>%
        VCorpus(., readerControl = list(language = "en")) %>%
        tm_map(., content_transformer(tolower)) %>%
        tm_map(., content_transformer(function(.) gsub("[^[:alnum:][:space:]'`]", " ", .))) %>%
        tm_map(., content_transformer(removeNumbers)) %>%
        tm_map(., content_transformer(stripWhitespace)) %>%
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


# Predictive Model

## Basic Ngram Model
NextWordPrediction <- function(input) {
        dat <- training %>%
                filter(., N_gram == str_count(input, "\\S+") + 1) %>%
                filter(grepl(paste("^", tolower(str_squish(input)), sep = ""), Word)) %>%
                arrange(., desc(Prop))
        
        if (nrow(dat) != 0) {
                assign("training",
                       training %>%
                               mutate(Frequency = ifelse(Word == "she was" &
                                                                 N_gram == 2, 
                                                         Frequency + 1,
                                                         Frequency)) %>%
                               group_by(., N_gram) %>%
                               mutate(., Prop = Frequency/ sum(Frequency)) %>%
                               data.frame(.),
                       envir = .GlobalEnv)
                
                val <- dat$Word_to_Predict[1]
                ans <- paste(str_squish(input), val)
                
                return(list(ans, head(dat,5)))
                
        } else if (nrow(dat) == 0) {
                input_1 <-  Reduce(paste, word(input, 2:str_count(input,"\\S+")))
                
                return(NextWordPrediction(input_1)) 
                
        } else if (word(input, 1) == "NA") {
                assign("training",
                       training %>%
                               add_row(., Word = tolower(input), Frequency = 1, N_gram = str_count(input, "\\S+")),
                       envir = .GlobalEnv)
                ans <- paste("Word not in dictionary. We added this to our database!")
                return(ans)
        }
}

NextWordPrediction("she was great tho days later")

