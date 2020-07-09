library(stringr); library(tm); library(dplyr); library(tibble); library(reshape); library(caret); library(RWeka); library(LaF)
library(jsonlite); library(purrr); library(data.table); library(tidyr); library(ggplot2); library(textcat); library(tidytext)

# Importing and CLeaning Data
if (! file.exists("Coursera-SwiftKey.zip")) {
        download.file("https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip", "Coursera-SwiftKey.zip", mode = "wb")
        unzip("Coursera-SwiftKey.zip")
}

files <- list.files(pattern = "\\.txt$", recursive = T)

set.seed(133)
for (i in 1:length(files)) {
        assign(files[i], sample_lines(files[i], 1000))
}

dat_all <- c(mget(unlist(files)))

rm(list = files)

dat_all <- dat_all %>%
        map(., as.data.frame) %>%
        rbindlist(., fill = T, idcol = T) %>%
        dplyr::rename(., doc_id = .id, text = `.x[[i]]`) %>%
        mutate(., doc_id = as.factor(sub(".*\\_ *(.*?) *.txt*", "\\1", doc_id)),
               language = ifelse(str_extract(doc_id, "[^.]+") == "US", "EN", str_extract(doc_id, "[^.]+")))

lang <- unique(gsub("(?:.*/){2}([^_]+)_.*", "\\1", files))
token <- function(x) {
        NGramTokenizer(x, Weka_control(min = 1, max = 4))
}

for (i in 1:length(lang)) {
        assign(paste("corpus", lang[i], sep = "_"),
               dat_all %>%
                       subset(., .$language == toupper(lang[i])) %>%
                       group_by(., doc_id) %>%
                       summarise(., text = Reduce(paste, text)) %>%
                       as.data.frame(.) %>%
                       DataframeSource(.) %>%
                       VCorpus(., readerControl = list(language = lang[i])) %>%
                       tm_map(., content_transformer(tolower)) %>%
                       tm_map(., content_transformer(removePunctuation)) %>%
                       tm_map(., content_transformer(removeNumbers)) %>%
                       tm_map(., content_transformer(stripWhitespace)) %>%
                       tm_map(., content_transformer(gsub), pattern="\\W",replace=" ") %>%
                       tm_map(., content_transformer(function(.) gsub("http[^[:space:]]*", "", .))) %>%
                       tm_map(., content_transformer(function(.) gsub("[^[:alpha:][:space:]]*", "", .))) %>%
                       #tm_map(., removeWords, stopwords(kind = lang[i])) %>% they play an important part in language  
                       #tm_map(., stemDocument, lang[i]) %>% will make evaluation harder
                       tm_map(., content_transformer(PlainTextDocument)) %>%
                       DocumentTermMatrix(., control = list(tokenize = token)) %>%
                       removeSparseTerms(., 0.99) %>%
                       as.matrix(.) %>%
                       as.data.frame(.) %>%
                       rownames_to_column(., var = "Doc") %>%
                       gather(., Word, Frequency, names(.)[-1]) %>%
                       mutate(., Language = ifelse(str_extract(Doc, "[^.]+") == "US", "EN", str_extract(Doc, "[^.]+"))) %>%
                       mutate_if(., is.character, as.factor) %>%
                       group_by(., Language, Word) %>%
                       summarise(., Frequency = sum(Frequency)) %>%
                       data.frame(.)
        )
}

dat_all <- reshape::merge_all(list(corpus_de, corpus_en, corpus_fi, corpus_ru)) %>%
        mutate(., N_gram = str_count(Word, "\\S+"))

rm(corpus_de, corpus_en, corpus_fi, corpus_ru)

# Training and Test Data

training <- dat_all %>%
        group_by(., Language, N_gram) %>%
        sample_frac(., 0.75) %>%
        data.frame(.)

test <- dat_all %>%
        group_by(., Language, N_gram) %>%
        sample_frac(., 0.25) %>%
        data.frame(.)
        
# Exploratory Analysis 

gram1.plot <- training %>%
        subset(., N_gram == 1) %>% 
        group_by(., Language) %>%
        top_n(., 10, Frequency) %>%
        ggplot(., aes(x = reorder_within(Word, -Frequency, Language), y = Frequency, fill = Language)) + 
        geom_bar(stat="identity", position="dodge") + facet_wrap(~ Language, ncol = 4, scales='free_x') + theme_bw() + 
        labs(title = "1-gram: Top 10 Words by Language", x = "Words") + scale_x_reordered() + 
        theme(axis.text.x = element_text(angle = 45,hjust = 1))
gram1.plot 

gram2.plot <- training %>%
        subset(., N_gram == 2) %>% 
        group_by(., Language) %>%
        top_n(., 10, Frequency) %>%
        ggplot(., aes(x = reorder_within(Word, -Frequency, Language), y = Frequency, fill = Language)) + 
        geom_bar(stat="identity", position="dodge") + facet_wrap(~ Language, ncol = 4, scales='free_x') + theme_bw() + 
        labs(title = "2-gram: Top 10 Words by Language", x = "Words") + scale_x_reordered() + 
        theme(axis.text.x = element_text(angle = 45,hjust = 1))
gram2.plot 

gram3.plot <- training %>%
        subset(., N_gram == 3) %>% 
        group_by(., Language) %>%
        top_n(., 10, Frequency) %>%
        ggplot(., aes(x = reorder_within(Word, -Frequency, Language), y = Frequency, fill = Language)) + 
        geom_bar(stat="identity", position="dodge") + facet_wrap(~ Language, ncol = 4, scales='free_x') + theme_bw() + 
        labs(title = "3-gram: Top 10 Words by Language", x = "Words") + scale_x_reordered() + 
        theme(axis.text.x = element_text(angle = 45,hjust = 1))
gram3.plot 

gram4.plot <- training %>%
        subset(., N_gram == 4) %>% 
        group_by(., Language) %>%
        top_n(., 10, Frequency) %>%
        ggplot(., aes(x = reorder_within(Word, -Frequency, Language), y = Frequency, fill = Language)) + 
        geom_bar(stat="identity", position="dodge") + facet_wrap(~ Language, ncol = 4, scales='free_x') + theme_bw() + 
        labs(title = "4-gram: Top 10 Words by Language", x = "Words") + scale_x_reordered() + 
        theme(axis.text.x = element_text(angle = 45,hjust = 1))
gram4.plot 

# Predictive Model

## Ngram Model
NextWordPrediction <- function(input) {
        dat <- training %>%
                subset(., N_gram == str_count(input, "\\S+") + 1) %>%
                filter(grepl(paste("^", tolower(str_squish(input)), sep = ""), Word)) %>%
                arrange(., desc(Frequency))
        if (nrow(dat) != 0) {
                # assign("training", training %>%
                #         subset(., N_gram == str_count(input, "\\S+")) %>%
                #         filter(grepl(paste("^", tolower(input), sep = ""), Word)) %>%
                #         arrange(., desc(Frequency)) %>%
                #         mutate(., Frequency = replace(Frequency, Frequency == max(Frequency), Frequency + 1)),
                #        envir = .GlobalEnv)
                
                val <- str_split(dat$Word[1], " ", simplify = T)[1, eval(str_count(input, "\\S+") + 1)]
                paste(str_squish(input), val)
        } else {
                # training <- training %>%
                #         add_row(., Word = tolower(input), Frequency = 1, N_gram = str_count(input, "\\S+"))
                print("Word not in dictionary")
        }
}

NextWordPrediction("hello")
