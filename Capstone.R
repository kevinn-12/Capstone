library(stringr); library(tm); library(dplyr); library(tibble); library(reshape); library(caret); library(RWeka); library(LaF)
library(jsonlite); library(purrr); library(data.table); library(tidyr); library(ggplot2); library(textcat)

# Importing and CLeaning Data
if (! file.exists("Coursera-SwiftKey.zip")) {
        download.file("https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip", "Coursera-SwiftKey.zip", mode = "wb")
        unzip("Coursera-SwiftKey.zip")
}

files <- list.files(pattern = "\\.txt$", recursive = T)

set.seed(123)
for (i in 1:length(files)) {
        assign(files[i], sample_lines(files[i], 1000))
}

dat_all <- c(mget(unlist(files)))

rm(list = files)

dat_all <- map(dat_all, as.data.frame)
dat_all <- rbindlist(dat_all, fill = T, idcol = T)

dat_all <- dat_all %>%
        dplyr::rename(., doc_id = .id, text = `.x[[i]]`) %>%
        mutate(., doc_id = as.factor(sub(".*\\_ *(.*?) *.txt*", "\\1", doc_id)),
               language = ifelse(str_extract(doc_id, "[^.]+") == "US", "EN", str_extract(doc_id, "[^.]+")))

lang <- unique(gsub("(?:.*/){2}([^_]+)_.*", "\\1", files))
token <- function(x) {
        NGramTokenizer(x, Weka_control(min = 2, max = 4))
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
                       tm_map(., content_transformer(PlainTextDocument)) %>%
                       tm_map(., removeWords, stopwords(kind = lang[i])) %>%
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

dat_all <- reshape::merge_all(list(corpus_de, corpus_en, corpus_fi, corpus_ru))
rm(corpus_de, corpus_en, corpus_fi, corpus_ru)

dat_all <- dat_all %>%
        mutate(., N_gram = str_count(Word, "\\S+"))

        
# Exploratory Analysis 
top_words_all <- n_gram_all %>%
        as.matrix(.) %>%
        as.data.frame(.) %>%
        rownames_to_column(., var = "Doc")

top_words_all <- top_words_all %>%
        gather(., Word, Frequency, 2:ncol(top_words_all)) %>%
        mutate(., Language = ifelse(str_extract(Doc, "[^.]+") == "US", "EN", str_extract(Doc, "[^.]+"))) %>%
        mutate_if(., is.character, as.factor) %>%
        group_by(., Language, Word) %>%
        summarise(., Frequency = sum(Frequency)) %>%
        data.frame(.) %>%
        group_by(., Language) %>%
        top_n(., 10, Frequency) %>%
        data_frame(.)

freq.plot_all <- ggplot(top_words_all, aes(x = reorder(Word, -Frequency), y = Frequency, fill = Language)) + 
        geom_bar(stat="identity", position="dodge") + facet_wrap(~ Language, ncol = 2, scales='free_x') + theme_bw() + 
        labs(title = "Top 10 Words by Language", x = "Words")
freq.plot_all 

