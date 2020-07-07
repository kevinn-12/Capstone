library(stringr); library(tm); library(dplyr); library(tibble); library(reshape); library(caret); library(RWeka); library(LaF)
library(jsonlite); library(purrr); library(data.table); library(tidyr); library(ggplot2)

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

dat <- c(mget(unlist(files)))

rm(list = files)

dat <- map(dat, as.data.frame)
dat <- rbindlist(dat, fill = T, idcol = T)

dat <- dat %>%
        dplyr::rename(., doc_id = .id, text = `.x[[i]]`) %>%
        mutate(., doc_id = as.factor(sub(".*\\_ *(.*?) *.txt*", "\\1", doc_id))) 

dat <- dat %>%
        group_by(., doc_id) %>%
        summarise(., text = Reduce(paste, text)) %>%
        mutate(., language = sub("\\..*", "", doc_id)) %>%
        as.data.frame(.) %>%
        DataframeSource(.) %>%
        VCorpus(.)

dat <- dat %>%
        tm_map(., content_transformer(tolower)) %>%
        tm_map(., content_transformer(removePunctuation)) %>%
        tm_map(., content_transformer(removeNumbers)) %>%
        tm_map(., content_transformer(stripWhitespace)) %>%
        tm_map(., content_transformer(PlainTextDocument)) %>%
        DocumentTermMatrix(.) %>%
        removeSparseTerms(., 0.99)
        
# Exploratory Analysis 
top_words <- dat %>%
        as.matrix(.) %>%
        as.data.frame(.) %>%
        rownames_to_column(., var = "Doc")

top_words <- top_words %>%
        gather(., Word, Frequency, 2:ncol(top_words)) %>%
        mutate(., Language = str_extract(Doc, "[^.]+")) %>%
        mutate_if(., is.character, as.factor) %>%
        group_by(., Language, Word) %>%
        summarise(., Frequency = sum(Frequency)) %>%
        data.frame(.) %>%
        group_by(., Language) %>%
        top_n(., 10, Frequency) %>%
        data_frame(.)
        #arrange(., desc(Frequency), group_by = Language)

freq.plot <- ggplot(top_words, aes(x = reorder(Word, -Frequency), y = Frequency, fill = Language)) + geom_bar(stat="identity", position="dodge") +
        facet_wrap(~ Language, ncol = 2, scales='free_x') + theme_bw() + labs(title = "Top 10 Words by Language", x = "Words")
freq.plot 

