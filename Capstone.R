library(stringr); library(tm); library(dplyr); library(tibble); library(reshape); library(caret); library(RWeka)


# Importing and CLeaning Data
if (! file.exists("Coursera-SwiftKey.zip")) {
        download.file("https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip", "Coursera-SwiftKey.zip", mode = "wb")
        unzip("Coursera-SwiftKey.zip")
}

folders <- list.files(path = "final", full.names = TRUE, recursive = FALSE)
names <- str_extract(folders, "[^/]*$")
lang <- str_extract(folders, "[^_]*$")

for (i in 1:length(folders)) {
        assign(names[i], VCorpus(DirSource(folders[i], encoding = "UTF-8"), readerControl = list(language = lang[i])))
}

for (i in 1:length(folders)) {
        assign(names[i], tm_map(get(names[i]), content_transformer(tolower)))
        assign(names[i], tm_map(get(names[i]), content_transformer(removePunctuation)))
        assign(names[i], tm_map(get(names[i]), content_transformer(removeNumbers)))
        assign(names[i], tm_map(get(names[i]), content_transformer(stripWhitespace)))
}


for (i in 1:length(names)) {
        assign(names[i],
               data.frame(
                       Language = lang[i],
                       Text = unlist(sapply(get(names[i]), `[`, "content")),
                       stringsAsFactors = FALSE))
}

for (i in 1:length(names)) {
        assign(names[i], get(names[i]) %>%
                       rownames_to_column(., "File"))
}

dat <- reshape::merge_all(list(de_DE, en_US, ru_RU, fi_FI))

rm(de_DE, en_US, ru_RU, fi_FI, folders, i, lang, names)

dat <- dat %>%
        mutate(., File = as.factor(gsub("^.+?\\.(.+?)\\..*$", "\\1", File)),
               Language = as.factor(Language))

# Tokenization






