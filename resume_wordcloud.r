#resume_wordcloud
library(wordcloud)
library(wordcloud2)
library(readtext)
library(textreadr)
library(tm)
library(readr)

install.packages("XML")
install.packages("pacman")
pacman::p_load(qdap)

getwd()
setwd("C:/Users/kiino/Programming/R")

KiInoue_Analyst_Resume <- read_docx("KiInoue_Analyst_Resume.docX", skip = 3, remove.empty = TRUE, trim = TRUE)

ResumeDoc <- read_docx("KiInoue_Analyst_Resume.docX",
                           skip = 3,
                           remove.empty = TRUE,
                           trim = TRUE,
                           combine = FALSE,
                           format = FALSE)
data(ResumeDoc)
peek(ResumeDoc)

# Preprocessing the filing text
ResumeDoc.text <- gsub("\\n|\\t|,", " ", ResumeDoc)
ResumeDoc.text <- paste(ResumeDoc.text, collapse=" ")
ResumeDoc.text <- gsub("'s ", "", ResumeDoc.text)
ResumeDoc.text <- gsub("[[:punct:]]", "", ResumeDoc.text, perl=T)
ResumeDoc.text <- gsub("[[:digit:]]", "", ResumeDoc.text, perl=T)
ResumeDoc.text <- iconv(ResumeDoc.text, from = 'UTF-8', to = 'ASCII//TRANSLIT')
ResumeDoc.text <- tolower(ResumeDoc.text)
ResumeDoc.text <- gsub("\\s{2,}", " ", ResumeDoc.text) 


Resume.corpus <- Corpus(VectorSource(ResumeDoc.text))
inspect(Resume.corpus)

toSpace <- content_transformer(function(x, pattern ) gsub(pattern, " ", x))
Resume.corpus <- tm_map(Resume.corpus, toSpace, "/")
Resume.corpus <- tm_map(Resume.corpus, toSpace, "@")
Resume.corpus <- tm_map(Resume.corpus, toSpace, "\\|")

# Remove english common stopwords
Resume.corpus <- tm_map(Resume.corpus, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
Resume.corpus <- tm_map(Resume.corpus, removeWords, c("august", "elavon", "centennial", "award", "denver" )) 
# Remove punctuations
Resume.corpus <- tm_map(Resume.corpus, removePunctuation)
# Eliminate extra white spaces
Resume.corpus <- tm_map(Resume.corpus, stripWhitespace)

dtm <- TermDocumentMatrix(Resume.corpus)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

set.seed(5234)
wordcloud(words = d$word, freq = d$freq,
          min.freq = 3,
          max.words = 50,
          random.order = FALSE,
          rot.per = 0.30,
          colors = brewer.pal(8,"Blues"))


library(wordcloud2) 
wordcloud2(d, size = 0.35, shape = 'star')
letterCloud( d, word = "R", color='random-light' , backgroundColor="pink")
#letterCloud( d, word = "PEACE", color="white", backgroundColor="black")



barplot(d[1:10,]$freq, las = 2, names.arg = d[1:10,]$word,
        col ="lightblue", main ="Most frequent words",
        ylab = "Word frequencies")

toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")

KiInoue_Analyst_Resume.text.preword.vector <- unlist(strsplit(ResumeDoc.text, "\\W"))
KiInoue_Analyst_Resume.text.vector <- KiInoue_Analyst_Resume.text.preword.vector[which(nchar(KiInoue_Analyst_Resume.text.preword.vector) > 0)]


KiInoue_Analyst_Resume <- removeNumbers(KiInoue_Analyst_Resume)
peek(KiInoue_Analyst_Resume)
KiInoue_Analyst_Resume.text <- tolower(KiInoue_Analyst_Resume)
KiInoue_Analyst_Resume.text.preword.vector <- unlist(strsplit(KiInoue_Analyst_Resume.text, "\\W"))
KiInoue_Analyst_Resume.text.vector <- KiInoue_Analyst_Resume.text.preword.vector[which(nchar(KiInoue_Analyst_Resume.text.preword.vector) > 0)]
head(KiInoue_Analyst_Resume.text.vector,100)

pdf(file = "fig_text_workcloud_of_R_code.pdf", width = 11, height = 8.5)
set.seed(1031)
wordcloud(KiInoue_Analyst_Resume.text.vector, min.freq = 3,
          max.words = 300,
          random.order = FALSE,
          random.color = TRUE,
          rot.per = 0.05,
          colors = brewer.pal(6, "Blues"),
          ordered.colors = FALSE,
          use.r.layout =FALSE,
          fixed.asp = TRUE)
dev.off()


library(ggplot2)
dtm <- DocumentTermMatrix(KiInoue_Analyst_Resume.text.vector)
freq <- sort(colSums(as.matrix(KiInoue_Analyst_Resume.text.vector)), decreasing=TRUE)
p <- ggplot(subset(KiInoue_Analyst_Resume.text.vector, freq > 3), aes()
