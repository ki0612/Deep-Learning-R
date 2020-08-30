#resume_wordcloud
library(wordcloud)
library(readtext)
library(textreadr)
library(tm)
library(readr)
library(officer)
library(magrittr)

getwd()
setwd("C:/Users/kiino/Programming/R")


KiInoue_Analyst_Resume <- read_docx("KiInoue_Analyst_Resume.docx")
styles_info(KiInoue_Analyst_Resume)

KiInoue_Analyst_Resume.text <- tolower(KiInoue_Analyst_Resume)
KiInoue_Analyst_Resume.text.preword.vector <- unlist(strsplit(KiInoue_Analyst_Resume.text, "\\W"))
KiInoue_Analyst_Resume.text.vector <- KiInoue_Analyst_Resume.text.preword.vector[which(nchar(KiInoue_Analyst_Resume.text.preword.vector) > 0)]

pdf(file = "fig_text_workcloud_of_R_code.pdf", width = 11, height = 8.5)
set.seed(1031)
wordcloud(KiInoue_Analyst_Resume.text.vector, min.freq = 10,
          max.words = 300,
          random.order = FALSE,
          random.color = FALSE,
          rot.per = 0.0,
          colors = "black",
          ordered.colors = FALSE,
          use.r.layout =FALSE,
          fixed.asp = TRUE)
dev.off()
