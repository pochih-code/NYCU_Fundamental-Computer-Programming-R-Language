# *** 第一題 和 第二題 需要分開執行
# 第一題
library(tm)
library(magrittr)

interview <- readLines("Elon_Musk_interview.txt")
corpus <- VCorpus(VectorSource(interview)) %>%
  tm_map(stripWhitespace) %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(removeWords, c(stopwords("english"), "thats"))
tdm <- TermDocumentMatrix(corpus) %>% 
  as.matrix() %>%
  rowSums() %>%
  sort(decreasing = T) %>%
  head()
tdmdf <- data.frame(freq=tdm)
tdmdf
bf <- barplot(tdmdf$freq, names.arg = rownames(tdmdf), xlab='words',
              ylab='freq', main='前六多詞彙長條圖', col='#00b3ff')
text(bf, tdmdf$freq, labels=tdmdf$freq, pos=1)

# 第二題
KB <- read.csv('KB.csv', header=T, sep=',')

KB_EFF <- ((KB$PTS+KB$TRB+KB$AST+KB$STL+KB$BLK)
           -((KB$FGA-KB$FG)+(KB$FTA-KB$FT)+KB$TOV)) / KB$G

LJ <- read.csv('LJ.csv', header=T, sep=',')

LJ_EFF <- ((LJ$PTS+LJ$TRB+LJ$AST+LJ$STL+LJ$BLK)
           -((LJ$FGA-LJ$FG)+(LJ$FTA-LJ$FT)+LJ$TOV)) / LJ$G

MJ <- read.csv('MJ.csv', header=T, sep=',')

MJ_EFF <- ((MJ$PTS+MJ$TRB+MJ$AST+MJ$STL+MJ$BLK)
           -((MJ$FGA-MJ$FG)+(MJ$FTA-MJ$FT)+MJ$TOV)) / MJ$G

# give vector length to variable
len_KB <- length(KB_EFF)
len_LJ <- length(LJ_EFF)
len_MJ <- length(MJ_EFF)

maxLen <- max(len_KB, len_LJ, len_MJ)

if (len_KB != maxLen)
  KB_EFF[len_KB:maxLen] <- 0
if (len_LJ != maxLen)
  LJ_EFF[len_LJ:maxLen] <- 0
if (len_MJ != maxLen)
  MJ_EFF[len_MJ:maxLen] <- 0

s1 = paste("KB_EFF ( begin at", KB$Season[1], ", end at", KB$Season[len_KB],")")
s2 = paste("LJ_EFF ( begin at", LJ$Season[1], ", end at", LJ$Season[len_LJ],")")
s3 = paste("MJ_EFF ( begin at", MJ$Season[1], ", end at", MJ$Season[len_MJ],")")

matrix_EFF <- rbind(KB_EFF, LJ_EFF, MJ_EFF)

xx <- barplot(matrix_EFF, col=c('4','7','5'), xlab="ith year", ylab="Efficiency", ylim=c(0, 2.5),
              names.arg=1:maxLen, density=100, beside=TRUE, main="NBA's Career performance")

legend("topleft", legend=c(s1, s2, s3), col=c('4','7','5'), lwd=10, bty = "n", cex=0.75)

text(x=xx, y=matrix_EFF, label=round(matrix_EFF, 2), pos=3, cex=0.75)