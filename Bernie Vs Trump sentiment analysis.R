#Sentiment analysis for Trump and Bernie Sunders

library(tm)
library(twitteR)
library(wordcloud)
library(RColorBrewer)
library(e1071)
library(class)

#Povezivanje na twitter


#ubacujemo pozitivne i negativne reči

pos.words <- readLines("C:/Users/vjovanovic/Desktop/R Udemy/Vezbe udemy/reci za sentiment analizu/positive_words.txt")
neg.words <- readLines("C:/Users/vjovanovic/Desktop/R Udemy/Vezbe udemy/reci za sentiment analizu/negative_words.txt")
head(pos.words)

library(stringr)
library(plyr)



score.sentiment1 = function(sentences, pos.words, neg.words, .progress='none')
  
{
  # we got a vector of sentences. plyr will handle a list or a vector as an "l" for us
  # we want a simple array of scores back, so we use "l" + "a" + "ply" = laply:
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    
    # clean up sentences with R's regex-driven global substitute, gsub():
    sentence = gsub('[[:punct:]]', '', sentence)
    sentence = gsub('[[:cntrl:]]', '', sentence)
    sentence = gsub('\\d+', '', sentence)
    # and convert to lower case:
    sentence = tolower(sentence)
    
    # split into words. str_split is in the stringr package
    word.list = str_split(sentence, '\\s+')
    # sometimes a list() is one level of hierarchy too much
    words = unlist(word.list)
    
    # compare our words to the dictionaries of positive & negative terms
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    
    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    score = sum(pos.matches) - sum(neg.matches)
    
    return(score)
  }, pos.words, neg.words, .progress=.progress )
  
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
  
}

#skupljanje tvitova za svaku kompaniju

bernie <- searchTwitter("#Bernie2020", n=9000, lang = "en")
trump <- searchTwitter("#TeamTrump", n=9000, lang = "en")



#uzimanje teksta

bernie_txt <- sapply(novartis, function(x) x$getText())
trump_txt <- sapply(roche, function(x) x$getText())


#broj tvitova

nd <- c(length(bernie_txt), length(trump_txt))
head(nd)

#pridruživanje tekstova

company <- c(bernie_txt, trump_txt)
head(company)

#primenjujemo gore kreiranu funkciju score_sentiment


scores <- score.sentiment1(company, pos.words, neg.words, .progress = "text")
head(scores)


#dodajemo varijable u bazu
scores$company <- factor(rep(c("bernie", "trump"), nd))
scores$very.pos <- as.numeric(scores$score >= 2)
scores$very.neg <- as.numeric(scores$score <= -2)

#koliko je veoma positivnih i negativnih tvitova

numpos <- sum(scores$very.pos)
numneg <- sum(scores$very.neg)

ukupan_skor <- 100*numpos/(numpos+numneg)

#graf
library(ggplot2)
ggplot(scores, aes(score)) + geom_bar(fill="red", color="black") + facet_wrap(~company) + xlab("Sentiment score") + ylab("number of tweets")+ geom_vline(xintercept = 1, linetype="dotted", 
                                                                                                                                                         color = "blue", size=1.5) + geom_hline(yintercept = 43, linetype ="dotted", color="purple", size=1.5)
