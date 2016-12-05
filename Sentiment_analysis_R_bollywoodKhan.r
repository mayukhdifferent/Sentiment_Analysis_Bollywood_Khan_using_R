pkgs <-c('twitteR','ROAuth','httr','plyr','stringr','ggplot2','plotly')
for(p in pkgs) if(p %in% rownames(installed.packages()) == FALSE) {install.packages(p)}
for(p in pkgs) suppressPackageStartupMessages(library(p, quietly=TRUE, character.only=TRUE))

# Set API Keys.Below keys are sample only.Keys need to be generated using personal twitter account.
api_key <- "dOWFWD0RnmMV4Pro4nUp8UG2C"
api_secret <- "0omNIMld9HrrBXLAKVXqaIdpUpeGCzlVr7G6aBCYsCHu3EMEYQ"
access_token <- "84622229-3D0eyGI6hCfeto44GKjsT3p67smjYsCOILiTCYi7v"
access_token_secret <- "DcNi2PzGG4tIkhaRpdd4Qa7FUHZDstFSPcDKwIfZu1cRW"
setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)

# Grab latest tweets
tweets_salman <- searchTwitter('@BeingSalmanKhan', n=5000)
tweets_srk <- searchTwitter('@iamsrk', n=5000)
tweets_aamir <- searchTwitter('@aamir_khan', n=5000)

# Loop over tweets and extract text
feed_salman <- laply(tweets_salman, function(t) t$getText())
feed_srk <- laply(tweets_srk, function(t) t$getText())
feed_aamir <- laply(tweets_aamir, function(t) t$getText())

# Read in dictionary of positive and negative workds
yay <- scan('opinion-lexicon-English/positive-words.txt',
                  what='character', comment.char=';')
boo <- scan('opinion-lexicon-English/negative-words.txt',
                  what='character', comment.char=';')
# Add a few twitter-specific negative phrases
bad_text <- c(boo, 'wtf','epicfail', 'slow','old','dumb','cannot','criminal')
good_text <- c(yay,'voted','hot','sexy','handsome',':)','Love','Loving')

score.sentiment <- function(sentences, good_text, bad_text, .progress='none')
{
    require(plyr)
    require(stringr)
    # we got a vector of sentences. plyr will handle a list
    # or a vector as an "l" for us
    # we want a simple array of scores back, so we use
    # "l" + "a" + "ply" = "laply":
    scores = laply(sentences, function(sentence, good_text, bad_text) {
        
        # clean up sentences with R's regex-driven global substitute, gsub():
        sentence = gsub('[[:punct:]]', '', sentence)
        sentence = gsub('[[:cntrl:]]', '', sentence)
        sentence = gsub('\\d+', '', sentence)
        #to remove emojis
        sentence <- iconv(sentence, 'UTF-8', 'ASCII')
        sentence = tolower(sentence)
        
        # split into words. str_split is in the stringr package
        word.list = str_split(sentence, '\\s+')
        # sometimes a list() is one level of hierarchy too much
        words = unlist(word.list)
        
        # compare our words to the dictionaries of positive & negative terms
        pos.matches = match(words, good_text)
        neg.matches = match(words, bad_text)
        
        # match() returns the position of the matched term or NA
        # we just want a TRUE/FALSE:
        pos.matches = !is.na(pos.matches)
        neg.matches = !is.na(neg.matches)
        
        # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
        score = sum(pos.matches) - sum(neg.matches)
        
        return(score)
    }, good_text, bad_text, .progress=.progress )
    
    scores.df = data.frame(score=scores, text=sentences)
    return(scores.df)
}

# Retreive scores and add candidate name.
thesalman <- score.sentiment(feed_salman, good_text, bad_text, .progress='text')
thesalman$name <- 'SalmanKhan'
thesrk <- score.sentiment(feed_srk, good_text, bad_text, .progress='text')
thesrk$name <- 'SRK'
theaamir <- score.sentiment(feed_aamir, good_text, bad_text, .progress='text')
theaamir$name <- 'AamirKhan'
# Merge into one dataframe for plotting
plotdat <- rbind(thesalman, thesrk, theaamir)
# Cut the text, just gets in the way
plotdat <- plotdat[c("name", "score")]
# Remove neutral values of 0
plotdat <- plotdat[!plotdat$score == 0, ]
# Remove anything less than -5 or greater than 5
plotdat <- plotdat[!plotdat$score > 5, ]
plotdat <- plotdat[!plotdat$score < (-5), ]

# Nice little quick plot
qplot(factor(score), data=plotdat, geom="bar", 
      fill=factor(name),
      xlab = "Sentiment Score",
	  ylab = "Twitter Text Counts")

# Or get funky with ggplot2 + Plotly
ep <- plotdat %>%
    ggplot(aes(x = score, fill = name)) +
    geom_histogram(binwidth = 1) +
    scale_fill_manual(values = c("#0167F7","#0200F7", "#12F701")) +
    theme_classic(base_size = 12) +
    scale_x_continuous(name = "Sentiment Score") +
    scale_y_continuous(name = "Text count of tweets") +
    ggtitle("Twitter Sentiment of Bollywood Khan: 2016")
    theme(axis.title.y = element_text(face="bold", colour="#000000", size=10),
          axis.title.x = element_text(face="bold", colour="#000000", size=8),
          axis.text.x = element_text(angle=16, vjust=0, size=8))
ggplotly(ep)
write.csv(thesrk, file = "srktweet.csv",row.names=FALSE)
write.csv(theaamir, file = "aamirtweet.csv",row.names=FALSE)
write.csv(thesalman, file= "salmantweet.csv",row.names=FALSE)
write.csv(plotdat, file= "sentimentkhan.csv",row.names=FALSE)