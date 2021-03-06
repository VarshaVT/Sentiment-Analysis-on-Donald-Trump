
##Sentiment Analysis on Donald Trump using R and Tableau
##
##url : http://datascienceplus.com/sentiment-analysis-on-donald-trump-using-r-and-tableau/

library(twitteR)
library(ROAuth)
require(RCurl)
library(stringr)
library(tm)
library(ggmap)
library(dplyr)
library(plyr)
library(tm)
library(wordcloud)

key="QwlmLQOZGIWA30nzv4a2mDQG2"
secret="gotVQzBfyHsEyUZFCrm2DuR3v3ygnN8WtfC5kQK0UzPq4QUcB2"

download.file(url="http://curl.haxx.se/ca/cacert.pem",
              destfile="C:\\Users\\Varsha\\OneDrive\\pro\\cacert.pem",
              method="auto")

authenticate <- OAuthFactory$new(consumerKey='QwlmLQOZGIWA30nzv4a2mDQG2',
                                 consumerSecret='gotVQzBfyHsEyUZFCrm2DuR3v3ygnN8WtfC5kQK0UzPq4QUcB2',
                                 requestURL="https://api.twitter.com/oauth/request_token",
                                 accessURL="htQwlmLQOZGIWA30nzv4a2mDQG2tps://api.twitter.com/oauth/access_token",
                                 authURL="https://api.twitter.com/oauth/authorize")
accessToken = "703082370912530432-lY4fP43LFl2Y3pISIPSdgxHrfykUBhK" # your access_token 
accessTokenSecret = "pvoe2hIeAlW5c5tEnevaGvf1OUM9ZLGT330eGFjZ8DvEe" #your access_token_sceret 

cred$handshake(cainfo = system.file('CurlSSL', 'cacert.pem', package = 'RCurl'))
#There is URL in Console. You need to go to it, get code and enter it on Console

setup_twitter_oauth(key,secret,accessToken,accessTokenSecret)

save(authenticate, file="twitter authentication.Rdata")


#Let's scrape most recent tweets from various cities across the US. Let's request 2000 tweets from each city. 
#    We will need the latitude and longitude of each city

# tweets to request from each query
N = 2000;

# radius in miles
S = 200;

latitude=c(38.9,40.7,37.8,39,37.4,28,30,42.4,48,36,32.3,33.5,34.7,33.8,37.2,41.2,46.8,
       46.6,37.2,43,42.7,40.8,36.2,38.6,35.8,40.3,43.6,40.8,44.9,44.9)

longitude=c(-77,-74,-122,-105.5,-122,-82.5,-98,-71,-122,-115,-86.3,-112,-92.3,-84.4,-93.3,
       -104.8,-100.8,-112, -93.3,-89,-84.5,-111.8,-86.8,-92.2,-78.6,-76.8,-116.2,-98.7,-123,-93)

#cities=DC,New York,San Fransisco,Colorado,Mountainview,Tampa,Austin,Boston,
#       Seatle,Vegas,Montgomery,Phoenix,Little Rock,Atlanta,Springfield,
#       Cheyenne,Bisruk,Helena,Springfield,Madison,Lansing,Salt Lake City,Nashville
#       Jefferson City,Raleigh,Harrisburg,Boise,Lincoln,Salem,St. Paul


donald=do.call(rbind,
               lapply(1:length(lats), 
                      function(i) searchTwitter('Donald+Trump',
                      lang="en",n=N,resultType="recent",geocode=paste(lats[i],lons[i],
                      paste0(S,"mi"),sep=","))))


#Let's get the latitude and longitude of each tweet, the tweet itself, 
#how many times it was re-twitted and favorited, the date and time it was twitted, etc.

donaldlat=sapply(donald, function(x) as.numeric(x$getLatitude()))
donaldlat=sapply(donaldlat, function(z) ifelse(length(z)==0,NA,z))

donaldlon=sapply(donald, function(x) as.numeric(x$getLongitude()))
donaldlon=sapply(donaldlon, function(z) ifelse(length(z)==0,NA,z))

donalddate=lapply(donald, function(x) x$getCreated())
donalddate=sapply(donalddate,function(x) strftime(x, format="%Y-%m-%d %H:%M:%S",tz = "UTC"))

donaldtext=sapply(donald, function(x) x$getText())
donaldtext=unlist(donaldtext)

isretweet=sapply(donald, function(x) x$getIsRetweet())
retweeted=sapply(donald, function(x) x$getRetweeted())
retweetcount=sapply(donald, function(x) x$getRetweetCount())

favoritecount=sapply(donald, function(x) x$getFavoriteCount())
favorited=sapply(donald, function(x) x$getFavorited())

data=as.data.frame(cbind(tweet=donaldtext,date=donalddate,lat=donaldlat,lon=donaldlon,
                         isretweet=isretweet,retweeted=retweeted, retweetcount=retweetcount,
                         favoritecount=favoritecount,favorited=favorited))

#First, let's create a word cloud of the tweets. A word cloud helps us to visualize 
#the most common words in the tweets 
#and have a general feeling of the tweets.

# Create corpus
corpus=Corpus(VectorSource(data$tweet))

# Convert to lower-case
corpus=tm_map(corpus, content_transformer(tolower))

corpus = tolower(corpus)

# Remove stopwords
corpus=tm_map(corpus,function(x) removeWords(x,stopwords()))

# convert corpus to a Plain Text Document
corpus=tm_map(corpus,PlainTextDocument)

col=brewer.pal(6,"Dark2")

wordcloud(corpus, min.freq=25, scale=c(5,2),rot.per = 0.25,
          random.color=T, max.word=45, random.order=F,colors=col)

corpus <- sapply(corpus,function(row) iconv(row, "latin1", "ASCII", sub=""))

data=filter(data, !is.na(lat),!is.na(lon))
lonlat=select(data,lon,lat)

result <- do.call(rbind, lapply(1:nrow(lonlat),
                                function(i) revgeocode(as.numeric(lonlat[i,1:2]))))


#So, we will apply some regular expression and string manipulation to 
#separate the city, zip code and state into different columns.

data2=lapply(result,  function(x) unlist(strsplit(x,",")))
address=sapply(data2,function(x) paste(x[1:3],collapse=''))
city=sapply(data2,function(x) x[2])
stzip=sapply(data2,function(x) x[3])
zipcode = as.numeric(str_extract(stzip,"[0-9]{5}"))   
state=str_extract(stzip,"[:alpha:]{2}")
data2=as.data.frame(list(address=address,city=city,zipcode=zipcode,state=state))

#Concatenate data2 to data:
data=cbind(data,data2)

#Some text cleaning:
tweet=data$tweet
tweet_list=lapply(tweet, function(x) iconv(x, "latin1", "ASCII", sub=""))
tweet_list=lapply(tweet_list, function(x) gsub("htt.*",' ',x))
tweet=unlist(tweet_list)
data$tweet=tweet

#We will use lexicon based sentiment analysis. A list of positive and negative 
#opinion words or sentiment words for English was downloaded from here.

positives= readLines("positivewords.txt")
negatives = readLines("negativewords.txt")

#First, let's have a wrapper function that calculates sentiment scores.

scores = function(tweets, positive_words, negative_words, .progress='none'){
  scores = laply(tweets,
                 function(tweet, positive_words, negative_words){
                   tweet = gsub("[[:punct:]]", "", tweet)    # remove punctuation
                   tweet = gsub("[[:cntrl:]]", "", tweet)   # remove control characters
                   tweet = gsub('\\d+', '', tweet)          # remove digits
                   
                   # Let's have error handling function when trying tolower
                   tryTolower = function(x){
                     # create missing value
                     y = NA
                     # tryCatch error
                     try_error = tryCatch(tolower(x), error=function(e) e)
                     # if not an error
                     if (!inherits(try_error, "error"))
                       y = tolower(x)
                     # result
                     return(y)
                   }
                   # use tryTolower with sapply
                   tweet = sapply(tweet, tryTolower)
                   # split sentence into words with str_split function from stringr package
                   word_list = str_split(tweet, "\\s+")
                   words = unlist(word_list)
                   # compare words to the dictionaries of positive & negative terms
                   positive_matches = match(words, positive_words)
                   negative_matches = match(words, negative_words)
                   # get the position of the matched term or NA
                   # we just want a TRUE/FALSE
                   positive_matches = !is.na(positive_matches)
                   negative_matches = !is.na(negative_matches)
                   # final score
                   score = sum(positive_matches) - sum(negative_matches)
                   return(score)
                 }, positive_matches, negative_matches, .progress=.progress )
  return(scores)
}

score = sentiment_scores(tweet, positives, negatives, .progress='text')
data$score = score

#Let's plot a histogram of the sentiment score:
hist(score,xlab=" ",main="Sentiment of sample tweets\n that have Donald Trump in them ",
       border="black",col="skyblue")


