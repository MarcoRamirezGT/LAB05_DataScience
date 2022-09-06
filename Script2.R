### Analisis de texto
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library("syuzhet")
library("ggplot2")


db<- read.csv('train.csv')


db$text<-gsub("#[A-Za-z0-9]+|@[A-Za-z0-9]+|\\w+(?:\\.\\w+)*/\\S+", "", db$text)
db$text<-toupper(db$text)

db$text<-gsub("HTTP://","",db$text)
db$text<-gsub("HTTPS://","",db$text)
db$text<-gsub("[^\x01-\x7F]", "",db$text)
db$text<-gsub("B&amp;N", "",db$text)
db$text<-gsub("#", "",db$text)
db$text<-gsub('(s+)(A|AN|AND|THE|I)(s+)', '', db$text)
db$text<-gsub(':', '', db$text)
db$text<-gsub("'", '', db$text)
db$text<-gsub("--|", '', db$text)
db$text<-gsub('[[:punct:]]', '', db$text)
db$id<-toupper(db$id)
db$keyword<-toupper(db$keyword)
db$location<-toupper(db$location)


db[db == ""]<-NA

TextDoc <- Corpus(VectorSource(db$text))
#Replacing "/", "@" and "|" with space
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
TextDoc <- tm_map(TextDoc, toSpace, "/")
TextDoc <- tm_map(TextDoc, toSpace, "@")
TextDoc <- tm_map(TextDoc, toSpace, "\\|")
# Convert the text to lower case
TextDoc <- tm_map(TextDoc, content_transformer(tolower))
# Remove numbers
TextDoc <- tm_map(TextDoc, removeNumbers)
# Remove english common stopwords
TextDoc <- tm_map(TextDoc, removeWords, stopwords("english"))
# Remove your own stop word
# specify your custom stopwords as a character vector
TextDoc <- tm_map(TextDoc, removeWords, c("like", "just", "get",'will','new','now','via','dont','one')) 
# Remove punctuations
TextDoc <- tm_map(TextDoc, removePunctuation)
# Eliminate extra white spaces
TextDoc <- tm_map(TextDoc, stripWhitespace)
# Text stemming - which reduces words to their root form
TextDoc <- tm_map(TextDoc, stemDocument)

TextDoc_dtm <- TermDocumentMatrix(TextDoc)
dtm_m <- as.matrix(TextDoc_dtm)
# Sort by descearing value of frequency
dtm_v <- sort(rowSums(dtm_m),decreasing=TRUE)
dtm_d <- data.frame(word = names(dtm_v),freq=dtm_v)
# Display the top 5 most frequent words
head(dtm_d)
# Plot the most frequent words
barplot(dtm_d[1:5,]$freq, las = 2, names.arg = dtm_d[1:5,]$word,
        col ="lightgreen", main ="Top 5 most frequent words",
        ylab = "Word frequencies")
set.seed(1234)
wordcloud(words = dtm_d$word, freq = dtm_d$freq, min.freq = 5,
          max.words=100, random.order=FALSE, rot.per=0.40, 
          colors=brewer.pal(8, "Dark2"))
# findAssocs(TextDoc_dtm, terms = c("fire","amp","bomb"), corlimit = 0.25)			
# 
# findAssocs(TextDoc_dtm, terms = findFreqTerms(TextDoc_dtm, lowfreq = 50), corlimit = 0.25)

# regular sentiment score using get_sentiment() function and method of your choice
# please note that different methods may have different scales
syuzhet_vector <- get_sentiment(db$text, method="syuzhet")
# see the first row of the vector
head(syuzhet_vector)
# see summary statistics of the vector
summary(syuzhet_vector)

# bing
bing_vector <- get_sentiment(db$text, method="bing")
head(bing_vector)
summary(bing_vector)
#affin
afinn_vector <- get_sentiment(db$text, method="afinn")
head(afinn_vector)
summary(afinn_vector)

#compare the first row of each vector using sign function
rbind(
  sign(head(syuzhet_vector)),
  sign(head(bing_vector)),
  sign(head(afinn_vector))
)

d<-get_nrc_sentiment(db$text)
# head(d,10) - to see top 10 lines of the get_nrc_sentiment dataframe
head (d,10)


#transpose
td<-data.frame(t(d))
#The function rowSums computes column sums across rows for each level of a grouping variable.
td_new <- data.frame(rowSums(td[2:253]))
#Transformation and cleaning
names(td_new)[1] <- "count"
td_new <- cbind("sentiment" = rownames(td_new), td_new)
rownames(td_new) <- NULL
td_new2<-td_new[1:8,]
#Plot One - count of words associated with each sentiment
quickplot(sentiment, data=td_new2, weight=count, geom="bar", fill=sentiment, ylab="count")+ggtitle("Survey sentiments")


#Plot two - count of words associated with each sentiment, expressed as a percentage
barplot(
  sort(colSums(prop.table(d[, 1:8]))), 
  horiz = TRUE, 
  cex.names = 0.7, 
  las = 1, 
  main = "Emotions in Text", xlab="Percentage"
)


#Agregamos columna de sentimiento a cada tweet
db$Escala <- get_sentiment(db$text, method="bing")
View(db)
# 
# 
db$Sentimiento[db$Escala>=0]<-1
db$Sentimiento[db$Escala<0]<-0
# db$Sentimiento[db$Escala>-1 & db$Escala<1]<-'Neutro'


# Prediccion


library(ModelMetrics)
library(ggplot2)
library(caret)
library(dummy)
library(GGally)

porcentaje<-0.7
db$Sentimiento<-as.factor(db$Sentimiento)
db$text<-as.factor(db$text)

corte <- sample(nrow(db),nrow(db)*porcentaje)
train<-db[corte,]
test<-db[-corte,]

train<-train[complete.cases(train), ]
sum(is.na(train))
# 
mymodel<-glm(Sentimiento~.,data = train,family = 'binomial')


modelo<-lm(Sentimiento~., data = train,family = binomial())
p1<-predict(modelo, train, 
            type = 'response')
head(p1)
prediccion<-ifelse(p1>=0.5,1,0)

confusionMatrix(as.factor(test$Sentimiento),as.factor(prediccion))

# 
# pred<-predict(modelo,newdata = test, type = "response")
