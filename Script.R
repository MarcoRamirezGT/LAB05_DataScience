library(wordcloud2)
library(ggplot2)



db<- read.csv('train.csv')

View(db)
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
View(db)

freq<-table(db$keyword, useNA = 'no')

wordcloud2(data = freq, size = 0.15, shape = "cloud",
           color="random-dark", ellipticity = 0.5)

hist(x = freq, main = "Histograma De Palabras Más Repetidas", 
     xlab = "Frecuencia", ylab = "",
     col = "ivory")
no_disaster<-subset(x = db, subset = target == 0, select = c("keyword"))

freq_no_disaster<-table(no_disaster$keyword)
tabla_ordenada1<-freq_no_disaster[order(freq_no_disaster, decreasing = TRUE, na.last = TRUE)]
#View(tabla_ordenada1)
h1<-head(tabla_ordenada1)
h1<-as.data.frame(h1)

wordcloud2(data = freq_no_disaster, size = 0.2, shape = "cloud",
           color="random-dark", ellipticity = 0.5)



ggplot(data=h1, aes(x=Var1, y=Freq, fill=Var1)) +
  geom_bar(stat="identity", position=position_dodge())+
  geom_text(aes(label=as.integer(Freq)), vjust=1.6, color="black",
            position = position_dodge(0.9), size=3.5)+
  labs(title="No Disaster Tweets",x='Palabra', y="Frecuencia")+
  theme(legend.position="none")

disaster<-subset(x = db, subset = target == 1, select = c("keyword"))

freq_disaster<-table(disaster$keyword)
tabla_ordenada2<-freq_disaster[order(freq_disaster, decreasing = TRUE, na.last = TRUE)]

h2<-head(tabla_ordenada2)
h2<-as.data.frame(h2)

ggplot(data=h2, aes(x=Var1, y=Freq, fill=Var1)) +
  geom_bar(stat="identity", position=position_dodge())+
  geom_text(aes(label=as.integer(Freq)), vjust=1.6, color="black",
            position = position_dodge(0.9), size=3.5)+
  labs(title="Disaster Tweets",x='Palabra', y="Frecuencia")+
  theme(legend.position="none")

wordcloud2(data = freq_disaster, size = 0.2, shape = "cloud",
           color="random-dark", ellipticity = 0.5)





