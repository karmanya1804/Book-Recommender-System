library('tidyverse')
library("gridExtra")
#install.packages('DataExplorer')
library("DataExplorer")
library(magrittr)
library(ggplot2)
library(class)
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library(FNN) #To get indices of neighbours using knn



books<-read.csv("books.csv")
summary(books)
str(books)
#Drop bookID, isbn13
books <- select(books,-bookID , -isbn13) 
books %>% sapply(function(x) length(which(is.na(x))))
nrow(books)

# average rating distribution
ggplot(books,aes(x=average_rating)) +      
  geom_density(alpha= 0.5,fill='yellow')+ 
  geom_vline(aes(xintercept=mean(average_rating)), linetype='dashed')+
  geom_text(aes(x=4.1, y=.8, label='mean'), size=3)+
  ggtitle('Average Rating Destribution')

# page number distribution
np1<-ggplot(books, aes(x=num_pages)) + geom_histogram(bins = 30)+ ggtitle('Page Number Destribution')
np1

# Drop page numbers= 0 records.
books <-filter(books, books$num_pages>30)
summary(books$num_pages) 
# page number distribution
np2<ggplot(books, aes(x=log1p(num_pages))) + geom_density(fill='#76becc')+ ggtitle('Page Number Destribution')
np2
grid.arrange(np1,np2,nrow=1)

#Title
books$title %>% length -
  books$title %>% unique %>% length
books %>%
  group_by(title) %>%
  summarise(Number = n()) %>%
  arrange(desc(Number)) %>%
  head(n=20) %>% ggplot(aes(x=reorder(title, Number), y=Number, fill = Number)) +
  geom_col() +
  coord_flip() +
  labs(x="Title", y="Number of books")+
  ggtitle('Same title books')

books$authors %>% unique %>% length
d1 = books %>% select(authors) %>% 
  group_by(authors) %>% 
  summarise(count = n()) %>% arrange(desc(count)) %>% top_n(15)
plot_ly(d1, 
        x= ~reorder(authors, -count),
        y=~count,
        type= "bar",
        marker=list(color='tomato')) %>% 
  layout(title= "Most published authors", xaxis = list(title= 'Author'))

p1 <-ggplot(books, aes(x=num_pages, y=ratings_count))+ geom_point(na.rm=T) + theme_classic() + xlab("number of pages") + ylab("number of ratings") + ggtitle("Number of pages vs rating") +  theme(legend.position=0) 
p2 <-ggplot(books, aes(x=num_pages, y=text_reviews_count, color="#56B4E9"))+ geom_point(na.rm=T) + theme_classic() + xlab("number of pages") + ylab("number of reviews") + ggtitle("Number of pages vs reviews")+ theme(legend.position=0)
grid.arrange(p1, p2, ncol=2)


#language
lc<- books %>%
  group_by(language_code) %>%
  summarize(count = length(language_code)) %>%
  arrange(desc(count)) %>%
  ggplot(aes(x = reorder(language_code, count), y = count)) + geom_col(fill = "purple", alpha = 0.3) + 
  labs(x = "Languange Code", y = "Number of Books") + 
  ggtitle("Language Distribution")+
  coord_flip()
library(plyr)
# Language group
books$language_code<- revalue(books$language_code,c("en-US"="Eng","en-GB"="Eng","en-CA"="Eng","eng"="Eng",
                                                    "ara"="others",'cat'="others",'dan'="others",'ale'="others",
                                                    'gla'="others",'glg'="others",'grc'="others","others",
                                                    'ita'="others",'lat'='others','msa'='others','mul'='others',
                                                    'nl'='others','nor'='others','por'='others','rus'='others',
                                                    'srp'='others','swe'='others','tur'='others','wel'='others',
                                                    'zho'='others',"enm"="others","heb"="others",
                                                    'spa'='others','ger'='others','fre'='others','jpn'='others'))
detach("package:plyr")
# Plot
lct <- books %>%
  group_by(language_code) %>%
  summarize(count = length(language_code), perc = paste(round(count / nrow(books) * 100, 2),"%")) %>%
  arrange(desc(count)) %>%
  ggplot(aes(x = factor(language_code,levels = c('Eng','others')), y = count, label = perc)) +
  geom_col(fill = "#9932CD", alpha = 0.3) + 
  geom_label(nudge_y = 600) +
  labs(x = "Languange Code", y = "Number of Books") + theme_bw() +
  ggtitle("Language Distribution")
grid.arrange(lc, lct, nrow=1, top="Tranformed Language code distribution before& after")

# language VS rating
books %>% 
  ggplot(aes(x=average_rating, fill=language_code)) +
  geom_density(alpha=0.5) +
  geom_vline(aes(xintercept=round(mean(average_rating,2))), linetype='dashed')+
  ggtitle('Language x AverageRating') +
  geom_text(aes(x=4, y=1, label='mean'), size=3)+
  theme(legend.position="bottom")

#MODEL
books$LogNumberPages <- log1p(books$num_pages)
books$Logratings_count <- log1p(books$ratings_count)

# train & test
set.seed(1234)
row_number <- sample(1:nrow(books),0.75*nrow(books))
train_books <- books[row_number,]
test_books <- books[-row_number,]

# full model
lm.fit2 <- lm(average_rating~ LogNumberPages+Logratings_count+language_code, data = train)
summary(lm.fit2)
pred2 <- predict(lm.fit2,newdata = test,type = 'response')
RMSE2 <- sqrt(mean((test$average_rating-pred2)^2))
RMSE2

#K-MEANS CLUSTERING

trial<- books[, c("average_rating","ratings_count")]
head(trial)
k1<- kmeans(trial, centers=2, nstart=25)
str(k1)
k1

#Elbow curve to find optimal clusters
set.seed(123)

wss <- function(k) {
  kmeans(trial, k, nstart = 10 )$tot.withinss
}

k.values <- 2:30

wss_values <- map_dbl(k.values, wss)

plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

#Since elbow lies around k=5, we will take k=5
set.seed(123) 
final <- kmeans(trial, 5, nstart = 25)
print(final)

fviz_cluster(final, data = trial)

#Removing outliers
#max(trial$ratings_count)
#which.max(trial$ratings_count)
#trial<-trial[-c(10059),]

#set.seed(123)
#final <- kmeans(trial, 5, nstart = 25)
#print(final)

#fviz_cluster(final, data = trial)

clusters=final$cluster
books<-cbind(books, clusters)

trial_knn<-cbind(trial, clusters)

normalize <- function(x){
  return ((x-min(x))/(max(x)-min(x)))
}

books.new<- as.data.frame(lapply(trial_knn,normalize))
head(books.new)

library(caTools)

set.seed(12345)
split= sample.split(books.new$average_rating,SplitRatio = .75)
split

Training_set= subset(books.new,split == TRUE)
Test_set= subset(books.new,split == FALSE)

model<-knn(train=Training_set,test=Test_set,cl=Training_set[,3],k=6)
model
str(model)

cm= table(Test_set[,3],model)
cm

Accuracy=sum(diag(cm))/nrow(Test_set)
Accuracy

#To get indices of nearest neighbours
a<- get.knn(data=books.new[,1:2], k=6, algorithm = "kd_tree")
a
str(a)

a$nn.index
a$nn.dist

typeof(a)

#dataframe with books dataset combined with neighbours for each book
a_df<-data.frame(books, a$nn.index)
head(a_df)

name=readline(prompt="Enter name of the book: ")

#Function to print similar books
print_similar_books<- function(x) {
  target=which.max(a_df$title==x)
  target
  
  indices=a_df[target, 14:19]
  for (i in indices) {
    print(a_df$title[i])
  }
}
  
print_similar_books(name)
