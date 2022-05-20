
library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyr)
library(DT)
library(ggthemes)
library(wordcloud)
library(tm)
library(SnowballC)

##a. Read the YouTube stat from locations = CA, FR, GB, IN, US and prepare the data. 


IN_Video = tail(read.csv("D:/R/youtube/INvideos/INvideos.csv",encoding = "UTF-8"),20000)

head(IN_Video)

IN_Video$trending_date <- ydm(IN_Video$trending_date)
IN_Video$publish_time <- ydm(substr(IN_Video$publish_time, 
                                    start = 0, stop = 9))
tail(IN_Video)

CA_Video = tail(read.csv("D:/R/youtube/CAvideos/CAvideos.csv",encoding = "UTF-8"),20000)

head(CA_Video)

CA_Video$trending_date <- ydm(CA_Video$trending_date)
CA_Video$publish_time <- ydm(substr(CA_Video$publish_time, 
                                    start = 0, stop = 9))
tail(CA_Video)

FR_Video = tail(read.csv("D:/R/youtube/FRvideos/FRvideos.csv",encoding = "UTF-8"),20000)

head(FR_Video)

FR_Video$trending_date <- ydm(FR_Video$trending_date)
FR_Video$publish_time <- ydm(substr(FR_Video$publish_time, 
                                    start = 0, stop = 9))
tail(FR_Video)

US_Video = tail(read.csv("D:/R/youtube/USvideos/USvideos.csv",encoding = "UTF-8"),20000)

head(US_Video)

US_Video$trending_date <- ydm(US_Video$trending_date)
US_Video$publish_time <- ydm(substr(US_Video$publish_time, 
                                    start = 0, stop = 9))
tail(US_Video)

GB_Video = tail(read.csv("D:/R/youtube/GBvideos/GBvideos.csv",encoding = "UTF-8"),20000)

head(GB_Video)

GB_Video$trending_date <- ydm(GB_Video$trending_date)
GB_Video$publish_time <- ydm(substr(GB_Video$publish_time, 
                                    start = 1, stop = 10))
tail(GB_Video)

youtube_videos <- rbind(CA_Video,FR_Video,GB_Video,IN_Video,US_Video)
head(youtube_videos)

##b. Display the correlation plot between category_id, views, likes, dislikes, comment_count. Which two have stronger and weaker correlation

youtube <- youtube_videos[, 8:11]
groups <- youtube_videos[, 5]
head(youtube)

pairs(youtube, labels = colnames(youtube),
      pch = 21,
      bg = rainbow(3)[groups],
      col = rainbow(3)[groups])

library(corrplot)

corrplot(cor(youtube), method = 'number')
corrplot(cor(youtube), method = 'color')
corrplot(cor(youtube), method = 'pie')

##c. Display Top 10 most viewed videos of YouTube.

mostviewed <- head(youtube_videos %>%
                     group_by(video_id,title)%>%
                     dplyr::summarise(Total= sum(views))%>%
                     arrange(desc(Total)),10)
datatable(mostviewed)

ggplot(mostviewed,aes(video_id, Total)) + geom_bar(stat = "identity", fill="red")+ggtitle("Top most viewed videos")

##d. Show Top 10 most liked videos on YouTube

mostliked <- head(youtube_videos %>%
                     group_by(video_id,title)%>%
                     dplyr::summarise(Total= sum(likes))%>%
                     arrange(desc(Total)),10)
datatable(mostliked)

ggplot(mostliked,aes(video_id, Total)) + geom_bar(stat = "identity", fill="red")+ggtitle("Top most liked videos")

##e. Show Top 10 most disliked videos on YouTube.

mostdisliked <- head(youtube_videos %>%
                    group_by(video_id,title)%>%
                    dplyr::summarise(Total= sum(dislikes))%>%
                    arrange(desc(Total)),10)
datatable(mostdisliked)

ggplot(mostdisliked,aes(video_id, Total)) + geom_bar(stat = "identity", fill="red")+ggtitle("Top most disliked videos")


##f. Show Top 10 most commented video of YouTube

mostcommented <- head(youtube_videos %>%
                       group_by(video_id,title)%>%
                       dplyr::summarise(Total= sum(comment_count))%>%
                       arrange(desc(Total)),10)
datatable(mostcommented)

ggplot(mostcommented,aes(video_id, Total)) + geom_bar(stat = "identity", fill="blue")+ggtitle("Top 10 most commented videos")

##g. Show Top 15 videos with maximum percentage (%) of Likes on basis of views on video.
#Hint: round (100* max (likes, na.rm = T)/ max (views, na.rm = T), digits = 2)) 

max_like <- head(youtube_videos %>%
                   group_by(video_id,title) %>%
                   dplyr::summarise(Total= 
                    round(100*max(likes, na.rm = T)/max(views, na.rm = T))) %>%
                    arrange(desc(Total)) ,15)
datatable(max_like)

ggplot(max_like, aes(video_id, Total)) + geom_bar(stat = "identity", fill="blue")+
  ggtitle("Top 15 videos with max percentage of likes")

##h. Show Top 15 videos with maximum percentage (%) of Dislikes on basis of views on video.

max_dislike <- head(youtube_videos %>%
                   group_by(video_id,title) %>%
                   dplyr::summarise(Total= 
                                      round(100*max(dislikes, na.rm = T)/max(views, na.rm = T))) %>%
                   arrange(desc(Total)) ,15)
datatable(max_dislike)

ggplot(max_dislike, aes(video_id, Total)) + geom_bar(stat = "identity", fill="blue")+
  ggtitle("Top 15 videos with max percentage of dislikes")


##i. Show Top 15 videos with maximum percentage (%) of Comments on basis of views on video.

max_comment <- head(youtube_videos %>%
                   group_by(video_id,title) %>%
                   dplyr::summarise(Total= 
                                      round(100*max(comment_count, na.rm = T)/max(views, na.rm = T))) %>%
                   arrange(desc(Total)) ,15)
datatable(max_comment)

ggplot(max_comment, aes(video_id, Total)) + geom_bar(stat = "identity", fill="blue")+
  ggtitle("Top 15 videos with max percentage of comments")


##j. Top trending YouTube channels in all countries

Trending <- head(youtube_videos %>%
                       group_by(channel_title,video_id)%>%
                       dplyr::summarise(Total= sum(views))%>%
                       arrange(desc(Total)), 10)
datatable(Trending)

ggplot(Trending,aes(channel_title, Total)) + geom_bar(stat = "identity", fill="red")+ggtitle("Top trending channels in all countries")


##k. Top trending YouTube channels in India.

Trending <- head(IN_Video %>%
                   group_by(channel_title,video_id)%>%
                   dplyr::summarise(Total= sum(views))%>%
                   arrange(desc(Total)), 10)
datatable(Trending)

ggplot(Trending,aes(channel_title, Total)) + geom_bar(stat = "identity", fill="red")+ggtitle("Top trending channels in india")

##l. Create a YouTube Title Wordcloud 

corpus = Corpus(VectorSource(list(sample(youtube_videos$title, size = 3000))))

corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, content_transformer(tolower))
corpus = tm_map(corpus, removeNumbers)
corpus = tm_map(corpus, stripWhitespace)
corpus = tm_map(corpus, removeWords, stopwords('english'))

dtm_us = TermDocumentMatrix(corpus)
matrix <- as.matrix(dtm_us)

words <- sort(rowSums(matrix), decreasing = TRUE)
df <- data.frame(word = names(words), freq = words)

head(df)

wordcloud(words = df$word, freq = df$freq, min.freq = 5, 
          random.order = FALSE, colors = brewer.pal(6, "Dark2"))


##m. Show Top Category ID

top_category <- head(youtube_videos %>%
                       group_by(category_id) %>%
                       dplyr::summarise(Total=n()) %>%
                       arrange(desc(Total)), 10)
datatable(top_category)

ggplot(top_category, aes(category_id, Total)) + geom_bar(stat = "identity", fill=rainbow(10))+
          ylab("category id") + ggtitle("Top category ID")

##n. How much time passes between published and trending?

pub_tre <- head(youtube_videos %>%
                  group_by(video_id) %>%
                  dplyr::summarise(Total = difftime(publish_time, 
                  trending_date,units = "days")) %>%
                  arrange(Total), 10)

head(pub_tre, 10)

datatable(pub_tre)

ggplot(pub_tre, aes(video_id, Total)) + geom_bar(stat = "identity", fill="blue") + 
  ylab("difference") + scale_y_continuous() + 
  ggtitle("Time passes between published and trending date")
  

##o. Show the relationship plots between Views Vs. Likes on Youtube.

plot(x = youtube_videos$views , y = youtube_videos$likes,
     pch = 16, col = "red", 
     xlab = "views",       # For x-label
     ylab = "likes",       # For y-label
     main = "Scatter Plot")

##p. Top Countries In total number of Views in absolute numbers

countries <- c("CA_Video","FR_Video","GB_Video","IN_Video","US_Video")
views <- c(sum(CA_Video$views), sum(FR_Video$views), sum(GB_Video$views), sum(IN_Video$views), sum(US_Video$views))
likes <- c(sum(CA_Video$likes), sum(FR_Video$likes), sum(GB_Video$likes), sum(IN_Video$likes), sum(US_Video$likes))
dislikes <- c(sum(CA_Video$dislikes), sum(FR_Video$dislikes), sum(GB_Video$dislikes), sum(IN_Video$dislikes), sum(US_Video$dislikes))
comments <- c(sum(CA_Video$comment_count), sum(FR_Video$comment_count), sum(GB_Video$comment_count), sum(IN_Video$comment_count), sum(US_Video$comment_count))

Top_countries <- data.frame(countries, views, likes, dislikes, comments)
Top_countries


##p

top_views <- head(Top_countries %>%
                    group_by(countries) %>%
                    arrange(desc(views)))
datatable(top_views)

ggplot(top_views, aes(x=countries, y=views)) + geom_bar(stat = "identity", fill="blue") + 
              ggtitle("Top countries in total number of views in absolute numbers")

##q. Top Countries In total number of Likes in absolute numbers

top_likes <- head(Top_countries %>%
                    group_by(countries) %>%
                    arrange(desc(likes)))
datatable(top_likes)

ggplot(top_likes, aes(x=countries, y=likes)) + geom_bar(stat = "identity", fill="blue") + 
  ggtitle("Top countries in total number of likes in absolute numbers")


##r. Top Countries In total number of Dislikes in absolute numbers

top_dislikes <- head(Top_countries %>%
                    group_by(countries) %>%
                    arrange(desc(dislikes)))
datatable(top_dislikes)

ggplot(top_views, aes(x=countries, y=dislikes)) + geom_bar(stat = "identity", fill="blue") + 
  ggtitle("Top countries in total number of dislikes in absolute numbers")


##s. Top Countries In total number of Comments in absolute numbers

top_comments <- head(Top_countries %>%
                    group_by(countries) %>%
                    arrange(desc(comments)))
datatable(top_comments)

ggplot(top_views, aes(x=countries, y=comments)) + geom_bar(stat = "identity", fill="blue") + 
  ggtitle("Top countries in total number of comments in absolute numbers")


##t. Title length words Frequency Distribution 

freq_dis <- youtube_videos %>%
              group_by(title) %>%
                summarise(counts = n())
datatable(freq_dis)


words <- sort(rowSums(matrix), decreasing = TRUE)
df <- data.frame(word = names(words), freq = words)

wordcloud(words = df$word, freq = df$freq, min.freq = 5, 
          random.order = FALSE, colors = brewer.pal(6, "Dark2"))

ggplot(top_views, aes(x=countries, y=comments)) + geom_bar(stat = "identity", fill="blue") + 
  ggtitle("Top countries in total number of comments in absolute numbers")

    
    
    
    


