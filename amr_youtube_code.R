# 0.0. Datasets used

df_final
df_interviewee
df_topic

# 0.1. Install packages

install.packages("httr")
library(httr)
install.packages("httr2")
library(httr2) #Communicate with API and get the raw data from YouTube in JSON format
install.packages("jsonlite")
library(jsonlite) # Convert JSON data into readable format
install.packages("dplyr") # For manipulating data
library(dplyr)
install.packages("here")
library(here)
install.packages("ggplot2")
library("ggplot2")
install.packages("tidyverse")
library("tidyverse")
install.packages("rstatix")
library("rstatix")
install.packages("FSA")
library("FSA")
install.packages("patchwork")
library("patchwork")
install.packages("corrplot")
library("corrplot")
install.packages("irr")
library("irr")
install.packages("MASS")
library(MASS)

# 1.0. YouTube information download with Google API 

###API call
## {Reousce}: information we want to extract from Yt
# cHANNELS
# PlaylistItems
# Videos

## {Parameters}: further customize the results
# Key(required)
# id, forUsername, playlistId
# Part for the specific data points

key <- "" ##Generated from Google Cloud API Key ##
channel_id <- "UCcR-kuK5lpUvUs9jTEjQWsg"
base <- "https://www.googleapis.com/youtube/v3/"

required_packages <- c("httr", "jsonlite", "here", "dplyr")
for(i in required_packages) {
  if(!require(i, character.only = T)) {
    #  if package is not existing, install then load the package
    install.packages(i, dependencies = T, repos = "http://cran.us.r-project.org")
    # install.packages(i, dependencies = T, repos = "https://cran.stat.upd.edu.ph/")
    require(i, character.only = T)
  }
}

# Construct the API call
api_params <- 
  paste(paste0("key=", key), 
        paste0("id=", channel_id), 
        "part=snippet,contentDetails,statistics",
        sep = "&")
api_call <- paste0(base, "channels", "?", api_params)
api_result <- GET(api_call)
json_result <- content(api_result, "text", encoding="UTF-8")

# Process the raw data into a data frame
channel.json <- fromJSON(json_result, flatten = T)
channel.df <- as.data.frame(channel.json)
channel.df

playlist_id <- "PLridrslPI_sFZkhhkUzxbUMDpO-S9hGVY"

# temporary variables
nextPageToken <- ""
upload.df <- NULL
pageInfo <- NULL

# Loop through the playlist while there is still a next page
while (!is.null(nextPageToken)) {
  # Construct the API call
  api_params <- 
    paste(paste0("key=", key), 
          paste0("playlistId=", playlist_id), 
          "part=snippet,contentDetails,status",
          "maxResults=300",
          sep = "&")
  
  # Add the page token for page 2 onwards
  if (nextPageToken != "") {
    api_params <- paste0(api_params,
                         "&pageToken=",nextPageToken)
  }
  
  api_call <- paste0(base, "playlistItems", "?", api_params)
  api_result <- GET(api_call)
  json_result <- content(api_result, "text", encoding="UTF-8")
  upload.json <- fromJSON(json_result, flatten = T)
  
  nextPageToken <- upload.json$nextPageToken
  pageInfo <- upload.json$pageInfo
  
  curr.df <- as.data.frame(upload.json$items)
  if (is.null(upload.df)) {
    upload.df <- curr.df
  } else {
    upload.df <- bind_rows(upload.df, curr.df)
  }
}

video.df<- NULL
# Loop through all uploaded videos
for (i in 1:nrow(upload.df)) {
  # Construct the API call
  video_id <- upload.df$contentDetails.videoId[i]
  api_params <- 
    paste(paste0("key=", key), 
          paste0("id=", video_id), 
          "part=id,statistics,contentDetails",
          sep = "&")
  
  api_call <- paste0(base, "videos", "?", api_params)
  api_result <- GET(api_call)
  json_result <- content(api_result, "text", encoding="UTF-8")
  video.json <- fromJSON(json_result, flatten = T)
  
  curr.df <- as.data.frame(video.json$items)
  
  if (is.null(video.df)) {
    video.df <- curr.df
  } else {
    video.df <- bind_rows(video.df, curr.df)
  }
}  

# Combine all video data frames
video.df$contentDetails.videoId <- video.df$id
video_final.df2 <- merge(x = upload.df, 
                         y = video.df,
                         by = "contentDetails.videoId")

video_final.df2 <- data.frame(video_final.df2)
video_final.df2 <- apply(video_final.df2,2,as.character)

write.csv(video_final.df2, "",row.names=FALSE) #File path

## 2.0. Channel regions 

Reorder(table(df_final$channel_region))
region_table <- table(df_final$channel_region)
order_region_table <- region_table[order(-region_table)]
order_region_table


## 3.0. Channel types vs. outcomes 

# 3.1. Converting channel type names into abbreviated versions
df_final$channel_type_abr <- ifelse(df_final$channel_type_final == "Broadcasting agencies","Broadcast",
                                    ifelse(df_final$channel_type_final == "Education by non-medical professionals (e.g. science education or explanatory media)","Non-Med-Ed",                                            
                                           ifelse(df_final$channel_type_final == "Educational by medical professionals", "Med-Ed",
                                                  ifelse(df_final$channel_type_final =="Internet media (e.g. newsmagazine show or talk shows)","Internet media",
                                                         ifelse(df_final$channel_type_final == "Non-profit or medical organizations (e.g. hospitals, government organizations, universities)","Non-profit",
                                                                ifelse(df_final$channel_type_final == "Independent non-medical users (e.g. vloggers with no obvious affiliations)","Independent",
                                                                       "others"))))))


table(df_final$channel_type_abr)

# 3.2. Inidividual dataframes per outcome

channel_df_view <- df_final %>%
  select(channel_type_final,channel_type_abr,view_count)
channel_df_view <- channel_df_view[complete.cases(channel_df_view),]

table(channel_df_view$channel_type_abr)

channel_df_viewday <- df_final %>%
  select(channel_type_final,channel_type_abr,views_per_day)

channel_df_viewday<- channel_df_viewday[complete.cases(channel_df_viewday),]

table(channel_df_viewday$channel_type_abr)

channel_df_like <- df_final %>%
  select(channel_type_final,channel_type_abr,like_count)

channel_df_like <- channel_df_like[complete.cases(channel_df_like),]

table(channel_df_like$channel_type_abr)

channel_df_comment <- df_final %>%
  select(channel_type_final,channel_type_abr,comment_count)

channel_df_comment <- channel_df_comment[complete.cases(channel_df_comment),]

table(channel_df_comment$channel_type_abr)

library(FSA)

# 3.3. Kruskal-Wallis test

channel_df_kruskal_view <- kruskal.test(view_count ~ channel_type_abr, data = channel_df_view)

channel_df_kruskal_view


###Dunn not needed ### channel_df_view_dunn <- dunnTest(view_count ~ channel_type_abr, data = channel_df_view, method = "bonferroni")

channel_df_kruskal_viewday <- kruskal.test(views_per_day ~ channel_type_abr, data = channel_df_viewday)

channel_df_kruskal_viewday

channel_df_viewday_dunn <- dunnTest(views_per_day ~ channel_type_abr, data = channel_df_viewday, method = "bonferroni")

channel_df_kruskal_like <- kruskal.test(like_count ~ channel_type_abr, data = channel_df_like)
channel_df_kruskal_like

channel_df_like_dunn <- dunnTest(like_count ~ channel_type_abr, data = channel_df_like, method = "bonferroni")
channel_df_like_dunn

channel_df_kruskal_comment <- kruskal.test(comment_count ~ channel_type_abr, data = channel_df_comment)

channel_df_kruskal_comment

channel_df_comment_dunn <- dunnTest(comment_count ~ channel_type_abr, data = channel_df_comment, method = "bonferroni")
channel_df_comment_dunn


# 3.4. Plotting channel types vs. engagement graphs
chr_a1.1 <- channel_df_view %>%
  group_by(channel_type_abr) %>%
  mutate(count=n()) %>%
  mutate(mean = mean(view_count)) %>%
  ggplot(aes(y =view_count, x =channel_type_abr))+
  geom_boxplot(fill = "lightpink")+
  scale_y_log10(breaks = c(1000, 10000, 100000,1000000, 10000000), labels = expression(10^3, 10^4,10^5,10^6, 10^7)) + labs(x = "Channel type", y = "View count")+
  annotate("text",x = 1.6, y = 10000000,size = 7, label = paste("p-value = 0.070")) + theme_classic2() + theme(
    axis.title.x =  element_text(size = 18),
    axis.title.y = element_text(size = 18),
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16)) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  NULL 

chr_a1.1

chr_a2.1 <- channel_df_viewday %>%
  group_by(channel_type_abr) %>%
  mutate(count=n()) %>%
  mutate(mean = mean(views_per_day)) %>%
  ggplot(aes(y =views_per_day, x =channel_type_abr))+
  geom_boxplot(fill = "lightyellow")+
  scale_y_log10(breaks = c(0.1, 1, 10,100, 1000,10000), labels = expression(0.1, 1,10,10^2, 10^3, 10^4)) + labs(x = "Channel type", y = "Views per day")+
  annotate("text",x = 1.7, y = 50000,size = 7, label = paste("p-value = ", round(channel_df_kruskal_views_day$p.value,3)))+ theme_classic2() + theme(
    axis.title.x =  element_text(size = 18),
    axis.title.y = element_text(size = 18),
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16)) +  
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  NULL 

chr_a2.1

chr_a3.1 <- channel_df_like %>%
  group_by(channel_type_abr) %>%
  mutate(count=n()) %>%
  mutate(mean = mean(like_count)) %>%
  ggplot(aes(y =like_count, x =channel_type_abr))+
  geom_boxplot(fill = "lightblue")+
  scale_y_log10(breaks = c(10,100,1000,10000, 100000), labels = expression(10,10^2,10^3,10^4, 10^5)) + labs(x = "Channel type", y = "Like count")+
  annotate("text",x = 1.7, y = 450000,size = 7, label = paste("p-value = ", round(channel_df_kruskal_like$p.value,3)))+
  theme_classic2() + theme(
    axis.title.x =  element_text(size = 18),
    axis.title.y = element_text(size = 18),
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16)) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  NULL

chr_a3.1

chr_a4.1 <- channel_df_comment %>%
  group_by(channel_type_abr) %>%
  mutate(count=n()) %>%
  mutate(mean = mean(comment_count)) %>%
  ggplot(aes(y =comment_count, x =channel_type_abr))+
  geom_boxplot(fill = "lightgreen")+
  scale_y_log10(breaks = c(1, 10, 100,1000, 10000), labels = expression(1, 10,10^2,10^3, 10^4)) + labs(x = "Channel type", y = "Comment count")+
  annotate("text",x = 1.7, y = 20000,size = 7, label = paste("p-value = ", round(channel_df_kruskal_comment$p.value,3)))+
  theme_classic2() + theme(
    axis.title.x =  element_text(size = 18),
    axis.title.y = element_text(size = 18),
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16)) +  
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  NULL

chr_a4.1

#Combined plot#
chr_a1.1+chr_a2.1+chr_a3.1+chr_a4.1 + plot_annotation(tag_levels ='I')


## 4.0. Video style
# 4.1. Converting video style names into abbreviated versions
df_final$video_style_abr <-  ifelse(df_final$video_style_final == "Hosted (communicator presents information while other people also part of the content)","Hosted",
                                    ifelse(df_final$video_style_final == "Interview (where the person delivering the content is being interviewed by a person off camera)","Interview",
                                           ifelse(df_final$video_style_final == "Presentation (when presenting to an audience)", "Presentation",
                                                  ifelse(df_final$video_style_final =="Speech","Speech",
                                                         ifelse(df_final$video_style_final == "Text over visuals","Text over visuals",
                                                                ifelse(df_final$video_style_final == "Vlog (where the presenter delivers content by talking directly to the camera)","Vlog",
                                                                       ifelse(df_final$video_style_final == "Voice over visual","Voice over visuals",
                                                                              ifelse(df_final$video_style_final == "webinar","webinar",
                                                                                     ifelse(df_final$video_style_final == "Zoom meeting interview style","Zoom",NA)))))))))

table(df_final$video_style_abr)

# 4.2. dataframes for individual outcomes
style_df_view <- df_final %>%
  select(video_style_final,video_style_abr,view_count)
style_df_view <- style_df_view[complete.cases(style_df_view),]

table(style_df_view$video_style_abr)

style_df_viewday <- df_final %>%
  select(video_style_final,video_style_abr,views_per_day)

style_df_viewday<- style_df_viewday[complete.cases(style_df_viewday),]

table(style_df_viewday$video_style_abr)

style_df_like <- df_final %>%
  select(video_style_final,video_style_abr,like_count)

style_df_like <- style_df_like[complete.cases(style_df_like),]

table(style_df_like$video_style_abr)

style_df_comment <- df_final %>%
  select(video_style_final,video_style_abr,comment_count)

style_df_comment <- style_df_comment[complete.cases(style_df_comment),]

table(channel_df_comment$channel_type_abr)


# 4.3. Kruskal-Wallis test

style_df_kruskal_view <- kruskal.test(view_count ~ video_style_abr, data = style_df_view)

style_df_kruskal_view

###Not needed ### style_df_view_dunn <- dunnTest(view_count ~ video_style_abr, data = style_df_view, method = "bonferroni")


style_df_kruskal_views_day <- kruskal.test(views_per_day ~ video_style_abr, data = style_df_viewday)

style_df_kruskal_views_day

###Not needed ### style_df_views_day_dunn <- dunnTest(views_per_day ~ video_style_abr, data = style_df_viewday, method = "bonferroni")

style_df_kruskal_like <- kruskal.test(like_count ~ video_style_abr, data = style_df_like)

style_df_kruskal_like

### Not needed ### style_df_like_dunn <- dunnTest(like_count ~ video_style_abr, data = style_df_like, method = "bonferroni")

style_df_kruskal_comment <- kruskal.test(comment_count ~ video_style_abr, data = style_df_comment)

style_df_kruskal_comment

### Not needed ### style_df_comment_dunn <- dunnTest(comment_count ~ video_style_abr, data = style_df_comment, method = "bonferroni")

#4.4.Plot graphs

##Plot graph
chr_b1.1 <- style_df_view %>%
  group_by(video_style_abr) %>%
  mutate(count=n()) %>%
  mutate(mean = mean(view_count)) %>%
  ggplot(aes(y =view_count, x =video_style_abr))+
  geom_boxplot(fill = "lightpink")+
  scale_y_log10(breaks = c(1000, 10000, 100000,1000000, 10000000), labels = expression(10^3, 10^4,10^5,10^6, 10^7)) + labs(x = "Video style", y = "View count")+
  annotate("text",x = 1.7, y = 10000000,size = 7, label = paste("p-value = ", round(style_df_kruskal_view$p.value,3)))+
  theme_classic2() + theme(
    axis.title.x =  element_text(size = 18),
    axis.title.y = element_text(size = 18),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 16))+
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  NULL
chr_b1.1

chr_b2.1 <- style_df_viewday %>%
  group_by(video_style_abr) %>%
  mutate(count=n()) %>%
  mutate(mean = mean(views_per_day)) %>%
  ggplot(aes(y =views_per_day, x =video_style_abr))+
  geom_boxplot(fill = "lightyellow")+
  scale_y_log10(breaks = c(0.1, 1, 10,100, 1000,10000), labels = expression(0.1, 1,10,10^2, 10^3, 10^4)) + labs(x = "Video style", y = "Views per day")+
  annotate("text",x = 1.7, y = 5000,size = 7, label = paste("p-value = ", round(style_df_kruskal_views_day$p.value,3)))+
  theme_classic2() + theme(
    axis.title.x =  element_text(size = 18),
    axis.title.y = element_text(size = 18),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 16))+
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  NULL

chr_b2.1

chr_b3.1 <- style_df_like %>%
  group_by(video_style_abr) %>%
  mutate(count=n()) %>%
  mutate(mean = mean(like_count)) %>%
  ggplot(aes(y =like_count, x =video_style_abr))+
  geom_boxplot(fill = "lightblue")+
  scale_y_log10(breaks = c(10,100,1000,10000, 100000), labels = expression(10,10^2,10^3,10^4, 10^5)) + labs(x = "Video style", y = "Like count")+
  annotate("text",x = 1.7, y = 200000,size = 7, label = paste("p-value = ", round(style_df_kruskal_like$p.value,3)))+
  theme_classic2() + theme(
    axis.title.x =  element_text(size = 18),
    axis.title.y = element_text(size = 18),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 16))+
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  NULL


chr_b3.1

chr_b4.1 <- style_df_comment %>%
  group_by(video_style_abr) %>%
  mutate(count=n()) %>%
  mutate(mean = mean(comment_count)) %>%
  ggplot(aes(y =comment_count, x =video_style_abr))+
  geom_boxplot(fill = "lightgreen")+
  scale_y_log10(breaks = c(1.5, 10, 100,1000, 10000), labels = expression(1, 10,10^2,10^3, 10^4)) + labs(x = "Video style", y = "Comment count")+
  annotate("text",x = 1.7, y = 10000,size = 7, label = paste("p-value = ", round(style_df_kruskal_comment$p.value,3)))+
  theme_classic2() + theme(
    axis.title.x =  element_text(size = 18),
    axis.title.y = element_text(size = 18),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 16))+
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  NULL

chr_b4.1

#combined plot#
chr_b1.1+chr_b2.1+chr_b3.1+chr_b4.1 + plot_annotation(tag_levels ='I')


## 5.0. Video characteristics 

#Create functions to facilitate data conversion#

df_create_view <- function(column1){new_data <- df_final %>%
  select({{column1}},view_count)

clean_df <- new_data[complete.cases(new_data), ]

return(clean_df)}

df_create_viewday <- function(column1){new_data <- df_final %>%
  select({{column1}},views_per_day)

clean_df <- new_data[complete.cases(new_data), ]

return(clean_df)}

df_create_like <- function(column1){new_data <- df_final %>%
  select({{column1}},like_count)

clean_df <- new_data[complete.cases(new_data), ]

return(clean_df)}

df_create_comment <- function(column1){new_data <- df_final %>%
  select({{column1}},comment_count)

clean_df <- new_data[complete.cases(new_data), ]

return(clean_df)}

# 5.1. Thumbnail_special
table(df_final$thumbnail_special_yn)

df_final$thumbnail_special_yn <- ifelse(df_final$thumbnail_special_yn == "1","Y","N")
table(df_final$thumbnail_special_yn)

df_final$thumbnail_special_yn<- factor(df_final$thumbnail_special_yn, levels=c("Y", "N"))

thumbnail_special_view <- df_create_view(thumbnail_special_yn)

table(thumbnail_special_view$thumbnail_special_yn)

thumbnail_special_Y <- thumbnail_special_view %>%
  subset(thumbnail_special_yn == "Y") %>%
  summarize(count = n(), median_view = median(view_count), Q1_view = quantile(view_count,0.25), Q3_view = quantile(view_count, 0.75), min = quantile(view_count,0),max = quantile(view_count,1))
thumbnail_special_Y

thumbnail_special_N <- thumbnail_special_view %>%
  subset(thumbnail_special_yn == "N") %>%
  summarize(count = n(), median_view = median(view_count), Q1_view = quantile(view_count,0.25), Q3_view = quantile(view_count, 0.75), min = quantile(view_count,0),max = quantile(view_count,1))
thumbnail_special_N

thumbnail_special_view_sum <- rbind(thumbnail_special_Y,thumbnail_special_N)
thumbnail_special_view_sum <- as.data.frame(thumbnail_special_view_sum)

rownames(thumbnail_special_view_sum) <- c("thumbnail_special_y","thumbnail_special_n")
thumbnail_special_view_sum

thumbnail_special_viewday <- df_create_viewday(thumbnail_special_yn)
thumbnail_special_Y <- thumbnail_special_viewday %>%
  subset(thumbnail_special_yn == "Y") %>%
  summarize(count = n(), median_viewday = median(views_per_day), Q1_viewday = quantile(views_per_day,0.25), Q3_viewday = quantile(views_per_day, 0.75), min = quantile(views_per_day,0),max = quantile(views_per_day,1))
thumbnail_special_Y

thumbnail_special_N <- thumbnail_special_viewday %>%
  subset(thumbnail_special_yn == "N") %>%
  summarize(count = n(), median_viewday = median(views_per_day), Q1_viewday = quantile(views_per_day,0.25), Q3_viewday = quantile(views_per_day, 0.75), min = quantile(views_per_day,0),max = quantile(views_per_day,1))
thumbnail_special_N

thumbnail_special_viewday_sum <- rbind(thumbnail_special_Y,thumbnail_special_N)
thumbnail_special_viewday_sum <- as.data.frame(thumbnail_special_viewday_sum)

rownames(thumbnail_special_viewday_sum) <- c("thumbnail_special_y","thumbnail_special_n")
thumbnail_special_viewday_sum

thumbnail_special_like <- df_create_like(thumbnail_special_yn)

thumbnail_special_like <- df_create_like(thumbnail_special_yn)
thumbnail_special_Y <- thumbnail_special_like %>%
  subset(thumbnail_special_yn == "Y") %>%
  summarize(count = n(), median_like = median(like_count), Q1_like = quantile(like_count,0.25), Q3_like = quantile(like_count, 0.75), min = quantile(like_count,0),max = quantile(like_count,1))
thumbnail_special_Y

thumbnail_special_N <- thumbnail_special_like %>%
  subset(thumbnail_special_yn == "N") %>%
  summarize(count = n(), median_like = median(like_count), Q1_like = quantile(like_count,0.25), Q3_like = quantile(like_count, 0.75), min = quantile(like_count,0),max = quantile(like_count,1))
thumbnail_special_N

thumbnail_special_like_sum <- rbind(thumbnail_special_Y,thumbnail_special_N)
thumbnail_special_like_sum <- as.data.frame(thumbnail_special_like_sum)

rownames(thumbnail_special_like_sum) <- c("thumbnail_special_y","thumbnail_special_n")
thumbnail_special_like_sum

thumbnail_special_comment <- df_create_comment(thumbnail_special_yn)
thumbnail_special_Y <- thumbnail_special_comment %>%
  subset(thumbnail_special_yn == "Y") %>%
  summarize(count = n(), median_comment = median(comment_count), Q1_comment = quantile(comment_count,0.25), Q3_comment = quantile(comment_count, 0.75), min = quantile(comment_count,0),max = quantile(comment_count,1))
thumbnail_special_Y

thumbnail_special_N <- thumbnail_special_comment %>%
  subset(thumbnail_special_yn == "N") %>%
  summarize(count = n(), median_comment = median(comment_count), Q1_comment = quantile(comment_count,0.25), Q3_comment = quantile(comment_count, 0.75), min = quantile(comment_count,0),max = quantile(comment_count,1))
thumbnail_special_N

thumbnail_special_comment_sum <- rbind(thumbnail_special_Y,thumbnail_special_N)
thumbnail_special_comment_sum <- as.data.frame(thumbnail_special_comment_sum)

rownames(thumbnail_special_comment_sum) <- c("thumbnail_special_y","thumbnail_special_n")
thumbnail_special_comment_sum


thumbnail_special_view_wilcox <- wilcox.test(view_count ~ thumbnail_special_yn, data = thumbnail_special_view)
thumbnail_special_view_wilcox 

thumbnail_special_viewday_wilcox <- wilcox.test(views_per_day ~ thumbnail_special_yn, data = thumbnail_special_viewday)
thumbnail_special_viewday_wilcox 

thumbnail_special_like_wilcox <- wilcox.test(like_count ~ thumbnail_special_yn, data = thumbnail_special_like)
thumbnail_special_like_wilcox 

thumbnail_special_comment_wilcox <- wilcox.test(comment_count ~ thumbnail_special_yn, data = thumbnail_special_comment)
thumbnail_special_comment_wilcox 


# 5.2. Thumbnail_texts 
table(df_final$thumbnail_text_yn)

df_final$thumbnail_text_yn <- ifelse(df_final$thumbnail_text_yn == "1","Y","N")
table(df_final$thumbnail_text_yn)


df_final$thumbnail_text_yn<- factor(df_final$thumbnail_text_yn, levels=c("Y", "N"))

thumbnail_texts_view <- df_create_view(thumbnail_text_yn)

thumbnail_texts_viewday <- df_create_viewday(thumbnail_text_yn)

thumbnail_texts_like <- df_create_like(thumbnail_text_yn)

thumbnail_texts_comment <- df_create_comment(thumbnail_text_yn)

##
thumbnail_texts_Y <- thumbnail_texts_view %>%
  subset(thumbnail_text_yn == "Y") %>%
  summarize(count = n(), median_view = median(view_count), Q1_view = quantile(view_count,0.25), Q3_view = quantile(view_count, 0.75), min = quantile(view_count,0),max = quantile(view_count,1))
thumbnail_texts_Y

thumbnail_texts_N <- thumbnail_texts_view %>%
  subset(thumbnail_text_yn == "N") %>%
  summarize(count = n(), median_view = median(view_count), Q1_view = quantile(view_count,0.25), Q3_view = quantile(view_count, 0.75), min = quantile(view_count,0),max = quantile(view_count,1))
thumbnail_texts_N

thumbnail_texts_view_sum <- rbind(thumbnail_texts_Y,thumbnail_texts_N)
thumbnail_texts_view_sum <- as.data.frame(thumbnail_texts_view_sum)

rownames(thumbnail_texts_view_sum) <- c("thumbnail_texts_y","thumbnail_texts_n")
thumbnail_texts_view_sum

##
thumbnail_texts_Y <- thumbnail_texts_viewday %>%
  subset(thumbnail_text_yn == "Y") %>%
  summarize(count = n(), median_viewday = median(views_per_day), Q1_viewday = quantile(views_per_day,0.25), Q3_viewday = quantile(views_per_day, 0.75), min = quantile(views_per_day,0),max = quantile(views_per_day,1))
thumbnail_texts_Y

thumbnail_texts_N <- thumbnail_texts_viewday %>%
  subset(thumbnail_text_yn == "N") %>%
  summarize(count = n(), median_viewday = median(views_per_day), Q1_viewday = quantile(views_per_day,0.25), Q3_viewday = quantile(views_per_day, 0.75), min = quantile(views_per_day,0),max = quantile(views_per_day,1))
thumbnail_texts_N


thumbnail_texts_viewday_sum <- rbind(thumbnail_texts_Y,thumbnail_texts_N)
thumbnail_texts_viewday_sum <- as.data.frame(thumbnail_texts_viewday_sum)

rownames(thumbnail_texts_viewday_sum) <- c("thumbnail_texts_y","thumbnail_texts_n")
thumbnail_texts_viewday_sum

##
thumbnail_texts_Y <- thumbnail_texts_like %>%
  subset(thumbnail_text_yn == "Y") %>%
  summarize(count = n(), median_like = median(like_count), Q1_like = quantile(like_count,0.25), Q3_like = quantile(like_count, 0.75), min = quantile(like_count,0),max = quantile(like_count,1))
thumbnail_texts_Y

thumbnail_texts_N <- thumbnail_texts_like %>%
  subset(thumbnail_text_yn == "N") %>%
  summarize(count = n(), median_like = median(like_count), Q1_like = quantile(like_count,0.25), Q3_like = quantile(like_count, 0.75), min = quantile(like_count,0),max = quantile(like_count,1))
thumbnail_texts_N


thumbnail_texts_like_sum <- rbind(thumbnail_texts_Y,thumbnail_texts_N)
thumbnail_texts_like_sum <- as.data.frame(thumbnail_texts_like_sum)

rownames(thumbnail_texts_like_sum) <- c("thumbnail_texts_y","thumbnail_texts_n")
thumbnail_texts_like_sum

##
thumbnail_texts_Y <- thumbnail_texts_comment %>%
  subset(thumbnail_text_yn == "Y") %>%
  summarize(count = n(), median_comment = median(comment_count), Q1_comment = quantile(comment_count,0.25), Q3_comment = quantile(comment_count, 0.75), min = quantile(comment_count,0),max = quantile(comment_count,1))
thumbnail_texts_Y

thumbnail_texts_N <- thumbnail_texts_comment %>%
  subset(thumbnail_text_yn == "N") %>%
  summarize(count = n(), median_comment = median(comment_count), Q1_comment = quantile(comment_count,0.25), Q3_comment = quantile(comment_count, 0.75), min = quantile(comment_count,0),max = quantile(comment_count,1))
thumbnail_texts_N

thumbnail_texts_comment_sum <- rbind(thumbnail_texts_Y,thumbnail_texts_N)
thumbnail_texts_comment_sum <- as.data.frame(thumbnail_texts_comment_sum)

rownames(thumbnail_texts_comment_sum) <- c("thumbnail_texts_y","thumbnail_texts_n")
thumbnail_texts_comment_sum

##
thumbnail_texts_view_wilcox <- wilcox.test(view_count ~ thumbnail_text_yn, data = thumbnail_texts_view)
thumbnail_texts_view_wilcox 

thumbnail_texts_viewday_wilcox <- wilcox.test(views_per_day ~ thumbnail_text_yn, data = thumbnail_texts_viewday)
thumbnail_texts_viewday_wilcox 

thumbnail_texts_like_wilcox <- wilcox.test(like_count ~ thumbnail_text_yn, data = thumbnail_texts_like)
thumbnail_texts_like_wilcox 

thumbnail_texts_comment_wilcox <- wilcox.test(comment_count ~ thumbnail_text_yn, data = thumbnail_texts_comment)
thumbnail_texts_comment_wilcox 

# 5.3. Thumbnail logo 

table(df_final$thumbnail_logo_yn)
df_final$thumbnail_logo_yn <- as.character(df_final$intro_yn)

df_final$thumbnail_logo_yn <- ifelse(df_final$thumbnail_logo_yn == "1","Y","N")
table(df_final$thumbnail_logo_yn)

df_final$thumbnail_logo_yn<- factor(df_final$thumbnail_logo_yn, levels=c("Y", "N"))
df_final$thumbnail_logo_yn

thumbnail_logo_view <- df_create_view(thumbnail_logo_yn)
table(thumbnail_logo_view$thumbnail_logo_yn)

thumbnail_logo_viewday <- df_create_viewday(thumbnail_logo_yn)
thumbnail_logo_like <- df_create_like(thumbnail_logo_yn)
thumbnail_logo_comment <- df_create_comment(thumbnail_logo_yn)

##
thumbnail_logo_Y <- thumbnail_logo_view %>%
  subset(thumbnail_logo_yn == "Y") %>%
  summarize(count = n(), median_view = median(view_count), Q1_view = quantile(view_count,0.25), Q3_view = quantile(view_count, 0.75), min = quantile(view_count,0),max = quantile(view_count,1))
thumbnail_logo_Y

thumbnail_logo_N <- thumbnail_logo_view %>%
  subset(thumbnail_logo_yn == "N") %>%
  summarize(count = n(), median_view = median(view_count), Q1_view = quantile(view_count,0.25), Q3_view = quantile(view_count, 0.75), min = quantile(view_count,0),max = quantile(view_count,1))
thumbnail_logo_N

thumbnail_logo_view_sum <- rbind(thumbnail_logo_Y,thumbnail_logo_N)
thumbnail_logo_view_sum <- as.data.frame(thumbnail_logo_view_sum)

rownames(thumbnail_logo_view_sum) <- c("thumbnail_logo_y","thumbnail_logo_n")
thumbnail_logo_view_sum

##
thumbnail_logo_Y <- thumbnail_logo_viewday %>%
  subset(thumbnail_logo_yn == "Y") %>%
  summarize(count = n(), median_viewday = median(views_per_day), Q1_viewday = quantile(views_per_day,0.25), Q3_viewday = quantile(views_per_day, 0.75), min = quantile(views_per_day,0),max = quantile(views_per_day,1))
thumbnail_logo_Y

thumbnail_logo_N <- thumbnail_logo_viewday %>%
  subset(thumbnail_logo_yn == "N") %>%
  summarize(count = n(), median_viewday = median(views_per_day), Q1_viewday = quantile(views_per_day,0.25), Q3_viewday = quantile(views_per_day, 0.75), min = quantile(views_per_day,0),max = quantile(views_per_day,1))
thumbnail_logo_N

thumbnail_logo_viewday_sum <- rbind(thumbnail_logo_Y,thumbnail_logo_N)
thumbnail_logo_viewday_sum <- as.data.frame(thumbnail_logo_viewday_sum)

rownames(thumbnail_logo_viewday_sum) <- c("thumbnail_logo_y","thumbnail_logo_n")
thumbnail_logo_viewday_sum

##
thumbnail_logo_Y <- thumbnail_logo_like %>%
  subset(thumbnail_logo_yn == "Y") %>%
  summarize(count = n(), median_like = median(like_count), Q1_like = quantile(like_count,0.25), Q3_like = quantile(like_count, 0.75), min = quantile(like_count,0),max = quantile(like_count,1))
thumbnail_logo_Y

thumbnail_logo_N <- thumbnail_logo_like %>%
  subset(thumbnail_logo_yn == "N") %>%
  summarize(count = n(), median_like = median(like_count), Q1_like = quantile(like_count,0.25), Q3_like = quantile(like_count, 0.75), min = quantile(like_count,0),max = quantile(like_count,1))
thumbnail_logo_N

thumbnail_logo_like_sum <- rbind(thumbnail_logo_Y,thumbnail_logo_N)
thumbnail_logo_like_sum <- as.data.frame(thumbnail_logo_like_sum)

rownames(thumbnail_logo_like_sum) <- c("thumbnail_logo_y","thumbnail_logo_n")
thumbnail_logo_like_sum

##
thumbnail_logo_Y <- thumbnail_logo_comment %>%
  subset(thumbnail_logo_yn == "Y") %>%
  summarize(count = n(), median_comment = median(comment_count), Q1_comment = quantile(comment_count,0.25), Q3_comment = quantile(comment_count, 0.75), min = quantile(comment_count,0),max = quantile(comment_count,1))
thumbnail_logo_Y

thumbnail_logo_N <- thumbnail_logo_comment %>%
  subset(thumbnail_logo_yn == "N") %>%
  summarize(count = n(), median_comment = median(comment_count), Q1_comment = quantile(comment_count,0.25), Q3_comment = quantile(comment_count, 0.75), min = quantile(comment_count,0),max = quantile(comment_count,1))
thumbnail_logo_N

thumbnail_logo_comment_sum <- rbind(thumbnail_logo_Y,thumbnail_logo_N)
thumbnail_logo_comment_sum <- as.data.frame(thumbnail_logo_comment_sum)

rownames(thumbnail_logo_comment_sum) <- c("thumbnail_logo_y","thumbnail_logo_n")
thumbnail_logo_comment_sum

##

thumbnail_logo_view_wilcox <- wilcox.test(view_count ~ thumbnail_logo_yn, data = thumbnail_logo_view)
thumbnail_logo_view_wilcox 

thumbnail_logo_viewday_wilcox <- wilcox.test(views_per_day ~ thumbnail_logo_yn, data = thumbnail_logo_viewday)
thumbnail_logo_viewday_wilcox 

thumbnail_logo_like_wilcox <- wilcox.test(like_count ~ thumbnail_logo_yn, data = thumbnail_logo_like)
thumbnail_logo_like_wilcox 

thumbnail_logo_comment_wilcox <- wilcox.test(comment_count ~ thumbnail_logo_yn, data = thumbnail_logo_comment)
thumbnail_logo_comment_wilcox 


##Thumbnail combined ##

thumbnail_combined <- thumbnail_special_combine/thumbnail_texts_combine/thumbnail_logo_combine
thumbnail_combined

thumbnail_view_summary <- rbind(thumbnail_special_view_sum,thumbnail_texts_view_sum,thumbnail_logo_view_sum)
thumbnail_view_summary

thumbnail_viewday_summary <- rbind(thumbnail_special_viewday_sum,thumbnail_texts_viewday_sum,thumbnail_logo_viewday_sum)
thumbnail_viewday_summary

thumbnail_like_summary <- rbind(thumbnail_special_like_sum,thumbnail_texts_like_sum,thumbnail_logo_like_sum)
thumbnail_like_summary

thumbnail_comment_summary <- rbind(thumbnail_special_comment_sum,thumbnail_texts_comment_sum,thumbnail_logo_comment_sum)
thumbnail_comment_summary

# 5.4. Introduction

table(df_final$intro_yn)
df_final$intro_yn <- as.character(df_final$intro_yn)

df_final$intro_yn <- ifelse(df_final$intro_yn == "1","Y","N")
table(df_final$intro_yn)

df_final$intro_yn<- factor(df_final$intro_yn, levels=c("Y", "N"))
table(df_final$intro_yn)
intro_view <- df_create_view(intro_yn)
intro_viewday <- df_create_viewday(intro_yn)
intro_like <- df_create_like(intro_yn)
intro_comment <- df_create_comment(intro_yn)

##
intro_view_Y <- intro_view %>%
  subset(intro_yn == "Y") %>%
  summarize(count = n(), median_view = median(view_count), Q1_view = quantile(view_count,0.25), Q3_view = quantile(view_count, 0.75), min = quantile(view_count,0),max = quantile(view_count,1))
intro_view_Y

intro_view_N <- intro_view %>%
  subset(intro_yn == "N") %>%
  summarize(count = n(), median_view = median(view_count), Q1_view = quantile(view_count,0.25), Q3_view = quantile(view_count, 0.75), min = quantile(view_count,0),max = quantile(view_count,1))
intro_view_N


intro_view_sum <- rbind(intro_view_Y,intro_view_N)
intro_view_sum <- as.data.frame(intro_view_sum)

rownames(intro_view_sum) <- c("intro_view_Y","intro_view_N")
intro_view_sum

##
intro_Y <- intro_viewday %>%
  subset(intro_yn == "Y") %>%
  summarize(count = n(), median_viewday = median(views_per_day), Q1_viewday = quantile(views_per_day,0.25), Q3_viewday = quantile(views_per_day, 0.75), min = quantile(views_per_day,0),max = quantile(views_per_day,1))
intro_Y

intro_N <- intro_viewday %>%
  subset(intro_yn == "N") %>%
  summarize(count = n(), median_viewday = median(views_per_day), Q1_viewday = quantile(views_per_day,0.25), Q3_viewday = quantile(views_per_day, 0.75), min = quantile(views_per_day,0),max = quantile(views_per_day,1))
intro_N

intro_viewday_sum <- rbind(intro_Y,intro_N)
intro_viewday_sum <- as.data.frame(intro_viewday_sum)

rownames(intro_viewday_sum) <- c("intro_y","intro_n")
intro_viewday_sum

##
intro_Y <- intro_like %>%
  subset(intro_yn == "Y") %>%
  summarize(count = n(), median_like = median(like_count), Q1_like = quantile(like_count,0.25), Q3_like = quantile(like_count, 0.75), min = quantile(like_count,0),max = quantile(like_count,1))
intro_Y

intro_N <- intro_like %>%
  subset(intro_yn == "N") %>%
  summarize(count = n(), median_like = median(like_count), Q1_like = quantile(like_count,0.25), Q3_like = quantile(like_count, 0.75), min = quantile(like_count,0),max = quantile(like_count,1))
intro_N


intro_like_sum <- rbind(intro_Y,intro_N)
intro_like_sum <- as.data.frame(intro_like_sum)

rownames(intro_like_sum) <- c("intro_y","intro_n")
intro_like_sum

##
intro_Y <- intro_comment %>%
  subset(intro_yn == "Y") %>%
  summarize(count = n(), median_comment = median(comment_count), Q1_comment = quantile(comment_count,0.25), Q3_comment = quantile(comment_count, 0.75), min = quantile(comment_count,0),max = quantile(comment_count,1))
intro_Y

intro_N <- intro_comment %>%
  subset(intro_yn == "N") %>%
  summarize(count = n(), median_comment = median(comment_count), Q1_comment = quantile(comment_count,0.25), Q3_comment = quantile(comment_count, 0.75), min = quantile(comment_count,0),max = quantile(comment_count,1))
intro_N


intro_comment_sum <- rbind(intro_Y,intro_N)
intro_comment_sum <- as.data.frame(intro_comment_sum)

rownames(intro_comment_sum) <- c("intro_y","intro_n")
intro_comment_sum

##

intro_view_wilcox <- wilcox.test(view_count ~ intro_yn, data = intro_view)
intro_view_wilcox 

intro_viewday_wilcox <- wilcox.test(views_per_day ~ intro_yn, data = intro_viewday)
intro_viewday_wilcox 

intro_like_wilcox <- wilcox.test(like_count ~ intro_yn, data = intro_like)
intro_like_wilcox 

intro_comment_wilcox <- wilcox.test(comment_count ~ intro_yn, data = intro_comment)
intro_comment_wilcox 

# 5.5. Introduction topic

table(df_final$intro_topic_yn)
df_final$intro_topic_yn <- as.character(df_final$intro_topic_yn)

intro_topic_view <- df_final %>%
  select(intro_topic_yn,view_count) %>%
  filter(intro_topic_yn == "0"|intro_topic_yn == "1")

intro_topic_view <- intro_topic_view[complete.cases(intro_topic_view ),]

table(intro_topic_view$intro_topic_yn)

intro_topic_view$intro_topic_yn <- ifelse(intro_topic_view$intro_topic_yn == "1","Y","N")

intro_topic_view$intro_topic_yn<- factor(intro_topic_view$intro_topic_yn, levels=c("Y", "N"))
table(intro_topic_view$intro_topic_yn)

intro_topic_viewday <- df_final %>%
  select(intro_topic_yn,views_per_day) %>%
  filter(intro_topic_yn == "0"|intro_topic_yn == "1")

intro_topic_viewday <- intro_topic_viewday[complete.cases(intro_topic_viewday),]

intro_topic_viewday$intro_topic_yn <- ifelse(intro_topic_viewday$intro_topic_yn == "1","Y","N")

intro_topic_viewday$intro_topic_yn<- factor(intro_topic_viewday$intro_topic_yn, levels=c("Y", "N"))
table(intro_topic_viewday$intro_topic_yn)

intro_topic_like <- df_final %>%
  select(intro_topic_yn,like_count) %>%
  filter(intro_topic_yn == "0"|intro_topic_yn == "1")

intro_topic_like <- intro_topic_like[complete.cases(intro_topic_like),]
intro_topic_like

intro_topic_like$intro_topic_yn <- ifelse(intro_topic_like$intro_topic_yn == "1","Y","N")

intro_topic_like$intro_topic_yn<- factor(intro_topic_like$intro_topic_yn, levels=c("Y", "N"))
table(intro_topic_like$intro_topic_yn)

intro_topic_comment <- df_final %>%
  select(intro_topic_yn,comment_count) %>%
  filter(intro_topic_yn == "0"|intro_topic_yn == "1")

intro_topic_comment <- intro_topic_comment[complete.cases(intro_topic_comment),]


intro_topic_comment$intro_topic_yn <- ifelse(intro_topic_comment$intro_topic_yn == "1","Y","N")

intro_topic_comment$intro_topic_yn<- factor(intro_topic_comment$intro_topic_yn, levels=c("Y", "N"))
table(intro_topic_comment$intro_topic_yn)

##

intro_topic_view_Y <- intro_topic_view %>%
  subset(intro_topic_yn == "Y") %>%
  summarize(count = n(), median_view = median(view_count), Q1_view = quantile(view_count,0.25), Q3_view = quantile(view_count, 0.75), min = quantile(view_count,0),max = quantile(view_count,1))
intro_topic_view_Y

intro_topic_view_N <- intro_topic_view %>%
  subset(intro_topic_yn == "N") %>%
  summarize(count = n(), median_view = median(view_count), Q1_view = quantile(view_count,0.25), Q3_view = quantile(view_count, 0.75), min = quantile(view_count,0),max = quantile(view_count,1))
intro_topic_view_N

intro_topic_view_sum <- rbind(intro_topic_view_Y,intro_topic_view_N)
intro_topic_view_sum <- as.data.frame(intro_topic_view_sum)

rownames(intro_topic_view_sum) <- c("intro_topic_Y","intro_topic_N")
intro_topic_view_sum

##
intro_topic_Y <- intro_topic_viewday %>%
  subset(intro_topic_yn == "Y") %>%
  summarize(count = n(), median_viewday = median(views_per_day), Q1_viewday = quantile(views_per_day,0.25), Q3_viewday = quantile(views_per_day, 0.75), min = quantile(views_per_day,0),max = quantile(views_per_day,1))
intro_topic_Y

intro_topic_N <- intro_topic_viewday %>%
  subset(intro_topic_yn == "N") %>%
  summarize(count = n(), median_viewday = median(views_per_day), Q1_viewday = quantile(views_per_day,0.25), Q3_viewday = quantile(views_per_day, 0.75), min = quantile(views_per_day,0),max = quantile(views_per_day,1))
intro_topic_N

intro_topic_viewday_sum <- rbind(intro_topic_Y,intro_topic_N)
intro_topic_viewday_sum <- as.data.frame(intro_topic_viewday_sum)

rownames(intro_topic_viewday_sum) <- c("intro_topic_y","intro_topic_n")
intro_topic_viewday_sum

##
intro_topic_Y <- intro_topic_like %>%
  subset(intro_topic_yn == "Y") %>%
  summarize(count = n(), median_like = median(like_count), Q1_like = quantile(like_count,0.25), Q3_like = quantile(like_count, 0.75), min = quantile(like_count,0),max = quantile(like_count,1))
intro_topic_Y

intro_topic_N <- intro_topic_like %>%
  subset(intro_topic_yn == "N") %>%
  summarize(count = n(), median_like = median(like_count), Q1_like = quantile(like_count,0.25), Q3_like = quantile(like_count, 0.75), min = quantile(like_count,0),max = quantile(like_count,1))
intro_topic_N


intro_topic_like_sum <- rbind(intro_topic_Y,intro_topic_N)
intro_topic_like_sum <- as.data.frame(intro_topic_like_sum)

rownames(intro_topic_like_sum) <- c("intro_topic_y","intro_topic_n")
intro_topic_like_sum

##
intro_topic_Y <- intro_topic_comment %>%
  subset(intro_topic_yn == "Y") %>%
  summarize(count = n(), median_comment = median(comment_count), Q1_comment = quantile(comment_count,0.25), Q3_comment = quantile(comment_count, 0.75), min = quantile(comment_count,0),max = quantile(comment_count,1))
intro_topic_Y

intro_topic_N <- intro_topic_comment %>%
  subset(intro_topic_yn == "N") %>%
  summarize(count = n(), median_comment = median(comment_count), Q1_comment = quantile(comment_count,0.25), Q3_comment = quantile(comment_count, 0.75), min = quantile(comment_count,0),max = quantile(comment_count,1))
intro_topic_N

intro_topic_comment_sum <- rbind(intro_topic_Y,intro_topic_N)
intro_topic_comment_sum <- as.data.frame(intro_topic_comment_sum)

rownames(intro_topic_comment_sum) <- c("intro_topic_y","intro_topic_n")
intro_topic_comment_sum

intro_topic_view_wilcox <- wilcox.test(view_count ~ intro_topic_yn, data = intro_topic_view)
intro_topic_view_wilcox 

intro_topic_viewday_wilcox <- wilcox.test(views_per_day ~ intro_topic_yn, data = intro_topic_viewday)
intro_topic_viewday_wilcox 

intro_topic_like_wilcox <- wilcox.test(like_count ~ intro_topic_yn, data = intro_topic_like)
intro_topic_like_wilcox 

intro_topic_comment_wilcox <- wilcox.test(comment_count ~ intro_topic_yn, data = intro_topic_comment)
intro_topic_comment_wilcox 

# 5.6. Introduction question#
table(df_final$intro_question_yn)

df_final$intro_question_yn <- as.character(df_final$intro_question_yn)

intro_question_view <- df_final %>%
  select(intro_question_yn,view_count) %>%
  filter(intro_question_yn == "0"|intro_question_yn == "1")

intro_question_view <- intro_question_view[complete.cases(intro_question_view ),]

table(intro_question_view$intro_question_yn)

intro_question_view$intro_question_yn <- ifelse(intro_question_view$intro_question_yn == "1","Y","N")

intro_question_view$intro_question_yn<- factor(intro_question_view$intro_question_yn, levels=c("Y", "N"))
table(intro_question_view$intro_question_yn)

intro_question_viewday <- df_final %>%
  select(intro_question_yn,views_per_day) %>%
  filter(intro_question_yn == "0"|intro_question_yn == "1")

intro_question_viewday <- intro_question_viewday[complete.cases(intro_question_viewday),]

intro_question_viewday$intro_question_yn <- ifelse(intro_question_viewday$intro_question_yn == "1","Y","N")

intro_question_viewday$intro_question_yn<- factor(intro_question_viewday$intro_question_yn, levels=c("Y", "N"))
table(intro_question_viewday$intro_question_yn)

intro_question_like <- df_final %>%
  select(intro_question_yn,like_count) %>%
  filter(intro_question_yn == "0"|intro_question_yn == "1")

intro_question_like <- intro_question_like[complete.cases(intro_question_like),]


intro_question_like$intro_question_yn <- ifelse(intro_question_like$intro_question_yn == "1","Y","N")

intro_question_like$intro_question_yn<- factor(intro_question_like$intro_question_yn, levels=c("Y", "N"))
table(intro_question_like$intro_question_yn)


intro_question_comment <- df_final %>%
  select(intro_question_yn,comment_count) %>%
  filter(intro_question_yn == "0"|intro_question_yn == "1")

intro_question_comment <- intro_question_comment[complete.cases(intro_question_comment),]


intro_question_comment$intro_question_yn <- ifelse(intro_question_comment$intro_question_yn == "1","Y","N")

intro_question_comment$intro_question_yn<- factor(intro_question_comment$intro_question_yn, levels=c("Y", "N"))
table(intro_question_comment$intro_question_yn)

##
intro_question_view_Y <- intro_question_view %>%
  subset(intro_question_yn == "Y") %>%
  summarize(count = n(), median_view = median(view_count), Q1_view = quantile(view_count,0.25), Q3_view = quantile(view_count, 0.75), min = quantile(view_count,0),max = quantile(view_count,1))
intro_question_view_Y

intro_question_view_N <- intro_question_view %>%
  subset(intro_question_yn == "N") %>%
  summarize(count = n(), median_view = median(view_count), Q1_view = quantile(view_count,0.25), Q3_view = quantile(view_count, 0.75), min = quantile(view_count,0),max = quantile(view_count,1))
intro_question_view_N

intro_question_view_sum <- rbind(intro_question_view_Y,intro_question_view_N)
intro_question_view_sum <- as.data.frame(intro_question_view_sum)

rownames(intro_question_view_sum) <- c("intro_question_Y","intro_question_N")
intro_question_view_sum

##
intro_question_Y <- intro_question_viewday %>%
  subset(intro_question_yn == "Y") %>%
  summarize(count = n(), median_viewday = median(views_per_day), Q1_viewday = quantile(views_per_day,0.25), Q3_viewday = quantile(views_per_day, 0.75), min = quantile(views_per_day,0),max = quantile(views_per_day,1))
intro_question_Y

intro_question_N <- intro_question_viewday %>%
  subset(intro_question_yn == "N") %>%
  summarize(count = n(), median_viewday = median(views_per_day), Q1_viewday = quantile(views_per_day,0.25), Q3_viewday = quantile(views_per_day, 0.75), min = quantile(views_per_day,0),max = quantile(views_per_day,1))
intro_question_N


intro_question_viewday_sum <- rbind(intro_question_Y,intro_question_N)
intro_question_viewday_sum <- as.data.frame(intro_question_viewday_sum)

rownames(intro_question_viewday_sum) <- c("intro_question_y","intro_question_n")
intro_question_viewday_sum

##
intro_question_Y <- intro_question_like %>%
  subset(intro_question_yn == "Y") %>%
  summarize(count = n(), median_like = median(like_count), Q1_like = quantile(like_count,0.25), Q3_like = quantile(like_count, 0.75), min = quantile(like_count,0),max = quantile(like_count,1))
intro_question_Y

intro_question_N <- intro_question_like %>%
  subset(intro_question_yn == "N") %>%
  summarize(count = n(), median_like = median(like_count), Q1_like = quantile(like_count,0.25), Q3_like = quantile(like_count, 0.75), min = quantile(like_count,0),max = quantile(like_count,1))
intro_question_N

intro_question_like_sum <- rbind(intro_question_Y,intro_question_N)
intro_question_like_sum <- as.data.frame(intro_question_like_sum)

rownames(intro_question_like_sum) <- c("intro_question_y","intro_question_n")
intro_question_like_sum

##
intro_question_Y <- intro_question_comment %>%
  subset(intro_question_yn == "Y") %>%
  summarize(count = n(), median_comment = median(comment_count), Q1_comment = quantile(comment_count,0.25), Q3_comment = quantile(comment_count, 0.75), min = quantile(comment_count,0),max = quantile(comment_count,1))
intro_question_Y

intro_question_N <- intro_question_comment %>%
  subset(intro_question_yn == "N") %>%
  summarize(count = n(), median_comment = median(comment_count), Q1_comment = quantile(comment_count,0.25), Q3_comment = quantile(comment_count, 0.75), min = quantile(comment_count,0),max = quantile(comment_count,1))
intro_question_N

intro_question_comment_sum <- rbind(intro_question_Y,intro_question_N)
intro_question_comment_sum <- as.data.frame(intro_question_comment_sum)

rownames(intro_question_comment_sum) <- c("intro_question_y","intro_question_n")
intro_question_comment_sum

##

intro_question_view_wilcox <- wilcox.test(view_count ~ intro_question_yn, data = intro_question_view)
intro_question_view_wilcox 

intro_question_viewday_wilcox <- wilcox.test(views_per_day ~ intro_question_yn, data = intro_question_viewday)
intro_question_viewday_wilcox 

intro_question_like_wilcox <- wilcox.test(like_count ~ intro_question_yn, data = intro_question_like)
intro_question_like_wilcox 

intro_question_comment_wilcox <- wilcox.test(comment_count ~ intro_question_yn, data = intro_question_comment)
intro_question_comment_wilcox 

##intro combined##

intro_view_summary <- rbind(intro_view_sum,intro_topic_view_sum,intro_question_view_sum)
intro_view_summary

intro_viewday_summary <- rbind(intro_viewday_sum,intro_topic_viewday_sum,intro_question_viewday_sum)
intro_viewday_summary

intro_like_summary <- rbind(intro_like_sum,intro_topic_like_sum,intro_question_like_sum)
intro_like_summary

intro_comment_summary <- rbind(intro_comment_sum,intro_topic_comment_sum,intro_question_comment_sum)
intro_comment_summary


# 5.7. starting music

table(df_final$start_music_yn)

df_final$start_music_yn <- ifelse(df_final$start_music_yn == "0", "0",
                                  ifelse(df_final$start_music_yn == "1", "1",
                                         ifelse(df_final$start_music_yn == "0 - Sound effect", "0", NA)))
table(df_final$start_music_yn)

df_final$start_music_yn <- ifelse(df_final$start_music_yn == "1","Y","N")
table(df_final$start_music_yn)

df_final$start_music_yn<- factor(df_final$start_music_yn, levels=c("Y", "N"))

start_music_view <- df_create_view(start_music_yn)

start_music_viewday <- df_create_viewday(start_music_yn)
start_music_like <- df_create_like(start_music_yn)
start_music_comment <- df_create_comment(start_music_yn)

##
start_music_view_Y <- start_music_view %>%
  subset(start_music_yn == "Y") %>%
  summarize(count = n(), median_view = median(view_count), Q1_view = quantile(view_count,0.25), Q3_view = quantile(view_count, 0.75), min = quantile(view_count,0),max = quantile(view_count,1))
start_music_view_Y

start_music_view_N <- start_music_view %>%
  subset(start_music_yn == "N") %>%
  summarize(count = n(), median_view = median(view_count), Q1_view = quantile(view_count,0.25), Q3_view = quantile(view_count, 0.75), min = quantile(view_count,0),max = quantile(view_count,1))
start_music_view_N


start_music_view_sum <- rbind(start_music_view_Y,start_music_view_N)
start_music_view_sum <- as.data.frame(start_music_view_sum)

rownames(start_music_view_sum) <- c("start_music_Y","start_music_N")
start_music_view_sum

##
start_music_Y <- start_music_viewday %>%
  subset(start_music_yn == "Y") %>%
  summarize(count = n(), median_viewday = median(views_per_day), Q1_viewday = quantile(views_per_day,0.25), Q3_viewday = quantile(views_per_day, 0.75), min = quantile(views_per_day,0),max = quantile(views_per_day,1))
start_music_Y

start_music_N <- start_music_viewday %>%
  subset(start_music_yn == "N") %>%
  summarize(count = n(), median_viewday = median(views_per_day), Q1_viewday = quantile(views_per_day,0.25), Q3_viewday = quantile(views_per_day, 0.75), min = quantile(views_per_day,0),max = quantile(views_per_day,1))
start_music_N


start_music_viewday_sum <- rbind(start_music_Y,start_music_N)
start_music_viewday_sum <- as.data.frame(start_music_viewday_sum)

rownames(start_music_viewday_sum) <- c("start_music_y","start_music_n")
start_music_viewday_sum

##
start_music_Y <- start_music_like %>%
  subset(start_music_yn == "Y") %>%
  summarize(count = n(), median_like = median(like_count), Q1_like = quantile(like_count,0.25), Q3_like = quantile(like_count, 0.75), min = quantile(like_count,0),max = quantile(like_count,1))
start_music_Y

start_music_N <- start_music_like %>%
  subset(start_music_yn == "N") %>%
  summarize(count = n(), median_like = median(like_count), Q1_like = quantile(like_count,0.25), Q3_like = quantile(like_count, 0.75), min = quantile(like_count,0),max = quantile(like_count,1))
start_music_N

start_music_like_sum <- rbind(start_music_Y,start_music_N)
start_music_like_sum <- as.data.frame(start_music_like_sum)

rownames(start_music_like_sum) <- c("start_music_y","start_music_n")
start_music_like_sum

##
start_music_Y <- start_music_comment %>%
  subset(start_music_yn == "Y") %>%
  summarize(count = n(), median_comment = median(comment_count), Q1_comment = quantile(comment_count,0.25), Q3_comment = quantile(comment_count, 0.75), min = quantile(comment_count,0),max = quantile(comment_count,1))
start_music_Y

start_music_N <- start_music_comment %>%
  subset(start_music_yn == "N") %>%
  summarize(count = n(), median_comment = median(comment_count), Q1_comment = quantile(comment_count,0.25), Q3_comment = quantile(comment_count, 0.75), min = quantile(comment_count,0),max = quantile(comment_count,1))
start_music_N

start_music_comment_sum <- rbind(start_music_Y,start_music_N)
start_music_comment_sum <- as.data.frame(start_music_comment_sum)

rownames(start_music_comment_sum) <- c("start_music_y","start_music_n")
start_music_comment_sum

##
start_music_view_wilcox <- wilcox.test(view_count ~ start_music_yn, data = start_music_view)
start_music_view_wilcox 

start_music_viewday_wilcox <- wilcox.test(views_per_day ~ start_music_yn, data = start_music_viewday)
start_music_viewday_wilcox 

start_music_like_wilcox <- wilcox.test(like_count ~ start_music_yn, data = start_music_like)
start_music_like_wilcox 

start_music_comment_wilcox <- wilcox.test(comment_count ~ start_music_yn, data = start_music_comment)
start_music_comment_wilcox 

# 5.8. start_narration

table(df_final$start_narration_yn)

df_final$start_narration_yn <- as.character(df_final$start_narration_yn)

start_narration_view <- df_final %>%
  select(start_narration_yn,view_count) %>%
  filter(start_narration_yn == "0"|start_narration_yn == "1")

start_narration_view <- start_narration_view[complete.cases(start_narration_view ),]

table(start_narration_view$start_narration_yn)

start_narration_view$start_narration_yn <- ifelse(start_narration_view$start_narration_yn == "1","Y","N")

start_narration_view$start_narration_yn<- factor(start_narration_view$start_narration_yn, levels=c("Y", "N"))
table(start_narration_view$start_narration_yn)

start_narration_viewday <- df_final %>%
  select(start_narration_yn,views_per_day) %>%
  filter(start_narration_yn == "0"|start_narration_yn == "1")

start_narration_viewday <- start_narration_viewday[complete.cases(start_narration_viewday),]

start_narration_viewday$start_narration_yn <- ifelse(start_narration_viewday$start_narration_yn == "1","Y","N")

start_narration_viewday$start_narration_yn<- factor(start_narration_viewday$start_narration_yn, levels=c("Y", "N"))
table(start_narration_viewday$start_narration_yn)

start_narration_like <- df_final %>%
  select(start_narration_yn,like_count) %>%
  filter(start_narration_yn == "0"|start_narration_yn == "1")

start_narration_like <- start_narration_like[complete.cases(start_narration_like),]

start_narration_like$start_narration_yn <- ifelse(start_narration_like$start_narration_yn == "1","Y","N")

start_narration_like$start_narration_yn<- factor(start_narration_like$start_narration_yn, levels=c("Y", "N"))
table(start_narration_like$start_narration_yn)

start_narration_comment <- df_final %>%
  select(start_narration_yn,comment_count) %>%
  filter(start_narration_yn == "0"|start_narration_yn == "1")

start_narration_comment <- start_narration_comment[complete.cases(start_narration_comment),]

start_narration_comment$start_narration_yn <- ifelse(start_narration_comment$start_narration_yn == "1","Y","N")

start_narration_comment$start_narration_yn<- factor(start_narration_comment$start_narration_yn, levels=c("Y", "N"))
table(start_narration_comment$start_narration_yn)

##
start_narration_view_Y <- start_narration_view %>%
  subset(start_narration_yn == "Y") %>%
  summarize(count = n(), median_view = median(view_count), Q1_view = quantile(view_count,0.25), Q3_view = quantile(view_count, 0.75), min = quantile(view_count,0),max = quantile(view_count,1))
start_narration_view_Y

start_narration_view_N <- start_narration_view %>%
  subset(start_narration_yn == "N") %>%
  summarize(count = n(), median_view = median(view_count), Q1_view = quantile(view_count,0.25), Q3_view = quantile(view_count, 0.75), min = quantile(view_count,0),max = quantile(view_count,1))
start_narration_view_N


start_narration_view_sum <- rbind(start_narration_view_Y,start_narration_view_N)
start_narration_view_sum <- as.data.frame(start_narration_view_sum)

rownames(start_narration_view_sum) <- c("start_narration_Y","start_narration_N")
start_narration_view_sum

##
start_narration_Y <- start_narration_viewday %>%
  subset(start_narration_yn == "Y") %>%
  summarize(count = n(), median_viewday = median(views_per_day), Q1_viewday = quantile(views_per_day,0.25), Q3_viewday = quantile(views_per_day, 0.75), min = quantile(views_per_day,0),max = quantile(views_per_day,1))
start_narration_Y

start_narration_N <- start_narration_viewday %>%
  subset(start_narration_yn == "N") %>%
  summarize(count = n(), median_viewday = median(views_per_day), Q1_viewday = quantile(views_per_day,0.25), Q3_viewday = quantile(views_per_day, 0.75), min = quantile(views_per_day,0),max = quantile(views_per_day,1))
start_narration_N

start_narration_viewday_sum <- rbind(start_narration_Y,start_narration_N)
start_narration_viewday_sum <- as.data.frame(start_narration_viewday_sum)

rownames(start_narration_viewday_sum) <- c("start_narration_y","start_narration_n")
start_narration_viewday_sum

##
start_narration_Y <- start_narration_like %>%
  subset(start_narration_yn == "Y") %>%
  summarize(count = n(), median_like = median(like_count), Q1_like = quantile(like_count,0.25), Q3_like = quantile(like_count, 0.75), min = quantile(like_count,0),max = quantile(like_count,1))
start_narration_Y

start_narration_N <- start_narration_like %>%
  subset(start_narration_yn == "N") %>%
  summarize(count = n(), median_like = median(like_count), Q1_like = quantile(like_count,0.25), Q3_like = quantile(like_count, 0.75), min = quantile(like_count,0),max = quantile(like_count,1))
start_narration_N

start_narration_like_sum <- rbind(start_narration_Y,start_narration_N)
start_narration_like_sum <- as.data.frame(start_narration_like_sum)

rownames(start_narration_like_sum) <- c("start_narration_y","start_narration_n")
start_narration_like_sum

##
start_narration_Y <- start_narration_comment %>%
  subset(start_narration_yn == "Y") %>%
  summarize(count = n(), median_comment = median(comment_count), Q1_comment = quantile(comment_count,0.25), Q3_comment = quantile(comment_count, 0.75), min = quantile(comment_count,0),max = quantile(comment_count,1))
start_narration_Y

start_narration_N <- start_narration_comment %>%
  subset(start_narration_yn == "N") %>%
  summarize(count = n(), median_comment = median(comment_count), Q1_comment = quantile(comment_count,0.25), Q3_comment = quantile(comment_count, 0.75), min = quantile(comment_count,0),max = quantile(comment_count,1))
start_narration_N

start_narration_comment_sum <- rbind(start_narration_Y,start_narration_N)
start_narration_comment_sum <- as.data.frame(start_narration_comment_sum)

rownames(start_narration_comment_sum) <- c("start_narration_y","start_narration_n")
start_narration_comment_sum

##

start_narration_view_wilcox <- wilcox.test(view_count ~ start_narration_yn, data = start_narration_view)
start_narration_view_wilcox 

start_narration_viewday_wilcox <- wilcox.test(views_per_day ~ start_narration_yn, data = start_narration_viewday)
start_narration_viewday_wilcox 

start_narration_like_wilcox <- wilcox.test(like_count ~ start_narration_yn, data = start_narration_like)
start_narration_like_wilcox 

start_narration_comment_wilcox <- wilcox.test(comment_count ~ start_narration_yn, data = start_narration_comment)
start_narration_comment_wilcox 

# 5.9. start channel name

table(df_final$start_channel_name_yn)

df_final$start_channel_name_yn <- as.character(df_final$start_channel_name_yn)

df_final$start_channel_name_yn <- ifelse(df_final$start_channel_name_yn == "1","Y","N")
table(df_final$start_channel_name_yn)

df_final$start_channel_name_yn<- factor(df_final$start_channel_name_yn, levels=c("Y", "N"))

start_channel_name_view <- df_create_view(start_channel_name_yn)
start_channel_name_viewday <- df_create_viewday(start_channel_name_yn)
start_channel_name_like <- df_create_like(start_channel_name_yn)
start_channel_name_comment <- df_create_comment(start_channel_name_yn)

table(start_channel_name_view$start_channel_name_yn)

#
start_channel_name_view_Y <- start_channel_name_view %>%
  subset(start_channel_name_yn == "Y") %>%
  summarize(count = n(), median_view = median(view_count), Q1_view = quantile(view_count,0.25), Q3_view = quantile(view_count, 0.75), min = quantile(view_count,0),max = quantile(view_count,1))
start_channel_name_view_Y

start_channel_name_view_N <- start_channel_name_view %>%
  subset(start_channel_name_yn == "N") %>%
  summarize(count = n(), median_view = median(view_count), Q1_view = quantile(view_count,0.25), Q3_view = quantile(view_count, 0.75), min = quantile(view_count,0),max = quantile(view_count,1))
start_channel_name_view_N

start_channel_name_view_sum <- rbind(start_channel_name_view_Y,start_channel_name_view_N)
start_channel_name_view_sum <- as.data.frame(start_channel_name_view_sum)

rownames(start_channel_name_view_sum) <- c("start_channel_name_Y","start_channel_name_N")
start_channel_name_view_sum

#
start_channel_name_Y <- start_channel_name_viewday %>%
  subset(start_channel_name_yn == "Y") %>%
  summarize(count = n(), median_viewday = median(views_per_day), Q1_viewday = quantile(views_per_day,0.25), Q3_viewday = quantile(views_per_day, 0.75), min = quantile(views_per_day,0),max = quantile(views_per_day,1))
start_channel_name_Y

start_channel_name_N <- start_channel_name_viewday %>%
  subset(start_channel_name_yn == "N") %>%
  summarize(count = n(), median_viewday = median(views_per_day), Q1_viewday = quantile(views_per_day,0.25), Q3_viewday = quantile(views_per_day, 0.75), min = quantile(views_per_day,0),max = quantile(views_per_day,1))
start_channel_name_N

start_channel_name_viewday_sum <- rbind(start_channel_name_Y,start_channel_name_N)
start_channel_name_viewday_sum <- as.data.frame(start_channel_name_viewday_sum)

rownames(start_channel_name_viewday_sum) <- c("start_channel_name_y","start_channel_name_n")
start_channel_name_viewday_sum

#
start_channel_name_Y <- start_channel_name_like %>%
  subset(start_channel_name_yn == "Y") %>%
  summarize(count = n(), median_like = median(like_count), Q1_like = quantile(like_count,0.25), Q3_like = quantile(like_count, 0.75), min = quantile(like_count,0),max = quantile(like_count,1))
start_channel_name_Y

start_channel_name_N <- start_channel_name_like %>%
  subset(start_channel_name_yn == "N") %>%
  summarize(count = n(), median_like = median(like_count), Q1_like = quantile(like_count,0.25), Q3_like = quantile(like_count, 0.75), min = quantile(like_count,0),max = quantile(like_count,1))
start_channel_name_N

start_channel_name_like_sum <- rbind(start_channel_name_Y,start_channel_name_N)
start_channel_name_like_sum <- as.data.frame(start_channel_name_like_sum)

rownames(start_channel_name_like_sum) <- c("start_channel_name_y","start_channel_name_n")
start_channel_name_like_sum

#
start_channel_name_Y <- start_channel_name_comment %>%
  subset(start_channel_name_yn == "Y") %>%
  summarize(count = n(), median_comment = median(comment_count), Q1_comment = quantile(comment_count,0.25), Q3_comment = quantile(comment_count, 0.75), min = quantile(comment_count,0),max = quantile(comment_count,1))
start_channel_name_Y

start_channel_name_N <- start_channel_name_comment %>%
  subset(start_channel_name_yn == "N") %>%
  summarize(count = n(), median_comment = median(comment_count), Q1_comment = quantile(comment_count,0.25), Q3_comment = quantile(comment_count, 0.75), min = quantile(comment_count,0),max = quantile(comment_count,1))
start_channel_name_N

start_channel_name_comment_sum <- rbind(start_channel_name_Y,start_channel_name_N)
start_channel_name_comment_sum <- as.data.frame(start_channel_name_comment_sum)

rownames(start_channel_name_comment_sum) <- c("start_channel_name_y","start_channel_name_n")
start_channel_name_comment_sum

##

start_channel_name_view_wilcox <- wilcox.test(view_count ~ start_channel_name_yn, data = start_channel_name_view)
start_channel_name_view_wilcox 

start_channel_name_viewday_wilcox <- wilcox.test(views_per_day ~ start_channel_name_yn, data = start_channel_name_viewday)
start_channel_name_viewday_wilcox 

start_channel_name_like_wilcox <- wilcox.test(like_count ~ start_channel_name_yn, data = start_channel_name_like)
start_channel_name_like_wilcox 

start_channel_name_comment_wilcox <- wilcox.test(comment_count ~ start_channel_name_yn, data = start_channel_name_comment)
start_channel_name_comment_wilcox 

##intro combined## 
intro_view_summary <- rbind(intro_view_summary,start_music_view_sum, start_narration_view_sum, start_channel_name_view_sum)
intro_view_summary

intro_viewday_summary <- rbind(intro_viewday_summary,start_music_viewday_sum, start_narration_viewday_sum, start_channel_name_viewday_sum)
intro_viewday_summary

intro_like_summary <- rbind(intro_like_summary,start_music_like_sum, start_narration_like_sum, start_channel_name_like_sum)
intro_like_summary

intro_comment_summary <- rbind(intro_comment_summary,start_music_comment_sum, start_narration_comment_sum, start_channel_name_comment_sum)
intro_comment_summary

# 5.10. Music

table(df_final$music_yn)
df_final$music_yn1 <- ifelse(df_final$music_yn == "Partly - I"|df_final$music_yn == "Partly - I&c"|df_final$music_yn == "Partly - Showing questions","Partly",
                             ifelse(df_final$music_yn == "1","1","0"))

table(df_final$music_yn1)

df_final$music_yn2 <- ifelse(df_final$music_yn1 == "Partly","1",
                             ifelse(df_final$music_yn1 == "1","1","0"))

table(df_final$music_yn2)

df_final$music_yn2 <- ifelse(df_final$music_yn2 == "1","Y","N")
table(df_final$music_yn2)

df_final$music_yn2<- factor(df_final$music_yn2, levels=c("Y", "N"))

df_final$music_yn1 <- ifelse(df_final$music_yn1 == "1","Y",
                             ifelse(df_final$music_yn1 == "0","N","Partly"))
table(df_final$music_yn1)


df_final$music_yn1<- factor(df_final$music_yn1, levels=c("Y", "N","Partly"))

music_view <- df_create_view(music_yn2)
music_viewday <- df_create_viewday(music_yn2)
music_like <- df_create_like(music_yn2)
music_comment <- df_create_comment(music_yn2)

##
music_view_Y <- music_view %>%
  subset(music_yn2 == "Y") %>%
  summarize(count = n(), median_view = median(view_count), Q1_view = quantile(view_count,0.25), Q3_view = quantile(view_count, 0.75), min = quantile(view_count,0),max = quantile(view_count,1))
music_view_Y

music_view_N <- music_view %>%
  subset(music_yn2 == "N") %>%
  summarize(count = n(), median_view = median(view_count), Q1_view = quantile(view_count,0.25), Q3_view = quantile(view_count, 0.75), min = quantile(view_count,0),max = quantile(view_count,1))
music_view_N

music_view_sum <- rbind(music_view_Y,music_view_N)
music_view_sum <- as.data.frame(music_view_sum)

rownames(music_view_sum) <- c("music_Y","music_N")
music_view_sum

##
music_Y <- music_viewday %>%
  subset(music_yn2 == "Y") %>%
  summarize(count = n(), median_viewday = median(views_per_day), Q1_viewday = quantile(views_per_day,0.25), Q3_viewday = quantile(views_per_day, 0.75), min = quantile(views_per_day,0),max = quantile(views_per_day,1))
music_Y

music_N <- music_viewday %>%
  subset(music_yn2 == "N") %>%
  summarize(count = n(), median_viewday = median(views_per_day), Q1_viewday = quantile(views_per_day,0.25), Q3_viewday = quantile(views_per_day, 0.75), min = quantile(views_per_day,0),max = quantile(views_per_day,1))
music_N

music_viewday_sum <- rbind(music_Y,music_N)
music_viewday_sum <- as.data.frame(music_viewday_sum)

rownames(music_viewday_sum) <- c("music_y","music_n")
music_viewday_sum

##
music_Y <- music_like %>%
  subset(music_yn2 == "Y") %>%
  summarize(count = n(), median_like = median(like_count), Q1_like = quantile(like_count,0.25), Q3_like = quantile(like_count, 0.75), min = quantile(like_count,0),max = quantile(like_count,1))
music_Y

music_N <- music_like %>%
  subset(music_yn2 == "N") %>%
  summarize(count = n(), median_like = median(like_count), Q1_like = quantile(like_count,0.25), Q3_like = quantile(like_count, 0.75), min = quantile(like_count,0),max = quantile(like_count,1))
music_N

music_like_sum <- rbind(music_Y,music_N)
music_like_sum <- as.data.frame(music_like_sum)

rownames(music_like_sum) <- c("music_y","music_n")
music_like_sum

##
music_Y <- music_comment %>%
  subset(music_yn2 == "Y") %>%
  summarize(count = n(), median_comment = median(comment_count), Q1_comment = quantile(comment_count,0.25), Q3_comment = quantile(comment_count, 0.75), min = quantile(comment_count,0),max = quantile(comment_count,1))
music_Y

music_N <- music_comment %>%
  subset(music_yn2 == "N") %>%
  summarize(count = n(), median_comment = median(comment_count), Q1_comment = quantile(comment_count,0.25), Q3_comment = quantile(comment_count, 0.75), min = quantile(comment_count,0),max = quantile(comment_count,1))
music_N

music_comment_sum <- rbind(music_Y,music_N)
music_comment_sum <- as.data.frame(music_comment_sum)

rownames(music_comment_sum) <- c("music_y","music_n")
music_comment_sum
##

music_view_wilcox <- wilcox.test(view_count ~ music_yn2, data = music_view)
music_view_wilcox 

music_viewday_wilcox <- wilcox.test(views_per_day ~ music_yn2, data = music_viewday)
music_viewday_wilcox 

music_like_wilcox <- wilcox.test(like_count ~ music_yn2, data = music_like)
music_like_wilcox 

music_comment_wilcox <- wilcox.test(comment_count ~ music_yn2, data = music_comment)
music_comment_wilcox 

music_view2 <- df_create_view(music_yn1)
table(music_view2$music_yn1)
music_viewday2 <- df_create_viewday(music_yn1)
music_like2 <- df_create_like(music_yn1)
music_comment2 <- df_create_comment(music_yn1)

##music: Yes, No, Partly
music_view2_Y <- music_view2 %>%
  subset(music_yn1 == "Y") %>%
  summarize(count = n(), median_view = median(view_count), Q1_view = quantile(view_count,0.25), Q3_view = quantile(view_count, 0.75), min = quantile(view_count,0),max = quantile(view_count,1))
music_view2_Y

music_view2_N <- music_view2 %>%
  subset(music_yn1 == "N") %>%
  summarize(count = n(), median_view = median(view_count), Q1_view = quantile(view_count,0.25), Q3_view = quantile(view_count, 0.75), min = quantile(view_count,0),max = quantile(view_count,1))
music_view2_N

music_view2_Partly <- music_view2 %>%
  subset(music_yn1 == "Partly") %>%
  summarize(count = n(), median_view = median(view_count), Q1_view = quantile(view_count,0.25), Q3_view = quantile(view_count, 0.75), min = quantile(view_count,0),max = quantile(view_count,1))
music_view2_Partly

music_view2_sum <- rbind(music_view2_Y,music_view2_N,music_view2_Partly)
music_view2_sum <- as.data.frame(music_view2_sum)

rownames(music_view2_sum) <- c("music_Y","music_N","music_partly")
music_view2_sum

##
music_viewday2_Y <- music_viewday2 %>%
  subset(music_yn1 == "Y") %>%
  summarize(count = n(), median_viewday = median(views_per_day), Q1_viewday = quantile(views_per_day,0.25), Q3_viewday = quantile(views_per_day, 0.75), min = quantile(views_per_day,0),max = quantile(views_per_day,1))
music_viewday2_Y

music_viewday2_N <- music_viewday2 %>%
  subset(music_yn1 == "N") %>%
  summarize(count = n(), median_viewday = median(views_per_day), Q1_viewday = quantile(views_per_day,0.25), Q3_viewday = quantile(views_per_day, 0.75), min = quantile(views_per_day,0),max = quantile(views_per_day,1))
music_viewday2_N

music_viewday2_Partly <- music_viewday2 %>%
  subset(music_yn1 == "Partly") %>%
  summarize(count = n(), median_viewday = median(views_per_day), Q1_viewday = quantile(views_per_day,0.25), Q3_viewday = quantile(views_per_day, 0.75), min = quantile(views_per_day,0),max = quantile(views_per_day,1))
music_viewday2_Partly

music_viewday2_sum <- rbind(music_viewday2_Y,music_viewday2_N,music_viewday2_Partly)
music_viewday2_sum <- as.data.frame(music_viewday2_sum)

rownames(music_viewday2_sum) <- c("music_Y","music_N","music_partly")
music_viewday2_sum

##
music_like2_Y <- music_like2 %>%
  subset(music_yn1 == "Y") %>%
  summarize(count = n(), median_like = median(like_count), Q1_like = quantile(like_count,0.25), Q3_like = quantile(like_count, 0.75), min = quantile(like_count,0),max = quantile(like_count,1))
music_like2_Y

music_like2_N <- music_like2 %>%
  subset(music_yn1 == "N") %>%
  summarize(count = n(), median_like = median(like_count), Q1_like = quantile(like_count,0.25), Q3_like = quantile(like_count, 0.75), min = quantile(like_count,0),max = quantile(like_count,1))
music_like2_N

music_like2_Partly <- music_like2 %>%
  subset(music_yn1 == "Partly") %>%
  summarize(count = n(), median_like = median(like_count), Q1_like = quantile(like_count,0.25), Q3_like = quantile(like_count, 0.75), min = quantile(like_count,0),max = quantile(like_count,1))
music_like2_Partly


music_like2_sum <- rbind(music_like2_Y,music_like2_N,music_like2_Partly)
music_like2_sum <- as.data.frame(music_like2_sum)

rownames(music_like2_sum) <- c("music_Y","music_N","music_partly")
music_like2_sum

##
music_comment2_Y <- music_comment2 %>%
  subset(music_yn1 == "Y") %>%
  summarize(count = n(), median_comment = median(comment_count), Q1_comment = quantile(comment_count,0.25), Q3_comment = quantile(comment_count, 0.75), min = quantile(comment_count,0),max = quantile(comment_count,1))
music_comment2_Y

music_comment2_N <- music_comment2 %>%
  subset(music_yn1 == "N") %>%
  summarize(count = n(), median_comment = median(comment_count), Q1_comment = quantile(comment_count,0.25), Q3_comment = quantile(comment_count, 0.75), min = quantile(comment_count,0),max = quantile(comment_count,1))
music_comment2_N

music_comment2_Partly <- music_comment2 %>%
  subset(music_yn1 == "Partly") %>%
  summarize(count = n(), median_comment = median(comment_count), Q1_comment = quantile(comment_count,0.25), Q3_comment = quantile(comment_count, 0.75), min = quantile(comment_count,0),max = quantile(comment_count,1))
music_comment2_Partly

music_comment2_sum <- rbind(music_comment2_Y,music_comment2_N,music_comment2_Partly)
music_comment2_sum <- as.data.frame(music_comment2_sum)

rownames(music_comment2_sum) <- c("music_Y","music_N","music_partly")
music_comment2_sum

music_view_kruskal <- kruskal.test(view_count ~ music_yn1, data = music_view2)
music_view_kruskal 

music_viewday_kruskal <- kruskal.test(views_per_day ~ music_yn1, data = music_viewday2)
music_viewday_kruskal 

music_like_kruskal <- kruskal.test(like_count ~ music_yn1, data = music_like2)
music_like_kruskal 

music_comment_kruskal <- kruskal.test(comment_count ~ music_yn1, data = music_comment2)
music_comment_kruskal 

# 5.11. Music track
table(df_final$music_track_yn)

df_final$music_track_yn <- as.character(df_final$music_track_yn)

music_track_view <- df_final %>%
  select(music_track_yn,view_count) %>%
  filter(music_track_yn == "0"|music_track_yn == "1")

music_track_view <- music_track_view[complete.cases(music_track_view ),]

table(music_track_view$music_track_yn)

music_track_view$music_track_yn <- ifelse(music_track_view$music_track_yn == "1","Y","N")

music_track_view$music_track_yn<- factor(music_track_view$music_track_yn, levels=c("Y", "N"))
table(music_track_view$music_track_yn)

music_track_viewday <- df_final %>%
  select(music_track_yn,views_per_day) %>%
  filter(music_track_yn == "0"|music_track_yn == "1")

music_track_viewday <- music_track_viewday[complete.cases(music_track_viewday),]

music_track_viewday$music_track_yn <- ifelse(music_track_viewday$music_track_yn == "1","Y","N")

music_track_viewday$music_track_yn<- factor(music_track_viewday$music_track_yn, levels=c("Y", "N"))
table(music_track_viewday$music_track_yn)

music_track_like <- df_final %>%
  select(music_track_yn,like_count) %>%
  filter(music_track_yn == "0"|music_track_yn == "1")

music_track_like <- music_track_like[complete.cases(music_track_like),]

music_track_like$music_track_yn <- ifelse(music_track_like$music_track_yn == "1","Y","N")

music_track_like$music_track_yn<- factor(music_track_like$music_track_yn, levels=c("Y", "N"))
table(music_track_like$music_track_yn)

music_track_comment <- df_final %>%
  select(music_track_yn,comment_count) %>%
  filter(music_track_yn == "0"|music_track_yn == "1")

music_track_comment <- music_track_comment[complete.cases(music_track_comment),]

music_track_comment$music_track_yn <- ifelse(music_track_comment$music_track_yn == "1","Y","N")

music_track_comment$music_track_yn<- factor(music_track_comment$music_track_yn, levels=c("Y", "N"))
table(music_track_comment$music_track_yn)

##
music_track_view_Y <- music_track_view %>%
  subset(music_track_yn == "Y") %>%
  summarize(count = n(), median_view = median(view_count), Q1_view = quantile(view_count,0.25), Q3_view = quantile(view_count, 0.75), min = quantile(view_count,0),max = quantile(view_count,1))
music_track_view_Y

music_track_view_N <- music_track_view %>%
  subset(music_track_yn == "N") %>%
  summarize(count = n(), median_view = median(view_count), Q1_view = quantile(view_count,0.25), Q3_view = quantile(view_count, 0.75), min = quantile(view_count,0),max = quantile(view_count,1))
music_track_view_N

music_track_view_sum <- rbind(music_track_view_Y,music_track_view_N)
music_track_view_sum <- as.data.frame(music_track_view_sum)

rownames(music_track_view_sum) <- c("music_track_Y","music_track_N")
music_track_view_sum

#
music_track_Y <- music_track_viewday %>%
  subset(music_track_yn == "Y") %>%
  summarize(count = n(), median_viewday = median(views_per_day), Q1_viewday = quantile(views_per_day,0.25), Q3_viewday = quantile(views_per_day, 0.75), min = quantile(views_per_day,0),max = quantile(views_per_day,1))
music_track_Y

music_track_N <- music_track_viewday %>%
  subset(music_track_yn == "N") %>%
  summarize(count = n(), median_viewday = median(views_per_day), Q1_viewday = quantile(views_per_day,0.25), Q3_viewday = quantile(views_per_day, 0.75), min = quantile(views_per_day,0),max = quantile(views_per_day,1))
music_track_N


music_track_viewday_sum <- rbind(music_track_Y,music_track_N)
music_track_viewday_sum <- as.data.frame(music_track_viewday_sum)

rownames(music_track_viewday_sum) <- c("music_track_y","music_track_n")
music_track_viewday_sum

#
music_track_Y <- music_track_like %>%
  subset(music_track_yn == "Y") %>%
  summarize(count = n(), median_like = median(like_count), Q1_like = quantile(like_count,0.25), Q3_like = quantile(like_count, 0.75), min = quantile(like_count,0),max = quantile(like_count,1))
music_track_Y

music_track_N <- music_track_like %>%
  subset(music_track_yn == "N") %>%
  summarize(count = n(), median_like = median(like_count), Q1_like = quantile(like_count,0.25), Q3_like = quantile(like_count, 0.75), min = quantile(like_count,0),max = quantile(like_count,1))
music_track_N

music_track_like_sum <- rbind(music_track_Y,music_track_N)
music_track_like_sum <- as.data.frame(music_track_like_sum)

rownames(music_track_like_sum) <- c("music_track_y","music_track_n")
music_track_like_sum

##
music_track_Y <- music_track_comment %>%
  subset(music_track_yn == "Y") %>%
  summarize(count = n(), median_comment = median(comment_count), Q1_comment = quantile(comment_count,0.25), Q3_comment = quantile(comment_count, 0.75), min = quantile(comment_count,0),max = quantile(comment_count,1))
music_track_Y

music_track_N <- music_track_comment %>%
  subset(music_track_yn == "N") %>%
  summarize(count = n(), median_comment = median(comment_count), Q1_comment = quantile(comment_count,0.25), Q3_comment = quantile(comment_count, 0.75), min = quantile(comment_count,0),max = quantile(comment_count,1))
music_track_N

music_track_comment_sum <- rbind(music_track_Y,music_track_N)
music_track_comment_sum <- as.data.frame(music_track_comment_sum)

rownames(music_track_comment_sum) <- c("music_track_y","music_track_n")
music_track_comment_sum
##

music_track_view_wilcox <- wilcox.test(view_count ~ music_track_yn, data = music_track_view)
music_track_view_wilcox 

music_track_viewday_wilcox <- wilcox.test(views_per_day ~ music_track_yn, data = music_track_viewday)
music_track_viewday_wilcox 

music_track_like_wilcox <- wilcox.test(like_count ~ music_track_yn, data = music_track_like)
music_track_like_wilcox 

music_track_comment_wilcox <- wilcox.test(comment_count ~ music_track_yn, data = music_track_comment)
music_track_comment_wilcox 

# 5.12. Music pace
table(df_final$music_pace)

df_final$music_pace <- as.character(df_final$music_pace)

music_pace_view <- df_final %>%
  select(music_pace,view_count) %>%
  filter(music_pace == "Fast"|music_pace == "Moderate"|music_pace == "Slow")

music_pace_view <- music_pace_view[complete.cases(music_pace_view),]

table(music_pace_view$music_pace)


music_pace_viewday <- df_final %>%
  select(music_pace,views_per_day) %>%
  filter(music_pace == "Fast"|music_pace == "Moderate"|music_pace == "Slow")

music_pace_viewday <- music_pace_viewday[complete.cases(music_pace_viewday),]

music_pace_like <- df_final %>%
  select(music_pace,like_count) %>%
  filter(music_pace == "Fast"|music_pace == "Moderate"|music_pace == "Slow")

music_pace_like <- music_pace_like[complete.cases(music_pace_like),]

music_pace_comment <- df_final %>%
  select(music_pace,comment_count) %>%
  filter(music_pace == "Fast"|music_pace == "Moderate"|music_pace == "Slow")

music_pace_comment <- music_pace_comment[complete.cases(music_pace_comment),]

##
music_pace_view_Slow <- music_pace_view %>%
  subset(music_pace == "Slow") %>%
  summarize(count = n(), median_view = median(view_count), Q1_view = quantile(view_count,0.25), Q3_view = quantile(view_count, 0.75), min = quantile(view_count,0),max = quantile(view_count,1))
music_pace_view_Slow

music_pace_view_Moderate <- music_pace_view %>%
  subset(music_pace == "Moderate") %>%
  summarize(count = n(), median_view = median(view_count), Q1_view = quantile(view_count,0.25), Q3_view = quantile(view_count, 0.75), min = quantile(view_count,0),max = quantile(view_count,1))
music_pace_view_Moderate

music_pace_view_Fast <- music_pace_view %>%
  subset(music_pace == "Fast") %>%
  summarize(count = n(), median_view = median(view_count), Q1_view = quantile(view_count,0.25), Q3_view = quantile(view_count, 0.75), min = quantile(view_count,0),max = quantile(view_count,1))
music_pace_view_Fast

music_pace_view_sum <- rbind(music_pace_view_Slow,music_pace_view_Moderate,music_pace_view_Fast)
music_pace_view_sum <- as.data.frame(music_pace_view_sum)

rownames(music_pace_view_sum) <- c("music_pace_Slow","music_pace_Moderate","music_pace_Fast")
music_pace_view_sum

##
music_pace_viewday_Slow <- music_pace_viewday %>%
  subset(music_pace == "Slow") %>%
  summarize(count = n(), median_viewday = median(views_per_day), Q1_viewday = quantile(views_per_day,0.25), Q3_viewday = quantile(views_per_day, 0.75), min = quantile(views_per_day,0),max = quantile(views_per_day,1))
music_pace_viewday_Slow

music_pace_viewday_Moderate <- music_pace_viewday %>%
  subset(music_pace == "Moderate") %>%
  summarize(count = n(), median_viewday = median(views_per_day), Q1_viewday = quantile(views_per_day,0.25), Q3_viewday = quantile(views_per_day, 0.75), min = quantile(views_per_day,0),max = quantile(views_per_day,1))
music_pace_viewday_Moderate

music_pace_viewday_Fast <- music_pace_viewday %>%
  subset(music_pace == "Fast") %>%
  summarize(count = n(), median_viewday = median(views_per_day), Q1_viewday = quantile(views_per_day,0.25), Q3_viewday = quantile(views_per_day, 0.75), min = quantile(views_per_day,0),max = quantile(views_per_day,1))
music_pace_viewday_Fast


music_pace_viewday_sum <- rbind(music_pace_viewday_Slow,music_pace_viewday_Moderate,music_pace_viewday_Fast)
music_pace_viewday_sum <- as.data.frame(music_pace_viewday_sum)

rownames(music_pace_viewday_sum) <- c("music_pace_Slow","music_pace_Moderate","music_pace_Fast")
music_pace_viewday_sum

##
music_pace_like_Slow <- music_pace_like %>%
  subset(music_pace == "Slow") %>%
  summarize(count = n(), median_like = median(like_count), Q1_like = quantile(like_count,0.25), Q3_like = quantile(like_count, 0.75), min = quantile(like_count,0),max = quantile(like_count,1))
music_pace_like_Slow

music_pace_like_Moderate <- music_pace_like %>%
  subset(music_pace == "Moderate") %>%
  summarize(count = n(), median_like = median(like_count), Q1_like = quantile(like_count,0.25), Q3_like = quantile(like_count, 0.75), min = quantile(like_count,0),max = quantile(like_count,1))
music_pace_like_Moderate

music_pace_like_Fast <- music_pace_like %>%
  subset(music_pace == "Fast") %>%
  summarize(count = n(), median_like = median(like_count), Q1_like = quantile(like_count,0.25), Q3_like = quantile(like_count, 0.75), min = quantile(like_count,0),max = quantile(like_count,1))
music_pace_like_Fast

music_pace_like_sum <- rbind(music_pace_like_Slow,music_pace_like_Moderate,music_pace_like_Fast)
music_pace_like_sum <- as.data.frame(music_pace_like_sum)

rownames(music_pace_like_sum) <- c("music_pace_Slow","music_pace_Moderate","music_pace_Fast")
music_pace_like_sum

##
music_pace_comment_Slow <- music_pace_comment %>%
  subset(music_pace == "Slow") %>%
  summarize(count = n(), median_comment = median(comment_count), Q1_comment = quantile(comment_count,0.25), Q3_comment = quantile(comment_count, 0.75), min = quantile(comment_count,0),max = quantile(comment_count,1))
music_pace_comment_Slow

music_pace_comment_Moderate <- music_pace_comment %>%
  subset(music_pace == "Moderate") %>%
  summarize(count = n(), median_comment = median(comment_count), Q1_comment = quantile(comment_count,0.25), Q3_comment = quantile(comment_count, 0.75), min = quantile(comment_count,0),max = quantile(comment_count,1))
music_pace_comment_Moderate

music_pace_comment_Fast <- music_pace_comment %>%
  subset(music_pace == "Fast") %>%
  summarize(count = n(), median_comment = median(comment_count), Q1_comment = quantile(comment_count,0.25), Q3_comment = quantile(comment_count, 0.75), min = quantile(comment_count,0),max = quantile(comment_count,1))
music_pace_comment_Fast

music_pace_comment_sum <- rbind(music_pace_comment_Slow,music_pace_comment_Moderate,music_pace_comment_Fast)
music_pace_comment_sum <- as.data.frame(music_pace_comment_sum)

rownames(music_pace_comment_sum) <- c("music_pace_Slow","music_pace_Moderate","music_pace_Fast")
music_pace_comment_sum
##

music_pace_view_kruskal <- kruskal.test(view_count ~ music_pace, data = music_pace_view)
music_pace_view_kruskal 

music_pace_viewday_kruskal <- kruskal.test(views_per_day ~ music_pace, data = music_pace_viewday)
music_pace_viewday_kruskal 

music_pace_like_kruskal <- kruskal.test(like_count ~ music_pace, data = music_pace_like)
music_pace_like_kruskal 

music_pace_comment_kruskal <- kruskal.test(comment_count ~ music_pace, data = music_pace_comment)
music_pace_comment_kruskal 

##Music combined##

music_combined <- music_combine/music2_combine/music_track_combine/music_pace_combine
music_combined

music_view_summary <- rbind(music_view_sum, music_track_view_sum, music_pace_view_sum)
music_view_summary

music_viewday_summary <- rbind(music_viewday_sum, music_track_viewday_sum, music_pace_viewday_sum)
music_viewday_summary

music_like_summary <- rbind(music_like_sum, music_track_like_sum, music_pace_like_sum)
music_like_summary

music_comment_summary <- rbind(music_comment_sum, music_track_comment_sum, music_pace_comment_sum)
music_comment_summary

music_view2_summary <- rbind(music_view2_sum, music_track_view_sum, music_pace_view_sum)
music_view2_summary

music_viewday2_summary <- rbind(music_viewday2_sum, music_track_viewday_sum, music_pace_viewday_sum)
music_viewday2_summary


music_like2_summary <- rbind(music_like2_sum, music_track_like_sum, music_pace_like_sum)
music_like2_summary

music_comment2_summary <- rbind(music_comment2_sum, music_track_comment_sum, music_pace_comment_sum)
music_comment2_summary

# 5.13. Introduction length
df$intro_length_s <- as.numeric(df$intro_length_s)

summary(df$intro_length_s)

df_intro <- df %>%
  select(view_count, intro_length_s)

df_intro_clean <- df_intro[complete.cases(df_intro), ]

# 5.14. Animation_yn
table(df_final$animation_yn)

df_final$animation_yn2 <- ifelse(df_final$animation_yn == "Partly","1",
                                 ifelse(df_final$animation_yn == "1","1","0"))

table(df_final$animation_yn2)


df_final$animation_yn2 <- ifelse(df_final$animation_yn2 == "1","Y","N")
table(df_final$animation_yn2)

df_final$animation_yn2<- factor(df_final$animation_yn2, levels=c("Y", "N"))

df_final$animation_yn <- ifelse(df_final$animation_yn == "1","Y",
                                ifelse(df_final$animation_yn == "0","N","Partly"))
table(df_final$animation_yn)

df_final$animation_yn<- factor(df_final$animation_yn, levels=c("Y", "N","Partly"))
table(df_final$animation_yn)

animation_view <- df_create_view(animation_yn2)
animation_viewday <- df_create_viewday(animation_yn2)
animation_like <- df_create_like(animation_yn2)
animation_comment <- df_create_comment(animation_yn2)

##
animation_view_Y <- animation_view %>%
  subset(animation_yn2 == "Y") %>%
  summarize(count = n(), median_view = median(view_count), Q1_view = quantile(view_count,0.25), Q3_view = quantile(view_count, 0.75), min = quantile(view_count,0),max = quantile(view_count,1))
animation_view_Y

animation_view_N <- animation_view %>%
  subset(animation_yn2 == "N") %>%
  summarize(count = n(), median_view = median(view_count), Q1_view = quantile(view_count,0.25), Q3_view = quantile(view_count, 0.75), min = quantile(view_count,0),max = quantile(view_count,1))
animation_view_N

animation_view_sum <- rbind(animation_view_Y,animation_view_N)
animation_view_sum <- as.data.frame(animation_view_sum)

rownames(animation_view_sum) <- c("animation_Y","animation_N")
animation_view_sum

##
animation_Y <- animation_viewday %>%
  subset(animation_yn2 == "Y") %>%
  summarize(count = n(), median_viewday = median(views_per_day), Q1_viewday = quantile(views_per_day,0.25), Q3_viewday = quantile(views_per_day, 0.75), min = quantile(views_per_day,0),max = quantile(views_per_day,1))
animation_Y

animation_N <- animation_viewday %>%
  subset(animation_yn2 == "N") %>%
  summarize(count = n(), median_viewday = median(views_per_day), Q1_viewday = quantile(views_per_day,0.25), Q3_viewday = quantile(views_per_day, 0.75), min = quantile(views_per_day,0),max = quantile(views_per_day,1))
animation_N

animation_viewday_sum <- rbind(animation_Y,animation_N)
animation_viewday_sum <- as.data.frame(animation_viewday_sum)

rownames(animation_viewday_sum) <- c("animation_y","animation_n")
animation_viewday_sum

##
animation_Y <- animation_like %>%
  subset(animation_yn2 == "Y") %>%
  summarize(count = n(), median_like = median(like_count), Q1_like = quantile(like_count,0.25), Q3_like = quantile(like_count, 0.75), min = quantile(like_count,0),max = quantile(like_count,1))
animation_Y

animation_N <- animation_like %>%
  subset(animation_yn2 == "N") %>%
  summarize(count = n(), median_like = median(like_count), Q1_like = quantile(like_count,0.25), Q3_like = quantile(like_count, 0.75), min = quantile(like_count,0),max = quantile(like_count,1))
animation_N


animation_like_sum <- rbind(animation_Y,animation_N)
animation_like_sum <- as.data.frame(animation_like_sum)

rownames(animation_like_sum) <- c("animation_y","animation_n")
animation_like_sum

##
animation_Y <- animation_comment %>%
  subset(animation_yn2 == "Y") %>%
  summarize(count = n(), median_comment = median(comment_count), Q1_comment = quantile(comment_count,0.25), Q3_comment = quantile(comment_count, 0.75), min = quantile(comment_count,0),max = quantile(comment_count,1))
animation_Y

animation_N <- animation_comment %>%
  subset(animation_yn2 == "N") %>%
  summarize(count = n(), median_comment = median(comment_count), Q1_comment = quantile(comment_count,0.25), Q3_comment = quantile(comment_count, 0.75), min = quantile(comment_count,0),max = quantile(comment_count,1))
animation_N

animation_comment_sum <- rbind(animation_Y,animation_N)
animation_comment_sum <- as.data.frame(animation_comment_sum)

rownames(animation_comment_sum) <- c("animation_y","animation_n")
animation_comment_sum

##

animation_view_wilcox <- wilcox.test(view_count ~ animation_yn2, data = animation_view)
animation_view_wilcox 

animation_viewday_wilcox <- wilcox.test(views_per_day ~ animation_yn2, data = animation_viewday)
animation_viewday_wilcox 

animation_like_wilcox <- wilcox.test(like_count ~ animation_yn2, data = animation_like)
animation_like_wilcox 

animation_comment_wilcox <- wilcox.test(comment_count ~ animation_yn2, data = animation_comment)
animation_comment_wilcox 

# 5.15.Animation_colour_scheme

table(df_final$animation_colour_yn)

df_final$animation_colour_yn <- as.character(df_final$animation_colour_yn)

animation_colour_view <- df_final %>%
  select(animation_colour_yn,view_count) %>%
  filter(animation_colour_yn == "0"|animation_colour_yn == "1")

animation_colour_view <- animation_colour_view[complete.cases(animation_colour_view ),]

table(animation_colour_view$animation_colour_yn)

animation_colour_view$animation_colour_yn <- ifelse(animation_colour_view$animation_colour_yn == "1","Y","N")

animation_colour_view$animation_colour_yn<- factor(animation_colour_view$animation_colour_yn, levels=c("Y", "N"))
table(animation_colour_view$animation_colour_yn)

animation_colour_viewday <- df_final %>%
  select(animation_colour_yn,views_per_day) %>%
  filter(animation_colour_yn == "0"|animation_colour_yn == "1")


animation_colour_viewday <- animation_colour_viewday[complete.cases(animation_colour_viewday),]


animation_colour_viewday$animation_colour_yn <- ifelse(animation_colour_viewday$animation_colour_yn == "1","Y","N")

animation_colour_viewday$animation_colour_yn<- factor(animation_colour_viewday$animation_colour_yn, levels=c("Y", "N"))
table(animation_colour_viewday$animation_colour_yn)

animation_colour_like <- df_final %>%
  select(animation_colour_yn,like_count) %>%
  filter(animation_colour_yn == "0"|animation_colour_yn == "1")

animation_colour_like <- animation_colour_like[complete.cases(animation_colour_like),]

animation_colour_like$animation_colour_yn <- ifelse(animation_colour_like$animation_colour_yn == "1","Y","N")

animation_colour_like$animation_colour_yn<- factor(animation_colour_like$animation_colour_yn, levels=c("Y", "N"))
table(animation_colour_like$animation_colour_yn)

animation_colour_comment <- df_final %>%
  select(animation_colour_yn,comment_count) %>%
  filter(animation_colour_yn == "0"|animation_colour_yn == "1")

animation_colour_comment <- animation_colour_comment[complete.cases(animation_colour_comment),]

animation_colour_comment$animation_colour_yn <- ifelse(animation_colour_comment$animation_colour_yn == "1","Y","N")

animation_colour_comment$animation_colour_yn<- factor(animation_colour_comment$animation_colour_yn, levels=c("Y", "N"))
table(animation_colour_comment$animation_colour_yn)

##
animation_colour_view_Y <- animation_colour_view %>%
  subset(animation_colour_yn == "Y") %>%
  summarize(count = n(), median_view = median(view_count), Q1_view = quantile(view_count,0.25), Q3_view = quantile(view_count, 0.75), min = quantile(view_count,0),max = quantile(view_count,1))
animation_colour_view_Y

animation_colour_view_N <- animation_colour_view %>%
  subset(animation_colour_yn == "N") %>%
  summarize(count = n(), median_view = median(view_count), Q1_view = quantile(view_count,0.25), Q3_view = quantile(view_count, 0.75), min = quantile(view_count,0),max = quantile(view_count,1))
animation_colour_view_N

animation_colour_view_sum <- rbind(animation_colour_view_Y,animation_colour_view_N)
animation_colour_view_sum <- as.data.frame(animation_colour_view_sum)

rownames(animation_colour_view_sum) <- c("animation_colour_Y","animation_colour_N")
animation_colour_view_sum

##
animation_colour_Y <- animation_colour_viewday %>%
  subset(animation_colour_yn == "Y") %>%
  summarize(count = n(), median_viewday = median(views_per_day), Q1_viewday = quantile(views_per_day,0.25), Q3_viewday = quantile(views_per_day, 0.75), min = quantile(views_per_day,0),max = quantile(views_per_day,1))
animation_colour_Y

animation_colour_N <- animation_colour_viewday %>%
  subset(animation_colour_yn == "N") %>%
  summarize(count = n(), median_viewday = median(views_per_day), Q1_viewday = quantile(views_per_day,0.25), Q3_viewday = quantile(views_per_day, 0.75), min = quantile(views_per_day,0),max = quantile(views_per_day,1))
animation_colour_N

animation_colour_viewday_sum <- rbind(animation_colour_Y,animation_colour_N)
animation_colour_viewday_sum <- as.data.frame(animation_colour_viewday_sum)

rownames(animation_colour_viewday_sum) <- c("animation_colour_y","animation_colour_n")
animation_colour_viewday_sum

##
animation_colour_Y <- animation_colour_like %>%
  subset(animation_colour_yn == "Y") %>%
  summarize(count = n(), median_like = median(like_count), Q1_like = quantile(like_count,0.25), Q3_like = quantile(like_count, 0.75), min = quantile(like_count,0),max = quantile(like_count,1))
animation_colour_Y

animation_colour_N <- animation_colour_like %>%
  subset(animation_colour_yn == "N") %>%
  summarize(count = n(), median_like = median(like_count), Q1_like = quantile(like_count,0.25), Q3_like = quantile(like_count, 0.75), min = quantile(like_count,0),max = quantile(like_count,1))
animation_colour_N

animation_colour_like_sum <- rbind(animation_colour_Y,animation_colour_N)
animation_colour_like_sum <- as.data.frame(animation_colour_like_sum)

rownames(animation_colour_like_sum) <- c("animation_colour_y","animation_colour_n")
animation_colour_like_sum

##
animation_colour_Y <- animation_colour_comment %>%
  subset(animation_colour_yn == "Y") %>%
  summarize(count = n(), median_comment = median(comment_count), Q1_comment = quantile(comment_count,0.25), Q3_comment = quantile(comment_count, 0.75), min = quantile(comment_count,0),max = quantile(comment_count,1))
animation_colour_Y

animation_colour_N <- animation_colour_comment %>%
  subset(animation_colour_yn == "N") %>%
  summarize(count = n(), median_comment = median(comment_count), Q1_comment = quantile(comment_count,0.25), Q3_comment = quantile(comment_count, 0.75), min = quantile(comment_count,0),max = quantile(comment_count,1))
animation_colour_N


animation_colour_comment_sum <- rbind(animation_colour_Y,animation_colour_N)
animation_colour_comment_sum <- as.data.frame(animation_colour_comment_sum)

rownames(animation_colour_comment_sum) <- c("animation_colour_y","animation_colour_n")
animation_colour_comment_sum
##

animation_colour_view_wilcox <- wilcox.test(view_count ~ animation_colour_yn, data = animation_colour_view)
animation_colour_view_wilcox 

animation_colour_viewday_wilcox <- wilcox.test(views_per_day ~ animation_colour_yn, data = animation_colour_viewday)
animation_colour_viewday_wilcox 

animation_colour_like_wilcox <- wilcox.test(like_count ~ animation_colour_yn, data = animation_colour_like)
animation_colour_like_wilcox 

animation_colour_comment_wilcox <- wilcox.test(comment_count ~ animation_colour_yn, data = animation_colour_comment)
animation_colour_comment_wilcox 

# 5.16. Main animation character 

table(df_final$animation_character_yn)

df_final$animation_character_yn <- as.character(df_final$animation_character_yn)

animation_character_view <- df_final %>%
  select(animation_character_yn,view_count) %>%
  filter(animation_character_yn == "0"|animation_character_yn == "1")

animation_character_view <- animation_character_view[complete.cases(animation_character_view ),]

table(animation_character_view$animation_character_yn)

animation_character_view$animation_character_yn <- ifelse(animation_character_view$animation_character_yn == "1","Y","N")

animation_character_view$animation_character_yn<- factor(animation_character_view$animation_character_yn, levels=c("Y", "N"))
table(animation_character_view$animation_character_yn)

animation_character_viewday <- df_final %>%
  select(animation_character_yn,views_per_day) %>%
  filter(animation_character_yn == "0"|animation_character_yn == "1")

animation_character_viewday <- animation_character_viewday[complete.cases(animation_character_viewday),]

animation_character_viewday$animation_character_yn <- ifelse(animation_character_viewday$animation_character_yn == "1","Y","N")

animation_character_viewday$animation_character_yn<- factor(animation_character_viewday$animation_character_yn, levels=c("Y", "N"))
table(animation_character_viewday$animation_character_yn)

animation_character_like <- df_final %>%
  select(animation_character_yn,like_count) %>%
  filter(animation_character_yn == "0"|animation_character_yn == "1")

animation_character_like <- animation_character_like[complete.cases(animation_character_like),]

animation_character_like$animation_character_yn <- ifelse(animation_character_like$animation_character_yn == "1","Y","N")

animation_character_like$animation_character_yn<- factor(animation_character_like$animation_character_yn, levels=c("Y", "N"))
table(animation_character_like$animation_character_yn)

animation_character_comment <- df_final %>%
  select(animation_character_yn,comment_count) %>%
  filter(animation_character_yn == "0"|animation_character_yn == "1")

animation_character_comment <- animation_character_comment[complete.cases(animation_character_comment),]

animation_character_comment$animation_character_yn <- ifelse(animation_character_comment$animation_character_yn == "1","Y","N")

animation_character_comment$animation_character_yn<- factor(animation_character_comment$animation_character_yn, levels=c("Y", "N"))
table(animation_character_comment$animation_character_yn)

##
animation_character_view_Y <- animation_character_view %>%
  subset(animation_character_yn == "Y") %>%
  summarize(count = n(), median_view = median(view_count), Q1_view = quantile(view_count,0.25), Q3_view = quantile(view_count, 0.75), min = quantile(view_count,0),max = quantile(view_count,1))
animation_character_view_Y

animation_character_view_N <- animation_character_view %>%
  subset(animation_character_yn == "N") %>%
  summarize(count = n(), median_view = median(view_count), Q1_view = quantile(view_count,0.25), Q3_view = quantile(view_count, 0.75), min = quantile(view_count,0),max = quantile(view_count,1))
animation_character_view_N

animation_character_view_sum <- rbind(animation_character_view_Y,animation_character_view_N)
animation_character_view_sum <- as.data.frame(animation_character_view_sum)

rownames(animation_character_view_sum) <- c("animation_character_Y","animation_character_N")
animation_character_view_sum

##
animation_character_Y <- animation_character_viewday %>%
  subset(animation_character_yn == "Y") %>%
  summarize(count = n(), median_viewday = median(views_per_day), Q1_viewday = quantile(views_per_day,0.25), Q3_viewday = quantile(views_per_day, 0.75), min = quantile(views_per_day,0),max = quantile(views_per_day,1))
animation_character_Y

animation_character_N <- animation_character_viewday %>%
  subset(animation_character_yn == "N") %>%
  summarize(count = n(), median_viewday = median(views_per_day), Q1_viewday = quantile(views_per_day,0.25), Q3_viewday = quantile(views_per_day, 0.75), min = quantile(views_per_day,0),max = quantile(views_per_day,1))
animation_character_N

animation_character_viewday_sum <- rbind(animation_character_Y,animation_character_N)
animation_character_viewday_sum <- as.data.frame(animation_character_viewday_sum)

rownames(animation_character_viewday_sum) <- c("animation_character_y","animation_character_n")
animation_character_viewday_sum

##
animation_character_Y <- animation_character_like %>%
  subset(animation_character_yn == "Y") %>%
  summarize(count = n(), median_like = median(like_count), Q1_like = quantile(like_count,0.25), Q3_like = quantile(like_count, 0.75), min = quantile(like_count,0),max = quantile(like_count,1))
animation_character_Y

animation_character_N <- animation_character_like %>%
  subset(animation_character_yn == "N") %>%
  summarize(count = n(), median_like = median(like_count), Q1_like = quantile(like_count,0.25), Q3_like = quantile(like_count, 0.75), min = quantile(like_count,0),max = quantile(like_count,1))
animation_character_N

animation_character_like_sum <- rbind(animation_character_Y,animation_character_N)
animation_character_like_sum <- as.data.frame(animation_character_like_sum)

rownames(animation_character_like_sum) <- c("animation_character_y","animation_character_n")
animation_character_like_sum

##
animation_character_Y <- animation_character_comment %>%
  subset(animation_character_yn == "Y") %>%
  summarize(count = n(), median_comment = median(comment_count), Q1_comment = quantile(comment_count,0.25), Q3_comment = quantile(comment_count, 0.75), min = quantile(comment_count,0),max = quantile(comment_count,1))
animation_character_Y

animation_character_N <- animation_character_comment %>%
  subset(animation_character_yn == "N") %>%
  summarize(count = n(), median_comment = median(comment_count), Q1_comment = quantile(comment_count,0.25), Q3_comment = quantile(comment_count, 0.75), min = quantile(comment_count,0),max = quantile(comment_count,1))
animation_character_N


animation_character_comment_sum <- rbind(animation_character_Y,animation_character_N)
animation_character_comment_sum <- as.data.frame(animation_character_comment_sum)

rownames(animation_character_comment_sum) <- c("animation_character_y","animation_character_n")
animation_character_comment_sum

##

animation_character_view_wilcox <- wilcox.test(view_count ~ animation_character_yn, data = animation_character_view)
animation_character_view_wilcox 

animation_character_viewday_wilcox <- wilcox.test(views_per_day ~ animation_character_yn, data = animation_character_viewday)
animation_character_viewday_wilcox 

animation_character_like_wilcox <- wilcox.test(like_count ~ animation_character_yn, data = animation_character_like)
animation_character_like_wilcox 

animation_character_comment_wilcox <- wilcox.test(comment_count ~ animation_character_yn, data = animation_character_comment)
animation_character_comment_wilcox 


# 5.17. Does the animation have sound effect? 

table(df_final$sound_effect_yn)

sound_effect_view <- df_final %>%
  select(sound_effect_yn,view_count) %>%
  filter(sound_effect_yn == "0"|sound_effect_yn == "1")

sound_effect_view <- sound_effect_view[complete.cases(sound_effect_view ),]

table(sound_effect_view$sound_effect_yn)

sound_effect_view$sound_effect_yn <- ifelse(sound_effect_view$sound_effect_yn == "1","Y","N")

sound_effect_view$sound_effect_yn<- factor(sound_effect_view$sound_effect_yn, levels=c("Y", "N"))
table(sound_effect_view$sound_effect_yn)

sound_effect_viewday <- df_final %>%
  select(sound_effect_yn,views_per_day) %>%
  filter(sound_effect_yn == "0"|sound_effect_yn == "1")

sound_effect_viewday <- sound_effect_viewday[complete.cases(sound_effect_viewday),]

sound_effect_viewday$sound_effect_yn <- ifelse(sound_effect_viewday$sound_effect_yn == "1","Y","N")

sound_effect_viewday$sound_effect_yn<- factor(sound_effect_viewday$sound_effect_yn, levels=c("Y", "N"))
table(sound_effect_viewday$sound_effect_yn)

sound_effect_like <- df_final %>%
  select(sound_effect_yn,like_count) %>%
  filter(sound_effect_yn == "0"|sound_effect_yn == "1")

sound_effect_like <- sound_effect_like[complete.cases(sound_effect_like),]

sound_effect_like$sound_effect_yn <- ifelse(sound_effect_like$sound_effect_yn == "1","Y","N")

sound_effect_like$sound_effect_yn<- factor(sound_effect_like$sound_effect_yn, levels=c("Y", "N"))
table(sound_effect_like$sound_effect_yn)


sound_effect_comment <- df_final %>%
  select(sound_effect_yn,comment_count) %>%
  filter(sound_effect_yn == "0"|sound_effect_yn == "1")


sound_effect_comment <- sound_effect_comment[complete.cases(sound_effect_comment),]


sound_effect_comment$sound_effect_yn <- ifelse(sound_effect_comment$sound_effect_yn == "1","Y","N")

sound_effect_comment$sound_effect_yn<- factor(sound_effect_comment$sound_effect_yn, levels=c("Y", "N"))
table(sound_effect_comment$sound_effect_yn)

##
sound_effect_view_Y <- sound_effect_view %>%
  subset(sound_effect_yn == "Y") %>%
  summarize(count = n(), median_view = median(view_count), Q1_view = quantile(view_count,0.25), Q3_view = quantile(view_count, 0.75), min = quantile(view_count,0),max = quantile(view_count,1))
sound_effect_view_Y

sound_effect_view_N <- sound_effect_view %>%
  subset(sound_effect_yn == "N") %>%
  summarize(count = n(), median_view = median(view_count), Q1_view = quantile(view_count,0.25), Q3_view = quantile(view_count, 0.75), min = quantile(view_count,0),max = quantile(view_count,1))
sound_effect_view_N

sound_effect_view_sum <- rbind(sound_effect_view_Y,sound_effect_view_N)
sound_effect_view_sum <- as.data.frame(sound_effect_view_sum)

rownames(sound_effect_view_sum) <- c("sound_effect_Y","sound_effect_N")
sound_effect_view_sum

##
sound_effect_Y <- sound_effect_viewday %>%
  subset(sound_effect_yn == "Y") %>%
  summarize(count = n(), median_viewday = median(views_per_day), Q1_viewday = quantile(views_per_day,0.25), Q3_viewday = quantile(views_per_day, 0.75), min = quantile(views_per_day,0),max = quantile(views_per_day,1))
sound_effect_Y

sound_effect_N <- sound_effect_viewday %>%
  subset(sound_effect_yn == "N") %>%
  summarize(count = n(), median_viewday = median(views_per_day), Q1_viewday = quantile(views_per_day,0.25), Q3_viewday = quantile(views_per_day, 0.75), min = quantile(views_per_day,0),max = quantile(views_per_day,1))
sound_effect_N

sound_effect_viewday_sum <- rbind(sound_effect_Y,sound_effect_N)
sound_effect_viewday_sum <- as.data.frame(sound_effect_viewday_sum)

rownames(sound_effect_viewday_sum) <- c("sound_effect_y","sound_effect_n")
sound_effect_viewday_sum

##
sound_effect_Y <- sound_effect_like %>%
  subset(sound_effect_yn == "Y") %>%
  summarize(count = n(), median_like = median(like_count), Q1_like = quantile(like_count,0.25), Q3_like = quantile(like_count, 0.75), min = quantile(like_count,0),max = quantile(like_count,1))
sound_effect_Y

sound_effect_N <- sound_effect_like %>%
  subset(sound_effect_yn == "N") %>%
  summarize(count = n(), median_like = median(like_count), Q1_like = quantile(like_count,0.25), Q3_like = quantile(like_count, 0.75), min = quantile(like_count,0),max = quantile(like_count,1))
sound_effect_N

sound_effect_like_sum <- rbind(sound_effect_Y,sound_effect_N)
sound_effect_like_sum <- as.data.frame(sound_effect_like_sum)

rownames(sound_effect_like_sum) <- c("sound_effect_y","sound_effect_n")
sound_effect_like_sum

##
sound_effect_Y <- sound_effect_comment %>%
  subset(sound_effect_yn == "Y") %>%
  summarize(count = n(), median_comment = median(comment_count), Q1_comment = quantile(comment_count,0.25), Q3_comment = quantile(comment_count, 0.75), min = quantile(comment_count,0),max = quantile(comment_count,1))
sound_effect_Y

sound_effect_N <- sound_effect_comment %>%
  subset(sound_effect_yn == "N") %>%
  summarize(count = n(), median_comment = median(comment_count), Q1_comment = quantile(comment_count,0.25), Q3_comment = quantile(comment_count, 0.75), min = quantile(comment_count,0),max = quantile(comment_count,1))
sound_effect_N


sound_effect_comment_sum <- rbind(sound_effect_Y,sound_effect_N)
sound_effect_comment_sum <- as.data.frame(sound_effect_comment_sum)

rownames(sound_effect_comment_sum) <- c("sound_effect_y","sound_effect_n")
sound_effect_comment_sum
##

sound_effect_view_wilcox <- wilcox.test(view_count ~ sound_effect_yn, data = sound_effect_view)
sound_effect_view_wilcox 

sound_effect_viewday_wilcox <- wilcox.test(views_per_day ~ sound_effect_yn, data = sound_effect_viewday)
sound_effect_viewday_wilcox 

sound_effect_like_wilcox <- wilcox.test(like_count ~ sound_effect_yn, data = sound_effect_like)
sound_effect_like_wilcox 

sound_effect_comment_wilcox <- wilcox.test(comment_count ~ sound_effect_yn, data = sound_effect_comment)
sound_effect_comment_wilcox 

##Animation combined##

animation_view_summary <- rbind(animation_view_sum,animation_colour_view_sum,animation_character_view_sum,sound_effect_view_sum)
animation_view_summary 

animation_viewday_summary <- rbind(animation_viewday_sum,animation_colour_viewday_sum,animation_character_viewday_sum,sound_effect_viewday_sum)
animation_viewday_summary 

animation_like_summary <- rbind(animation_like_sum,animation_colour_like_sum,animation_character_like_sum,sound_effect_like_sum)
animation_like_summary 

animation_comment_summary <- rbind(animation_comment_sum,animation_colour_comment_sum,animation_character_comment_sum,sound_effect_comment_sum)
animation_comment_summary 

# 5.18. Presenter accreditation ###########
table(df_final$presenter_accred_yn)

df_final$presenter_accred_yn <- as.character(df_final$presenter_accred_yn)

presenter_accred_view <- df_final %>%
  select(presenter_accred_yn,view_count) %>%
  filter(presenter_accred_yn == "0"|presenter_accred_yn == "1")

presenter_accred_view <- presenter_accred_view[complete.cases(presenter_accred_view ),]

table(presenter_accred_view$presenter_accred_yn)

presenter_accred_view$presenter_accred_yn <- ifelse(presenter_accred_view$presenter_accred_yn == "1","Y","N")

presenter_accred_view$presenter_accred_yn<- factor(presenter_accred_view$presenter_accred_yn, levels=c("Y", "N"))
table(presenter_accred_view$presenter_accred_yn)

presenter_accred_viewday <- df_final %>%
  select(presenter_accred_yn,views_per_day) %>%
  filter(presenter_accred_yn == "0"|presenter_accred_yn == "1")

presenter_accred_viewday <- presenter_accred_viewday[complete.cases(presenter_accred_viewday),]

presenter_accred_viewday$presenter_accred_yn <- ifelse(presenter_accred_viewday$presenter_accred_yn == "1","Y","N")

presenter_accred_viewday$presenter_accred_yn<- factor(presenter_accred_viewday$presenter_accred_yn, levels=c("Y", "N"))
table(presenter_accred_viewday$presenter_accred_yn)

presenter_accred_like <- df_final %>%
  select(presenter_accred_yn,like_count) %>%
  filter(presenter_accred_yn == "0"|presenter_accred_yn == "1")

presenter_accred_like <- presenter_accred_like[complete.cases(presenter_accred_like),]

presenter_accred_like$presenter_accred_yn <- ifelse(presenter_accred_like$presenter_accred_yn == "1","Y","N")

presenter_accred_like$presenter_accred_yn<- factor(presenter_accred_like$presenter_accred_yn, levels=c("Y", "N"))
table(presenter_accred_like$presenter_accred_yn)

presenter_accred_comment <- df_final %>%
  select(presenter_accred_yn,comment_count) %>%
  filter(presenter_accred_yn == "0"|presenter_accred_yn == "1")

presenter_accred_comment <- presenter_accred_comment[complete.cases(presenter_accred_comment),]

presenter_accred_comment$presenter_accred_yn <- ifelse(presenter_accred_comment$presenter_accred_yn == "1","Y","N")

presenter_accred_comment$presenter_accred_yn<- factor(presenter_accred_comment$presenter_accred_yn, levels=c("Y", "N"))
table(presenter_accred_comment$presenter_accred_yn)

##
presenter_accred_view_Y <- presenter_accred_view %>%
  subset(presenter_accred_yn == "Y") %>%
  summarize(count = n(), median_view = median(view_count), Q1_view = quantile(view_count,0.25), Q3_view = quantile(view_count, 0.75), min = quantile(view_count,0),max = quantile(view_count,1))
presenter_accred_view_Y

presenter_accred_view_N <- presenter_accred_view %>%
  subset(presenter_accred_yn == "N") %>%
  summarize(count = n(), median_view = median(view_count), Q1_view = quantile(view_count,0.25), Q3_view = quantile(view_count, 0.75), min = quantile(view_count,0),max = quantile(view_count,1))
presenter_accred_view_N


presenter_accred_view_sum <- rbind(presenter_accred_view_Y,presenter_accred_view_N)
presenter_accred_view_sum <- as.data.frame(presenter_accred_view_sum)

rownames(presenter_accred_view_sum) <- c("presenter_accred_Y","presenter_accred_N")
presenter_accred_view_sum

##
presenter_accred_Y <- presenter_accred_viewday %>%
  subset(presenter_accred_yn == "Y") %>%
  summarize(count = n(), median_viewday = median(views_per_day), Q1_viewday = quantile(views_per_day,0.25), Q3_viewday = quantile(views_per_day, 0.75), min = quantile(views_per_day,0),max = quantile(views_per_day,1))
presenter_accred_Y

presenter_accred_N <- presenter_accred_viewday %>%
  subset(presenter_accred_yn == "N") %>%
  summarize(count = n(), median_viewday = median(views_per_day), Q1_viewday = quantile(views_per_day,0.25), Q3_viewday = quantile(views_per_day, 0.75), min = quantile(views_per_day,0),max = quantile(views_per_day,1))
presenter_accred_N

presenter_accred_viewday_sum <- rbind(presenter_accred_Y,presenter_accred_N)
presenter_accred_viewday_sum <- as.data.frame(presenter_accred_viewday_sum)

rownames(presenter_accred_viewday_sum) <- c("presenter_accred_y","presenter_accred_n")
presenter_accred_viewday_sum

##
presenter_accred_Y <- presenter_accred_like %>%
  subset(presenter_accred_yn == "Y") %>%
  summarize(count = n(), median_like = median(like_count), Q1_like = quantile(like_count,0.25), Q3_like = quantile(like_count, 0.75), min = quantile(like_count,0),max = quantile(like_count,1))
presenter_accred_Y

presenter_accred_N <- presenter_accred_like %>%
  subset(presenter_accred_yn == "N") %>%
  summarize(count = n(), median_like = median(like_count), Q1_like = quantile(like_count,0.25), Q3_like = quantile(like_count, 0.75), min = quantile(like_count,0),max = quantile(like_count,1))
presenter_accred_N

presenter_accred_like_sum <- rbind(presenter_accred_Y,presenter_accred_N)
presenter_accred_like_sum <- as.data.frame(presenter_accred_like_sum)

rownames(presenter_accred_like_sum) <- c("presenter_accred_y","presenter_accred_n")
presenter_accred_like_sum

##
presenter_accred_Y <- presenter_accred_comment %>%
  subset(presenter_accred_yn == "Y") %>%
  summarize(count = n(), median_comment = median(comment_count), Q1_comment = quantile(comment_count,0.25), Q3_comment = quantile(comment_count, 0.75), min = quantile(comment_count,0),max = quantile(comment_count,1))
presenter_accred_Y

presenter_accred_N <- presenter_accred_comment %>%
  subset(presenter_accred_yn == "N") %>%
  summarize(count = n(), median_comment = median(comment_count), Q1_comment = quantile(comment_count,0.25), Q3_comment = quantile(comment_count, 0.75), min = quantile(comment_count,0),max = quantile(comment_count,1))
presenter_accred_N

presenter_accred_comment_sum <- rbind(presenter_accred_Y,presenter_accred_N)
presenter_accred_comment_sum <- as.data.frame(presenter_accred_comment_sum)

rownames(presenter_accred_comment_sum) <- c("presenter_accred_y","presenter_accred_n")
presenter_accred_comment_sum
##

presenter_accred_view_wilcox <- wilcox.test(view_count ~ presenter_accred_yn, data = presenter_accred_view)
presenter_accred_view_wilcox 

presenter_accred_viewday_wilcox <- wilcox.test(views_per_day ~ presenter_accred_yn, data = presenter_accred_viewday)
presenter_accred_viewday_wilcox 

presenter_accred_like_wilcox <- wilcox.test(like_count ~ presenter_accred_yn, data = presenter_accred_like)
presenter_accred_like_wilcox 

presenter_accred_comment_wilcox <- wilcox.test(comment_count ~ presenter_accred_yn, data = presenter_accred_comment)
presenter_accred_comment_wilcox 

# 5.19. Presenter gender #####

table(df_final$narrator_gender)


df_final$narrator_genders <- ifelse(df_final$narrator_gender == "Female","Female",
                                    ifelse(df_final$narrator_gender == "Male","Male",
                                           ifelse(df_final$narrator_gender == "NA","NA",
                                                  ifelse(df_final$narrator_gender == "No narrator","NA", "Others"))))

table(df_final$narrator_genders)

narrator_gender_view <- df_final %>%
  select(narrator_genders,view_count) %>%
  filter(narrator_genders == "Female"|narrator_genders == "Male"|narrator_genders == "Others")

narrator_gender_view <- narrator_gender_view[complete.cases(narrator_gender_view),]

table(narrator_gender_view$narrator_genders)

narrator_gender_viewday <- df_final %>%
  select(narrator_genders,views_per_day) %>%
  filter(narrator_genders == "Female"|narrator_genders == "Male"|narrator_genders == "Others")

narrator_gender_viewday <- narrator_gender_viewday[complete.cases(narrator_gender_viewday),]

narrator_gender_like <- df_final %>%
  select(narrator_genders,like_count) %>%
  filter(narrator_genders == "Female"|narrator_genders == "Male"|narrator_genders == "Others")

narrator_gender_like <- narrator_gender_like[complete.cases(narrator_gender_like),]

narrator_gender_comment <- df_final %>%
  select(narrator_genders,comment_count) %>%
  filter(narrator_genders == "Female"|narrator_genders == "Male"|narrator_genders == "Others")

narrator_gender_comment <- narrator_gender_comment[complete.cases(narrator_gender_comment),]

##
narrator_gender_view_Female <- narrator_gender_view %>%
  subset(narrator_genders == "Female") %>%
  summarize(count = n(), median_view = median(view_count), Q1_view = quantile(view_count,0.25), Q3_view = quantile(view_count, 0.75), min = quantile(view_count,0),max = quantile(view_count,1))
narrator_gender_view_Female

narrator_gender_view_Male <- narrator_gender_view %>%
  subset(narrator_genders == "Male") %>%
  summarize(count = n(), median_view = median(view_count), Q1_view = quantile(view_count,0.25), Q3_view = quantile(view_count, 0.75), min = quantile(view_count,0),max = quantile(view_count,1))
narrator_gender_view_Male

narrator_gender_view_Others <- narrator_gender_view %>%
  subset(narrator_genders == "Others") %>%
  summarize(count = n(), median_view = median(view_count), Q1_view = quantile(view_count,0.25), Q3_view = quantile(view_count, 0.75), min = quantile(view_count,0),max = quantile(view_count,1))
narrator_gender_view_Others

narrator_gender_view_sum <- rbind(narrator_gender_view_Female,narrator_gender_view_Male,narrator_gender_view_Others)
narrator_gender_view_sum <- as.data.frame(narrator_gender_view_sum)

rownames(narrator_gender_view_sum) <- c("narrator_gender_Female","narrator_gender_Male","narrator_gender_Others")
narrator_gender_view_sum

##
narrator_gender_viewday_Female <- narrator_gender_viewday %>%
  subset(narrator_genders == "Female") %>%
  summarize(count = n(), median_viewday = median(views_per_day), Q1_viewday = quantile(views_per_day,0.25), Q3_viewday = quantile(views_per_day, 0.75), min = quantile(views_per_day,0),max = quantile(views_per_day,1))
narrator_gender_viewday_Female

narrator_gender_viewday_Male <- narrator_gender_viewday %>%
  subset(narrator_genders == "Male") %>%
  summarize(count = n(), median_viewday = median(views_per_day), Q1_viewday = quantile(views_per_day,0.25), Q3_viewday = quantile(views_per_day, 0.75), min = quantile(views_per_day,0),max = quantile(views_per_day,1))
narrator_gender_viewday_Male

narrator_gender_viewday_Others <- narrator_gender_viewday %>%
  subset(narrator_genders == "Others") %>%
  summarize(count = n(), median_viewday = median(views_per_day), Q1_viewday = quantile(views_per_day,0.25), Q3_viewday = quantile(views_per_day, 0.75), min = quantile(views_per_day,0),max = quantile(views_per_day,1))
narrator_gender_viewday_Others

narrator_gender_viewday_sum <- rbind(narrator_gender_viewday_Female,narrator_gender_viewday_Male,narrator_gender_viewday_Others)
narrator_gender_viewday_sum <- as.data.frame(narrator_gender_viewday_sum)

rownames(narrator_gender_viewday_sum) <- c("narrator_gender_Female","narrator_gender_Male","narrator_gender_Others")
narrator_gender_viewday_sum

##
narrator_gender_like_Female <- narrator_gender_like %>%
  subset(narrator_genders == "Female") %>%
  summarize(count = n(), median_like = median(like_count), Q1_like = quantile(like_count,0.25), Q3_like = quantile(like_count, 0.75), min = quantile(like_count,0),max = quantile(like_count,1))
narrator_gender_like_Female

narrator_gender_like_Male <- narrator_gender_like %>%
  subset(narrator_genders == "Male") %>%
  summarize(count = n(), median_like = median(like_count), Q1_like = quantile(like_count,0.25), Q3_like = quantile(like_count, 0.75), min = quantile(like_count,0),max = quantile(like_count,1))
narrator_gender_like_Male

narrator_gender_like_Others <- narrator_gender_like %>%
  subset(narrator_genders == "Others") %>%
  summarize(count = n(), median_like = median(like_count), Q1_like = quantile(like_count,0.25), Q3_like = quantile(like_count, 0.75), min = quantile(like_count,0),max = quantile(like_count,1))
narrator_gender_like_Others


narrator_gender_like_sum <- rbind(narrator_gender_like_Female,narrator_gender_like_Male,narrator_gender_like_Others)
narrator_gender_like_sum <- as.data.frame(narrator_gender_like_sum)

rownames(narrator_gender_like_sum) <- c("narrator_gender_Female","narrator_gender_Male","narrator_gender_Others")
narrator_gender_like_sum

##
narrator_gender_comment_Female <- narrator_gender_comment %>%
  subset(narrator_genders == "Female") %>%
  summarize(count = n(), median_comment = median(comment_count), Q1_comment = quantile(comment_count,0.25), Q3_comment = quantile(comment_count, 0.75), min = quantile(comment_count,0),max = quantile(comment_count,1))
narrator_gender_comment_Female

narrator_gender_comment_Male <- narrator_gender_comment %>%
  subset(narrator_genders == "Male") %>%
  summarize(count = n(), median_comment = median(comment_count), Q1_comment = quantile(comment_count,0.25), Q3_comment = quantile(comment_count, 0.75), min = quantile(comment_count,0),max = quantile(comment_count,1))
narrator_gender_comment_Male

narrator_gender_comment_Others <- narrator_gender_comment %>%
  subset(narrator_genders == "Others") %>%
  summarize(count = n(), median_comment = median(comment_count), Q1_comment = quantile(comment_count,0.25), Q3_comment = quantile(comment_count, 0.75), min = quantile(comment_count,0),max = quantile(comment_count,1))
narrator_gender_comment_Others


narrator_gender_comment_sum <- rbind(narrator_gender_comment_Female,narrator_gender_comment_Male,narrator_gender_comment_Others)
narrator_gender_comment_sum <- as.data.frame(narrator_gender_comment_sum)

rownames(narrator_gender_comment_sum) <- c("narrator_gender_Female","narrator_gender_Male","narrator_gender_Others")
narrator_gender_comment_sum
##

narrator_gender_view_kruskal <- kruskal.test(view_count ~ narrator_genders, data = narrator_gender_view)
narrator_gender_view_kruskal 

narrator_gender_view_dunn <- dunnTest(view_count ~ narrator_genders, data = narrator_gender_view, method = "bonferroni")
narrator_gender_view_dunn

narrator_gender_viewday_kruskal <- kruskal.test(views_per_day ~ narrator_genders, data = narrator_gender_viewday)
narrator_gender_viewday_kruskal 


narrator_gender_viewday_dunn <- dunnTest(views_per_day ~ narrator_genders, data = narrator_gender_viewday, method = "bonferroni")
narrator_gender_viewday_dunn


narrator_gender_like_kruskal <- kruskal.test(like_count ~ narrator_genders, data = narrator_gender_like)
narrator_gender_like_kruskal 

narrator_gender_comment_kruskal <- kruskal.test(comment_count ~ narrator_genders, data = narrator_gender_comment)
narrator_gender_comment_kruskal 

narrator_gender_comment_dunn <- dunnTest(comment_count ~ narrator_genders, data = narrator_gender_comment, method = "bonferroni")
narrator_gender_comment_dunn


#Post-hoc test#
gender_dunn <- dunnTest(view_count ~ final, data = df_narrator_gender, method = "bonferroni")
gender_dunn

# 5.21. Interviewee y/n 
df_interviewee$interviewee_yn <- df_interviewee$`Do they have additional interviewees? Y/N`

df_interviewee$interviewee_yn <- factor(df_interviewee$interviewee_yn, levels=c("Y", "N"))
table(df_interviewee$interviewee_yn)

df_interviewee_y_view <- df_interviewee %>%
  select(interviewee_yn,view_count)

df_interviewee_y_view <- df_interviewee_y_view[complete.cases(df_interviewee_y_view),]

table(df_interviewee_y_view$interviewee_yn)

df_interviewee_y_viewday <- df_interviewee %>%
  select(interviewee_yn,views_per_day)
df_interviewee_y_viewday <- df_interviewee_y_viewday[complete.cases(df_interviewee_y_viewday),]

table(df_interviewee_y_viewday$interviewee_yn)

df_interviewee_y_like <- df_interviewee %>%
  select(interviewee_yn,like_count)
df_interviewee_y_like <- df_interviewee_y_like[complete.cases(df_interviewee_y_like),]

table(df_interviewee_y_like$interviewee_yn)

df_interviewee_y_comment <- df_interviewee %>%
  select(interviewee_yn,comment_count)
df_interviewee_y_comment <- df_interviewee_y_comment[complete.cases(df_interviewee_y_comment),]

table(df_interviewee_y_comment$interviewee_yn)

##
df_interviewee_view_Y <- df_interviewee_y_view %>%
  subset(interviewee_yn == "Y") %>%
  summarize(count = n(), median_view = median(view_count), Q1_view = quantile(view_count,0.25), Q3_view = quantile(view_count, 0.75), min = quantile(view_count,0),max = quantile(view_count,1))
df_interviewee_view_Y

df_interviewee_view_N <- df_interviewee_y_view %>%
  subset(interviewee_yn == "N") %>%
  summarize(count = n(), median_view = median(view_count), Q1_view = quantile(view_count,0.25), Q3_view = quantile(view_count, 0.75), min = quantile(view_count,0),max = quantile(view_count,1))
df_interviewee_view_N

df_interviewee_view_sum <- rbind(df_interviewee_view_Y,df_interviewee_view_N)
df_interviewee_view_sum <- as.data.frame(df_interviewee_view_sum)

rownames(df_interviewee_view_sum) <- c("df_interviewee_Y","df_interviewee_N")
df_interviewee_view_sum

##
df_interviewee_Y <- df_interviewee_y_viewday %>%
  subset(interviewee_yn == "Y") %>%
  summarize(count = n(), median_viewday = median(views_per_day), Q1_viewday = quantile(views_per_day,0.25), Q3_viewday = quantile(views_per_day, 0.75), min = quantile(views_per_day,0),max = quantile(views_per_day,1))
df_interviewee_Y

df_interviewee_N <- df_interviewee_y_viewday %>%
  subset(interviewee_yn == "N") %>%
  summarize(count = n(), median_viewday = median(views_per_day), Q1_viewday = quantile(views_per_day,0.25), Q3_viewday = quantile(views_per_day, 0.75), min = quantile(views_per_day,0),max = quantile(views_per_day,1))
df_interviewee_N

df_interviewee_viewday_sum <- rbind(df_interviewee_Y,df_interviewee_N)
df_interviewee_viewday_sum <- as.data.frame(df_interviewee_viewday_sum)

rownames(df_interviewee_viewday_sum) <- c("df_interviewee_y","df_interviewee_n")
df_interviewee_viewday_sum

##
df_interviewee_Y <- df_interviewee_y_like %>%
  subset(interviewee_yn == "Y") %>%
  summarize(count = n(), median_like = median(like_count), Q1_like = quantile(like_count,0.25), Q3_like = quantile(like_count, 0.75), min = quantile(like_count,0),max = quantile(like_count,1))
df_interviewee_Y

df_interviewee_N <- df_interviewee_y_like %>%
  subset(interviewee_yn == "N") %>%
  summarize(count = n(), median_like = median(like_count), Q1_like = quantile(like_count,0.25), Q3_like = quantile(like_count, 0.75), min = quantile(like_count,0),max = quantile(like_count,1))
df_interviewee_N


df_interviewee_like_sum <- rbind(df_interviewee_Y,df_interviewee_N)
df_interviewee_like_sum <- as.data.frame(df_interviewee_like_sum)

rownames(df_interviewee_like_sum) <- c("df_interviewee_y","df_interviewee_n")
df_interviewee_like_sum

##
df_interviewee_Y <- df_interviewee_y_comment %>%
  subset(interviewee_yn == "Y") %>%
  summarize(count = n(), median_comment = median(comment_count), Q1_comment = quantile(comment_count,0.25), Q3_comment = quantile(comment_count, 0.75), min = quantile(comment_count,0),max = quantile(comment_count,1))
df_interviewee_Y

df_interviewee_N <- df_interviewee_y_comment %>%
  subset(interviewee_yn == "N") %>%
  summarize(count = n(), median_comment = median(comment_count), Q1_comment = quantile(comment_count,0.25), Q3_comment = quantile(comment_count, 0.75), min = quantile(comment_count,0),max = quantile(comment_count,1))
df_interviewee_N

df_interviewee_comment_sum <- rbind(df_interviewee_Y,df_interviewee_N)
df_interviewee_comment_sum <- as.data.frame(df_interviewee_comment_sum)

rownames(df_interviewee_comment_sum) <- c("df_interviewee_y","df_interviewee_n")
df_interviewee_comment_sum

##

df_interviewee_view_wilcox <- wilcox.test(view_count ~ interviewee_yn, data = df_interviewee_y_view)
df_interviewee_view_wilcox 

df_interviewee_viewday_wilcox <- wilcox.test(views_per_day ~ interviewee_yn, data = df_interviewee_y_viewday)
df_interviewee_viewday_wilcox 

df_interviewee_like_wilcox <- wilcox.test(like_count ~ interviewee_yn, data = df_interviewee_y_like)
df_interviewee_like_wilcox 

df_interviewee_comment_wilcox <- wilcox.test(comment_count ~ interviewee_yn, data = df_interviewee_y_comment)
df_interviewee_comment_wilcox 

# 5.22. Does the view have expert interviewees?
df_interviewee$expert_total_chr <- as.character(df_interviewee$expert_total)
df_interviewee$expert_total

df_interviewee$expert_yn <- ifelse(df_interviewee$expert_total == "NA","N","Y")
table(df_interviewee$expert_yn)


df_interviewee$expert_yn <- factor(df_interviewee$expert_yn, levels=c("Y", "N"))
table(df_interviewee$expert_yn)

df_expert_y_view <- df_interviewee %>%
  select(expert_yn,view_count)

df_expert_y_view <- df_expert_y_view[complete.cases(df_expert_y_view),]

df_expert_y_viewday <- df_interviewee %>%
  select(expert_yn,views_per_day)
df_expert_y_viewday <- df_expert_y_viewday[complete.cases(df_expert_y_viewday),]

df_expert_y_like <- df_interviewee %>%
  select(expert_yn,like_count)
df_expert_y_like <- df_expert_y_like[complete.cases(df_expert_y_like),]

df_expert_y_comment <- df_interviewee %>%
  select(expert_yn,comment_count)
df_expert_y_comment <- df_expert_y_comment[complete.cases(df_expert_y_comment),]

table(df_expert_y_comment$expert_yn)

##
df_expert_view_Y <- df_expert_y_view %>%
  subset(expert_yn == "Y") %>%
  summarize(count = n(), median_view = median(view_count), Q1_view = quantile(view_count,0.25), Q3_view = quantile(view_count, 0.75), min = quantile(view_count,0),max = quantile(view_count,1))
df_expert_view_Y

df_expert_view_N <- df_expert_y_view %>%
  subset(expert_yn == "N") %>%
  summarize(count = n(), median_view = median(view_count), Q1_view = quantile(view_count,0.25), Q3_view = quantile(view_count, 0.75), min = quantile(view_count,0),max = quantile(view_count,1))
df_expert_view_N

df_expert_view_sum <- rbind(df_expert_view_Y,df_expert_view_N)
df_expert_view_sum <- as.data.frame(df_expert_view_sum)

rownames(df_expert_view_sum) <- c("df_expert_Y","df_expert_N")

df_expert_view_sum

##
df_expert_Y <- df_expert_y_viewday %>%
  subset(expert_yn == "Y") %>%
  summarize(count = n(), median_viewday = median(views_per_day), Q1_viewday = quantile(views_per_day,0.25), Q3_viewday = quantile(views_per_day, 0.75), min = quantile(views_per_day,0),max = quantile(views_per_day,1))
df_expert_Y

df_expert_N <- df_expert_y_viewday %>%
  subset(expert_yn == "N") %>%
  summarize(count = n(), median_viewday = median(views_per_day), Q1_viewday = quantile(views_per_day,0.25), Q3_viewday = quantile(views_per_day, 0.75), min = quantile(views_per_day,0),max = quantile(views_per_day,1))
df_expert_N

df_expert_viewday_sum <- rbind(df_expert_Y,df_expert_N)
df_expert_viewday_sum <- as.data.frame(df_expert_viewday_sum)

rownames(df_expert_viewday_sum) <- c("df_expert_y","df_expert_n")
df_expert_viewday_sum

##
df_expert_Y <- df_expert_y_like %>%
  subset(expert_yn == "Y") %>%
  summarize(count = n(), median_like = median(like_count), Q1_like = quantile(like_count,0.25), Q3_like = quantile(like_count, 0.75), min = quantile(like_count,0),max = quantile(like_count,1))
df_expert_Y

df_expert_N <- df_expert_y_like %>%
  subset(expert_yn == "N") %>%
  summarize(count = n(), median_like = median(like_count), Q1_like = quantile(like_count,0.25), Q3_like = quantile(like_count, 0.75), min = quantile(like_count,0),max = quantile(like_count,1))
df_expert_N

df_expert_like_sum <- rbind(df_expert_Y,df_expert_N)
df_expert_like_sum <- as.data.frame(df_expert_like_sum)

rownames(df_expert_like_sum) <- c("df_expert_y","df_expert_n")
df_expert_like_sum

##
df_expert_Y <- df_expert_y_comment %>%
  subset(expert_yn == "Y") %>%
  summarize(count = n(), median_comment = median(comment_count), Q1_comment = quantile(comment_count,0.25), Q3_comment = quantile(comment_count, 0.75), min = quantile(comment_count,0),max = quantile(comment_count,1))
df_expert_Y

df_expert_N <- df_expert_y_comment %>%
  subset(expert_yn == "N") %>%
  summarize(count = n(), median_comment = median(comment_count), Q1_comment = quantile(comment_count,0.25), Q3_comment = quantile(comment_count, 0.75), min = quantile(comment_count,0),max = quantile(comment_count,1))
df_expert_N

df_expert_comment_sum <- rbind(df_expert_Y,df_expert_N)
df_expert_comment_sum <- as.data.frame(df_expert_comment_sum)

rownames(df_expert_comment_sum) <- c("df_expert_y","df_expert_n")
df_expert_comment_sum

##

df_expert_view_wilcox <- wilcox.test(view_count ~ expert_yn, data = df_expert_y_view)
df_expert_view_wilcox 

df_expert_viewday_wilcox <- wilcox.test(views_per_day ~ expert_yn, data = df_expert_y_viewday)
df_expert_viewday_wilcox 

df_expert_like_wilcox <- wilcox.test(like_count ~ expert_yn, data = df_expert_y_like)
df_expert_like_wilcox 

df_expert_comment_wilcox <- wilcox.test(comment_count ~ expert_yn, data = df_expert_y_comment)
df_expert_comment_wilcox 

# 5.23. Does the video have lay interviewees?

df_interviewee$lay_total_n <- as.character(df_interviewee$lay_total_n)
df_interviewee$lay_total_n 

df_interviewee$lay_yn <- ifelse(df_interviewee$lay_total_n == "NA","N",
                                ifelse(df_interviewee$lay_total_n == "0","N","Y"))
table(df_interviewee$lay_yn)

df_interviewee$lay_yn <- factor(df_interviewee$lay_yn, levels=c("Y", "N"))
table(df_interviewee$lay_yn)

df_lay_y_view <- df_interviewee %>%
  select(lay_yn,view_count)

df_lay_y_view <- df_lay_y_view[complete.cases(df_lay_y_view),]

df_lay_y_viewday <- df_interviewee %>%
  select(lay_yn,views_per_day)
df_lay_y_viewday <- df_lay_y_viewday[complete.cases(df_lay_y_viewday),]

df_lay_y_like <- df_interviewee %>%
  select(lay_yn,like_count)
df_lay_y_like <- df_lay_y_like[complete.cases(df_lay_y_like),]

df_lay_y_comment <- df_interviewee %>%
  select(lay_yn,comment_count)
df_lay_y_comment <- df_lay_y_comment[complete.cases(df_lay_y_comment),]

table(df_lay_y_comment$lay_yn)

##
df_lay_view_Y <- df_lay_y_view %>%
  subset(lay_yn == "Y") %>%
  summarize(count = n(), median_view = median(view_count), Q1_view = quantile(view_count,0.25), Q3_view = quantile(view_count, 0.75), min = quantile(view_count,0),max = quantile(view_count,1))
df_lay_view_Y

df_lay_view_N <- df_lay_y_view %>%
  subset(lay_yn == "N") %>%
  summarize(count = n(), median_view = median(view_count), Q1_view = quantile(view_count,0.25), Q3_view = quantile(view_count, 0.75), min = quantile(view_count,0),max = quantile(view_count,1))
df_lay_view_N

df_lay_view_sum <- rbind(df_lay_view_Y,df_lay_view_N)
df_lay_view_sum <- as.data.frame(df_lay_view_sum)

rownames(df_lay_view_sum) <- c("df_lay_Y","df_lay_N")

df_lay_view_sum

##
df_lay_Y <- df_lay_y_viewday %>%
  subset(lay_yn == "Y") %>%
  summarize(count = n(), median_viewday = median(views_per_day), Q1_viewday = quantile(views_per_day,0.25), Q3_viewday = quantile(views_per_day, 0.75), min = quantile(views_per_day,0),max = quantile(views_per_day,1))
df_lay_Y

df_lay_N <- df_lay_y_viewday %>%
  subset(lay_yn == "N") %>%
  summarize(count = n(), median_viewday = median(views_per_day), Q1_viewday = quantile(views_per_day,0.25), Q3_viewday = quantile(views_per_day, 0.75), min = quantile(views_per_day,0),max = quantile(views_per_day,1))
df_lay_N

df_lay_viewday_sum <- rbind(df_lay_Y,df_lay_N)
df_lay_viewday_sum <- as.data.frame(df_lay_viewday_sum)

rownames(df_lay_viewday_sum) <- c("df_lay_y","df_lay_n")
df_lay_viewday_sum

##
df_lay_Y <- df_lay_y_like %>%
  subset(lay_yn == "Y") %>%
  summarize(count = n(), median_like = median(like_count), Q1_like = quantile(like_count,0.25), Q3_like = quantile(like_count, 0.75), min = quantile(like_count,0),max = quantile(like_count,1))
df_lay_Y

df_lay_N <- df_lay_y_like %>%
  subset(lay_yn == "N") %>%
  summarize(count = n(), median_like = median(like_count), Q1_like = quantile(like_count,0.25), Q3_like = quantile(like_count, 0.75), min = quantile(like_count,0),max = quantile(like_count,1))
df_lay_N


df_lay_like_sum <- rbind(df_lay_Y,df_lay_N)
df_lay_like_sum <- as.data.frame(df_lay_like_sum)

rownames(df_lay_like_sum) <- c("df_lay_y","df_lay_n")
df_lay_like_sum

##
df_lay_Y <- df_lay_y_comment %>%
  subset(lay_yn == "Y") %>%
  summarize(count = n(), median_comment = median(comment_count), Q1_comment = quantile(comment_count,0.25), Q3_comment = quantile(comment_count, 0.75), min = quantile(comment_count,0),max = quantile(comment_count,1))
df_lay_Y

df_lay_N <- df_lay_y_comment %>%
  subset(lay_yn == "N") %>%
  summarize(count = n(), median_comment = median(comment_count), Q1_comment = quantile(comment_count,0.25), Q3_comment = quantile(comment_count, 0.75), min = quantile(comment_count,0),max = quantile(comment_count,1))
df_lay_N

df_lay_comment_sum <- rbind(df_lay_Y,df_lay_N)
df_lay_comment_sum <- as.data.frame(df_lay_comment_sum)

rownames(df_lay_comment_sum) <- c("df_lay_y","df_lay_n")
df_lay_comment_sum

##
df_lay_view_wilcox <- wilcox.test(view_count ~ lay_yn, data = df_lay_y_view)
df_lay_view_wilcox 

df_lay_viewday_wilcox <- wilcox.test(views_per_day ~ lay_yn, data = df_lay_y_viewday)
df_lay_viewday_wilcox 

df_lay_like_wilcox <- wilcox.test(like_count ~ lay_yn, data = df_lay_y_like)
df_lay_like_wilcox 

df_lay_comment_wilcox <- wilcox.test(comment_count ~ lay_yn, data = df_lay_y_comment)
df_lay_comment_wilcox 

##### combined ##########
people_view_summary <- rbind(presenter_accred_view_sum, narrator_gender_view_sum, df_interviewee_view_sum, df_expert_view_sum, df_lay_view_sum)
people_view_summary


people_viewday_summary <- rbind(presenter_accred_viewday_sum, narrator_gender_viewday_sum, df_interviewee_viewday_sum, df_expert_viewday_sum, df_lay_viewday_sum)
people_viewday_summary


people_like_summary <- rbind(presenter_accred_like_sum, narrator_gender_like_sum, df_interviewee_like_sum, df_expert_like_sum, df_lay_like_sum)
people_like_summary

people_comment_summary <- rbind(presenter_accred_comment_sum, narrator_gender_comment_sum, df_interviewee_comment_sum, df_expert_comment_sum, df_lay_comment_sum)
people_comment_summary


# 5.24. Encourage social interactions
table(df_final$encourage_yn)

encourage_yn_view <- df_final %>%
  select(encourage_yn,view_count) 
encourage_yn_view <- encourage_yn_view[complete.cases(encourage_yn_view ),]

table(encourage_yn_view$encourage_yn)

encourage_yn_view$encourage_yn<- factor(encourage_yn_view$encourage_yn, levels=c("Y", "N"))
table(encourage_yn_view$encourage_yn)

encourage_yn_viewday <- df_final %>%
  select(encourage_yn,views_per_day) 

encourage_yn_viewday <- encourage_yn_viewday[complete.cases(encourage_yn_viewday),]

encourage_yn_viewday$encourage_yn<- factor(encourage_yn_viewday$encourage_yn, levels=c("Y", "N"))
table(encourage_yn_viewday$encourage_yn)

encourage_yn_like <- df_final %>%
  select(encourage_yn,like_count)
encourage_yn_like <- encourage_yn_like[complete.cases(encourage_yn_like),]

encourage_yn_like$encourage_yn<- factor(encourage_yn_like$encourage_yn, levels=c("Y", "N"))
table(encourage_yn_like$encourage_yn)

encourage_yn_comment <- df_final %>%
  select(encourage_yn,comment_count)

encourage_yn_comment <- encourage_yn_comment[complete.cases(encourage_yn_comment),]

encourage_yn_comment$encourage_yn<- factor(encourage_yn_comment$encourage_yn, levels=c("Y", "N"))
table(encourage_yn_comment$encourage_yn)

##
encourage_yn_view_Y <- encourage_yn_view %>%
  subset(encourage_yn == "Y") %>%
  summarize(count = n(), median_view = median(view_count), Q1_view = quantile(view_count,0.25), Q3_view = quantile(view_count, 0.75), min = quantile(view_count,0),max = quantile(view_count,1))
encourage_yn_view_Y

encourage_yn_view_N <- encourage_yn_view %>%
  subset(encourage_yn == "N") %>%
  summarize(count = n(), median_view = median(view_count), Q1_view = quantile(view_count,0.25), Q3_view = quantile(view_count, 0.75), min = quantile(view_count,0),max = quantile(view_count,1))
encourage_yn_view_N


encourage_yn_view_sum <- rbind(encourage_yn_view_Y,encourage_yn_view_N)
encourage_yn_view_sum <- as.data.frame(encourage_yn_view_sum)

rownames(encourage_yn_view_sum) <- c("encourage_yn_Y","encourage_yn_N")

encourage_yn_view_sum

##
encourage_yn_Y <- encourage_yn_viewday %>%
  subset(encourage_yn == "Y") %>%
  summarize(count = n(), median_viewday = median(views_per_day), Q1_viewday = quantile(views_per_day,0.25), Q3_viewday = quantile(views_per_day, 0.75), min = quantile(views_per_day,0),max = quantile(views_per_day,1))
encourage_yn_Y

encourage_yn_N <- encourage_yn_viewday %>%
  subset(encourage_yn == "N") %>%
  summarize(count = n(), median_viewday = median(views_per_day), Q1_viewday = quantile(views_per_day,0.25), Q3_viewday = quantile(views_per_day, 0.75), min = quantile(views_per_day,0),max = quantile(views_per_day,1))
encourage_yn_N


encourage_yn_viewday_sum <- rbind(encourage_yn_Y,encourage_yn_N)
encourage_yn_viewday_sum <- as.data.frame(encourage_yn_viewday_sum)

rownames(encourage_yn_viewday_sum) <- c("encourage_yn_y","encourage_yn_n")
encourage_yn_viewday_sum

##
encourage_yn_Y <- encourage_yn_like %>%
  subset(encourage_yn == "Y") %>%
  summarize(count = n(), median_like = median(like_count), Q1_like = quantile(like_count,0.25), Q3_like = quantile(like_count, 0.75), min = quantile(like_count,0),max = quantile(like_count,1))
encourage_yn_Y

encourage_yn_N <- encourage_yn_like %>%
  subset(encourage_yn == "N") %>%
  summarize(count = n(), median_like = median(like_count), Q1_like = quantile(like_count,0.25), Q3_like = quantile(like_count, 0.75), min = quantile(like_count,0),max = quantile(like_count,1))
encourage_yn_N


encourage_yn_like_sum <- rbind(encourage_yn_Y,encourage_yn_N)
encourage_yn_like_sum <- as.data.frame(encourage_yn_like_sum)

rownames(encourage_yn_like_sum) <- c("encourage_yn_y","encourage_yn_n")
encourage_yn_like_sum

##
encourage_yn_Y <- encourage_yn_comment %>%
  subset(encourage_yn == "Y") %>%
  summarize(count = n(), median_comment = median(comment_count), Q1_comment = quantile(comment_count,0.25), Q3_comment = quantile(comment_count, 0.75), min = quantile(comment_count,0),max = quantile(comment_count,1))
encourage_yn_Y

encourage_yn_N <- encourage_yn_comment %>%
  subset(encourage_yn == "N") %>%
  summarize(count = n(), median_comment = median(comment_count), Q1_comment = quantile(comment_count,0.25), Q3_comment = quantile(comment_count, 0.75), min = quantile(comment_count,0),max = quantile(comment_count,1))
encourage_yn_N


encourage_yn_comment_sum <- rbind(encourage_yn_Y,encourage_yn_N)
encourage_yn_comment_sum <- as.data.frame(encourage_yn_comment_sum)

rownames(encourage_yn_comment_sum) <- c("encourage_yn_y","encourage_yn_n")
encourage_yn_comment_sum

##

encourage_yn_view_wilcox <- wilcox.test(view_count ~ encourage_yn, data = encourage_yn_view)
encourage_yn_view_wilcox 

encourage_yn_viewday_wilcox <- wilcox.test(views_per_day ~ encourage_yn, data = encourage_yn_viewday)
encourage_yn_viewday_wilcox 

encourage_yn_like_wilcox <- wilcox.test(like_count ~ encourage_yn, data = encourage_yn_like)
encourage_yn_like_wilcox 

encourage_yn_comment_wilcox <- wilcox.test(comment_count ~ encourage_yn, data = encourage_yn_comment)
encourage_yn_comment_wilcox 

# 5.25. Channel owner interactions_reply
table(df_final$reply_channel_owner)

reply_channel_owner_view <- df_final %>%
  select(reply_channel_owner,view_count) %>%
  filter(reply_channel_owner == "0"|reply_channel_owner == "1")

reply_channel_owner_view <- reply_channel_owner_view[complete.cases(reply_channel_owner_view ),]

table(reply_channel_owner_view$reply_channel_owner)

reply_channel_owner_view$reply_channel_owner <- ifelse(reply_channel_owner_view$reply_channel_owner == "1","Y","N")

reply_channel_owner_view$reply_channel_owner<- factor(reply_channel_owner_view$reply_channel_owner, levels=c("Y", "N"))
table(reply_channel_owner_view$reply_channel_owner)

reply_channel_owner_viewday <- df_final %>%
  select(reply_channel_owner,views_per_day) %>%
  filter(reply_channel_owner == "0"|reply_channel_owner == "1")

reply_channel_owner_viewday <- reply_channel_owner_viewday[complete.cases(reply_channel_owner_viewday),]

reply_channel_owner_viewday$reply_channel_owner <- ifelse(reply_channel_owner_viewday$reply_channel_owner == "1","Y","N")

reply_channel_owner_viewday$reply_channel_owner<- factor(reply_channel_owner_viewday$reply_channel_owner, levels=c("Y", "N"))
table(reply_channel_owner_viewday$reply_channel_owner)

reply_channel_owner_like <- df_final %>%
  select(reply_channel_owner,like_count) %>%
  filter(reply_channel_owner == "0"|reply_channel_owner == "1")

reply_channel_owner_like <- reply_channel_owner_like[complete.cases(reply_channel_owner_like),]

reply_channel_owner_like$reply_channel_owner <- ifelse(reply_channel_owner_like$reply_channel_owner == "1","Y","N")

reply_channel_owner_like$reply_channel_owner<- factor(reply_channel_owner_like$reply_channel_owner, levels=c("Y", "N"))
table(reply_channel_owner_like$reply_channel_owner)

reply_channel_owner_comment <- df_final %>%
  select(reply_channel_owner,comment_count) %>%
  filter(reply_channel_owner == "0"|reply_channel_owner == "1")

reply_channel_owner_comment <- reply_channel_owner_comment[complete.cases(reply_channel_owner_comment),]

reply_channel_owner_comment$reply_channel_owner <- ifelse(reply_channel_owner_comment$reply_channel_owner == "1","Y","N")

reply_channel_owner_comment$reply_channel_owner<- factor(reply_channel_owner_comment$reply_channel_owner, levels=c("Y", "N"))
table(reply_channel_owner_comment$reply_channel_owner)

##
reply_channel_owner_view_Y <- reply_channel_owner_view %>%
  subset(reply_channel_owner == "Y") %>%
  summarize(count = n(), median_view = median(view_count), Q1_view = quantile(view_count,0.25), Q3_view = quantile(view_count, 0.75), min = quantile(view_count,0),max = quantile(view_count,1))
reply_channel_owner_view_Y

reply_channel_owner_view_N <- reply_channel_owner_view %>%
  subset(reply_channel_owner == "N") %>%
  summarize(count = n(), median_view = median(view_count), Q1_view = quantile(view_count,0.25), Q3_view = quantile(view_count, 0.75), min = quantile(view_count,0),max = quantile(view_count,1))
reply_channel_owner_view_N


reply_channel_owner_view_sum <- rbind(reply_channel_owner_view_Y,reply_channel_owner_view_N)
reply_channel_owner_view_sum <- as.data.frame(reply_channel_owner_view_sum)

rownames(reply_channel_owner_view_sum) <- c("reply_channel_owner_Y","reply_channel_owner_N")

reply_channel_owner_view_sum

##
reply_channel_owner_viewday_Y <- reply_channel_owner_viewday %>%
  subset(reply_channel_owner == "Y") %>%
  summarize(count = n(), median_viewday = median(views_per_day), Q1_viewday = quantile(views_per_day,0.25), Q3_viewday = quantile(views_per_day, 0.75), min = quantile(views_per_day,0),max = quantile(views_per_day,1))
reply_channel_owner_viewday_Y

reply_channel_owner_viewday_N <- reply_channel_owner_viewday %>%
  subset(reply_channel_owner == "N") %>%
  summarize(count = n(), median_viewday = median(views_per_day), Q1_viewday = quantile(views_per_day,0.25), Q3_viewday = quantile(views_per_day, 0.75), min = quantile(views_per_day,0),max = quantile(views_per_day,1))
reply_channel_owner_viewday_N


reply_channel_owner_viewday_sum <- rbind(reply_channel_owner_viewday_Y,reply_channel_owner_viewday_N)
reply_channel_owner_viewday_sum <- as.data.frame(reply_channel_owner_viewday_sum)

rownames(reply_channel_owner_viewday_sum) <- c("reply_channel_owner_y","reply_channel_owner_n")
reply_channel_owner_viewday_sum

##
reply_channel_owner_like_Y <- reply_channel_owner_like %>%
  subset(reply_channel_owner == "Y") %>%
  summarize(count = n(), median_like = median(like_count), Q1_like = quantile(like_count,0.25), Q3_like = quantile(like_count, 0.75), min = quantile(like_count,0),max = quantile(like_count,1))
reply_channel_owner_like_Y

reply_channel_owner_like_N <- reply_channel_owner_like %>%
  subset(reply_channel_owner == "N") %>%
  summarize(count = n(), median_like = median(like_count), Q1_like = quantile(like_count,0.25), Q3_like = quantile(like_count, 0.75), min = quantile(like_count,0),max = quantile(like_count,1))
reply_channel_owner_like_N


reply_channel_owner_like_sum <- rbind(reply_channel_owner_like_Y,reply_channel_owner_like_N)
reply_channel_owner_like_sum <- as.data.frame(reply_channel_owner_like_sum)

rownames(reply_channel_owner_like_sum) <- c("reply_channel_owner_y","reply_channel_owner_n")
reply_channel_owner_like_sum

##
reply_channel_owner_comment_Y <- reply_channel_owner_comment %>%
  subset(reply_channel_owner == "Y") %>%
  summarize(count = n(), median_comment = median(comment_count), Q1_comment = quantile(comment_count,0.25), Q3_comment = quantile(comment_count, 0.75), min = quantile(comment_count,0),max = quantile(comment_count,1))
reply_channel_owner_comment_Y

reply_channel_owner_comment_N <- reply_channel_owner_comment %>%
  subset(reply_channel_owner == "N") %>%
  summarize(count = n(), median_comment = median(comment_count), Q1_comment = quantile(comment_count,0.25), Q3_comment = quantile(comment_count, 0.75), min = quantile(comment_count,0),max = quantile(comment_count,1))
reply_channel_owner_comment_N


reply_channel_owner_comment_sum <- rbind(reply_channel_owner_comment_Y,reply_channel_owner_comment_N)
reply_channel_owner_comment_sum <- as.data.frame(reply_channel_owner_comment_sum)

rownames(reply_channel_owner_comment_sum) <- c("reply_channel_owner_y","reply_channel_owner_n")
reply_channel_owner_comment_sum

reply_channel_owner_view_wilcox <- wilcox.test(view_count ~ reply_channel_owner, data = reply_channel_owner_view)
reply_channel_owner_view_wilcox 

reply_channel_owner_viewday_wilcox <- wilcox.test(views_per_day ~ reply_channel_owner, data = reply_channel_owner_viewday)
reply_channel_owner_viewday_wilcox 

reply_channel_owner_like_wilcox <- wilcox.test(like_count ~ reply_channel_owner, data = reply_channel_owner_like)
reply_channel_owner_like_wilcox 

reply_channel_owner_comment_wilcox <- wilcox.test(comment_count ~ reply_channel_owner, data = reply_channel_owner_comment)
reply_channel_owner_comment_wilcox 


# 5.26. Channel ownder interaction_like ##########
table(df_final$like_channel_owner)

like_channel_owner_view <- df_final %>%
  select(like_channel_owner,view_count) %>%
  filter(like_channel_owner == "0"|like_channel_owner == "1")

like_channel_owner_view <- like_channel_owner_view[complete.cases(like_channel_owner_view ),]

table(like_channel_owner_view$like_channel_owner)

like_channel_owner_view$like_channel_owner <- ifelse(like_channel_owner_view$like_channel_owner == "1","Y","N")

like_channel_owner_view$like_channel_owner<- factor(like_channel_owner_view$like_channel_owner, levels=c("Y", "N"))
table(like_channel_owner_view$like_channel_owner)

like_channel_owner_viewday <- df_final %>%
  select(like_channel_owner,views_per_day) %>%
  filter(like_channel_owner == "0"|like_channel_owner == "1")

like_channel_owner_viewday <- like_channel_owner_viewday[complete.cases(like_channel_owner_viewday),]

like_channel_owner_viewday$like_channel_owner <- ifelse(like_channel_owner_viewday$like_channel_owner == "1","Y","N")

like_channel_owner_viewday$like_channel_owner<- factor(like_channel_owner_viewday$like_channel_owner, levels=c("Y", "N"))
table(like_channel_owner_viewday$like_channel_owner)

like_channel_owner_like <- df_final %>%
  select(like_channel_owner,like_count) %>%
  filter(like_channel_owner == "0"|like_channel_owner == "1")

like_channel_owner_like <- like_channel_owner_like[complete.cases(like_channel_owner_like),]

like_channel_owner_like$like_channel_owner <- ifelse(like_channel_owner_like$like_channel_owner == "1","Y","N")

like_channel_owner_like$like_channel_owner<- factor(like_channel_owner_like$like_channel_owner, levels=c("Y", "N"))
table(like_channel_owner_like$like_channel_owner)

like_channel_owner_comment <- df_final %>%
  select(like_channel_owner,comment_count) %>%
  filter(like_channel_owner == "0"|like_channel_owner == "1")

like_channel_owner_comment <- like_channel_owner_comment[complete.cases(like_channel_owner_comment),]

like_channel_owner_comment$like_channel_owner <- ifelse(like_channel_owner_comment$like_channel_owner == "1","Y","N")

like_channel_owner_comment$like_channel_owner<- factor(like_channel_owner_comment$like_channel_owner, levels=c("Y", "N"))
table(like_channel_owner_comment$like_channel_owner)

##
like_channel_owner_view_Y <- like_channel_owner_view %>%
  subset(like_channel_owner == "Y") %>%
  summarize(count = n(), median_view = median(view_count), Q1_view = quantile(view_count,0.25), Q3_view = quantile(view_count, 0.75), min = quantile(view_count,0),max = quantile(view_count,1))
like_channel_owner_view_Y

like_channel_owner_view_N <- like_channel_owner_view %>%
  subset(like_channel_owner == "N") %>%
  summarize(count = n(), median_view = median(view_count), Q1_view = quantile(view_count,0.25), Q3_view = quantile(view_count, 0.75), min = quantile(view_count,0),max = quantile(view_count,1))
like_channel_owner_view_N

like_channel_owner_view_sum <- rbind(like_channel_owner_view_Y,like_channel_owner_view_N)
like_channel_owner_view_sum <- as.data.frame(like_channel_owner_view_sum)

rownames(like_channel_owner_view_sum) <- c("like_channel_owner_Y","like_channel_owner_N")

like_channel_owner_view_sum

##
like_channel_owner_viewday_Y <- like_channel_owner_viewday %>%
  subset(like_channel_owner == "Y") %>%
  summarize(count = n(), median_viewday = median(views_per_day), Q1_viewday = quantile(views_per_day,0.25), Q3_viewday = quantile(views_per_day, 0.75), min = quantile(views_per_day,0),max = quantile(views_per_day,1))
like_channel_owner_viewday_Y

like_channel_owner_viewday_N <- like_channel_owner_viewday %>%
  subset(like_channel_owner == "N") %>%
  summarize(count = n(), median_viewday = median(views_per_day), Q1_viewday = quantile(views_per_day,0.25), Q3_viewday = quantile(views_per_day, 0.75), min = quantile(views_per_day,0),max = quantile(views_per_day,1))
like_channel_owner_viewday_N


like_channel_owner_viewday_sum <- rbind(like_channel_owner_viewday_Y,like_channel_owner_viewday_N)
like_channel_owner_viewday_sum <- as.data.frame(like_channel_owner_viewday_sum)

rownames(like_channel_owner_viewday_sum) <- c("like_channel_owner_y","like_channel_owner_n")
like_channel_owner_viewday_sum

##
like_channel_owner_like_Y <- like_channel_owner_like %>%
  subset(like_channel_owner == "Y") %>%
  summarize(count = n(), median_like = median(like_count), Q1_like = quantile(like_count,0.25), Q3_like = quantile(like_count, 0.75), min = quantile(like_count,0),max = quantile(like_count,1))
like_channel_owner_like_Y

like_channel_owner_like_N <- like_channel_owner_like %>%
  subset(like_channel_owner == "N") %>%
  summarize(count = n(), median_like = median(like_count), Q1_like = quantile(like_count,0.25), Q3_like = quantile(like_count, 0.75), min = quantile(like_count,0),max = quantile(like_count,1))
like_channel_owner_like_N


like_channel_owner_like_sum <- rbind(like_channel_owner_like_Y,like_channel_owner_like_N)
like_channel_owner_like_sum <- as.data.frame(like_channel_owner_like_sum)

rownames(like_channel_owner_like_sum) <- c("like_channel_owner_y","like_channel_owner_n")
like_channel_owner_like_sum

##
like_channel_owner_comment_Y <- like_channel_owner_comment %>%
  subset(like_channel_owner == "Y") %>%
  summarize(count = n(), median_comment = median(comment_count), Q1_comment = quantile(comment_count,0.25), Q3_comment = quantile(comment_count, 0.75), min = quantile(comment_count,0),max = quantile(comment_count,1))
like_channel_owner_comment_Y

like_channel_owner_comment_N <- like_channel_owner_comment %>%
  subset(like_channel_owner == "N") %>%
  summarize(count = n(), median_comment = median(comment_count), Q1_comment = quantile(comment_count,0.25), Q3_comment = quantile(comment_count, 0.75), min = quantile(comment_count,0),max = quantile(comment_count,1))
like_channel_owner_comment_N


like_channel_owner_comment_sum <- rbind(like_channel_owner_comment_Y,like_channel_owner_comment_N)
like_channel_owner_comment_sum <- as.data.frame(like_channel_owner_comment_sum)

rownames(like_channel_owner_comment_sum) <- c("like_channel_owner_y","like_channel_owner_n")
like_channel_owner_comment_sum

##

like_channel_owner_view_wilcox <- wilcox.test(view_count ~ like_channel_owner, data = like_channel_owner_view)
like_channel_owner_view_wilcox 

like_channel_owner_viewday_wilcox <- wilcox.test(views_per_day ~ like_channel_owner, data = like_channel_owner_viewday)
like_channel_owner_viewday_wilcox 

like_channel_owner_like_wilcox <- wilcox.test(like_count ~ like_channel_owner, data = like_channel_owner_like)
like_channel_owner_like_wilcox 

like_channel_owner_comment_wilcox <- wilcox.test(comment_count ~ like_channel_owner, data = like_channel_owner_comment)
like_channel_owner_comment_wilcox 

##combined encourage##

encourage_yn_combine/encourage_reply_combine/encourage_like_combine

encourage_view_summary <- rbind(encourage_yn_view_sum,reply_channel_owner_view_sum,like_channel_owner_view_sum)
encourage_view_summary

encourage_viewday_summary <- rbind(encourage_yn_viewday_sum,reply_channel_owner_viewday_sum,like_channel_owner_viewday_sum)
encourage_viewday_summary

encourage_like_summary <- rbind(encourage_yn_like_sum,reply_channel_owner_like_sum,like_channel_owner_like_sum)
encourage_like_summary

encourage_comment_summary <- rbind(encourage_yn_comment_sum,reply_channel_owner_comment_sum,like_channel_owner_comment_sum)
encourage_comment_summary


# 5.27. Visual quality
table(df_final$visual_quality)

df_final$visual_quality_binary <- ifelse(df_final$visual_quality == "1080p","1080p or above",
                                         ifelse(df_final$visual_quality == "1080p HD","1080p or above",
                                                ifelse(df_final$visual_quality == "1080p HD Premium","1080p or above",
                                                       ifelse(df_final$visual_quality == "1080p60HD","1080p or above",
                                                              ifelse(df_final$visual_quality == "2160p","1080p or above","Below 1080p")))))


table(df_final$visual_quality_binary)


visual_quality_view <- df_create_view(visual_quality_binary)
visual_quality_viewday <- df_create_viewday(visual_quality_binary)
visual_quality_like <- df_create_like(visual_quality_binary)
visual_quality_comment <- df_create_comment(visual_quality_binary)

##
visual_quality_view_above <- visual_quality_view %>%
  subset(visual_quality_binary == "1080p or above") %>%
  summarize(count = n(), median_view = median(view_count), Q1_view = quantile(view_count,0.25), Q3_view = quantile(view_count, 0.75), min = quantile(view_count,0),max = quantile(view_count,1))
visual_quality_view_above
visual_quality_view_below <- visual_quality_view %>%
  subset(visual_quality_binary == "Below 1080p") %>%
  summarize(count = n(), median_view = median(view_count), Q1_view = quantile(view_count,0.25), Q3_view = quantile(view_count, 0.75), min = quantile(view_count,0),max = quantile(view_count,1))
visual_quality_view_below

visual_quality_view_sum <- rbind(visual_quality_view_above,visual_quality_view_below)
visual_quality_view_sum <- as.data.frame(visual_quality_view_sum)

rownames(visual_quality_view_sum) <- c("visual_quality_above1080","visual_quality_below1080")

visual_quality_view_sum

##
visual_quality_viewday_above <- visual_quality_viewday %>%
  subset(visual_quality_binary == "1080p or above") %>%
  summarize(count = n(), median_viewday = median(views_per_day), Q1_viewday = quantile(views_per_day,0.25), Q3_viewday = quantile(views_per_day, 0.75), min = quantile(views_per_day,0),max = quantile(views_per_day,1))
visual_quality_viewday_above
visual_quality_viewday_below <- visual_quality_viewday %>%
  subset(visual_quality_binary == "Below 1080p") %>%
  summarize(count = n(), median_viewday = median(views_per_day), Q1_viewday = quantile(views_per_day,0.25), Q3_viewday = quantile(views_per_day, 0.75), min = quantile(views_per_day,0),max = quantile(views_per_day,1))
visual_quality_viewday_below

visual_quality_viewday_sum <- rbind(visual_quality_viewday_above,visual_quality_viewday_below)
visual_quality_viewday_sum <- as.data.frame(visual_quality_viewday_sum)

rownames(visual_quality_viewday_sum) <- c("visual_quality_above1080","visual_quality_below1080")

visual_quality_viewday_sum
##
visual_quality_like_above <- visual_quality_like %>%
  subset(visual_quality_binary == "1080p or above") %>%
  summarize(count = n(), median_like = median(like_count), Q1_like = quantile(like_count,0.25), Q3_like = quantile(like_count, 0.75), min = quantile(like_count,0),max = quantile(like_count,1))
visual_quality_like_above
visual_quality_like_below <- visual_quality_like %>%
  subset(visual_quality_binary == "Below 1080p") %>%
  summarize(count = n(), median_like = median(like_count), Q1_like = quantile(like_count,0.25), Q3_like = quantile(like_count, 0.75), min = quantile(like_count,0),max = quantile(like_count,1))
visual_quality_like_below

visual_quality_like_sum <- rbind(visual_quality_like_above,visual_quality_like_below)
visual_quality_like_sum <- as.data.frame(visual_quality_like_sum)

rownames(visual_quality_like_sum) <- c("visual_quality_above1080","visual_quality_below1080")

visual_quality_like_sum

##
visual_quality_comment_above <- visual_quality_comment %>%
  subset(visual_quality_binary == "1080p or above") %>%
  summarize(count = n(), median_comment = median(comment_count), Q1_comment = quantile(comment_count,0.25), Q3_comment = quantile(comment_count, 0.75), min = quantile(comment_count,0),max = quantile(comment_count,1))
visual_quality_comment_above
visual_quality_comment_below <- visual_quality_comment %>%
  subset(visual_quality_binary == "Below 1080p") %>%
  summarize(count = n(), median_comment = median(comment_count), Q1_comment = quantile(comment_count,0.25), Q3_comment = quantile(comment_count, 0.75), min = quantile(comment_count,0),max = quantile(comment_count,1))
visual_quality_comment_below


visual_quality_comment_sum <- rbind(visual_quality_comment_above,visual_quality_comment_below)
visual_quality_comment_sum <- as.data.frame(visual_quality_comment_sum)

rownames(visual_quality_comment_sum) <- c("visual_quality_above1080","visual_quality_below1080")

visual_quality_comment_sum

#
visual_quality_view_wilcox <- wilcox.test(view_count ~ visual_quality_binary, data = visual_quality_view)
visual_quality_view_wilcox 

visual_quality_viewday_wilcox <- wilcox.test(views_per_day ~ visual_quality_binary, data = visual_quality_viewday)
visual_quality_viewday_wilcox 

visual_quality_like_wilcox <- wilcox.test(like_count ~ visual_quality_binary, data = visual_quality_like)
visual_quality_like_wilcox 

visual_quality_comment_wilcox <- wilcox.test(comment_count ~ visual_quality_binary, data = visual_quality_comment)
visual_quality_comment_wilcox 

# 5.28. Humour
table(df_final$humor_yn)

df_final$humor_yn <- as.character(df_final$humor_yn)

df_final$humor_yn <- ifelse(df_final$humor_yn == "1","Y","N")
table(df_final$humor_yn)

df_final$humor_yn <- factor(df_final$humor_yn, levels=c("Y", "N"))

humor_view <- df_create_view(humor_yn)
humor_viewday <- df_create_viewday(humor_yn)
humor_like <- df_create_like(humor_yn)
humor_comment <- df_create_comment(humor_yn)

##
humor_view_Y <- humor_view %>%
  subset(humor_yn == "Y") %>%
  summarize(count = n(), median_view = median(view_count), Q1_view = quantile(view_count,0.25), Q3_view = quantile(view_count, 0.75), min = quantile(view_count,0),max = quantile(view_count,1))
humor_view_Y

humor_view_N <- humor_view %>%
  subset(humor_yn == "N") %>%
  summarize(count = n(), median_view = median(view_count), Q1_view = quantile(view_count,0.25), Q3_view = quantile(view_count, 0.75), min = quantile(view_count,0),max = quantile(view_count,1))
humor_view_N

humor_view_sum <- rbind(humor_view_Y,humor_view_N)
humor_view_sum <- as.data.frame(humor_view_sum)

rownames(humor_view_sum) <- c("humor_Y","humor_N")
humor_view_sum

##
humor_viewday_Y <- humor_viewday %>%
  subset(humor_yn == "Y") %>%
  summarize(count = n(), median_viewday = median(views_per_day), Q1_viewday = quantile(views_per_day,0.25), Q3_viewday = quantile(views_per_day, 0.75), min = quantile(views_per_day,0),max = quantile(views_per_day,1))
humor_viewday_Y

humor_viewday_N <- humor_viewday %>%
  subset(humor_yn == "N") %>%
  summarize(count = n(), median_viewday = median(views_per_day), Q1_viewday = quantile(views_per_day,0.25), Q3_viewday = quantile(views_per_day, 0.75), min = quantile(views_per_day,0),max = quantile(views_per_day,1))
humor_viewday_N

humor_viewday_sum <- rbind(humor_viewday_Y,humor_viewday_N)
humor_viewday_sum <- as.data.frame(humor_viewday_sum)

rownames(humor_viewday_sum) <- c("humor_y","humor_n")
humor_viewday_sum

##
humor_like_Y <- humor_like %>%
  subset(humor_yn == "Y") %>%
  summarize(count = n(), median_like = median(like_count), Q1_like = quantile(like_count,0.25), Q3_like = quantile(like_count, 0.75), min = quantile(like_count,0),max = quantile(like_count,1))
humor_like_Y

humor_like_N <- humor_like %>%
  subset(humor_yn == "N") %>%
  summarize(count = n(), median_like = median(like_count), Q1_like = quantile(like_count,0.25), Q3_like = quantile(like_count, 0.75), min = quantile(like_count,0),max = quantile(like_count,1))
humor_like_N

humor_like_sum <- rbind(humor_like_Y,humor_like_N)
humor_like_sum <- as.data.frame(humor_like_sum)

rownames(humor_like_sum) <- c("humor_y","humor_n")
humor_like_sum
##
humor_comment_Y <- humor_comment %>%
  subset(humor_yn == "Y") %>%
  summarize(count = n(), median_comment = median(comment_count), Q1_comment = quantile(comment_count,0.25), Q3_comment = quantile(comment_count, 0.75), min = quantile(comment_count,0),max = quantile(comment_count,1))
humor_comment_Y

humor_comment_N <- humor_comment %>%
  subset(humor_yn == "N") %>%
  summarize(count = n(), median_comment = median(comment_count), Q1_comment = quantile(comment_count,0.25), Q3_comment = quantile(comment_count, 0.75), min = quantile(comment_count,0),max = quantile(comment_count,1))
humor_comment_N

humor_comment_sum <- rbind(humor_comment_Y,humor_comment_N)
humor_comment_sum <- as.data.frame(humor_comment_sum)

rownames(humor_comment_sum) <- c("humor_y","humor_n")
humor_comment_sum
##
humor_view_wilcox <- wilcox.test(view_count ~ humor_yn, data = humor_view)
humor_view_wilcox 

humor_viewday_wilcox <- wilcox.test(views_per_day ~ humor_yn, data = humor_viewday)
humor_viewday_wilcox 

humor_like_wilcox <- wilcox.test(like_count ~ humor_yn, data = humor_like)
humor_like_wilcox 

humor_comment_wilcox <- wilcox.test(comment_count ~ humor_yn, data = humor_comment)
humor_comment_wilcox 


# 5.29. YouTube Health

table(df_final$youtube_health_yn)

df_final$youtube_health_yn <- ifelse(df_final$youtube_health_yn == "T","Y","N")
table(df_final$youtube_health_yn)

df_final$yt_health_yn <- factor(df_final$youtube_health_yn, levels=c("Y", "N"))

youtube_health_view <- df_create_view(yt_health_yn)
table(youtube_health_view$yt_health_yn)

youtube_health_viewday <- df_create_viewday(yt_health_yn)

youtube_health_like <- df_create_like(yt_health_yn)

youtube_health_comment <- df_create_comment(yt_health_yn)

youtube_health_comment$comment_count <- as.numeric(youtube_health_comment$comment_count)

##
youtube_health_view_Y <- youtube_health_view %>%
  subset(yt_health_yn == "Y") %>%
  summarize(count = n(), median_view = median(view_count), Q1_view = quantile(view_count,0.25), Q3_view = quantile(view_count, 0.75), min = quantile(view_count,0),max = quantile(view_count,1))
youtube_health_view_Y

youtube_health_view_N <- youtube_health_view %>%
  subset(yt_health_yn == "N") %>%
  summarize(count = n(), median_view = median(view_count), Q1_view = quantile(view_count,0.25), Q3_view = quantile(view_count, 0.75), min = quantile(view_count,0),max = quantile(view_count,1))
youtube_health_view_N

youtube_health_view_sum <- rbind(youtube_health_view_Y,youtube_health_view_N)
youtube_health_view_sum <- as.data.frame(youtube_health_view_sum)

rownames(youtube_health_view_sum) <- c("youtube_health_Y","youtube_health_N")
youtube_health_view_sum

##
youtube_health_viewday_Y <- youtube_health_viewday %>%
  subset(yt_health_yn == "Y") %>%
  summarize(count = n(), median_viewday = median(views_per_day), Q1_viewday = quantile(views_per_day,0.25), Q3_viewday = quantile(views_per_day, 0.75), min = quantile(views_per_day,0),max = quantile(views_per_day,1))
youtube_health_viewday_Y

youtube_health_viewday_N <- youtube_health_viewday %>%
  subset(yt_health_yn == "N") %>%
  summarize(count = n(), median_viewday = median(views_per_day), Q1_viewday = quantile(views_per_day,0.25), Q3_viewday = quantile(views_per_day, 0.75), min = quantile(views_per_day,0),max = quantile(views_per_day,1))
youtube_health_viewday_N

youtube_health_viewday_sum <- rbind(youtube_health_viewday_Y,youtube_health_viewday_N)
youtube_health_viewday_sum <- as.data.frame(youtube_health_viewday_sum)

rownames(youtube_health_viewday_sum) <- c("youtube_health_y","youtube_health_n")
youtube_health_viewday_sum

##
youtube_health_like_Y <- youtube_health_like %>%
  subset(yt_health_yn == "Y") %>%
  summarize(count = n(), median_like = median(like_count), Q1_like = quantile(like_count,0.25), Q3_like = quantile(like_count, 0.75), min = quantile(like_count,0),max = quantile(like_count,1))
youtube_health_like_Y

youtube_health_like_N <- youtube_health_like %>%
  subset(yt_health_yn == "N") %>%
  summarize(count = n(), median_like = median(like_count), Q1_like = quantile(like_count,0.25), Q3_like = quantile(like_count, 0.75), min = quantile(like_count,0),max = quantile(like_count,1))
youtube_health_like_N

youtube_health_like_sum <- rbind(youtube_health_like_Y,youtube_health_like_N)
youtube_health_like_sum <- as.data.frame(youtube_health_like_sum)

rownames(youtube_health_like_sum) <- c("youtube_health_y","youtube_health_n")
youtube_health_like_sum

##
youtube_health_comment_Y <- youtube_health_comment %>%
  subset(yt_health_yn == "Y") %>%
  summarize(count = n(), median_comment = median(comment_count), Q1_comment = quantile(comment_count,0.25), Q3_comment = quantile(comment_count, 0.75), min = quantile(comment_count,0),max = quantile(comment_count,1))
youtube_health_comment_Y

youtube_health_comment_N <- youtube_health_comment %>%
  subset(yt_health_yn == "N") %>%
  summarize(count = n(), median_comment = median(comment_count), Q1_comment = quantile(comment_count,0.25), Q3_comment = quantile(comment_count, 0.75), min = quantile(comment_count,0),max = quantile(comment_count,1))
youtube_health_comment_N


youtube_health_comment_sum <- rbind(youtube_health_comment_Y,youtube_health_comment_N)
youtube_health_comment_sum <- as.data.frame(youtube_health_comment_sum)

rownames(youtube_health_comment_sum) <- c("youtube_health_y","youtube_health_n")
youtube_health_comment_sum

##
youtube_health_view_wilcox <- wilcox.test(view_count ~ yt_health_yn, data = youtube_health_view)
youtube_health_view_wilcox 

youtube_health_viewday_wilcox <- wilcox.test(views_per_day ~ yt_health_yn, data = youtube_health_viewday)
youtube_health_viewday_wilcox 

youtube_health_like_wilcox <- wilcox.test(like_count ~ yt_health_yn, data = youtube_health_like)
youtube_health_like_wilcox 

youtube_health_comment_wilcox <- wilcox.test(comment_count ~ yt_health_yn, data = youtube_health_comment)
youtube_health_comment_wilcox 

##Combine others##
combine_others_view_summary <- rbind(visual_quality_view_sum,humor_view_sum,youtube_health_view_sum)
combine_others_view_summary

combine_others_viewday_summary <- rbind(visual_quality_viewday_sum,humor_viewday_sum,youtube_health_viewday_sum)
combine_others_viewday_summary

combine_others_like_summary <- rbind(visual_quality_like_sum,humor_like_sum,youtube_health_like_sum)
combine_others_like_summary


combine_others_comment_summary <- rbind(visual_quality_comment_sum,humor_comment_sum,youtube_health_comment_sum)
combine_others_comment_summary

# 5. Collated table
combined_view <- rbind(thumbnail_view_summary,intro_view_summary, music_view2_summary,animation_view_summary, people_view_summary, encourage_view_summary,combine_others_view_summary)
combined_view
combined_viewday <- rbind(thumbnail_viewday_summary,intro_viewday_summary, music_viewday2_summary,animation_viewday_summary, people_viewday_summary, encourage_viewday_summary,combine_others_viewday_summary)
combined_viewday

combined_like <- rbind(thumbnail_like_summary,intro_like_summary, music_like2_summary,animation_like_summary, people_like_summary, encourage_like_summary,combine_others_like_summary)
combined_like

combined_comment <- rbind(thumbnail_comment_summary,intro_comment_summary, music_comment2_summary,animation_comment_summary, people_comment_summary, encourage_comment_summary,combine_others_comment_summary)
combined_comment

write.csv(combined_view,"", row.names=FALSE)
write.csv(combined_viewday,"", row.names=FALSE)
write.csv(combined_like,"", row.names=FALSE)
write.csv(combined_comment,"", row.names=FALSE)

# 6.0 AMR Topics
glimpse(df_topic)

df_topic$like_count <- as.numeric(df_topic$like_count)

df_topic$comment_count <- as.numeric(df_topic$comment_count)

# 6.1. Topic 1
table(df_topic$Topic1_final)

Topic1_view <- df_topic %>%
  select(Topic1_final,view_count) 
Topic1_view <- Topic1_view[complete.cases(Topic1_view ),]

table(Topic1_view$Topic1_final)

Topic1_view$Topic1_final<- factor(Topic1_view$Topic1_final, levels=c("Y", "N"))
table(Topic1_view$Topic1_final)

Topic1_viewday <- df_topic %>%
  select(Topic1_final,views_per_day) 

Topic1_viewday <- Topic1_viewday[complete.cases(Topic1_viewday),]

Topic1_viewday$Topic1_final<- factor(Topic1_viewday$Topic1_final, levels=c("Y", "N"))
table(Topic1_viewday$Topic1_final)

Topic1_like <- df_topic %>%
  select(Topic1_final,like_count) 
Topic1_like <- Topic1_like[complete.cases(Topic1_like),]

Topic1_like$Topic1_final<- factor(Topic1_like$Topic1_final, levels=c("Y", "N"))
table(Topic1_like$Topic1_final)

Topic1_comment <- df_topic %>%
  select(Topic1_final,comment_count) 

Topic1_comment <- Topic1_comment[complete.cases(Topic1_comment),]

Topic1_comment$Topic1_final<- factor(Topic1_comment$Topic1_final, levels=c("Y", "N"))
table(Topic1_comment$Topic1_final)

Topic1_view_wilcox <- wilcox.test(view_count ~ Topic1_final, data = Topic1_view)
Topic1_view_wilcox 

Topic1_viewday_wilcox <- wilcox.test(views_per_day ~ Topic1_final, data = Topic1_viewday)
Topic1_viewday_wilcox 

Topic1_like_wilcox <- wilcox.test(like_count ~ Topic1_final, data = Topic1_like)
Topic1_like_wilcox 

Topic1_comment_wilcox <- wilcox.test(comment_count ~ Topic1_final, data = Topic1_comment)
Topic1_comment_wilcox 

#view#
Topic1_view_Y <- Topic1_view %>%
  subset(Topic1_final == "Y") %>%
  summarize(count = n(), median_view = median(view_count), Q1_view = quantile(view_count,0.25), Q3_view = quantile(view_count, 0.75), min = quantile(view_count,0),max = quantile(view_count,1))
Topic1_view_Y

Topic1_view_N <- Topic1_view%>%
  subset(Topic1_final == "N") %>%
  summarize(count = n(), median_view = median(view_count), Q1_view = quantile(view_count,0.25), Q3_view = quantile(view_count, 0.75), min = quantile(view_count,0),max = quantile(view_count,1))
Topic1_view_N

Topic1_view_sum <- rbind(Topic1_view_Y,Topic1_view_N)
Topic1_view_sum <- as.data.frame(Topic1_view_sum)

rownames(Topic1_view_sum) <- c("Topic1_view_y","Topic1_view_N")
Topic1_view_sum

#views per day#
Topic1_viewday_Y <- Topic1_viewday %>%
  subset(Topic1_final == "Y") %>%
  summarize(count = n(), median_viewday = median(views_per_day), Q1_viewday = quantile(views_per_day,0.25), Q3_viewday = quantile(views_per_day, 0.75), min = quantile(views_per_day,0),max = quantile(views_per_day,1))
Topic1_viewday_Y

Topic1_viewday_N <- Topic1_viewday%>%
  subset(Topic1_final == "N") %>%
  summarize(count = n(), median_viewday = median(views_per_day), Q1_viewday = quantile(views_per_day,0.25), Q3_viewday = quantile(views_per_day, 0.75), min = quantile(views_per_day,0),max = quantile(views_per_day,1))
Topic1_viewday_N

Topic1_viewday_sum <- rbind(Topic1_viewday_Y,Topic1_viewday_N)
Topic1_viewday_sum <- as.data.frame(Topic1_viewday_sum)

rownames(Topic1_viewday_sum) <- c("Topic1_viewday_y","Topic1_viewday_N")
Topic1_viewday_sum

#like#
Topic1_like_Y <- Topic1_like %>%
  subset(Topic1_final == "Y") %>%
  summarize(count = n(), median_like = median(like_count), Q1_like = quantile(like_count,0.25), Q3_like = quantile(like_count, 0.75), min = quantile(like_count,0),max = quantile(like_count,1))
Topic1_like_Y

Topic1_like_N <- Topic1_like%>%
  subset(Topic1_final == "N") %>%
  summarize(count = n(), median_like = median(like_count), Q1_like = quantile(like_count,0.25), Q3_like = quantile(like_count, 0.75), min = quantile(like_count,0),max = quantile(like_count,1))
Topic1_like_N

Topic1_like_sum <- rbind(Topic1_like_Y,Topic1_like_N)
Topic1_like_sum <- as.data.frame(Topic1_like_sum)

rownames(Topic1_like_sum) <- c("Topic1_like_y","Topic1_like_N")
Topic1_like_sum

#comment#
Topic1_comment_Y <- Topic1_comment %>%
  subset(Topic1_final == "Y") %>%
  summarize(count = n(), median_comment = median(comment_count), Q1_comment = quantile(comment_count,0.25), Q3_comment = quantile(comment_count, 0.75), min = quantile(comment_count,0),max = quantile(comment_count,1))
Topic1_comment_Y

Topic1_comment_N <- Topic1_comment%>%
  subset(Topic1_final == "N") %>%
  summarize(count = n(), median_comment = median(comment_count), Q1_comment = quantile(comment_count,0.25), Q3_comment = quantile(comment_count, 0.75), min = quantile(comment_count,0),max = quantile(comment_count,1))
Topic1_comment_N

Topic1_comment_sum <- rbind(Topic1_comment_Y,Topic1_comment_N)
Topic1_comment_sum <- as.data.frame(Topic1_comment_sum)

rownames(Topic1_comment_sum) <- c("Topic1_comment_y","Topic1_comment_N")
Topic1_comment_sum

# 6.2. Topic_2 
table(df_topic$Topic2_final)

Topic2_view <- df_topic %>%
  select(Topic2_final,view_count) 
Topic2_view <- Topic2_view[complete.cases(Topic2_view ),]

table(Topic2_view$Topic2_final)

Topic2_view$Topic2_final<- factor(Topic2_view$Topic2_final, levels=c("Y", "N"))
table(Topic2_view$Topic2_final)

Topic2_viewday <- df_topic %>%
  select(Topic2_final,views_per_day) 

Topic2_viewday <- Topic2_viewday[complete.cases(Topic2_viewday),]

Topic2_viewday$Topic2_final<- factor(Topic2_viewday$Topic2_final, levels=c("Y", "N"))
table(Topic2_viewday$Topic2_final)

Topic2_like <- df_topic %>%
  select(Topic2_final,like_count) 
Topic2_like <- Topic2_like[complete.cases(Topic2_like),]

Topic2_like$Topic2_final<- factor(Topic2_like$Topic2_final, levels=c("Y", "N"))
table(Topic2_like$Topic2_final)

Topic2_comment <- df_topic %>%
  select(Topic2_final,comment_count) 

Topic2_comment <- Topic2_comment[complete.cases(Topic2_comment),]

Topic2_comment$Topic2_final<- factor(Topic2_comment$Topic2_final, levels=c("Y", "N"))
table(Topic2_comment$Topic2_final)

Topic2_view_wilcox <- wilcox.test(view_count ~ Topic2_final, data = Topic2_view)
Topic2_view_wilcox 

Topic2_viewday_wilcox <- wilcox.test(views_per_day ~ Topic2_final, data = Topic2_viewday)
Topic2_viewday_wilcox 

Topic2_like_wilcox <- wilcox.test(like_count ~ Topic2_final, data = Topic2_like)
Topic2_like_wilcox 

Topic2_comment_wilcox <- wilcox.test(comment_count ~ Topic2_final, data = Topic2_comment)
Topic2_comment_wilcox 

#view#
Topic2_view_Y <- Topic2_view %>%
  subset(Topic2_final == "Y") %>%
  summarize(count = n(), median_view = median(view_count), Q1_view = quantile(view_count,0.25), Q3_view = quantile(view_count, 0.75), min = quantile(view_count,0),max = quantile(view_count,1))
Topic2_view_Y

Topic2_view_N <- Topic2_view%>%
  subset(Topic2_final == "N") %>%
  summarize(count = n(), median_view = median(view_count), Q1_view = quantile(view_count,0.25), Q3_view = quantile(view_count, 0.75), min = quantile(view_count,0),max = quantile(view_count,1))
Topic2_view_N

Topic2_view_sum <- rbind(Topic2_view_Y,Topic2_view_N)
Topic2_view_sum <- as.data.frame(Topic2_view_sum)

rownames(Topic2_view_sum) <- c("Topic2_view_y","Topic2_view_N")
Topic2_view_sum

#viewday#
Topic2_viewday_Y <- Topic2_viewday %>%
  subset(Topic2_final == "Y") %>%
  summarize(count = n(), median_viewday = median(views_per_day), Q1_viewday = quantile(views_per_day,0.25), Q3_viewday = quantile(views_per_day, 0.75), min = quantile(views_per_day,0),max = quantile(views_per_day,1))
Topic2_viewday_Y

Topic2_viewday_N <- Topic2_viewday%>%
  subset(Topic2_final == "N") %>%
  summarize(count = n(), median_viewday = median(views_per_day), Q1_viewday = quantile(views_per_day,0.25), Q3_viewday = quantile(views_per_day, 0.75), min = quantile(views_per_day,0),max = quantile(views_per_day,1))
Topic2_viewday_N

Topic2_viewday_sum <- rbind(Topic2_viewday_Y,Topic2_viewday_N)
Topic2_viewday_sum <- as.data.frame(Topic2_viewday_sum)

rownames(Topic2_viewday_sum) <- c("Topic2_viewday_y","Topic2_viewday_N")
Topic2_viewday_sum

#like#
Topic2_like_Y <- Topic2_like %>%
  subset(Topic2_final == "Y") %>%
  summarize(count = n(), median_like = median(like_count), Q1_like = quantile(like_count,0.25), Q3_like = quantile(like_count, 0.75), min = quantile(like_count,0),max = quantile(like_count,1))
Topic2_like_Y

Topic2_like_N <- Topic2_like%>%
  subset(Topic2_final == "N") %>%
  summarize(count = n(), median_like = median(like_count), Q1_like = quantile(like_count,0.25), Q3_like = quantile(like_count, 0.75), min = quantile(like_count,0),max = quantile(like_count,1))
Topic2_like_N

Topic2_like_sum <- rbind(Topic2_like_Y,Topic2_like_N)
Topic2_like_sum <- as.data.frame(Topic2_like_sum)

rownames(Topic2_like_sum) <- c("Topic2_like_y","Topic2_like_N")
Topic2_like_sum

#comment#
Topic2_comment_Y <- Topic2_comment %>%
  subset(Topic2_final == "Y") %>%
  summarize(count = n(), median_comment = median(comment_count), Q1_comment = quantile(comment_count,0.25), Q3_comment = quantile(comment_count, 0.75), min = quantile(comment_count,0),max = quantile(comment_count,1))
Topic2_comment_Y

Topic2_comment_N <- Topic2_comment%>%
  subset(Topic2_final == "N") %>%
  summarize(count = n(), median_comment = median(comment_count), Q1_comment = quantile(comment_count,0.25), Q3_comment = quantile(comment_count, 0.75), min = quantile(comment_count,0),max = quantile(comment_count,1))
Topic2_comment_N

Topic2_comment_sum <- rbind(Topic2_comment_Y,Topic2_comment_N)
Topic2_comment_sum <- as.data.frame(Topic2_comment_sum)

rownames(Topic2_comment_sum) <- c("Topic2_comment_y","Topic2_comment_N")
Topic2_comment_sum
 
# 6.3. Topic_3
df_topic$Topic3_final <- ifelse(df_topic$Topic3_final == "y","Y",
                                ifelse(df_topic$Topic3_final == "Y","Y","N"))
table(df_topic$Topic3_final)

Topic3_view <- df_topic %>%
  select(Topic3_final,view_count) 
Topic3_view <- Topic3_view[complete.cases(Topic3_view ),]

table(Topic3_view$Topic3_final)

Topic3_view$Topic3_final<- factor(Topic3_view$Topic3_final, levels=c("Y", "N"))
table(Topic3_view$Topic3_final)

Topic3_viewday <- df_topic %>%
  select(Topic3_final,views_per_day) 

Topic3_viewday <- Topic3_viewday[complete.cases(Topic3_viewday),]

Topic3_viewday$Topic3_final<- factor(Topic3_viewday$Topic3_final, levels=c("Y", "N"))
table(Topic3_viewday$Topic3_final)

Topic3_like <- df_topic %>%
  select(Topic3_final,like_count) 
Topic3_like <- Topic3_like[complete.cases(Topic3_like),]

Topic3_like$Topic3_final<- factor(Topic3_like$Topic3_final, levels=c("Y", "N"))
table(Topic3_like$Topic3_final)

Topic3_comment <- df_topic %>%
  select(Topic3_final,comment_count) 

Topic3_comment <- Topic3_comment[complete.cases(Topic3_comment),]

Topic3_comment$Topic3_final<- factor(Topic3_comment$Topic3_final, levels=c("Y", "N"))
table(Topic3_comment$Topic3_final)

Topic3_view_wilcox <- wilcox.test(view_count ~ Topic3_final, data = Topic3_view)
Topic3_view_wilcox 

Topic3_viewday_wilcox <- wilcox.test(views_per_day ~ Topic3_final, data = Topic3_viewday)
Topic3_viewday_wilcox 

Topic3_like_wilcox <- wilcox.test(like_count ~ Topic3_final, data = Topic3_like)
Topic3_like_wilcox 

Topic3_comment_wilcox <- wilcox.test(comment_count ~ Topic3_final, data = Topic3_comment)
Topic3_comment_wilcox 

#view#
Topic3_view_Y <- Topic3_view %>%
  subset(Topic3_final == "Y") %>%
  summarize(count = n(), median_view = median(view_count), Q1_view = quantile(view_count,0.25), Q3_view = quantile(view_count, 0.75), min = quantile(view_count,0),max = quantile(view_count,1))
Topic3_view_Y

Topic3_view_N <- Topic3_view%>%
  subset(Topic3_final == "N") %>%
  summarize(count = n(), median_view = median(view_count), Q1_view = quantile(view_count,0.25), Q3_view = quantile(view_count, 0.75), min = quantile(view_count,0),max = quantile(view_count,1))
Topic3_view_N

Topic3_view_sum <- rbind(Topic3_view_Y,Topic3_view_N)
Topic3_view_sum <- as.data.frame(Topic3_view_sum)

rownames(Topic3_view_sum) <- c("Topic3_view_y","Topic3_view_N")
Topic3_view_sum

#viewday#
Topic3_viewday_Y <- Topic3_viewday %>%
  subset(Topic3_final == "Y") %>%
  summarize(count = n(), median_viewday = median(views_per_day), Q1_viewday = quantile(views_per_day,0.25), Q3_viewday = quantile(views_per_day, 0.75), min = quantile(views_per_day,0),max = quantile(views_per_day,1))
Topic3_viewday_Y

Topic3_viewday_N <- Topic3_viewday%>%
  subset(Topic3_final == "N") %>%
  summarize(count = n(), median_viewday = median(views_per_day), Q1_viewday = quantile(views_per_day,0.25), Q3_viewday = quantile(views_per_day, 0.75), min = quantile(views_per_day,0),max = quantile(views_per_day,1))
Topic3_viewday_N

Topic3_viewday_sum <- rbind(Topic3_viewday_Y,Topic3_viewday_N)
Topic3_viewday_sum <- as.data.frame(Topic3_viewday_sum)

rownames(Topic3_viewday_sum) <- c("Topic3_viewday_y","Topic3_viewday_N")
Topic3_viewday_sum

#like#
Topic3_like_Y <- Topic3_like %>%
  subset(Topic3_final == "Y") %>%
  summarize(count = n(), median_like = median(like_count), Q1_like = quantile(like_count,0.25), Q3_like = quantile(like_count, 0.75), min = quantile(like_count,0),max = quantile(like_count,1))
Topic3_like_Y

Topic3_like_N <- Topic3_like%>%
  subset(Topic3_final == "N") %>%
  summarize(count = n(), median_like = median(like_count), Q1_like = quantile(like_count,0.25), Q3_like = quantile(like_count, 0.75), min = quantile(like_count,0),max = quantile(like_count,1))
Topic3_like_N

Topic3_like_sum <- rbind(Topic3_like_Y,Topic3_like_N)
Topic3_like_sum <- as.data.frame(Topic3_like_sum)

rownames(Topic3_like_sum) <- c("Topic3_like_y","Topic3_like_N")
Topic3_like_sum

#comment#
Topic3_comment_Y <- Topic3_comment %>%
  subset(Topic3_final == "Y") %>%
  summarize(count = n(), median_comment = median(comment_count), Q1_comment = quantile(comment_count,0.25), Q3_comment = quantile(comment_count, 0.75), min = quantile(comment_count,0),max = quantile(comment_count,1))
Topic3_comment_Y

Topic3_comment_N <- Topic3_comment%>%
  subset(Topic3_final == "N") %>%
  summarize(count = n(), median_comment = median(comment_count), Q1_comment = quantile(comment_count,0.25), Q3_comment = quantile(comment_count, 0.75), min = quantile(comment_count,0),max = quantile(comment_count,1))
Topic3_comment_N

Topic3_comment_sum <- rbind(Topic3_comment_Y,Topic3_comment_N)
Topic3_comment_sum <- as.data.frame(Topic3_comment_sum)

rownames(Topic3_comment_sum) <- c("Topic3_comment_y","Topic3_comment_N")
Topic3_comment_sum

# 6.4. Topic_4 
table(df_topic$Topic4_final)
Topic4_view <- df_topic %>%
  select(Topic4_final,view_count) 
Topic4_view <- Topic4_view[complete.cases(Topic4_view ),]

table(Topic4_view$Topic4_final)

Topic4_view$Topic4_final<- factor(Topic4_view$Topic4_final, levels=c("Y", "N"))
table(Topic4_view$Topic4_final)

Topic4_viewday <- df_topic %>%
  select(Topic4_final,views_per_day) 

Topic4_viewday <- Topic4_viewday[complete.cases(Topic4_viewday),]

Topic4_viewday$Topic4_final<- factor(Topic4_viewday$Topic4_final, levels=c("Y", "N"))
table(Topic4_viewday$Topic4_final)

Topic4_like <- df_topic %>%
  select(Topic4_final,like_count) 
Topic4_like <- Topic4_like[complete.cases(Topic4_like),]

Topic4_like$Topic4_final<- factor(Topic4_like$Topic4_final, levels=c("Y", "N"))
table(Topic4_like$Topic4_final)

Topic4_comment <- df_topic %>%
  select(Topic4_final,comment_count) 

Topic4_comment <- Topic4_comment[complete.cases(Topic4_comment),]

Topic4_comment$Topic4_final<- factor(Topic4_comment$Topic4_final, levels=c("Y", "N"))
table(Topic4_comment$Topic4_final)

Topic4_view_wilcox <- wilcox.test(view_count ~ Topic4_final, data = Topic4_view)
Topic4_view_wilcox 

Topic4_viewday_wilcox <- wilcox.test(views_per_day ~ Topic4_final, data = Topic4_viewday)
Topic4_viewday_wilcox 

Topic4_like_wilcox <- wilcox.test(like_count ~ Topic4_final, data = Topic4_like)
Topic4_like_wilcox 

Topic4_comment_wilcox <- wilcox.test(comment_count ~ Topic4_final, data = Topic4_comment)
Topic4_comment_wilcox 

#view#
Topic4_view_Y <- Topic4_view %>%
  subset(Topic4_final == "Y") %>%
  summarize(count = n(), median_view = median(view_count), Q1_view = quantile(view_count,0.25), Q3_view = quantile(view_count, 0.75), min = quantile(view_count,0),max = quantile(view_count,1))
Topic4_view_Y

Topic4_view_N <- Topic4_view%>%
  subset(Topic4_final == "N") %>%
  summarize(count = n(), median_view = median(view_count), Q1_view = quantile(view_count,0.25), Q3_view = quantile(view_count, 0.75), min = quantile(view_count,0),max = quantile(view_count,1))
Topic4_view_N

Topic4_view_sum <- rbind(Topic4_view_Y,Topic4_view_N)
Topic4_view_sum <- as.data.frame(Topic4_view_sum)

rownames(Topic4_view_sum) <- c("Topic4_view_y","Topic4_view_N")
Topic4_view_sum

#viewday#
Topic4_viewday_Y <- Topic4_viewday %>%
  subset(Topic4_final == "Y") %>%
  summarize(count = n(), median_viewday = median(views_per_day), Q1_viewday = quantile(views_per_day,0.25), Q3_viewday = quantile(views_per_day, 0.75), min = quantile(views_per_day,0),max = quantile(views_per_day,1))
Topic4_viewday_Y

Topic4_viewday_N <- Topic4_viewday%>%
  subset(Topic4_final == "N") %>%
  summarize(count = n(), median_viewday = median(views_per_day), Q1_viewday = quantile(views_per_day,0.25), Q3_viewday = quantile(views_per_day, 0.75), min = quantile(views_per_day,0),max = quantile(views_per_day,1))
Topic4_viewday_N

Topic4_viewday_sum <- rbind(Topic4_viewday_Y,Topic4_viewday_N)
Topic4_viewday_sum <- as.data.frame(Topic4_viewday_sum)

rownames(Topic4_viewday_sum) <- c("Topic4_viewday_y","Topic4_viewday_N")
Topic4_viewday_sum

#like#
Topic4_like_Y <- Topic4_like %>%
  subset(Topic4_final == "Y") %>%
  summarize(count = n(), median_like = median(like_count), Q1_like = quantile(like_count,0.25), Q3_like = quantile(like_count, 0.75), min = quantile(like_count,0),max = quantile(like_count,1))
Topic4_like_Y

Topic4_like_N <- Topic4_like%>%
  subset(Topic4_final == "N") %>%
  summarize(count = n(), median_like = median(like_count), Q1_like = quantile(like_count,0.25), Q3_like = quantile(like_count, 0.75), min = quantile(like_count,0),max = quantile(like_count,1))
Topic4_like_N

Topic4_like_sum <- rbind(Topic4_like_Y,Topic4_like_N)
Topic4_like_sum <- as.data.frame(Topic4_like_sum)

rownames(Topic4_like_sum) <- c("Topic4_like_y","Topic4_like_N")
Topic4_like_sum

#comment#
Topic4_comment_Y <- Topic4_comment %>%
  subset(Topic4_final == "Y") %>%
  summarize(count = n(), median_comment = median(comment_count), Q1_comment = quantile(comment_count,0.25), Q3_comment = quantile(comment_count, 0.75), min = quantile(comment_count,0),max = quantile(comment_count,1))
Topic4_comment_Y

Topic4_comment_N <- Topic4_comment%>%
  subset(Topic4_final == "N") %>%
  summarize(count = n(), median_comment = median(comment_count), Q1_comment = quantile(comment_count,0.25), Q3_comment = quantile(comment_count, 0.75), min = quantile(comment_count,0),max = quantile(comment_count,1))
Topic4_comment_N

Topic4_comment_sum <- rbind(Topic4_comment_Y,Topic4_comment_N)
Topic4_comment_sum <- as.data.frame(Topic4_comment_sum)

rownames(Topic4_comment_sum) <- c("Topic4_comment_y","Topic4_comment_N")
Topic4_comment_sum

# 6.5. Topic_5 
table(df_topic$Topic5_final)

Topic5_view <- df_topic %>%
  select(Topic5_final,view_count) 
Topic5_view <- Topic5_view[complete.cases(Topic5_view ),]

table(Topic5_view$Topic5_final)

Topic5_view$Topic5_final<- factor(Topic5_view$Topic5_final, levels=c("Y", "N"))
table(Topic5_view$Topic5_final)

Topic5_viewday <- df_topic %>%
  select(Topic5_final,views_per_day) 

Topic5_viewday <- Topic5_viewday[complete.cases(Topic5_viewday),]

Topic5_viewday$Topic5_final<- factor(Topic5_viewday$Topic5_final, levels=c("Y", "N"))
table(Topic5_viewday$Topic5_final)

Topic5_like <- df_topic %>%
  select(Topic5_final,like_count) 
Topic5_like <- Topic5_like[complete.cases(Topic5_like),]

Topic5_like$Topic5_final<- factor(Topic5_like$Topic5_final, levels=c("Y", "N"))
table(Topic5_like$Topic5_final)

Topic5_comment <- df_topic %>%
  select(Topic5_final,comment_count) 

Topic5_comment <- Topic5_comment[complete.cases(Topic5_comment),]

Topic5_comment$Topic5_final<- factor(Topic5_comment$Topic5_final, levels=c("Y", "N"))
table(Topic5_comment$Topic5_final)

Topic5_view_wilcox <- wilcox.test(view_count ~ Topic5_final, data = Topic5_view)
Topic5_view_wilcox 

Topic5_viewday_wilcox <- wilcox.test(views_per_day ~ Topic5_final, data = Topic5_viewday)
Topic5_viewday_wilcox 

Topic5_like_wilcox <- wilcox.test(like_count ~ Topic5_final, data = Topic5_like)
Topic5_like_wilcox 

Topic5_comment_wilcox <- wilcox.test(comment_count ~ Topic5_final, data = Topic5_comment)
Topic5_comment_wilcox 

#view#
Topic5_view_Y <- Topic5_view %>%
  subset(Topic5_final == "Y") %>%
  summarize(count = n(), median_view = median(view_count), Q1_view = quantile(view_count,0.25), Q3_view = quantile(view_count, 0.75), min = quantile(view_count,0),max = quantile(view_count,1))
Topic5_view_Y

Topic5_view_N <- Topic5_view%>%
  subset(Topic5_final == "N") %>%
  summarize(count = n(), median_view = median(view_count), Q1_view = quantile(view_count,0.25), Q3_view = quantile(view_count, 0.75), min = quantile(view_count,0),max = quantile(view_count,1))
Topic5_view_N

Topic5_view_sum <- rbind(Topic5_view_Y,Topic5_view_N)
Topic5_view_sum <- as.data.frame(Topic5_view_sum)

rownames(Topic5_view_sum) <- c("Topic5_view_y","Topic5_view_N")
Topic5_view_sum

#viewday#
Topic5_viewday_Y <- Topic5_viewday %>%
  subset(Topic5_final == "Y") %>%
  summarize(count = n(), median_viewday = median(views_per_day), Q1_viewday = quantile(views_per_day,0.25), Q3_viewday = quantile(views_per_day, 0.75), min = quantile(views_per_day,0),max = quantile(views_per_day,1))
Topic5_viewday_Y

Topic5_viewday_N <- Topic5_viewday%>%
  subset(Topic5_final == "N") %>%
  summarize(count = n(), median_viewday = median(views_per_day), Q1_viewday = quantile(views_per_day,0.25), Q3_viewday = quantile(views_per_day, 0.75), min = quantile(views_per_day,0),max = quantile(views_per_day,1))
Topic5_viewday_N

Topic5_viewday_sum <- rbind(Topic5_viewday_Y,Topic5_viewday_N)
Topic5_viewday_sum <- as.data.frame(Topic5_viewday_sum)

rownames(Topic5_viewday_sum) <- c("Topic5_viewday_y","Topic5_viewday_N")
Topic5_viewday_sum

#like#
Topic5_like_Y <- Topic5_like %>%
  subset(Topic5_final == "Y") %>%
  summarize(count = n(), median_like = median(like_count), Q1_like = quantile(like_count,0.25), Q3_like = quantile(like_count, 0.75), min = quantile(like_count,0),max = quantile(like_count,1))
Topic5_like_Y

Topic5_like_N <- Topic5_like%>%
  subset(Topic5_final == "N") %>%
  summarize(count = n(), median_like = median(like_count), Q1_like = quantile(like_count,0.25), Q3_like = quantile(like_count, 0.75), min = quantile(like_count,0),max = quantile(like_count,1))
Topic5_like_N

Topic5_like_sum <- rbind(Topic5_like_Y,Topic5_like_N)
Topic5_like_sum <- as.data.frame(Topic5_like_sum)

rownames(Topic5_like_sum) <- c("Topic5_like_y","Topic5_like_N")
Topic5_like_sum

#comment#
Topic5_comment_Y <- Topic5_comment %>%
  subset(Topic5_final == "Y") %>%
  summarize(count = n(), median_comment = median(comment_count), Q1_comment = quantile(comment_count,0.25), Q3_comment = quantile(comment_count, 0.75), min = quantile(comment_count,0),max = quantile(comment_count,1))
Topic5_comment_Y

Topic5_comment_N <- Topic5_comment%>%
  subset(Topic5_final == "N") %>%
  summarize(count = n(), median_comment = median(comment_count), Q1_comment = quantile(comment_count,0.25), Q3_comment = quantile(comment_count, 0.75), min = quantile(comment_count,0),max = quantile(comment_count,1))
Topic5_comment_N

Topic5_comment_sum <- rbind(Topic5_comment_Y,Topic5_comment_N)
Topic5_comment_sum <- as.data.frame(Topic5_comment_sum)

rownames(Topic5_comment_sum) <- c("Topic5_comment_y","Topic5_comment_N")
Topic5_comment_sum

# 6.6.Topic_6 
table(df_topic$Topic6_final)

Topic6_view <- df_topic %>%
  select(Topic6_final,view_count) 
Topic6_view <- Topic6_view[complete.cases(Topic6_view ),]

table(Topic6_view$Topic6_final)

Topic6_view$Topic6_final<- factor(Topic6_view$Topic6_final, levels=c("Y", "N"))
table(Topic6_view$Topic6_final)

Topic6_viewday <- df_topic %>%
  select(Topic6_final,views_per_day) 

Topic6_viewday <- Topic6_viewday[complete.cases(Topic6_viewday),]

Topic6_viewday$Topic6_final<- factor(Topic6_viewday$Topic6_final, levels=c("Y", "N"))
table(Topic6_viewday$Topic6_final)

Topic6_like <- df_topic %>%
  select(Topic6_final,like_count) 
Topic6_like <- Topic6_like[complete.cases(Topic6_like),]

Topic6_like$Topic6_final<- factor(Topic6_like$Topic6_final, levels=c("Y", "N"))
table(Topic6_like$Topic6_final)

Topic6_comment <- df_topic %>%
  select(Topic6_final,comment_count) 

Topic6_comment <- Topic6_comment[complete.cases(Topic6_comment),]

Topic6_comment$Topic6_final<- factor(Topic6_comment$Topic6_final, levels=c("Y", "N"))
table(Topic6_comment$Topic6_final)

Topic6_view_wilcox <- wilcox.test(view_count ~ Topic6_final, data = Topic6_view)
Topic6_view_wilcox 

Topic6_viewday_wilcox <- wilcox.test(views_per_day ~ Topic6_final, data = Topic6_viewday)
Topic6_viewday_wilcox 

Topic6_like_wilcox <- wilcox.test(like_count ~ Topic6_final, data = Topic6_like)
Topic6_like_wilcox 

Topic6_comment_wilcox <- wilcox.test(comment_count ~ Topic6_final, data = Topic6_comment)
Topic6_comment_wilcox 

#view#
Topic6_view_Y <- Topic6_view %>%
  subset(Topic6_final == "Y") %>%
  summarize(count = n(), median_view = median(view_count), Q1_view = quantile(view_count,0.25), Q3_view = quantile(view_count, 0.75), min = quantile(view_count,0),max = quantile(view_count,1))
Topic6_view_Y

Topic6_view_N <- Topic6_view%>%
  subset(Topic6_final == "N") %>%
  summarize(count = n(), median_view = median(view_count), Q1_view = quantile(view_count,0.25), Q3_view = quantile(view_count, 0.75), min = quantile(view_count,0),max = quantile(view_count,1))
Topic6_view_N

Topic6_view_sum <- rbind(Topic6_view_Y,Topic6_view_N)
Topic6_view_sum <- as.data.frame(Topic6_view_sum)

rownames(Topic6_view_sum) <- c("Topic6_view_y","Topic6_view_N")
Topic6_view_sum

#viewday#
Topic6_viewday_Y <- Topic6_viewday %>%
  subset(Topic6_final == "Y") %>%
  summarize(count = n(), median_viewday = median(views_per_day), Q1_viewday = quantile(views_per_day,0.25), Q3_viewday = quantile(views_per_day, 0.75), min = quantile(views_per_day,0),max = quantile(views_per_day,1))
Topic6_viewday_Y

Topic6_viewday_N <- Topic6_viewday%>%
  subset(Topic6_final == "N") %>%
  summarize(count = n(), median_viewday = median(views_per_day), Q1_viewday = quantile(views_per_day,0.25), Q3_viewday = quantile(views_per_day, 0.75), min = quantile(views_per_day,0),max = quantile(views_per_day,1))
Topic6_viewday_N

Topic6_viewday_sum <- rbind(Topic6_viewday_Y,Topic6_viewday_N)
Topic6_viewday_sum <- as.data.frame(Topic6_viewday_sum)

rownames(Topic6_viewday_sum) <- c("Topic6_viewday_y","Topic6_viewday_N")
Topic6_viewday_sum

#like#
Topic6_like_Y <- Topic6_like %>%
  subset(Topic6_final == "Y") %>%
  summarize(count = n(), median_like = median(like_count), Q1_like = quantile(like_count,0.25), Q3_like = quantile(like_count, 0.75), min = quantile(like_count,0),max = quantile(like_count,1))
Topic6_like_Y

Topic6_like_N <- Topic6_like%>%
  subset(Topic6_final == "N") %>%
  summarize(count = n(), median_like = median(like_count), Q1_like = quantile(like_count,0.25), Q3_like = quantile(like_count, 0.75), min = quantile(like_count,0),max = quantile(like_count,1))
Topic6_like_N

Topic6_like_sum <- rbind(Topic6_like_Y,Topic6_like_N)
Topic6_like_sum <- as.data.frame(Topic6_like_sum)

rownames(Topic6_like_sum) <- c("Topic6_like_y","Topic6_like_N")
Topic6_like_sum

#comment#
Topic6_comment_Y <- Topic6_comment %>%
  subset(Topic6_final == "Y") %>%
  summarize(count = n(), median_comment = median(comment_count), Q1_comment = quantile(comment_count,0.25), Q3_comment = quantile(comment_count, 0.75), min = quantile(comment_count,0),max = quantile(comment_count,1))
Topic6_comment_Y

Topic6_comment_N <- Topic6_comment%>%
  subset(Topic6_final == "N") %>%
  summarize(count = n(), median_comment = median(comment_count), Q1_comment = quantile(comment_count,0.25), Q3_comment = quantile(comment_count, 0.75), min = quantile(comment_count,0),max = quantile(comment_count,1))
Topic6_comment_N

Topic6_comment_sum <- rbind(Topic6_comment_Y,Topic6_comment_N)
Topic6_comment_sum <- as.data.frame(Topic6_comment_sum)

rownames(Topic6_comment_sum) <- c("Topic6_comment_y","Topic6_comment_N")
Topic6_comment_sum

# 6.7. Topic_7
table(df_topic$Topic7_final)

Topic7_view <- df_topic %>%
  select(Topic7_final,view_count) 
Topic7_view <- Topic7_view[complete.cases(Topic7_view ),]

table(Topic7_view$Topic7_final)

Topic7_view$Topic7_final<- factor(Topic7_view$Topic7_final, levels=c("Y", "N"))
table(Topic7_view$Topic7_final)

Topic7_viewday <- df_topic %>%
  select(Topic7_final,views_per_day) 

Topic7_viewday <- Topic7_viewday[complete.cases(Topic7_viewday),]

Topic7_viewday$Topic7_final<- factor(Topic7_viewday$Topic7_final, levels=c("Y", "N"))
table(Topic7_viewday$Topic7_final)


Topic7_like <- df_topic %>%
  select(Topic7_final,like_count) 
Topic7_like <- Topic7_like[complete.cases(Topic7_like),]

Topic7_like$Topic7_final<- factor(Topic7_like$Topic7_final, levels=c("Y", "N"))
table(Topic7_like$Topic7_final)

Topic7_comment <- df_topic %>%
  select(Topic7_final,comment_count) 

Topic7_comment <- Topic7_comment[complete.cases(Topic7_comment),]

Topic7_comment$Topic7_final<- factor(Topic7_comment$Topic7_final, levels=c("Y", "N"))
table(Topic7_comment$Topic7_final)

Topic7_view_wilcox <- wilcox.test(view_count ~ Topic7_final, data = Topic7_view)
Topic7_view_wilcox 

Topic7_viewday_wilcox <- wilcox.test(views_per_day ~ Topic7_final, data = Topic7_viewday)
Topic7_viewday_wilcox 

Topic7_like_wilcox <- wilcox.test(like_count ~ Topic7_final, data = Topic7_like)
Topic7_like_wilcox 

Topic7_comment_wilcox <- wilcox.test(comment_count ~ Topic7_final, data = Topic7_comment)
Topic7_comment_wilcox 

#view#
Topic7_view_Y <- Topic7_view %>%
  subset(Topic7_final == "Y") %>%
  summarize(count = n(), median_view = median(view_count), Q1_view = quantile(view_count,0.25), Q3_view = quantile(view_count, 0.75), min = quantile(view_count,0),max = quantile(view_count,1))
Topic7_view_Y

Topic7_view_N <- Topic7_view%>%
  subset(Topic7_final == "N") %>%
  summarize(count = n(), median_view = median(view_count), Q1_view = quantile(view_count,0.25), Q3_view = quantile(view_count, 0.75), min = quantile(view_count,0),max = quantile(view_count,1))
Topic7_view_N

Topic7_view_sum <- rbind(Topic7_view_Y,Topic7_view_N)
Topic7_view_sum <- as.data.frame(Topic7_view_sum)

rownames(Topic7_view_sum) <- c("Topic7_view_y","Topic7_view_N")
Topic7_view_sum

#viewday#
Topic7_viewday_Y <- Topic7_viewday %>%
  subset(Topic7_final == "Y") %>%
  summarize(count = n(), median_viewday = median(views_per_day), Q1_viewday = quantile(views_per_day,0.25), Q3_viewday = quantile(views_per_day, 0.75), min = quantile(views_per_day,0),max = quantile(views_per_day,1))
Topic7_viewday_Y

Topic7_viewday_N <- Topic7_viewday%>%
  subset(Topic7_final == "N") %>%
  summarize(count = n(), median_viewday = median(views_per_day), Q1_viewday = quantile(views_per_day,0.25), Q3_viewday = quantile(views_per_day, 0.75), min = quantile(views_per_day,0),max = quantile(views_per_day,1))
Topic7_viewday_N

Topic7_viewday_sum <- rbind(Topic7_viewday_Y,Topic7_viewday_N)
Topic7_viewday_sum <- as.data.frame(Topic7_viewday_sum)

rownames(Topic7_viewday_sum) <- c("Topic7_viewday_y","Topic7_viewday_N")
Topic7_viewday_sum

#like#
Topic7_like_Y <- Topic7_like %>%
  subset(Topic7_final == "Y") %>%
  summarize(count = n(), median_like = median(like_count), Q1_like = quantile(like_count,0.25), Q3_like = quantile(like_count, 0.75), min = quantile(like_count,0),max = quantile(like_count,1))
Topic7_like_Y

Topic7_like_N <- Topic7_like%>%
  subset(Topic7_final == "N") %>%
  summarize(count = n(), median_like = median(like_count), Q1_like = quantile(like_count,0.25), Q3_like = quantile(like_count, 0.75), min = quantile(like_count,0),max = quantile(like_count,1))
Topic7_like_N

Topic7_like_sum <- rbind(Topic7_like_Y,Topic7_like_N)
Topic7_like_sum <- as.data.frame(Topic7_like_sum)

rownames(Topic7_like_sum) <- c("Topic7_like_y","Topic7_like_N")
Topic7_like_sum

#comment#
Topic7_comment_Y <- Topic7_comment %>%
  subset(Topic7_final == "Y") %>%
  summarize(count = n(), median_comment = median(comment_count), Q1_comment = quantile(comment_count,0.25), Q3_comment = quantile(comment_count, 0.75), min = quantile(comment_count,0),max = quantile(comment_count,1))
Topic7_comment_Y

Topic7_comment_N <- Topic7_comment%>%
  subset(Topic7_final == "N") %>%
  summarize(count = n(), median_comment = median(comment_count), Q1_comment = quantile(comment_count,0.25), Q3_comment = quantile(comment_count, 0.75), min = quantile(comment_count,0),max = quantile(comment_count,1))
Topic7_comment_N

Topic7_comment_sum <- rbind(Topic7_comment_Y,Topic7_comment_N)
Topic7_comment_sum <- as.data.frame(Topic7_comment_sum)

rownames(Topic7_comment_sum) <- c("Topic7_comment_y","Topic7_comment_N")
Topic7_comment_sum

# 6.8. Topic_8 
table(df_topic$Topic8_final)

Topic8_view <- df_topic %>%
  select(Topic8_final,view_count) 
Topic8_view <- Topic8_view[complete.cases(Topic8_view ),]

table(Topic8_view$Topic8_final)

Topic8_view$Topic8_final<- factor(Topic8_view$Topic8_final, levels=c("Y", "N"))
table(Topic8_view$Topic8_final)

Topic8_viewday <- df_topic %>%
  select(Topic8_final,views_per_day) 

Topic8_viewday <- Topic8_viewday[complete.cases(Topic8_viewday),]

Topic8_viewday$Topic8_final<- factor(Topic8_viewday$Topic8_final, levels=c("Y", "N"))
table(Topic8_viewday$Topic8_final)

Topic8_like <- df_topic %>%
  select(Topic8_final,like_count) 
Topic8_like <- Topic8_like[complete.cases(Topic8_like),]

Topic8_like$Topic8_final<- factor(Topic8_like$Topic8_final, levels=c("Y", "N"))
table(Topic8_like$Topic8_final)

Topic8_comment <- df_topic %>%
  select(Topic8_final,comment_count) 

Topic8_comment <- Topic8_comment[complete.cases(Topic8_comment),]

Topic8_comment$Topic8_final<- factor(Topic8_comment$Topic8_final, levels=c("Y", "N"))
table(Topic8_comment$Topic8_final)

Topic8_view_wilcox <- wilcox.test(view_count ~ Topic8_final, data = Topic8_view)
Topic8_view_wilcox 

Topic8_viewday_wilcox <- wilcox.test(views_per_day ~ Topic8_final, data = Topic8_viewday)
Topic8_viewday_wilcox 

Topic8_like_wilcox <- wilcox.test(like_count ~ Topic8_final, data = Topic8_like)
Topic8_like_wilcox 

Topic8_comment_wilcox <- wilcox.test(comment_count ~ Topic8_final, data = Topic8_comment)
Topic8_comment_wilcox 

#view#
Topic8_view_Y <- Topic8_view %>%
  subset(Topic8_final == "Y") %>%
  summarize(count = n(), median_view = median(view_count), Q1_view = quantile(view_count,0.25), Q3_view = quantile(view_count, 0.75), min = quantile(view_count,0),max = quantile(view_count,1))
Topic8_view_Y

Topic8_view_N <- Topic8_view%>%
  subset(Topic8_final == "N") %>%
  summarize(count = n(), median_view = median(view_count), Q1_view = quantile(view_count,0.25), Q3_view = quantile(view_count, 0.75), min = quantile(view_count,0),max = quantile(view_count,1))
Topic8_view_N

Topic8_view_sum <- rbind(Topic8_view_Y,Topic8_view_N)
Topic8_view_sum <- as.data.frame(Topic8_view_sum)

rownames(Topic8_view_sum) <- c("Topic8_view_y","Topic8_view_N")
Topic8_view_sum

#viewday#
Topic8_viewday_Y <- Topic8_viewday %>%
  subset(Topic8_final == "Y") %>%
  summarize(count = n(), median_viewday = median(views_per_day), Q1_viewday = quantile(views_per_day,0.25), Q3_viewday = quantile(views_per_day, 0.75), min = quantile(views_per_day,0),max = quantile(views_per_day,1))
Topic8_viewday_Y

Topic8_viewday_N <- Topic8_viewday%>%
  subset(Topic8_final == "N") %>%
  summarize(count = n(), median_viewday = median(views_per_day), Q1_viewday = quantile(views_per_day,0.25), Q3_viewday = quantile(views_per_day, 0.75), min = quantile(views_per_day,0),max = quantile(views_per_day,1))
Topic8_viewday_N

Topic8_viewday_sum <- rbind(Topic8_viewday_Y,Topic8_viewday_N)
Topic8_viewday_sum <- as.data.frame(Topic8_viewday_sum)

rownames(Topic8_viewday_sum) <- c("Topic8_viewday_y","Topic8_viewday_N")
Topic8_viewday_sum

#like#
Topic8_like_Y <- Topic8_like %>%
  subset(Topic8_final == "Y") %>%
  summarize(count = n(), median_like = median(like_count), Q1_like = quantile(like_count,0.25), Q3_like = quantile(like_count, 0.75), min = quantile(like_count,0),max = quantile(like_count,1))
Topic8_like_Y

Topic8_like_N <- Topic8_like%>%
  subset(Topic8_final == "N") %>%
  summarize(count = n(), median_like = median(like_count), Q1_like = quantile(like_count,0.25), Q3_like = quantile(like_count, 0.75), min = quantile(like_count,0),max = quantile(like_count,1))
Topic8_like_N

Topic8_like_sum <- rbind(Topic8_like_Y,Topic8_like_N)
Topic8_like_sum <- as.data.frame(Topic8_like_sum)

rownames(Topic8_like_sum) <- c("Topic8_like_y","Topic8_like_N")
Topic8_like_sum

#comment#
Topic8_comment_Y <- Topic8_comment %>%
  subset(Topic8_final == "Y") %>%
  summarize(count = n(), median_comment = median(comment_count), Q1_comment = quantile(comment_count,0.25), Q3_comment = quantile(comment_count, 0.75), min = quantile(comment_count,0),max = quantile(comment_count,1))
Topic8_comment_Y

Topic8_comment_N <- Topic8_comment%>%
  subset(Topic8_final == "N") %>%
  summarize(count = n(), median_comment = median(comment_count), Q1_comment = quantile(comment_count,0.25), Q3_comment = quantile(comment_count, 0.75), min = quantile(comment_count,0),max = quantile(comment_count,1))
Topic8_comment_N

Topic8_comment_sum <- rbind(Topic8_comment_Y,Topic8_comment_N)
Topic8_comment_sum <- as.data.frame(Topic8_comment_sum)

rownames(Topic8_comment_sum) <- c("Topic8_comment_y","Topic8_comment_N")
Topic8_comment_sum
###

# 6.9. Topic_9

table(df_topic$Topic9_final)

Topic9_view <- df_topic %>%
  select(Topic9_final,view_count) 
Topic9_view <- Topic9_view[complete.cases(Topic9_view ),]

table(Topic9_view$Topic9_final)

Topic9_view$Topic9_final<- factor(Topic9_view$Topic9_final, levels=c("Y", "N"))
table(Topic9_view$Topic9_final)

Topic9_viewday <- df_topic %>%
  select(Topic9_final,views_per_day) 

Topic9_viewday <- Topic9_viewday[complete.cases(Topic9_viewday),]

Topic9_viewday$Topic9_final<- factor(Topic9_viewday$Topic9_final, levels=c("Y", "N"))
table(Topic9_viewday$Topic9_final)

Topic9_like <- df_topic %>%
  select(Topic9_final,like_count) 
Topic9_like <- Topic9_like[complete.cases(Topic9_like),]

Topic9_like$Topic9_final<- factor(Topic9_like$Topic9_final, levels=c("Y", "N"))
table(Topic9_like$Topic9_final)

Topic9_comment <- df_topic %>%
  select(Topic9_final,comment_count) 

Topic9_comment <- Topic9_comment[complete.cases(Topic9_comment),]

Topic9_comment$Topic9_final<- factor(Topic9_comment$Topic9_final, levels=c("Y", "N"))
table(Topic9_comment$Topic9_final)

Topic9_view_wilcox <- wilcox.test(view_count ~ Topic9_final, data = Topic9_view)
Topic9_view_wilcox 

Topic9_viewday_wilcox <- wilcox.test(views_per_day ~ Topic9_final, data = Topic9_viewday)
Topic9_viewday_wilcox 

Topic9_like_wilcox <- wilcox.test(like_count ~ Topic9_final, data = Topic9_like)
Topic9_like_wilcox 

Topic9_comment_wilcox <- wilcox.test(comment_count ~ Topic9_final, data = Topic9_comment)
Topic9_comment_wilcox 

#view#
Topic9_view_Y <- Topic9_view %>%
  subset(Topic9_final == "Y") %>%
  summarize(count = n(), median_view = median(view_count), Q1_view = quantile(view_count,0.25), Q3_view = quantile(view_count, 0.75), min = quantile(view_count,0),max = quantile(view_count,1))
Topic9_view_Y

Topic9_view_N <- Topic9_view%>%
  subset(Topic9_final == "N") %>%
  summarize(count = n(), median_view = median(view_count), Q1_view = quantile(view_count,0.25), Q3_view = quantile(view_count, 0.75), min = quantile(view_count,0),max = quantile(view_count,1))
Topic9_view_N

Topic9_view_sum <- rbind(Topic9_view_Y,Topic9_view_N)
Topic9_view_sum <- as.data.frame(Topic9_view_sum)

rownames(Topic9_view_sum) <- c("Topic9_view_y","Topic9_view_N")
Topic9_view_sum

#viewday#
Topic9_viewday_Y <- Topic9_viewday %>%
  subset(Topic9_final == "Y") %>%
  summarize(count = n(), median_viewday = median(views_per_day), Q1_viewday = quantile(views_per_day,0.25), Q3_viewday = quantile(views_per_day, 0.75), min = quantile(views_per_day,0),max = quantile(views_per_day,1))
Topic9_viewday_Y

Topic9_viewday_N <- Topic9_viewday%>%
  subset(Topic9_final == "N") %>%
  summarize(count = n(), median_viewday = median(views_per_day), Q1_viewday = quantile(views_per_day,0.25), Q3_viewday = quantile(views_per_day, 0.75), min = quantile(views_per_day,0),max = quantile(views_per_day,1))
Topic9_viewday_N

Topic9_viewday_sum <- rbind(Topic9_viewday_Y,Topic9_viewday_N)
Topic9_viewday_sum <- as.data.frame(Topic9_viewday_sum)

rownames(Topic9_viewday_sum) <- c("Topic9_viewday_y","Topic9_viewday_N")
Topic9_viewday_sum

#like#
Topic9_like_Y <- Topic9_like %>%
  subset(Topic9_final == "Y") %>%
  summarize(count = n(), median_like = median(like_count), Q1_like = quantile(like_count,0.25), Q3_like = quantile(like_count, 0.75), min = quantile(like_count,0),max = quantile(like_count,1))
Topic9_like_Y

Topic9_like_N <- Topic9_like%>%
  subset(Topic9_final == "N") %>%
  summarize(count = n(), median_like = median(like_count), Q1_like = quantile(like_count,0.25), Q3_like = quantile(like_count, 0.75), min = quantile(like_count,0),max = quantile(like_count,1))
Topic9_like_N

Topic9_like_sum <- rbind(Topic9_like_Y,Topic9_like_N)
Topic9_like_sum <- as.data.frame(Topic9_like_sum)

rownames(Topic9_like_sum) <- c("Topic9_like_y","Topic9_like_N")
Topic9_like_sum

#comment#
Topic9_comment_Y <- Topic9_comment %>%
  subset(Topic9_final == "Y") %>%
  summarize(count = n(), median_comment = median(comment_count), Q1_comment = quantile(comment_count,0.25), Q3_comment = quantile(comment_count, 0.75), min = quantile(comment_count,0),max = quantile(comment_count,1))
Topic9_comment_Y

Topic9_comment_N <- Topic9_comment%>%
  subset(Topic9_final == "N") %>%
  summarize(count = n(), median_comment = median(comment_count), Q1_comment = quantile(comment_count,0.25), Q3_comment = quantile(comment_count, 0.75), min = quantile(comment_count,0),max = quantile(comment_count,1))
Topic9_comment_N

Topic9_comment_sum <- rbind(Topic9_comment_Y,Topic9_comment_N)
Topic9_comment_sum <- as.data.frame(Topic9_comment_sum)

rownames(Topic9_comment_sum) <- c("Topic9_comment_y","Topic9_comment_N")
Topic9_comment_sum

#6.10. Topic_10 
table(df_topic$Topic10_final)

Topic10_view <- df_topic %>%
  select(Topic10_final,view_count) 
Topic10_view <- Topic10_view[complete.cases(Topic10_view ),]

table(Topic10_view$Topic10_final)

Topic10_view$Topic10_final<- factor(Topic10_view$Topic10_final, levels=c("Y", "N"))
table(Topic10_view$Topic10_final)

Topic10_viewday <- df_topic %>%
  select(Topic10_final,views_per_day) 

Topic10_viewday <- Topic10_viewday[complete.cases(Topic10_viewday),]

Topic10_viewday$Topic10_final<- factor(Topic10_viewday$Topic10_final, levels=c("Y", "N"))
table(Topic10_viewday$Topic10_final)

Topic10_like <- df_topic %>%
  select(Topic10_final,like_count) 
Topic10_like <- Topic10_like[complete.cases(Topic10_like),]

Topic10_like$Topic10_final<- factor(Topic10_like$Topic10_final, levels=c("Y", "N"))
table(Topic10_like$Topic10_final)

Topic10_comment <- df_topic %>%
  select(Topic10_final,comment_count) 

Topic10_comment <- Topic10_comment[complete.cases(Topic10_comment),]

Topic10_comment$Topic10_final<- factor(Topic10_comment$Topic10_final, levels=c("Y", "N"))
table(Topic10_comment$Topic10_final)

Topic10_view_wilcox <- wilcox.test(view_count ~ Topic10_final, data = Topic10_view)
Topic10_view_wilcox 

Topic10_viewday_wilcox <- wilcox.test(views_per_day ~ Topic10_final, data = Topic10_viewday)
Topic10_viewday_wilcox 

Topic10_like_wilcox <- wilcox.test(like_count ~ Topic10_final, data = Topic10_like)
Topic10_like_wilcox 

Topic10_comment_wilcox <- wilcox.test(comment_count ~ Topic10_final, data = Topic10_comment)
Topic10_comment_wilcox 

#view#
Topic10_view_Y <- Topic10_view %>%
  subset(Topic10_final == "Y") %>%
  summarize(count = n(), median_view = median(view_count), Q1_view = quantile(view_count,0.25), Q3_view = quantile(view_count, 0.75), min = quantile(view_count,0),max = quantile(view_count,1))
Topic10_view_Y

Topic10_view_N <- Topic10_view%>%
  subset(Topic10_final == "N") %>%
  summarize(count = n(), median_view = median(view_count), Q1_view = quantile(view_count,0.25), Q3_view = quantile(view_count, 0.75), min = quantile(view_count,0),max = quantile(view_count,1))
Topic10_view_N

Topic10_view_sum <- rbind(Topic10_view_Y,Topic10_view_N)
Topic10_view_sum <- as.data.frame(Topic10_view_sum)

rownames(Topic10_view_sum) <- c("Topic10_view_y","Topic10_view_N")
Topic10_view_sum

#viewday#
Topic10_viewday_Y <- Topic10_viewday %>%
  subset(Topic10_final == "Y") %>%
  summarize(count = n(), median_viewday = median(views_per_day), Q1_viewday = quantile(views_per_day,0.25), Q3_viewday = quantile(views_per_day, 0.75), min = quantile(views_per_day,0),max = quantile(views_per_day,1))
Topic10_viewday_Y

Topic10_viewday_N <- Topic10_viewday%>%
  subset(Topic10_final == "N") %>%
  summarize(count = n(), median_viewday = median(views_per_day), Q1_viewday = quantile(views_per_day,0.25), Q3_viewday = quantile(views_per_day, 0.75), min = quantile(views_per_day,0),max = quantile(views_per_day,1))
Topic10_viewday_N

Topic10_viewday_sum <- rbind(Topic10_viewday_Y,Topic10_viewday_N)
Topic10_viewday_sum <- as.data.frame(Topic10_viewday_sum)

rownames(Topic10_viewday_sum) <- c("Topic10_viewday_y","Topic10_viewday_N")
Topic10_viewday_sum

#like#
Topic10_like_Y <- Topic10_like %>%
  subset(Topic10_final == "Y") %>%
  summarize(count = n(), median_like = median(like_count), Q1_like = quantile(like_count,0.25), Q3_like = quantile(like_count, 0.75), min = quantile(like_count,0),max = quantile(like_count,1))
Topic10_like_Y

Topic10_like_N <- Topic10_like%>%
  subset(Topic10_final == "N") %>%
  summarize(count = n(), median_like = median(like_count), Q1_like = quantile(like_count,0.25), Q3_like = quantile(like_count, 0.75), min = quantile(like_count,0),max = quantile(like_count,1))
Topic10_like_N

Topic10_like_sum <- rbind(Topic10_like_Y,Topic10_like_N)
Topic10_like_sum <- as.data.frame(Topic10_like_sum)

rownames(Topic10_like_sum) <- c("Topic10_like_y","Topic10_like_N")
Topic10_like_sum

#comment#
Topic10_comment_Y <- Topic10_comment %>%
  subset(Topic10_final == "Y") %>%
  summarize(count = n(), median_comment = median(comment_count), Q1_comment = quantile(comment_count,0.25), Q3_comment = quantile(comment_count, 0.75), min = quantile(comment_count,0),max = quantile(comment_count,1))
Topic10_comment_Y

Topic10_comment_N <- Topic10_comment%>%
  subset(Topic10_final == "N") %>%
  summarize(count = n(), median_comment = median(comment_count), Q1_comment = quantile(comment_count,0.25), Q3_comment = quantile(comment_count, 0.75), min = quantile(comment_count,0),max = quantile(comment_count,1))
Topic10_comment_N

Topic10_comment_sum <- rbind(Topic10_comment_Y,Topic10_comment_N)
Topic10_comment_sum <- as.data.frame(Topic10_comment_sum)

rownames(Topic10_comment_sum) <- c("Topic10_comment_y","Topic10_comment_N")
Topic10_comment_sum

# 6.11. Topic_11
table(df_topic$Topic11_final...54)

df_topic$Topic11_final <- df_topic$Topic11_final...54

table(df_topic$Topic11_final)

Topic11_view <- df_topic %>%
  select(Topic11_final,view_count) 
Topic11_view <- Topic11_view[complete.cases(Topic11_view ),]

table(Topic11_view$Topic11_final)

Topic11_view$Topic11_final<- factor(Topic11_view$Topic11_final, levels=c("Y", "N"))
table(Topic11_view$Topic11_final)

Topic11_viewday <- df_topic %>%
  select(Topic11_final,views_per_day) 

Topic11_viewday <- Topic11_viewday[complete.cases(Topic11_viewday),]

Topic11_viewday$Topic11_final<- factor(Topic11_viewday$Topic11_final, levels=c("Y", "N"))
table(Topic11_viewday$Topic11_final)

Topic11_like <- df_topic %>%
  select(Topic11_final,like_count) 
Topic11_like <- Topic11_like[complete.cases(Topic11_like),]

Topic11_like$Topic11_final<- factor(Topic11_like$Topic11_final, levels=c("Y", "N"))
table(Topic11_like$Topic11_final)

Topic11_comment <- df_topic %>%
  select(Topic11_final,comment_count) 

Topic11_comment <- Topic11_comment[complete.cases(Topic11_comment),]

Topic11_comment$Topic11_final<- factor(Topic11_comment$Topic11_final, levels=c("Y", "N"))
table(Topic11_comment$Topic11_final)

Topic11_view_wilcox <- wilcox.test(view_count ~ Topic11_final, data = Topic11_view)
Topic11_view_wilcox 

Topic11_viewday_wilcox <- wilcox.test(views_per_day ~ Topic11_final, data = Topic11_viewday)
Topic11_viewday_wilcox 

Topic11_like_wilcox <- wilcox.test(like_count ~ Topic11_final, data = Topic11_like)
Topic11_like_wilcox 

Topic11_comment_wilcox <- wilcox.test(comment_count ~ Topic11_final, data = Topic11_comment)
Topic11_comment_wilcox 

#view#
Topic11_view_Y <- Topic11_view %>%
  subset(Topic11_final == "Y") %>%
  summarize(count = n(), median_view = median(view_count), Q1_view = quantile(view_count,0.25), Q3_view = quantile(view_count, 0.75), min = quantile(view_count,0),max = quantile(view_count,1))
Topic11_view_Y

Topic11_view_N <- Topic11_view%>%
  subset(Topic11_final == "N") %>%
  summarize(count = n(), median_view = median(view_count), Q1_view = quantile(view_count,0.25), Q3_view = quantile(view_count, 0.75), min = quantile(view_count,0),max = quantile(view_count,1))
Topic11_view_N

Topic11_view_sum <- rbind(Topic11_view_Y,Topic11_view_N)
Topic11_view_sum <- as.data.frame(Topic11_view_sum)

rownames(Topic11_view_sum) <- c("Topic11_view_y","Topic11_view_N")
Topic11_view_sum

#viewday#
Topic11_viewday_Y <- Topic11_viewday %>%
  subset(Topic11_final == "Y") %>%
  summarize(count = n(), median_viewday = median(views_per_day), Q1_viewday = quantile(views_per_day,0.25), Q3_viewday = quantile(views_per_day, 0.75), min = quantile(views_per_day,0),max = quantile(views_per_day,1))
Topic11_viewday_Y

Topic11_viewday_N <- Topic11_viewday%>%
  subset(Topic11_final == "N") %>%
  summarize(count = n(), median_viewday = median(views_per_day), Q1_viewday = quantile(views_per_day,0.25), Q3_viewday = quantile(views_per_day, 0.75), min = quantile(views_per_day,0),max = quantile(views_per_day,1))
Topic11_viewday_N

Topic11_viewday_sum <- rbind(Topic11_viewday_Y,Topic11_viewday_N)
Topic11_viewday_sum <- as.data.frame(Topic11_viewday_sum)

rownames(Topic11_viewday_sum) <- c("Topic11_viewday_y","Topic11_viewday_N")
Topic11_viewday_sum

#like#
Topic11_like_Y <- Topic11_like %>%
  subset(Topic11_final == "Y") %>%
  summarize(count = n(), median_like = median(like_count), Q1_like = quantile(like_count,0.25), Q3_like = quantile(like_count, 0.75), min = quantile(like_count,0),max = quantile(like_count,1))
Topic11_like_Y

Topic11_like_N <- Topic11_like%>%
  subset(Topic11_final == "N") %>%
  summarize(count = n(), median_like = median(like_count), Q1_like = quantile(like_count,0.25), Q3_like = quantile(like_count, 0.75), min = quantile(like_count,0),max = quantile(like_count,1))
Topic11_like_N

Topic11_like_sum <- rbind(Topic11_like_Y,Topic11_like_N)
Topic11_like_sum <- as.data.frame(Topic11_like_sum)

rownames(Topic11_like_sum) <- c("Topic11_like_y","Topic11_like_N")
Topic11_like_sum

#comment#
Topic11_comment_Y <- Topic11_comment %>%
  subset(Topic11_final == "Y") %>%
  summarize(count = n(), median_comment = median(comment_count), Q1_comment = quantile(comment_count,0.25), Q3_comment = quantile(comment_count, 0.75), min = quantile(comment_count,0),max = quantile(comment_count,1))
Topic11_comment_Y

Topic11_comment_N <- Topic11_comment%>%
  subset(Topic11_final == "N") %>%
  summarize(count = n(), median_comment = median(comment_count), Q1_comment = quantile(comment_count,0.25), Q3_comment = quantile(comment_count, 0.75), min = quantile(comment_count,0),max = quantile(comment_count,1))
Topic11_comment_N

Topic11_comment_sum <- rbind(Topic11_comment_Y,Topic11_comment_N)
Topic11_comment_sum <- as.data.frame(Topic11_comment_sum)

rownames(Topic11_comment_sum) <- c("Topic11_comment_y","Topic11_comment_N")
Topic11_comment_sum

# 6.12. Topic_12
table(df_topic$Topic12_final...58)

df_topic$Topic12_final <- df_topic$Topic12_final...58

Topic12_view <- df_topic %>%
  select(Topic12_final,view_count) 
Topic12_view <- Topic12_view[complete.cases(Topic12_view ),]

table(Topic12_view$Topic12_final)

Topic12_view$Topic12_final<- factor(Topic12_view$Topic12_final, levels=c("Y", "N"))
table(Topic12_view$Topic12_final)

Topic12_viewday <- df_topic %>%
  select(Topic12_final,views_per_day) 

Topic12_viewday <- Topic12_viewday[complete.cases(Topic12_viewday),]

Topic12_viewday$Topic12_final<- factor(Topic12_viewday$Topic12_final, levels=c("Y", "N"))
table(Topic12_viewday$Topic12_final)


Topic12_like <- df_topic %>%
  select(Topic12_final,like_count) 
Topic12_like <- Topic12_like[complete.cases(Topic12_like),]

Topic12_like$Topic12_final<- factor(Topic12_like$Topic12_final, levels=c("Y", "N"))
table(Topic12_like$Topic12_final)

Topic12_comment <- df_topic %>%
  select(Topic12_final,comment_count) 

Topic12_comment <- Topic12_comment[complete.cases(Topic12_comment),]

Topic12_comment$Topic12_final<- factor(Topic12_comment$Topic12_final, levels=c("Y", "N"))
table(Topic12_comment$Topic12_final)

Topic12_view_wilcox <- wilcox.test(view_count ~ Topic12_final, data = Topic12_view)
Topic12_view_wilcox 

Topic12_viewday_wilcox <- wilcox.test(views_per_day ~ Topic12_final, data = Topic12_viewday)
Topic12_viewday_wilcox 

Topic12_like_wilcox <- wilcox.test(like_count ~ Topic12_final, data = Topic12_like)
Topic12_like_wilcox 

Topic12_comment_wilcox <- wilcox.test(comment_count ~ Topic12_final, data = Topic12_comment)
Topic12_comment_wilcox 

#view#
Topic12_view_Y <- Topic12_view %>%
  subset(Topic12_final == "Y") %>%
  summarize(count = n(), median_view = median(view_count), Q1_view = quantile(view_count,0.25), Q3_view = quantile(view_count, 0.75), min = quantile(view_count,0),max = quantile(view_count,1))
Topic12_view_Y

Topic12_view_N <- Topic12_view%>%
  subset(Topic12_final == "N") %>%
  summarize(count = n(), median_view = median(view_count), Q1_view = quantile(view_count,0.25), Q3_view = quantile(view_count, 0.75), min = quantile(view_count,0),max = quantile(view_count,1))
Topic12_view_N

Topic12_view_sum <- rbind(Topic12_view_Y,Topic12_view_N)
Topic12_view_sum <- as.data.frame(Topic12_view_sum)

rownames(Topic12_view_sum) <- c("Topic12_view_y","Topic12_view_N")
Topic12_view_sum

#viewday#
Topic12_viewday_Y <- Topic12_viewday %>%
  subset(Topic12_final == "Y") %>%
  summarize(count = n(), median_viewday = median(views_per_day), Q1_viewday = quantile(views_per_day,0.25), Q3_viewday = quantile(views_per_day, 0.75), min = quantile(views_per_day,0),max = quantile(views_per_day,1))
Topic12_viewday_Y

Topic12_viewday_N <- Topic12_viewday%>%
  subset(Topic12_final == "N") %>%
  summarize(count = n(), median_viewday = median(views_per_day), Q1_viewday = quantile(views_per_day,0.25), Q3_viewday = quantile(views_per_day, 0.75), min = quantile(views_per_day,0),max = quantile(views_per_day,1))
Topic12_viewday_N

Topic12_viewday_sum <- rbind(Topic12_viewday_Y,Topic12_viewday_N)
Topic12_viewday_sum <- as.data.frame(Topic12_viewday_sum)

rownames(Topic12_viewday_sum) <- c("Topic12_viewday_y","Topic12_viewday_N")
Topic12_viewday_sum

#like#
Topic12_like_Y <- Topic12_like %>%
  subset(Topic12_final == "Y") %>%
  summarize(count = n(), median_like = median(like_count), Q1_like = quantile(like_count,0.25), Q3_like = quantile(like_count, 0.75), min = quantile(like_count,0),max = quantile(like_count,1))
Topic12_like_Y

Topic12_like_N <- Topic12_like%>%
  subset(Topic12_final == "N") %>%
  summarize(count = n(), median_like = median(like_count), Q1_like = quantile(like_count,0.25), Q3_like = quantile(like_count, 0.75), min = quantile(like_count,0),max = quantile(like_count,1))
Topic12_like_N

Topic12_like_sum <- rbind(Topic12_like_Y,Topic12_like_N)
Topic12_like_sum <- as.data.frame(Topic12_like_sum)

rownames(Topic12_like_sum) <- c("Topic12_like_y","Topic12_like_N")
Topic12_like_sum

#comment#
Topic12_comment_Y <- Topic12_comment %>%
  subset(Topic12_final == "Y") %>%
  summarize(count = n(), median_comment = median(comment_count), Q1_comment = quantile(comment_count,0.25), Q3_comment = quantile(comment_count, 0.75), min = quantile(comment_count,0),max = quantile(comment_count,1))
Topic12_comment_Y

Topic12_comment_N <- Topic12_comment%>%
  subset(Topic12_final == "N") %>%
  summarize(count = n(), median_comment = median(comment_count), Q1_comment = quantile(comment_count,0.25), Q3_comment = quantile(comment_count, 0.75), min = quantile(comment_count,0),max = quantile(comment_count,1))
Topic12_comment_N

Topic12_comment_sum <- rbind(Topic12_comment_Y,Topic12_comment_N)
Topic12_comment_sum <- as.data.frame(Topic12_comment_sum)

rownames(Topic12_comment_sum) <- c("Topic12_comment_y","Topic12_comment_N")
Topic12_comment_sum

##Combine summary## 
#Views#
Topic_view_summary <- rbind(Topic1_view_sum,Topic2_view_sum,Topic3_view_sum,Topic4_view_sum,Topic5_view_sum,Topic6_view_sum,Topic7_view_sum,Topic8_view_sum,Topic9_view_sum,Topic10_view_sum,Topic11_view_sum,Topic12_view_sum)
Topic_view_summary

Topic_viewday_summary <- rbind(Topic1_viewday_sum,Topic2_viewday_sum,Topic3_viewday_sum,Topic4_viewday_sum,Topic5_viewday_sum,Topic6_viewday_sum,Topic7_viewday_sum,Topic8_viewday_sum,Topic9_viewday_sum,Topic10_viewday_sum,Topic11_viewday_sum,Topic12_viewday_sum)
Topic_viewday_summary

Topic_like_summary <- rbind(Topic1_like_sum,Topic2_like_sum,Topic3_like_sum,Topic4_like_sum,Topic5_like_sum,Topic6_like_sum,Topic7_like_sum,Topic8_like_sum,Topic9_like_sum,Topic10_like_sum,Topic11_like_sum,Topic12_like_sum)
Topic_like_summary

Topic_comment_summary <- rbind(Topic1_comment_sum,Topic2_comment_sum,Topic3_comment_sum,Topic4_comment_sum,Topic5_comment_sum,Topic6_comment_sum,Topic7_comment_sum,Topic8_comment_sum,Topic9_comment_sum,Topic10_comment_sum,Topic11_comment_sum,Topic12_comment_sum)
Topic_comment_summary

write.csv(Topic_view_summary,"", row.names = TRUE) #Filepath
write.csv(Topic_viewday_summary,"", row.names = TRUE) #Filepath
write.csv(Topic_like_summary,"", row.names = TRUE) #Filepath
write.csv(Topic_comment_summary,"", row.names = TRUE) #Filepath

# 6.13. Number of topics analysis 
df_topic_final <- df_topic %>%
  select(Topic1_final,Topic2_final,Topic3_final,Topic4_final,
         Topic5_final,Topic6_final,Topic7_final,Topic8_final,
         Topic9_final,Topic10_final,Topic11_final,Topic12_final)
df_topic_final$Y_count <- rowSums(df_topic_final == "Y")
table(df_topic_final$Y_count)

# 7.0 Engagement analytics
summary(df_final$view_count)
sum(df_final$view_count,na.rm =TRUE)
shapiro.test(df_final$view_count)

summary(df_final$views_per_day)
sum(df_final$views_per_day,na.rm =TRUE)
shapiro.test(df_final$views_per_day)

summary(df_final$like_count)
sum(df_final$like_count,na.rm =TRUE)
shapiro.test(df_final$like_count)

summary(df_final$comment_count,na.rm=TRUE)
sum(df_final$comment_count,na.rm=TRUE)
shapiro.test(df_final$comment_count)

# 7.1. Views_per_day vs. View count 
a1_sum_plot <- ggplot(df_final,aes(y = "", x = df_final$views_per_day)) + geom_boxplot() + scale_y_log10()
a1_sum_plot
a1_cor <- cor.test(df_final$views_per_day, df_final$view_count, method = "spearman", exact = FALSE)

print(a1_cor)
a1_cor$estimate

a1_plot = ggscatter(data = df_final, x = "views_per_day", y = "view_count")+ 
  geom_smooth(method = "lm", se = FALSE)+
  annotate("text",x = 0.5, y = 5000000,size = 8,
           label = paste("ρ = ", round(a1_cor$estimate,3)))+
  scale_y_log10(breaks = c(1000, 10000, 100000,1000000, 10000000), labels = expression(10^3, 10^4,10^5,10^6, 10^7)) + scale_x_log10(breaks = c(0.1, 1, 10, 100, 1000), labels = expression(0.1,1,10,10^2, 10^3)) +labs(x = "Views per day", y = "View count") + theme(
    axis.title.x =  element_text(size = 18),
    axis.title.y = element_text(size = 18),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18))

a1_plot

# 7.2. likes vs. views 
df_final$like_count <- as.numeric(df_final$like_count)
summary(df_final$like_count)

a2_sum_plot <- ggplot(df_final,aes(x = "", y = df_final$like_count)) + geom_boxplot() + scale_y_log10()
a2_sum_plot

a2_cor <- cor.test(df_final$view_count, df_final$like_count, method = "spearman",exact = FALSE)

print(a2_cor)
a2_plot = ggscatter(data = df_final, x = "like_count", y = "view_count") + 
  geom_smooth(method = "lm", se = FALSE)+
  annotate("text",x = 25, y = 5000000,size = 8,
           label = paste("ρ = ", round(a2_cor$estimate,3))
  ) + scale_y_log10(breaks = c(1000, 10000, 100000,1000000, 10000000), labels = expression(10^3, 10^4,10^5,10^6, 10^7)) + scale_x_log10(breaks = c(10,100,1000,10000, 100000), labels = expression(10,10^2,10^3,10^4, 10^5)) +labs(y = "View count", x = "Like count")+ theme(
    axis.title.x =  element_text(size = 18),
    axis.title.y = element_text(size = 18),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18))

a2_plot

# 7.3. comments vs. views 
df_final$comment_count <- as.numeric(df_final$comment_count)
summary(df_final$comment_count)

a3_sum_plot <- ggplot(df_final,aes(x = "", y = df_final$comment_count)) + geom_boxplot() + scale_y_log10()
a3_sum_plot

a3_cor <- cor.test(df_final$view_count,df_final$comment_count, method = "spearman",exact = FALSE)

a3_cor
a3_plot = ggscatter(data = df_final, x = "comment_count", y = "view_count") + 
  geom_smooth(method = "lm", se = FALSE)+
  annotate("text",x = 7, y = 5000000,size = 8, label = paste("ρ = ", round(a3_cor$estimate,3))) + scale_y_log10(breaks = c(1000, 10000, 100000,1000000, 10000000), labels = expression(10^3, 10^4,10^5,10^6, 10^7)) + scale_x_log10(breaks = c(1, 10, 100,1000, 10000), labels = expression(1, 10,10^2,10^3, 10^4))+labs(y = "View count", x = "Comment count")+ theme(
    axis.title.x =  element_text(size = 18),
    axis.title.y = element_text(size = 18),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18))
a3_plot

# 7.4. subscriber count vs. views 
df_final$subscriber_count_num <- as.numeric(df_final$subscriber_count_num)
summary(df_final$subscriber_count_num)

a4_sum_plot <- ggplot(df_final,aes(x = "", y = df_final$subscriber_count_num)) + geom_boxplot() + scale_y_log10()
a4_sum_plot

a4_cor <- cor.test(df_final$view_count,df_final$subscriber_count_num, method = "spearman",exact = FALSE)
a4_cor

a4_plot = ggscatter(data = df_final, x = "subscriber_count_num", y = "view_count") + 
  geom_smooth(method = "lm", se = FALSE)+
  annotate("text",x = 50, y = 5000000,size = 8,
           label = paste("ρ = ", round(a4_cor$estimate,3))
  ) + scale_y_log10(breaks = c(1000, 10000, 100000,1000000, 10000000), labels = expression(10^3, 10^4,10^5,10^6, 10^7)) + scale_x_log10(breaks = c(10, 1000, 100000,10000000), labels = expression(10, 10^3,10^5,10^7)) +labs(y = "View count", x = "Subscriber count")+ theme(
    axis.title.x =  element_text(size = 18),
    axis.title.y = element_text(size = 18),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18))
a4_plot

##Combine the plots##
a1_plot + a2_plot + a3_plot + a4_plot + plot_annotation(tag_levels ='I')

# 8.0. Video quality analysis ###############################

df_final$discern_total
# 8.1. DISCERN and channel types 
channel_type_discern <- df_final %>%
  group_by(channel_type_abr) %>%
  mutate(count=n()) %>%
  mutate(median = median(discern_total)) %>%
  ggplot(aes(y =discern_total, x =channel_type_abr))+
  geom_boxplot() + labs(x = "channel_type", y = "discern_total")

discern_channel_kruskal <- kruskal.test(discern_total ~ channel_type_abr, data = df_final)

discern_channel_dunn <- dunnTest(discern_total ~ channel_type_abr, data = df_final, method = "bonferroni")

install.packages("ggpubr")

df_final$discern_total

library(ggpubr)

channel_discern_plot <- df_final %>%
  group_by(channel_type_abr) %>%
  ggboxplot(x = "channel_type_abr", y= "discern_total",fill = "salmon")+ 
  labs(x = "Channel type", y = "DISCERN score")+
  annotate("text",x = 1.5, y = 45,size = 5, label = paste("p-value = ", round(discern_channel_kruskal$p.value,3))) +
  theme_classic2() + theme(
    axis.title.x =  element_text(size = 18),
    axis.title.y = element_text(size = 18),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 16))+
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  NULL

# 8.2. DISCERN by items 
df_final$discern_total <- rowSums(df_final[, c("discern_q1_f", "discern_q2_f", "discern_q3_f","discern_q4_f","discern_q5_f","discern_q6_f","discern_q7_f","discern_q8_f","discern_q9_f")])
summary(df_final$discern_total)

discern_item <- rbind(summary(df_final$discern_q1_f), summary(df_final$discern_q2_f), summary(df_final$discern_q3_f), summary(df_final$discern_q4_f), summary(df_final$discern_q5_f), summary(df_final$discern_q6_f), summary(df_final$discern_q7_f), summary(df_final$discern_q8_f), summary(df_final$discern_q9_f))

discern_item <- data.frame(discern_item)
discern_item
id <- c(1:9)
discern_item$Discern <- id
discern_item$Discern <- as.character(discern_item$Discern)

ggbarplot(discern_item, x = "Discern", y = "Mean", fill = "grey", ylab = "Median Score", xlab = "DISCERN items") + geom_errorbar(aes(ymin = "Mean" - se, ymax = "Mean" + se), width = 0.2)+ggtitle("DISCERN scores by item") + theme(plot.title = element_text(hjust = 0.5))

shapiro.test(df_final$discern_q1_f)

discern_q1 <- data.frame(score = df_final$discern_q1_f)
discern_q1

discern_q1_plot <- ggplot(discern_q1, aes(y = score)) +
  geom_boxplot(width = 1,fill = "skyblue", color = "black") +
  labs(x = "1.Clear aim",y = "Score") +theme_classic2() + scale_y_continuous(limits = c(1,5),breaks = c(1,2,3,4,5))+
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())+theme(
    axis.title.x =  element_text(size = 16),
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 18))+
  theme(axis.title.x = element_text(angle = 45, hjust = 0.5,vjust = 0.5))
discern_q1_plot

shapiro.test(df_final$discern_q2_f)

discern_q2 <- data.frame(score = df_final$discern_q2_f)
discern_q2

discern_q2_plot <- ggplot(discern_q2, aes(y = score)) +
  geom_boxplot(width = 0.3,fill = "skyblue", color = "black") +
  labs(x = "2.Achieve aim",y = "Score") +theme_classic2() + scale_y_continuous(limits = c(1,5),breaks = c(1,2,3,4,5)) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())+theme(
    axis.title.x =  element_text(size = 16),
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 18))+
  theme(axis.title.x = element_text(angle = 45, hjust = 0.5,vjust = 0.5))
discern_q2_plot

shapiro.test(df_final$discern_q3_f)

discern_q3 <- data.frame(score = df_final$discern_q3_f)
discern_q3

discern_q3_plot <- ggplot(discern_q3, aes(y = score)) +
  geom_boxplot(width = 0.3,fill = "skyblue", color = "black") +
  labs(x = "3.Relevant",y = "Score") + theme_classic2()+ scale_y_continuous(limits = c(1,5),breaks = c(1,2,3,4,5))+
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())+theme(
    axis.title.x =  element_text(size = 16),
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 18))+
  theme(axis.title.x = element_text(angle = 45, hjust = 0.5,vjust = 0.5))
discern_q3_plot

shapiro.test(df_final$discern_q4_f)

discern_q4 <- data.frame(score = df_final$discern_q4_f)
discern_q4

discern_q4_plot <- ggplot(discern_q4, aes(y = score)) +
  geom_boxplot(width = 0.3,fill = "skyblue", color = "black") +
  labs(x = "4.Info source",y = "Score") + theme_classic2()+ scale_y_continuous(limits = c(1,5),breaks = c(1,2,3,4,5))+
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())+theme(
    axis.title.x =  element_text(size = 16),
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 18))+
  theme(axis.title.x = element_text(angle = 45, hjust = 0.5,vjust = 0.5))
discern_q4_plot

shapiro.test(df_final$discern_q5_f)

discern_q5 <- data.frame(score = df_final$discern_q5_f)
discern_q5

discern_q5_plot <- ggplot(discern_q5, aes(y = score)) +
  geom_boxplot(width = 0.3,fill = "skyblue", color = "black") +
  labs(x = "5.Info date",y = "Score") + theme_classic2()+ scale_y_continuous(limits = c(1,5),breaks = c(1,2,3,4,5))+
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())+theme(
    axis.title.x =  element_text(size = 16),
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 18))+
  theme(axis.title.x = element_text(angle = 45, hjust = 0.5,vjust = 0.5))
discern_q5_plot

shapiro.test(df_final$discern_q6_f)

discern_q6 <- data.frame(score = df_final$discern_q6_f)
discern_q6
discern_q6_plot <- ggplot(discern_q6, aes(y = score)) +
  geom_boxplot(width = 0.3,fill = "skyblue", color = "black") +
  labs(x = "6.Unbiased",y = "Score")+ theme_classic2()+ scale_y_continuous(limits = c(1,5),breaks = c(1,2,3,4,5))+
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())+theme(
    axis.title.x =  element_text(size = 16),
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 18))+
  theme(axis.title.x = element_text(angle = 45, hjust = 0.5,vjust = 0.5))
discern_q6_plot

shapiro.test(df_final$discern_q7_f)

discern_q7 <- data.frame(score = df_final$discern_q7_f)
discern_q7
discern_q7_plot <- ggplot(discern_q7, aes(y = score)) +
  geom_boxplot(width = 0.3,fill = "skyblue", color = "black") +
  labs(x = "7.Additional sources",y = "Score") + theme_classic2()+ scale_y_continuous(limits = c(1,5),breaks = c(1,2,3,4,5))+
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())+theme(
    axis.title.x =  element_text(size = 16),
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 18))+
  theme(axis.title.x = element_text(angle = 45, hjust = 0.5,vjust = 0.5))
discern_q7_plot

shapiro.test(df_final$discern_q8_f)

discern_q8 <- data.frame(score = df_final$discern_q8_f)
discern_q8
discern_q8_plot <- ggplot(discern_q8, aes(y = score)) +
  geom_boxplot(width = 0.3,fill = "skyblue", color = "black") +
  labs(x = "8.Uncertainty",y = "Score") + theme_classic2()+ scale_y_continuous(limits = c(1,5),breaks = c(1,2,3,4,5))+
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())+theme(
    axis.title.x =  element_text(size = 16),
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 18))+
  theme(axis.title.x = element_text(angle = 45, hjust = 0.5,vjust = 0.5))
discern_q8_plot

shapiro.test(df_final$discern_total) ### Not normally distributed ###

discern_q9 <- data.frame(score = df_final$discern_q9_f)
discern_q9
discern_q9_plot <- ggplot(discern_q9, aes(y = score)) +
  geom_boxplot(width = 0.3,fill = "skyblue", color = "black") +
  labs(x = "9.Overall",y = "Score") + theme_classic2()+ scale_y_continuous(limits = c(1,5),breaks = c(1,2,3,4,5))+
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())+theme(
    axis.title.x =  element_text(size = 16),
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 18))+
  theme(axis.title.x = element_text(angle = 45, hjust = 0.5,vjust = 0.5))
discern_q9_plot

discern_q1_plot + discern_q2_plot + plot_layout(axes = "collect")+ plot_layout(axis_titles = "collect")


discern_q1_plot | discern_q2_plot | discern_q3_plot |discern_q4_plot | discern_q5_plot | discern_q6_plot | discern_q7_plot | discern_q8_plot | discern_q9_plot | plot_layout(axes = "collect")

# 8.0. Plotting heatmap of numerical variables
# 8.1. Numerical variables##
summary(df_final$duration_min)
sum(df_final$duration_min,na.rm =TRUE)
shapiro.test(df_final$duration_min)

summary(df_final$days_since_publication)
shapiro.test(df_final$days_since_publication)

summary(df_final$intro_length_s)
df_final$intro_length_s <- as.numeric(df_final$intro_length_s)
shapiro.test(df_final$intro_length_s)

summary(df_final$intro_length_s)
shapiro.test(df_final$intro_length_s)

df_interviewee$interviewee_n <- as.numeric(df_interviewee$interviewee_n)
summary(df_interviewee$interviewee_n)
shapiro.test(df_interviewee$interviewee_n)

df_numerical <- df_final %>%
  select(view_count, views_per_day,like_count,comment_count, subscriber_count_num,days_since_publication,duration_min,intro_length_s,discern_total)

glimpse(df_numerical)
colnames(df_numerical) <- c("View count","Views per day","Like count","Comment count","Subscriber count","Days since publication","Video duration","Intro duration","DISCERN score")

# 8.2. plot heatmaps#
library(corrplot)
cor_matrix <- cor(df_numerical,method = "spearman", use = "complete.obs")
plot <- corrplot(
  cor_matrix,
  method = "color",
  type = "upper",
  tl.cex = 0.5,
  tl.col = "black",
  tl.srt =45,
  addCoef.col = "black")


# 9.0. Interrater reliability calculation

# 9.1. Channel type 
cohen.kappa(x=cbind(df_final$channel_type,df_final$`Channel type_2`))

# 9.2. video style ###

cohen.kappa(x=cbind(df_final$video_style,df_final$video_style_reviewer_2))

# 9.3. Topics
df_topic

#Combined Topic#
Reviewer1_topic <- c(df_topic$`Topic_1: Antibiotics and mechanisms of action`,df_topic$`Topic_2:Mechanisms of AMR`,df_topic$`Topic_3:Antimicrobial usage and over-prescription`,df_topic$`Topic_4:Agricultural and environmental factors linked with AMR`,df_topic$`Topic_5:AMR patient experiences/cases`,df_topic$`Topic_6: Drug development in antibiotics and AMR`,df_topic$`Topic_7: Novel research in AMR (e.g. phage therapy, novel diagnostics)`,df_topic$`Topic_8: Actions against AMR`, df_topic$`Topic_9: Governmental actions/Legislations`,df_topic$`Topic_10: Severity of AMR (epidemiology; consequences without)`, df_topic$`Topic_11: Interviews with AMR-related experts`, df_topic$`Topic_12: AMR-related laboratory experiments`)
Reviewer2_topic <- c(df_topic$Topic2_1, df_topic$Topic2_2, df_topic$Topic2_3, df_topic$Topic2_4,df_topic$Topic2_5, df_topic$Topic2_6, df_topic$Topic7_1, df_topic$Topic2_8, df_topic$Topic9_1, df_topic$Topic10_1, df_topic$Topic2_11...52, df_topic$Topic2_12...56)

overall_topic_irr <- cohen.kappa(x=cbind(Reviewer1_topic, Reviewer2_topic))

# 9.4. DISCERN
df_discern

#Combined Items#
reviewer1_discern <- c(df_discern$q1_convert1,df_discern$q2_convert, df_discern$q3_convert,df_discern$q4_convert,
                       df_discern$q5_convert,df_discern$q6_convert,df_discern$q7_convert,df_discern$q8_convert,df_discern$q9_convert)
reviewer2_discern <- c(df_discern$q1_convert2,df_discern$q2_convert2,df_discern$q3_convert2,df_discern$q4_convert2,
                       df_discern$q5_convert1,df_discern$q6_convert2,df_discern$q7_convert2,df_discern$q8_convert2,
                       df_discern$q9_convert2)
overall_discern_irr <- cohen.kappa(x=cbind(reviewer1_discern,reviewer2_discern))
overall_discern_irr


