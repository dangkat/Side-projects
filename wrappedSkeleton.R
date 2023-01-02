################## load libraries ########################
library(dplyr)
library(tidyverse)
library(lubridate)
library(jsonlite)
library(scales)
library(ggrepel)

################## data preparation ########################
# replace ... with specific data directory
# make sure to replace all backslashes (\) with forward slashes (/)
setwd("...")

# create vector of streaming history files - modify based on largest number of streaminghistory JSON you have 
stream <- paste0(rep("StreamingHistory"), 
       rep(0: #input number here
             ), ".json")

# merge files
streamHistory = do.call("rbind", lapply(stream, function(f) fromJSON(f)))

# create tibble of additional time variables
spotify = streamHistory %>% 
  as_tibble() %>% 
  mutate_at("endTime", ymd_hm) %>% 
  mutate(endTime = endTime - hours(6)) %>% 
  mutate(date = floor_date(endTime, "day") %>% 
  as_date, seconds = msPlayed / 1000, minutes = seconds / 60, hours = minutes / 60) %>%
  mutate(month = format(date, "%B")) 

# check first row
spotify %>% head(n=1)

################## plots ########################
# top songs by month
# create vector of month names
order = c('January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December')

mostPlayedTrack <- function(currentYear) {
  firstDayofYear <- paste0(currentYear, "/01/01")
  
  months = spotify %>% filter(date >= firstDayofYear) %>%
    group_by(trackName, month) %>%
    summarize(totalHour = sum(hours), .groups = 'drop') %>%
    group_by(month) %>%
    slice_max(totalHour, n = 1) %>%
    mutate(month = factor(month, levels = order))
  
  options(repr.plot.res = 300, repr.plot.height = 5, repr.plot.width = 7)
  ggplot(months, aes(x = month, y = totalHour)) + geom_point(color = "seagreen3") +
    geom_text_repel(data = months, size= 3, aes(label= trackName), 
                    fontface = 'bold', force= 1, point.padding = unit(4, 'lines')) +
    labs(x= "Month", y= "Hours of music playback", 
         title = paste0("Most played track each month\n(January-November ", currentYear, ")")) + #change month based on data
    theme_grey() + theme(axis.text.x = element_text(angle = 45,  hjust= 1), 
                         plot.title = element_text(hjust = 0.5))
}
mostPlayedTrack()

# streaming trend over the months
streamingTrend <- function(currentYear){
  
  firstDayofYear <- paste0(currentYear, "/01/01")
  
  streamingHours = spotify %>% filter(date >= firstDayofYear) %>%
    group_by(date) %>% 
    group_by(date = floor_date(date, "week")) %>%
    summarize(hours = sum(minutes) / 60, minutes= sum(seconds)/60) %>% 
    arrange(date)
  
  ggplot(streamingHours, aes(x = date, y = hours, color= minutes)) + geom_line() +
    labs(x= "Month", y= "Hours of music playback", 
         title = paste("Playback activity per week over the months \n(January-November", currentYear, ")"), #change month based on data
         color= "Minutes") +
    scale_color_continuous(labels = comma) + scale_x_date(date_labels = "%b", date_breaks = "1 month") +
    theme_minimal() + theme(plot.title = element_text(hjust = 0.5))
}
streamingTrend()

# heatmap 
heatMap <- function(){
  hoursDay = spotify %>% 
    group_by(date, hour = hour(endTime), weekday = wday(date, label = TRUE))%>% 
    summarize(minutesListened = sum(minutes))
  
  hoursDay_df = hoursDay %>% 
    group_by(weekday, hour) %>% 
    summarize(minutes = sum(minutesListened)) 
  
  ggplot(hoursDay_df, aes(x = hour, weekday, fill= minutes)) + geom_tile() +
    scale_fill_gradient(low = "yellow", high = "red") + 
    labs(x= "Time of the day (Hour 0-24)", y= "Weekday", 
         title= "What day of the week and time of day \ndo I listen to music the most on Spotify?", fill = "Minutes") +
    scale_x_continuous(breaks = c(0,4,8,12,16,20,24)) + theme_classic() + 
    theme(plot.title = element_text(hjust = 0.5))
}
heatMap()

# top artists
topArtists <- function(numberArtists=10){
  artist = spotify %>% 
    group_by(artistName) %>% 
    summarize(minutesListened = sum(minutes), hoursListened = sum(hours)) %>% 
    slice_max(order_by = minutesListened, n= numberArtists)
  
  ggplot(artist, aes(x = reorder(artistName, desc(minutesListened)), y = minutesListened)) + geom_col(fill= "hotpink1") +
    labs(x= "Artist", y= "Minutes of music playback", 
               title = paste0("My top ", numberArtists, " most streamed artists on Spotify")) + 
    scale_y_continuous(labels = comma) + theme_bw() + theme(axis.text.x = element_text(angle = 45,  hjust= 1))
}
topArtists()

# top songs by specific artist

topSongsbyArtist <- function(artist_name){
  
  # total number of minutes you listened to this artist
  totalMinbyArtist <- spotify %>% filter(artistName == artist_name) %>% 
                      summarize(totalArtist = sum(minutes))
  # top songs by this artist
  artistSong = spotify %>%
    filter(artistName == artist_name) %>%
    group_by(trackName) %>%
    summarize(minutes = sum(minutes), .groups = 'drop') %>%
    arrange(minutes) %>%
    slice_max(minutes, n = 10) %>%
    mutate(trackName = fct_inorder(trackName))
  
  print(totalMinbyArtist)
  
  ggplot(artistSong, aes(x=trackName, y= minutes)) + geom_col(fill='darkolivegreen3') + 
    theme_light() + theme(axis.text.x = element_text(angle = 45,  hjust= 1)) +
    scale_y_continuous(labels = comma) + labs(x= "Track name", y= "Minutes of music playback", 
               title = paste("My top 10 most listened tracks by", artist_name))
}
# replace ... with artist name 
topSongsbyArtist("...")


totalMin <- function(currentYear){
    firstDayofYear <- paste0(currentYear, "/01/01")
    
    totalMin <- spotify %>% filter(date >= firstDayofYear) %>% 
              summarize(totalMinutes=round(sum(minutes),0), percentage=totalMinutes/525600,
                        percent = round(percentage*100, 0))
    paste0("You listened to ", totalMin$totalMinutes, " minutes of music. That's ",
          totalMin$percent, "% of your year spent bumping to some bangers!")

}
totalMin()