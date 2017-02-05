# docker run -d -p 5901:5900 -p 127.0.0.1:4445:4444 selenium/standalone-firefox-debug:2.53.1
# then open vinagra at port 127.0.0.1:5901 for VCN method
library(RSelenium)
library(stringi)
remDr <- remoteDriver(port = 4445L)
remDr$open(silent = TRUE)

whyR_meetup <- function(group_url = "https://www.meetup.com/Spotkania-Entuzjastow-R-Warsaw-R-Users-Group-Meetup/events/past",
                        n_unroll = 3){
  
  remDr$navigate(group_url)
  for(i in 1:n_unroll){
    remDr$findElements("class name", "page-meetups")[[3]]$clickElement()
    Sys.sleep(1)
  }
  
  events <- remDr$findElements("class name", "past")
  
  meetup_info <- list()
  for(event in seq_along(events)){
    date <- events[[event]]$findChildElement("class name", "row-item")$getElementText()[[1]]
    date <- strsplit(date, split = " · ")[[1]][1]
    
    title <- events[[event]]$findChildElement("class name", "event-title")$getElementText()[[1]]
    
    users <- events[[event]]$findChildElement("class name", "event-rating")$getElementText()[[1]]
    users <- stri_extract_all_regex(
      strsplit(users,
               split = "|", fixed = TRUE)[[1]][1],
      "[0-9]+")[[1]]
    
    meetup_info[[event]] <- data.frame(date = date, title = title, 
                                       users = as.numeric(as.character(users)),
                                       stringsAsFactors = FALSE)
  }
  do.call(rbind,meetup_info)
}

Warsaw <- whyR_meetup()
Cracow <- whyR_meetup("https://www.meetup.com/Cracow-R-User-Group/events/past", 2)
Poznan <- whyR_meetup("https://www.meetup.com/Poznan-R-User-Group-PAZUR/events/past", 1)
Wroclaw <- whyR_meetup("https://www.meetup.com/Data-Science-Wroclaw/events/past", 3)
Lodz <- whyR_meetup("https://www.meetup.com/Data-Science-%C5%81od%C5%BA/events/past", 1)

rbind(cbind(Warsaw, city = "Warsaw", label = NA),
      cbind(Cracow, city = "Cracow", label = NA),
      cbind(Poznan, city = "Poznań", label = NA),
      cbind(Wroclaw, city = "Wrocław", label = NA),
      cbind(Lodz, city = "Lódź", label = NA),
      data.frame(date = "Oct 13, 2016",
                 title = "European R Users Meeting",
                 users = 286, city = "Poznań", label = "eRum 2016"),
      data.frame(date = "Oct 15, 2014",
                 title = "Polski Akademicki Zlot Użytkowników R",
                 users = 100, city = "Poznań", label = "PAZUR 2014"),
      data.frame(date = "Sep 28, 2017",
                 title = "Why R?",
                 users = 200, city = "Warsaw", label = "Why R? 2017"),
      data.frame(date = "Jan 12, 2017",
                 title = "meet(R) in Tricity!",
                 users = 14, city = "Trójmiasto", label = "upcoming"),
      data.frame(date = "Nov 30, 2016",
                 title = "Rzeszów 1",
                 users = 30, city = "Rzeszów", label = "no meetup")) -> whyR

whyR$date <- ifelse(grepl(",", whyR$date), # there was a year
                    whyR$date, # only convert to character
                    paste0(substr(unlist(lapply(strsplit(whyR$date, split = " ", fixed = TRUE), `[`, 1)), 1,3), # take 3 signs of a month
                           " ",   unlist(lapply(strsplit(whyR$date, split = " ", fixed = TRUE), `[`, 2)), # take day
                           ", ", # add colon
                           lubridate::year(Sys.Date()))) # add year
                    
lct <- Sys.getlocale("LC_TIME"); Sys.setlocale("LC_TIME", "C")
whyR$date <- as.Date(whyR$date, "%b %d, %Y")

levels(whyR$city) <- paste(levels(whyR$city), " (",table(whyR$city), ")", sep = "")


library(ggplot2)
library(ggthemes)

png("title.png", width = 784, height = 250)
ggplot(whyR, aes(x = date, y = users, col = city)) + 
  #geom_bar(stat="identity", position="dodge") +
  geom_point() +
  xlab("") +
  labs(title = "R Users Meetings in Poland",
       subtitle = "Since last polish R confernce in 2014 till the next one in Warsaw, September 2017",
       caption = "code: github.com/whyR-conference/meetup-harvesting") +
  ylab("Attendees") +
  theme_hc(bgcolor = "darkunica") + scale_fill_hc() +
  theme(legend.position = "top", legend.title = element_blank(), legend.box = "horizontal",
        axis.text = element_text(color = "gray")) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") + 
  guides(colour = guide_legend(nrow = 1)) +
  #geom_text_repel(aes(label = label))
  #+  coord_cartesian(ylim = c(0,200))
  annotate("text", x = c(min(whyR$date)+15, max(whyR$date)-15), colour = "#FFFFFF", size = 4,
                          y = c(115, 215), label = c("PAZUR 2014", "Why R? 2017")) +
  annotate("text", x = c(as.Date("2016-10-12")), colour = "#FFFFFF", size = 4,
                          y = c(270), label = c("eRum 2016")) +
  annotate("text", x = c(min(whyR$date)+30, max(whyR$date)-25), colour = "#FFFFFF", size = 3,
                          y = c(85, 180), label = c("Last polish R conference",
                                                    "Next polish R conference")) +
  annotate("text", x = c(as.Date("2017-09-20")), colour = "#FFFFFF", size = 6,
                          y = c(150), label = c("whyr.pl")) 
dev.off()
  

Sys.setlocale("LC_TIME", lct)
