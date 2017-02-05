# docker run -d -p 5901:5900 -p 127.0.0.1:4445:4444 selenium/standalone-firefox-debug:2.53.1
# then open vinagra at port 127.0.0.1:5901 for VCN method
library(RSelenium)
library(stringi)
remDr <- remoteDriver(port = 4445L)
remDr$open(silent = TRUE)

# NOW LOG IN MANUALLY IN VIAGRA

# I do not assign remDr$findElements as I had troubles with http://www.seleniumhq.org/exceptions/stale_element_reference.jsp

whyR_meetup <- function(group_url = "https://www.meetup.com/Spotkania-Entuzjastow-R-Warsaw-R-Users-Group-Meetup"){
  
  past <- file.path(group_url, 'events', 'past')
  remDr$navigate(past)
  
  what_can_be_clicked <-
    unlist(
      lapply(remDr$findElements("class name", "page-meetups"), function(driverElement){
        driverElement$getElementText()
      })
    )
  
  cat("Listing past events...", "\n")
  while("More Meetups" %in% what_can_be_clicked) {
    remDr$findElements("class name", "page-meetups")[[which("More Meetups" == what_can_be_clicked)]]$clickElement()
    Sys.sleep(3)
    what_can_be_clicked <-
    unlist(
      lapply(remDr$findElements("class name", "page-meetups"), function(driverElement){
        driverElement$getElementText()
      }))
    
    Sys.sleep(3)
  }
  cat("Finished listing. Starting data collection...", "\n")
  cat(length(remDr$findElements("class name", "past")), " past events ...", "\n")
  #events <- remDr$findElements("class name", "past")
  meetup_info <- list()
  for(event in seq_along(remDr$findElements("class name", "past"))){
    date <- remDr$findElements("class name", "past")[[event]]$findChildElement("class name", "row-item")$getElementText()[[1]]
    date <- strsplit(date, split = " · ")[[1]][1]
    
    title <- remDr$findElements("class name", "past")[[event]]$findChildElement("class name", "event-title")$getElementText()[[1]]
    
    users <- remDr$findElements("class name", "past")[[event]]$findChildElement("class name", "event-rating")$getElementText()[[1]]
    users <- stri_extract_all_regex(
      strsplit(users,
               split = "|", fixed = TRUE)[[1]][1],
      "[0-9]+")[[1]]
    cat(title, users, date, "\n")
    meetup_info[[event]] <- data.frame(date = date, title = title, 
                                       users = as.numeric(as.character(users)),
                                       stringsAsFactors = FALSE)
  }
  
  # # upcoming
  # cat("Going to upcoming events...", "\n")
  # cat(length(remDr$findElements("class name", "event-list")), " upcoming events ...", "\n")
  # remDr$navigate(file.path(group_url, "#upcoming"))
  # Sys.sleep(3)
  # for(event in seq_along(remDr$findElements("class name", "event-list"))) {
  #   date <- remDr$findElements("class name", "event-list")[[event]]$findChildElement("class name", "date")$getElementText()[[1]]
  #   date <- strsplit(date, split = " · ")[[1]][1]
  #   
  #   title <- remDr$findElements("class name", "event-list")[[event]]$findChildElement("class name", "hoverLink")$getElementText()[[1]]
  #   
  #   users <- remDr$findElements("class name", "event-list")[[event]]$findChildElement("class name", "unlink")$getElementText()[[1]]
  #   users <- stri_extract_all_regex(
  #     strsplit(users,
  #              split = "|", fixed = TRUE)[[1]][1],
  #     "[0-9]+")[[1]]
  #   cat(title, users, date, "\n")
  #   meetup_info[[length(meetup_info)+1]] <- data.frame(date = date, title = title, 
  #                                      users = as.numeric(as.character(users)),
  #                                      stringsAsFactors = FALSE)
  # }
  
  do.call(rbind,meetup_info)
}

Warsaw <- whyR_meetup()
Cracow <- whyR_meetup("https://www.meetup.com/Cracow-R-User-Group")
Poznan <- whyR_meetup("https://www.meetup.com/Poznan-R-User-Group-PAZUR")
Wroclaw <- whyR_meetup("https://www.meetup.com/Wroclaw-R-Users-Group")
Trojmiasto <- whyR_meetup("https://www.meetup.com/Trojmiejska-Grupa-Entuzjastow-R/")

Poznan[1,1] <- "Feb 2, 2017"

rbind(cbind(Warsaw, city = "Warsaw", label = NA),
      cbind(Cracow, city = "Cracow", label = NA),
      cbind(Poznan, city = "Poznań", label = NA),
      cbind(Wroclaw, city = "Wrocław", label = NA),
      cbind(Trojmiasto, city = "Tri-City", label = NA),
      data.frame(date = "Oct 13, 2016",
                 title = "European R Users Meeting",
                 users = 286, city = "Poznań", label = "eRum 2016"),
      data.frame(date = "Oct 15, 2014",
                 title = "Polski Akademicki Zlot Użytkowników R",
                 users = 100, city = "Poznań", label = "PAZUR 2014"),
      data.frame(date = "Sep 28, 2017",
                 title = "Why R?",
                 users = 200, city = "Warsaw", label = "Why R? 2017"),
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
p <- ggplot(whyR, aes(x = date, y = users, col = city)) + 
  #geom_bar(stat="identity", position="dodge") +
  geom_point() +
  xlab(paste0("State for ", Sys.Date())) +
  labs(title = "R Users Meetings in Poland",
       subtitle = "Since last polish R confernce in 2014 till the next one in Warsaw, September 2017",
       caption = "code: github.com/whyR-conference/meetup-harvesting") +
  ylab("Registered Attendees") +
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

png("whyr.png", width = 784, height = 250)
plot(p)
dev.off()

pdf("whyr.pdf", 14, 4)
plot(p)
dev.off()
  

Sys.setlocale("LC_TIME", lct)

write.csv(whyR, file = "whyR.csv", quote = FALSE, row.names = FALSE)
