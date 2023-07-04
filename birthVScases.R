library(ggplot2)
library(dplyr)
library(lubridate)

#Converting numbers to corresponding month
birth8 <- read.csv("2018-birth.csv")

birth8$Month = recode(birth8$Month, `1`="January",
                      `2`="February",
                      `3`="March",
                      `4`="April",
                      `5`="May",
                      `6`="June",
                      `7`="July",
                      `8`="August",
                      `9`="September",
                      `10`="October",
                      `11`="November",
                      `12`="December")

birth9 <- read.csv("2019-birth.csv")

birth9$Month = recode(birth9$Month, `1`="January",
                      `2`="February",
                      `3`="March",
                      `4`="April",
                      `5`="May",
                      `6`="June",
                      `7`="July",
                      `8`="August",
                      `9`="September",
                      `10`="October",
                      `11`="November",
                      `12`="December")

birth2 <- read.csv("2020-birth.csv")

birth2$Month = recode(birth2$Month, `1`="January",
                      `2`="February",
                      `3`="March",
                      `4`="April",
                      `5`="May",
                      `6`="June",
                      `7`="July",
                      `8`="August",
                      `9`="September",
                      `10`="October",
                      `11`="November",
                      `12`="December")

birth21 <- read.csv("2021-birth.csv")

birth21$Month = recode(birth21$Month, `1`="January",
                       `2`="February",
                       `3`="March",
                       `4`="April",
                       `5`="May",
                       `6`="June",
                       `7`="July",
                       `8`="August",
                       `9`="September",
                       `10`="October",
                       `11`="November",
                       `12`="December")

#Summarizing the total birth in the Philippines
birth8 <- birth8 %>%
  group_by(Month) %>%
  summarize(Total.Birth=sum(Total.Birth)) %>%
  mutate(i = match(Month, month.name))

a <- sum(birth8[, 'Total.Birth'], na.rm = TRUE)

birth9 <- birth9 %>%
  group_by(Month) %>%
  summarize(Total.Birth=sum(Total.Birth)) %>%
  mutate(i = match(Month, month.name))

b <- sum(birth9[, 'Total.Birth'], na.rm = TRUE)

birth2 <- birth2 %>%
  group_by(Month) %>%
  summarize(Total.Birth=sum(Total.Birth)) %>%
  mutate(i = match(Month, month.name))

c <- sum(birth2[, 'Total.Birth'], na.rm = TRUE)

birth21 <- birth21 %>%
  group_by(Month) %>%
  summarize(Total.Birth=sum(Total.Birth)) %>%
  mutate(i = match(Month, month.name))

d <- sum(birth21[, 'Total.Birth'], na.rm = TRUE)

#Creating a barplot to visualize the data
df_ <- data.frame(births=c(a, b, c, d), year=as.factor(2018:2021))
ggplot(df_, aes(year, births, fill=year)) +
  geom_bar(stat="identity")

##########################################################################################################

#filtering the Covid-19 data
df <- read.csv("WHO-COVID-19-global-data.csv")
colnames(df) <- c("date", "code", "country", "WHO_region", "new_cases", "cum_cases", "new_deaths", "cum_deaths")

df <- df %>% filter(code=="PH") %>%
  filter(as.Date(date, format="%Y-%m-%d") < as.Date("2021-01-01", format="%Y-%m-%d")) %>% 
  mutate(date=as.Date(date, format="%Y-%m-%d"), group=format(date, "%Y-%m")) %>%
  group_by(group) %>%
  summarise(total=sum(new_cases)) %>% 
  mutate(date=as.Date(paste(as.factor(group), "01", sep="-"), format="%Y-%m-%d"))

#filtering the month and year of covid cases
birth20 <- read.csv("2020-birth.csv") %>% 
  mutate(i=match(Month, month.name)) %>% 
  top_n(3, i) %>% 
  mutate(date2=as.Date(paste("2020-", i, "-01", sep=""), format="%Y-%m-%d"))
birth21 <- read.csv("2021-birth.csv") %>% 
  mutate(i=match(Month, month.name)) %>% 
  top_n(9, -i) %>% 
  mutate(date2=as.Date(paste("2021-", i, "-01", sep=""), format="%Y-%m-%d"))

z <- rbind(birth20, birth21)

#plotting the data base on their date
ggplot() +
  geom_point(data=df, aes(date, total), color="red") +
  geom_point(data=z, aes(date2, Total.Birth), color="blue") +
  geom_smooth(data=df, aes(date, total), se=FALSE, color="red") + 
  geom_smooth(data=z, aes(date2, Total.Birth), se=FALSE, color="blue")

#combining both data to better visualize it
ggplot(cbind(df, z)) +
  geom_point(aes(date, total), color="red") +
  geom_point(aes(date, Total.Birth), color="blue") +
  geom_smooth(aes(date, total), se=FALSE, color="red") + 
  geom_smooth(aes(date, Total.Birth), se=FALSE, color="blue") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())