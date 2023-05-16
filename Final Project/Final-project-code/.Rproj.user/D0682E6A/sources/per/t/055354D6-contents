#Web Scraping
library(rvest)

uniDetails <- read_html("https://cwur.org/2022-23.php")

rank <- html_text(html_nodes(uniDetails,"td:nth-child(1)"))
rank

institution <- html_text(html_nodes(uniDetails,"#cwurTable a:nth-child(1)"))
institution

location <- html_text(html_nodes(uniDetails,"td:nth-child(3)"))
location

national_rank <- html_text(html_nodes(uniDetails,"td:nth-child(4)"))
national_rank

edu_rank<- html_text(html_nodes(uniDetails,"td:nth-child(5)"))
edu_rank

emp_rank <- html_text(html_nodes(uniDetails,"td:nth-child(6)"))
emp_rank

faculty_rank <- html_text(html_nodes(uniDetails,"td:nth-child(7)"))
faculty_rank

research_rank <- html_text(html_nodes(uniDetails,"td:nth-child(8)"))
research_rank

score <- html_text(html_nodes(uniDetails,"td:nth-child(9)"))
score

#Creating Dataframe

TopUniversityDetails <- data.frame(rank,institution,location, national_rank, edu_rank, emp_rank, faculty_rank, research_rank, score)
TopUniversityDetails

#Export the dataframe
write.csv(TopUniversityDetails, "D:\\final-project\\TopUniversityDetails.csv", row.names = FALSE)


#Handling missing data
#data reduction

#data discretization

TopUniversityDetails1 <- TopUniversityDetails

TopUniversityDetails1$rank <- as.numeric(TopUniversityDetails1$rank)

TopUniversityDetails1$national_rank <- as.numeric(TopUniversityDetails1$national_rank)
TopUniversityDetails1$national_rank <- ifelse(is.na(TopUniversityDetails1$national_rank), mean(TopUniversityDetails1$national_rank, na.rm = TRUE),TopUniversityDetails1$national_rank)
TopUniversityDetails1$national_rank <- as.numeric(format(round(TopUniversityDetails1$national_rank, 0)))
View(TopUniversityDetails1)

TopUniversityDetails1$edu_rank <- as.numeric(TopUniversityDetails1$edu_rank)
TopUniversityDetails1$edu_rank <- ifelse(is.na(TopUniversityDetails1$edu_rank), mean(TopUniversityDetails1$edu_rank, na.rm = TRUE),TopUniversityDetails1$edu_rank)
TopUniversityDetails1$edu_rank <- as.numeric(format(round(TopUniversityDetails1$edu_rank, 0)))
View(TopUniversityDetails1)

TopUniversityDetails1$emp_rank <- as.numeric(TopUniversityDetails1$emp_rank)
TopUniversityDetails1$emp_rank <- ifelse(is.na(TopUniversityDetails1$emp_rank), mean(TopUniversityDetails1$emp_rank, na.rm = TRUE),TopUniversityDetails1$emp_rank)
TopUniversityDetails1$emp_rank <- as.numeric(format(round(TopUniversityDetails1$emp_rank, 0)))
View(TopUniversityDetails1)

TopUniversityDetails1$faculty_rank <- as.numeric(TopUniversityDetails1$faculty_rank)
TopUniversityDetails1$faculty_rank <- ifelse(is.na(TopUniversityDetails1$faculty_rank), mean(TopUniversityDetails1$faculty_rank, na.rm = TRUE),TopUniversityDetails1$faculty_rank)
TopUniversityDetails1$faculty_rank <- as.numeric(format(round(TopUniversityDetails1$faculty_rank, 0)))
View(TopUniversityDetails1)

TopUniversityDetails1$research_rank <- as.numeric(TopUniversityDetails1$research_rank)
TopUniversityDetails1$research_rank <- ifelse(is.na(TopUniversityDetails1$research_rank), mean(TopUniversityDetails1$research_rank, na.rm = TRUE),TopUniversityDetails1$research_rank)
TopUniversityDetails1$research_rank <- as.numeric(format(round(TopUniversityDetails1$research_rank, 0)))
View(TopUniversityDetails1)

TopUniversityDetails1$score <- as.numeric(TopUniversityDetails1$score)
TopUniversityDetails1$score <- ifelse(is.na(TopUniversityDetails1$score), mean(TopUniversityDetails1$score, na.rm = TRUE),TopUniversityDetails1$score)
TopUniversityDetails1$score <- as.numeric(format(round(TopUniversityDetails1$score, 0)))
View(TopUniversityDetails1)

TopUniversityDetails1$research_rank <- as.numeric(TopUniversityDetails1$research_rank)
quantile(TopUniversityDetails1$research_rank)


TopUniversityDetails1$research_rank<- with(TopUniversityDetails1, ifelse(TopUniversityDetails1$research_rank < 50, 'high level', 
                                          ifelse(TopUniversityDetails1$research_rank< 60, 'average level', 
                                          ifelse(TopUniversityDetails1$research_rank < 70, 'low level', 'lowest level'))))


#Export the dataframe
write.csv(TopUniversityDetails1, "D:\\final-project\\TopUniversityDetails1.csv", row.names = FALSE)


#Descriptive Statistics:
#Mean:

mean(TopUniversityDetails1$score)


#Median:

median(TopUniversityDetails1$score)


colnames(TopUniversityDetails1)[9] = "score"



#Range:



max(TopUniversityDetails1$score)- min(TopUniversityDetails1$score)


#variance

var(TopUniversityDetails1$score)

#standard deviation
sd(TopUniversityDetails1$score)

#Quantile:

quantile(TopUniversityDetails1$score)


#mode
mode <- function(x) 
{
  unique_values <- unique(x)
  table <- tabulate(match(x, unique_values))
  unique_values[table == max(table)]
  
}

mode(TopUniversityDetails1$score)

#graph plot

library(ggplot2)

plot1 <- ggplot(data = TopUniversityDetails1, mapping = aes(x = edu_rank, y = national_rank)) +
  geom_point(alpha = .7) +
  geom_smooth(formula = y ~ x, method = "lm", se = FALSE) + 
  scale_x_continuous(breaks = seq(0, 600, 50)) +
  scale_y_continuous(breaks = seq(0, 400, 20))
ggsave(file = "plot1.png", plot = plot1, height=20, width=20, units=c("cm"))


plot2 <- ggplot(TopUniversityDetails1, mapping = aes(x = location)) +
  geom_bar(fill = "red", bw = .8) +
  labs(title = "Location vs University Count", y = "University Count", x = "Location")

ggsave(file = "plot2.png", plot = plot2, height=20, width=150, units=c("cm"), limitsize = FALSE)


plot3<- ggplot(data = TopUniversityDetails1, mapping = aes(x = rank, y = score)) +
  geom_point(alpha = .7) +
  geom_smooth(formula = y ~ x, method = "lm", se = FALSE) + 
  scale_x_continuous(breaks = seq(0, 2000, 100)) +
  scale_y_continuous(breaks = seq(0, 100, 10))
ggsave(file = "plot3.png", plot = plot3, height=20, width=20, units=c("cm"))


