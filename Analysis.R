### Package
library(ggplot2)

### QUESTION 1 ###

setwd("C:/Users/Madheshvaran/Desktop/Biostatistics")
prelim_data <- read.csv("raw data/List of Volumes and Issues for year 2011 and 2012.csv")

prop_list <- c()
for (i in 1:5) {
  set.seed(2021+i)
  
  sno <- sample(prelim_data$S_No, 10)
  sdata <- prelim_data[sno,]
  total_article <- sum(sdata$Research_Articles)
  total_review <- sum(sdata$Review_Articles)
  
  prop_article <- total_article/(total_article+total_review)
  prop_list <- c(prop_list, prop_article)
}

mean_prop_article <- mean(prop_list)
round(mean_prop_article*13)

### Sampling Method  ### 

# For 2011
sdata <- prelim_data[which(prelim_data$Year == "2011"), ]

# Selection of Research Articles
set.seed(29)
article11 <- sample(sdata$S_No, 11)
list_article_11 <- c()
for (i in article11) {
  set.seed(1000+i)
  x <- sample(1:sdata$Research_Articles[which(sdata$S_No == i)],1)
  list_article_11 <- c(list_article_11, x)
}
temp <- data.frame(S_No = article11, Article_Type = rep("Research Article"), 
                   Article_Chosen = list_article_11)
final_article_11 <- merge(sdata, temp, by = "S_No")

# Selection of Reviews
set.seed(49)
review11 <- sample(sdata$S_No, 2)
list_review_11 <- c()
for (i in review11) {
  set.seed(500+i)
  x <- sample(1:sdata$Review_Articles[which(sdata$S_No == i)],1)
  list_review_11 <- c(list_review_11, x)
}
temp <- data.frame(S_No = review11, Article_Type = rep("Review Article"), 
                   Article_Chosen = list_review_11)
final_review_11 <- merge(sdata, temp, by = "S_No")
final_selection_11 <- rbind(final_article_11, final_review_11)[,-c(7:8)]


# For 2012
sdata <- prelim_data[which(prelim_data$Year == "2012"), ]

# Selection of Research Articles
set.seed(31)
article12 <- sample(sdata$S_No, 11)
list_article_12 <- c()
for (i in article12) {
  set.seed(10+i)
  x <- sample(1:sdata$Research_Articles[which(sdata$S_No == i)],1)
  list_article_12 <- c(list_article_12, x)
}
temp <- data.frame(S_No = article12, Article_Type = rep("Research Article"), 
                   Article_Chosen = list_article_12)
final_article_12 <- merge(sdata, temp, by = "S_No")

# Selection of Reviews
set.seed(51)
review12 <- sample(sdata$S_No, 2)
list_review_12 <- c()
for (i in review12) {
  set.seed(70+i)
  x <- sample(1:sdata$Review_Articles[which(sdata$S_No == i)],1)
  list_review_12 <- c(list_review_12, x)
}
temp <- data.frame(S_No = review12, Article_Type = rep("Review Article"), 
                   Article_Chosen = list_review_12)
final_review_12 <- merge(sdata, temp, by = "S_No")
final_selection_12 <- rbind(final_article_12, final_review_12)[,-c(7:8)]

final_selection <- rbind(final_selection_11, final_selection_12)
write.csv(final_selection, "results/Sample Selection List.csv", row.names = FALSE)

data <- read.csv("raw data/Sample Dataset.csv")
data$Type <- factor(data$Type) 
data$Year <- factor(data$Year)
data$Type_and_Year <- paste(data$Year, " - ", data$Type, sep = "")

### QUESTION 2 ###

ggplot(data = data, aes(x = Citations, fill = Type))+
  geom_histogram(binwidth = 2, color = "black")+
  scale_x_continuous(breaks = seq(0, 34, by = 2))+
  labs(title = "Distribution of Number of Citations By Type of Article",
       x = "Number of Citations", y = "Frequency",
       fill = "Legend Box")+
  scale_fill_manual(values = c("grey60", "grey20"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

ggplot(data = data, aes(x = Citations, fill = Year))+
  geom_histogram(binwidth = 2, color = "black")+
  scale_x_continuous(breaks = seq(0, 34, by = 2))+
  labs(title = "Distribution of Number of Citations By Year of Publication",
       x = "Number of Citations", y = "Frequency",
       fill = "Legend Box")+
  scale_fill_manual(values = c("olivedrab1", "steelblue2"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

ggplot(data = data, aes(x = Citations, fill = Type_and_Year))+
  geom_histogram(binwidth = 2, color = "black")+
  scale_x_continuous(breaks = seq(0, 34, by = 2))+
  labs(title = "Distribution of Number of Citations By Type of Article and Year of Publication",
       x = "Number of Citations", y = "Frequency",
       fill = "Legend Box")+
  scale_fill_manual(values = c("olivedrab1", "olivedrab4", "steelblue1", "steelblue4"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

### QUESTION 3 ###

mean(data$Citations)
median(data$Citations)

### QUESTION 4 ###

research_article_group <- data[which(data$Type == "Research Article"), "Citations"]
review_article_group <- data[which(data$Type == "Review Article"), "Citations"]

t.test(research_article_group, review_article_group, alternative = "two.sided")
wilcox.test(research_article_group, review_article_group, alternative = "two.sided")

### QUESTION 5 ###

error <- qt(0.975, df = length(data$Citations) - 1)*(sd(data$Citations)/sqrt(length(data$Citations)))
left <- mean(data$Citations) - error
right <- mean(data$Citations) + error
