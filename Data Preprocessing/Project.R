dataset<-read.csv("dataset.csv")
print(dataset)

#summery of the data set
summary(dataset)

#finding the missing value
colSums(is.na(dataset))

#function for calculating the frequent catagorical value 
calc_mode <- function(x){
  distinct_values <- unique(x)
  distinct_tabulate <- tabulate(match(x, distinct_values))
  distinct_values[which.max(distinct_tabulate)]
} 

#finding the empty space in data set and replacing with most frequent value
dataset$Profession[grepl("^\\s*$", dataset$Profession)] <- calc_mode(dataset$Profession)


#removing the NA's
dataset$Annual_Income_Dollers <- ifelse(is.na(dataset$Annual_Income_Dollers),
                          mean(dataset$Annual_Income_Dollers, 
                               na.rm = TRUE),dataset$Annual_Income_Dollers)
dataset$Annual_Income_Dollers <- as.numeric(format(round(dataset$Annual_Income_Dollers, 0)))
print(dataset)

dataset$Spending.Score_1_to_100 <- ifelse(is.na(dataset$Spending.Score_1_to_100),
                                        mean(dataset$Spending.Score_1_to_100, 
                                             na.rm = TRUE),dataset$Spending.Score_1_to_100)
dataset$Spending.Score_1_to_100 <- as.numeric(format(round(dataset$Spending.Score_1_to_100, 0)))
print(dataset)

dataset$Work.Experience <- ifelse(is.na(dataset$Work.Experience),
                                          mean(dataset$Work.Experience, 
                                               na.rm = TRUE),dataset$Work.Experience)
dataset$Work.Experience <- as.numeric(format(round(dataset$Work.Experience, 0)))
print(dataset)

#dealing with negative number
dataset$Annual_Income_Dollers <- ifelse(sign(dataset$Annual_Income_Dollers) == -1, 
                                        abs(dataset$Annual_Income_Dollers),
                                        dataset$Annual_Income_Dollers )

