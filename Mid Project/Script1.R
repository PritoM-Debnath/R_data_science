dataset<-read.csv("dataset.csv")
print(dataset)

dataset$Assault <- ifelse(is.na(dataset$Assault),
                          mean(dataset$Assault, 
                               na.rm = TRUE),dataset$Assault)
print(dataset)

dataset$Assault <- as.numeric(format(round(dataset$Assault, 0)))
print(dataset)

fix_UrbanPopulation <- function(df){
  i=1
  for(data in df){
    while(data>100){
      data <- data/10
    }
    df[i] <- data
    i <- i+1
  }
  return (df)
}
dataset$Urban.population.... <- fix_UrbanPopulation(dataset$Urban.population....)
print(dataset)

dataset$Polpulation_level<- with(dataset, ifelse(dataset$Urban.population.... < 50, 'small',
                                                 ifelse(dataset$Urban.population.... < 60, 'medium', 
                                                 ifelse(dataset$Urban.population.... < 70, 'large','extra-large'))))

dataset$Ordered_factor_population <- with(dataset, ifelse(dataset$Polpulation_level == 'small', 1,
                                     ifelse(dataset$Polpulation_level == 'medium', 2,
                                     ifelse(Polpulation_level == 'large', 3, 
                                     4))))