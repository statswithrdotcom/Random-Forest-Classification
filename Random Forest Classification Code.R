
# library the random forest package
library(randomForest)

# load the iris data set
data(iris)

# Choose Size of training data and trees used
Train_N <- 75 # 50% split
Num_Trees <- 64 # 64 trees used

# Split the data set into training and test
set.seed(123) #makes it repeatable
Ind <- sample(1:nrow(iris), Train_N, replace = FALSE)
Train <- iris[Ind,]
Test <- iris[-Ind,]

# The random forest model created using the training data
model <- randomForest(Species ~ ., data = Train, ntree = Num_Trees)

# A data frame containing the predicted and actual flower species
Results <- data.frame(predict(model,Test[,-5]), Test[,5])
names(Results) <- c("Predicted","Actual")

# Initializing values for the loop
Correct <- rep(0,(150-Train_N))
Group_1 <- Group_2 <- Group_3 <- 0
C1 <- C2 <- C3 <- 0

# For loop that iterates through the row indexes of the "Results" data frame
for(i in 1:(150-Train_N)){
  # Assigns a 1 to "Correct" if it is correct
  if(Results$Predicted[i] == Results$Actual[i]){
    Correct[i] = 1
  }
  # Counts up C1 and accumulates group 1 if correct
  if(Results$Actual[i] == "setosa"){
    C1 = C1 + 1
    if(Correct[i] == 1){Group_1 = Group_1 + 1}
  }
  # Counts up C2 and accumulates group 2 if correct
  if(Results$Actual[i] == "versicolor"){
    C2 = C2 + 1
    if(Correct[i] == 1){Group_2 = Group_2 + 1}
  }
  # Counts up C3 and accumulates group 3 if correct
  if(Results$Actual[i] == "virginica"){
    C3 = C3 + 1
    if(Correct[i] == 1){Group_3 = Group_3 + 1}
  }
}

# Calculating the percent correct ovar all and by species
Correct_Total = round(sum(Correct)*100/(150-Train_N),2)
Correct_Seto = round(Group_1*100/C1,2)
Correct_Vers = round(Group_2*100/C2,2)
Correct_Virg = round(Group_3*100/C3,2)

# Printing percent correct in the console
print(paste("Total accuracy: ", Correct_Total, "%",
            "    Setosa accuracy: ", Correct_Seto, "%",
            "    Versicolor accuracy: ", Correct_Vers, "%",
            "    Virginica accuracy: ", Correct_Virg, "%", sep = ""))

# Visualizing percent correct as a bar plot
barplot(c(Correct_Total,Correct_Seto,Correct_Vers,Correct_Virg),
        names.arg = c("Total Acc","Seto Acc", "Vers Acc", "Virg Acc"),
        main = paste("Accuracy for",Num_Trees,"Trees"),xlab = "Different Metics",
        ylab = "Accuracy %", col = c("green","blue","orange","yellow"))



