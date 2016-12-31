#reading data and preprocessing
library(readr)
fertility_Diagnosis <- read_csv("~/Downloads/fertility_Diagnosis.txt",col_names = FALSE)


fertility_Diagnosis[fertility_Diagnosis=="O"]<-1
fertility_Diagnosis[fertility_Diagnosis=="N"]<-0
colnames(fertility_Diagnosis) <- c("Seasons", "Age","Childish_disease", "Accident_or_trauma","Surgeries","fevers_in_past_year","alcohol_consumption","smoking_habbit","hours_spent_sitting","diagnosis")
fertility_Diagnosis <- transform(fertility_Diagnosis, diagnosis = as.numeric(diagnosis))


head(fertility_Diagnosis)

#neural net
install.packages("neuralnet")
library(neuralnet)

nn = neuralnet(diagnosis ~ Seasons+Age+Childish_disease+Accident_or_trauma+Surgeries+fevers_in_past_year+alcohol_consumption+smoking_habbit+hours_spent_sitting, 
               data=fertility_Diagnosis,hidden = 2,err.fct = "ce", linear.output = FALSE)
#plotting nueral net
plot(nn)

#analysing result
nn$net.result[[1]]
nn1 = ifelse(nn$net.result[[1]]>0.5,1,0)
nn1 #the prdicted output
errorPercentage = mean(fertility_Diagnosis$diagnosis != nn1)
errorPercentage
#data vs predicted for neural net
nnDataVsPredicted = cbind(fertility_Diagnosis$diagnosis,nn1)
nnDataVsPredicted

#saving vs to file
write.csv(as(nnDataVsPredicted, "matrix"), file="neuralNetDataVsPred.csv")




