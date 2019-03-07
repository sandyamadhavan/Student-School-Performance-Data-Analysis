####IST 707 Data analytics project####
##Author-Sandya Madhavan

###Student performance in maths and portuguese subjects####
##url: https://archive.ics.uci.edu/ml/datasets/student+performance#

###Read the two datasets-maths csv and portugue csv

student_mat<-read.csv("C:/Users/Sandya/Desktop/3rd sem/IST 707/Project/files/student-mat.csv")
student_por<-read.csv("C:/Users/Sandya/Desktop/3rd sem/IST 707/Project/files/student-por.csv")


###View the structure of both the datasets
str(student_mat)
str(student_por)


###Both the datsets have the same fields.I am going to add a field to specify the subject name 
###and merge both the datsets for further analysis


###Add a column in the datsets

student_mat$subjectname<-rep('Maths',nrow(student_mat))
student_por$subjectname<-rep("Portuguese",nrow(student_por))

##merge both the datasets into a single dataset
student_perf<-rbind(student_mat,student_por)

###view the structure of the combined dataset
str(student_perf)


###convert subjectname as factor as there are only two values
student_perf$subjectname<-as.factor(student_perf$subjectname)


###Convert the grade points to 100 from 20
student_perf$G1<-student_perf$G1*5
student_perf$G2<-student_perf$G2*5
student_perf$G3<-student_perf$G3*5


#add an id column to depuct the ids of students
student_perf$ID <- seq.int(nrow(student_perf))

View(student_perf)

##Download the merged dataframe
write.csv(student_perf,'studentdata.csv')


##does the following pattern exist:
#a)Grade 3 is more than ATLEAST BY 10 marks compared to G1

student_perf$G3G1<-(student_perf$G3-student_perf$G1)>=10

student_perf$G3G1<-as.numeric(student_perf$G3G1)


#B)Grade 3 is more than ATLEAST BY 10 marks compared to G2
student_perf$G3G2<-(student_perf$G3-student_perf$G2)>=10

student_perf$G3G2<-as.numeric(student_perf$G3G2)


#c)Grade 2 is more than ATLEAST BY 10 marks compared to G1
student_perf$G2G1<-(student_perf$G2-student_perf$G1)>=10

student_perf$G2G1<-as.numeric(student_perf$G2G1)


#d)Grade 3 is lessthan ATLEAST BY 10 marks compared to G1

student_perf$G31_red<-(student_perf$G1-student_perf$G3)>=10

student_perf$G31_red<-as.numeric(student_perf$G31_red)


#e)Grade 3 is lessthan ATLEAST BY 10 marks compared to G2

student_perf$G32_red<-(student_perf$G2-student_perf$G3)>=10

student_perf$G32_red<-as.numeric(student_perf$G32_red)


###Grade increase or reduction analysis is done offline after this point


##View the frequency of all columns-used to find the various statistics in the dataset
###This is used to find the distribution of all variables
freq_tables = apply(student_perf,2, table)
View(freq_tables)






###############################################################################


#histogram of g3
#To see the distribution of grade 3
histogram(student_perf$G3,xlab="Grade3",ylab="Percentage of students")
###################################################################################


##ggplot for the distribution of G3,subjectname,school and id

p <- ggplot(student_perf, aes(ID,G3,colour=subjectname,size=G3,shape=school))
p<-p + geom_point()
p

######################################################################################



###High and low scores
#outstanding-more than 80
#Good-60 to 79
#medium- 40-59
#low-less than 40

student_perf$Category[student_perf$G3 >=80] = "Outstanding"
student_perf$Category[student_perf$G3 >=60 & student_perf$G3<=79] = "Good"
student_perf$Category[student_perf$G3 >=40 & student_perf$G3<=59] = "Medium"
student_perf$Category[student_perf$G3 <40] = "Low"


table(student_perf$Category)

library(ggplot2)

##analysis 2:
##Father's education Vs. Mother's education -Grouped by student's higher education interest

a<-ggplot(student_perf, aes(x = Fedu, y = Medu, fill =Category,xlab)) + geom_boxplot() +
  facet_wrap(~ higher, ncol = 1)+ ggtitle("Parents education and student's interest towards higher education") + xlab("Father's eductaion") + ylab("Mother's education")


##Student's age Vs Study time -Grouped by student's higher education interest
b<-ggplot(student_perf, aes(x = age, y =studytime , fill =Category,xlab)) + geom_boxplot() +
  facet_wrap(~ higher, ncol = 1)+ ggtitle("Agewise study time allotment  and students interest towards higher education") + xlab("Age") + ylab("Studytime")

#Merge both the graphs
grid.arrange(a,b,ncol=1)


