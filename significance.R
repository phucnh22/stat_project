compensation <-read.csv("C:\\Users\\marta\\Documents\\Big data\\NOVA IMS\\20202021\\1st Semester\\Statistics for Data Science\\Project\\Final\\stack.csv", header = TRUE)

compensation$ConvertedComp<-compensation$ConvertedComp/1000
compensation$ConvertedComp



#model only with numerical variables
model_stack<-lm(ConvertedComp ~ Year+JobSat+YearsCodePro, data=compensation)
summary(model_stack)

#model with all variables
model_st<-lm( ConvertedComp ~ . ,data=compensation)
summary(model_st)

install.packages('knitr', dependencies = TRUE)
install.packages('jtools', dependencies = TRUE)
install.packages("ggplot2", dependencies = TRUE)
install.packages("pixiedust",dependencies = TRUE )
install.packages("broom", dependencies = TRUE)



#to create the html file
install.packages("stargazer", dependencies = TRUE)
library(stargazer)
stargazer(model_st, type = "html", out = "model_st_lm.html")

# Get the significant vars.
summ <- summary(model_st)
summ
pvals <- summ[[4]][, 4]
pvals

significant <- c(which(pvals <= 0.05))
significant
length(significant)  #75 variables will be significant

#to transform to csv so it can be glued in word
write.csv(significant)



#Reset test
library(rtf)
# regression for RESET test
RESETreg <- lm(ConvertedComp ~ Year+JobSat+YearsCodePro+I(fitted(model_stack)^2)+
                 I(fitted(model_stack)^3), data=compensation)
summary(RESETreg)


#Reset test
install.packages("lmtest")
library("lmtest")
#there is mistake in the formulation of the model, since we have to regect the null hypothesis
resettest(model_stack, power = 2:3, type = "regressor", data = d)

  
          
# to create the languages coefficients plot
install.packages("sjPlot", dependencies = TRUE)
library(sjPlot)
library(sjlabelled)
library(sjmisc)
library(ggplot2)


theme_set(theme_sjplot(), axis.title.size = .9)
reg2 <- lm(ConvertedComp ~ . ,data = d, weights = d$Freq)
plot_model(reg2,sort.est = TRUE, terms = c('Python','Dart','Go', 'Groovy', 'Matlab', 'Objective-C', 'PHP', 'R', 'Ruby', 'SQL', 'Scala', 'TypeScript', 'VBA', 'Bash/Shell/PowerShell', 'CSS', 'HTML/CSS', 'Kotlin'))