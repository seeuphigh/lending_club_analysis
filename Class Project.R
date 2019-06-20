#purpose of this analysis is to understand what factors contribute to defaults on loan payments and
#use those factors to predict whether or not a borrower will default

loandata<-LoanStats3a

#create binary category variable represented whether a loan has defaulted (TRUE) or not (FALSE)
paid_column<- data.frame(PAID=(loandata$loan_status=="Fully Paid"))
loandata<-cbind(loandata, paid_column)

#use logistic regression to predict whether a loan defaults or not
glm_loan<- glm(PAID ~ installment, family= binomial, data=loandata)
summary(glm_loan)


###########run model for multiple factors 
formula<-PAID ~ installment  +   annual_inc  + dti + revol_util + 
 
glm_test<-glm(formula, data=loandata, family="binomial")
summary(glm_test)
coefficients(glm_test)



#bring in market loans
market_loans<- primaryMarketNotes_browseNotes_1_RETAIL


loans_predict<-predict(glm_test, newdata=market_loans, type="response")
loans_predict<-round(loans_predict,3)*100
print(loans_predict)

plot(loandata$installment, loandata$PAID)
curve(predict(glm_loan, data.frame(installment=x), type="response"), add=TRUE)

#historical data all loan categories
hist(loandata, main="Distribution of Defaulted Loans", xlab="grade_numeric")   #does all the data?
#loan amounts all
hist(loandata$loan_amnt, col="blue", xlab="Loan Amount", main="Distribution of Loan Amounts") 
hist(Loan_default$loan_amnt, col="yellow", main="Distribution of Defaulted Loans by Loan Amount", xlab="Loan Amount")
options(scipen=5)
#annual income all##
hist(loandata$annual_inc, main="Distribution of Loans", xlab="Annual Income",
     breaks=5000,xlim=c(0,100000))  
#annual income by defaulted loans##
#create subset of charged off loans only
Loan_default<- loandata[which(loandata$PAID=='FALSE'),]

Loan_nondefault<- loandata[which(loandata$PAID=='TRUE'),]


hist(Loan_default$annual_inc, main="Distribution of Defaulted Loans", xlab="Annual Income",
     breaks=5000, col= c("blue"), xlim=c(0,100000))
hist(Loan_nondefault$annual_inc, main="Distribution of Non Defaulted Loans", xlab="Annual Income",
     breaks=5000, col= c("green"), xlim=c(0,100000))
#now that we see that $60000 annual income and lets subset even more
Loan_default_60000_income<- Loan_default[which(Loan_default$annual_inc==60000),]
#let's see 60000 by grade distribution
#Load plyr Library
library(ggplot2)
grades_plot<-ggplot(data.frame(Loan_default_60000_income), aes(x=grade, col="blue")) + geom_bar()
#lets see by state
state_plot<-ggplot(data.frame(Loan_default_60000_income), aes(x=addr_state, col="red")) + geom_bar()

#grades all
grades_plot_all<-ggplot(data.frame(loandata), aes(x=PAID, col="red")) + geom_bar()

interest_rate_all<-ggplot(data.frame(loandata), aes(x=int_rate_round, col="green")) + geom_bar()


#income_all
hist(loandata$annual_inc, main="Distribution of Annual Income", xlab="Annual Income",
     breaks=5000, col= c("blue"), xlim=c(0,100000))
#now we see that Grade C defaults the most, create Grade C subset
Loan_default_C<- Loan_default[which(Loan_default$grade=="C"),]
Loan_nondefault_C<-Loan_nondefault[which(Loan_nondefault$grade=="C"),]
#now graph loan_amt by grade C
grades_C_default<-ggplot(data.frame(Loan_default_C), aes(x=loan_amnt, col="Blue")) + geom_bar()
grades_C_nondefault<-ggplot(data.frame(Loan_nondefault_C), aes(x=loan_amnt, col="Blue")) + geom_bar()

ggplot(data.frame(loandata), aes(x=PAID, col="Blue"))+ geom_bar()
ggplot(data.frame(Loan_nondefault), aes(x=loan_amnt, col="Blue"))+ geom_bar()

#states all##
states_plot<-ggplot(data.frame(loandata_sample), aes(x=addr_state, col="blue", main= "states")) + geom_bar()
#grades by defaulted loans only#
states_plot_default<-ggplot(data.frame(Loan_default), aes(x=addr_state, col="blue", main= "states")) + geom_bar()

#piegraph for home ownership

#annual income lm test
lm_dti<- lm(loandata$PAID ~ loandata$dti)
summary(lm_dti)
predict_paid<-lm_dti$coeff[1]+ 17.1 * lm_dti$coeff[2]
#plot residuals to test fit
ggplot(data=loandata, aes(lm_dti$residuals))+
  geom_histogram(binwidth = 1, color="black", fill="purple4") +
  theme(panel.background = element_rect(fill="white"), axis.line.x = element_line(),axis.line.y = element_line())
+ggtitle("Histogram for model residuals")


lm_interest<- lm(loandata$PAID ~ loandata$int_rate_round)
summary(lm_interest)
predict_paid<-lm_dti$coeff[1]+ 17.1 * lm_dti$coeff[2]
#plot residuals to test fit
ggplot(data=loandata, aes(lm_interest$residuals))+
  geom_histogram(binwidth = 0.05, color="black", fill="purple4") +
  theme(panel.background = element_rect(fill="white"), axis.line.x = element_line(),axis.line.y = element_line())
+ggtitle("Histogram for model residuals")






library(dplyr)
df<-count(loandata,home_ownership)
library(ggplot2)

#pie chart

pie = ggplot(df, aes(x="", y=n, fill=home_ownership)) + geom_bar(stat="identity", width=1)

pie = pie + coord_polar("y", start=0)


lm_installment<- lm(loandata$PAID ~ loandata$installment_round)
summary(lm_installment)
ggplot(data=loandata, aes(lm_installment$residuals))+
  geom_histogram(binwidth = .1, color="black", fill="purple4") +
  theme(panel.background = element_rect(fill="white"), axis.line.x = element_line(),axis.line.y = element_line())
+ggtitle("Histogram for model residuals")
par(mfrow=c(2,2))
installment_plot<-plot(lm_installment)
annual_plot<-plot(lm_annualincome)

lm_annualincome<- lm(loandata$PAID ~ loandata$annual_inc)
summary(lm_annualincome)
ggplot(data=loandata, aes(lm_annualincome$residuals))+
  geom_histogram(binwidth = 1, color="black", fill="purple4") +
  theme(panel.background = element_rect(fill="white"), axis.line.x = element_line(),axis.line.y = element_line())
+ggtitle("Histogram for model residuals")
lm_grade<- lm(loandata$PAID ~ loandata$grade_numeric)
summary(lm_grade)
plot(lm_dti)