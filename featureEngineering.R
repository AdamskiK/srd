

CreatureFeatures <- function(data) {
  data$MonthlyCreditSpending <- data$Monthly_Spendings/data$Credit_amount
  data$MonthlyCreditIncome <- data$Monthly_Income/data$Credit_amount
  data$ChildPerCreditValue <- as.numeric(data$Household_children/data$Credit_amount)
  # data$AverageIncomePerCredit <- data$Average_income/data$Credit_amount
  data$Total_spending <- data$Spending_clothing + data$Spending_clothing + data$Spending_footwear + data$Spending_household + data$Spending_glassware +
    data$Spending_personal_care + data$Spending_personal_care + data$Spending_catering + data$Spending_electronics + data$Spending_recreational_and_cultural
  data$AgeBucket1 <- ifelse(data$Age < 20, 1, 0)
  data$AgeBucket2 <- ifelse(data$Age > 20 & data$Age < 30, 1, 0)
  data$AgeBucket3 <- ifelse(data$Age > 30 & data$Age < 40, 1, 0)
  data$AgeBucket4 <- ifelse(data$Age > 40 & data$Age < 50, 1, 0)
  data$AgeBucket5 <- ifelse(data$Age > 50 & data$Age < 60, 1, 0)
  data$AgeBucket6 <- ifelse(data$Age > 60, 1, 0)
  
  return(data)
}