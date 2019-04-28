CreatureFeatures <- function(data) {
  data$MonthlyCreditSpending <- data$Monthly_Spendings/data$Credit_amount
  data$MonthlyCreditIncome <- data$Monthly_Income/data$Credit_amount
  data$ChildPerCreditValue <- as.numeric(data$Household_children/data$Credit_amount)
  # data$AverageIncomePerCredit <- data$Average_income/data$Credit_amount
  data$Total_spending <- data$Spending_clothing + data$Spending_clothing + data$Spending_footwear + data$Spending_household + data$Spending_glassware 
  + data$Spending_personal_care + data$Spending_personal_care + data$Spending_catering + data$Spending_electronics + data$Spending_recreational_and_cultural
  data$AgeBucket1 <- ifelse(data$Age < 20, 1, 0)
  data$AgeBucket2 <- ifelse(data$Age > 20 & data$Age < 30, 1, 0)
  data$AgeBucket3 <- ifelse(data$Age > 30 & data$Age < 40, 1, 0)
  data$AgeBucket4 <- ifelse(data$Age > 40 & data$Age < 50, 1, 0)
  data$AgeBucket5 <- ifelse(data$Age > 50 & data$Age < 60, 1, 0)
  data$AgeBucket6 <- ifelse(data$Age > 60, 1, 0)
  data$CarPerPerson <- data$Personal_car_number/data$Total_population
  data$TrackPerPerson <- data$Truck_number/data$Total_population
  data$TractorPerPerson <- data$Tractor_number/data$Total_population
  data$IndBuildPerPerson <- data$Building_permit_individual_number/data$Total_population
  data$IndBuildPercent <- data$Building_permit_individual_number/data$Building_permit_number
  data$ApartProjPercent <- data$Apartment_project_subbmission_number/data$Building_project_subbmission_number
  data$ApartProjAvgSize <- data$Apartment_project_subbmission_area/data$Apartment_project_subbmission_number
  data$EmplMenPerc <- data$Employed_number_men/data$Employed_number_total
  data$EmplFinancePerc <- data$Employed_finance_number/data$Employed_number_total
  data$EmplAgriPerc <- data$Employed_agricultural_number/data$Employed_number_total
  data$EmplIndustrPerc <- data$Employed_industry_number/data$Employed_number_total
  data$EmplTransPerc <- data$Emplyed_trade_transport_number/data$Employed_number_total
  data$WorkingPopulPerc <- data$Working_age_population/data$Total_population
  data$UnemplRate <- data$Unemployed_total/(data$Unemployed_total+data$Working_age_population)
  data$LowEducationUnemplPerc <- (data$Unemployed_highschool_and_lower_number+data$Unemployed_vocational_number)/data$Unemployed_total
  data$AvSpendingInArea <- data$Total_spending/data$Total_population
  data$SpendingVsAvg <- data$Monthly_Spendings/data$AvSpendingInArea
  data$IncomeVsAvg <- as.numeric(data$Monthly_Income)/as.numeric(data$Average_income)
  data$InstallmentValue <- data$Credit_amount/data$Number_of_installments
  DPDFrame <- data.frame(data$DPD_t0, data$DPD_lag1, data$DPD_lag2, data$DPD_lag3, data$DPD_lag4 
                 , data$DPD_lag5, data$DPD_lag6, data$DPD_lag7, data$DPD_lag8 
                 , data$DPD_lag9, data$DPD_lag10, data$DPD_lag11, data$DPD_lag12)
  NotionalOverdueFrame <- data.frame(data$NotionalOverdue_t0, data$NotionalOverdue_lag1, data$NotionalOverdue_lag2, 
                                     data$NotionalOverdue_lag3, data$NotionalOverdue_lag4, data$NotionalOverdue_lag5, 
                                     data$NotionalOverdue_lag6, data$NotionalOverdue_lag7, data$NotionalOverdue_lag8, 
                                     data$NotionalOverdue_lag9, data$NotionalOverdue_lag10, data$NotionalOverdue_lag11,
                                     data$NotionalOverdue_lag12)
  NotionalValueFrame <- data.frame(data$NotionalValue_t0, data$NotionalValue_lag1, data$NotionalValue_lag2, 
                                   data$NotionalValue_lag3, data$NotionalValue_lag4, data$NotionalValue_lag5, 
                                   data$NotionalValue_lag6, data$NotionalValue_lag7, data$NotionalValue_lag8, 
                                   data$NotionalValue_lag9, data$NotionalValue_lag10, data$NotionalValue_lag11,
                                   data$NotionalValue_lag12)
  Weights = c(6,4,4,4,3,3,3,2,2,2,1.5,1.5,1.5)
  data$AvgDPD <- rowMeans(DPDFrame)
  data$AvgNotionalOverdue <- rowMeans(NotionalOverdueFrame)
  data$AvgNotionalValue <- rowMeans(NotionalValueFrame)
  data$WghtAvgDPD <- tcrossprod(as.matrix(DPDFrame), t(Weights)) / sum(Weights)
  data$WghtNotionalOverdue <- tcrossprod(as.matrix(NotionalOverdueFrame), t(Weights)) / sum(Weights)
  data$WghtNotionalValue <- tcrossprod(as.matrix(NotionalValueFrame), t(Weights)) / sum(Weights)
  #data$AvgOverdueByValue <- rowMeans(NotionalOverdueFrame) / rowMeans(NotionalValueFrame)
  data$HasEverDefaulted <- as.numeric(apply(DPDFrame, 1, function(x) any(x > 90)))
  data$TotalDebtVsIncome <- data$Monthly_Income/(data$Credit_amount + data$NotionalValue_t0)
  data$IncomeVsInstallment <- data$Monthly_Income/data$InstallmentValue
  data$DispInvomeVsInstallment <- (data$Monthly_Income - data$Monthly_Spendings)/data$InstallmentValue
  data$AvgOverdueVsIncome <- data$AvgNotionalOverdue / data$Monthly_Income
  data$AvgOverdueVsDispIncome <- data$AvgNotionalOverdue / (data$Monthly_Income - data$Monthly_Spendings)
  return(data)
}
