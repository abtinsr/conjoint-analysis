######################################################
##### PACKAGES
######################################################
# Get all necessary standard packages and custom functions
source("../news-conjoint-analysis/2_Script/conjoint_functions.R") 

######################################################
##### DESIGN EXPERIMENT VARIANTS
######################################################
# Create a Fractional design
FracDesign <- createFracDesign()

# Check how many variants you save between the Full Factorial design and Fractional design
compareFullDesign()

# Saves the set-seeded Fractional design in the Output folder
# saveFracDesign(frac_design = FracDesign)

######################################################
##### IMPORT AND CLEAN DATA
######################################################
df_list <- importAndCleanData()

######################################################
##### CREATE COMPLETE PREFERENCES
######################################################
Data = createCompletePreferences(input_data = df_list)
Data$random_id <- NULL

######################################################
##### FILTER SEGMENTS
######################################################

filter_values = c("Under 30 år", "30-39 år")

Data <- filterData(data = Data, 
                   variable = quo(age_group), # IMPORTANT! ADD QUO() AROUND THE VARIABLE.
                   values_list = filter_values)

######################################################
##### CHECK SEGMENT DISTRIBUTIONS
######################################################

getDemographics <- function() {
  
  d1 <- df_list$cv1[,c("age_group", "subs_status")]
  d2 <- df_list$cv2[,c("age_group", "subs_status")]
  d3 <- df_list$cv3[,c("age_group", "subs_status")]
  
  d1 <- rbind(d1, d2)
  d1 <- rbind(d1, d3)
  
  d1$age_group %<>% 
    factor(levels = c("Under 40 år", "40-65 år", "Över 65 år"))
  
  d1$subs_status %<>% 
    factor(levels = c("Nej, har ej prenumeration", "Ja (papperstidning)", "Ja (enbart digitalt)"))
  
  gg1 <- ggplot(data=d1, aes(x=age_group)) + 
    geom_bar(color='coral1', fill='white') + 
    coord_flip() + 
    theme_minimal() + 
    xlab('Age group') + 
    ylab('# of respondents')
  
  gg2 <- ggplot(data=d1, aes(x=subs_status)) + 
    geom_bar(color='coral1', fill='white') + 
    coord_flip() + 
    theme_minimal() + 
    xlab('Subscription status') + 
    ylab('# of respondents')
  
  grid.arrange(gg1, gg2, nrow=2, ncol=1)
  
  print(nrow(d1))
  
}

getDemographics()
  
Data$age_group %<>% 
  factor(levels = c("Under 40 år", "40-65 år", "Över 65 år"))

ggplot(data=Data, aes(x=age_group)) + 
  geom_bar(color='coral1', fill='white') + 
  coord_flip() + 
  theme_minimal() + 
  xlab('Age group') + 
  ylab('# of full-preference respondents')


Data$subs_status %<>% 
  factor(levels = c("Nej och har aldrig varit", "Nej, men har varit förut", "Ja (papperstidning)", "Ja (enbart digitalt)"))

ggplot(data=Data, aes(x=subs_status)) + 
  geom_bar(color='coral1', fill='white') + 
  coord_flip() + 
  theme_minimal() + 
  xlab('Subscription status') + 
  ylab('# of full-preference respondents')

######################################################
##### ESTIMATE PART-WORTHS
######################################################
# Merging FracDesign and Data to create a Conjoint dataset
Data[,c('age_group', 'subs_status', 'random_id')] <- NULL

n_respondents <- nrow(Data) # Used for several loops.
n_attributes <- nrow(FracDesign) # Used for several loops.

Data <- transpose(Data)
rownames(Data) <- c(1:n_attributes) # 
Conjoint <- cbind(FracDesign, Data)

# Estimate part-worths by calculating the linear regression coefficients of each attibute level per respondent, and taking their averages.
Results <- estimatePartWorths(frac_design = FracDesign, conjoint_data = Conjoint, n_respondents = n_respondents)
Results_Only_Avgs <- Results[,c('Variable', 'Levels', 'Average')]

######################################################
##### VISUALISE PART-WORTHS, RELATIVE IMPORTANCE AND MARKET SHARES
######################################################
# Select attributes to visualise
attrs <- c('Price', 'ExtraAccounts', 'eDN', 'AdFree', 'Korsord', 'ErbjudandeDiscount', 'Arkiv')

# Visualise part-worths
visualisePartWorths(part_worths_data = Results, attribute_name_list = attrs)

# Compute and visualise relative importance
Regressions <- respondentRegressions(frac_design = FracDesign, conjoint_data = Conjoint)
Importances <- estimateRelativeImportances(respondent_regressions = Regressions, attribute_name_list = attrs)
visualiseRelativeImportances(importances_data = Importances)

# Compute and visualise potential market share
Market <- simulateMarket(respondent_regressions = Regressions, attribute_name_list = attrs)
visualiseMarketShare(market_data = Market, attribute_name = 'Korsord')


######################################################
##### SANITY CHECK: REGRESSION COEFFICIENTS BASED ON AN AVERAGE PACKAGE RATING - HOW DOES THAT AFFECT OUR CONCLUSIONS? 
######################################################

alternative_Results <- checkAverageRegression(frac_design = FracDesign, 
                                              filter_variable = quo(age_group), 
                                              filter_values_list = filter_values)

comparison <- merge(Results, alternative_Results) %>% 
  dplyr::select(c(Variable, Levels, Average, Alternative_Average))

ggplot(comparison, aes(x=Levels, group = 1)) + 
  geom_line(aes(y = Average), color = "coral", size=1, alpha = 0.7) + 
  geom_line(aes(y = Alternative_Average), color="steelblue", size = 1, alpha = 0.7) + 
  theme_minimal()

# --> Here, we can compare our Part-Worths (which is based on regressions for each respondent's valuation), with Alternative Results (which is based on regressions for the respondents' average valuation). 
# --> We see that there is only slight difference between these methods, suggesting that our method for creating full-preferences works well in this regard. 

# Does not work for filtered data. 
alternative_Importances <- checkAverageImportances(frac_design = FracDesign, attribute_name_list = attrs)

comparison <- merge(Importances, alternative_Importances) %>% 
  dplyr::select(c(Variable, Average, Alternative_Average)) 

ggplot(comparison, aes(x=Variable, group = 1)) + 
  geom_line(aes(y = Average), color = "coral", size=1, alpha = 0.7) + 
  geom_line(aes(y = Alternative_Average), color="steelblue", size = 1, alpha = 0.7) + 
  theme_minimal()

# --> Here, we can compare our Importances (which is based on regressions for each respondent's valuation), with Alternative Importances (which is based on regressions for the respondents' average valuation). 
# --> We see that there is a significant difference between these methods, suggesting that our method for creating full-preferences works poorly in this regard. 

