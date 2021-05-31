######################################################
##### PACKAGES
######################################################

# Package "renv" is used to manage and lock dependencies. 

# Get all necessary standard packages and custom functions
source("../news-conjoint-analysis/2_Script/conjoint_functions.R") 
source("../news-conjoint-analysis/2_Script/conjoint_auto_calculations.R") 

######################################################
##### GOOD TO KNOW
######################################################

# In this script, the regressions that are the foundation of generating part-worths, importances, etc., 
# are generated in TWO contrasting ways. This is a consequence of having incomplete survey data (since 
# respondents were split into three segments and received different parts of the survey). 

# Short explanation: The PRIMARY METHOD is an average of individual regressions, whereas
# the SECONDARY METHOD (which proved better) is a regression of the average valuation.

# PRIMARY METHOD: Here, we "combine" real respondents based on demographic characteristics so as to generate artificial respondents 
# with "complete" or "full" preferences. This is flawed in the sense that we still might combine individuals with greatly 
# different preferences or scales. Then, based on these "full-preferences," regressions are made INDIVIDUALLY - 
# that is, based on each new-made "individual." These regressions are then aggregated to an average regression.

# SECONDARY METHOD: Instead of creating artificial respondents with "full-preferences," we first calculate an average
# valuation of each package that the respondents have ranked. This creates one field with the average rating of each
# package. A regression is then made on this AVERAGE. 

# My initial focus was on the PRIMARY METHOD, but it proved less robust than the SECONDARY METHOD. Because of this, 
# I presented results based on the SECONDARY (ALTERNATIVE) METHOD. 

# This method is HARD-CODED into the script. If you wish to change it to the INDIVIDUAL-BASED REGRESSIONS, you have to 
# change some of the functions under "conjoint_functions.R".

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
Data = createCompletePreferences(input_data = df_list) # This is the key in the PRIMARY METHOD.
Data$random_id <- NULL

######################################################
##### GET CONJOINT DATA PER SEGMENT
######################################################

# Check unique values in the descriptive fields to use as filter
unique(Data$age_group)
unique(Data$subs_status)

# Set your filter settings (this is changeable)
filter_variable <- quo(age_group) # IMPORTANT! ADD QUO() AROUND THE VARIABLE.
filter_values = c("Under 40 år", "40-65 år", "Över 65 år") # c("Under 40 år", "40-65 år", "Över 65 år")

# Run the conjoint analysis
CJA <- runConjointAnalysis(data = Data, 
                            frac_design = FracDesign,
                            filter_variable = filter_variable,
                            filter_values_list = filter_values) 

CJA$Results
CJA$Importances
CJA$Market

######################################################
##### VISUALISE CONJOINT DATA PER SEGMENT
######################################################
attrs <- c('Price', 'ExtraAccounts', 'eDN', 'AdFree', 'Korsord', 'ErbjudandeDiscount', 'Arkiv') # This is hard-coded into the script. If you wish to change it, you have to change lots of stuff :) 
visualisePartWorths(part_worths_data = CJA$Results, attribute_name_list = attrs)
visualiseRelativeImportances(importances_data = CJA$Importances)
visualiseMarketShare(market_data = CJA$Market, attribute_name = 'Korsord')

######################################################
##### SANITY CHECK: REGRESSION COEFFICIENTS BASED ON AN AVERAGE PACKAGE RATING - HOW DOES THAT AFFECT OUR CONCLUSIONS? 
######################################################
ggplot(CJA$Results, aes(x=Levels, group = 1)) + 
  geom_line(aes(y = Average), color = "coral", size=1, alpha = 0.7) + 
  geom_line(aes(y = Alternative_Average), color="steelblue", size = 1, alpha = 0.7) + 
  theme_minimal()
# --> Here, we can compare our Part-Worths (which is based on regressions for each respondent's valuation), with Alternative Part-Worths (which is based on regressions for the respondents' average valuation). 
# --> We see that there is only slight difference between these methods, suggesting that our method for creating full-preferences works well in this regard. 

ggplot(CJA$Importances, aes(x=Variable, group = 1)) + 
  geom_line(aes(y = Average), color = "coral", size=1, alpha = 0.7) + 
  geom_line(aes(y = Alternative_Average), color="steelblue", size = 1, alpha = 0.7) + 
  theme_minimal()
# --> Here, we can compare our Importances (which is based on regressions of each respondent's valuation), with Alternative Importances (which is based on regressions for the respondents' average valuation). 
# --> We see that there is a significant difference between these methods, suggesting that our method for creating full-preferences works poorly in this regard. 

######################################################
##### RUN CONJOINT ANALYSIS FOR ALL SEGMENT SIMULTANEOUSLY
######################################################

# Set your filter settings (this is changeable)
#filter_variable <- quo(age_group) 
filter_variable <- quo(subs_status) # IMPORTANT! ADD QUO() AROUND THE VARIABLE.

# Select the filter variables to loop through.
#filter_list = list(c("Under 40 år"), c("40-65 år"), c("Över 65 år"))
filter_list = list(c("Ja (enbart digitalt)"), c("Ja (papperstidning)"), c("Nej, har ej prenumeration"))


# Run a segmented CJA
CJA2 <- runSegmentedConjointAnalysis(data = Data, 
                                     frac_design = FracDesign,
                                     filter_variable = filter_variable,
                                     filter_values_list = filter_list) 

CJA2$Results
CJA2$Importances


visualisePartWorths(part_worths_data = CJA2$Results, attribute_name_list = attrs, segmented = TRUE)

visualiseRelativeImportances_Segmented(importances_data = CJA2$Importances)
