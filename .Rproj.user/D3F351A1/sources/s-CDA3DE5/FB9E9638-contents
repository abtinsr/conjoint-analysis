######################################################
##### PACKAGES
######################################################
# Get all necessary standard packages and custom functions
source("../news-conjoint-analysis/2_Script/conjoint_functions.R") 

runConjointAnalysis <- function(data, frac_design, filter_variable, filter_values_list) {
  
  ######################################################
  ##### FILTER SEGMENTS
  ######################################################
  data <- filterData(data = data, 
                     variable = filter_variable, # IMPORTANT! ADD QUO() AROUND THE VARIABLE.
                     values_list = filter_values_list)
  
  ######################################################
  ##### ESTIMATE PART-WORTHS
  ######################################################
  # Merging FracDesign and Data to create a Conjoint dataset
  data[,c('age_group', 'subs_status', 'random_id')] <- NULL
  
  n_respondents <- nrow(data) # Used for several loops.
  n_attributes <- nrow(frac_design) # Used for several loops.
  
  data <- transpose(data)
  rownames(data) <- c(1:n_attributes) # 
  Conjoint <- cbind(frac_design, data)
  
  # Estimate part-worths by calculating the linear regression coefficients of each attibute level per respondent, and taking their averages.
  Results <- estimatePartWorths(frac_design = frac_design, conjoint_data = Conjoint, n_respondents = n_respondents)
  Results_Only_Avgs <- Results[,c('Variable', 'Levels', 'Average')]
  
  # ALTERNATIVE (SECONDARY) METHOD FOR PART-WORTHS
  alternative_Results <- checkAverageRegression(frac_design = frac_design, 
                                                filter_variable = filter_variable, 
                                                filter_values_list = filter_values_list)
  
  Results <- merge(Results, alternative_Results) %>% 
    dplyr::select(c(Variable, Levels, Average, Alternative_Average))
  
  ######################################################
  ##### ESTIMATE RELATIVE IMPORTANCE
  ######################################################
  # Select attributes to visualise
  attrs <- c('Price', 'ExtraAccounts', 'eDN', 'AdFree', 'Korsord', 'ErbjudandeDiscount', 'Arkiv')
  
  # Compute and visualise relative importance
  Regressions <- respondentRegressions(frac_design = frac_design, conjoint_data = Conjoint)
  Importances <- estimateRelativeImportances(respondent_regressions = Regressions, attribute_name_list = attrs)
  
  # ALTERNATIVE METHOD FOR IMPORTANCES
  alternative_Importances <- checkAverageImportances(frac_design = frac_design,
                                                     attribute_name_list = attrs,
                                                     filter_variable = filter_variable, 
                                                     filter_values_list = filter_values_list)
  
  Importances <- merge(Importances, alternative_Importances) %>% 
    dplyr::select(c(Variable, Average, Alternative_Average))
  
  ######################################################
  ##### ESTIMATE MARKET SHARES
  ######################################################
  Market <- simulateMarket(respondent_regressions = Regressions, attribute_name_list = attrs)
  
  ######################################################
  ##### SAVE OUTPUT VALUES
  ######################################################
  lst = list()
  
  lst$Results <- Results
  lst$Importances <- Importances
  lst$Market <- Market
  
  return(lst)
  
}

runSegmentedConjointAnalysis <- function(data, frac_design, filter_variable, filter_values_list) {
  
  ######################################################
  # CREATE TWO EMPTY DATAFRAMES TO FILL
  ######################################################
  full_results <- data.frame(Variable=as.character(),
                             Levels=character(), 
                             Average=as.integer(), 
                             Alternative_Average=as.integer(),
                             Segment=as.character()) 
  
  full_importances <- data.frame(Variable=as.character(),
                                 Average=as.integer(), 
                                 Alternative_Average=as.integer(),
                                 Segment=as.character()) 
  
  ######################################################
  # LOOP THROUGH THE CONJOINT ANALYSIS FOR EACH FILTERED DATASET
  ######################################################
  for (i in 1:length(filter_values_list)) {
    
    # Run the CJA for each segment i. 
    CJA <- runConjointAnalysis(data = Data, 
                               frac_design = FracDesign,
                               filter_variable = filter_variable,
                               filter_values_list = filter_values_list[i])
    
    # Add a field with the filtered segment's name.
    CJA$Results$Segment <- unlist(filter_values_list[i])
    CJA$Importances$Segment <- unlist(filter_values_list[i])
    
    # Create a temporary dataframe for the part-worths.
    temp_part_worths <- CJA$Results
    temp_importances <- CJA$Importances
    
    # Append the temporary dataframe to our main dataframe.
    full_results <- rbind(full_results, temp_part_worths)
    full_importances <- rbind(full_importances, temp_importances)
    
  }
  
  ######################################################
  ##### SAVE OUTPUT VALUES
  ######################################################
  lst = list()
  
  lst$Results <- full_results
  lst$Importances <- full_importances
  
  return(lst)
}