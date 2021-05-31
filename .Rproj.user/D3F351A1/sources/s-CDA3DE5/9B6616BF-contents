######################################################
##### PACKAGES
######################################################
library(DoE.base) # Used for Fractional design
library(xlsx) # Used to export as Excel file
library(AlgDesign) # Used for Full Factorial design
library(data.table) # Used for estimating part-worths
library(rlist) # Used to compute regressions for each respondent
library(ggplot2)
library(gridExtra) # Used to create visuals with subplots
library(relaimpo) # Used to compute relative importance
library(dplyr)
library(magrittr)

######################################################
##### EXPERIMENTAL DESIGN
######################################################

# Create a Fractional design
# (Please note: You have to manually update these attributes and levels based on your specific test.)
createFracDesign <- function() {
  
  test.design <-oa.design(nlevels = c(3,4,2,2,2,2,2), seed = 42) # The number of levels within each attribute. Important to set a seed if you want to design surveys based on the package order.
  
  FracDesign <- as.data.frame(test.design)
  names(FracDesign) <- c("Price", "ExtraAccounts", "eDN", "AdFree", "Korsord", "ErbjudandeDiscount", "Arkiv") # Names of the attributes.
  levels(FracDesign$Price) <- c("119", "249", "349") # Names of the levels within the Price attribute.
  levels(FracDesign$ExtraAccounts) <-c("0", "1", "3", "5")
  levels(FracDesign$eDN) <- c("eDN Included", "eDN Excluded")
  levels(FracDesign$AdFree) <- c("Ad Free", "With Ads")
  levels(FracDesign$Korsord) <- c("Korsord Included", "Korsord Excluded")
  levels(FracDesign$ErbjudandeDiscount) <- c("Erbjudande Discounts", "No Erbjudande Discounts")
  levels(FracDesign$Arkiv) <- c("Arkiv Included", "Arkiv Excluded")
  
  return(FracDesign)
}

# Save Fractional design
saveFracDesign <- function(frac_design) {
  # Save design into an excel file
  write.xlsx(frac_design, "../news-conjoint-analysis/3_Output/ExperimentalDesignExample.xlsx")
}

# Compare with full factorial design
compareFullDesign <- function() {
  
  numberlevel = c(c(3,4,2,2,2,2,2))
  fulldesign <-gen.factorial(numberlevel)
  
  nrows_full_design <- nrow(fulldesign) # Runs full factorial
  nrows_frac_design <- nrow(createFracDesign()) # Runs fractional factorial
  
  lst = data.frame(nrows_full_design, nrows_frac_design)
  
  return(lst)
}

######################################################
##### IMPORTING, CLEANING AND MERGING DATA
######################################################

# Import and clean survey data
importAndCleanData  <- function() {
  
  lst = list() # Create empty list to store dataframes - since R can't handle returning multiple variables. 
  
  # BACKGROUND TO THE DATASET
  # Each cv is a variant of a (rank-based conjoint) survey made in Google Forms.
  # As such, we need to clean each and name the columns based on our Fractional design. 
  # cv1 = packages 1-8 in our FracDesign, cv2 = packages 9-16, cv3 = packages 17-24.
  # Because of this, the DRY principle is not applicable.
  
  cv1 <- read.csv(file="../news-conjoint-analysis/1_Data/conjoint_enkät_variant1_mockup.csv", header=TRUE)
  cv2 <- read.csv(file="../news-conjoint-analysis/1_Data/conjoint_enkät_variant2_mockup.csv", header=TRUE)
  cv3 <- read.csv(file="../news-conjoint-analysis/1_Data/conjoint_enkät_variant3_mockup.csv", header=TRUE)
  
  # DEALING WITH NON-SERIOUS ANSWERS
  # An issue is people answering the survey without reading it properly. 
  # We can never know this for sure. 
  # However, one way to catch at least some of these people is by looking at the variance between answers. 
  # If the variance is 0, that is, they have assigned the same value to each package variant, we could assume they've just clicked through ignorantly. 
  # Thus, we remove respondents with a response variance = 0. 
  
  cv1 %<>% 
    rename(
      p1 = Hur.troligt.är.det.att.du.tecknar.paketet.,
      p2 = Hur.troligt.är.det.att.du.tecknar.paketet..1,
      p3 = Hur.troligt.är.det.att.du.tecknar.paketet..2,
      p4 = Hur.troligt.är.det.att.du.tecknar.paketet..3,
      p5 = Hur.troligt.är.det.att.du.tecknar.paketet..4,
      p6 = Hur.troligt.är.det.att.du.tecknar.paketet..5,
      p7 = Hur.troligt.är.det.att.du.tecknar.paketet..6,
      p8 = Hur.troligt.är.det.att.du.tecknar.paketet..7,
      #comment_p1 = X,
      subs_status = Är.du.prenumerant.hos.DN.,
      age_group = Vilken.åldersgrupp.tillhör.du.
    ) %>% 
    rowwise() %>% # To ensure that the variance is calculated per row.
    mutate(
      answer_var = var(c(p1, p2, p3, p4, p5, p6, p7, p8)) # Checking variance allows us to filter out users that have given the same rating to every package - an indication that they haven't checked them properly. 
    ) %>% 
    filter(answer_var != 0)
  cv1[, c('Timestamp', 'answer_var')] <- NULL # , 'comment_p1'
  
  
  cv2 %<>% 
    rename(
      p9 = Hur.troligt.är.det.att.du.tecknar.paketet.,
      p10 = Hur.troligt.är.det.att.du.tecknar.paketet..1,
      p11 = Hur.troligt.är.det.att.du.tecknar.paketet..2,
      p12 = Hur.troligt.är.det.att.du.tecknar.paketet..3,
      p13 = Hur.troligt.är.det.att.du.tecknar.paketet..4,
      p14 = Hur.troligt.är.det.att.du.tecknar.paketet..5,
      p15 = Hur.troligt.är.det.att.du.tecknar.paketet..6,
      p16 = Hur.troligt.är.det.att.du.tecknar.paketet..7,
      #comment_p9 = X,
      subs_status = Är.du.prenumerant.hos.DN.,
      age_group = Vilken.åldersgrupp.tillhör.du.
    ) %>% 
    rowwise() %>% # To ensure that the variance is calculated per row.
    mutate(
      answer_var = var(c(p9, p10, p11, p12, p13, p14, p15, p16)) # Checking variance allows us to filter out users that have given the same rating to every package - an indication that they haven't checked them properly. 
    ) %>% 
    filter(answer_var != 0)
  cv2[, c('Timestamp', 'answer_var')] <- NULL # , 'comment_p9'
  
  cv3 %<>% 
    rename(
      p17 = Hur.troligt.är.det.att.du.tecknar.paketet.,
      p18 = Hur.troligt.är.det.att.du.tecknar.paketet..1,
      p19 = Hur.troligt.är.det.att.du.tecknar.paketet..2,
      p20 = Hur.troligt.är.det.att.du.tecknar.paketet..3,
      p21 = Hur.troligt.är.det.att.du.tecknar.paketet..4,
      p22 = Hur.troligt.är.det.att.du.tecknar.paketet..5,
      p23 = Hur.troligt.är.det.att.du.tecknar.paketet..6,
      p24 = Hur.troligt.är.det.att.du.tecknar.paketet..7,
      #comment_p17 = X,
      subs_status = Är.du.prenumerant.hos.DN.,
      age_group = Vilken.åldersgrupp.tillhör.du.
      
    ) %>% 
    rowwise() %>% # To ensure that the variance is calculated per row.
    mutate(
      answer_var = var(c(p17, p18, p19, p20, p21, p22, p23, p24)) # Checking variance allows us to filter out users that have given the same rating to every package - an indication that they haven't checked them properly. 
    ) %>% 
    filter(answer_var != 0)
  cv3[, c('Timestamp', 'answer_var')] <- NULL # , 'comment_p17'
  
  lst$cv1 <- cv1
  lst$cv2 <- cv2
  lst$cv3 <- cv3
  
  ###############################################################
  #### HARD-CODED MANIPULATION OF SEGMENTS THAT ARE TOO SMALL
  ###############################################################
  
  lst$cv1$age_group <- as.character(lst$cv1$age_group)
  lst$cv1[lst$cv1=="Under 30 år" | lst$cv1=="30-39 år"] <- "Under 40 år"
  lst$cv1$age_group <- as.factor(lst$cv1$age_group)
  
  lst$cv2$age_group <- as.character(lst$cv2$age_group)
  lst$cv2[lst$cv2=="Under 30 år" | lst$cv2=="30-39 år"] <- "Under 40 år"
  lst$cv2$age_group <- as.factor(lst$cv2$age_group)
  
  lst$cv3$age_group <- as.character(lst$cv3$age_group)
  lst$cv3[lst$cv3=="Under 30 år" | lst$cv3=="30-39 år"] <- "Under 40 år"
  lst$cv3$age_group <- as.factor(lst$cv3$age_group)
  
  
  lst$cv1$subs_status <- as.character(lst$cv1$subs_status)
  lst$cv1[lst$cv1=="Nej och har aldrig varit" | lst$cv1=="Nej, men har varit förut"] <- "Nej, har ej prenumeration"
  lst$cv1$subs_status <- as.factor(lst$cv1$subs_status)
  
  lst$cv2$subs_status <- as.character(lst$cv2$subs_status)
  lst$cv2[lst$cv2=="Nej och har aldrig varit" | lst$cv2=="Nej, men har varit förut"] <- "Nej, har ej prenumeration"
  lst$cv2$subs_status <- as.factor(lst$cv2$subs_status)
  
  lst$cv3$subs_status <- as.character(lst$cv3$subs_status)
  lst$cv3[lst$cv3=="Nej och har aldrig varit" | lst$cv3=="Nej, men har varit förut"] <- "Nej, har ej prenumeration"
  lst$cv3$subs_status <- as.factor(lst$cv3$subs_status)

  ###############################################################
  #### HARD-CODED MANIPULATION OF SEGMENTS THAT ARE TOO SMALL
  ###############################################################
  
  return(lst)
  
}

# Merge partial survey datasets to create full-preferences
createCompletePreferences <- function(input_data) {
  
  # DEALING WITH PARTIAL PREFERENCES
  # We have a problem to solve in our data.
  # Although we're created a Fractional design, 24 variants is still too much for a respondent to patiently answer. 
  # To combat this, we've created three surveys with 8 variants each. The variants are randomly assigned to each survey. 
  # But this means we're not getting full preferences per individual. In fact, this incomplete picture is insufficient to generate some of the insights we desire. 
  # Luckily, the exposure to our surveys has been completely random, meaning, we have the same distribution of preferences across each survey. 
  # We know two things about these users: their age group and subscription status.
  # Based on combinations of age group and subscription status, we can randomly create "complete preferences" by combining the valuations of different users from different surveys. 
  # This is an imperfect solution that is bound to create some issues: what is to say that the preference of the combined individuals is the same? 
  # However, since (1) this is done randomly, (2) we assume preferences to be somewhat similar within segments, and (3)  our sample size is fairly large, we could assume that it works sufficiently well. 
  
  cv1 <- input_data$cv1
  cv2 <- input_data$cv2
  cv3 <- input_data$cv3
  
  set.seed(42) # 42. Set seed to control randomisation that occurs when combining individual preferences. 
  
  Data <- data.frame(age_group=factor(),
                     subs_status=factor(),
                     p1 = integer(),
                     p2 = integer(),
                     p3 = integer(),
                     p4 = integer(),
                     p5 = integer(),
                     p6 = integer(),
                     p7 = integer(),
                     p8 = integer(),
                     p9 = integer(),
                     p10 = integer(),
                     p11 = integer(),
                     p12 = integer(),
                     p13 = integer(),
                     p14 = integer(),
                     p15 = integer(),
                     p16 = integer(),
                     p17 = integer(),
                     p18 = integer(),
                     p19 = integer(),
                     p20 = integer(),
                     p21 = integer(),
                     p22 = integer(),
                     p23 = integer(),
                     p24 = integer()
  ) 
  
  ages <- as.vector(unique(cv1$age_group))
  subs <- as.vector(unique(cv1$subs_status))
  
  for (a in ages) { # For each age group... 
    for (s in subs) { # ... and subscription status... 
      
      # Filter for certain age and subscription segments
      temp_cv1 <- cv1 %>% 
        filter(age_group == a & subs_status == s)
      temp_cv2 <- cv2 %>% 
        filter(age_group == a & subs_status == s)
      temp_cv3 <- cv3 %>% 
        filter(age_group == a & subs_status == s)
      
      # Identify dataframe with fewest rows
      n <- c(nrow(temp_cv1), nrow(temp_cv2), nrow(temp_cv3))
      min_rows <- min(n)
      
      # Drop rows so that other datasets have as many rows as the smallest dataframe
      temp_cv1 <- temp_cv1[c(1:min_rows), ]
      temp_cv2 <- temp_cv2[c(1:min_rows), ]
      temp_cv3 <- temp_cv3[c(1:min_rows), ]
      
      # Generate random ids
      temp_cv1$random_id <- sample(seq(1:min_rows))
      temp_cv2$random_id <- sample(seq(1:min_rows))
      temp_cv3$random_id <- sample(seq(1:min_rows))
      
      # Join the data
      joined_df <- left_join(temp_cv1, temp_cv2, 
                             by = c("age_group", "subs_status", "random_id"))
      joined_df <- left_join(joined_df, temp_cv3, 
                             by = c("age_group", "subs_status", "random_id"))
      
      Data <- rbind(Data, joined_df)
    }
  }
  
  return(Data)
}

# Filter data based on a variable and its values.
filterData <- function(data, variable, values_list) {
  
  # Remember that the input variable needs to be quo(variable)!
  data <- data %>% 
    rowwise() %>% 
    filter(!!variable %in% values_list)
  
  return(data)
}

######################################################
##### ESTIMATE PART-WORTHS
######################################################

# Compute linear regression for each person
respondentRegressions <- function(frac_design, conjoint_data) {
  
  Regressions <- list()
  
  pos_first_respondent <- length(frac_design) + 1
  pos_last_respondent <- ncol(conjoint_data)
  
  for (person in pos_first_respondent:pos_last_respondent) {
    model <- lm(conjoint_data[,person] ~ factor(Price) + # We're interested in the beta of each attribute - to determine how much it drives the score (ranking) that the respondent has given a package.
                  factor(ExtraAccounts) + 
                  factor(eDN) + 
                  factor(AdFree) +
                  factor(Korsord) + 
                  factor(ErbjudandeDiscount) +
                  factor(Arkiv)
                , data=conjoint_data)
    Regressions <- list.append(Regressions, model)
  }
  
  return(Regressions)
}

# Generate part-worths, which is the average linear regression of each respondent plus intercepts. 
estimatePartWorths <- function(frac_design, conjoint_data, n_respondents) {
  
  # Create dataframe with each attribute and its levels as well as an intercept - we aim to combine our data with our regression model. 
  vars <- c("Intercept",
            rep("Price",3),
            rep("ExtraAccounts",4),
            rep("eDN",2),
            rep("AdFree", 2),
            rep("Korsord", 2),
            rep("ErbjudandeDiscount", 2),
            rep("Arkiv", 2)
  )
  lvls <- c("Intercept",
            as.character(levels(conjoint_data$Price)),
            as.character(levels(conjoint_data$ExtraAccounts)),
            as.character(levels(conjoint_data$eDN)),
            as.character(levels(conjoint_data$AdFree)),
            as.character(levels(conjoint_data$Korsord)),
            as.character(levels(conjoint_data$ErbjudandeDiscount)),
            as.character(levels(conjoint_data$Arkiv))
  )
  Results <- data.frame(Variable=vars,Levels=lvls)
  
  # Import the regressions per respondent - we need to use the attribute level betas. 
  Regressions <- respondentRegressions(frac_design = frac_design, conjoint_data = conjoint_data)
  
  # For each attribute level, add the respondents' coefficient levels, one by one. 
  for (person in 1:n_respondents) {
    c <- as.vector(Regressions[[person]]$coefficients)
    coef <-c(c[1],0,c[2:3],0,c[4:6],0,c[7],0,c[8],0,c[9],0,c[10],0,c[11]) # This looks scary - but it's just coefficients (betas) for each attribute plus a "zero level" which acts as its own intercept. 
    Results[,paste("Person",person,sep="")] <- round(coef, digits = 1)
  }
  
  # Get averages and visualize them for each variable
  Results[,"Average"] <-try(round(rowMeans(Results[,-c(1,2)], na.rm = TRUE),digits = 1), silent=TRUE)
  
  if(is.numeric(Results$Average) == FALSE) { 
    Results[,"Average"] <- NULL
    Results <- Results %>% 
      rename(
        Alternative_Average = Person1
      )
  }
  
  return(Results)
}

######################################################
##### VISUALISE PART-WORTHS, RELATIVE IMPORTANCE AND MARKET SHARES
######################################################

# Create plots for the part-worths per attribute
partWorthPlot <- function(part_worths_data, attribute_name) {
  
  subs <- droplevels(subset(part_worths_data, Variable == attribute_name))
  subs$Levels <- reorder(subs$Levels, subs$Alternative_Average)
  
  if (min(subs$Alternative_Average)<0) {
    subs$Alternative_Average <- subs$Alternative_Average + abs(min(subs$Alternative_Average))
  }
  gg1 <- ggplot(data=subs,aes(x=Levels, y=Alternative_Average, group=1)) +
    geom_line() +
    geom_point() +
    ylab("Part-Worth Average") +
    ggtitle(attribute_name) + 
    theme_minimal()
  
  return(gg1)
}

partWorthPlot_Segmented <- function(part_worths_data, attribute_name) {
  
  subs <- droplevels(subset(part_worths_data, Variable == attribute_name))
  subs$Levels <- reorder(subs$Levels, subs$Alternative_Average)
  
  gg1 <- ggplot(data=subs,aes(x=Levels, y=Alternative_Average, group=Segment)) +
    geom_line(aes(color = Segment), size = 2, alpha = 0.5) +
    geom_point() +
    ylab("Part-Worth Average") +
    ggtitle(attribute_name) + 
    theme_minimal()
  
  return(gg1)
}

# Plot the part-worth subplots
visualisePartWorths <- function(part_worths_data, attribute_name_list, segmented = FALSE) {
  
  for (i in 1:length(attribute_name_list)) {
    
    attr_name <- attribute_name_list[i]
    plot_name <- paste("gg",i,sep="") 
    
    if (segmented) {
      assign(plot_name, 
             partWorthPlot_Segmented(part_worths_data = part_worths_data, attribute_name = attr_name))
    } else {
      assign(plot_name, 
             partWorthPlot(part_worths_data = part_worths_data, attribute_name = attr_name)) 
    }
    
  }
  
  grid.arrange(gg1, gg2, gg3, gg4, gg5, gg6, gg7, nrow=3, ncol=3)
}

# Estimate relative importance
estimateRelativeImportances <- function(respondent_regressions, attribute_name_list) {
  
  Importances <- data.frame(Variable = attribute_name_list) 
  
  respondent_regressions[[209]] <- NULL # This is a faulty piece of data. For some reason, the covariance matrix fails.
  
  for (model in 1:length(respondent_regressions)) {
    relImp <- calc.relimp(respondent_regressions[[model]], type = c("lmg"), rela = TRUE)
    relImp <- as.vector(relImp@lmg)
    Importances[,paste("Person",model,sep="")] <- round(relImp, digits = 3)
  }
  
  Importances$Average <-try(round(rowMeans(Importances[,-1]), digits = 3), silent=TRUE)
  
  if(is.numeric(Importances$Average) == FALSE) { 
    Importances[,"Average"] <- NULL
    Importances <- Importances %>% 
      rename(
        Alternative_Average = Person1
      )
  }
  
  #Importances <- subset(Importances, select = c(Variable, Average))
  #Importances <- reorder(Importances$Variable,Importances$Average)
  
  return(Importances)
}

# Plot relative importance
visualiseRelativeImportances <- function(importances_data) {
  
  ggplot(importances_data, aes(x=reorder(Variable, Alternative_Average), y=Alternative_Average)) + 
    geom_col(color='coral1', fill='coral1') + 
    coord_flip() +
    scale_y_continuous(labels = function(x)paste(x*100, "%")) + 
    ylab("Relative Importance") + 
    xlab("Attributes") + 
    labs(title = "Price is value-driving, but not alone",
         subtitle = "Other attributes split the remaining 40% of value") + 
    theme_minimal()
}

visualiseRelativeImportances_Segmented <- function(importances_data) {
  
  ggplot(importances_data, aes(x=reorder(Variable, Alternative_Average), y=Alternative_Average, group=Segment)) + 
    geom_col(aes(fill=Segment), position = "dodge") + 
    coord_flip() +
    scale_y_continuous(labels = function(x)paste(x*100, "%")) + 
    ylab("Relative Importance") + 
    xlab("Attributes") + 
    labs(title = "Price is value-driving",
         subtitle = "Other attributes carry varying but insignificant weight") + 
    theme_minimal()
}

# Simulate a market with 50 different packages
simulateMarket <- function(respondent_regressions, attribute_name_list) {
  
  set.seed(42)
  
  price <-sample(c("119", "249", "349"), 50, replace = TRUE)
  extraAccounts <- sample(c("0", "1", "3"), 50, replace = TRUE)
  eDN <- sample(c("eDN Excluded","eDN Included"), 50, replace = TRUE)
  adFree <- sample(c("Ad Free", "With Ads"), 50, replace = TRUE)
  korsord <- sample(c("Korsord Included", "Korsord Excluded"), 50, replace = TRUE)
  erbjudandeDiscount <- sample(c("Erbjudande Discounts", "No Erbjudande Discounts"), 50, replace = TRUE)
  arkiv <- sample(c("Arkiv Excluded", "Arkiv Included"), 50, replace = TRUE)
  
  Market <- data.frame(a=price, b=extraAccounts, c=eDN, d=adFree, e=korsord, f=erbjudandeDiscount, g=arkiv)
  names(Market) <- attribute_name_list
  
  # Calculate utility scores for each subscription package for each respondent
  for (participant in 1:length(respondent_regressions)) {
    Market[,paste("P",participant,sep="")] <- predict(respondent_regressions[[participant]], newdata = Market[,1:7]) # 1:7 because we have 7 attributes.
  } 
  
  return(Market)
}

# Visualise market share based on a certain attribute's levels
visualiseMarketShare <- function(market_data, attribute_name) {
  
  # Determine the potential market share
  purchased <-unlist(apply(market_data[,8:ncol(market_data)], 2, function(x) which(x == max(x))))
  purchased <-market_data[,c(attribute_name)][purchased]
  count <-as.data.frame(table(purchased))
  count$Freq <- count$Freq/ sum(count$Freq)
  
  ggplot(count, aes(x=purchased,y=Freq)) + 
    geom_bar(stat="identity", color='coral1', fill='coral1') + 
    scale_y_continuous(labels = function(x)paste(x*100, "%")) +
    theme_minimal()
  
}

######################################################
##### SANITY CHECK WITH AVERAGE-BASED REGRESSION MODEL
######################################################

# Calculate average valuations for each package based on available respondents (that is, partial-preferences)
calculateRowAverages <- function(df, filter_variable, filter_values_list) {
  
  segs <- c('age_group', 'subs_status')
  
  df <- as.data.frame(df) # Deal with the fact that df_list is a list of dataframes. 
  colnames(df) <- sub("....", "", colnames(df)) # And that the names are cvx.*
  
  cleaned_df <- filterData(data = df, 
                           variable = filter_variable, 
                           values_list = filter_values_list)
  
  cleaned_df %<>% 
    dplyr::select(- one_of(segs)) %>% # Using dplyr:: because it seems it confuses the select function with something else... 
    transpose()
  
  cleaned_df %<>% 
    mutate(cleaned_df, Average = rowMeans(select(cleaned_df, starts_with("V")), na.rm = TRUE)) %>% 
    dplyr::select(Average)
  
  return(cleaned_df)
  
}

# Calculate regression betas (coefficients) based on the average rating of each package
checkAverageRegression <- function(frac_design, filter_variable, filter_values_list) {
  
  df_list <- importAndCleanData()
  
  for (i in 1:length(df_list)) {
    
    df_name <- paste("cv",i,sep="") # cv1, cv2, cv3
    assign(df_name, calculateRowAverages(df = df_list[i], 
                                         filter_variable = filter_variable, 
                                         filter_values_list = filter_values_list))
  }
  
  cvs <- rbind(cv1, cv2)
  cvs <- rbind(cvs, cv3)
  
  temp_Conjoint <- cbind(frac_design, cvs)
  
  temp_Results <- estimatePartWorths(frac_design = frac_design, conjoint_data = temp_Conjoint, n_respondents = 1)
  
  return(temp_Results)
}

# Calculate relative importances based on the average rating of each package
checkAverageImportances <- function(frac_design, attribute_name_list, filter_variable, filter_values_list) {
  
  df_list <- importAndCleanData()
  
  for (i in 1:length(df_list)) {
    
    df_name <- paste("cv",i,sep="") 
    assign(df_name, calculateRowAverages(df=df_list[i], 
                                         filter_variable = filter_variable, 
                                         filter_values_list = filter_values_list)) 
  }
  
  cvs <- rbind(cv1, cv2)
  cvs <- rbind(cvs, cv3)
  
  temp_Conjoint <- cbind(frac_design, cvs)
  
  temp_Regression <- respondentRegressions(frac_design = frac_design, conjoint_data = temp_Conjoint)
  
  temp_RelativeImportances <- estimateRelativeImportances(respondent_regressions = temp_Regression, attribute_name_list = attribute_name_list)
  
  return(temp_RelativeImportances)
}

