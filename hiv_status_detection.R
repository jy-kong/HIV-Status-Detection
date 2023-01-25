library(Amelia) # plot missing map
library(haven) # read SAS, SPSS, and STATA file
library(tidyverse) # to install and load core packages from the tidyverse
# library(dplyr) # data manipulation
library(caret) # streamline model training process + pre-processing
library(caretEnsemble) # a framework for fitting multiple caret models using the same re-sampling strategy and creating ensembles of such models
library(ranger) # a fast implementation of random forests
library(xgboost) # an efficient implementation of the gradient boosting framework
library(klaR) # miscellaneous functions for classification and visualization
library(kknn) # perform k-nearest neighbor classification
library(nnet) # software for feed-forward neural networks with a single hidden layer
library(igraph) # create and manipulate graphs and analyze networks
library(forecast) # provide methods and tools for displaying and analyzing univariate time series forecasts
library(smotefamily) # SMOTE algorithm to solve unbalanced classification problems
library(ROSE) # Random Over-Sampling Examples: to deal with binary classification problems in the presence of imbalanced classes
library(themis) # deal with unbalanced data
library(vtreat) # prepare real-world data for predictive modeling in a statistically sound manner
library(magrittr) # offer a set of operators which make code more readable
library(Boruta) # work with any classification method that output variable importance measure (VIM) - feature selection
library(devtools) # make package development easier by providing R functions that simplify and expedite common tasks
library(tm) # a framework for text mining applications within R
library(wordcloud2) # a fast visualization tool for creating wordcloud by using 'wordcloud2.js'
library(caTools) # to calculate AUC in this case
library(pROC) # tools for visualizing, smoothing and comparing receiver operating characteristic (ROC curves)
library(lattice) # a powerful and elegant high-level data visualization system for R
library(DataExplorer) # to automate most of data handling and visualization
library(SmartEDA) # multiple custom functions to perform initial exploratory analysis (EDA) on any input data (structure, relationships)

#@

# write specific dataset in the workspace into a csv file
write_to_csv <- function(data, filename) {
  write.csv(data, file = filename, row.names = FALSE)
}

#@


# AR - HIV Test Result
angola_hiv <- read_dta("/Users/kjyuaan8/Desktop/Year 3 Sem 1/WIH3001 DSP/Implementation/AOAR71FL.DTA")
# IR - Women Aged 15-49 years
angola_women <- read_dta("/Users/kjyuaan8/Desktop/Year 3 Sem 1/WIH3001 DSP/Implementation/AOIR71FL.DTA")

## HIV Test Result (Angola) - Preview:
head(angola_hiv)
dim(angola_hiv)
colnames(angola_hiv)
glimpse(angola_hiv)
str(angola_hiv)
class(angola_hiv)
summary(angola_hiv)
# to see labels for each label (HIV)
# sapply(angola_hiv, attr, "labels")


##### Special for EDA (less variables)
angola_hiv_label <- as.character(labelled::var_label(angola_hiv))
# OR angola_hiv_label <- as.character(sapply(angola_hiv, attr, "label"))
angola_hiv_factor <- as_factor(angola_hiv)
# Tips: levels = c("default", "labels", "values", "both")
angola_hiv_data <- angola_hiv_factor
colnames(angola_hiv_data) <- angola_hiv_label
head(angola_hiv_data)
angola_hiv_data <- dplyr::select(angola_hiv_data, -c(cluster, household, line)) ###


## IR - Women Aged 15-49 years (Angola) - Preview: -> many columns
head(angola_women[,1:6])
dim(angola_women) # many columns
head(colnames(angola_women), 10)
glimpse(angola_women[,1:6])
str(angola_women[,1:6])
summary(angola_women[,1:6])
# to see labels for each label (Women)
# sapply(angola_women, attr, "labels")

angola_women_label <- as.character(labelled::var_label(angola_women))
# OR angola_women_label <- as.character(sapply(angola_women, attr, "label"))
angola_women_factor <- as_factor(angola_women)
angola_women_data <- angola_women_factor
colnames(angola_women_data) <- angola_women_label
head(angola_women_data[, 1:10])

##### IMPORTANT STEP - Related Columns Selection [Women's HIV] #####
# Extracting only the required sections of columns (under my study) - refer to the Standard Recode Manual
## REC75 - AIDS, STIs, and Condom Use [Len: 172]
## REC80 - AIDS, STIs, and Condom Use (continued) [Len: 113]
angola_women_selected_factor <- dplyr::select(angola_women_factor, v001, v002, v003, v750:v858)


## Merging / joining both the data
# Base file [unit of analysis] --> <angola_hiv_factor> file
final_dataset_factor_angola <- right_join(angola_women_selected_factor, angola_hiv_factor,
                                          by = c("v001" = "hivclust",
                                                 "v002" = "hivnumb",
                                                 "v003" = "hivline"),
                                          keep = FALSE,
                                          na_matches = "never")
# drop the 3 matching variables/columns (with <keep = FALSE>) for encoding purposes (carry no importance weightage)
final_dataset_factor_angola <- dplyr::select(final_dataset_factor_angola, -c(v001, v002, v003))


final_dataset_factor_angola_label <- as.character(labelled::var_label(final_dataset_factor_angola))
# OR final_dataset_factor_angola_label <- as.character(sapply(final_dataset_factor_angola, attr, "label"))
final_dataset_angola <- final_dataset_factor_angola
colnames(final_dataset_angola) <- final_dataset_factor_angola_label
head(final_dataset_angola[, 1:10])
dim(final_dataset_angola)
head(colnames(final_dataset_angola), 10)
tail(colnames(final_dataset_angola), 10)
glimpse(final_dataset_angola[,1:6])
str(final_dataset_angola[,1:6])
summary(final_dataset_angola)[,1:6]

# -----------------------------------------------------------------------------------------------------------------------------------

# Exploratory Data Analysis (EDA)
## Before anything else, EDA first

### 1) DataExplorer
# final_dataset_angola

# final_dataset_angola %>% create_report(
#   output_file = "EDA_DataExplorer",
#   output_dir = "/Users/kjyuaan8/Desktop/Year 3 Sem 1/WIH3001 DSP/Implementation/Reports - EDA",
#   report_title = "Exploratory Data Analysis (EDA) - Data Profiling Report - DHS HIV Dataset (Angola's Women)",
#   y = "blood.test.result")

## CONTENTS
# basic description

# introduce(final_dataset_angola)
# plot_intro(final_dataset_angola)
# plot_str(final_dataset_angola)
# # missing value distribution
# plot_missing(final_dataset_angola)
# # overall correlation heatmap
# plot_correlation(final_dataset_angola)

### 2) SmartEDA

# final_dataset_angola %>% ExpReport(
#   Target = "blood.test.result",
#   op_file = "EDA_SmartEDA",
#   op_dir = "/Users/kjyuaan8/Desktop/Year 3 Sem 1/WIH3001 DSP/Implementation/Reports - EDA")


# Data Visualization
## Histogram   <<Error in plot.new() : figure margins too large>>
# par(mfrow = c(1, 4))
# # only certain columns...
# for (i in 1:5) {
#   if (i != 3) { # manually selected...
#     hist(lapply(final_dataset_angola, as.numeric)[[i]], main = names(final_dataset_angola)[i])
#   }
# }


### Observe frequency and percentage of the target variable: <blood.test.result>
cbind(freq = table(final_dataset_angola$`blood test result`), percentage =
        prop.table(table(final_dataset_angola$`blood test result`)) * 100)

## EDA [Missing Plot]
missmap(final_dataset_angola[, c(1, 2, 149, 150, 152, 153, 173, 216, 217, 218, 219, 220, 221, 222)],
        col = c("black", "grey"), legend = TRUE) # some selected parts only


# -----------------------------------------------------------------------------------------------------------------------------------

# ### Data Pre-Processing Steps
# There are duplicated column names! MAKE THEM UNIQUE!!!
names(final_dataset_angola) <- make.names(names(final_dataset_angola), unique = TRUE)
colnames(final_dataset_angola)


## Important STEP!!! {feature selection/feature engineering}
# Removing na (not applicable) variables - column names starting with "na" /
# questions that are no longer part of the DHS VII core questionnaire from the final_dataset_angola
final_dataset_reduced_1 <- dplyr::select(final_dataset_angola, -starts_with("na."))

# Replacing some survey questions' responses of NA values with their corresponding values [NA -> "no"]
index_holder_1 <- which(colnames(final_dataset_reduced_1) == "sought.sti.advice.treatment.from..central.hospital")
index_holder_2 <- which(colnames(final_dataset_reduced_1) == "sought.sti.advice.treatment.from..other")
# convert factor to character
final_dataset_reduced_1[index_holder_1:index_holder_2] <- lapply(final_dataset_reduced_1[index_holder_1:index_holder_2],
                                                                 as.character)
final_dataset_reduced_1 <- final_dataset_reduced_1 %>% mutate_at(seq(index_holder_1, index_holder_2), ~replace_na(., "no"))
# convert character back to factor
final_dataset_reduced_1[index_holder_1:index_holder_2] <- lapply(final_dataset_reduced_1[index_holder_1:index_holder_2],
                                                                 as.factor)

index_holder_3 <- which(colnames(final_dataset_reduced_1) == "place.for.hiv.test..central.hospital")
index_holder_4 <- which(colnames(final_dataset_reduced_1) == "place.for.hiv.test..other")
# convert factor to character
final_dataset_reduced_1[index_holder_3:index_holder_4] <- lapply(final_dataset_reduced_1[index_holder_3:index_holder_4],
                                                                 as.character)
final_dataset_reduced_1 <- final_dataset_reduced_1 %>% mutate_at(seq(index_holder_3, index_holder_4), ~replace_na(., "no"))
# convert character back to factor
final_dataset_reduced_1[index_holder_3:index_holder_4] <- lapply(final_dataset_reduced_1[index_holder_3:index_holder_4],
                                                                 as.factor)


sapply(final_dataset_reduced_1, attr, "levels") # to see factors/levels of the latest variables
colnames(final_dataset_reduced_1)
str(final_dataset_reduced_1)

list_of_characters <- c("number.of.sex.partners..excluding.spouse..in.last.12.months",
                        "number.of.sex.partners..including.spouse..in.last.12.months",
                        "months.ago.most.recent.hiv.test",
                        "time.since.last.sex.with.2nd.to.most.recent.partner",
                        "time.since.last.sex.with.3rd.to.most.recent.partner",
                        "age.of.most.recent.partner",
                        "age.of.2nd.to.most.recent.partner",
                        "age.of.3rd.to.most.recent.partner",
                        "total.lifetime.number.of.sex.partners",
                        "how.long.ago.first.had.sex.with.most.recent.partner",
                        "how.long.ago.first.had.sex.with.2nd.most.recent.partner",
                        "how.long.ago.first.had.sex.with.3rd.most.recent.partner",
                        "times.in.last.12.months.had.sex.with.most.recent.partner",
                        "times.in.last.12.months.had.sex.with.2nd.most.recent.partner",
                        "times.in.last.12.months.had.sex.with.3rd.most.recent.partner")
# some manually selected factors into <dbl> {numeric}
final_dataset_reduced_1[list_of_characters] <- lapply(final_dataset_reduced_1[list_of_characters], as.numeric)
glimpse(final_dataset_reduced_1)

# dealing with HIV (response) part of the dataset
final_dataset_reduced_2 <- dplyr::select(final_dataset_reduced_1, -bar.code, -lab.number, -assay.1.result,
                                         -assay.2.result, -assay.3.result)
final_dataset_reduced_2["blood.test.result"] <- lapply(final_dataset_reduced_2["blood.test.result"], as.character)
# miracle -> only 4 unique values left! ("hiv negative", "hiv  positive", "inconclusive", "indeterminate")
final_dataset_reduced_2["blood.test.result"][final_dataset_reduced_2["blood.test.result"] == "indeterminate" |
                                               final_dataset_reduced_2["blood.test.result"] == "inconclusive"] <-
  "hiv negative"
final_dataset_reduced_2["blood.test.result"][final_dataset_reduced_2["blood.test.result"] == "hiv  positive"] <- "hiv positive"
# back to factor
final_dataset_reduced_2["blood.test.result"] <- lapply(final_dataset_reduced_2["blood.test.result"], as.factor)
str(final_dataset_reduced_2)
sapply(final_dataset_reduced_2, attr, "levels") # to see factors/levels of the latest variables



### METHOD/TECHNIQUE 1: Removal of variables with too many null/missing values
## Only choose columns where proportion of null values in each column is less than 80% (less null/missing values)
final_dataset_reduced_3 <- final_dataset_reduced_2[colSums(is.na(final_dataset_reduced_2)) /
                                                     nrow(final_dataset_reduced_2) < .8]

### METHOD/TECHNIQUE 2: Identification of near zero variance predictors
# nearZeroVar() diagnoses predictors that have one unique value (i.e. are zero variance predictors) or
# predictors that are have both of the following characteristics:
# (i) they have very few unique values relative to the number of samples [uniqueCut]; and
# (ii) the ratio of the frequency of the most common value to the frequency of the second most common value is large [freqCut].
# Filter out low-information predictors/variables/columns [low-variance and correlated variables - prevents collinearity]
### DEFAULT: freqCut = 19, uniqueCut = 10
final_dataset_low_variance <- names(final_dataset_reduced_3)[nearZeroVar(final_dataset_reduced_3)] # <caret> package
# taking out the response variable <blood.test.result> and <sample.weight> dataset
final_dataset_low_variance <- final_dataset_low_variance[!final_dataset_low_variance %in% c("blood.test.result", "sample.weight")]
final_dataset_reduced_4 <- dplyr::select(final_dataset_reduced_3, -all_of(final_dataset_low_variance)) # FEATURE ENGINEERING
women_hiv <- final_dataset_reduced_4


##### cleaning data again -- removing NA values
# (i) median imputation in all numeric columns
women_hiv <- women_hiv %>% mutate_if(is.double, function(x) ifelse(is.na(x), median(x, na.rm = T), x))

# (ii) replacing the missing values in bespoke category for all non-numeric columns
for (i in 1:length(women_hiv)) {
  if (sum(is.na(women_hiv[i])) > 0) {
    if ("don't know/not sure/depends" %in% levels(women_hiv[[i]])) {
      women_hiv[[i]][is.na(women_hiv[[i]])] <- "don't know/not sure/depends"
    }
    else if ("other" %in% levels(women_hiv[[i]])) {
      women_hiv[[i]][is.na(women_hiv[[i]])] <- "other"
    }
  }
}
for (i in 1:length(women_hiv)) {
  if (sum(is.na(women_hiv[i])) > 0) {
    if ("don't know" %in% levels(women_hiv[[i]])) {
      women_hiv[[i]][is.na(women_hiv[[i]])] <- "don't know"
    }
    else {
      levels(women_hiv[[i]]) <- c(levels(women_hiv[[i]]), "don't know") # add extra levels to the factors
      women_hiv[[i]][is.na(women_hiv[[i]])] <- "don't know"
    }
  }
}
any(is.na(women_hiv)) # to check still got any null values?


##########################################################################################################################################################
##########################################################################################################################################################
##########################################################################################################################################################

# One-Hot-Encoding for Categorical Variables (using "vtreat" package)
## designTreatmentsZ() - to design a treatment plan
## prepare() - to create "clean" data with (1) all numerical values; and (2) without any missing values

# varslist <- colnames(final_dataset_reduced_4)[-which(colnames(final_dataset_reduced_4) == "blood.test.result")]
### actually: should design variable treatments with no outcome variable <"blood.test.result">
varslist <- colnames(final_dataset_reduced_4)
treatplan <- designTreatmentsZ(final_dataset_reduced_4, varslist, verbose = FALSE)
summary(treatplan)
glimpse(treatplan$scoreFrame)
head(scoreFrame <- treatplan$scoreFrame %>%
       dplyr::select(varName, origName, code))
# OR
# head(scoreFrame <- treatplan %>%
#        use_series(scoreFrame) %>%
#        dplyr::select(varName, origName, code))
head(newvars <- scoreFrame %>%
       filter(code %in% c("clean", "lev")) %>%
       use_series(varName))
# with response variables
women_hiv_treat1 <- prepare(treatplan, final_dataset_reduced_4, varRestriction = newvars)
women_hiv_treat2 <- dplyr::select(women_hiv_treat1, -c(blood_test_result_lev_x_hiv_positive, blood_test_result_lev_x_hiv_negative))
women_hiv_treat2$blood.test.result <- final_dataset_reduced_4$blood.test.result


### CLEAN DATA WITHOUT ANY MISSING VALUES
varslist2 <- colnames(women_hiv)
treatplan2 <- designTreatmentsZ(women_hiv, varslist2, verbose = FALSE)
summary(treatplan2)
glimpse(treatplan2$scoreFrame)
head(scoreFrame2 <- treatplan2 %>%
       use_series(scoreFrame) %>%
       dplyr::select(varName, origName, code))
head(newvars2 <- scoreFrame2 %>%
       filter(code %in% c("clean", "lev")) %>%
       use_series(varName))
# with response variables
women_hiv_treat3 <- prepare(treatplan2, women_hiv, varRestriction = newvars2)
women_hiv_treat4 <- dplyr::select(women_hiv_treat3, -c(blood_test_result_lev_x_hiv_positive, blood_test_result_lev_x_hiv_negative))
women_hiv_treat4$blood.test.result <- women_hiv$blood.test.result

##########################################################################################################################################################
##########################################################################################################################################################
##########################################################################################################################################################

# Synthetic Minority Oversampling TEchnique (SMOTE) - to deal with class imbalance issue
## SMOTE is an oversampling technique that creates synthetic minority class data points to balance the dataset.

##### (i) With NULL values after treatment
# Make this example reproducible
set.seed(101)
# View distribution of response variable (<"blood.test.result">)
table(women_hiv_treat2$blood.test.result) # 12021 -ve, 271 +ve
women_hiv_treat2["blood.test.result"] <- lapply(women_hiv_treat2["blood.test.result"], as.character)
women_hiv_treat2["blood.test.result"][women_hiv_treat2["blood.test.result"] == "hiv positive"] <- 1
women_hiv_treat2["blood.test.result"][women_hiv_treat2["blood.test.result"] == "hiv negative"] <- 0
women_hiv_treat2["blood.test.result"] <- lapply(women_hiv_treat2["blood.test.result"], as.double)
# Use SMOTE to create new dataset that is more balanced
smote <- SMOTE(women_hiv_treat2, women_hiv_treat2["blood.test.result"])
summary(smote)
women_hiv_treat_smote_NULL <- smote$data ### 12021 (-ve) + 271 (+ve) + 11653 (synthetic +ve) = 23945 ROWS
women_hiv_treat_smote_NULL <- dplyr::select(women_hiv_treat_smote_NULL, -class)
women_hiv_treat_smote_NULL["blood.test.result"][women_hiv_treat_smote_NULL["blood.test.result"] == 0] <- "hiv negative"
women_hiv_treat_smote_NULL["blood.test.result"][women_hiv_treat_smote_NULL["blood.test.result"] == 1] <- "hiv positive"
women_hiv_treat_smote_NULL["blood.test.result"] <- lapply(women_hiv_treat_smote_NULL["blood.test.result"], as.factor)
# View distribution of response variable (<"blood.test.result">) in new dataset
table(women_hiv_treat_smote_NULL$blood.test.result) # 12021 -ve, 11924 +ve
data.frame(women_hiv_treat_smote_NULL %>% count(blood.test.result)) # alternative
glimpse(tail(women_hiv_treat_smote_NULL))


##### (ii) Without NULL values after treatment (FULL)
# Make this example reproducible
set.seed(102)
# View distribution of response variable (<"blood.test.result">)
table(women_hiv_treat4$blood.test.result) # 12021 -ve, 271 +ve
women_hiv_treat4["blood.test.result"] <- lapply(women_hiv_treat4["blood.test.result"], as.character)
women_hiv_treat4["blood.test.result"][women_hiv_treat4["blood.test.result"] == "hiv positive"] <- 1
women_hiv_treat4["blood.test.result"][women_hiv_treat4["blood.test.result"] == "hiv negative"] <- 0
women_hiv_treat4["blood.test.result"] <- lapply(women_hiv_treat4["blood.test.result"], as.double)
# Use SMOTE to create new dataset that is more balanced
smote2 <- SMOTE(women_hiv_treat4, women_hiv_treat4["blood.test.result"])
summary(smote2)
women_hiv_treat_smote_FULL <- smote2$data ### 12021 (-ve) + 271 (+ve) + 11653 (synthetic +ve) = 23945 ROWS
women_hiv_treat_smote_FULL <- dplyr::select(women_hiv_treat_smote_FULL, -class)
women_hiv_treat_smote_FULL["blood.test.result"][women_hiv_treat_smote_FULL["blood.test.result"] == 0] <- "hiv negative"
women_hiv_treat_smote_FULL["blood.test.result"][women_hiv_treat_smote_FULL["blood.test.result"] == 1] <- "hiv positive"
women_hiv_treat_smote_FULL["blood.test.result"] <- lapply(women_hiv_treat_smote_FULL["blood.test.result"], as.factor)
# View distribution of response variable (<"blood.test.result">) in new dataset
table(women_hiv_treat_smote_FULL$blood.test.result) # 12021 -ve, 11924 +ve
glimpse(tail(women_hiv_treat_smote_FULL))


##### (iii) Without NULL values (imputed) before treatment (ORI)
# Make this example reproducible
set.seed(103)
# View distribution of response variable (<"blood.test.result">)
table(women_hiv$blood.test.result) # 12021 -ve, 271 +ve
women_hiv2 <- women_hiv

###
# important {{keep this first, maybe it is useful in the future!!!}}
for (i in 1:length(women_hiv)) {
  if (is.factor(women_hiv[[i]])) {
    women_hiv[[i]] <- unclass(women_hiv[[i]])
  }
} # easier method
###################### find("dummify") -> "package:DataExplorer" : can try explore dummify() in doing the same job ######################

glimpse(women_hiv)
sapply(women_hiv, attr, "levels")
###

women_hiv2[sapply(women_hiv2, is.factor)] <- data.matrix(women_hiv2[sapply(women_hiv2, is.factor)])
glimpse(women_hiv2)
sapply(women_hiv2, attr, "levels")
# Use SMOTE to create new dataset that is more balanced
smote3 <- SMOTE(women_hiv2, women_hiv2["blood.test.result"])
summary(smote3)
women_hiv_treat_smote_ORI <- smote3$data ### 12021 (-ve) + 271 (+ve) + 11653 (synthetic +ve) = 23945 ROWS
women_hiv_treat_smote_ORI <- dplyr::select(women_hiv_treat_smote_ORI, -class)
women_hiv_treat_smote_ORI["blood.test.result"] <- lapply(women_hiv_treat_smote_ORI["blood.test.result"], as.character)
women_hiv_treat_smote_ORI["blood.test.result"][women_hiv_treat_smote_ORI["blood.test.result"] == "1"] <- "hiv.negative"
women_hiv_treat_smote_ORI["blood.test.result"][women_hiv_treat_smote_ORI["blood.test.result"] == "2"] <- "hiv.positive"
women_hiv_treat_smote_ORI["blood.test.result"] <- lapply(women_hiv_treat_smote_ORI["blood.test.result"], as.factor)
# View distribution of response variable (<"blood.test.result">) in new dataset
table(women_hiv_treat_smote_ORI$blood.test.result) # 12021 -ve, 11924 +ve
glimpse(tail(women_hiv_treat_smote_ORI))

##########################################################################################################################################################
##########################################################################################################################################################
##########################################################################################################################################################

# Real FEATURE SELECTION methods/techniques:
### 3 Methods:
# (i) Stepwise Forward and Backward Regression - 30 out of 34 variables chosen
# (ii) Boruta algorithm - 34 out of 34 variables chosen (ALL)
# (iii) Variable Importance from machine learning algorithm - 12 out of 34 variables chosen
# (iv) Pearson's Chi-squared test - 9 out of 34 variables chosen


## Method 1: Stepwise Forward and Backward Regression
# Specify a null/base model with no predictors
null_model <- glm(blood.test.result ~ 1, data = women_hiv_treat_smote_ORI, family = "binomial")
null_model
summary(null_model)

# Specify the full model using all of the potential predictors
full_model <- glm(blood.test.result ~ ., data = women_hiv_treat_smote_ORI, family = "binomial")
full_model
summary(full_model)

# Use forward + backward ("both") stepwise algorithms to build a parsimonious model
step_model <- stats::step(null_model, scope = list(lower = null_model, upper = full_model),
                          direction = "both", trace = 0, steps = 1000) # TAKE TIME!!!
step_model
summary(step_model)

# Get the shortlisted variables
step_vars <- names(unlist(step_model[["coefficients"]]))
(step_vars <- step_vars[!step_vars %in% "(Intercept)"]) # remove intercept

stepwise_final <- women_hiv_treat_smote_ORI[step_vars]
stepwise_final$blood.test.result <- women_hiv_treat_smote_ORI$blood.test.result
### Conclusion: 30/34 features selected


## Method 2: Boruta algorithm - a feature ranking and selection algorithm based on random forests algorithm
# Perform Boruta search
boruta_output <- Boruta(blood.test.result ~ ., data = women_hiv_treat_smote_ORI, doTrace = 0) # TAKE TIME!!!

# Do a tentative rough fix
roughFixMod <- TentativeRoughFix(boruta_output)
(boruta_vars <- getSelectedAttributes(roughFixMod))

# Variable Importance Scores
imps <- attStats(roughFixMod)
imps2 <- imps[imps$decision != 'Rejected', c('meanImp', 'decision')]
head(imps2[order(-imps2$meanImp), ]) # descending sort

# Plot variable importance
plot(boruta_output, cex.axis = .7, las = 2, xlab = "", main = "Boruta: Variable Importance")

boruta_final <- women_hiv_treat_smote_ORI[boruta_vars]
boruta_final$blood.test.result <- women_hiv_treat_smote_ORI$blood.test.result
### Conclusion: 34/34 (ALL) features selected


## Method 3: Variable Importance from machine learning algorithm <caret>
# varImp(): Calculation of variable importance for regression and classification models
## -- Recursive Partitioning and Regression Trees (rpart)
# Train an rpart model and compute variable importance
set.seed(300)
rpartMod <- train(blood.test.result ~ ., data = women_hiv_treat_smote_ORI, method = "rpart")
(rpartImp <- varImp(rpartMod))
(rpart_vars <- rownames(rpartImp[["importance"]])[rpartImp[["importance"]][["Overall"]] > 0])
plot(rpartImp, top = 15, main = "Machine Learning - rpart: Variable Importance")

rpart_final <- women_hiv_treat_smote_ORI[rpart_vars]
rpart_final$blood.test.result <- women_hiv_treat_smote_ORI$blood.test.result
### Conclusion: 12/34 features selected


## Method 4: Pearson's Chi-squared test (contingency tables)
# -- a statistical hypothesis test used in the analysis of contingency tables (RANKING) when the sample sizes are large
# -- a statistical test applied to sets of categorical data to evaluate how likely it is that any observed difference
# between the sets arose by chance
# - to examine whether two categorical variables (two dimensions of the contingency table) are
# independent in influencing the test statistic (values within the table)

### !! This method is used here because HIV status detection is a classification problem with CATEGORICAL OUTPUT VARIABLE !!

# initializing (dummy row)
vars_selection <- "start"
chi_squared <- 0
p_value <- 0
chi_sq_table <- data.frame(vars_selection, chi_squared, p_value)

# move the response variable to the last column
chi_squared_data <- women_hiv_treat_smote_ORI %>% relocate(blood.test.result, .after = last_col())
# create a data frame for Pearson's Chi-squared test
for (i in 1:(ncol(chi_squared_data) - 1)) {
  chi_sq = chisq.test(chi_squared_data[["blood.test.result"]], chi_squared_data[[i]], correct = FALSE)
  new_row = c(vars_selection = colnames(chi_squared_data[i]), chi_squared = chi_sq[["statistic"]][["X-squared"]],
              p_value = chi_sq[["p.value"]])
  chi_sq_table = rbind(chi_sq_table, new_row)
} # only can be run once
chi_sq_table = chi_sq_table[-1, ] # remove the first dummy row
# sort according to p_value in ascending order (smallest to biggest). ranking order /
## descending order of chi_squared because [low p_value corresponds to high chi_squared]
(chi_sq_table <- chi_sq_table[order(chi_sq_table$chi_squared, decreasing = TRUE), ]) # wrong here actually but never mind!
# maybe right?

#### Theory behind:
# H0 (null hypothesis): The two variables are independent
# H1 (alternative hypothesis): The two variables relate to each other (dependent)
## Significance Level = 0.05 (predetermined)
# Condition 1: high chi-squared OR p-value < 0.05 (significance level) --> reject null hypothesis --> dependent
# Condition 2: low chi-squared OR p-value >= 0.05 (significance level) --> accept null hypothesis --> independent

chi_sq_vars <- chi_sq_table[23:31, ]$vars_selection # 9 variables selected {{manually}}
chi_sq_final <- women_hiv_treat_smote_ORI[chi_sq_vars]
chi_sq_final$blood.test.result <- women_hiv_treat_smote_ORI$blood.test.result
### Conclusion: 9/34 features selected

##########################################################################################################################################################
##########################################################################################################################################################
##########################################################################################################################################################

## Data Viz - Word Clouds
# Word clouds for the overall dataset first!!! +
# Creating word clouds for the variables selected using those 4 techniques: <stepwise_final>, <boruta_final>, <rpart_final>,
# <chi_sq_final> and observe their differences!


# function to create a word cloud
create_wordcloud <- function(data, num_words = 100, background = "white") {
  
  # If a dataframe is provided, make sure it has the required columns
  if (is.data.frame(data)) {
    if (!"word" %in% names(data) || !"freq" %in% names(data)) {
      stop("Invalid data: expecting two columns named 'word' and 'freq'")
    }
  }
  
  # If text is provided, convert it to a dataframe of word frequencies
  if (is.character(data)) {
    corpus <- Corpus(VectorSource(data))
    corpus <- tm_map(corpus, tolower)
    corpus <- tm_map(corpus, removePunctuation)
    corpus <- tm_map(corpus, removeNumbers)
    corpus <- tm_map(corpus, removeWords, stopwords("english"))
    tdm <- as.matrix(TermDocumentMatrix(corpus))
    data <- sort(rowSums(tdm), decreasing = TRUE)
    data <- data.frame(word = names(data), freq = as.numeric(data))
  }
  
  # Make sure a proper num_words is provided
  if (!is.numeric(num_words) || num_words < 3) {
    num_words <- 3
  }
  
  # Grab the top n most common words
  data <- head(data, n = num_words)
  if (nrow(data) == 0) {
    return(NULL)
  }
  
  wordcloud2(data, backgroundColor = background)
}

# 1) Overall Dataset Word Cloud
create_wordcloud(paste(colnames(angola_women_data), collapse = " "), 150)

# 2) Word Cloud - Stepwise Forward and Backward Regression
create_wordcloud(paste(strsplit(paste(colnames(stepwise_final), collapse = " "), "[.]")[[1]], collapse = " "), 25)

# 3) Word Cloud - Boruta algorithm
create_wordcloud(paste(strsplit(paste(colnames(boruta_final), collapse = " "), "[.]")[[1]], collapse = " "), 25)

# 4) Word Cloud - Variable Importance from machine learning algorithm (rpart)
create_wordcloud(paste(strsplit(paste(colnames(rpart_final), collapse = " "), "[.]")[[1]], collapse = " "), 25)

# 5) Word Cloud - Pearson's Chi-squared test
create_wordcloud(paste(strsplit(paste(colnames(chi_sq_final), collapse = " "), "[.]")[[1]], collapse = " "), 25)

##########################################################################################################################################################
##########################################################################################################################################################
##########################################################################################################################################################

##### Data Modelling Process/Phase + Evaluation Metrics: AUC #####
### <CLASSIFICATION PROBLEM> ###
# Recall that we have 4 types of data with different variable counts: <stepwise_final>, <boruta_final>, <rpart_final>,
# <chi_sq_final>
## <stepwise_final>: Stepwise Forward and Backward Regression - 30 features, 1 target/response
## <boruta_final>: Boruta algorithm - 34 features, 1 target/response [full model]
## <rpart_final>: Variable Importance from machine learning algorithm (rpart) - 12 features, 1 target/response
## <chi_sq_final>: Pearson's Chi-squared test - 9 features, 1 target/response

# We will compare each dataset with different variables and find out which final data yields the best results (highest AUC)
#### Machine Learning Models/Algorithms:
# 1) Elastic Net - glmnet <glmnet>
# 2) Random Forest - rf <ranger>
# 3) Linear Discriminant Analysis - lda <lda>
# 4) Support Vector Machine with a radial kernel - svm <svmRadial>
# 5) Extreme Gradient Boosting - xgboost <xgbTree>
# 6) Naive Bayes - nb <nb>
# 7) K-Nearest Neighbors - knn <kknn>
# 8) Neural Network - nn <nnet>



# (A) Stepwise Forward and Backward Regression #
# Create custom indices: myFolds
set.seed(800)
myFolds_stepwise <- createFolds(stepwise_final$blood.test.result, k = 5) # number of folds: 5

# Create reusable trainControl object: myControl (for a fair comparison of models)
# cross-validation in the model (trainControl) itself (not manually)
set.seed(801)
## default p = 0.75 (cross-validation split: training percentage)
myControl_stepwise <- trainControl(
  summaryFunction = twoClassSummary,
  classProbs = TRUE, # IMPORTANT!
  verboseIter = FALSE,
  savePredictions = TRUE,
  index = myFolds_stepwise
)

# to observe the percentage of the target classes for each fold (are they near to each other?)
table(stepwise_final$blood.test.result) / length(stepwise_final$blood.test.result)
table(stepwise_final$blood.test.result[myFolds_stepwise$Fold1]) / length(myFolds_stepwise$Fold1)
table(stepwise_final$blood.test.result[myFolds_stepwise$Fold2]) / length(myFolds_stepwise$Fold2)
table(stepwise_final$blood.test.result[myFolds_stepwise$Fold3]) / length(myFolds_stepwise$Fold3)
table(stepwise_final$blood.test.result[myFolds_stepwise$Fold4]) / length(myFolds_stepwise$Fold4)
table(stepwise_final$blood.test.result[myFolds_stepwise$Fold5]) / length(myFolds_stepwise$Fold5)


# A1) Fit elastic net model: model_glmnet
set.seed(950)
(stepwise_model_glmnet <- train(
  x = dplyr::select(stepwise_final, -blood.test.result),
  y = stepwise_final$blood.test.result,
  metric = "ROC", # AUC as the evaluation metric
  method = "glmnet",
  trControl = myControl_stepwise,
  # "zv": to remove constant columns; "nzv": to remove nearly constant columns ["pca" as an alternative - it is better]
  # "medianImpute": if missing at random (MAR)
  # "knnImpute": if missing not at random (MNAR)
  # "BoxCox" or "YeoJohnson": for transformation
  # "spatialSign": to sometimes replace "pca" [convert numeric data into a projection on to a unit sphere]
  preProcess = c("center", "scale", "pca")
  # , family = "binomial" # (if train/test manually and observe confusionMatrix)
  # , sampling = c("smote", "rose") # (if sampling was not performed priorly)
))


### a) THRESHOLD: 0.5
# If probabilities of predictions (for "hiv negative") exceed threshold of 0.5, "hiv.negative" else "hiv.positive"
glmnet_predict_stepwise_0.5 <- factor(ifelse(predict(stepwise_model_glmnet, type = "prob")$hiv.negative > 0.5,
                                             "hiv.negative", "hiv.positive"), levels = levels(stepwise_final$blood.test.result))
# OR
# glmnet_predict_stepwise_0.5 <- predict(stepwise_model_glmnet) # DEFAULT type = "raw"
table(glmnet_predict_stepwise_0.5)
# Create confusion matrix
(cm_glmnet_predict_stepwise_0.5 <- confusionMatrix(glmnet_predict_stepwise_0.5, stepwise_final$blood.test.result))

### b) THRESHOLD: 0.8 (trying another threshold)
# If probabilities of predictions (for "hiv negative") exceed threshold of 0.8, "hiv.negative" else "hiv.positive"
glmnet_predict_stepwise_0.8 <- factor(ifelse(predict(stepwise_model_glmnet, type = "prob")$hiv.negative > 0.8,
                                             "hiv.negative", "hiv.positive"), levels = levels(stepwise_final$blood.test.result))
table(glmnet_predict_stepwise_0.8)
# Create confusion matrix
(cm_glmnet_predict_stepwise_0.8 <- confusionMatrix(glmnet_predict_stepwise_0.8, stepwise_final$blood.test.result))

### c) THRESHOLD: 0.2 (trying another threshold)
# If probabilities of predictions (for "hiv negative") exceed threshold of 0.2, "hiv.negative" else "hiv.positive"
glmnet_predict_stepwise_0.2 <- factor(ifelse(predict(stepwise_model_glmnet, type = "prob")$hiv.negative > 0.2,
                                             "hiv.negative", "hiv.positive"), levels = levels(stepwise_final$blood.test.result))
table(glmnet_predict_stepwise_0.2)
# Create confusion matrix
(cm_glmnet_predict_stepwise_0.2 <- confusionMatrix(glmnet_predict_stepwise_0.2, stepwise_final$blood.test.result))


# Make ROC curve <hiv.positive>
(roc_glmnet_predict_stepwise <- colAUC(predict(stepwise_model_glmnet, type = "prob")$hiv.positive,
                                       stepwise_final$blood.test.result, plotROC = TRUE))
# OR (either): yields the same ROC curve <hiv.negative>
# (roc_glmnet_predict_stepwise <- colAUC(predict(stepwise_model_glmnet, type = "prob")$hiv.negative,
#                                        stepwise_final$blood.test.result, plotROC = TRUE))



### NOTES:
### Lasso regression (1): penalizes number of non-zero coefficients [number]
### Ridge regression (0): penalizes absolute magnitude of coefficients [size]
### 'glmnet' is the combination of lasso and ridge regression where:
### alpha = 0 is pure ridge regression, alpha = 1 is pure lasso regression [Mixing Percentage]
### lambda is the size of the penalty [Regularization Parameter]
###

# to observe the hyperparameters tuned (grid search/tuning grid)
plot(stepwise_model_glmnet)
## ROC was used to select the optimal model using the largest value.
## The final values used for the model were alpha = 1 [pure lasso] and lambda = 0.002063035.

# to observe the full regularization path for all of the models with alpha = 0 [pure ridge]
## Left: intercept only model (high value of lambda)
## Right: full model with no penalty (low value of lambda)
### The plot shows how the regression coefficients are "shrunk" from right to left as we increase the strength of the penalty
### on coefficient size, and therefore decrease the complexity of the model.
### We can also see some lines hitting zero as we increase lambda, which represents these coefficients dropping out of the model.
plot(stepwise_model_glmnet$finalModel)




# A2) Fit random forest: model_rf
set.seed(951)
(stepwise_model_rf <- train(
  x = dplyr::select(stepwise_final, -blood.test.result),
  y = stepwise_final$blood.test.result,
  tuneLength = 5, # the maximum number of tuning parameter combinations that will be generated by the random search
  metric = "ROC", # AUC as the evaluation metric
  method = "ranger", # use "ranger" instead of "rf" - faster and more effective
  trControl = myControl_stepwise
  # , family = "binomial" # (if train/test manually and observe confusionMatrix)
))


### a) THRESHOLD: 0.5
# If probabilities of predictions (for "hiv negative") exceed threshold of 0.5, "hiv.negative" else "hiv.positive"
rf_predict_stepwise_0.5 <- factor(ifelse(predict(stepwise_model_rf, type = "prob")$hiv.negative > 0.5,
                                         "hiv.negative", "hiv.positive"), levels = levels(stepwise_final$blood.test.result))
# OR
# rf_predict_stepwise_0.5 <- predict(stepwise_model_rf) # DEFAULT type = "raw"
table(rf_predict_stepwise_0.5)
# Create confusion matrix
(cm_rf_predict_stepwise_0.5 <- confusionMatrix(rf_predict_stepwise_0.5, stepwise_final$blood.test.result))

### b) THRESHOLD: 0.8 (trying another threshold)
# If probabilities of predictions (for "hiv negative") exceed threshold of 0.8, "hiv.negative" else "hiv.positive"
rf_predict_stepwise_0.8 <- factor(ifelse(predict(stepwise_model_rf, type = "prob")$hiv.negative > 0.8,
                                         "hiv.negative", "hiv.positive"), levels = levels(stepwise_final$blood.test.result))
table(rf_predict_stepwise_0.8)
# Create confusion matrix
(cm_rf_predict_stepwise_0.8 <- confusionMatrix(rf_predict_stepwise_0.8, stepwise_final$blood.test.result))

### c) THRESHOLD: 0.2 (trying another threshold)
# If probabilities of predictions (for "hiv negative") exceed threshold of 0.2, "hiv.negative" else "hiv.positive"
rf_predict_stepwise_0.2 <- factor(ifelse(predict(stepwise_model_rf, type = "prob")$hiv.negative > 0.2,
                                         "hiv.negative", "hiv.positive"), levels = levels(stepwise_final$blood.test.result))
table(rf_predict_stepwise_0.2)
# Create confusion matrix
(cm_rf_predict_stepwise_0.2 <- confusionMatrix(rf_predict_stepwise_0.2, stepwise_final$blood.test.result))


# Make ROC curve <hiv.positive>
(roc_rf_predict_stepwise <- colAUC(predict(stepwise_model_rf, type = "prob")$hiv.positive,
                                   stepwise_final$blood.test.result, plotROC = TRUE))
# OR (either): yields the same ROC curve <hiv.negative>
# (roc_rf_predict_stepwise <- colAUC(predict(stepwise_model_rf, type = "prob")$hiv.negative,
#                                        stepwise_final$blood.test.result, plotROC = TRUE))


# to observe the hyperparameters tuned (grid search/tuning grid)
## mtry = "#Randomly Selected Predictors"; splitrule = "Splitting Rule"
plot(stepwise_model_rf)
## Tuning parameter 'min.node.size' was held constant at a value of 1.
## ROC was used to select the optimal model using the largest value.
## The final values used for the model were mtry = 23, splitrule = gini and min.node.size = 1.




# A3) Fit linear discriminant analysis: model_lda
set.seed(952)
(stepwise_model_lda <- train(
  x = dplyr::select(stepwise_final, -blood.test.result),
  y = stepwise_final$blood.test.result,
  metric = "ROC", # AUC as the evaluation metric
  method = "lda",
  trControl = myControl_stepwise
  # , family = "binomial" # (if train/test manually and observe confusionMatrix)
))


### a) THRESHOLD: 0.5
# If probabilities of predictions (for "hiv negative") exceed threshold of 0.5, "hiv.negative" else "hiv.positive"
lda_predict_stepwise_0.5 <- factor(ifelse(predict(stepwise_model_lda, type = "prob")$hiv.negative > 0.5,
                                          "hiv.negative", "hiv.positive"), levels = levels(stepwise_final$blood.test.result))
# OR
# lda_predict_stepwise_0.5 <- predict(stepwise_model_lda) # DEFAULT type = "raw"
table(lda_predict_stepwise_0.5)
# Create confusion matrix
(cm_lda_predict_stepwise_0.5 <- confusionMatrix(lda_predict_stepwise_0.5, stepwise_final$blood.test.result))

### b) THRESHOLD: 0.8 (trying another threshold)
# If probabilities of predictions (for "hiv negative") exceed threshold of 0.8, "hiv.negative" else "hiv.positive"
lda_predict_stepwise_0.8 <- factor(ifelse(predict(stepwise_model_lda, type = "prob")$hiv.negative > 0.8,
                                          "hiv.negative", "hiv.positive"), levels = levels(stepwise_final$blood.test.result))
table(lda_predict_stepwise_0.8)
# Create confusion matrix
(cm_lda_predict_stepwise_0.8 <- confusionMatrix(lda_predict_stepwise_0.8, stepwise_final$blood.test.result))

### c) THRESHOLD: 0.2 (trying another threshold)
# If probabilities of predictions (for "hiv negative") exceed threshold of 0.2, "hiv.negative" else "hiv.positive"
lda_predict_stepwise_0.2 <- factor(ifelse(predict(stepwise_model_lda, type = "prob")$hiv.negative > 0.2,
                                          "hiv.negative", "hiv.positive"), levels = levels(stepwise_final$blood.test.result))
table(lda_predict_stepwise_0.2)
# Create confusion matrix
(cm_lda_predict_stepwise_0.2 <- confusionMatrix(lda_predict_stepwise_0.2, stepwise_final$blood.test.result))


# Make ROC curve <hiv.positive>
(roc_lda_predict_stepwise <- colAUC(predict(stepwise_model_lda, type = "prob")$hiv.positive,
                                    stepwise_final$blood.test.result, plotROC = TRUE))
# OR (either): yields the same ROC curve <hiv.negative>
# (roc_lda_predict_stepwise <- colAUC(predict(stepwise_model_lda, type = "prob")$hiv.negative,
#                                        stepwise_final$blood.test.result, plotROC = TRUE))




# A4) Fit support vector machine with a radial kernel: model_svm
set.seed(953)
(stepwise_model_svm <- train(
  x = dplyr::select(stepwise_final, -blood.test.result),
  y = stepwise_final$blood.test.result,
  metric = "ROC", # AUC as the evaluation metric
  method = "svmRadial",
  trControl = myControl_stepwise
  # , family = "binomial" # (if train/test manually and observe confusionMatrix)
))


### a) THRESHOLD: 0.5
# If probabilities of predictions (for "hiv negative") exceed threshold of 0.5, "hiv.negative" else "hiv.positive"
svm_predict_stepwise_0.5 <- factor(ifelse(predict(stepwise_model_svm, type = "prob")$hiv.negative > 0.5,
                                          "hiv.negative", "hiv.positive"), levels = levels(stepwise_final$blood.test.result))
# OR
# svm_predict_stepwise_0.5 <- predict(stepwise_model_svm) # DEFAULT type = "raw"
table(svm_predict_stepwise_0.5)
# Create confusion matrix
(cm_svm_predict_stepwise_0.5 <- confusionMatrix(svm_predict_stepwise_0.5, stepwise_final$blood.test.result))

### b) THRESHOLD: 0.8 (trying another threshold)
# If probabilities of predictions (for "hiv negative") exceed threshold of 0.8, "hiv.negative" else "hiv.positive"
svm_predict_stepwise_0.8 <- factor(ifelse(predict(stepwise_model_svm, type = "prob")$hiv.negative > 0.8,
                                          "hiv.negative", "hiv.positive"), levels = levels(stepwise_final$blood.test.result))
table(svm_predict_stepwise_0.8)
# Create confusion matrix
(cm_svm_predict_stepwise_0.8 <- confusionMatrix(svm_predict_stepwise_0.8, stepwise_final$blood.test.result))

### c) THRESHOLD: 0.2 (trying another threshold)
# If probabilities of predictions (for "hiv negative") exceed threshold of 0.2, "hiv.negative" else "hiv.positive"
svm_predict_stepwise_0.2 <- factor(ifelse(predict(stepwise_model_svm, type = "prob")$hiv.negative > 0.2,
                                          "hiv.negative", "hiv.positive"), levels = levels(stepwise_final$blood.test.result))
table(svm_predict_stepwise_0.2)
# Create confusion matrix
(cm_svm_predict_stepwise_0.2 <- confusionMatrix(svm_predict_stepwise_0.2, stepwise_final$blood.test.result))


# Make ROC curve <hiv.positive>
(roc_svm_predict_stepwise <- colAUC(predict(stepwise_model_svm, type = "prob")$hiv.positive,
                                    stepwise_final$blood.test.result, plotROC = TRUE))
# OR (either): yields the same ROC curve <hiv.negative>
# (roc_svm_predict_stepwise <- colAUC(predict(stepwise_model_svm, type = "prob")$hiv.negative,
#                                        stepwise_final$blood.test.result, plotROC = TRUE))


# to observe the hyperparameters tuned (grid search/tuning grid)
## C = "Cost"
plot(stepwise_model_svm)
## Tuning parameter 'sigma' was held constant at a value of 0.07275356.
## ROC was used to select the optimal model using the largest value.
## The final values used for the model were sigma = 0.07275356 and C = 1.




# A5) Fit extreme gradient boosting: model_xgboost
set.seed(954)
(stepwise_model_xgboost <- train(
  x = dplyr::select(stepwise_final, -blood.test.result),
  y = stepwise_final$blood.test.result,
  metric = "ROC", # AUC as the evaluation metric
  method = "xgbTree",
  trControl = myControl_stepwise
  # , family = "binomial" # (if train/test manually and observe confusionMatrix)
))


### a) THRESHOLD: 0.5
# If probabilities of predictions (for "hiv negative") exceed threshold of 0.5, "hiv.negative" else "hiv.positive"
xgboost_predict_stepwise_0.5 <- factor(ifelse(predict(stepwise_model_xgboost, type = "prob")$hiv.negative > 0.5,
                                              "hiv.negative", "hiv.positive"), levels = levels(stepwise_final$blood.test.result))
# OR
# xgboost_predict_stepwise_0.5 <- predict(stepwise_model_xgboost) # DEFAULT type = "raw"
table(xgboost_predict_stepwise_0.5)
# Create confusion matrix
(cm_xgboost_predict_stepwise_0.5 <- confusionMatrix(xgboost_predict_stepwise_0.5, stepwise_final$blood.test.result))

### b) THRESHOLD: 0.8 (trying another threshold)
# If probabilities of predictions (for "hiv negative") exceed threshold of 0.8, "hiv.negative" else "hiv.positive"
xgboost_predict_stepwise_0.8 <- factor(ifelse(predict(stepwise_model_xgboost, type = "prob")$hiv.negative > 0.8,
                                              "hiv.negative", "hiv.positive"), levels = levels(stepwise_final$blood.test.result))
table(xgboost_predict_stepwise_0.8)
# Create confusion matrix
(cm_xgboost_predict_stepwise_0.8 <- confusionMatrix(xgboost_predict_stepwise_0.8, stepwise_final$blood.test.result))

### c) THRESHOLD: 0.2 (trying another threshold)
# If probabilities of predictions (for "hiv negative") exceed threshold of 0.2, "hiv.negative" else "hiv.positive"
xgboost_predict_stepwise_0.2 <- factor(ifelse(predict(stepwise_model_xgboost, type = "prob")$hiv.negative > 0.2,
                                              "hiv.negative", "hiv.positive"), levels = levels(stepwise_final$blood.test.result))
table(xgboost_predict_stepwise_0.2)
# Create confusion matrix
(cm_xgboost_predict_stepwise_0.2 <- confusionMatrix(xgboost_predict_stepwise_0.2, stepwise_final$blood.test.result))


# Make ROC curve <hiv.positive>
(roc_xgboost_predict_stepwise <- colAUC(predict(stepwise_model_xgboost, type = "prob")$hiv.positive,
                                        stepwise_final$blood.test.result, plotROC = TRUE))
# OR (either): yields the same ROC curve <hiv.negative>
# (roc_xgboost_predict_stepwise <- colAUC(predict(stepwise_model_xgboost, type = "prob")$hiv.negative,
#                                        stepwise_final$blood.test.result, plotROC = TRUE))


# to observe the hyperparameters tuned (grid search/tuning grid) - {7 tuning parameters altogether}
## max_depth = "Max Tree Depth"; nrounds = "Boosting Iterations"
plot(stepwise_model_xgboost)
## Tuning parameter 'gamma' was held constant at a value of 0.
## Tuning parameter 'min_child_weight' was held constant at a value of 1.
## ROC was used to select the optimal model using the largest value.
## The final values used for the model were nrounds = 150, max_depth = 3, eta = 0.4, gamma = 0, colsample_bytree = 0.8,
## min_child_weight = 1 and subsample = 1.




# A6) Fit naive bayes: model_nb (different syntax??? - actually can... just need to specify dplyr::) {{{TAKE LONG TIME!!!!!}}}
# - maybe because of the previous model package "xgboost" <due to package conflict>
# use find() to see which packages the conflict is coming from, i.e., find("select")
# find("select")
set.seed(955)
(stepwise_model_nb <- train(
  ## OR
  ## x = dplyr::select(stepwise_final, -blood.test.result),
  ## y = stepwise_final$blood.test.result,
  blood.test.result ~ .,
  data = stepwise_final,
  metric = "ROC", # AUC as the evaluation metric
  method = "nb",
  trControl = myControl_stepwise
  # , family = "binomial" # (if train/test manually and observe confusionMatrix)
))


### a) THRESHOLD: 0.5
# If probabilities of predictions (for "hiv negative") exceed threshold of 0.5, "hiv.negative" else "hiv.positive"
nb_predict_stepwise_0.5 <- factor(ifelse(predict(stepwise_model_nb, type = "prob")$hiv.negative > 0.5,
                                         "hiv.negative", "hiv.positive"), levels = levels(stepwise_final$blood.test.result))
# OR
# nb_predict_stepwise_0.5 <- predict(stepwise_model_nb) # DEFAULT type = "raw"
table(nb_predict_stepwise_0.5)
# Create confusion matrix
(cm_nb_predict_stepwise_0.5 <- confusionMatrix(nb_predict_stepwise_0.5, stepwise_final$blood.test.result))

### b) THRESHOLD: 0.8 (trying another threshold)
# If probabilities of predictions (for "hiv negative") exceed threshold of 0.8, "hiv.negative" else "hiv.positive"
nb_predict_stepwise_0.8 <- factor(ifelse(predict(stepwise_model_nb, type = "prob")$hiv.negative > 0.8,
                                         "hiv.negative", "hiv.positive"), levels = levels(stepwise_final$blood.test.result))
table(nb_predict_stepwise_0.8)
# Create confusion matrix
(cm_nb_predict_stepwise_0.8 <- confusionMatrix(nb_predict_stepwise_0.8, stepwise_final$blood.test.result))

### c) THRESHOLD: 0.2 (trying another threshold)
# If probabilities of predictions (for "hiv negative") exceed threshold of 0.2, "hiv.negative" else "hiv.positive"
nb_predict_stepwise_0.2 <- factor(ifelse(predict(stepwise_model_nb, type = "prob")$hiv.negative > 0.2,
                                         "hiv.negative", "hiv.positive"), levels = levels(stepwise_final$blood.test.result))
table(nb_predict_stepwise_0.2)
# Create confusion matrix
(cm_nb_predict_stepwise_0.2 <- confusionMatrix(nb_predict_stepwise_0.2, stepwise_final$blood.test.result))


# Make ROC curve <hiv.positive>
(roc_nb_predict_stepwise <- colAUC(predict(stepwise_model_nb, type = "prob")$hiv.positive,
                                   stepwise_final$blood.test.result, plotROC = TRUE))
# OR (either): yields the same ROC curve <hiv.negative>
# (roc_nb_predict_stepwise <- colAUC(predict(stepwise_model_nb, type = "prob")$hiv.negative,
#                                        stepwise_final$blood.test.result, plotROC = TRUE))


# to observe the hyperparameters tuned (grid search/tuning grid)
## userkernel = FALSE -> "Distribution Type" = "Gaussian"; usekernel = TRUE -> "Distribution Type" = "Nonparametric"
plot(stepwise_model_nb)
## Tuning parameter 'fL' was held constant at a value of 0.
## Tuning parameter 'adjust' was held constant at a value of 1.
## ROC was used to select the optimal model using the largest value.
## The final values used for the model were fL = 0, usekernel = TRUE and adjust = 1.




# A7) Fit k-nearest neighbors: model_knn
set.seed(956)
(stepwise_model_knn <- train(
  blood.test.result ~ .,
  data = stepwise_final,
  metric = "ROC", # AUC as the evaluation metric
  method = "kknn",
  trControl = myControl_stepwise
  # , family = "binomial" # (if train/test manually and observe confusionMatrix)
))


### a) THRESHOLD: 0.5
# If probabilities of predictions (for "hiv negative") exceed threshold of 0.5, "hiv.negative" else "hiv.positive"
knn_predict_stepwise_0.5 <- factor(ifelse(predict(stepwise_model_knn, type = "prob")$hiv.negative > 0.5,
                                          "hiv.negative", "hiv.positive"), levels = levels(stepwise_final$blood.test.result))
# OR
# knn_predict_stepwise_0.5 <- predict(stepwise_model_knn) # DEFAULT type = "raw"
table(knn_predict_stepwise_0.5)
# Create confusion matrix
(cm_knn_predict_stepwise_0.5 <- confusionMatrix(knn_predict_stepwise_0.5, stepwise_final$blood.test.result))

### b) THRESHOLD: 0.8 (trying another threshold)
# If probabilities of predictions (for "hiv negative") exceed threshold of 0.8, "hiv.negative" else "hiv.positive"
knn_predict_stepwise_0.8 <- factor(ifelse(predict(stepwise_model_knn, type = "prob")$hiv.negative > 0.8,
                                          "hiv.negative", "hiv.positive"), levels = levels(stepwise_final$blood.test.result))
table(knn_predict_stepwise_0.8)
# Create confusion matrix
(cm_knn_predict_stepwise_0.8 <- confusionMatrix(knn_predict_stepwise_0.8, stepwise_final$blood.test.result))

### c) THRESHOLD: 0.2 (trying another threshold)
# If probabilities of predictions (for "hiv negative") exceed threshold of 0.2, "hiv.negative" else "hiv.positive"
knn_predict_stepwise_0.2 <- factor(ifelse(predict(stepwise_model_knn, type = "prob")$hiv.negative > 0.2,
                                          "hiv.negative", "hiv.positive"), levels = levels(stepwise_final$blood.test.result))
table(knn_predict_stepwise_0.2)
# Create confusion matrix
(cm_knn_predict_stepwise_0.2 <- confusionMatrix(knn_predict_stepwise_0.2, stepwise_final$blood.test.result))


# Make ROC curve <hiv.positive>
(roc_knn_predict_stepwise <- colAUC(predict(stepwise_model_knn, type = "prob")$hiv.positive,
                                    stepwise_final$blood.test.result, plotROC = TRUE))
# OR (either): yields the same ROC curve <hiv.negative>
# (roc_knn_predict_stepwise <- colAUC(predict(stepwise_model_knn, type = "prob")$hiv.negative,
#                                        stepwise_final$blood.test.result, plotROC = TRUE))


# to observe the hyperparameters tuned (grid search/tuning grid)
## kmax = "max. #Neighbors"
plot(stepwise_model_knn)
## Tuning parameter 'distance' was held constant at a value of 2.
## Tuning parameter 'kernel' was held constant at a value of optimal.
## ROC was used to select the optimal model using the largest value.
## The final values used for the model were kmax = 9, distance = 2 and kernel = optimal.

# plot of misclassification vs. k (when k increases, misclassification increases)
plot(stepwise_model_knn$finalModel)




# A8) Fit neural network: model_nn
set.seed(957)
(stepwise_model_nn <- train(
  blood.test.result ~ .,
  data = stepwise_final,
  metric = "ROC", # AUC as the evaluation metric
  method = "nnet",
  trControl = myControl_stepwise
  # , family = "binomial" # (if train/test manually and observe confusionMatrix)
))


### a) THRESHOLD: 0.5
# If probabilities of predictions (for "hiv negative") exceed threshold of 0.5, "hiv.negative" else "hiv.positive"
nn_predict_stepwise_0.5 <- factor(ifelse(predict(stepwise_model_nn, type = "prob")$hiv.negative > 0.5,
                                         "hiv.negative", "hiv.positive"), levels = levels(stepwise_final$blood.test.result))
# OR
# nn_predict_stepwise_0.5 <- predict(stepwise_model_nn) # DEFAULT type = "raw"
table(nn_predict_stepwise_0.5)
# Create confusion matrix
(cm_nn_predict_stepwise_0.5 <- confusionMatrix(nn_predict_stepwise_0.5, stepwise_final$blood.test.result))

### b) THRESHOLD: 0.8 (trying another threshold)
# If probabilities of predictions (for "hiv negative") exceed threshold of 0.8, "hiv.negative" else "hiv.positive"
nn_predict_stepwise_0.8 <- factor(ifelse(predict(stepwise_model_nn, type = "prob")$hiv.negative > 0.8,
                                         "hiv.negative", "hiv.positive"), levels = levels(stepwise_final$blood.test.result))
table(nn_predict_stepwise_0.8)
# Create confusion matrix
(cm_nn_predict_stepwise_0.8 <- confusionMatrix(nn_predict_stepwise_0.8, stepwise_final$blood.test.result))

### c) THRESHOLD: 0.2 (trying another threshold)
# If probabilities of predictions (for "hiv negative") exceed threshold of 0.2, "hiv.negative" else "hiv.positive"
nn_predict_stepwise_0.2 <- factor(ifelse(predict(stepwise_model_nn, type = "prob")$hiv.negative > 0.2,
                                         "hiv.negative", "hiv.positive"), levels = levels(stepwise_final$blood.test.result))
table(nn_predict_stepwise_0.2)
# Create confusion matrix
(cm_nn_predict_stepwise_0.2 <- confusionMatrix(nn_predict_stepwise_0.2, stepwise_final$blood.test.result))


# Make ROC curve <hiv.positive>
(roc_nn_predict_stepwise <- colAUC(predict(stepwise_model_nn, type = "prob")$hiv.positive,
                                   stepwise_final$blood.test.result, plotROC = TRUE))
# OR (either): yields the same ROC curve <hiv.negative>
# (roc_nn_predict_stepwise <- colAUC(predict(stepwise_model_nn, type = "prob")$hiv.negative,
#                                        stepwise_final$blood.test.result, plotROC = TRUE))


# to observe the hyperparameters tuned (grid search/tuning grid)
## decay = "Weight Decay"; size = "#Hidden Units"
plot(stepwise_model_nn)
## ROC was used to select the optimal model using the largest value.
## The final values used for the model were size = 3 and decay = 0.1.






# (B) Boruta Algorithm #
# Create custom indices: myFolds
set.seed(8000)
myFolds_boruta <- createFolds(boruta_final$blood.test.result, k = 5) # number of folds: 5

# Create reusable trainControl object: myControl (for a fair comparison of models)
# cross-validation in the model (trainControl) itself (not manually)
set.seed(8001)
## default p = 0.75 (cross-validation split: training percentage)
myControl_boruta <- trainControl(
  summaryFunction = twoClassSummary,
  classProbs = TRUE, # IMPORTANT!
  verboseIter = FALSE,
  savePredictions = TRUE,
  index = myFolds_boruta
)

# to observe the percentage of the target classes for each fold (are there near to each other?)
table(boruta_final$blood.test.result) / length(boruta_final$blood.test.result)
table(boruta_final$blood.test.result[myFolds_boruta$Fold1]) / length(myFolds_boruta$Fold1)
table(boruta_final$blood.test.result[myFolds_boruta$Fold2]) / length(myFolds_boruta$Fold2)
table(boruta_final$blood.test.result[myFolds_boruta$Fold3]) / length(myFolds_boruta$Fold3)
table(boruta_final$blood.test.result[myFolds_boruta$Fold4]) / length(myFolds_boruta$Fold4)
table(boruta_final$blood.test.result[myFolds_boruta$Fold5]) / length(myFolds_boruta$Fold5)


# B1) Fit elastic net model: model_glmnet
set.seed(9500)
(boruta_model_glmnet <- train(
  x = dplyr::select(boruta_final, -blood.test.result),
  y = boruta_final$blood.test.result,
  metric = "ROC", # AUC as the evaluation metric
  method = "glmnet",
  trControl = myControl_boruta,
  # "zv": to remove constant columns; "nzv": to remove nearly constant columns ["pca" as an alternative - it is better]
  # "medianImpute": if missing at random (MAR)
  # "knnImpute": if missing not at random (MNAR)
  # "BoxCox" or "YeoJohnson": for transformation
  # "spatialSign": to sometimes replace "pca" [convert numeric data into a projection on to a unit sphere]
  preProcess = c("center", "scale", "pca")
  # , family = "binomial" # (if train/test manually and observe confusionMatrix)
  # , sampling = c("smote", "rose") # (if sampling was not performed priorly)
))


### a) THRESHOLD: 0.5
# If probabilities of predictions (for "hiv negative") exceed threshold of 0.5, "hiv.negative" else "hiv.positive"
glmnet_predict_boruta_0.5 <- factor(ifelse(predict(boruta_model_glmnet, type = "prob")$hiv.negative > 0.5,
                                           "hiv.negative", "hiv.positive"), levels = levels(boruta_final$blood.test.result))
# OR
# glmnet_predict_boruta_0.5 <- predict(boruta_model_glmnet) # DEFAULT type = "raw"
table(glmnet_predict_boruta_0.5)
# Create confusion matrix
(cm_glmnet_predict_boruta_0.5 <- confusionMatrix(glmnet_predict_boruta_0.5, boruta_final$blood.test.result))

### b) THRESHOLD: 0.8 (trying another threshold)
# If probabilities of predictions (for "hiv negative") exceed threshold of 0.8, "hiv.negative" else "hiv.positive"
glmnet_predict_boruta_0.8 <- factor(ifelse(predict(boruta_model_glmnet, type = "prob")$hiv.negative > 0.8,
                                           "hiv.negative", "hiv.positive"), levels = levels(boruta_final$blood.test.result))
table(glmnet_predict_boruta_0.8)
# Create confusion matrix
(cm_glmnet_predict_boruta_0.8 <- confusionMatrix(glmnet_predict_boruta_0.8, boruta_final$blood.test.result))

### c) THRESHOLD: 0.2 (trying another threshold)
# If probabilities of predictions (for "hiv negative") exceed threshold of 0.2, "hiv.negative" else "hiv.positive"
glmnet_predict_boruta_0.2 <- factor(ifelse(predict(boruta_model_glmnet, type = "prob")$hiv.negative > 0.2,
                                           "hiv.negative", "hiv.positive"), levels = levels(boruta_final$blood.test.result))
table(glmnet_predict_boruta_0.2)
# Create confusion matrix
(cm_glmnet_predict_boruta_0.2 <- confusionMatrix(glmnet_predict_boruta_0.2, boruta_final$blood.test.result))


# Make ROC curve <hiv.positive>
(roc_glmnet_predict_boruta <- colAUC(predict(boruta_model_glmnet, type = "prob")$hiv.positive,
                                     boruta_final$blood.test.result, plotROC = TRUE))
# OR (either): yields the same ROC curve <hiv.negative>
# (roc_glmnet_predict_boruta <- colAUC(predict(boruta_model_glmnet, type = "prob")$hiv.negative,
#                                        boruta_final$blood.test.result, plotROC = TRUE))



### NOTES:
### Lasso regression (1): penalizes number of non-zero coefficients [number]
### Ridge regression (0): penalizes absolute magnitude of coefficients [size]
### 'glmnet' is the combination of lasso and ridge regression where:
### alpha = 0 is pure ridge regression, alpha = 1 is pure lasso regression [Mixing Percentage]
### lambda is the size of the penalty [Regularization Parameter]
###

# to observe the hyperparameters tuned (grid search/tuning grid)
plot(boruta_model_glmnet)
## ROC was used to select the optimal model using the largest value.
## The final values used for the model were alpha = 1 [pure lasso] and lambda = 0.002068238.

# to observe the full regularization path for all of the models with alpha = 0 [pure ridge]
## Left: intercept only model (high value of lambda)
## Right: full model with no penalty (low value of lambda)
### The plot shows how the regression coefficients are "shrunk" from right to left as we increase the strength of the penalty
### on coefficient size, and therefore decrease the complexity of the model.
### We can also see some lines hitting zero as we increase lambda, which represents these coefficients dropping out of the model.
plot(boruta_model_glmnet$finalModel)




# B2) Fit random forest: model_rf
set.seed(9501)
(boruta_model_rf <- train(
  x = dplyr::select(boruta_final, -blood.test.result),
  y = boruta_final$blood.test.result,
  tuneLength = 5, # the maximum number of tuning parameter combinations that will be generated by the random search
  metric = "ROC", # AUC as the evaluation metric
  method = "ranger", # use "ranger" instead of "rf" - faster and more effective
  trControl = myControl_boruta
  # , family = "binomial" # (if train/test manually and observe confusionMatrix)
))


### a) THRESHOLD: 0.5
# If probabilities of predictions (for "hiv negative") exceed threshold of 0.5, "hiv.negative" else "hiv.positive"
rf_predict_boruta_0.5 <- factor(ifelse(predict(boruta_model_rf, type = "prob")$hiv.negative > 0.5,
                                       "hiv.negative", "hiv.positive"), levels = levels(boruta_final$blood.test.result))
# OR
# rf_predict_boruta_0.5 <- predict(boruta_model_rf) # DEFAULT type = "raw"
table(rf_predict_boruta_0.5)
# Create confusion matrix
(cm_rf_predict_boruta_0.5 <- confusionMatrix(rf_predict_boruta_0.5, boruta_final$blood.test.result))

### b) THRESHOLD: 0.8 (trying another threshold)
# If probabilities of predictions (for "hiv negative") exceed threshold of 0.8, "hiv.negative" else "hiv.positive"
rf_predict_boruta_0.8 <- factor(ifelse(predict(boruta_model_rf, type = "prob")$hiv.negative > 0.8,
                                       "hiv.negative", "hiv.positive"), levels = levels(boruta_final$blood.test.result))
table(rf_predict_boruta_0.8)
# Create confusion matrix
(cm_rf_predict_boruta_0.8 <- confusionMatrix(rf_predict_boruta_0.8, boruta_final$blood.test.result))

### c) THRESHOLD: 0.2 (trying another threshold)
# If probabilities of predictions (for "hiv negative") exceed threshold of 0.2, "hiv.negative" else "hiv.positive"
rf_predict_boruta_0.2 <- factor(ifelse(predict(boruta_model_rf, type = "prob")$hiv.negative > 0.2,
                                       "hiv.negative", "hiv.positive"), levels = levels(boruta_final$blood.test.result))
table(rf_predict_boruta_0.2)
# Create confusion matrix
(cm_rf_predict_boruta_0.2 <- confusionMatrix(rf_predict_boruta_0.2, boruta_final$blood.test.result))


# Make ROC curve <hiv.positive>
(roc_rf_predict_boruta <- colAUC(predict(boruta_model_rf, type = "prob")$hiv.positive,
                                 boruta_final$blood.test.result, plotROC = TRUE))
# OR (either): yields the same ROC curve <hiv.negative>
# (roc_rf_predict_boruta <- colAUC(predict(boruta_model_rf, type = "prob")$hiv.negative,
#                                        boruta_final$blood.test.result, plotROC = TRUE))


# to observe the hyperparameters tuned (grid search/tuning grid)
## mtry = "#Randomly Selected Predictors"; splitrule = "Splitting Rule"
plot(boruta_model_rf)
## Tuning parameter 'min.node.size' was held constant at a value of 1.
## ROC was used to select the optimal model using the largest value.
## The final values used for the model were mtry = 26, splitrule = gini and min.node.size = 1.




# B3) Fit linear discriminant analysis: model_lda
set.seed(9502)
(boruta_model_lda <- train(
  x = dplyr::select(boruta_final, -blood.test.result),
  y = boruta_final$blood.test.result,
  metric = "ROC", # AUC as the evaluation metric
  method = "lda",
  trControl = myControl_boruta
  # , family = "binomial" # (if train/test manually and observe confusionMatrix)
))


### a) THRESHOLD: 0.5
# If probabilities of predictions (for "hiv negative") exceed threshold of 0.5, "hiv.negative" else "hiv.positive"
lda_predict_boruta_0.5 <- factor(ifelse(predict(boruta_model_lda, type = "prob")$hiv.negative > 0.5,
                                        "hiv.negative", "hiv.positive"), levels = levels(boruta_final$blood.test.result))
# OR
# lda_predict_boruta_0.5 <- predict(boruta_model_lda) # DEFAULT type = "raw"
table(lda_predict_boruta_0.5)
# Create confusion matrix
(cm_lda_predict_boruta_0.5 <- confusionMatrix(lda_predict_boruta_0.5, boruta_final$blood.test.result))

### b) THRESHOLD: 0.8 (trying another threshold)
# If probabilities of predictions (for "hiv negative") exceed threshold of 0.8, "hiv.negative" else "hiv.positive"
lda_predict_boruta_0.8 <- factor(ifelse(predict(boruta_model_lda, type = "prob")$hiv.negative > 0.8,
                                        "hiv.negative", "hiv.positive"), levels = levels(boruta_final$blood.test.result))
table(lda_predict_boruta_0.8)
# Create confusion matrix
(cm_lda_predict_boruta_0.8 <- confusionMatrix(lda_predict_boruta_0.8, boruta_final$blood.test.result))

### c) THRESHOLD: 0.2 (trying another threshold)
# If probabilities of predictions (for "hiv negative") exceed threshold of 0.2, "hiv.negative" else "hiv.positive"
lda_predict_boruta_0.2 <- factor(ifelse(predict(boruta_model_lda, type = "prob")$hiv.negative > 0.2,
                                        "hiv.negative", "hiv.positive"), levels = levels(boruta_final$blood.test.result))
table(lda_predict_boruta_0.2)
# Create confusion matrix
(cm_lda_predict_boruta_0.2 <- confusionMatrix(lda_predict_boruta_0.2, boruta_final$blood.test.result))


# Make ROC curve <hiv.positive>
(roc_lda_predict_boruta <- colAUC(predict(boruta_model_lda, type = "prob")$hiv.positive,
                                  boruta_final$blood.test.result, plotROC = TRUE))
# OR (either): yields the same ROC curve <hiv.negative>
# (roc_lda_predict_boruta <- colAUC(predict(boruta_model_lda, type = "prob")$hiv.negative,
#                                        boruta_final$blood.test.result, plotROC = TRUE))




# B4) Fit support vector machine with a radial kernel: model_svm
set.seed(9503)
(boruta_model_svm <- train(
  x = dplyr::select(boruta_final, -blood.test.result),
  y = boruta_final$blood.test.result,
  metric = "ROC", # AUC as the evaluation metric
  method = "svmRadial",
  trControl = myControl_boruta
  # , family = "binomial" # (if train/test manually and observe confusionMatrix)
))


### a) THRESHOLD: 0.5
# If probabilities of predictions (for "hiv negative") exceed threshold of 0.5, "hiv.negative" else "hiv.positive"
svm_predict_boruta_0.5 <- factor(ifelse(predict(boruta_model_svm, type = "prob")$hiv.negative > 0.5,
                                        "hiv.negative", "hiv.positive"), levels = levels(boruta_final$blood.test.result))
# OR
# svm_predict_boruta_0.5 <- predict(boruta_model_svm) # DEFAULT type = "raw"
table(svm_predict_boruta_0.5)
# Create confusion matrix
(cm_svm_predict_boruta_0.5 <- confusionMatrix(svm_predict_boruta_0.5, boruta_final$blood.test.result))

### b) THRESHOLD: 0.8 (trying another threshold)
# If probabilities of predictions (for "hiv negative") exceed threshold of 0.8, "hiv.negative" else "hiv.positive"
svm_predict_boruta_0.8 <- factor(ifelse(predict(boruta_model_svm, type = "prob")$hiv.negative > 0.8,
                                        "hiv.negative", "hiv.positive"), levels = levels(boruta_final$blood.test.result))
table(svm_predict_boruta_0.8)
# Create confusion matrix
(cm_svm_predict_boruta_0.8 <- confusionMatrix(svm_predict_boruta_0.8, boruta_final$blood.test.result))

### c) THRESHOLD: 0.2 (trying another threshold)
# If probabilities of predictions (for "hiv negative") exceed threshold of 0.2, "hiv.negative" else "hiv.positive"
svm_predict_boruta_0.2 <- factor(ifelse(predict(boruta_model_svm, type = "prob")$hiv.negative > 0.2,
                                        "hiv.negative", "hiv.positive"), levels = levels(boruta_final$blood.test.result))
table(svm_predict_boruta_0.2)
# Create confusion matrix
(cm_svm_predict_boruta_0.2 <- confusionMatrix(svm_predict_boruta_0.2, boruta_final$blood.test.result))


# Make ROC curve <hiv.positive>
(roc_svm_predict_boruta <- colAUC(predict(boruta_model_svm, type = "prob")$hiv.positive,
                                  boruta_final$blood.test.result, plotROC = TRUE))
# OR (either): yields the same ROC curve <hiv.negative>
# (roc_svm_predict_boruta <- colAUC(predict(boruta_model_svm, type = "prob")$hiv.negative,
#                                        boruta_final$blood.test.result, plotROC = TRUE))


# to observe the hyperparameters tuned (grid search/tuning grid)
## C = "Cost"
plot(boruta_model_svm)
## Tuning parameter 'sigma' was held constant at a value of 0.0700876.
## ROC was used to select the optimal model using the largest value.
## The final values used for the model were sigma = 0.0700876 and C = 1.




# B5) Fit extreme gradient boosting: model_xgboost
set.seed(9504)
(boruta_model_xgboost <- train(
  x = dplyr::select(boruta_final, -blood.test.result),
  y = boruta_final$blood.test.result,
  metric = "ROC", # AUC as the evaluation metric
  method = "xgbTree",
  trControl = myControl_boruta
  # , family = "binomial" # (if train/test manually and observe confusionMatrix)
))


### a) THRESHOLD: 0.5
# If probabilities of predictions (for "hiv negative") exceed threshold of 0.5, "hiv.negative" else "hiv.positive"
xgboost_predict_boruta_0.5 <- factor(ifelse(predict(boruta_model_xgboost, type = "prob")$hiv.negative > 0.5,
                                            "hiv.negative", "hiv.positive"), levels = levels(boruta_final$blood.test.result))
# OR
# xgboost_predict_boruta_0.5 <- predict(boruta_model_xgboost) # DEFAULT type = "raw"
table(xgboost_predict_boruta_0.5)
# Create confusion matrix
(cm_xgboost_predict_boruta_0.5 <- confusionMatrix(xgboost_predict_boruta_0.5, boruta_final$blood.test.result))

### b) THRESHOLD: 0.8 (trying another threshold)
# If probabilities of predictions (for "hiv negative") exceed threshold of 0.8, "hiv.negative" else "hiv.positive"
xgboost_predict_boruta_0.8 <- factor(ifelse(predict(boruta_model_xgboost, type = "prob")$hiv.negative > 0.8,
                                            "hiv.negative", "hiv.positive"), levels = levels(boruta_final$blood.test.result))
table(xgboost_predict_boruta_0.8)
# Create confusion matrix
(cm_xgboost_predict_boruta_0.8 <- confusionMatrix(xgboost_predict_boruta_0.8, boruta_final$blood.test.result))

### c) THRESHOLD: 0.2 (trying another threshold)
# If probabilities of predictions (for "hiv negative") exceed threshold of 0.2, "hiv.negative" else "hiv.positive"
xgboost_predict_boruta_0.2 <- factor(ifelse(predict(boruta_model_xgboost, type = "prob")$hiv.negative > 0.2,
                                            "hiv.negative", "hiv.positive"), levels = levels(boruta_final$blood.test.result))
table(xgboost_predict_boruta_0.2)
# Create confusion matrix
(cm_xgboost_predict_boruta_0.2 <- confusionMatrix(xgboost_predict_boruta_0.2, boruta_final$blood.test.result))


# Make ROC curve <hiv.positive>
(roc_xgboost_predict_boruta <- colAUC(predict(boruta_model_xgboost, type = "prob")$hiv.positive,
                                      boruta_final$blood.test.result, plotROC = TRUE))
# OR (either): yields the same ROC curve <hiv.negative>
# (roc_xgboost_predict_boruta <- colAUC(predict(boruta_model_xgboost, type = "prob")$hiv.negative,
#                                        boruta_final$blood.test.result, plotROC = TRUE))


# to observe the hyperparameters tuned (grid search/tuning grid) - {7 tuning parameters altogether}
## max_depth = "Max Tree Depth"; nrounds = "Boosting Iterations"
plot(boruta_model_xgboost)
## Tuning parameter 'gamma' was held constant at a value of 0.
## Tuning parameter 'min_child_weight' was held constant at a value of 1.
## ROC was used to select the optimal model using the largest value.
## The final values used for the model were nrounds = 150, max_depth = 3, eta = 0.4, gamma = 0, colsample_bytree = 0.8,
## min_child_weight = 1 and subsample = 1.




# B6) Fit naive bayes: model_nb  {{{TAKE LONG TIME!!!!!}}}
set.seed(9505)
(boruta_model_nb <- train(
  blood.test.result ~ .,
  data = boruta_final,
  metric = "ROC", # AUC as the evaluation metric
  method = "nb",
  trControl = myControl_boruta
  # , family = "binomial" # (if train/test manually and observe confusionMatrix)
))


### a) THRESHOLD: 0.5
# If probabilities of predictions (for "hiv negative") exceed threshold of 0.5, "hiv.negative" else "hiv.positive"
nb_predict_boruta_0.5 <- factor(ifelse(predict(boruta_model_nb, type = "prob")$hiv.negative > 0.5,
                                       "hiv.negative", "hiv.positive"), levels = levels(boruta_final$blood.test.result))
# OR
# nb_predict_boruta_0.5 <- predict(boruta_model_nb) # DEFAULT type = "raw"
table(nb_predict_boruta_0.5)
# Create confusion matrix
(cm_nb_predict_boruta_0.5 <- confusionMatrix(nb_predict_boruta_0.5, boruta_final$blood.test.result))

### b) THRESHOLD: 0.8 (trying another threshold)
# If probabilities of predictions (for "hiv negative") exceed threshold of 0.8, "hiv.negative" else "hiv.positive"
nb_predict_boruta_0.8 <- factor(ifelse(predict(boruta_model_nb, type = "prob")$hiv.negative > 0.8,
                                       "hiv.negative", "hiv.positive"), levels = levels(boruta_final$blood.test.result))
table(nb_predict_boruta_0.8)
# Create confusion matrix
(cm_nb_predict_boruta_0.8 <- confusionMatrix(nb_predict_boruta_0.8, boruta_final$blood.test.result))

### c) THRESHOLD: 0.2 (trying another threshold)
# If probabilities of predictions (for "hiv negative") exceed threshold of 0.2, "hiv.negative" else "hiv.positive"
nb_predict_boruta_0.2 <- factor(ifelse(predict(boruta_model_nb, type = "prob")$hiv.negative > 0.2,
                                       "hiv.negative", "hiv.positive"), levels = levels(boruta_final$blood.test.result))
table(nb_predict_boruta_0.2)
# Create confusion matrix
(cm_nb_predict_boruta_0.2 <- confusionMatrix(nb_predict_boruta_0.2, boruta_final$blood.test.result))


# Make ROC curve <hiv.positive>
(roc_nb_predict_boruta <- colAUC(predict(boruta_model_nb, type = "prob")$hiv.positive,
                                 boruta_final$blood.test.result, plotROC = TRUE))
# OR (either): yields the same ROC curve <hiv.negative>
# (roc_nb_predict_boruta <- colAUC(predict(boruta_model_nb, type = "prob")$hiv.negative,
#                                        boruta_final$blood.test.result, plotROC = TRUE))


# to observe the hyperparameters tuned (grid search/tuning grid)
## userkernel = FALSE -> "Distribution Type" = "Gaussian"; usekernel = TRUE -> "Distribution Type" = "Nonparametric"
plot(boruta_model_nb)
## Tuning parameter 'fL' was held constant at a value of 0.
## Tuning parameter 'adjust' was held constant at a value of 1.
## ROC was used to select the optimal model using the largest value.
## The final values used for the model were fL = 0, usekernel = TRUE and adjust = 1.




# B7) Fit k-nearest neighbors: model_knn
set.seed(9506)
(boruta_model_knn <- train(
  blood.test.result ~ .,
  data = boruta_final,
  metric = "ROC", # AUC as the evaluation metric
  method = "kknn",
  trControl = myControl_boruta
  # , family = "binomial" # (if train/test manually and observe confusionMatrix)
))


### a) THRESHOLD: 0.5
# If probabilities of predictions (for "hiv negative") exceed threshold of 0.5, "hiv.negative" else "hiv.positive"
knn_predict_boruta_0.5 <- factor(ifelse(predict(boruta_model_knn, type = "prob")$hiv.negative > 0.5,
                                        "hiv.negative", "hiv.positive"), levels = levels(boruta_final$blood.test.result))
# OR
# knn_predict_boruta_0.5 <- predict(boruta_model_knn) # DEFAULT type = "raw"
table(knn_predict_boruta_0.5)
# Create confusion matrix
(cm_knn_predict_boruta_0.5 <- confusionMatrix(knn_predict_boruta_0.5, boruta_final$blood.test.result))

### b) THRESHOLD: 0.8 (trying another threshold)
# If probabilities of predictions (for "hiv negative") exceed threshold of 0.8, "hiv.negative" else "hiv.positive"
knn_predict_boruta_0.8 <- factor(ifelse(predict(boruta_model_knn, type = "prob")$hiv.negative > 0.8,
                                        "hiv.negative", "hiv.positive"), levels = levels(boruta_final$blood.test.result))
table(knn_predict_boruta_0.8)
# Create confusion matrix
(cm_knn_predict_boruta_0.8 <- confusionMatrix(knn_predict_boruta_0.8, boruta_final$blood.test.result))

### c) THRESHOLD: 0.2 (trying another threshold)
# If probabilities of predictions (for "hiv negative") exceed threshold of 0.2, "hiv.negative" else "hiv.positive"
knn_predict_boruta_0.2 <- factor(ifelse(predict(boruta_model_knn, type = "prob")$hiv.negative > 0.2,
                                        "hiv.negative", "hiv.positive"), levels = levels(boruta_final$blood.test.result))
table(knn_predict_boruta_0.2)
# Create confusion matrix
(cm_knn_predict_boruta_0.2 <- confusionMatrix(knn_predict_boruta_0.2, boruta_final$blood.test.result))


# Make ROC curve <hiv.positive>
(roc_knn_predict_boruta <- colAUC(predict(boruta_model_knn, type = "prob")$hiv.positive,
                                  boruta_final$blood.test.result, plotROC = TRUE))
# OR (either): yields the same ROC curve <hiv.negative>
# (roc_knn_predict_boruta <- colAUC(predict(boruta_model_knn, type = "prob")$hiv.negative,
#                                        boruta_final$blood.test.result, plotROC = TRUE))


# to observe the hyperparameters tuned (grid search/tuning grid)
## kmax = "max. #Neighbors"
plot(boruta_model_knn)
## Tuning parameter 'distance' was held constant at a value of 2.
## Tuning parameter 'kernel' was held constant at a value of optimal.
## ROC was used to select the optimal model using the largest value.
## The final values used for the model were kmax = 9, distance = 2 and kernel = optimal.

# plot of misclassification vs. k (when k increases, misclassification increases)
plot(boruta_model_knn$finalModel)




# B8) Fit neural network: model_nn
set.seed(9507)
(boruta_model_nn <- train(
  blood.test.result ~ .,
  data = boruta_final,
  metric = "ROC", # AUC as the evaluation metric
  method = "nnet",
  trControl = myControl_boruta
  # , family = "binomial" # (if train/test manually and observe confusionMatrix)
))


### a) THRESHOLD: 0.5
# If probabilities of predictions (for "hiv negative") exceed threshold of 0.5, "hiv.negative" else "hiv.positive"
nn_predict_boruta_0.5 <- factor(ifelse(predict(boruta_model_nn, type = "prob")$hiv.negative > 0.5,
                                       "hiv.negative", "hiv.positive"), levels = levels(boruta_final$blood.test.result))
# OR
# nn_predict_boruta_0.5 <- predict(boruta_model_nn) # DEFAULT type = "raw"
table(nn_predict_boruta_0.5)
# Create confusion matrix
(cm_nn_predict_boruta_0.5 <- confusionMatrix(nn_predict_boruta_0.5, boruta_final$blood.test.result))

### b) THRESHOLD: 0.8 (trying another threshold)
# If probabilities of predictions (for "hiv negative") exceed threshold of 0.8, "hiv.negative" else "hiv.positive"
nn_predict_boruta_0.8 <- factor(ifelse(predict(boruta_model_nn, type = "prob")$hiv.negative > 0.8,
                                       "hiv.negative", "hiv.positive"), levels = levels(boruta_final$blood.test.result))
table(nn_predict_boruta_0.8)
# Create confusion matrix
(cm_nn_predict_boruta_0.8 <- confusionMatrix(nn_predict_boruta_0.8, boruta_final$blood.test.result))

### c) THRESHOLD: 0.2 (trying another threshold)
# If probabilities of predictions (for "hiv negative") exceed threshold of 0.2, "hiv.negative" else "hiv.positive"
nn_predict_boruta_0.2 <- factor(ifelse(predict(boruta_model_nn, type = "prob")$hiv.negative > 0.2,
                                       "hiv.negative", "hiv.positive"), levels = levels(boruta_final$blood.test.result))
table(nn_predict_boruta_0.2)
# Create confusion matrix
(cm_nn_predict_boruta_0.2 <- confusionMatrix(nn_predict_boruta_0.2, boruta_final$blood.test.result))


# Make ROC curve <hiv.positive>
(roc_nn_predict_boruta <- colAUC(predict(boruta_model_nn, type = "prob")$hiv.positive,
                                 boruta_final$blood.test.result, plotROC = TRUE))
# OR (either): yields the same ROC curve <hiv.negative>
# (roc_nn_predict_boruta <- colAUC(predict(boruta_model_nn, type = "prob")$hiv.negative,
#                                        boruta_final$blood.test.result, plotROC = TRUE))


# to observe the hyperparameters tuned (grid search/tuning grid)
## decay = "Weight Decay"; size = "#Hidden Units"
plot(boruta_model_nn)
## ROC was used to select the optimal model using the largest value.
## The final values used for the model were size = 1 and decay = 0.1.






# (C) Variable Importance from Machine Learning Algorithm - (Classification and Regression Trees) #
# Create custom indices: myFolds
set.seed(80000)
myFolds_rpart <- createFolds(rpart_final$blood.test.result, k = 5) # number of folds: 5

# Create reusable trainControl object: myControl (for a fair comparison of models)
# cross-validation in the model (trainControl) itself (not manually)
set.seed(80001)
## default p = 0.75 (cross-validation split: training percentage)
myControl_rpart <- trainControl(
  summaryFunction = twoClassSummary,
  classProbs = TRUE, # IMPORTANT!
  verboseIter = FALSE,
  savePredictions = TRUE,
  index = myFolds_rpart
)

# to observe the percentage of the target classes for each fold (are there near to each other?)
table(rpart_final$blood.test.result) / length(rpart_final$blood.test.result)
table(rpart_final$blood.test.result[myFolds_rpart$Fold1]) / length(myFolds_rpart$Fold1)
table(rpart_final$blood.test.result[myFolds_rpart$Fold2]) / length(myFolds_rpart$Fold2)
table(rpart_final$blood.test.result[myFolds_rpart$Fold3]) / length(myFolds_rpart$Fold3)
table(rpart_final$blood.test.result[myFolds_rpart$Fold4]) / length(myFolds_rpart$Fold4)
table(rpart_final$blood.test.result[myFolds_rpart$Fold5]) / length(myFolds_rpart$Fold5)


# C1) Fit elastic net model: model_glmnet
set.seed(95000)
(rpart_model_glmnet <- train(
  x = dplyr::select(rpart_final, -blood.test.result),
  y = rpart_final$blood.test.result,
  metric = "ROC", # AUC as the evaluation metric
  method = "glmnet",
  trControl = myControl_rpart,
  # "zv": to remove constant columns; "nzv": to remove nearly constant columns ["pca" as an alternative - it is better]
  # "medianImpute": if missing at random (MAR)
  # "knnImpute": if missing not at random (MNAR)
  # "BoxCox" or "YeoJohnson": for transformation
  # "spatialSign": to sometimes replace "pca" [convert numeric data into a projection on to a unit sphere]
  preProcess = c("center", "scale", "pca")
  # , family = "binomial" # (if train/test manually and observe confusionMatrix)
  # , sampling = c("smote", "rose") # (if sampling was not performed priorly)
))


### a) THRESHOLD: 0.5
# If probabilities of predictions (for "hiv negative") exceed threshold of 0.5, "hiv.negative" else "hiv.positive"
glmnet_predict_rpart_0.5 <- factor(ifelse(predict(rpart_model_glmnet, type = "prob")$hiv.negative > 0.5,
                                          "hiv.negative", "hiv.positive"), levels = levels(rpart_final$blood.test.result))
# OR
# glmnet_predict_rpart_0.5 <- predict(rpart_model_glmnet) # DEFAULT type = "raw"
table(glmnet_predict_rpart_0.5)
# Create confusion matrix
(cm_glmnet_predict_rpart_0.5 <- confusionMatrix(glmnet_predict_rpart_0.5, rpart_final$blood.test.result))

### b) THRESHOLD: 0.8 (trying another threshold)
# If probabilities of predictions (for "hiv negative") exceed threshold of 0.8, "hiv.negative" else "hiv.positive"
glmnet_predict_rpart_0.8 <- factor(ifelse(predict(rpart_model_glmnet, type = "prob")$hiv.negative > 0.8,
                                          "hiv.negative", "hiv.positive"), levels = levels(rpart_final$blood.test.result))
table(glmnet_predict_rpart_0.8)
# Create confusion matrix
(cm_glmnet_predict_rpart_0.8 <- confusionMatrix(glmnet_predict_rpart_0.8, rpart_final$blood.test.result))

### c) THRESHOLD: 0.2 (trying another threshold)
# If probabilities of predictions (for "hiv negative") exceed threshold of 0.2, "hiv.negative" else "hiv.positive"
glmnet_predict_rpart_0.2 <- factor(ifelse(predict(rpart_model_glmnet, type = "prob")$hiv.negative > 0.2,
                                          "hiv.negative", "hiv.positive"), levels = levels(rpart_final$blood.test.result))
table(glmnet_predict_rpart_0.2)
# Create confusion matrix
(cm_glmnet_predict_rpart_0.2 <- confusionMatrix(glmnet_predict_rpart_0.2, rpart_final$blood.test.result))


# Make ROC curve <hiv.positive>
(roc_glmnet_predict_rpart <- colAUC(predict(rpart_model_glmnet, type = "prob")$hiv.positive,
                                    rpart_final$blood.test.result, plotROC = TRUE))
# OR (either): yields the same ROC curve <hiv.negative>
# (roc_glmnet_predict_rpart <- colAUC(predict(rpart_model_glmnet, type = "prob")$hiv.negative,
#                                        rpart_final$blood.test.result, plotROC = TRUE))



### NOTES:
### Lasso regression (1): penalizes number of non-zero coefficients [number]
### Ridge regression (0): penalizes absolute magnitude of coefficients [size]
### 'glmnet' is the combination of lasso and ridge regression where:
### alpha = 0 is pure ridge regression, alpha = 1 is pure lasso regression [Mixing Percentage]
### lambda is the size of the penalty [Regularization Parameter]
###

# to observe the hyperparameters tuned (grid search/tuning grid)
plot(rpart_model_glmnet)
## ROC was used to select the optimal model using the largest value.
## The final values used for the model were alpha = 1 [pure lasso] and lambda = 0.001950662.

# to observe the full regularization path for all of the models with alpha = 0 [pure ridge]
## Left: intercept only model (high value of lambda)
## Right: full model with no penalty (low value of lambda)
### The plot shows how the regression coefficients are "shrunk" from right to left as we increase the strength of the penalty
### on coefficient size, and therefore decrease the complexity of the model.
### We can also see some lines hitting zero as we increase lambda, which represents these coefficients dropping out of the model.
plot(rpart_model_glmnet$finalModel)




# C2) Fit random forest: model_rf
set.seed(95001)
(rpart_model_rf <- train(
  x = dplyr::select(rpart_final, -blood.test.result),
  y = rpart_final$blood.test.result,
  tuneLength = 5, # the maximum number of tuning parameter combinations that will be generated by the random search
  metric = "ROC", # AUC as the evaluation metric
  method = "ranger", # use "ranger" instead of "rf" - faster and more effective
  trControl = myControl_rpart
  # , family = "binomial" # (if train/test manually and observe confusionMatrix)
))


### a) THRESHOLD: 0.5
# If probabilities of predictions (for "hiv negative") exceed threshold of 0.5, "hiv.negative" else "hiv.positive"
rf_predict_rpart_0.5 <- factor(ifelse(predict(rpart_model_rf, type = "prob")$hiv.negative > 0.5,
                                      "hiv.negative", "hiv.positive"), levels = levels(rpart_final$blood.test.result))
# OR
# rf_predict_rpart_0.5 <- predict(rpart_model_rf) # DEFAULT type = "raw"
table(rf_predict_rpart_0.5)
# Create confusion matrix
(cm_rf_predict_rpart_0.5 <- confusionMatrix(rf_predict_rpart_0.5, rpart_final$blood.test.result))

### b) THRESHOLD: 0.8 (trying another threshold)
# If probabilities of predictions (for "hiv negative") exceed threshold of 0.8, "hiv.negative" else "hiv.positive"
rf_predict_rpart_0.8 <- factor(ifelse(predict(rpart_model_rf, type = "prob")$hiv.negative > 0.8,
                                      "hiv.negative", "hiv.positive"), levels = levels(rpart_final$blood.test.result))
table(rf_predict_rpart_0.8)
# Create confusion matrix
(cm_rf_predict_rpart_0.8 <- confusionMatrix(rf_predict_rpart_0.8, rpart_final$blood.test.result))

### c) THRESHOLD: 0.2 (trying another threshold)
# If probabilities of predictions (for "hiv negative") exceed threshold of 0.2, "hiv.negative" else "hiv.positive"
rf_predict_rpart_0.2 <- factor(ifelse(predict(rpart_model_rf, type = "prob")$hiv.negative > 0.2,
                                      "hiv.negative", "hiv.positive"), levels = levels(rpart_final$blood.test.result))
table(rf_predict_rpart_0.2)
# Create confusion matrix
(cm_rf_predict_rpart_0.2 <- confusionMatrix(rf_predict_rpart_0.2, rpart_final$blood.test.result))


# Make ROC curve <hiv.positive>
(roc_rf_predict_rpart <- colAUC(predict(rpart_model_rf, type = "prob")$hiv.positive,
                                rpart_final$blood.test.result, plotROC = TRUE))
# OR (either): yields the same ROC curve <hiv.negative>
# (roc_rf_predict_rpart <- colAUC(predict(rpart_model_rf, type = "prob")$hiv.negative,
#                                        rpart_final$blood.test.result, plotROC = TRUE))


# to observe the hyperparameters tuned (grid search/tuning grid)
## mtry = "#Randomly Selected Predictors"; splitrule = "Splitting Rule"
plot(rpart_model_rf)
## Tuning parameter 'min.node.size' was held constant at a value of 1.
## ROC was used to select the optimal model using the largest value.
## The final values used for the model were mtry = 7, splitrule = gini and min.node.size = 1.




# C3) Fit linear discriminant analysis: model_lda
set.seed(95002)
(rpart_model_lda <- train(
  x = dplyr::select(rpart_final, -blood.test.result),
  y = rpart_final$blood.test.result,
  metric = "ROC", # AUC as the evaluation metric
  method = "lda",
  trControl = myControl_rpart
  # , family = "binomial" # (if train/test manually and observe confusionMatrix)
))


### a) THRESHOLD: 0.5
# If probabilities of predictions (for "hiv negative") exceed threshold of 0.5, "hiv.negative" else "hiv.positive"
lda_predict_rpart_0.5 <- factor(ifelse(predict(rpart_model_lda, type = "prob")$hiv.negative > 0.5,
                                       "hiv.negative", "hiv.positive"), levels = levels(rpart_final$blood.test.result))
# OR
# lda_predict_rpart_0.5 <- predict(rpart_model_lda) # DEFAULT type = "raw"
table(lda_predict_rpart_0.5)
# Create confusion matrix
(cm_lda_predict_rpart_0.5 <- confusionMatrix(lda_predict_rpart_0.5, rpart_final$blood.test.result))

### b) THRESHOLD: 0.8 (trying another threshold)
# If probabilities of predictions (for "hiv negative") exceed threshold of 0.8, "hiv.negative" else "hiv.positive"
lda_predict_rpart_0.8 <- factor(ifelse(predict(rpart_model_lda, type = "prob")$hiv.negative > 0.8,
                                       "hiv.negative", "hiv.positive"), levels = levels(rpart_final$blood.test.result))
table(lda_predict_rpart_0.8)
# Create confusion matrix
(cm_lda_predict_rpart_0.8 <- confusionMatrix(lda_predict_rpart_0.8, rpart_final$blood.test.result))

### c) THRESHOLD: 0.2 (trying another threshold)
# If probabilities of predictions (for "hiv negative") exceed threshold of 0.2, "hiv.negative" else "hiv.positive"
lda_predict_rpart_0.2 <- factor(ifelse(predict(rpart_model_lda, type = "prob")$hiv.negative > 0.2,
                                       "hiv.negative", "hiv.positive"), levels = levels(rpart_final$blood.test.result))
table(lda_predict_rpart_0.2)
# Create confusion matrix
(cm_lda_predict_rpart_0.2 <- confusionMatrix(lda_predict_rpart_0.2, rpart_final$blood.test.result))


# Make ROC curve <hiv.positive>
(roc_lda_predict_rpart <- colAUC(predict(rpart_model_lda, type = "prob")$hiv.positive,
                                 rpart_final$blood.test.result, plotROC = TRUE))
# OR (either): yields the same ROC curve <hiv.negative>
# (roc_lda_predict_rpart <- colAUC(predict(rpart_model_lda, type = "prob")$hiv.negative,
#                                        rpart_final$blood.test.result, plotROC = TRUE))




# C4) Fit support vector machine with a radial kernel: model_svm
set.seed(95003)
(rpart_model_svm <- train(
  x = dplyr::select(rpart_final, -blood.test.result),
  y = rpart_final$blood.test.result,
  metric = "ROC", # AUC as the evaluation metric
  method = "svmRadial",
  trControl = myControl_rpart
  # , family = "binomial" # (if train/test manually and observe confusionMatrix)
))


### a) THRESHOLD: 0.5
# If probabilities of predictions (for "hiv negative") exceed threshold of 0.5, "hiv.negative" else "hiv.positive"
svm_predict_rpart_0.5 <- factor(ifelse(predict(rpart_model_svm, type = "prob")$hiv.negative > 0.5,
                                       "hiv.negative", "hiv.positive"), levels = levels(rpart_final$blood.test.result))
# OR
# svm_predict_rpart_0.5 <- predict(rpart_model_svm) # DEFAULT type = "raw"
table(svm_predict_rpart_0.5)
# Create confusion matrix
(cm_svm_predict_rpart_0.5 <- confusionMatrix(svm_predict_rpart_0.5, rpart_final$blood.test.result))

### b) THRESHOLD: 0.8 (trying another threshold)
# If probabilities of predictions (for "hiv negative") exceed threshold of 0.8, "hiv.negative" else "hiv.positive"
svm_predict_rpart_0.8 <- factor(ifelse(predict(rpart_model_svm, type = "prob")$hiv.negative > 0.8,
                                       "hiv.negative", "hiv.positive"), levels = levels(rpart_final$blood.test.result))
table(svm_predict_rpart_0.8)
# Create confusion matrix
(cm_svm_predict_rpart_0.8 <- confusionMatrix(svm_predict_rpart_0.8, rpart_final$blood.test.result))

### c) THRESHOLD: 0.2 (trying another threshold)
# If probabilities of predictions (for "hiv negative") exceed threshold of 0.2, "hiv.negative" else "hiv.positive"
svm_predict_rpart_0.2 <- factor(ifelse(predict(rpart_model_svm, type = "prob")$hiv.negative > 0.2,
                                       "hiv.negative", "hiv.positive"), levels = levels(rpart_final$blood.test.result))
table(svm_predict_rpart_0.2)
# Create confusion matrix
(cm_svm_predict_rpart_0.2 <- confusionMatrix(svm_predict_rpart_0.2, rpart_final$blood.test.result))


# Make ROC curve <hiv.positive>
(roc_svm_predict_rpart <- colAUC(predict(rpart_model_svm, type = "prob")$hiv.positive,
                                 rpart_final$blood.test.result, plotROC = TRUE))
# OR (either): yields the same ROC curve <hiv.negative>
# (roc_svm_predict_rpart <- colAUC(predict(rpart_model_svm, type = "prob")$hiv.negative,
#                                        rpart_final$blood.test.result, plotROC = TRUE))


# to observe the hyperparameters tuned (grid search/tuning grid)
## C = "Cost"
plot(rpart_model_svm)
## Tuning parameter 'sigma' was held constant at a value of 0.1252481.
## ROC was used to select the optimal model using the largest value.
## The final values used for the model were sigma = 0.1252481 and C = 1.




# C5) Fit extreme gradient boosting: model_xgboost
set.seed(95004)
(rpart_model_xgboost <- train(
  x = dplyr::select(rpart_final, -blood.test.result),
  y = rpart_final$blood.test.result,
  metric = "ROC", # AUC as the evaluation metric
  method = "xgbTree",
  trControl = myControl_rpart
  # , family = "binomial" # (if train/test manually and observe confusionMatrix)
))


### a) THRESHOLD: 0.5
# If probabilities of predictions (for "hiv negative") exceed threshold of 0.5, "hiv.negative" else "hiv.positive"
xgboost_predict_rpart_0.5 <- factor(ifelse(predict(rpart_model_xgboost, type = "prob")$hiv.negative > 0.5,
                                           "hiv.negative", "hiv.positive"), levels = levels(rpart_final$blood.test.result))
# OR
# xgboost_predict_rpart_0.5 <- predict(rpart_model_xgboost) # DEFAULT type = "raw"
table(xgboost_predict_rpart_0.5)
# Create confusion matrix
(cm_xgboost_predict_rpart_0.5 <- confusionMatrix(xgboost_predict_rpart_0.5, rpart_final$blood.test.result))

### b) THRESHOLD: 0.8 (trying another threshold)
# If probabilities of predictions (for "hiv negative") exceed threshold of 0.8, "hiv.negative" else "hiv.positive"
xgboost_predict_rpart_0.8 <- factor(ifelse(predict(rpart_model_xgboost, type = "prob")$hiv.negative > 0.8,
                                           "hiv.negative", "hiv.positive"), levels = levels(rpart_final$blood.test.result))
table(xgboost_predict_rpart_0.8)
# Create confusion matrix
(cm_xgboost_predict_rpart_0.8 <- confusionMatrix(xgboost_predict_rpart_0.8, rpart_final$blood.test.result))

### c) THRESHOLD: 0.2 (trying another threshold)
# If probabilities of predictions (for "hiv negative") exceed threshold of 0.2, "hiv.negative" else "hiv.positive"
xgboost_predict_rpart_0.2 <- factor(ifelse(predict(rpart_model_xgboost, type = "prob")$hiv.negative > 0.2,
                                           "hiv.negative", "hiv.positive"), levels = levels(rpart_final$blood.test.result))
table(xgboost_predict_rpart_0.2)
# Create confusion matrix
(cm_xgboost_predict_rpart_0.2 <- confusionMatrix(xgboost_predict_rpart_0.2, rpart_final$blood.test.result))


# Make ROC curve <hiv.positive>
(roc_xgboost_predict_rpart <- colAUC(predict(rpart_model_xgboost, type = "prob")$hiv.positive,
                                     rpart_final$blood.test.result, plotROC = TRUE))
# OR (either): yields the same ROC curve <hiv.negative>
# (roc_xgboost_predict_rpart <- colAUC(predict(rpart_model_xgboost, type = "prob")$hiv.negative,
#                                        rpart_final$blood.test.result, plotROC = TRUE))


# to observe the hyperparameters tuned (grid search/tuning grid) - {7 tuning parameters altogether}
## max_depth = "Max Tree Depth"; nrounds = "Boosting Iterations"
plot(rpart_model_xgboost)
## Tuning parameter 'gamma' was held constant at a value of 0.
## Tuning parameter 'min_child_weight' was held constant at a value of 1.
## ROC was used to select the optimal model using the largest value.
## The final values used for the model were nrounds = 100, max_depth = 3, eta = 0.3, gamma = 0, colsample_bytree = 0.6,
## min_child_weight = 1 and subsample = 1.




# C6) Fit naive bayes: model_nb  {{{TAKE LONG TIME!!!!!}}}
set.seed(95005)
(rpart_model_nb <- train(
  blood.test.result ~ .,
  data = rpart_final,
  metric = "ROC", # AUC as the evaluation metric
  method = "nb",
  trControl = myControl_rpart
  # , family = "binomial" # (if train/test manually and observe confusionMatrix)
))


### a) THRESHOLD: 0.5
# If probabilities of predictions (for "hiv negative") exceed threshold of 0.5, "hiv.negative" else "hiv.positive"
nb_predict_rpart_0.5 <- factor(ifelse(predict(rpart_model_nb, type = "prob")$hiv.negative > 0.5,
                                      "hiv.negative", "hiv.positive"), levels = levels(rpart_final$blood.test.result))
# OR
# nb_predict_rpart_0.5 <- predict(rpart_model_nb) # DEFAULT type = "raw"
table(nb_predict_rpart_0.5)
# Create confusion matrix
(cm_nb_predict_rpart_0.5 <- confusionMatrix(nb_predict_rpart_0.5, rpart_final$blood.test.result))

### b) THRESHOLD: 0.8 (trying another threshold)
# If probabilities of predictions (for "hiv negative") exceed threshold of 0.8, "hiv.negative" else "hiv.positive"
nb_predict_rpart_0.8 <- factor(ifelse(predict(rpart_model_nb, type = "prob")$hiv.negative > 0.8,
                                      "hiv.negative", "hiv.positive"), levels = levels(rpart_final$blood.test.result))
table(nb_predict_rpart_0.8)
# Create confusion matrix
(cm_nb_predict_rpart_0.8 <- confusionMatrix(nb_predict_rpart_0.8, rpart_final$blood.test.result))

### c) THRESHOLD: 0.2 (trying another threshold)
# If probabilities of predictions (for "hiv negative") exceed threshold of 0.2, "hiv.negative" else "hiv.positive"
nb_predict_rpart_0.2 <- factor(ifelse(predict(rpart_model_nb, type = "prob")$hiv.negative > 0.2,
                                      "hiv.negative", "hiv.positive"), levels = levels(rpart_final$blood.test.result))
table(nb_predict_rpart_0.2)
# Create confusion matrix
(cm_nb_predict_rpart_0.2 <- confusionMatrix(nb_predict_rpart_0.2, rpart_final$blood.test.result))


# Make ROC curve <hiv.positive>
(roc_nb_predict_rpart <- colAUC(predict(rpart_model_nb, type = "prob")$hiv.positive,
                                rpart_final$blood.test.result, plotROC = TRUE))
# OR (either): yields the same ROC curve <hiv.negative>
# (roc_nb_predict_rpart <- colAUC(predict(rpart_model_nb, type = "prob")$hiv.negative,
#                                        rpart_final$blood.test.result, plotROC = TRUE))


# to observe the hyperparameters tuned (grid search/tuning grid)
## userkernel = FALSE -> "Distribution Type" = "Gaussian"; usekernel = TRUE -> "Distribution Type" = "Nonparametric"
plot(rpart_model_nb)
## Tuning parameter 'fL' was held constant at a value of 0.
## Tuning parameter 'adjust' was held constant at a value of 1.
## ROC was used to select the optimal model using the largest value.
## The final values used for the model were fL = 0, usekernel = TRUE and adjust = 1.




# C7) Fit k-nearest neighbors: model_knn
set.seed(95006)
(rpart_model_knn <- train(
  blood.test.result ~ .,
  data = rpart_final,
  metric = "ROC", # AUC as the evaluation metric
  method = "kknn",
  trControl = myControl_rpart
  # , family = "binomial" # (if train/test manually and observe confusionMatrix)
))


### a) THRESHOLD: 0.5
# If probabilities of predictions (for "hiv negative") exceed threshold of 0.5, "hiv.negative" else "hiv.positive"
knn_predict_rpart_0.5 <- factor(ifelse(predict(rpart_model_knn, type = "prob")$hiv.negative > 0.5,
                                       "hiv.negative", "hiv.positive"), levels = levels(rpart_final$blood.test.result))
# OR
# knn_predict_rpart_0.5 <- predict(rpart_model_knn) # DEFAULT type = "raw"
table(knn_predict_rpart_0.5)
# Create confusion matrix
(cm_knn_predict_rpart_0.5 <- confusionMatrix(knn_predict_rpart_0.5, rpart_final$blood.test.result))

### b) THRESHOLD: 0.8 (trying another threshold)
# If probabilities of predictions (for "hiv negative") exceed threshold of 0.8, "hiv.negative" else "hiv.positive"
knn_predict_rpart_0.8 <- factor(ifelse(predict(rpart_model_knn, type = "prob")$hiv.negative > 0.8,
                                       "hiv.negative", "hiv.positive"), levels = levels(rpart_final$blood.test.result))
table(knn_predict_rpart_0.8)
# Create confusion matrix
(cm_knn_predict_rpart_0.8 <- confusionMatrix(knn_predict_rpart_0.8, rpart_final$blood.test.result))

### c) THRESHOLD: 0.2 (trying another threshold)
# If probabilities of predictions (for "hiv negative") exceed threshold of 0.2, "hiv.negative" else "hiv.positive"
knn_predict_rpart_0.2 <- factor(ifelse(predict(rpart_model_knn, type = "prob")$hiv.negative > 0.2,
                                       "hiv.negative", "hiv.positive"), levels = levels(rpart_final$blood.test.result))
table(knn_predict_rpart_0.2)
# Create confusion matrix
(cm_knn_predict_rpart_0.2 <- confusionMatrix(knn_predict_rpart_0.2, rpart_final$blood.test.result))


# Make ROC curve <hiv.positive>
(roc_knn_predict_rpart <- colAUC(predict(rpart_model_knn, type = "prob")$hiv.positive,
                                 rpart_final$blood.test.result, plotROC = TRUE))
# OR (either): yields the same ROC curve <hiv.negative>
# (roc_knn_predict_rpart <- colAUC(predict(rpart_model_knn, type = "prob")$hiv.negative,
#                                        rpart_final$blood.test.result, plotROC = TRUE))


# to observe the hyperparameters tuned (grid search/tuning grid)
## kmax = "max. #Neighbors"
plot(rpart_model_knn)
## Tuning parameter 'distance' was held constant at a value of 2.
## Tuning parameter 'kernel' was held constant at a value of optimal.
## ROC was used to select the optimal model using the largest value.
## The final values used for the model were kmax = 9, distance = 2 and kernel = optimal.

# plot of misclassification vs. k (when k increases, misclassification {decreases})
plot(rpart_model_knn$finalModel)




# C8) Fit neural network: model_nn
set.seed(95007)
(rpart_model_nn <- train(
  blood.test.result ~ .,
  data = rpart_final,
  metric = "ROC", # AUC as the evaluation metric
  method = "nnet",
  trControl = myControl_rpart
  # , family = "binomial" # (if train/test manually and observe confusionMatrix)
))


### a) THRESHOLD: 0.5
# If probabilities of predictions (for "hiv negative") exceed threshold of 0.5, "hiv.negative" else "hiv.positive"
nn_predict_rpart_0.5 <- factor(ifelse(predict(rpart_model_nn, type = "prob")$hiv.negative > 0.5,
                                      "hiv.negative", "hiv.positive"), levels = levels(rpart_final$blood.test.result))
# OR
# nn_predict_rpart_0.5 <- predict(rpart_model_nn) # DEFAULT type = "raw"
table(nn_predict_rpart_0.5)
# Create confusion matrix
(cm_nn_predict_rpart_0.5 <- confusionMatrix(nn_predict_rpart_0.5, rpart_final$blood.test.result))

### b) THRESHOLD: 0.8 (trying another threshold)
# If probabilities of predictions (for "hiv negative") exceed threshold of 0.8, "hiv.negative" else "hiv.positive"
nn_predict_rpart_0.8 <- factor(ifelse(predict(rpart_model_nn, type = "prob")$hiv.negative > 0.8,
                                      "hiv.negative", "hiv.positive"), levels = levels(rpart_final$blood.test.result))
table(nn_predict_rpart_0.8)
# Create confusion matrix
(cm_nn_predict_rpart_0.8 <- confusionMatrix(nn_predict_rpart_0.8, rpart_final$blood.test.result))

### c) THRESHOLD: 0.2 (trying another threshold)
# If probabilities of predictions (for "hiv negative") exceed threshold of 0.2, "hiv.negative" else "hiv.positive"
nn_predict_rpart_0.2 <- factor(ifelse(predict(rpart_model_nn, type = "prob")$hiv.negative > 0.2,
                                      "hiv.negative", "hiv.positive"), levels = levels(rpart_final$blood.test.result))
table(nn_predict_rpart_0.2)
# Create confusion matrix
(cm_nn_predict_rpart_0.2 <- confusionMatrix(nn_predict_rpart_0.2, rpart_final$blood.test.result))


# Make ROC curve <hiv.positive>
(roc_nn_predict_rpart <- colAUC(predict(rpart_model_nn, type = "prob")$hiv.positive,
                                rpart_final$blood.test.result, plotROC = TRUE))
# OR (either): yields the same ROC curve <hiv.negative>
# (roc_nn_predict_rpart <- colAUC(predict(rpart_model_nn, type = "prob")$hiv.negative,
#                                        rpart_final$blood.test.result, plotROC = TRUE))


# to observe the hyperparameters tuned (grid search/tuning grid)
## decay = "Weight Decay"; size = "#Hidden Units"
plot(rpart_model_nn)
## ROC was used to select the optimal model using the largest value.
## The final values used for the model were size = 5 and decay = 0.1.






# (D) Pearson's Chi-squared Test #
# Create custom indices: myFolds
set.seed(800000)
myFolds_chi_sq <- createFolds(chi_sq_final$blood.test.result, k = 5) # number of folds: 5

# Create reusable trainControl object: myControl (for a fair comparison of models)
# cross-validation in the model (trainControl) itself (not manually)
set.seed(800001)
## default p = 0.75 (cross-validation split: training percentage)
myControl_chi_sq <- trainControl(
  summaryFunction = twoClassSummary,
  classProbs = TRUE, # IMPORTANT!
  verboseIter = FALSE,
  savePredictions = TRUE,
  index = myFolds_chi_sq
)

# to observe the percentage of the target classes for each fold (are they near to each other?)
table(chi_sq_final$blood.test.result) / length(chi_sq_final$blood.test.result)
table(chi_sq_final$blood.test.result[myFolds_chi_sq$Fold1]) / length(myFolds_chi_sq$Fold1)
table(chi_sq_final$blood.test.result[myFolds_chi_sq$Fold2]) / length(myFolds_chi_sq$Fold2)
table(chi_sq_final$blood.test.result[myFolds_chi_sq$Fold3]) / length(myFolds_chi_sq$Fold3)
table(chi_sq_final$blood.test.result[myFolds_chi_sq$Fold4]) / length(myFolds_chi_sq$Fold4)
table(chi_sq_final$blood.test.result[myFolds_chi_sq$Fold5]) / length(myFolds_chi_sq$Fold5)


# D1) Fit elastic net model: model_glmnet
set.seed(950000)
(chi_sq_model_glmnet <- train(
  x = dplyr::select(chi_sq_final, -blood.test.result),
  y = chi_sq_final$blood.test.result,
  metric = "ROC", # AUC as the evaluation metric
  method = "glmnet",
  trControl = myControl_chi_sq,
  # "zv": to remove constant columns; "nzv": to remove nearly constant columns ["pca" as an alternative - it is better]
  # "medianImpute": if missing at random (MAR)
  # "knnImpute": if missing not at random (MNAR)
  # "BoxCox" or "YeoJohnson": for transformation
  # "spatialSign": to sometimes replace "pca" [convert numeric data into a projection on to a unit sphere]
  preProcess = c("center", "scale", "pca")
  # , family = "binomial" # (if train/test manually and observe confusionMatrix)
  # , sampling = c("smote", "rose") # (if sampling was not performed priorly)
))


### a) THRESHOLD: 0.5
# If probabilities of predictions (for "hiv negative") exceed threshold of 0.5, "hiv.negative" else "hiv.positive"
glmnet_predict_chi_sq_0.5 <- factor(ifelse(predict(chi_sq_model_glmnet, type = "prob")$hiv.negative > 0.5,
                                           "hiv.negative", "hiv.positive"), levels = levels(chi_sq_final$blood.test.result))
# OR
# glmnet_predict_chi_sq_0.5 <- predict(chi_sq_model_glmnet) # DEFAULT type = "raw"
table(glmnet_predict_chi_sq_0.5)
# Create confusion matrix
(cm_glmnet_predict_chi_sq_0.5 <- confusionMatrix(glmnet_predict_chi_sq_0.5, chi_sq_final$blood.test.result))

### b) THRESHOLD: 0.8 (trying another threshold)
# If probabilities of predictions (for "hiv negative") exceed threshold of 0.8, "hiv.negative" else "hiv.positive"
glmnet_predict_chi_sq_0.8 <- factor(ifelse(predict(chi_sq_model_glmnet, type = "prob")$hiv.negative > 0.8,
                                           "hiv.negative", "hiv.positive"), levels = levels(chi_sq_final$blood.test.result))
table(glmnet_predict_chi_sq_0.8)
# Create confusion matrix
(cm_glmnet_predict_chi_sq_0.8 <- confusionMatrix(glmnet_predict_chi_sq_0.8, chi_sq_final$blood.test.result))

### c) THRESHOLD: 0.2 (trying another threshold)
# If probabilities of predictions (for "hiv negative") exceed threshold of 0.2, "hiv.negative" else "hiv.positive"
glmnet_predict_chi_sq_0.2 <- factor(ifelse(predict(chi_sq_model_glmnet, type = "prob")$hiv.negative > 0.2,
                                           "hiv.negative", "hiv.positive"), levels = levels(chi_sq_final$blood.test.result))
table(glmnet_predict_chi_sq_0.2)
# Create confusion matrix
(cm_glmnet_predict_chi_sq_0.2 <- confusionMatrix(glmnet_predict_chi_sq_0.2, chi_sq_final$blood.test.result))


# Make ROC curve <hiv.positive>
(roc_glmnet_predict_chi_sq <- colAUC(predict(chi_sq_model_glmnet, type = "prob")$hiv.positive,
                                     chi_sq_final$blood.test.result, plotROC = TRUE))
# OR (either): yields the same ROC curve <hiv.negative>
# (roc_glmnet_predict_chi_sq <- colAUC(predict(chi_sq_model_glmnet, type = "prob")$hiv.negative,
#                                        chi_sq_final$blood.test.result, plotROC = TRUE))



### NOTES:
### Lasso regression (1): penalizes number of non-zero coefficients [number]
### Ridge regression (0): penalizes absolute magnitude of coefficients [size]
### 'glmnet' is the combination of lasso and ridge regression where:
### alpha = 0 is pure ridge regression, alpha = 1 is pure lasso regression [Mixing Percentage]
### lambda is the size of the penalty [Regularization Parameter]
###

# to observe the hyperparameters tuned (grid search/tuning grid)
plot(chi_sq_model_glmnet)
## ROC was used to select the optimal model using the largest value.
## The final values used for the model were alpha = 0.1 and lambda = 0.002034337.

# to observe the full regularization path for all of the models with alpha = 0 [pure ridge]
## Left: intercept only model (high value of lambda)
## Right: full model with no penalty (low value of lambda)
### The plot shows how the regression coefficients are "shrunk" from right to left as we increase the strength of the penalty
### on coefficient size, and therefore decrease the complexity of the model.
### We can also see some lines hitting zero as we increase lambda, which represents these coefficients dropping out of the model.
plot(chi_sq_model_glmnet$finalModel)




# D2) Fit random forest: model_rf
set.seed(950001)
(chi_sq_model_rf <- train(
  x = dplyr::select(chi_sq_final, -blood.test.result),
  y = chi_sq_final$blood.test.result,
  tuneLength = 5, # the maximum number of tuning parameter combinations that will be generated by the random search
  metric = "ROC", # AUC as the evaluation metric
  method = "ranger", # use "ranger" instead of "rf" - faster and more effective
  trControl = myControl_chi_sq
  # , family = "binomial" # (if train/test manually and observe confusionMatrix)
))


### a) THRESHOLD: 0.5
# If probabilities of predictions (for "hiv negative") exceed threshold of 0.5, "hiv.negative" else "hiv.positive"
rf_predict_chi_sq_0.5 <- factor(ifelse(predict(chi_sq_model_rf, type = "prob")$hiv.negative > 0.5,
                                       "hiv.negative", "hiv.positive"), levels = levels(chi_sq_final$blood.test.result))
# OR
# rf_predict_chi_sq_0.5 <- predict(chi_sq_model_rf) # DEFAULT type = "raw"
table(rf_predict_chi_sq_0.5)
# Create confusion matrix
(cm_rf_predict_chi_sq_0.5 <- confusionMatrix(rf_predict_chi_sq_0.5, chi_sq_final$blood.test.result))

### b) THRESHOLD: 0.8 (trying another threshold)
# If probabilities of predictions (for "hiv negative") exceed threshold of 0.8, "hiv.negative" else "hiv.positive"
rf_predict_chi_sq_0.8 <- factor(ifelse(predict(chi_sq_model_rf, type = "prob")$hiv.negative > 0.8,
                                       "hiv.negative", "hiv.positive"), levels = levels(chi_sq_final$blood.test.result))
table(rf_predict_chi_sq_0.8)
# Create confusion matrix
(cm_rf_predict_chi_sq_0.8 <- confusionMatrix(rf_predict_chi_sq_0.8, chi_sq_final$blood.test.result))

### c) THRESHOLD: 0.2 (trying another threshold)
# If probabilities of predictions (for "hiv negative") exceed threshold of 0.2, "hiv.negative" else "hiv.positive"
rf_predict_chi_sq_0.2 <- factor(ifelse(predict(chi_sq_model_rf, type = "prob")$hiv.negative > 0.2,
                                       "hiv.negative", "hiv.positive"), levels = levels(chi_sq_final$blood.test.result))
table(rf_predict_chi_sq_0.2)
# Create confusion matrix
(cm_rf_predict_chi_sq_0.2 <- confusionMatrix(rf_predict_chi_sq_0.2, chi_sq_final$blood.test.result))


# Make ROC curve <hiv.positive>
(roc_rf_predict_chi_sq <- colAUC(predict(chi_sq_model_rf, type = "prob")$hiv.positive,
                                 chi_sq_final$blood.test.result, plotROC = TRUE))
# OR (either): yields the same ROC curve <hiv.negative>
# (roc_rf_predict_chi_sq <- colAUC(predict(chi_sq_model_rf, type = "prob")$hiv.negative,
#                                        chi_sq_final$blood.test.result, plotROC = TRUE))


# to observe the hyperparameters tuned (grid search/tuning grid)
## mtry = "#Randomly Selected Predictors"; splitrule = "Splitting Rule"
plot(chi_sq_model_rf)
## Tuning parameter 'min.node.size' was held constant at a value of 1.
## ROC was used to select the optimal model using the largest value.
## The final values used for the model were mtry = 3, splitrule = gini and min.node.size = 1.




# D3) Fit linear discriminant analysis: model_lda
set.seed(950002)
(chi_sq_model_lda <- train(
  x = dplyr::select(chi_sq_final, -blood.test.result),
  y = chi_sq_final$blood.test.result,
  metric = "ROC", # AUC as the evaluation metric
  method = "lda",
  trControl = myControl_chi_sq
  # , family = "binomial" # (if train/test manually and observe confusionMatrix)
))


### a) THRESHOLD: 0.5
# If probabilities of predictions (for "hiv negative") exceed threshold of 0.5, "hiv.negative" else "hiv.positive"
lda_predict_chi_sq_0.5 <- factor(ifelse(predict(chi_sq_model_lda, type = "prob")$hiv.negative > 0.5,
                                        "hiv.negative", "hiv.positive"), levels = levels(chi_sq_final$blood.test.result))
# OR
# lda_predict_chi_sq_0.5 <- predict(chi_sq_model_lda) # DEFAULT type = "raw"
table(lda_predict_chi_sq_0.5)
# Create confusion matrix
(cm_lda_predict_chi_sq_0.5 <- confusionMatrix(lda_predict_chi_sq_0.5, chi_sq_final$blood.test.result))

### b) THRESHOLD: 0.8 (trying another threshold)
# If probabilities of predictions (for "hiv negative") exceed threshold of 0.8, "hiv.negative" else "hiv.positive"
lda_predict_chi_sq_0.8 <- factor(ifelse(predict(chi_sq_model_lda, type = "prob")$hiv.negative > 0.8,
                                        "hiv.negative", "hiv.positive"), levels = levels(chi_sq_final$blood.test.result))
table(lda_predict_chi_sq_0.8)
# Create confusion matrix
(cm_lda_predict_chi_sq_0.8 <- confusionMatrix(lda_predict_chi_sq_0.8, chi_sq_final$blood.test.result))

### c) THRESHOLD: 0.2 (trying another threshold)
# If probabilities of predictions (for "hiv negative") exceed threshold of 0.2, "hiv.negative" else "hiv.positive"
lda_predict_chi_sq_0.2 <- factor(ifelse(predict(chi_sq_model_lda, type = "prob")$hiv.negative > 0.2,
                                        "hiv.negative", "hiv.positive"), levels = levels(chi_sq_final$blood.test.result))
table(lda_predict_chi_sq_0.2)
# Create confusion matrix
(cm_lda_predict_chi_sq_0.2 <- confusionMatrix(lda_predict_chi_sq_0.2, chi_sq_final$blood.test.result))


# Make ROC curve <hiv.positive>
(roc_lda_predict_chi_sq <- colAUC(predict(chi_sq_model_lda, type = "prob")$hiv.positive,
                                  chi_sq_final$blood.test.result, plotROC = TRUE))
# OR (either): yields the same ROC curve <hiv.negative>
# (roc_lda_predict_chi_sq <- colAUC(predict(chi_sq_model_lda, type = "prob")$hiv.negative,
#                                        chi_sq_final$blood.test.result, plotROC = TRUE))




# D4) Fit support vector machine with a radial kernel: model_svm
set.seed(950003)
(chi_sq_model_svm <- train(
  x = dplyr::select(chi_sq_final, -blood.test.result),
  y = chi_sq_final$blood.test.result,
  metric = "ROC", # AUC as the evaluation metric
  method = "svmRadial",
  trControl = myControl_chi_sq
  # , family = "binomial" # (if train/test manually and observe confusionMatrix)
))


### a) THRESHOLD: 0.5
# If probabilities of predictions (for "hiv negative") exceed threshold of 0.5, "hiv.negative" else "hiv.positive"
svm_predict_chi_sq_0.5 <- factor(ifelse(predict(chi_sq_model_svm, type = "prob")$hiv.negative > 0.5,
                                        "hiv.negative", "hiv.positive"), levels = levels(chi_sq_final$blood.test.result))
# OR
# svm_predict_chi_sq_0.5 <- predict(chi_sq_model_svm) # DEFAULT type = "raw"
table(svm_predict_chi_sq_0.5)
# Create confusion matrix
(cm_svm_predict_chi_sq_0.5 <- confusionMatrix(svm_predict_chi_sq_0.5, chi_sq_final$blood.test.result))

### b) THRESHOLD: 0.8 (trying another threshold)
# If probabilities of predictions (for "hiv negative") exceed threshold of 0.8, "hiv.negative" else "hiv.positive"
svm_predict_chi_sq_0.8 <- factor(ifelse(predict(chi_sq_model_svm, type = "prob")$hiv.negative > 0.8,
                                        "hiv.negative", "hiv.positive"), levels = levels(chi_sq_final$blood.test.result))
table(svm_predict_chi_sq_0.8)
# Create confusion matrix
(cm_svm_predict_chi_sq_0.8 <- confusionMatrix(svm_predict_chi_sq_0.8, chi_sq_final$blood.test.result))

### c) THRESHOLD: 0.2 (trying another threshold)
# If probabilities of predictions (for "hiv negative") exceed threshold of 0.2, "hiv.negative" else "hiv.positive"
svm_predict_chi_sq_0.2 <- factor(ifelse(predict(chi_sq_model_svm, type = "prob")$hiv.negative > 0.2,
                                        "hiv.negative", "hiv.positive"), levels = levels(chi_sq_final$blood.test.result))
table(svm_predict_chi_sq_0.2)
# Create confusion matrix
(cm_svm_predict_chi_sq_0.2 <- confusionMatrix(svm_predict_chi_sq_0.2, chi_sq_final$blood.test.result))


# Make ROC curve <hiv.positive>
(roc_svm_predict_chi_sq <- colAUC(predict(chi_sq_model_svm, type = "prob")$hiv.positive,
                                  chi_sq_final$blood.test.result, plotROC = TRUE))
# OR (either): yields the same ROC curve <hiv.negative>
# (roc_svm_predict_chi_sq <- colAUC(predict(chi_sq_model_svm, type = "prob")$hiv.negative,
#                                        chi_sq_final$blood.test.result, plotROC = TRUE))


# to observe the hyperparameters tuned (grid search/tuning grid)
## C = "Cost"
plot(chi_sq_model_svm)
## Tuning parameter 'sigma' was held constant at a value of 0.1183116.
## ROC was used to select the optimal model using the largest value.
## The final values used for the model were sigma = 0.1183116 and C = 1.




# D5) Fit extreme gradient boosting: model_xgboost
set.seed(950004)
(chi_sq_model_xgboost <- train(
  x = dplyr::select(chi_sq_final, -blood.test.result),
  y = chi_sq_final$blood.test.result,
  metric = "ROC", # AUC as the evaluation metric
  method = "xgbTree",
  trControl = myControl_chi_sq
  # , family = "binomial" # (if train/test manually and observe confusionMatrix)
))


### a) THRESHOLD: 0.5
# If probabilities of predictions (for "hiv negative") exceed threshold of 0.5, "hiv.negative" else "hiv.positive"
xgboost_predict_chi_sq_0.5 <- factor(ifelse(predict(chi_sq_model_xgboost, type = "prob")$hiv.negative > 0.5,
                                            "hiv.negative", "hiv.positive"), levels = levels(chi_sq_final$blood.test.result))
# OR
# xgboost_predict_chi_sq_0.5 <- predict(chi_sq_model_xgboost) # DEFAULT type = "raw"
table(xgboost_predict_chi_sq_0.5)
# Create confusion matrix
(cm_xgboost_predict_chi_sq_0.5 <- confusionMatrix(xgboost_predict_chi_sq_0.5, chi_sq_final$blood.test.result))

### b) THRESHOLD: 0.8 (trying another threshold)
# If probabilities of predictions (for "hiv negative") exceed threshold of 0.8, "hiv.negative" else "hiv.positive"
xgboost_predict_chi_sq_0.8 <- factor(ifelse(predict(chi_sq_model_xgboost, type = "prob")$hiv.negative > 0.8,
                                            "hiv.negative", "hiv.positive"), levels = levels(chi_sq_final$blood.test.result))
table(xgboost_predict_chi_sq_0.8)
# Create confusion matrix
(cm_xgboost_predict_chi_sq_0.8 <- confusionMatrix(xgboost_predict_chi_sq_0.8, chi_sq_final$blood.test.result))

### c) THRESHOLD: 0.2 (trying another threshold)
# If probabilities of predictions (for "hiv negative") exceed threshold of 0.2, "hiv.negative" else "hiv.positive"
xgboost_predict_chi_sq_0.2 <- factor(ifelse(predict(chi_sq_model_xgboost, type = "prob")$hiv.negative > 0.2,
                                            "hiv.negative", "hiv.positive"), levels = levels(chi_sq_final$blood.test.result))
table(xgboost_predict_chi_sq_0.2)
# Create confusion matrix
(cm_xgboost_predict_chi_sq_0.2 <- confusionMatrix(xgboost_predict_chi_sq_0.2, chi_sq_final$blood.test.result))


# Make ROC curve <hiv.positive>
(roc_xgboost_predict_chi_sq <- colAUC(predict(chi_sq_model_xgboost, type = "prob")$hiv.positive,
                                      chi_sq_final$blood.test.result, plotROC = TRUE))
# OR (either): yields the same ROC curve <hiv.negative>
# (roc_xgboost_predict_chi_sq <- colAUC(predict(chi_sq_model_xgboost, type = "prob")$hiv.negative,
#                                        chi_sq_final$blood.test.result, plotROC = TRUE))


# to observe the hyperparameters tuned (grid search/tuning grid) - {7 tuning parameters altogether}
## max_depth = "Max Tree Depth"; nrounds = "Boosting Iterations"
plot(chi_sq_model_xgboost)
## Tuning parameter 'gamma' was held constant at a value of 0.
## Tuning parameter 'min_child_weight' was held constant at a value of 1.
## ROC was used to select the optimal model using the largest value.
## The final values used for the model were nrounds = 100, max_depth = 3, eta = 0.3, gamma = 0, colsample_bytree = 0.6,
## min_child_weight = 1 and subsample = 0.75.




# D6) Fit naive bayes: model_nb (different syntax??? - actually can... just need to specify dplyr::) {{{TAKE LONG TIME!!!!!}}}
# - maybe because of the previous model package "xgboost" <due to package conflict>
# use find() to see which packages the conflict is coming from, i.e., find("select")
# find("select")
set.seed(950005)
(chi_sq_model_nb <- train(
  ## OR
  ## x = dplyr::select(chi_sq_final, -blood.test.result),
  ## y = chi_sq_final$blood.test.result,
  blood.test.result ~ .,
  data = chi_sq_final,
  metric = "ROC", # AUC as the evaluation metric
  method = "nb",
  trControl = myControl_chi_sq
  # , family = "binomial" # (if train/test manually and observe confusionMatrix)
))


### a) THRESHOLD: 0.5
# If probabilities of predictions (for "hiv negative") exceed threshold of 0.5, "hiv.negative" else "hiv.positive"
nb_predict_chi_sq_0.5 <- factor(ifelse(predict(chi_sq_model_nb, type = "prob")$hiv.negative > 0.5,
                                       "hiv.negative", "hiv.positive"), levels = levels(chi_sq_final$blood.test.result))
# OR
# nb_predict_chi_sq_0.5 <- predict(chi_sq_model_nb) # DEFAULT type = "raw"
table(nb_predict_chi_sq_0.5)
# Create confusion matrix
(cm_nb_predict_chi_sq_0.5 <- confusionMatrix(nb_predict_chi_sq_0.5, chi_sq_final$blood.test.result))

### b) THRESHOLD: 0.8 (trying another threshold)
# If probabilities of predictions (for "hiv negative") exceed threshold of 0.8, "hiv.negative" else "hiv.positive"
nb_predict_chi_sq_0.8 <- factor(ifelse(predict(chi_sq_model_nb, type = "prob")$hiv.negative > 0.8,
                                       "hiv.negative", "hiv.positive"), levels = levels(chi_sq_final$blood.test.result))
table(nb_predict_chi_sq_0.8)
# Create confusion matrix
(cm_nb_predict_chi_sq_0.8 <- confusionMatrix(nb_predict_chi_sq_0.8, chi_sq_final$blood.test.result))

### c) THRESHOLD: 0.2 (trying another threshold)
# If probabilities of predictions (for "hiv negative") exceed threshold of 0.2, "hiv.negative" else "hiv.positive"
nb_predict_chi_sq_0.2 <- factor(ifelse(predict(chi_sq_model_nb, type = "prob")$hiv.negative > 0.2,
                                       "hiv.negative", "hiv.positive"), levels = levels(chi_sq_final$blood.test.result))
table(nb_predict_chi_sq_0.2)
# Create confusion matrix
(cm_nb_predict_chi_sq_0.2 <- confusionMatrix(nb_predict_chi_sq_0.2, chi_sq_final$blood.test.result))


# Make ROC curve <hiv.positive>
(roc_nb_predict_chi_sq <- colAUC(predict(chi_sq_model_nb, type = "prob")$hiv.positive,
                                 chi_sq_final$blood.test.result, plotROC = TRUE))
# OR (either): yields the same ROC curve <hiv.negative>
# (roc_nb_predict_chi_sq <- colAUC(predict(chi_sq_model_nb, type = "prob")$hiv.negative,
#                                        chi_sq_final$blood.test.result, plotROC = TRUE))


# to observe the hyperparameters tuned (grid search/tuning grid)
## userkernel = FALSE -> "Distribution Type" = "Gaussian"; usekernel = TRUE -> "Distribution Type" = "Nonparametric"
plot(chi_sq_model_nb)
## Tuning parameter 'fL' was held constant at a value of 0.
## Tuning parameter 'adjust' was held constant at a value of 1.
## ROC was used to select the optimal model using the largest value.
## The final values used for the model were fL = 0, usekernel = TRUE and adjust = 1.




# D7) Fit k-nearest neighbors: model_knn
set.seed(950006)
(chi_sq_model_knn <- train(
  blood.test.result ~ .,
  data = chi_sq_final,
  metric = "ROC", # AUC as the evaluation metric
  method = "kknn",
  trControl = myControl_chi_sq
  # , family = "binomial" # (if train/test manually and observe confusionMatrix)
))


### a) THRESHOLD: 0.5
# If probabilities of predictions (for "hiv negative") exceed threshold of 0.5, "hiv.negative" else "hiv.positive"
knn_predict_chi_sq_0.5 <- factor(ifelse(predict(chi_sq_model_knn, type = "prob")$hiv.negative > 0.5,
                                        "hiv.negative", "hiv.positive"), levels = levels(chi_sq_final$blood.test.result))
# OR
# knn_predict_chi_sq_0.5 <- predict(chi_sq_model_knn) # DEFAULT type = "raw"
table(knn_predict_chi_sq_0.5)
# Create confusion matrix
(cm_knn_predict_chi_sq_0.5 <- confusionMatrix(knn_predict_chi_sq_0.5, chi_sq_final$blood.test.result))

### b) THRESHOLD: 0.8 (trying another threshold)
# If probabilities of predictions (for "hiv negative") exceed threshold of 0.8, "hiv.negative" else "hiv.positive"
knn_predict_chi_sq_0.8 <- factor(ifelse(predict(chi_sq_model_knn, type = "prob")$hiv.negative > 0.8,
                                        "hiv.negative", "hiv.positive"), levels = levels(chi_sq_final$blood.test.result))
table(knn_predict_chi_sq_0.8)
# Create confusion matrix
(cm_knn_predict_chi_sq_0.8 <- confusionMatrix(knn_predict_chi_sq_0.8, chi_sq_final$blood.test.result))

### c) THRESHOLD: 0.2 (trying another threshold)
# If probabilities of predictions (for "hiv negative") exceed threshold of 0.2, "hiv.negative" else "hiv.positive"
knn_predict_chi_sq_0.2 <- factor(ifelse(predict(chi_sq_model_knn, type = "prob")$hiv.negative > 0.2,
                                        "hiv.negative", "hiv.positive"), levels = levels(chi_sq_final$blood.test.result))
table(knn_predict_chi_sq_0.2)
# Create confusion matrix
(cm_knn_predict_chi_sq_0.2 <- confusionMatrix(knn_predict_chi_sq_0.2, chi_sq_final$blood.test.result))


# Make ROC curve <hiv.positive>
(roc_knn_predict_chi_sq <- colAUC(predict(chi_sq_model_knn, type = "prob")$hiv.positive,
                                  chi_sq_final$blood.test.result, plotROC = TRUE))
# OR (either): yields the same ROC curve <hiv.negative>
# (roc_knn_predict_chi_sq <- colAUC(predict(chi_sq_model_knn, type = "prob")$hiv.negative,
#                                        chi_sq_final$blood.test.result, plotROC = TRUE))


# to observe the hyperparameters tuned (grid search/tuning grid)
## kmax = "max. #Neighbors"
plot(chi_sq_model_knn)
## Tuning parameter 'distance' was held constant at a value of 2.
## Tuning parameter 'kernel' was held constant at a value of optimal.
## ROC was used to select the optimal model using the largest value.
## The final values used for the model were kmax = 9, distance = 2 and kernel = optimal.

# plot of misclassification vs. k (when k increases, misclassification increases)
plot(chi_sq_model_knn$finalModel)




# D8) Fit neural network: model_nn
set.seed(950007)
(chi_sq_model_nn <- train(
  blood.test.result ~ .,
  data = chi_sq_final,
  metric = "ROC", # AUC as the evaluation metric
  method = "nnet",
  trControl = myControl_chi_sq
  # , family = "binomial" # (if train/test manually and observe confusionMatrix)
))


### a) THRESHOLD: 0.5
# If probabilities of predictions (for "hiv negative") exceed threshold of 0.5, "hiv.negative" else "hiv.positive"
nn_predict_chi_sq_0.5 <- factor(ifelse(predict(chi_sq_model_nn, type = "prob")$hiv.negative > 0.5,
                                       "hiv.negative", "hiv.positive"), levels = levels(chi_sq_final$blood.test.result))
# OR
# nn_predict_chi_sq_0.5 <- predict(chi_sq_model_nn) # DEFAULT type = "raw"
table(nn_predict_chi_sq_0.5)
# Create confusion matrix
(cm_nn_predict_chi_sq_0.5 <- confusionMatrix(nn_predict_chi_sq_0.5, chi_sq_final$blood.test.result))

### b) THRESHOLD: 0.8 (trying another threshold)
# If probabilities of predictions (for "hiv negative") exceed threshold of 0.8, "hiv.negative" else "hiv.positive"
nn_predict_chi_sq_0.8 <- factor(ifelse(predict(chi_sq_model_nn, type = "prob")$hiv.negative > 0.8,
                                       "hiv.negative", "hiv.positive"), levels = levels(chi_sq_final$blood.test.result))
table(nn_predict_chi_sq_0.8)
# Create confusion matrix
(cm_nn_predict_chi_sq_0.8 <- confusionMatrix(nn_predict_chi_sq_0.8, chi_sq_final$blood.test.result))

### c) THRESHOLD: 0.2 (trying another threshold)
# If probabilities of predictions (for "hiv negative") exceed threshold of 0.2, "hiv.negative" else "hiv.positive"
nn_predict_chi_sq_0.2 <- factor(ifelse(predict(chi_sq_model_nn, type = "prob")$hiv.negative > 0.2,
                                       "hiv.negative", "hiv.positive"), levels = levels(chi_sq_final$blood.test.result))
table(nn_predict_chi_sq_0.2)
# Create confusion matrix
(cm_nn_predict_chi_sq_0.2 <- confusionMatrix(nn_predict_chi_sq_0.2, chi_sq_final$blood.test.result))


# Make ROC curve <hiv.positive>
(roc_nn_predict_chi_sq <- colAUC(predict(chi_sq_model_nn, type = "prob")$hiv.positive,
                                 chi_sq_final$blood.test.result, plotROC = TRUE))
# OR (either): yields the same ROC curve <hiv.negative>
# (roc_nn_predict_chi_sq <- colAUC(predict(chi_sq_model_nn, type = "prob")$hiv.negative,
#                                        chi_sq_final$blood.test.result, plotROC = TRUE))


# to observe the hyperparameters tuned (grid search/tuning grid)
## decay = "Weight Decay"; size = "#Hidden Units"
plot(chi_sq_model_nn)
## ROC was used to select the optimal model using the largest value.
## The final values used for the model were size = 3 and decay = 0.1.


##########################################################################################################################################################
##########################################################################################################################################################

# COMPARISON OF MODELS
## 1. Among STEPWISE:

# Create model_list
model_list_stepwise <- list(glmnet = stepwise_model_glmnet, rf = stepwise_model_rf, lda = stepwise_model_lda,
                            svm = stepwise_model_svm, xgboost = stepwise_model_xgboost, nb = stepwise_model_nb,
                            knn = stepwise_model_knn, nn = stepwise_model_nn)
# Pass model_list to resamples(): resamples
(resamples_stepwise <- resamples(model_list_stepwise))
# Summarize the results
summary(resamples_stepwise)
summary(resamples_stepwise, metric = "ROC") # AUC only
# Create a box-and-whisker plot
bwplot(resamples_stepwise)
bwplot(resamples_stepwise, metric = "ROC") # AUC only
# Create a scatterplot
xyplot(resamples_stepwise, metric = "ROC")
# Create a density plot
densityplot(resamples_stepwise, metric = "ROC")
# Create a dot plot
dotplot(resamples_stepwise)
dotplot(resamples_stepwise, metric = "ROC")

# Print the BEST MODEL [among the STEPWISE feature selection method]
print(stepwise_model_rf) # Random Forest algorithm performs the best


## 2. Among BORUTA:

# Create model_list
model_list_boruta <- list(glmnet = boruta_model_glmnet, rf = boruta_model_rf, lda = boruta_model_lda,
                          svm = boruta_model_svm, xgboost = boruta_model_xgboost, nb = boruta_model_nb,
                          knn = boruta_model_knn, nn = boruta_model_nn)
# Pass model_list to resamples(): resamples
(resamples_boruta <- resamples(model_list_boruta))
# Summarize the results
summary(resamples_boruta)
summary(resamples_boruta, metric = "ROC") # AUC only
# Create a box-and-whisker plot
bwplot(resamples_boruta)
bwplot(resamples_boruta, metric = "ROC") # AUC only
# Create a scatterplot
xyplot(resamples_boruta, metric = "ROC")
# Create a density plot
densityplot(resamples_boruta, metric = "ROC")
# Create a dot plot
dotplot(resamples_boruta)
dotplot(resamples_boruta, metric = "ROC")

# Print the BEST MODEL [among the BORUTA feature selection method]
print(boruta_model_rf) # Random Forest algorithm performs the best


## 3. Among RPART:

# Create model_list
model_list_rpart <- list(glmnet = rpart_model_glmnet, rf = rpart_model_rf, lda = rpart_model_lda,
                         svm = rpart_model_svm, xgboost = rpart_model_xgboost, nb = rpart_model_nb,
                         knn = rpart_model_knn, nn = rpart_model_nn)
# Pass model_list to resamples(): resamples
(resamples_rpart <- resamples(model_list_rpart))
# Summarize the results
summary(resamples_rpart)
summary(resamples_rpart, metric = "ROC") # AUC only
# Create a box-and-whisker plot
bwplot(resamples_rpart)
bwplot(resamples_rpart, metric = "ROC") # AUC only
# Create a scatterplot
xyplot(resamples_rpart, metric = "ROC")
# Create a density plot
densityplot(resamples_rpart, metric = "ROC")
# Create a dot plot
dotplot(resamples_rpart)
dotplot(resamples_rpart, metric = "ROC")

# Print the BEST MODEL [among the RPART feature selection methods]
print(rpart_model_xgboost) # Extreme Gradient Boosting algorithm performs the best


## 4. Among CHI_SQ:

# Create model_list
model_list_chi_sq <- list(glmnet = chi_sq_model_glmnet, rf = chi_sq_model_rf, lda = chi_sq_model_lda,
                          svm = chi_sq_model_svm, xgboost = chi_sq_model_xgboost, nb = chi_sq_model_nb,
                          knn = chi_sq_model_knn, nn = chi_sq_model_nn)
# Pass model_list to resamples(): resamples
(resamples_chi_sq <- resamples(model_list_chi_sq))
# Summarize the results
summary(resamples_chi_sq)
summary(resamples_chi_sq, metric = "ROC") # AUC only
# Create a box-and-whisker plot
bwplot(resamples_chi_sq)
bwplot(resamples_chi_sq, metric = "ROC") # AUC only
# Create a scatterplot
xyplot(resamples_chi_sq, metric = "ROC")
# Create a density plot
densityplot(resamples_chi_sq, metric = "ROC")
# Create a dot plot
dotplot(resamples_chi_sq)
dotplot(resamples_chi_sq, metric = "ROC")

# Print the BEST MODEL [among the chi_sq feature selection methods]
print(chi_sq_model_xgboost) # Extreme Gradient Boosting algorithm performs the best


# 5) Comparisons of the final TOP 4 models from each feature selection technique: <rf_stepwise> OR <rf_boruta>
# OR <xgboost_rpart> or <xgboost_chi_sq>:

# Create model_list
model_list_final <- list(rf_stepwise = stepwise_model_rf, rf_boruta = boruta_model_rf,
                         xgboost_rpart = rpart_model_xgboost, xgboost_chi_sq = chi_sq_model_xgboost)
# Pass model_list to resamples(): resamples
(resamples_final <- resamples(model_list_final))
# Summarize the results
summary(resamples_final)
summary(resamples_final, metric = "ROC") # AUC only
# Create a box-and-whisker plot
bwplot(resamples_final)
bwplot(resamples_final, metric = "ROC") # AUC only
# Create a scatterplot
xyplot(resamples_final, metric = "ROC")
# Create a density plot
densityplot(resamples_final, metric = "ROC")
# Create a dot plot
dotplot(resamples_final)
dotplot(resamples_final, metric = "ROC")

# Print the BEST MODEL [among ALL the feature selection methods]
print(stepwise_model_rf)

### CONCLUSION: Between <rf_stepwise> and <rf_boruta>, the algorithm/model that yields (i) the higher median of AUC and
# (ii) smaller interquartile range/range of AUC is chosen/preferred (<rf_stepwise> performs the best!)
# Therefore, RANDOM FOREST algorithm using STEPWISE REGRESSION feature selection method performs the BEST on the overall dataset.

##########################################################################################################################################################

# Recall again the features used in the STEPWISE data
colnames(stepwise_final)
# Applying/testing the BEST model {stepwise_model_rf} on other countries HIV dataset to evaluate its performance
# based on AUC (Area under the ROC [Receiver Operating Characteristic] Curve)


### Extract datasets from the 3 countries -
## Malawi (MW) - Year 2015 - 2016 (same as Angola)
# AR - HIV Test Result
malawi_hiv <- read_dta("/Users/kjyuaan8/Desktop/Year 3 Sem 1/WIH3001 DSP/Implementation/MWAR7AFL.DTA")
# IR - Women Aged 15-49 years
malawi_women <- read_dta("/Users/kjyuaan8/Desktop/Year 3 Sem 1/WIH3001 DSP/Implementation/MWIR7AFL.DTA")

## Zambia (ZM) - Year 2018
# AR - HIV Test Result
zambia_hiv <- read_dta("/Users/kjyuaan8/Desktop/Year 3 Sem 1/WIH3001 DSP/Implementation/ZMAR71FL.DTA")
# IR - Women Aged 15-49 years
zambia_women <- read_dta("/Users/kjyuaan8/Desktop/Year 3 Sem 1/WIH3001 DSP/Implementation/ZMIR71FL.DTA")

## Zimbabwe (ZW) - Year 2015
# AR - HIV Test Result
zimbabwe_hiv <- read_dta("/Users/kjyuaan8/Desktop/Year 3 Sem 1/WIH3001 DSP/Implementation/ZWAR71FL.DTA")
# IR - Women Aged 15-49 years
zimbabwe_women <- read_dta("/Users/kjyuaan8/Desktop/Year 3 Sem 1/WIH3001 DSP/Implementation/ZWIR72FL.DTA")


## (A) Malawi
##### Cleaning MALAWI dataset [as what was done on Angola]
malawi_hiv_label <- as.character(labelled::var_label(malawi_hiv))
# OR malawi_hiv_label <- as.character(sapply(malawi_hiv, attr, "label"))
malawi_hiv_factor <- as_factor(malawi_hiv)
# Tips: levels = c("default", "labels", "values", "both")
malawi_hiv_data <- malawi_hiv_factor
colnames(malawi_hiv_data) <- malawi_hiv_label
head(malawi_hiv_data)
malawi_hiv_data <- dplyr::select(malawi_hiv_data, -c(cluster, household, line)) ###

## IR - Women Aged 15-49 years (Malawi) - Preview: -> many columns
malawi_women_label <- as.character(labelled::var_label(malawi_women))
# OR malawi_women_label <- as.character(sapply(malawi_women, attr, "label"))
malawi_women_factor <- as_factor(malawi_women)
malawi_women_data <- malawi_women_factor
colnames(malawi_women_data) <- malawi_women_label
head(malawi_women_data[, 1:10])
##### IMPORTANT STEP - Related Columns Selection [Women's HIV] #####
# Extracting only the required sections of columns (under my study) - refer to the Standard Recode Manual
## REC75 - AIDS, STIs, and Condom Use [Len: 172]
## REC80 - AIDS, STIs, and Condom Use (continued) [Len: 113]
malawi_women_selected_factor <- dplyr::select(malawi_women_factor, v001, v002, v003, v750:v858)

## Merging / joining both the data
# Base file [unit of analysis] --> <malawi_hiv_factor> file
final_dataset_factor_malawi <- right_join(malawi_women_selected_factor, malawi_hiv_factor,
                                          by = c("v001" = "hivclust",
                                                 "v002" = "hivnumb",
                                                 "v003" = "hivline"),
                                          keep = FALSE,
                                          na_matches = "never")
# drop the 3 matching variables/columns (with <keep = FALSE>) for encoding purposes (carry no importance weightage)
final_dataset_factor_malawi <- dplyr::select(final_dataset_factor_malawi, -c(v001, v002, v003))

final_dataset_factor_malawi_label <- as.character(labelled::var_label(final_dataset_factor_malawi))
# OR final_dataset_factor_malawi_label <- as.character(sapply(final_dataset_factor_malawi, attr, "label"))
final_dataset_malawi <- final_dataset_factor_malawi
colnames(final_dataset_malawi) <- final_dataset_factor_malawi_label

## ------------------------------------------------------------------------------------------------------------------------------------- ##

# ### Data Pre-Processing Steps
# There are duplicated column names! MAKE THEM UNIQUE!!!
names(final_dataset_malawi) <- make.names(names(final_dataset_malawi), unique = TRUE)
colnames(final_dataset_malawi)

## Important STEP!!! {feature selection/feature engineering}
# Removing na (not applicable) variables - column names starting with "na" /
# questions that are no longer part of the DHS VII core questionnaire from the final_dataset_malawi
malawi_fdr_1 <- dplyr::select(final_dataset_malawi, -starts_with("na."))

# Replacing some survey questions' responses of NA values with their corresponding values [NA -> "no"]
malawi_ih_1 <- which(colnames(malawi_fdr_1) == "sought.sti.advice.treatment.from..government.hospital")
malawi_ih_2 <- which(colnames(malawi_fdr_1) == "sought.sti.advice.treatment.from..other")
# convert factor to character
malawi_fdr_1[malawi_ih_1:malawi_ih_2] <- lapply(malawi_fdr_1[malawi_ih_1:malawi_ih_2],
                                                as.character)
malawi_fdr_1 <- malawi_fdr_1 %>% mutate_at(seq(malawi_ih_1, malawi_ih_2), ~replace_na(., "no"))
# convert character back to factor
malawi_fdr_1[malawi_ih_1:malawi_ih_2] <- lapply(malawi_fdr_1[malawi_ih_1:malawi_ih_2],
                                                as.factor)

malawi_ih_3 <- which(colnames(malawi_fdr_1) == "place.for.hiv.test..government.hospital")
malawi_ih_4 <- which(colnames(malawi_fdr_1) == "place.for.hiv.test..other")
# convert factor to character
malawi_fdr_1[malawi_ih_3:malawi_ih_4] <- lapply(malawi_fdr_1[malawi_ih_3:malawi_ih_4],
                                                as.character)
malawi_fdr_1 <- malawi_fdr_1 %>% mutate_at(seq(malawi_ih_3, malawi_ih_4), ~replace_na(., "no"))
# convert character back to factor
malawi_fdr_1[malawi_ih_3:malawi_ih_4] <- lapply(malawi_fdr_1[malawi_ih_3:malawi_ih_4],
                                                as.factor)

sapply(malawi_fdr_1, attr, "levels") # to see factors/levels of the latest variables
colnames(malawi_fdr_1)
str(malawi_fdr_1)

# some manually selected factors into <dbl> {numeric}
malawi_fdr_1[list_of_characters] <- lapply(malawi_fdr_1[list_of_characters], as.numeric)
glimpse(malawi_fdr_1)

# dealing with HIV (response) part of the dataset
malawi_fdr_2 <- dplyr::select(malawi_fdr_1, -bar.code, -lab.number)
malawi_fdr_2["blood.test.result"] <- lapply(malawi_fdr_2["blood.test.result"], as.character)
# miracle -> only 4 unique values left! ("hiv negative", "hiv  positive", "inconclusive", "indeterminate")
malawi_fdr_2["blood.test.result"][malawi_fdr_2["blood.test.result"] == "indeterminate" |
                                    malawi_fdr_2["blood.test.result"] == "inconclusive"] <-
  "hiv negative"
malawi_fdr_2["blood.test.result"][malawi_fdr_2["blood.test.result"] == "hiv  positive"] <- "hiv positive"
# back to factor
malawi_fdr_2["blood.test.result"] <- lapply(malawi_fdr_2["blood.test.result"], as.factor)
str(malawi_fdr_2)
sapply(malawi_fdr_2, attr, "levels") # to see factors/levels of the latest variables



##### Selected variables from the FINAL MODEL:
wh_malawi <- malawi_fdr_2
wh_malawi <- dplyr::select(wh_malawi, intersect(colnames(stepwise_final), colnames(wh_malawi)))

##### cleaning data again -- removing NA values
# (i) median imputation in all numeric columns
wh_malawi <- wh_malawi %>% mutate_if(is.double, function(x) ifelse(is.na(x), median(x, na.rm = T), x))

# (ii) replacing the missing values in bespoke category for all non-numeric columns
for (i in 1:length(wh_malawi)) {
  if (sum(is.na(wh_malawi[i])) > 0) {
    if ("don't know/not sure/depends" %in% levels(wh_malawi[[i]])) {
      wh_malawi[[i]][is.na(wh_malawi[[i]])] <- "don't know/not sure/depends"
    }
    else if ("other" %in% levels(wh_malawi[[i]])) {
      wh_malawi[[i]][is.na(wh_malawi[[i]])] <- "other"
    }
  }
}
for (i in 1:length(wh_malawi)) {
  if (sum(is.na(wh_malawi[i])) > 0) {
    if ("don't know" %in% levels(wh_malawi[[i]])) {
      wh_malawi[[i]][is.na(wh_malawi[[i]])] <- "don't know"
    }
    else {
      levels(wh_malawi[[i]]) <- c(levels(wh_malawi[[i]]), "don't know") # add extra levels to the factors
      wh_malawi[[i]][is.na(wh_malawi[[i]])] <- "don't know"
    }
  }
}
any(is.na(wh_malawi)) # to check still got any null values?

### convert categorical value into numerical
# important
for (i in 1:length(wh_malawi)) {
  if (is.factor(wh_malawi[[i]])) {
    wh_malawi[[i]] <- unclass(wh_malawi[[i]])
  }
} # easier method

wh_malawi <- lapply(wh_malawi, as.numeric)
wh_malawi <- data.frame(wh_malawi)
# applicable because happened to be only one column is "outersect"
wh_malawi[[setdiff(colnames(stepwise_final), colnames(wh_malawi))]] <- 1
wh_malawi[["blood.test.result"]] <- factor(wh_malawi[["blood.test.result"]], levels = c(1, 2), labels = c("hiv.negative", "hiv.positive"))
str(wh_malawi)


########## TESTING MODEL RESULTS ON MALAWI DATASET ##########
(cm_malawi <- confusionMatrix(factor(ifelse(predict(stepwise_model_rf, wh_malawi, type = "prob")$hiv.negative > 0.5,
                                            "hiv.negative", "hiv.positive"), levels = levels(wh_malawi$blood.test.result)),
                              wh_malawi$blood.test.result))
## ROC curve of prediction on new dataset (Malawi)
# (roc_malawi <- colAUC(predict(stepwise_model_rf, wh_malawi, type = "prob")$hiv.negative, wh_malawi$blood.test.result, plotROC = TRUE))
### Create 'roc' object
(roc_malawi <- roc(wh_malawi$blood.test.result, predict(stepwise_model_rf, wh_malawi, type = "prob")$hiv.positive))
# OR (roc_malawi <- roc(wh_malawi$blood.test.result, predict(stepwise_model_rf, wh_malawi, type = "prob")$hiv.negative)) # same result
### Plot ROC curve
dev.off()
plot(roc_malawi, main = "ROC Curve -- Malawi ")
(auc_malawi <- auc(roc_malawi)) # calculate AUC score: 0.5201458 (Malawi)



# (B) Zambia
##### Cleaning ZAMBIA dataset [as what was done on Angola]
zambia_hiv_label <- as.character(labelled::var_label(zambia_hiv))
# OR zambia_hiv_label <- as.character(sapply(zambia_hiv, attr, "label"))
zambia_hiv_factor <- as_factor(zambia_hiv)
# Tips: levels = c("default", "labels", "values", "both")
zambia_hiv_data <- zambia_hiv_factor
colnames(zambia_hiv_data) <- zambia_hiv_label
head(zambia_hiv_data)
zambia_hiv_data <- dplyr::select(zambia_hiv_data, -c(cluster, household, line)) ###

## IR - Women Aged 15-49 years (Zambia) - Preview: -> many columns
zambia_women_label <- as.character(labelled::var_label(zambia_women))
# OR zambia_women_label <- as.character(sapply(zambia_women, attr, "label"))
zambia_women_factor <- as_factor(zambia_women)
zambia_women_data <- zambia_women_factor
colnames(zambia_women_data) <- zambia_women_label
head(zambia_women_data[, 1:10])
##### IMPORTANT STEP - Related Columns Selection [Women's HIV] #####
# Extracting only the required sections of columns (under my study) - refer to the Standard Recode Manual
## REC75 - AIDS, STIs, and Condom Use [Len: 172]
## REC80 - AIDS, STIs, and Condom Use (continued) [Len: 113]
zambia_women_selected_factor <- dplyr::select(zambia_women_factor, v001, v002, v003, v750:v858)

## Merging / joining both the data
# Base file [unit of analysis] --> <zambia_hiv_factor> file
final_dataset_factor_zambia <- right_join(zambia_women_selected_factor, zambia_hiv_factor,
                                          by = c("v001" = "hivclust",
                                                 "v002" = "hivnumb",
                                                 "v003" = "hivline"),
                                          keep = FALSE,
                                          na_matches = "never")
# drop the 3 matching variables/columns (with <keep = FALSE>) for encoding purposes (carry no importance weightage)
final_dataset_factor_zambia <- dplyr::select(final_dataset_factor_zambia, -c(v001, v002, v003))

final_dataset_factor_zambia_label <- as.character(labelled::var_label(final_dataset_factor_zambia))
# OR final_dataset_factor_zambia_label <- as.character(sapply(final_dataset_factor_zambia, attr, "label"))
final_dataset_zambia <- final_dataset_factor_zambia
colnames(final_dataset_zambia) <- final_dataset_factor_zambia_label

## ------------------------------------------------------------------------------------------------------------------------------------- ##

# ### Data Pre-Processing Steps
# There are duplicated column names! MAKE THEM UNIQUE!!!
names(final_dataset_zambia) <- make.names(names(final_dataset_zambia), unique = TRUE)
colnames(final_dataset_zambia)

## Important STEP!!! {feature selection/feature engineering}
# Removing na (not applicable) variables - column names starting with "na" /
# questions that are no longer part of the DHS VII core questionnaire from the final_dataset_zambia
zambia_fdr_1 <- dplyr::select(final_dataset_zambia, -starts_with("na."))

# Replacing some survey questions' responses of NA values with their corresponding values [NA -> "no"]
zambia_ih_1 <- which(colnames(zambia_fdr_1) == "sought.sti.advice.treatment.from..government.hospital")
zambia_ih_2 <- which(colnames(zambia_fdr_1) == "sought.sti.advice.treatment.from..other")
# convert factor to character
zambia_fdr_1[zambia_ih_1:zambia_ih_2] <- lapply(zambia_fdr_1[zambia_ih_1:zambia_ih_2],
                                                as.character)
zambia_fdr_1 <- zambia_fdr_1 %>% mutate_at(seq(zambia_ih_1, zambia_ih_2), ~replace_na(., "no"))
# convert character back to factor
zambia_fdr_1[zambia_ih_1:zambia_ih_2] <- lapply(zambia_fdr_1[zambia_ih_1:zambia_ih_2],
                                                as.factor)

zambia_ih_3 <- which(colnames(zambia_fdr_1) == "place.for.hiv.test..government.hospital")
zambia_ih_4 <- which(colnames(zambia_fdr_1) == "place.for.hiv.test..other")
# convert factor to character
zambia_fdr_1[zambia_ih_3:zambia_ih_4] <- lapply(zambia_fdr_1[zambia_ih_3:zambia_ih_4],
                                                as.character)
zambia_fdr_1 <- zambia_fdr_1 %>% mutate_at(seq(zambia_ih_3, zambia_ih_4), ~replace_na(., "no"))
# convert character back to factor
zambia_fdr_1[zambia_ih_3:zambia_ih_4] <- lapply(zambia_fdr_1[zambia_ih_3:zambia_ih_4],
                                                as.factor)

sapply(zambia_fdr_1, attr, "levels") # to see factors/levels of the latest variables
colnames(zambia_fdr_1)
str(zambia_fdr_1)

# some manually selected factors into <dbl> {numeric}
zambia_fdr_1[list_of_characters] <- lapply(zambia_fdr_1[list_of_characters], as.numeric)
glimpse(zambia_fdr_1)

# dealing with HIV (response) part of the dataset
zambia_fdr_2 <- dplyr::select(zambia_fdr_1, -bar.code, -lab.number)
zambia_fdr_2["blood.test.result"] <- lapply(zambia_fdr_2["blood.test.result"], as.character)
# miracle -> only 4 unique values left! ("hiv negative", "hiv  positive", "inconclusive", "hiv2 positive")
zambia_fdr_2["blood.test.result"][zambia_fdr_2["blood.test.result"] == "inconclusive"] <- "hiv negative"
zambia_fdr_2["blood.test.result"][zambia_fdr_2["blood.test.result"] == "hiv  positive" |
                                    zambia_fdr_2["blood.test.result"] == "hiv2 positive"] <- "hiv positive"
# back to factor
zambia_fdr_2["blood.test.result"] <- lapply(zambia_fdr_2["blood.test.result"], as.factor)
str(zambia_fdr_2)
sapply(zambia_fdr_2, attr, "levels") # to see factors/levels of the latest variables



##### Selected variables from the FINAL MODEL:
wh_zambia <- zambia_fdr_2
wh_zambia <- dplyr::select(wh_zambia, intersect(colnames(stepwise_final), colnames(wh_zambia)))

##### cleaning data again -- removing NA values
# (i) median imputation in all numeric columns
wh_zambia <- wh_zambia %>% mutate_if(is.double, function(x) ifelse(is.na(x), median(x, na.rm = T), x))

# (ii) replacing the missing values in bespoke category for all non-numeric columns
for (i in 1:length(wh_zambia)) {
  if (sum(is.na(wh_zambia[i])) > 0) {
    if ("don't know/not sure/depends" %in% levels(wh_zambia[[i]])) {
      wh_zambia[[i]][is.na(wh_zambia[[i]])] <- "don't know/not sure/depends"
    }
    else if ("other" %in% levels(wh_zambia[[i]])) {
      wh_zambia[[i]][is.na(wh_zambia[[i]])] <- "other"
    }
  }
}
for (i in 1:length(wh_zambia)) {
  if (sum(is.na(wh_zambia[i])) > 0) {
    if ("don't know" %in% levels(wh_zambia[[i]])) {
      wh_zambia[[i]][is.na(wh_zambia[[i]])] <- "don't know"
    }
    else {
      levels(wh_zambia[[i]]) <- c(levels(wh_zambia[[i]]), "don't know") # add extra levels to the factors
      wh_zambia[[i]][is.na(wh_zambia[[i]])] <- "don't know"
    }
  }
}
any(is.na(wh_zambia)) # to check still got any null values?

### convert categorical value into numerical
# important
for (i in 1:length(wh_zambia)) {
  if (is.factor(wh_zambia[[i]])) {
    wh_zambia[[i]] <- unclass(wh_zambia[[i]])
  }
} # easier method

wh_zambia <- lapply(wh_zambia, as.numeric)
wh_zambia <- data.frame(wh_zambia)
# applicable because happened to be only one column is "outersect"
wh_zambia[[setdiff(colnames(stepwise_final), colnames(wh_zambia))]] <- 1
wh_zambia[["blood.test.result"]] <- factor(wh_zambia[["blood.test.result"]], levels = c(1, 2), labels = c("hiv.negative", "hiv.positive"))
str(wh_zambia)


########## TESTING MODEL RESULTS ON ZAMBIA DATASET ##########
(cm_zambia <- confusionMatrix(factor(ifelse(predict(stepwise_model_rf, wh_zambia, type = "prob")$hiv.negative > 0.5,
                                            "hiv.negative", "hiv.positive"), levels = levels(wh_zambia$blood.test.result)),
                              wh_zambia$blood.test.result))
## ROC curve of prediction on new dataset (Zambia)
# (roc_zambia <- colAUC(predict(stepwise_model_rf, wh_zambia, type = "prob")$hiv.negative, wh_zambia$blood.test.result, plotROC = TRUE))
### Create 'roc' object
(roc_zambia <- roc(wh_zambia$blood.test.result, predict(stepwise_model_rf, wh_zambia, type = "prob")$hiv.positive))
# OR (roc_zambia <- roc(wh_zambia$blood.test.result, predict(stepwise_model_rf, wh_zambia, type = "prob")$hiv.negative)) # same result
### Plot ROC curve
plot(roc_zambia, main = "ROC Curve -- Zambia ")
(auc_zambia <- auc(roc_zambia)) # calculate AUC score: 0.5172608 (Zambia)



# (C) Zimbabwe
##### Cleaning ZIMBABWE dataset [as what was done on Angola]
zimbabwe_hiv_label <- as.character(labelled::var_label(zimbabwe_hiv))
# OR zimbabwe_hiv_label <- as.character(sapply(zimbabwe_hiv, attr, "label"))
zimbabwe_hiv_factor <- as_factor(zimbabwe_hiv)
# Tips: levels = c("default", "labels", "values", "both")
zimbabwe_hiv_data <- zimbabwe_hiv_factor
colnames(zimbabwe_hiv_data) <- zimbabwe_hiv_label
head(zimbabwe_hiv_data)
zimbabwe_hiv_data <- dplyr::select(zimbabwe_hiv_data, -c(cluster, household, line)) ###

## IR - Women Aged 15-49 years (Zimbabwe) - Preview: -> many columns
zimbabwe_women_label <- as.character(labelled::var_label(zimbabwe_women))
# OR zimbabwe_women_label <- as.character(sapply(zimbabwe_women, attr, "label"))
zimbabwe_women_factor <- as_factor(zimbabwe_women)
zimbabwe_women_data <- zimbabwe_women_factor
colnames(zimbabwe_women_data) <- zimbabwe_women_label
head(zimbabwe_women_data[, 1:10])
##### IMPORTANT STEP - Related Columns Selection [Women's HIV] #####
# Extracting only the required sections of columns (under my study) - refer to the Standard Recode Manual
## REC75 - AIDS, STIs, and Condom Use [Len: 172]
## REC80 - AIDS, STIs, and Condom Use (continued) [Len: 113]
zimbabwe_women_selected_factor <- dplyr::select(zimbabwe_women_factor, v001, v002, v003, v750:v858)

## Merging / joining both the data
# Base file [unit of analysis] --> <zimbabwe_hiv_factor> file
final_dataset_factor_zimbabwe <- right_join(zimbabwe_women_selected_factor, zimbabwe_hiv_factor,
                                            by = c("v001" = "hivclust",
                                                   "v002" = "hivnumb",
                                                   "v003" = "hivline"),
                                            keep = FALSE,
                                            na_matches = "never")
# drop the 3 matching variables/columns (with <keep = FALSE>) for encoding purposes (carry no importance weightage)
final_dataset_factor_zimbabwe <- dplyr::select(final_dataset_factor_zimbabwe, -c(v001, v002, v003))

final_dataset_factor_zimbabwe_label <- as.character(labelled::var_label(final_dataset_factor_zimbabwe))
# OR final_dataset_factor_zimbabwe_label <- as.character(sapply(final_dataset_factor_zimbabwe, attr, "label"))
final_dataset_zimbabwe <- final_dataset_factor_zimbabwe
colnames(final_dataset_zimbabwe) <- final_dataset_factor_zimbabwe_label

## ------------------------------------------------------------------------------------------------------------------------------------- ##

# ### Data Pre-Processing Steps
# There are duplicated column names! MAKE THEM UNIQUE!!!
names(final_dataset_zimbabwe) <- make.names(names(final_dataset_zimbabwe), unique = TRUE)
colnames(final_dataset_zimbabwe)

## Important STEP!!! {feature selection/feature engineering}
# Removing na (not applicable) variables - column names starting with "na" /
# questions that are no longer part of the DHS VII core questionnaire from the final_dataset_zimbabwe
zimbabwe_fdr_1 <- dplyr::select(final_dataset_zimbabwe, -starts_with("na."))

# Replacing some survey questions' responses of NA values with their corresponding values [NA -> "no"]
zimbabwe_ih_1 <- which(colnames(zimbabwe_fdr_1) == "sought.sti.advice.treatment.from..government.central.hospital")
zimbabwe_ih_2 <- which(colnames(zimbabwe_fdr_1) == "sought.sti.advice.treatment.from..other")
# convert factor to character
zimbabwe_fdr_1[zimbabwe_ih_1:zimbabwe_ih_2] <- lapply(zimbabwe_fdr_1[zimbabwe_ih_1:zimbabwe_ih_2],
                                                      as.character)
zimbabwe_fdr_1 <- zimbabwe_fdr_1 %>% mutate_at(seq(zimbabwe_ih_1, zimbabwe_ih_2), ~replace_na(., "no"))
# convert character back to factor
zimbabwe_fdr_1[zimbabwe_ih_1:zimbabwe_ih_2] <- lapply(zimbabwe_fdr_1[zimbabwe_ih_1:zimbabwe_ih_2],
                                                      as.factor)

zimbabwe_ih_3 <- which(colnames(zimbabwe_fdr_1) == "place.for.hiv.test..government.hospital")
zimbabwe_ih_4 <- which(colnames(zimbabwe_fdr_1) == "place.for.hiv.test..other")
# convert factor to character
zimbabwe_fdr_1[zimbabwe_ih_3:zimbabwe_ih_4] <- lapply(zimbabwe_fdr_1[zimbabwe_ih_3:zimbabwe_ih_4],
                                                      as.character)
zimbabwe_fdr_1 <- zimbabwe_fdr_1 %>% mutate_at(seq(zimbabwe_ih_3, zimbabwe_ih_4), ~replace_na(., "no"))
# convert character back to factor
zimbabwe_fdr_1[zimbabwe_ih_3:zimbabwe_ih_4] <- lapply(zimbabwe_fdr_1[zimbabwe_ih_3:zimbabwe_ih_4],
                                                      as.factor)

sapply(zimbabwe_fdr_1, attr, "levels") # to see factors/levels of the latest variables
colnames(zimbabwe_fdr_1)
str(zimbabwe_fdr_1)

# some manually selected factors into <dbl> {numeric}
zimbabwe_fdr_1[list_of_characters] <- lapply(zimbabwe_fdr_1[list_of_characters], as.numeric)
glimpse(zimbabwe_fdr_1)

# dealing with HIV (response) part of the dataset
zimbabwe_fdr_2 <- dplyr::select(zimbabwe_fdr_1, -bar.code, -lab.number)
zimbabwe_fdr_2["blood.test.result"] <- lapply(zimbabwe_fdr_2["blood.test.result"], as.character)
# miracle -> only 5 unique values left! ("hiv negative", "hiv  positive", "8", "9", "indeterminate")
zimbabwe_fdr_2["blood.test.result"][zimbabwe_fdr_2["blood.test.result"] == "indeterminate" |
                                      zimbabwe_fdr_2["blood.test.result"] == "8" |
                                      zimbabwe_fdr_2["blood.test.result"] == "9"] <-
  "hiv negative"
zimbabwe_fdr_2["blood.test.result"][zimbabwe_fdr_2["blood.test.result"] == "hiv  positive"] <- "hiv positive"
# back to factor
zimbabwe_fdr_2["blood.test.result"] <- lapply(zimbabwe_fdr_2["blood.test.result"], as.factor)
str(zimbabwe_fdr_2)
sapply(zimbabwe_fdr_2, attr, "levels") # to see factors/levels of the latest variables



##### Selected variables from the FINAL MODEL:
wh_zimbabwe <- zimbabwe_fdr_2
wh_zimbabwe <- dplyr::select(wh_zimbabwe, intersect(colnames(stepwise_final), colnames(wh_zimbabwe)))

##### cleaning data again -- removing NA values
# (i) median imputation in all numeric columns
wh_zimbabwe <- wh_zimbabwe %>% mutate_if(is.double, function(x) ifelse(is.na(x), median(x, na.rm = T), x))

# (ii) replacing the missing values in bespoke category for all non-numeric columns
for (i in 1:length(wh_zimbabwe)) {
  if (sum(is.na(wh_zimbabwe[i])) > 0) {
    if ("don't know/not sure/depends" %in% levels(wh_zimbabwe[[i]])) {
      wh_zimbabwe[[i]][is.na(wh_zimbabwe[[i]])] <- "don't know/not sure/depends"
    }
    else if ("other" %in% levels(wh_zimbabwe[[i]])) {
      wh_zimbabwe[[i]][is.na(wh_zimbabwe[[i]])] <- "other"
    }
  }
}
for (i in 1:length(wh_zimbabwe)) {
  if (sum(is.na(wh_zimbabwe[i])) > 0) {
    if ("don't know" %in% levels(wh_zimbabwe[[i]])) {
      wh_zimbabwe[[i]][is.na(wh_zimbabwe[[i]])] <- "don't know"
    }
    else {
      levels(wh_zimbabwe[[i]]) <- c(levels(wh_zimbabwe[[i]]), "don't know") # add extra levels to the factors
      wh_zimbabwe[[i]][is.na(wh_zimbabwe[[i]])] <- "don't know"
    }
  }
}
any(is.na(wh_zimbabwe)) # to check still got any null values?

### convert categorical value into numerical
# important
for (i in 1:length(wh_zimbabwe)) {
  if (is.factor(wh_zimbabwe[[i]])) {
    wh_zimbabwe[[i]] <- unclass(wh_zimbabwe[[i]])
  }
} # easier method

wh_zimbabwe <- lapply(wh_zimbabwe, as.numeric)
wh_zimbabwe <- data.frame(wh_zimbabwe)
# applicable because happened to be only one column is "outersect"
wh_zimbabwe[[setdiff(colnames(stepwise_final), colnames(wh_zimbabwe))]] <- 1
wh_zimbabwe[["blood.test.result"]] <- factor(wh_zimbabwe[["blood.test.result"]], levels = c(1, 2), labels = c("hiv.negative", "hiv.positive"))
str(wh_zimbabwe)


########## TESTING MODEL RESULTS ON ZIMBABWE DATASET ##########
(cm_zimbabwe <- confusionMatrix(factor(ifelse(predict(stepwise_model_rf, wh_zimbabwe, type = "prob")$hiv.negative > 0.5,
                                              "hiv.negative", "hiv.positive"), levels = levels(wh_zimbabwe$blood.test.result)),
                                wh_zimbabwe$blood.test.result))
## ROC curve of prediction on new dataset (Zimbabwe)
# (roc_zimbabwe <- colAUC(predict(stepwise_model_rf, wh_zimbabwe, type = "prob")$hiv.negative, wh_zimbabwe$blood.test.result, plotROC = TRUE))
### Create 'roc' object
(roc_zimbabwe <- roc(wh_zimbabwe$blood.test.result, predict(stepwise_model_rf, wh_zimbabwe, type = "prob")$hiv.positive))
# OR (roc_zimbabwe <- roc(wh_zimbabwe$blood.test.result, predict(stepwise_model_rf, wh_zimbabwe, type = "prob")$hiv.negative)) # same result
### Plot ROC curve
plot(roc_zimbabwe, main = "ROC Curve -- Zimbabwe ")
(auc_zimbabwe <- auc(roc_zimbabwe)) # calculate AUC score: 0.6815505 (Zimbabwe)

# Conclusion:
## HIV Status Detection on New Datasets (Other Countries: Malawi, Zambia, Zimbabwe)
### Malawi AUC Score: 0.5201458
### Zambia AUC Score: 0.5172608
### Zimbabwe AUC Score: 0.6815505


#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#
### THE END!!! #
#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#

#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@
#!# For R Shiny only #!#
stepwise_variables_shiny <- c("relationship.with.most.recent.sex.partner", "know.a.place.to.get.hiv.test",
                              "total.lifetime.number.of.sex.partners", "respondent.can.ask.partner.to.use.a.condom",
                              "would.buy.vegetables.from.vendor.with.hiv",
                              "children.with.hiv.should.be.allowed.to.attend.school.with.children.without.hiv",
                              "hiv.transmitted.by.breastfeeding", "blood.test.result", "hiv.transmitted.during.pregnancy")

stepwise_dataset_shiny <- stepwise_final[stepwise_variables_shiny] ###!!

set.seed(175)
myFolds_stepwise_shiny <- createFolds(stepwise_dataset_shiny$blood.test.result, k = 5) # number of folds: 5
set.seed(176)
## default p = 0.75 (cross-validation split: training percentage)
myControl_stepwise_shiny <- trainControl(
  summaryFunction = twoClassSummary,
  classProbs = TRUE, # IMPORTANT!
  verboseIter = FALSE,
  savePredictions = TRUE,
  index = myFolds_stepwise_shiny
)
set.seed(177)
stepwise_rf_shiny <- train(
  x = dplyr::select(stepwise_dataset_shiny, -blood.test.result),
  y = stepwise_dataset_shiny$blood.test.result,
  tuneLength = 5, # the maximum number of tuning parameter combinations that will be generated by the random search
  metric = "ROC", # AUC as the evaluation metric
  method = "ranger", # use "ranger" instead of "rf" - faster and more effective
  trControl = myControl_stepwise_shiny
)

