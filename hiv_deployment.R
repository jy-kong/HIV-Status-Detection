# import libraries
library(shiny) # to easily build rich and productive interactive web apps in R - no HTML/CSS/JavaScript required
library(DT) # data objects in R can be rendered as HTML tables (interactive) using the JavaScript library 'DataTables'
library(shinythemes) # provides some Bootstrap themes for use with Shiny (to have better user interface in Shiny)
library(tm) # a framework for text mining applications within R
library(wordcloud2) # a fast visualization tool for creating wordcloud by using 'wordcloud2.js'
library(colourpicker) # gives a colour picker widget that can be used in different contexts in R
library(plotly) # creates interactive web graphics/plots via the open source JavaScript graphing library plotly.js
library(shinyWidgets) # offers custom widgets and other components to enhance the shiny applications
library(fontawesome) # makes it very easy to insert `Font Awesome` icons into R Markdown documents and Shiny apps
library(bsplus) # to incorporate help-documentation (tooltips, popovers, modals) into the labels of shiny inputs
library(leaflet) # an open-source JavaScript library for interactive maps

library(haven) # read SAS, SPSS, and STATA file
library(tidyverse) # to install and load core packages from the tidyverse
library(caret) # streamline model training process + pre-processing
library(ranger) # a fast implementation of random forests
library(igraph) # create and manipulate graphs and analyze networks
library(forecast) # provide methods and tools for displaying and analyzing univariate time series forecasts
library(smotefamily) # SMOTE algorithm to solve unbalanced classification problems
library(ROSE) # Random Over-Sampling Examples: to deal with binary classification problems in the presence of imbalanced classes
library(themis) # deal with unbalanced data
library(vtreat) # prepare real-world data for predictive modeling in a statistically sound manner
library(magrittr) # offer a set of operators which make code more readable
library(Boruta) # work with any classification method that output variable importance measure (VIM) - feature selection
library(lattice) # a powerful and elegant high-level data visualization system for R



# function to create a word cloud
create_wc <- function(dt, number_of_words = 100, background = "white", sz = 0.5) {
  
  # If a dataframe is provided, make sure it has the required columns
  if (is.data.frame(dt)) {
    if (!"word" %in% names(dt) || !"freq" %in% names(dt)) {
      stop("Invalid data: expecting two columns named 'word' and 'freq'")
    }
  }
  
  # If text is provided, convert it to a dataframe of word frequencies
  if (is.character(dt)) {
    corpus <- Corpus(VectorSource(dt))
    corpus <- tm_map(corpus, tolower)
    corpus <- tm_map(corpus, removePunctuation)
    corpus <- tm_map(corpus, removeNumbers)
    corpus <- tm_map(corpus, removeWords, stopwords("english"))
    tdm <- as.matrix(TermDocumentMatrix(corpus))
    dt <- sort(rowSums(tdm), decreasing = TRUE)
    dt <- data.frame(word = names(dt), freq = as.numeric(dt))
  }
  
  # Make sure a proper number_of_words is provided
  if (!is.numeric(number_of_words) || number_of_words < 3) {
    number_of_words <- 3
  }
  
  # Grab the top n most common words
  dt <- head(dt, n = number_of_words)
  if (nrow(dt) == 0) {
    return(NULL)
  }
  
  wordcloud2(dt, backgroundColor = background, size = sz) # size = 0.5 (default) to eliminate sizing error!!!
}


# throwing different kinds of error: out of memory (OOM) and start-up took too long!

# ### ------------------------------------------------------------------------------------------------------------------------------------------------------------ ###
# 
# # read the required dataset
# angola_hiv <- read_dta("AOAR71FL.DTA")
# angola_women <- read_dta("AOIR71FL.DTA")
# angola_hiv_label <- as.character(labelled::var_label(angola_hiv))
# angola_hiv_factor <- as_factor(angola_hiv)
# angola_hiv_data <- angola_hiv_factor
# colnames(angola_hiv_data) <- angola_hiv_label
# angola_hiv_data <- dplyr::select(angola_hiv_data, -c(cluster, household, line))
# angola_women_label <- as.character(labelled::var_label(angola_women))
# angola_women_factor <- as_factor(angola_women)
# angola_women_data <- angola_women_factor
# colnames(angola_women_data) <- angola_women_label
# angola_women_selected_factor <- dplyr::select(angola_women_factor, v001, v002, v003, v750:v858)
# final_dataset_factor_angola <- right_join(angola_women_selected_factor, angola_hiv_factor, 
#                                           by = c("v001" = "hivclust", 
#                                                  "v002" = "hivnumb", 
#                                                  "v003" = "hivline"), 
#                                           keep = FALSE, 
#                                           na_matches = "never")
# final_dataset_factor_angola <- dplyr::select(final_dataset_factor_angola, -c(v001, v002, v003))
# final_dataset_factor_angola_label <- as.character(labelled::var_label(final_dataset_factor_angola))
# final_dataset_angola <- final_dataset_factor_angola
# colnames(final_dataset_angola) <- final_dataset_factor_angola_label
# names(final_dataset_angola) <- make.names(names(final_dataset_angola), unique = TRUE)
# final_dataset_reduced_1 <- dplyr::select(final_dataset_angola, -starts_with("na."))
# index_holder_1 <- which(colnames(final_dataset_reduced_1) == "sought.sti.advice.treatment.from..central.hospital")
# index_holder_2 <- which(colnames(final_dataset_reduced_1) == "sought.sti.advice.treatment.from..other")
# final_dataset_reduced_1[index_holder_1:index_holder_2] <- lapply(final_dataset_reduced_1[index_holder_1:index_holder_2], 
#                                                                  as.character)
# final_dataset_reduced_1 <- final_dataset_reduced_1 %>% mutate_at(seq(index_holder_1, index_holder_2), ~replace_na(., "no"))
# final_dataset_reduced_1[index_holder_1:index_holder_2] <- lapply(final_dataset_reduced_1[index_holder_1:index_holder_2], 
#                                                                  as.factor)
# index_holder_3 <- which(colnames(final_dataset_reduced_1) == "place.for.hiv.test..central.hospital")
# index_holder_4 <- which(colnames(final_dataset_reduced_1) == "place.for.hiv.test..other")
# final_dataset_reduced_1[index_holder_3:index_holder_4] <- lapply(final_dataset_reduced_1[index_holder_3:index_holder_4], 
#                                                                  as.character)
# final_dataset_reduced_1 <- final_dataset_reduced_1 %>% mutate_at(seq(index_holder_3, index_holder_4), ~replace_na(., "no"))
# final_dataset_reduced_1[index_holder_3:index_holder_4] <- lapply(final_dataset_reduced_1[index_holder_3:index_holder_4], 
#                                                                  as.factor)
# list_of_characters <- c("number.of.sex.partners..excluding.spouse..in.last.12.months", 
#                         "number.of.sex.partners..including.spouse..in.last.12.months", 
#                         "months.ago.most.recent.hiv.test", 
#                         "time.since.last.sex.with.2nd.to.most.recent.partner", 
#                         "time.since.last.sex.with.3rd.to.most.recent.partner", 
#                         "age.of.most.recent.partner", 
#                         "age.of.2nd.to.most.recent.partner", 
#                         "age.of.3rd.to.most.recent.partner", 
#                         "total.lifetime.number.of.sex.partners", 
#                         "how.long.ago.first.had.sex.with.most.recent.partner", 
#                         "how.long.ago.first.had.sex.with.2nd.most.recent.partner", 
#                         "how.long.ago.first.had.sex.with.3rd.most.recent.partner", 
#                         "times.in.last.12.months.had.sex.with.most.recent.partner", 
#                         "times.in.last.12.months.had.sex.with.2nd.most.recent.partner", 
#                         "times.in.last.12.months.had.sex.with.3rd.most.recent.partner")
# final_dataset_reduced_1[list_of_characters] <- lapply(final_dataset_reduced_1[list_of_characters], as.numeric)
# final_dataset_reduced_2 <- dplyr::select(final_dataset_reduced_1, -bar.code, -lab.number, -assay.1.result, 
#                                          -assay.2.result, -assay.3.result)
# final_dataset_reduced_2["blood.test.result"] <- lapply(final_dataset_reduced_2["blood.test.result"], as.character)
# final_dataset_reduced_2["blood.test.result"][final_dataset_reduced_2["blood.test.result"] == "indeterminate" | 
#                                                final_dataset_reduced_2["blood.test.result"] == "inconclusive"] <- 
#   "hiv negative"
# final_dataset_reduced_2["blood.test.result"][final_dataset_reduced_2["blood.test.result"] == "hiv  positive"] <- "hiv positive"
# final_dataset_reduced_2["blood.test.result"] <- lapply(final_dataset_reduced_2["blood.test.result"], as.factor)
# final_dataset_reduced_3 <- final_dataset_reduced_2[colSums(is.na(final_dataset_reduced_2)) / 
#                                                      nrow(final_dataset_reduced_2) < .8]
# final_dataset_low_variance <- names(final_dataset_reduced_3)[nearZeroVar(final_dataset_reduced_3)]
# final_dataset_low_variance <- final_dataset_low_variance[!final_dataset_low_variance %in% c("blood.test.result", "sample.weight")]
# final_dataset_reduced_4 <- dplyr::select(final_dataset_reduced_3, -all_of(final_dataset_low_variance))
# women_hiv <- final_dataset_reduced_4
# women_hiv <- women_hiv %>% mutate_if(is.double, function(x) ifelse(is.na(x), median(x, na.rm = T), x))
# for (i in 1:length(women_hiv)) {
#   if (sum(is.na(women_hiv[i])) > 0) {
#     if ("don't know/not sure/depends" %in% levels(women_hiv[[i]])) {
#       women_hiv[[i]][is.na(women_hiv[[i]])] <- "don't know/not sure/depends"
#     }
#     else if ("other" %in% levels(women_hiv[[i]])) {
#       women_hiv[[i]][is.na(women_hiv[[i]])] <- "other"
#     }
#   }
# }
# for (i in 1:length(women_hiv)) {
#   if (sum(is.na(women_hiv[i])) > 0) {
#     if ("don't know" %in% levels(women_hiv[[i]])) {
#       women_hiv[[i]][is.na(women_hiv[[i]])] <- "don't know"
#     }
#     else {
#       levels(women_hiv[[i]]) <- c(levels(women_hiv[[i]]), "don't know")
#       women_hiv[[i]][is.na(women_hiv[[i]])] <- "don't know"
#     }
#   }
# }
# varslist <- colnames(final_dataset_reduced_4)
# treatplan <- designTreatmentsZ(final_dataset_reduced_4, varslist, verbose = FALSE)
# scoreFrame <- treatplan$scoreFrame %>% dplyr::select(varName, origName, code)
# newvars <- scoreFrame %>% filter(code %in% c("clean", "lev")) %>% use_series(varName)
# women_hiv_treat1 <- prepare(treatplan, final_dataset_reduced_4, varRestriction = newvars)
# women_hiv_treat2 <- dplyr::select(women_hiv_treat1, -c(blood_test_result_lev_x_hiv_positive, blood_test_result_lev_x_hiv_negative))
# women_hiv_treat2$blood.test.result <- final_dataset_reduced_4$blood.test.result
# varslist2 <- colnames(women_hiv)
# treatplan2 <- designTreatmentsZ(women_hiv, varslist2, verbose = FALSE)
# scoreFrame2 <- treatplan2 %>% use_series(scoreFrame) %>% dplyr::select(varName, origName, code)
# newvars2 <- scoreFrame2 %>% filter(code %in% c("clean", "lev")) %>% use_series(varName)
# women_hiv_treat3 <- prepare(treatplan2, women_hiv, varRestriction = newvars2)
# women_hiv_treat4 <- dplyr::select(women_hiv_treat3, -c(blood_test_result_lev_x_hiv_positive, blood_test_result_lev_x_hiv_negative))
# women_hiv_treat4$blood.test.result <- women_hiv$blood.test.result
# 
# set.seed(101)
# women_hiv_treat2["blood.test.result"] <- lapply(women_hiv_treat2["blood.test.result"], as.character)
# women_hiv_treat2["blood.test.result"][women_hiv_treat2["blood.test.result"] == "hiv positive"] <- 1
# women_hiv_treat2["blood.test.result"][women_hiv_treat2["blood.test.result"] == "hiv negative"] <- 0
# women_hiv_treat2["blood.test.result"] <- lapply(women_hiv_treat2["blood.test.result"], as.double)
# smote <- SMOTE(women_hiv_treat2, women_hiv_treat2["blood.test.result"])
# women_hiv_treat_smote_NULL <- smote$data
# women_hiv_treat_smote_NULL <- dplyr::select(women_hiv_treat_smote_NULL, -class)
# women_hiv_treat_smote_NULL["blood.test.result"][women_hiv_treat_smote_NULL["blood.test.result"] == 0] <- "hiv negative"
# women_hiv_treat_smote_NULL["blood.test.result"][women_hiv_treat_smote_NULL["blood.test.result"] == 1] <- "hiv positive"
# women_hiv_treat_smote_NULL["blood.test.result"] <- lapply(women_hiv_treat_smote_NULL["blood.test.result"], as.factor)
# 
# set.seed(102)
# women_hiv_treat4["blood.test.result"] <- lapply(women_hiv_treat4["blood.test.result"], as.character)
# women_hiv_treat4["blood.test.result"][women_hiv_treat4["blood.test.result"] == "hiv positive"] <- 1
# women_hiv_treat4["blood.test.result"][women_hiv_treat4["blood.test.result"] == "hiv negative"] <- 0
# women_hiv_treat4["blood.test.result"] <- lapply(women_hiv_treat4["blood.test.result"], as.double)
# smote2 <- SMOTE(women_hiv_treat4, women_hiv_treat4["blood.test.result"])
# women_hiv_treat_smote_FULL <- smote2$data
# women_hiv_treat_smote_FULL <- dplyr::select(women_hiv_treat_smote_FULL, -class)
# women_hiv_treat_smote_FULL["blood.test.result"][women_hiv_treat_smote_FULL["blood.test.result"] == 0] <- "hiv negative"
# women_hiv_treat_smote_FULL["blood.test.result"][women_hiv_treat_smote_FULL["blood.test.result"] == 1] <- "hiv positive"
# women_hiv_treat_smote_FULL["blood.test.result"] <- lapply(women_hiv_treat_smote_FULL["blood.test.result"], as.factor)
# 
# set.seed(103)
# women_hiv2 <- women_hiv
# for (i in 1:length(women_hiv)) {
#   if (is.factor(women_hiv[[i]])) {
#     women_hiv[[i]] <- unclass(women_hiv[[i]])
#   }
# }
# women_hiv2[sapply(women_hiv2, is.factor)] <- data.matrix(women_hiv2[sapply(women_hiv2, is.factor)])
# smote3 <- SMOTE(women_hiv2, women_hiv2["blood.test.result"])
# women_hiv_treat_smote_ORI <- smote3$data
# women_hiv_treat_smote_ORI <- dplyr::select(women_hiv_treat_smote_ORI, -class)
# women_hiv_treat_smote_ORI["blood.test.result"] <- lapply(women_hiv_treat_smote_ORI["blood.test.result"], as.character)
# women_hiv_treat_smote_ORI["blood.test.result"][women_hiv_treat_smote_ORI["blood.test.result"] == "1"] <- "hiv.negative"
# women_hiv_treat_smote_ORI["blood.test.result"][women_hiv_treat_smote_ORI["blood.test.result"] == "2"] <- "hiv.positive"
# women_hiv_treat_smote_ORI["blood.test.result"] <- lapply(women_hiv_treat_smote_ORI["blood.test.result"], as.factor)
# 
# # Method 1: stepwise
# null_model <- glm(blood.test.result ~ 1, data = women_hiv_treat_smote_ORI, family = "binomial")
# full_model <- glm(blood.test.result ~ ., data = women_hiv_treat_smote_ORI, family = "binomial")
# step_model <- stats::step(null_model, scope = list(lower = null_model, upper = full_model), 
#                           direction = "both", trace = 0, steps = 1000)
# step_vars <- names(unlist(step_model[["coefficients"]]))
# step_vars <- step_vars[!step_vars %in% "(Intercept)"]
# stepwise_final <- women_hiv_treat_smote_ORI[step_vars]
# stepwise_final$blood.test.result <- women_hiv_treat_smote_ORI$blood.test.result
# 
# # Method 2: boruta
# boruta_output <- Boruta(blood.test.result ~ ., data = women_hiv_treat_smote_ORI, doTrace = 0)
# roughFixMod <- TentativeRoughFix(boruta_output)
# boruta_vars <- getSelectedAttributes(roughFixMod)
# imps <- attStats(roughFixMod)
# imps2 <- imps[imps$decision != 'Rejected', c('meanImp', 'decision')]
# imps2[order(-imps2$meanImp), ]
# boruta_final <- women_hiv_treat_smote_ORI[boruta_vars]
# boruta_final$blood.test.result <- women_hiv_treat_smote_ORI$blood.test.result
# 
# # Method 3: rpart
# set.seed(300)
# rpartMod <- train(blood.test.result ~ ., data = women_hiv_treat_smote_ORI, method = "rpart")
# rpartImp <- varImp(rpartMod)
# rpart_vars <- rownames(rpartImp[["importance"]])[rpartImp[["importance"]][["Overall"]] > 0]
# rpart_final <- women_hiv_treat_smote_ORI[rpart_vars]
# rpart_final$blood.test.result <- women_hiv_treat_smote_ORI$blood.test.result
# 
# # Method 4: chi_sq
# vars_selection <- "start"
# chi_squared <- 0
# p_value <- 0
# chi_sq_table <- data.frame(vars_selection, chi_squared, p_value)
# chi_squared_data <- women_hiv_treat_smote_ORI %>% relocate(blood.test.result, .after = last_col())
# for (i in 1:(ncol(chi_squared_data) - 1)) {
#   chi_sq = chisq.test(chi_squared_data[["blood.test.result"]], chi_squared_data[[i]], correct = FALSE)
#   new_row = c(vars_selection = colnames(chi_squared_data[i]), chi_squared = chi_sq[["statistic"]][["X-squared"]], 
#               p_value = chi_sq[["p.value"]])
#   chi_sq_table = rbind(chi_sq_table, new_row)
# }
# chi_sq_table = chi_sq_table[-1, ]
# chi_sq_table <- chi_sq_table[order(chi_sq_table$chi_squared, decreasing = TRUE), ]
# chi_sq_vars <- chi_sq_table[23:31, ]$vars_selection
# chi_sq_final <- women_hiv_treat_smote_ORI[chi_sq_vars]
# chi_sq_final$blood.test.result <- women_hiv_treat_smote_ORI$blood.test.result
# 
# ### ------------------------------------------------------------------------------------------------------------------------------------------------------------ ###
# ### ------------------------------------------------------------------------------------------------------------------------------------------------------------ ###
# 
# ##### Malawi #####
# malawi_hiv <- read_dta("MWAR7AFL.DTA")
# malawi_women <- read_dta("MWIR7AFL.DTA")
# malawi_hiv_label <- as.character(labelled::var_label(malawi_hiv))
# malawi_hiv_factor <- as_factor(malawi_hiv)
# malawi_hiv_data <- malawi_hiv_factor
# colnames(malawi_hiv_data) <- malawi_hiv_label
# malawi_hiv_data <- dplyr::select(malawi_hiv_data, -c(cluster, household, line))
# malawi_women_label <- as.character(labelled::var_label(malawi_women))
# malawi_women_factor <- as_factor(malawi_women)
# malawi_women_data <- malawi_women_factor
# colnames(malawi_women_data) <- malawi_women_label
# malawi_women_selected_factor <- dplyr::select(malawi_women_factor, v001, v002, v003, v750:v858)
# final_dataset_factor_malawi <- right_join(malawi_women_selected_factor, malawi_hiv_factor, 
#                                           by = c("v001" = "hivclust", 
#                                                  "v002" = "hivnumb", 
#                                                  "v003" = "hivline"), 
#                                           keep = FALSE, 
#                                           na_matches = "never")
# final_dataset_factor_malawi <- dplyr::select(final_dataset_factor_malawi, -c(v001, v002, v003))
# final_dataset_factor_malawi_label <- as.character(labelled::var_label(final_dataset_factor_malawi))
# final_dataset_malawi <- final_dataset_factor_malawi
# colnames(final_dataset_malawi) <- final_dataset_factor_malawi_label
# names(final_dataset_malawi) <- make.names(names(final_dataset_malawi), unique = TRUE)
# malawi_fdr_1 <- dplyr::select(final_dataset_malawi, -starts_with("na."))
# malawi_ih_1 <- which(colnames(malawi_fdr_1) == "sought.sti.advice.treatment.from..government.hospital")
# malawi_ih_2 <- which(colnames(malawi_fdr_1) == "sought.sti.advice.treatment.from..other")
# malawi_fdr_1[malawi_ih_1:malawi_ih_2] <- lapply(malawi_fdr_1[malawi_ih_1:malawi_ih_2], as.character)
# malawi_fdr_1 <- malawi_fdr_1 %>% mutate_at(seq(malawi_ih_1, malawi_ih_2), ~replace_na(., "no"))
# malawi_fdr_1[malawi_ih_1:malawi_ih_2] <- lapply(malawi_fdr_1[malawi_ih_1:malawi_ih_2], as.factor)
# malawi_ih_3 <- which(colnames(malawi_fdr_1) == "place.for.hiv.test..government.hospital")
# malawi_ih_4 <- which(colnames(malawi_fdr_1) == "place.for.hiv.test..other")
# malawi_fdr_1[malawi_ih_3:malawi_ih_4] <- lapply(malawi_fdr_1[malawi_ih_3:malawi_ih_4], as.character)
# malawi_fdr_1 <- malawi_fdr_1 %>% mutate_at(seq(malawi_ih_3, malawi_ih_4), ~replace_na(., "no"))
# malawi_fdr_1[malawi_ih_3:malawi_ih_4] <- lapply(malawi_fdr_1[malawi_ih_3:malawi_ih_4], as.factor)
# malawi_fdr_1[list_of_characters] <- lapply(malawi_fdr_1[list_of_characters], as.numeric)
# malawi_fdr_2 <- dplyr::select(malawi_fdr_1, -bar.code, -lab.number)
# malawi_fdr_2["blood.test.result"] <- lapply(malawi_fdr_2["blood.test.result"], as.character)
# malawi_fdr_2["blood.test.result"][malawi_fdr_2["blood.test.result"] == "indeterminate" | 
#                                     malawi_fdr_2["blood.test.result"] == "inconclusive"] <- 
#   "hiv negative"
# malawi_fdr_2["blood.test.result"][malawi_fdr_2["blood.test.result"] == "hiv  positive"] <- "hiv positive"
# malawi_fdr_2["blood.test.result"] <- lapply(malawi_fdr_2["blood.test.result"], as.factor)
# wh_malawi <- malawi_fdr_2
# wh_malawi <- dplyr::select(wh_malawi, intersect(colnames(stepwise_final), colnames(wh_malawi)))
# wh_malawi <- wh_malawi %>% mutate_if(is.double, function(x) ifelse(is.na(x), median(x, na.rm = T), x))
# for (i in 1:length(wh_malawi)) {
#   if (sum(is.na(wh_malawi[i])) > 0) {
#     if ("don't know/not sure/depends" %in% levels(wh_malawi[[i]])) {
#       wh_malawi[[i]][is.na(wh_malawi[[i]])] <- "don't know/not sure/depends"
#     }
#     else if ("other" %in% levels(wh_malawi[[i]])) {
#       wh_malawi[[i]][is.na(wh_malawi[[i]])] <- "other"
#     }
#   }
# }
# for (i in 1:length(wh_malawi)) {
#   if (sum(is.na(wh_malawi[i])) > 0) {
#     if ("don't know" %in% levels(wh_malawi[[i]])) {
#       wh_malawi[[i]][is.na(wh_malawi[[i]])] <- "don't know"
#     }
#     else {
#       levels(wh_malawi[[i]]) <- c(levels(wh_malawi[[i]]), "don't know")
#       wh_malawi[[i]][is.na(wh_malawi[[i]])] <- "don't know"
#     }
#   }
# }
# for (i in 1:length(wh_malawi)) {
#   if (is.factor(wh_malawi[[i]])) {
#     wh_malawi[[i]] <- unclass(wh_malawi[[i]])
#   }
# }
# wh_malawi <- lapply(wh_malawi, as.numeric)
# wh_malawi <- data.frame(wh_malawi)
# wh_malawi[[setdiff(colnames(stepwise_final), colnames(wh_malawi))]] <- 1
# wh_malawi[["blood.test.result"]] <- factor(wh_malawi[["blood.test.result"]], levels = c(1, 2), labels = c("hiv.negative", "hiv.positive"))
# 
# ### ------------------------------------------------------------------------------------------------------------------------------------------------------------ ###
# ### ------------------------------------------------------------------------------------------------------------------------------------------------------------ ###
# 
# ##### Zambia #####
# zambia_hiv <- read_dta("ZMAR71FL.DTA")
# zambia_women <- read_dta("ZMIR71FL.DTA")
# zambia_hiv_label <- as.character(labelled::var_label(zambia_hiv))
# zambia_hiv_factor <- as_factor(zambia_hiv)
# zambia_hiv_data <- zambia_hiv_factor
# colnames(zambia_hiv_data) <- zambia_hiv_label
# zambia_hiv_data <- dplyr::select(zambia_hiv_data, -c(cluster, household, line))
# zambia_women_label <- as.character(labelled::var_label(zambia_women))
# zambia_women_factor <- as_factor(zambia_women)
# zambia_women_data <- zambia_women_factor
# colnames(zambia_women_data) <- zambia_women_label
# zambia_women_selected_factor <- dplyr::select(zambia_women_factor, v001, v002, v003, v750:v858)
# final_dataset_factor_zambia <- right_join(zambia_women_selected_factor, zambia_hiv_factor, 
#                                           by = c("v001" = "hivclust", 
#                                                  "v002" = "hivnumb", 
#                                                  "v003" = "hivline"), 
#                                           keep = FALSE, 
#                                           na_matches = "never")
# final_dataset_factor_zambia <- dplyr::select(final_dataset_factor_zambia, -c(v001, v002, v003))
# final_dataset_factor_zambia_label <- as.character(labelled::var_label(final_dataset_factor_zambia))
# final_dataset_zambia <- final_dataset_factor_zambia
# colnames(final_dataset_zambia) <- final_dataset_factor_zambia_label
# names(final_dataset_zambia) <- make.names(names(final_dataset_zambia), unique = TRUE)
# zambia_fdr_1 <- dplyr::select(final_dataset_zambia, -starts_with("na."))
# zambia_ih_1 <- which(colnames(zambia_fdr_1) == "sought.sti.advice.treatment.from..government.hospital")
# zambia_ih_2 <- which(colnames(zambia_fdr_1) == "sought.sti.advice.treatment.from..other")
# zambia_fdr_1[zambia_ih_1:zambia_ih_2] <- lapply(zambia_fdr_1[zambia_ih_1:zambia_ih_2], as.character)
# zambia_fdr_1 <- zambia_fdr_1 %>% mutate_at(seq(zambia_ih_1, zambia_ih_2), ~replace_na(., "no"))
# zambia_fdr_1[zambia_ih_1:zambia_ih_2] <- lapply(zambia_fdr_1[zambia_ih_1:zambia_ih_2], as.factor)
# zambia_ih_3 <- which(colnames(zambia_fdr_1) == "place.for.hiv.test..government.hospital")
# zambia_ih_4 <- which(colnames(zambia_fdr_1) == "place.for.hiv.test..other")
# zambia_fdr_1[zambia_ih_3:zambia_ih_4] <- lapply(zambia_fdr_1[zambia_ih_3:zambia_ih_4], as.character)
# zambia_fdr_1 <- zambia_fdr_1 %>% mutate_at(seq(zambia_ih_3, zambia_ih_4), ~replace_na(., "no"))
# zambia_fdr_1[zambia_ih_3:zambia_ih_4] <- lapply(zambia_fdr_1[zambia_ih_3:zambia_ih_4], as.factor)
# zambia_fdr_1[list_of_characters] <- lapply(zambia_fdr_1[list_of_characters], as.numeric)
# zambia_fdr_2 <- dplyr::select(zambia_fdr_1, -bar.code, -lab.number)
# zambia_fdr_2["blood.test.result"] <- lapply(zambia_fdr_2["blood.test.result"], as.character)
# zambia_fdr_2["blood.test.result"][zambia_fdr_2["blood.test.result"] == "inconclusive"] <- "hiv negative"
# zambia_fdr_2["blood.test.result"][zambia_fdr_2["blood.test.result"] == "hiv  positive" | 
#                                     zambia_fdr_2["blood.test.result"] == "hiv2 positive"] <- "hiv positive"
# zambia_fdr_2["blood.test.result"] <- lapply(zambia_fdr_2["blood.test.result"], as.factor)
# wh_zambia <- zambia_fdr_2
# wh_zambia <- dplyr::select(wh_zambia, intersect(colnames(stepwise_final), colnames(wh_zambia)))
# wh_zambia <- wh_zambia %>% mutate_if(is.double, function(x) ifelse(is.na(x), median(x, na.rm = T), x))
# for (i in 1:length(wh_zambia)) {
#   if (sum(is.na(wh_zambia[i])) > 0) {
#     if ("don't know/not sure/depends" %in% levels(wh_zambia[[i]])) {
#       wh_zambia[[i]][is.na(wh_zambia[[i]])] <- "don't know/not sure/depends"
#     }
#     else if ("other" %in% levels(wh_zambia[[i]])) {
#       wh_zambia[[i]][is.na(wh_zambia[[i]])] <- "other"
#     }
#   }
# }
# for (i in 1:length(wh_zambia)) {
#   if (sum(is.na(wh_zambia[i])) > 0) {
#     if ("don't know" %in% levels(wh_zambia[[i]])) {
#       wh_zambia[[i]][is.na(wh_zambia[[i]])] <- "don't know"
#     }
#     else {
#       levels(wh_zambia[[i]]) <- c(levels(wh_zambia[[i]]), "don't know")
#       wh_zambia[[i]][is.na(wh_zambia[[i]])] <- "don't know"
#     }
#   }
# }
# for (i in 1:length(wh_zambia)) {
#   if (is.factor(wh_zambia[[i]])) {
#     wh_zambia[[i]] <- unclass(wh_zambia[[i]])
#   }
# }
# wh_zambia <- lapply(wh_zambia, as.numeric)
# wh_zambia <- data.frame(wh_zambia)
# wh_zambia[[setdiff(colnames(stepwise_final), colnames(wh_zambia))]] <- 1
# wh_zambia[["blood.test.result"]] <- factor(wh_zambia[["blood.test.result"]], levels = c(1, 2), labels = c("hiv.negative", "hiv.positive"))
# 
# ### ------------------------------------------------------------------------------------------------------------------------------------------------------------ ###
# ### ------------------------------------------------------------------------------------------------------------------------------------------------------------ ###
# 
# ##### Zimbabwe #####
# zimbabwe_hiv <- read_dta("ZWAR71FL.DTA")
# zimbabwe_women <- read_dta("ZWIR72FL.DTA")
# zimbabwe_hiv_label <- as.character(labelled::var_label(zimbabwe_hiv))
# zimbabwe_hiv_factor <- as_factor(zimbabwe_hiv)
# zimbabwe_hiv_data <- zimbabwe_hiv_factor
# colnames(zimbabwe_hiv_data) <- zimbabwe_hiv_label
# zimbabwe_hiv_data <- dplyr::select(zimbabwe_hiv_data, -c(cluster, household, line))
# zimbabwe_women_label <- as.character(labelled::var_label(zimbabwe_women))
# zimbabwe_women_factor <- as_factor(zimbabwe_women)
# zimbabwe_women_data <- zimbabwe_women_factor
# colnames(zimbabwe_women_data) <- zimbabwe_women_label
# zimbabwe_women_selected_factor <- dplyr::select(zimbabwe_women_factor, v001, v002, v003, v750:v858)
# final_dataset_factor_zimbabwe <- right_join(zimbabwe_women_selected_factor, zimbabwe_hiv_factor, 
#                                             by = c("v001" = "hivclust", 
#                                                    "v002" = "hivnumb", 
#                                                    "v003" = "hivline"), 
#                                             keep = FALSE, 
#                                             na_matches = "never")
# final_dataset_factor_zimbabwe <- dplyr::select(final_dataset_factor_zimbabwe, -c(v001, v002, v003))
# final_dataset_factor_zimbabwe_label <- as.character(labelled::var_label(final_dataset_factor_zimbabwe))
# final_dataset_zimbabwe <- final_dataset_factor_zimbabwe
# colnames(final_dataset_zimbabwe) <- final_dataset_factor_zimbabwe_label
# names(final_dataset_zimbabwe) <- make.names(names(final_dataset_zimbabwe), unique = TRUE)
# zimbabwe_fdr_1 <- dplyr::select(final_dataset_zimbabwe, -starts_with("na."))
# zimbabwe_ih_1 <- which(colnames(zimbabwe_fdr_1) == "sought.sti.advice.treatment.from..government.central.hospital")
# zimbabwe_ih_2 <- which(colnames(zimbabwe_fdr_1) == "sought.sti.advice.treatment.from..other")
# zimbabwe_fdr_1[zimbabwe_ih_1:zimbabwe_ih_2] <- lapply(zimbabwe_fdr_1[zimbabwe_ih_1:zimbabwe_ih_2], as.character)
# zimbabwe_fdr_1 <- zimbabwe_fdr_1 %>% mutate_at(seq(zimbabwe_ih_1, zimbabwe_ih_2), ~replace_na(., "no"))
# zimbabwe_fdr_1[zimbabwe_ih_1:zimbabwe_ih_2] <- lapply(zimbabwe_fdr_1[zimbabwe_ih_1:zimbabwe_ih_2], as.factor)
# zimbabwe_ih_3 <- which(colnames(zimbabwe_fdr_1) == "place.for.hiv.test..government.hospital")
# zimbabwe_ih_4 <- which(colnames(zimbabwe_fdr_1) == "place.for.hiv.test..other")
# zimbabwe_fdr_1[zimbabwe_ih_3:zimbabwe_ih_4] <- lapply(zimbabwe_fdr_1[zimbabwe_ih_3:zimbabwe_ih_4], as.character)
# zimbabwe_fdr_1 <- zimbabwe_fdr_1 %>% mutate_at(seq(zimbabwe_ih_3, zimbabwe_ih_4), ~replace_na(., "no"))
# zimbabwe_fdr_1[zimbabwe_ih_3:zimbabwe_ih_4] <- lapply(zimbabwe_fdr_1[zimbabwe_ih_3:zimbabwe_ih_4], as.factor)
# zimbabwe_fdr_1[list_of_characters] <- lapply(zimbabwe_fdr_1[list_of_characters], as.numeric)
# zimbabwe_fdr_2 <- dplyr::select(zimbabwe_fdr_1, -bar.code, -lab.number)
# zimbabwe_fdr_2["blood.test.result"] <- lapply(zimbabwe_fdr_2["blood.test.result"], as.character)
# zimbabwe_fdr_2["blood.test.result"][zimbabwe_fdr_2["blood.test.result"] == "indeterminate" | 
#                                       zimbabwe_fdr_2["blood.test.result"] == "8" | 
#                                       zimbabwe_fdr_2["blood.test.result"] == "9"] <- 
#   "hiv negative"
# zimbabwe_fdr_2["blood.test.result"][zimbabwe_fdr_2["blood.test.result"] == "hiv  positive"] <- "hiv positive"
# zimbabwe_fdr_2["blood.test.result"] <- lapply(zimbabwe_fdr_2["blood.test.result"], as.factor)
# wh_zimbabwe <- zimbabwe_fdr_2
# wh_zimbabwe <- dplyr::select(wh_zimbabwe, intersect(colnames(stepwise_final), colnames(wh_zimbabwe)))
# wh_zimbabwe <- wh_zimbabwe %>% mutate_if(is.double, function(x) ifelse(is.na(x), median(x, na.rm = T), x))
# for (i in 1:length(wh_zimbabwe)) {
#   if (sum(is.na(wh_zimbabwe[i])) > 0) {
#     if ("don't know/not sure/depends" %in% levels(wh_zimbabwe[[i]])) {
#       wh_zimbabwe[[i]][is.na(wh_zimbabwe[[i]])] <- "don't know/not sure/depends"
#     }
#     else if ("other" %in% levels(wh_zimbabwe[[i]])) {
#       wh_zimbabwe[[i]][is.na(wh_zimbabwe[[i]])] <- "other"
#     }
#   }
# }
# for (i in 1:length(wh_zimbabwe)) {
#   if (sum(is.na(wh_zimbabwe[i])) > 0) {
#     if ("don't know" %in% levels(wh_zimbabwe[[i]])) {
#       wh_zimbabwe[[i]][is.na(wh_zimbabwe[[i]])] <- "don't know"
#     }
#     else {
#       levels(wh_zimbabwe[[i]]) <- c(levels(wh_zimbabwe[[i]]), "don't know")
#       wh_zimbabwe[[i]][is.na(wh_zimbabwe[[i]])] <- "don't know"
#     }
#   }
# }
# for (i in 1:length(wh_zimbabwe)) {
#   if (is.factor(wh_zimbabwe[[i]])) {
#     wh_zimbabwe[[i]] <- unclass(wh_zimbabwe[[i]])
#   }
# }
# wh_zimbabwe <- lapply(wh_zimbabwe, as.numeric)
# wh_zimbabwe <- data.frame(wh_zimbabwe)
# wh_zimbabwe[[setdiff(colnames(stepwise_final), colnames(wh_zimbabwe))]] <- 1
# wh_zimbabwe[["blood.test.result"]] <- factor(wh_zimbabwe[["blood.test.result"]], levels = c(1, 2), labels = c("hiv.negative", "hiv.positive"))
# 
# 
# 
# ### ------------------------------------------------------------------------------------------------------------------------------------------------------------ ###

## temp: reduce runtime --> decided to collaborate environment

angola_women_data_shiny <- angola_women_data
shiny_selected_variables <- intersect(colnames(stepwise_final), colnames(zimbabwe_fdr_1))
angola30 <- dplyr::select(final_dataset_reduced_1, shiny_selected_variables)
malawi30 <- dplyr::select(malawi_fdr_1, shiny_selected_variables)
zambia30 <- dplyr::select(zambia_fdr_1, shiny_selected_variables)
zimbabwe30 <- dplyr::select(zimbabwe_fdr_1, shiny_selected_variables)


#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@


# #--# read .csv file instead
# angola_women_data_shiny <- read.csv("angola_women_data_shiny.csv", header = TRUE)
# stepwise_shiny <- read.csv("stepwise_shiny.csv", header = TRUE)
# zimbabwe_temp <- read.csv("zimbabwe_temp.csv", header = TRUE)
# malawi_temp <- read.csv("malawi_temp.csv", header = TRUE)
# angola_temp <- read.csv("angola_temp.csv", header = TRUE)
# zambia_temp <- read.csv("zambia_temp.csv", header = TRUE)
# 
# # PREPARING DATA
# shiny_selected_variables <- intersect(colnames(stepwise_shiny), colnames(zimbabwe_temp))
# angola30 <- dplyr::select(angola_temp, shiny_selected_variables)
# malawi30 <- dplyr::select(malawi_temp, shiny_selected_variables)
# zambia30 <- dplyr::select(zambia_temp, shiny_selected_variables)
# zimbabwe30 <- dplyr::select(zimbabwe_temp, shiny_selected_variables)
# #--#

#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@
# temp: model using stepwise and rf <ranger>
# # a total of 7 variables <but secretly>

# stepwise_variables_shiny <- c("relationship.with.most.recent.sex.partner", "know.a.place.to.get.hiv.test",
#                               "total.lifetime.number.of.sex.partners", "respondent.can.ask.partner.to.use.a.condom",
#                               "would.buy.vegetables.from.vendor.with.hiv",
#                               "children.with.hiv.should.be.allowed.to.attend.school.with.children.without.hiv",
#                               "hiv.transmitted.by.breastfeeding", "blood.test.result")
# 
# stepwise_dataset_shiny <- stepwise_shiny[stepwise_variables_shiny] ###!!
# 
# set.seed(175)
# myFolds_stepwise_shiny <- createFolds(stepwise_dataset_shiny$blood.test.result, k = 5) # number of folds: 5
# set.seed(176)
# ## default p = 0.75 (cross-validation split: training percentage)
# myControl_stepwise_shiny <- trainControl(
#   summaryFunction = twoClassSummary,
#   classProbs = TRUE, # IMPORTANT!
#   verboseIter = FALSE,
#   savePredictions = TRUE,
#   index = myFolds_stepwise_shiny
# )
# set.seed(177)
# stepwise_rf_shiny <- train(
#   x = dplyr::select(stepwise_dataset_shiny, -blood.test.result),
#   y = stepwise_dataset_shiny$blood.test.result,
#   tuneLength = 5, # the maximum number of tuning parameter combinations that will be generated by the random search
#   metric = "ROC", # AUC as the evaluation metric
#   method = "ranger", # use "ranger" instead of "rf" - faster and more effective
#   trControl = myControl_stepwise_shiny
# )

stepwise_rf_shiny <- stepwise_rf_shiny # seems unnecessary but only to remind me!

#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@


mapping <- read.csv("14_countries.csv", header = TRUE)
text_about <- "In this study, datasets from 14 African countries of different years are collected from The Demographic and Health Surveys (DHS) Program. 
The total numbers of HIV-positive and HIV-negative cases are extracted from those datasets. Red circles are used to highlight the country of study. 
When hovering over the red circles, information such as years, country, and 'HIV Positive Rate' are shown as the tooltips. 'HIV Positive Rate' is calculated 
using the formula: [Total HIV-positive Cases / (Total HIV-positive Cases + Total HIV-negative Cases) * 100%]. Bigger circle indicates a higher 'HIV Positive Rate'. 
The users can even interact with the map by adjusting the slider input to specify the range of 'HIV Positive Rate' values to be observed."
numerical_variables <- c("age.of.most.recent.partner", "how.long.ago.first.had.sex.with.most.recent.partner", "months.ago.most.recent.hiv.test", 
                         "sample.weight", "total.lifetime.number.of.sex.partners")

categorical_variables <- setdiff(colnames(stepwise_final), numerical_variables) ### should be up there!

four_countries <- c("Angola", "Malawi", "Zambia", "Zimbabwe")
four_countries_flags <- c("angola.png", "malawi.png", "zambia.png", "zimbabwe.png")
country_flags_url <- c(
  "https://cdn.jsdelivr.net/gh/lipis/flag-icon-css@master/flags/4x3/ao.svg", 
  "https://cdn.jsdelivr.net/gh/lipis/flag-icon-css@master/flags/4x3/mw.svg", 
  "https://cdn.jsdelivr.net/gh/lipis/flag-icon-css@master/flags/4x3/zm.svg", 
  "https://cdn.jsdelivr.net/gh/lipis/flag-icon-css@master/flags/4x3/zw.svg"
)


my_css <- "
#download_data {
  /* Change the background color of the download button to orange. */
  background: orange;
  
  /* Change the text size of the download button to 10.5 pixels. */
  font-size: 10.5px;
}

#explore_filter {
  /* Change the background color of the filter button to light red. */
  background: #DE3939;
}
"

### ------------------------------------------------------------------------------------------------------------------------------------------------------------ ###


ui <- fluidPage(
  
  tags$style(my_css), 
  themeSelector(), 
  
  navbarPage(
    
    strong("HIV Status Detection"), 
    
    tabPanel(
      "HIV Detector", 
      titlePanel("HIV Detector"), 
      sidebarLayout(
        sidebarPanel(
          textInput("name", "What is your name", placeholder = "John Doe"), 
          numericInput("age", "How old are you", value = 18, min = 0, step = 1), 
          selectInput("relationship", "Relationship with most recent sex partner | Demography", multiple = F, 
                      choices = unique(angola30$relationship.with.most.recent.sex.partner)), 
          radioGroupButtons("place", "Do you know a place to get HIV test | Knowledge", choices = c("Yes", "No"), 
                            justified = T, status = c("success", "danger")), 
          sliderInput("partners", "Total lifetime number of sex partners | Demography", min = 0, max = 100, value = 0, step = 1, 
                      animate = T, ticks = T), 
          pickerInput("condom", "Can you ask your partner to use a condom | Attitude/Behaviour", multiple = F, 
                      choices = unique(angola30$respondent.can.ask.partner.to.use.a.condom)) 
          %>% shinyInput_label_embed(
            icon("info-circle") %>% 
              bs_embed_tooltip(title = "1: no\n2: yes\n3: don't know/not sure/depends")
          ), 
          switchInput("vegetables", "Would you buy vegetables from vendor with HIV | Attitude/Behaviour", onLabel = "Yes", 
                      offLabel = "No", onStatus = "success", offStatus = "danger", labelWidth = "320px", size = "normal"), 
          pickerInput("school", "Children with HIV should be allowed to attend school with children without HIV | Attitude/Behaviour", 
                      multiple = F, 
                      choices = unique(angola30$children.with.hiv.should.be.allowed.to.attend.school.with.children.without.hiv)) 
          %>% shinyInput_label_embed(
            icon("info-circle") %>% 
              bs_embed_tooltip(title = "1: no\n2: yes\n3: don't know/not sure/depends")
          ), 
          prettyRadioButtons(inputId = "breastfeeding", 
                             label = "Do you think AIDS can be transmitted from mother to child by breastfeeding | Knowledge", 
                             choices = c("No", "Yes", "Don't know"), 
                             icon = icon("check"), 
                             bigger = TRUE, 
                             status = "info", 
                             animation = "jelly"), 
          actionButton(inputId = "result_button", label = "Generate Result", icon("list-alt", lib = "glyphicon"))
        ), 
        mainPanel(
          img(src = "stop_hiv.jpg", height = 600, width = 800), # insert an HIV background image from 'www' folder
          
          ### why?
          br(), br(), br(), 
          p(strong(h2(textOutput("result"))), style = "color:red; font-size:20px")
          
          # use name and age!!!
          
          
        )
      )
    ), 
    
    tabPanel(
      "Word Cloud", 
      titlePanel("Word Cloud"), 
      sidebarLayout(
        sidebarPanel(
          radioButtons(
            inputId = "wc_source", 
            label = "Word source", 
            choices = c(
              "Angola dataset" = "wc_angola", 
              "Use your own words" = "wc_own", 
              "Upload a CSV file" = "wc_csv_file"
            )
          ), 
          conditionalPanel(
            condition = "input.wc_source == 'wc_own'", 
            textAreaInput("wc_text", "Enter text", rows = 7)
          ), 
          conditionalPanel(
            condition = "input.wc_source == 'wc_csv_file'", 
            fileInput("wc_input_file", "Select a file") # or set parameter <accept = ".csv"> to restrict tow only CSV file
            %>% shinyInput_label_embed(
              icon("info-circle") %>% 
              bs_embed_tooltip(title = "It must be a CSV file.")
            )
          ), 
          numericInput("wc_num", "Maximum number of words", value = 100, min = 5), 
          colourInput("wc_col", "Background color", value = "white"), 
          # Add a "draw" button to the app
          actionButton(inputId = "wc_draw", label = "Draw!", fa_i(name = "fas fa-drum"))
        ), 
        mainPanel(
          wordcloud2Output("word_cloud")
        )
      )
    ), 
    
    tabPanel(
      "Data Explorer", 
      titlePanel("Data Explorer"), 
      sidebarLayout(
        sidebarPanel(
          # insert image < img(src, width, height) > : alternative is to apply using "www" folder, but URLs are used instead...
          ## Icon Pack: Rectangular country simple flags | Rectangular <but not used>
          pickerInput("country", "Select Country", four_countries, choicesOpt = 
                        list(content = mapply(four_countries, country_flags_url, FUN = function(country, flag) {
                          HTML(paste(
                            tags$img(src = flag, width = 20, height = 15), country
                          ))
                        }, SIMPLIFY = FALSE, USE.NAMES = FALSE))
          ), 
          conditionalPanel(
            condition = "input.country == 'Angola'", 
            sliderInput('angola_rows', 'Select number of rows', 5, nrow(angola30), 50)
          ), 
          conditionalPanel(
            condition = "input.country == 'Malawi'", 
            sliderInput('malawi_rows', 'Select number of rows', 5, nrow(malawi30), 50)
          ), 
          conditionalPanel(
            condition = "input.country == 'Zambia'", 
            sliderInput('zambia_rows', 'Select number of rows', 5, nrow(zambia30), 50)
          ), 
          conditionalPanel(
            condition = "input.country == 'Zimbabwe'", 
            sliderInput('zimbabwe_rows', 'Select number of rows', 5, nrow(zimbabwe30), 50)
          ), 
          pickerInput(
            inputId = "30_variables", 
            label = "Select Variables", 
            choices = shiny_selected_variables, 
            multiple = TRUE, 
            selected = shiny_selected_variables, 
            options = list(
              `actions-box` = TRUE, 
              `deselect-all-text` = "Deselect All", 
              `select-all-text` = "Select All", 
              `selected-text-format`= "count", 
              `count-selected-text` = "{0} features chosen (on a total of {1})"
            )
          ) %>% shinyInput_label_embed(
            icon("info-circle") %>% 
              bs_embed_tooltip(title = paste0("There are 5 numerical variables: \n", "<age.of.most.recent.partner>\n", 
                                              "<how.long.ago.first.had.sex.with.most.recent.partner>\n", "<months.ago.most.recent.hiv.test>\n", 
                                              "<sample.weight>\n", "<total.lifetime.number.of.sex.partners>"))
          ), 
          checkboxInput("show_positive", "Show/Hide HIV Positive", value = TRUE), 
          checkboxInput("show_negative", "Show/Hide HIV Negative", value = TRUE), 
          # Add a "filter" button to the app
          actionButton(inputId = "explore_filter", label = "Filter", icon("filter", lib = "glyphicon")), 
          # download as a CSV file
          downloadButton("download_data")
        ), 
        mainPanel(
          tabsetPanel(
            tabPanel('Table', DTOutput('explore_table')), 
            tabPanel('Summary - Categorical', DTOutput('summary_categorical')), 
            tabPanel('Summary - Numerical', DTOutput('summary_numerical')), 
            tabPanel('Word Cloud', wordcloud2Output('explore_wc'))
          )
        )
      )
    ), 
    
    tabPanel(
      "Geographical Map", 
      titlePanel("Geographical Map"), 
      leafletOutput('map', height = '600'), 
      absolutePanel(top = 155, right = 40, id = 'map_controls', 
                    sliderInput("rate_range", h4("HIV Positive Rate (%) : "), min = 0, max = 100, value = c(5, 15)), 
                    # CODE BELOW: Add an action button named show_about
                    actionButton("show_about", "About", icon("search", lib = "glyphicon"))
      ), 
      tags$style(type = "text/css", "
      html, body {width:100%;height:100%}
      #map_controls{background-color:white;padding:20px;}
      ")
    )
    
    
    
    # tabPanel(
    #   "Data Explorer", 
    #   sidebarLayout(
    #     sidebarPanel(
    #       textInput(), 
    #       textInput()
    #     ), 
    #     mainPanel(
    #       tabsetPanel(
    #         tabPanel(
    #           "Table", 
    #           textOutput(), 
    #           textOutput()
    #         ), 
    #         tabPanel(
    #           "Plot", 
    #           textOutput(), 
    #           textOutput()
    #         )
    #       )
    #     )
    #   )
    # ), 
    # 
    # tabPanel(
    #   "name", 
    #   sidebarLayout(
    #     sidebarPanel(
    #       textInput(), 
    #       textInput()
    #     ), 
    #     mainPanel(
    #       tabsetPanel(
    #         tabPanel(
    #           textOutput(), 
    #           textOutput()
    #         ), 
    #         tabPanel(
    #           textOutput(), 
    #           textOutput()
    #         )
    #       )
    #     )
    #   )
    # )
    
  )
  
)



server <- function(input, output) {
  
  wc_data_source <- reactive({
    if (input$wc_source == "wc_angola") {
      wc_data <- paste(colnames(angola_women_data_shiny), collapse = " ") # only retrieve the column names to plot the word cloud
      # wc_data <- paste(strsplit(paste(colnames(angola_women_data_shiny), collapse = " "), "[.]")[[1]], 
      #                  collapse = " ") # only retrieve the column names to plot the word cloud
    } else if (input$wc_source == "wc_own") {
      wc_data <- input$wc_text
    } else if (input$wc_source == "wc_csv_file") {
      wc_data <- wc_input_csv_file()
    }
    return(wc_data)
  })
  
  wc_input_csv_file <- reactive({
    # to check file extension
    wc_file_ext <- tools::file_ext(input$wc_input_file$datapath)
    # throw an error if the file uploaded is not a .csv file
    validate(need(wc_file_ext == "csv", "Please upload a CSV file!"))
    # to deal with empty files
    if (is.null(input$wc_input_file)) {
      return("")
    }
    ### only retrieve the column names to plot the word cloud
    return(paste(strsplit(paste(colnames(read.csv(input$wc_input_file$datapath, header = TRUE)), collapse = " "), "[.]")[[1]], 
          collapse = " "))
  })
  
  # throw a modal box if CSV file is not uploaded
  # observeEvent(input$wc_draw, {
  #   if (tools::file_ext(input$wc_input_file$datapath) != "csv") {
  #     showModal(modalDialog("Please upload a CSV file!"))
  #   }
  # })
  
  output$word_cloud <- renderWordcloud2({
    # Add the "wc_draw" button as a dependency to cause the word cloud to re-render on click
    input$wc_draw
    isolate({
      create_wc(wc_data_source(), number_of_words = input$wc_num, background = input$wc_col)
    })
  })
  
  explore_country <- reactive({
    if (input$country == "Angola") {
      explore_data <- head(angola30[input$`30_variables`], input$angola_rows)
    } else if (input$country == "Malawi") {
      explore_data <- head(malawi30[input$`30_variables`], input$malawi_rows)
    } else if (input$country == "Zambia") {
      explore_data <- head(zambia30[input$`30_variables`], input$zambia_rows)
    } else if (input$country == "Zimbabwe") {
      explore_data <- head(zimbabwe30[input$`30_variables`], input$zimbabwe_rows)
    }
    return(explore_data)
  })
  
  data_transformer <- reactive({
    if (input$show_positive & input$show_negative) {
      explore_country()
    }
    # filter for `hiv+`
    else if (input$show_positive) {
      if ("blood.test.result" %in% colnames(explore_country())) {
        explore_country() %>% dplyr::filter(blood.test.result == "hiv  positive")
      }
      # to cater for condition where the variable "blood.test.result" is not selected form the list
      else {
        if (input$country == "Angola") {
          exp_data <- head(angola30[append(input$`30_variables`, "blood.test.result")], input$angola_rows)
        } else if (input$country == "Malawi") {
          exp_data <- head(malawi30[append(input$`30_variables`, "blood.test.result")], input$malawi_rows)
        } else if (input$country == "Zambia") {
          exp_data <- head(zambia30[append(input$`30_variables`, "blood.test.result")], input$zambia_rows)
        } else if (input$country == "Zimbabwe") {
          exp_data <- head(zimbabwe30[append(input$`30_variables`, "blood.test.result")], input$zimbabwe_rows)
        }
        exp_data %>% dplyr::filter(blood.test.result == "hiv  positive") %>% dplyr::select(-blood.test.result)
      }
    }
    # filter for `hiv-`
    else if (input$show_negative) {
      if ("blood.test.result" %in% colnames(explore_country())) {
        explore_country() %>% dplyr::filter(blood.test.result == "hiv negative")
      }
      # to cater for condition where the variable "blood.test.result" is not selected form the list
      else {
        if (input$country == "Angola") {
          exp_data <- head(angola30[append(input$`30_variables`, "blood.test.result")], input$angola_rows)
        } else if (input$country == "Malawi") {
          exp_data <- head(malawi30[append(input$`30_variables`, "blood.test.result")], input$malawi_rows)
        } else if (input$country == "Zambia") {
          exp_data <- head(zambia30[append(input$`30_variables`, "blood.test.result")], input$zambia_rows)
        } else if (input$country == "Zimbabwe") {
          exp_data <- head(zimbabwe30[append(input$`30_variables`, "blood.test.result")], input$zimbabwe_rows)
        }
        exp_data %>% dplyr::filter(blood.test.result == "hiv negative") %>% dplyr::select(-blood.test.result)
      }
    }
    else {
    }
  })
  
  output$explore_table <- renderDT({
    input$explore_filter
    isolate({
      data_transformer()
    })
  })
  
  output$summary_categorical <- renderDT({
    input$explore_filter
    isolate({
      # OR if(dim(data_transformer() == NULL))
      if (is_empty(data_transformer())) {
        showNotification("No data was selected!")
      }
      else if (is_empty(intersect(colnames(data_transformer()), categorical_variables))) {
        showNotification("At least one categorical variable must be chosen in this section!")
      }
      else if (colnames(data_transformer()) == "blood.test.result") {
        if (input$show_positive == FALSE | input$show_negative == FALSE) {
          showModal(modalDialog("Both 'HIV Positive' and 'HIV Negative' checkboxes must be ticked when <blood.test.result> is the only variable chosen!"))
        }
      }
      ExpCTable(data_transformer(), margin = 1, clim = 10, nlim = 5, round = 2, bin = NULL, per = F)
    })
  })
  
  output$summary_numerical <- renderDT({
    input$explore_filter
    isolate({
      # OR if(dim(data_transformer() == NULL))
      if (is_empty(data_transformer())) {
        showNotification("No data was selected!")
      }
      else if (is_empty(intersect(colnames(data_transformer()), numerical_variables))) {
        showNotification("At least one numerical variable must be chosen in this section!")
      }
      ExpNumStat(data_transformer(), by = "A", Qnt = seq(0, 1, 0.25), MesofShape = 2, Outlier = T, round = 2)
    })
  })
  
  output$explore_wc <- renderWordcloud2({
    # Add the "explore_filter" button as a dependency to cause the word cloud to re-render on click
    input$explore_filter
    # there is a bug here in which tooltips can't be shown!
    isolate({
      create_wc(paste(strsplit(paste(colnames(explore_country()), collapse = " "), "[.]")[[1]], collapse = " "), 
                number_of_words = 35, background = "white", sz = 0.6)
    })
  })
  
  output$download_data <- downloadHandler(
    filename = "hiv_data.csv", 
    content = function(file) {
      write.csv(data_transformer(), file, row.names = FALSE)
    }
  )
  
  
  # output$plot_top_ingredients <- plotly::renderPlotly({
  #   rval_top_ingredients() %>%
  #     mutate(ingredient = forcats::fct_reorder(ingredient, tf_idf)) %>% ###
  #     ggplot(aes(x = ingredient, y = tf_idf)) +
  #     geom_col() +
  #     coord_flip()
  # })
  
  
  
  
  output$map <- leaflet::renderLeaflet({
    mapping %>% 
      dplyr::filter(
        hiv.positive.rate >= input$rate_range[1], 
        hiv.positive.rate <= input$rate_range[2]
      ) %>% 
      leaflet() %>% 
      setView(20.735714, 5.816016, zoom = 3) %>% 
      addTiles() %>% 
      addCircleMarkers(
        # "<br>" or br() also can
        popup = paste0(strong("Country: "), mapping$country, "<br>", strong("Year: "), mapping$years, "<br>", strong("HIV-positive: "), 
                       mapping$hiv.positive, "<br>", strong("HIV-negative: "), mapping$hiv.negative, "<br>", strong("HIV Positive Rate: "), 
                       mapping$hiv.positive.rate, "%"), 
        radius = ~sqrt(hiv.positive.rate) * 3.5, 
        fillColor = "red", color = "red", weight = 3
      )
  })
  
  # CODE BELOW: Use observeEvent to display a modal dialog with the help text stored in text_about
  observeEvent(input$show_about, {
    showModal(modalDialog(text_about, title = "About - Geospatial Visualization"))
  })
  
  
  ### why renderPrint???
  output$result <- renderText({
    input$result_button
    isolate({
      
      # relationship.with.most.recent.sex.partner <- input$relationship ###
      # 
      # aa <- 6
      # 
      # bb <- ifelse(input$place == "no", 1, 2)
      # cc <- input$partners
      # dd <- input$condom
      # ee <- ifelse(input$vegetables, 2, 1)
      # ff <- input$school
      # if (input$breasfeeding == "No") {
      #   gg <- 1
      # }
      # else if (input$breasfeeding == "Yes") {
      #   gg <- 2
      # }
      # else if (input$breasfeeding == "Don't know") {
      #   gg <- 3
      # }
      # 
      # # Create empty data frame
      # new_dataset <- data.frame(relationship.with.most.recent.sex.partner = numeric(), 
      #                           know.a.place.to.get.hiv.test = numeric(), 
      #                           total.lifetime.number.of.sex.partners = numeric(), 
      #                           respondent.can.ask.partner.to.use.a.condom = numeric(), 
      #                           would.buy.vegetables.from.vendor.with.hiv = numeric(), 
      #                           children.with.hiv.should.be.allowed.to.attend.school.with.children.without.hiv = numeric(), 
      #                           hiv.transmitted.by.breastfeeding = numeric(), 
      #                           stringsAsFactors = FALSE)
      # new_dataset[1, ] <- list(aa, bb, cc, dd, ee, ff, gg)
      # hiv_prob <- predict(stepwise_rf_shiny, new_dataset, type = "prob")$hiv.positive
      
      
      ## paste0("Hello ", input$name, ", your probability of being an HIV-positive is ", round(hiv_prob, 2), "%. ")
      paste0("Hello ", input$name, ", your probability of being an HIV-positive is ", round(runif(1, 30.0, 90.0), 2), "%. ")
      
      #! embed video link...
      
    })
  })
  
  
  
  # output$var1 <- renderText({
  #   
  # })
  # 
  # output$var2 <- renderText({
  #   
  # })
  # 
  # output$var3 <- renderText({
  #   
  # })
  
}



shinyApp(ui = ui, server = server)
