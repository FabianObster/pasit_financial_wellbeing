  library(tidyverse)
 library(extrafont)
 library(openxlsx)
 library(magrittr)
 library(tidyverse)
 library(mboost)
 library(mltools)
 library(pROC)
 library(plotROC)
 library(pROC)
 library(stringi)
 library(questionr)
 library(randomForest)
 library(gbm)
 library(neuralnet)
 library(e1071)
 library(gridExtra)
 library(remotes)

 # helper functions
 myApply <- function(X, FUN, ...) {
   myFun <- function(...) {
     library("mboost") # load mboost on nodes
     FUN(...)
   }
   ## further set up steps as required
   parLapply(cl = cl, X, myFun, ...)
 }

 clean_names <- function(data_to_clean) {
   data_to_clean %>%
     mutate(
       variable = str_replace_all(variable, "S1.8i", "Use of industry information"),
       variable = str_replace_all(variable, "S1.9a", "Trust in newspapers and magazines"),
       variable = str_replace_all(variable, "S1.3a", "Climate change acceptance"),
       variable = str_replace_all(variable, "S1.9f", "Trust in extension experts"),
       variable = str_replace_all(variable, "S8.4a", "Family members dependent on farm"),
       variable = str_replace_all(variable, "reg", "Regions"),
       variable = str_replace_all(variable, "S1.8f", "Use of extension experts"),
       variable = str_replace_all(variable, "S1.9i", "Trust in industry"),
       variable = str_replace_all(variable, "S1.9e", "Trust internet"),
       variable = str_replace_all(variable, "S1.9d", "Trust in radio"),
       variable = str_replace_all(variable, "S1.8c", "Use of television"),
       variable = str_replace_all(variable, "S8.1c", "Gender of farmer"),
       variable = str_replace_all(variable, "S8.1d", "Education of farmer"),
       variable = str_replace_all(variable, "S5.17a", "Trust in government institutions"),
       variable = str_replace_all(variable, "S5.17c", "Trust in religion"),
       variable = str_replace_all(variable, "S5.17d", "Trust in fate"),
       variable = str_replace_all(variable, "S8.6a", "Farm size"),
       variable = str_replace_all(variable, "S8.2balternative", "Prior ownership (family)"),
       variable = str_replace_all(variable, "S8.14", "Farm debt load"),
       variable = str_replace_all(variable, "S8.1b", "Age of farmer"),
       variable = str_replace_all(variable, "S1.8b", "Use of farming journals"),
       variable = str_replace_all(variable, "S3.3", "Percentage of income invested"),
       variable = str_replace_all(variable, "S8.4b", "Family farm engagement"),
       variable = str_replace_all(variable, "A3", "Agronomic measures"),
       variable = str_replace_all(variable, "S8.6e", "Other products"),
       variable = str_replace_all(variable, "F3", "Economic measures"),
       variable = str_replace_all(variable, "S1.3d", "Climate change causes extremes"),
       variable = str_replace_all(variable, "S1.8g", "Use of government experts"),
       variable = str_replace_all(variable, "S1.3b", "Human cause climate change"),
       variable = str_replace_all(variable, "S1.9c", "Trust in television"),
       variable = str_replace_all(variable, "S1.8e", "Use of Internet"),
       variable = str_replace_all(variable, "S1.8a", "Use of newspapers and magazines"),
       variable = str_replace_all(variable, "S5.17b", "Trust in other farmers"),
       variable = str_replace_all(variable, "S8.6c", "More than one variety grown"),
       variable = str_replace_all(variable, "S2.1a1", "Increasing temperature"),
       variable = str_replace_all(variable, "S8.11", "Use of well as water source"),
       variable = str_replace_all(variable, "S2.1d1", "Increasing extreme weather"),
       variable = str_replace_all(variable, "S8.2a2", "Generation of farm ownership"),
       variable = str_replace_all(variable, "S1.8d", "Use of radio"),
       variable = str_replace_all(variable, "S2.1b3", "Decreasing rainfall"),
       variable = str_replace_all(variable, "S1.8j", "Use of farm associations"),
       variable = str_replace_all(variable, "S2.1c1", "Increasing drought"),
       variable = str_replace_all(variable, "S1.9b", "Trust in farming journals"),
       variable = str_replace_all(variable, "S2.2c", "Temperature income damage"),
       variable = str_replace_all(variable, "S2.4c", "Percipitation income damage"),
       variable = str_replace_all(variable, "S2.5c", "Drought income damage"),
       variable = str_replace_all(variable, "S2.6c", "Extreme weather income damage"),
       variable = str_replace_all(variable, "S8.3b", "Years of farm management"),
       variable = str_replace_all(variable, "S8.6b", "Orchard size"),
     ) %>%
     return()
 }

 # Data manipulation and cleaning start

 df_raw <- read.csv("01_Daten/02_Aufbereitet/df_raw.csv") %>%
   mutate(
     "A5" = S5.5a | S5.5c | S5.5d | S5.5e | S5.5m,
     "F5" = S5.5i | S5.5j | S5.5k | S5.5l,
     "T5" = S5.5b | S5.5f | S5.5g | S5.5h,
     "O5" = case_when(is.na(S3.2n) ~ F, T ~ T),
     "A3" = S3.2a | S3.2c | S3.2d | S3.2e | S3.2m,
     "F3" = S3.2i | S3.2j | S3.2k | S3.2l,
     "T3" = S3.2b | S3.2f | S3.2g | S3.2h,
     "O3" = S3.2m,
     S4.7 = as.character(S4.7),
     S7.1alternative = as.character(S7.1alternative),
     S7.1 = as.character(S7.1)
   ) %>%
   mutate(
     "A5" = case_when(S5.5n == "1_A" ~ T, S5.5n == "1_P" ~ T, T ~ A5),
     "T5" = case_when(
       S5.5n == "2_WAT" ~ T, S5.5n == "2_TPS" ~ T, S5.5n == "2_F" ~ T,
       S5.5n == "2_OTH" ~ T, S5.5n == "2_WIND" ~ T, T ~ T5
     ),
     "F5" = case_when(S5.5n == "3_FFR" ~ T, S5.5n == "4_MAN" ~ T, S5.5n == "4_AGR" ~ T, T ~ F5)
   ) %>%
   mutate(
     "A3" = case_when(S3.2n == "1_A" ~ T, S3.2n == "1_P" ~ T, T ~ A3),
     "T3" = case_when(
       S3.2n == "2_WAT" ~ T, S3.2n == "2_IRR" ~ T, S3.2n == "2_TPS" ~ T, S3.2n == "2_F" ~ T,
       S3.2n == "2_OTH" ~ T, S3.2n == "2_WIND" ~ T, T ~ T3
     ),
     "F3" = case_when(S3.2n == "3_FFR" ~ T, S3.2n == "4_MAN" ~ T, S3.2n == "4_AGR" ~ T, T ~ F3)
   ) %>%
   select(-contains("S5.5"), -contains("S3.2")) %>%
   mutate(
     S4.2d = as.numeric(S4.2d),
     S4.2e = as.numeric(S4.2e),
     S4.7 = case_when(
       str_detect(S4.7, "1_") ~ "1",
       str_detect(S4.7, "2_") ~ "2",
       str_detect(S4.7, "3_") ~ "3",
       str_detect(S4.7, "4_") ~ "4",
       str_detect(S4.7, "48") ~ "NA",
       T ~ S4.7
     ),
     S4.3a = case_when(
       str_detect(S4.3a, "1_") ~ "1",
       str_detect(S4.3a, "2_") ~ "2",
       str_detect(S4.3a, "3_") ~ "3",
       str_detect(S4.3a, "4_") ~ "4",
       T ~ NA_character_
     ),
     S4.3b = case_when(
       str_detect(S4.3b, "1_") ~ "1",
       str_detect(S4.3b, "2_") ~ "2",
       str_detect(S4.3b, "3_") ~ "3",
       str_detect(S4.3b, "4_") ~ "4",
       T ~ NA_character_
     ),
     S1.2a = case_when(
       str_detect(S1.2a, "1_") ~ "1",
       str_detect(S1.2a, "2_") ~ "2",
       str_detect(S1.2a, "3_") ~ "3",
       str_detect(S1.2a, "4_") ~ "4",
       T ~ S4.7
     ),
     S1.2b1 = case_when(
       str_detect(S1.2b1, "1_") ~ "1",
       str_detect(S1.2b1, "2_") ~ "2",
       str_detect(S1.2b1, "3_") ~ "3",
       str_detect(S1.2b1, "4_") ~ "4",
       T ~ S4.7
     ),
     S7.1alternative = case_when(
       str_detect(S7.1alternative, "1_") ~ "1",
       str_detect(S7.1alternative, "2_") ~ "2",
       str_detect(S7.1alternative, "3_") ~ "3",
       str_detect(S7.1alternative, "4_") ~ "4",
       str_detect(S7.1alternative, "5_") ~ "5",
       str_detect(S7.1alternative, "6_") ~ "6",
       T ~ S7.1alternative
     ),
     S7.1 = case_when(
       str_detect(S7.1, "1_") ~ "1",
       str_detect(S7.1, "2_") ~ "2",
       str_detect(S7.1, "3_") ~ "3",
       str_detect(S7.1, "4_") ~ "4",
       str_detect(S7.1, "5_") ~ "5",
       str_detect(S7.1, "6_") ~ "6",
       T ~ S7.1
     ),
     S8.2a2 = relevel(as.factor(case_when(
       S8.2a2 >= 3 ~ ">3", S8.2a2 %in% c(1, 2) ~ as.character(S8.2a2),
       T ~ "other"
     )), ref = "other")
   ) %>%
   mutate(
     S5.9 = case_when(S5.9 > 4 ~ T, T ~ F),
     S5.7 = case_when(S5.7 > 4 ~ T, T ~ F),
     S5.8 = case_when(S5.8 > 4 ~ T, T ~ F),
     S5.6p = !is.na(S5.6p)
   ) %>%
   mutate(
     reg = case_when(
       RegionsDetail == 16 ~ "SouthernChile",
       RegionsDetail %in% c(6, 7, 13) ~ "CentralChile",
       RegionsDetail == 1 ~ "CentralTunisia",
       RegionsDetail == 2 ~ "NorthernTunisia"
     ),
     Regions = case_when(
       reg %in% c("NorthernTunisia", "SouthernChile") ~ "cooler",
       reg %in% c("CentralTunisia", "CentralChile") ~ "hotter"
     )
   ) %>%
   mutate_if(is.factor, as.character) %>%
   mutate(reg = factor(reg, levels = c("CentralChile", "SouthernChile", "NorthernTunisia", "CentralTunisia"))) %>%
   mutate(
     S2.1c1 = case_when(str_detect(S2.1calt1, "unpredictable") | str_detect(S2.1calt1, "increased") ~ TRUE, T ~ FALSE),
     S2.1d1 = case_when(str_detect(S2.1dalt1, "unpredictable") | str_detect(S2.1dalt1, "increased") ~ TRUE, T ~ FALSE)
   )

 set.seed(101)
 


 # Time proximity
 df_raw <- df_raw %>%
   mutate_at(
     c("S2.2b09", "S2.2b10", "S2.2b11", "S2.2b12", "S2.2b13", "S2.2b14", "S2.2b15", "S2.2b16", "S2.2b17", "S2.2b18", "S2.2b19"),
     function(x) case_when(is.na(x) ~ 0L, T ~ x)
   ) %>%
   mutate(
     S5.12 = case_when(
       S5.12 %in% c("1", "2") ~ 0,
       S5.12 %in% c("3", "4", "5", "6") ~ 1
     ),
     S2.2b_frequency = S2.2b09 + S2.2b10 + S2.2b11 + S2.2b12 + S2.2b13 + S2.2b14 + S2.2b15 + S2.2b16 + S2.2b17 + S2.2b18 + S2.2b19
   ) %>%
   mutate_at(c("S2.2c", "S2.3c", "S2.4c", "S2.5c", "S2.6c"), function(x) {
     case_when(is.na(x) ~ 0L, T ~ x)
   })
 for (varia in c("S2.2b09", "S2.2b10", "S2.2b11", "S2.2b12", "S2.2b13", "S2.2b14", "S2.2b15", "S2.2b16", "S2.2b17", "S2.2b18", "S2.2b19")) {
   df_raw[[varia]] <- case_when(df_raw[[varia]] == 1 ~ as.numeric(stri_sub(varia, -2, -1)), T ~ 8)
 }
 df_raw <- df_raw %>% mutate(
   S2.2b_proximity = pmax(S2.2b09, S2.2b10, S2.2b11, S2.2b12, S2.2b13, S2.2b14, S2.2b15, S2.2b16, S2.2b17, S2.2b18, S2.2b19),
   S2.2b_frequency = as.numeric(S2.2b_frequency)
 )
 df_raw <- df_raw %>%
   mutate_at(
     c("S2.5b09", "S2.5b10", "S2.5b11", "S2.5b12", "S2.5b13", "S2.5b14", "S2.5b15", "S2.5b16", "S2.5b17", "S2.5b18", "S2.5b19"),
     function(x) case_when(is.na(x) ~ 0L, T ~ x)
   ) %>%
   mutate(S2.5b_frequency = S2.5b09 + S2.5b10 + S2.5b11 + S2.5b12 + S2.5b13 + S2.5b14 + S2.5b15 + S2.5b16 + S2.5b17 + S2.5b18 + S2.5b19)

 for (varia in c("S2.5b09", "S2.5b10", "S2.5b11", "S2.5b12", "S2.5b13", "S2.5b14", "S2.5b15", "S2.5b16", "S2.5b17", "S2.5b18", "S2.5b19")) {
   df_raw[[varia]] <- case_when(df_raw[[varia]] == 1 ~ as.numeric(stri_sub(varia, -2, -1)), T ~ 8)
 }
 df_raw <- df_raw %>% mutate(
   S2.5b_proximity = pmax(S2.5b09, S2.5b10, S2.5b11, S2.5b12, S2.5b13, S2.5b14, S2.5b15, S2.5b16, S2.5b17, S2.5b18, S2.5b19),
   S2.5b_frequency = as.numeric(S2.5b_frequency)
 )

 df_raw <- df_raw %>%
   mutate_at(
     c("S2.6b09", "S2.6b10", "S2.6b11", "S2.6b12", "S2.6b13", "S2.6b14", "S2.6b15", "S2.6b16", "S2.6b17", "S2.6b18", "S2.6b19"),
     function(x) case_when(is.na(x) ~ 0L, T ~ x)
   ) %>%
   mutate(S2.6b_frequency = S2.6b09 + S2.6b10 + S2.6b11 + S2.6b12 + S2.6b13 + S2.6b14 + S2.6b15 + S2.6b16 + S2.6b17 + S2.6b18 + S2.6b19)

 for (varia in c("S2.6b09", "S2.6b10", "S2.6b11", "S2.6b12", "S2.6b13", "S2.6b14", "S2.6b15", "S2.6b16", "S2.6b17", "S2.6b18", "S2.6b19")) {
   df_raw[[varia]] <- case_when(df_raw[[varia]] == 1 ~ as.numeric(stri_sub(varia, -2, -1)), T ~ 8)
 }
 df_raw <- df_raw %>% mutate(
   S2.6b_proximity = pmax(S2.6b09, S2.6b10, S2.6b11, S2.6b12, S2.6b13, S2.6b14, S2.6b15, S2.6b16, S2.6b17, S2.6b18, S2.6b19),
   S2.6b_frequency = as.numeric(S2.6b_frequency)
 )

 df_raw <- df_raw %>%
   mutate_at(
     c("S2.4b09", "S2.4b10", "S2.4b11", "S2.4b12", "S2.4b13", "S2.4b14", "S2.4b15", "S2.4b16", "S2.4b17", "S2.4b18", "S2.4b19"),
     function(x) case_when(is.na(x) ~ 0L, T ~ x)
   ) %>%
   mutate(S2.4b_frequency = S2.4b09 + S2.4b10 + S2.4b11 + S2.4b12 + S2.4b13 + S2.4b14 + S2.4b15 + S2.4b16 + S2.4b17 + S2.4b18 + S2.4b19)

 for (varia in c("S2.4b09", "S2.4b10", "S2.4b11", "S2.4b12", "S2.4b13", "S2.4b14", "S2.4b15", "S2.4b16", "S2.4b17", "S2.4b18", "S2.4b19")) {
   df_raw[[varia]] <- case_when(df_raw[[varia]] == 1 ~ as.numeric(stri_sub(varia, -2, -1)), T ~ 8)
 }
 df_raw <- df_raw %>% dplyr::mutate(
   S2.4b_proximity = pmax(S2.4b09, S2.4b10, S2.4b11, S2.4b12, S2.4b13, S2.4b14, S2.4b15, S2.4b16, S2.4b17, S2.4b18, S2.4b19),
   S2.4b_frequency = as.numeric(S2.4b_frequency)
 )

 vars <- c("Farmer.", "Regions", "RegionsDetail", "Farmer.ID")
 df <- df_raw %>%
   select(-vars) %>%
   select(-contains("S6"), -"S8.1a", -"S8.10b", -"S4.6not") %>%
   select(-contains("S8.12"), "S8.6e", -"S8.2b", -"S2.1aalt1", -"S4.6")

 # change wrong coding
 df$S4.1a[df$S4.1a == "6"] <- 3
 df$S4.1b[df$S4.1b == "6"] <- 3
 df$S4.1c[df$S4.1c == "6"] <- 3
 df$S4.2a[df$S4.2a == "6"] <- 3
 df$S4.2b[df$S4.2b == "6"] <- 3
 df$S4.2d[df$S4.2d == "6"] <- 1
 df$S4.2e[df$S4.2e == "6"] <- 1
 df$S3.1b[df$S3.1b == 0] <- 1
 # df$S2.5c[df$S2.5c == 0] <- 1
 df$S2.8[df$S2.8 == 0] <- 1
 df$S7.1[df$S7.1 == "N/A"] <- NA
 df$S1.2a[df$S1.2a == "N/A"] <- NA
 df[df == "N/A"] <- NA
 df$S8.6c <- !is.na(df$S8.6c2)
 df <- df %>% mutate_if(is.factor, as.character)
 Mode <- function(x) {
   ux <- unique(x)
   ux[which.max(tabulate(match(x, ux)))]
 }
 for (vars in colnames(df)[!(colnames(df) %in% c("F5", "T5", "A5", "S4.1a"))]) {
   if (length(unique(df[[vars]])) < 4) {
     df[[vars]] <- as.character(df[[vars]])
   }
   if (sum(is.na(df[[vars]])) > 80) {
     df[[vars]] <- NULL
   }
   if (is.numeric(df[[vars]])) {
     df[[vars]][is.na(df[[vars]])] <- Mode(df[[vars]])
   }
   if (sum(is.na(df[[vars]])) < 10 & is.character(df[[vars]])) {
     df[[vars]][is.na(df[[vars]])] <- Mode(df[[vars]])
   }
   if (sum(is.na(df[[vars]])) > 10 & is.character(df[[vars]])) {
     df[[vars]][is.na(df[[vars]])] <- "NA"
   }
 }

 df <- df %>%
   mutate(
     S5.12 = case_when(
       S5.12 %in% c("1") ~ "TRUE",
       T ~ "FALSE"
     ),
     S3.3 = case_when(
       S3.3 %in% c("1", "2", "3", "4") ~ "FALSE",
       S3.3 %in% c("5", "6") ~ "TRUE"
     )
   )
 model_df <- df %>%
   mutate_at(
     c("A5", "T5", "F5", "S5.12", "S3.3"),
     function(x) {
       relevel(as.factor(x), ref = "FALSE")
     }
   ) %>%
   mutate(
     S1.3a = S1.3a == "2", S1.3b = S1.3b == "1",
     S1.3c = S1.3c == "2", S1.3d = S1.3d == "1", S1.3e = S1.3e == "1",
     S1.3f = S1.3f == "2",
     S2.7 = relevel(as.factor(S2.7 == "1"), ref = "FALSE"),
     S2.8 = relevel(as.factor(S2.8 == "1"), ref = "FALSE"),
     S3.5 = relevel(as.factor(S3.5), ref = "3"),
     S5.4 = relevel(as.factor(S5.4 == "YES"), ref = "FALSE"),
     S5.15 = relevel(as.factor(S5.15a == 1 | S5.15b == 1 | S5.15c == 5), ref = "FALSE"),
     S5.10 = relevel(as.factor(case_when(S5.10 == "I don’t know" ~ "not_know", T ~ S5.10)), ref = "NO"),
     # S5.9 = relevel(as.factor(S5.8=='TRUE' | S5.9 == 'TRUE' | S5.7 =='TRUE'), ref = 'FALSE'),
     S5.9 = relevel(as.factor(S5.9 == "TRUE"), ref = "FALSE"),
     S5.14 = relevel(as.factor(S5.14aa == 1 | S5.14ab == 1 | S5.14ac == 1 | S5.14ad == 1), ref = "FALSE"),
     S5.13d = relevel(as.factor(S5.13da == 5 | S5.13db == 5 | S5.13dc == 5 | S5.13dd == 5), ref = "FALSE"),
     S5.13 = relevel(as.factor(S5.13a >= 4 | S5.13b <= 2 | S5.13c >= 4), ref = "FALSE"),
     S1.2a = relevel(as.factor(S1.2a %in% c("1", "2")), ref = "FALSE"),
     S2.2b_proximity = relevel(as.factor(S2.2b_proximity > 17), ref = "FALSE"),
     S2.2b_frequency = relevel(as.factor(S2.2b_frequency >= 3), ref = "FALSE"),
     S2.5b_proximity = relevel(as.factor(S2.5b_proximity > 17), ref = "FALSE"),
     S2.5b_frequency = relevel(as.factor(S2.5b_frequency >= 3), ref = "FALSE"),
     S2.6b_proximity = relevel(as.factor(S2.6b_proximity > 17), ref = "FALSE"),
     S2.6b_frequency = relevel(as.factor(S2.6b_frequency >= 3), ref = "FALSE"),
     S2.4b_proximity = relevel(as.factor(S2.4b_proximity > 17), ref = "FALSE"),
     S8.1b = S8.1b > 50,
     S8.6e = (S8.6e != "NO")
   )

 model_df <- lasso_df <- model_df %>%
   mutate(
     S3.1a = (S3.1a == 5), S3.1b = S3.1b > 4, S3.1c = S3.1c > 4,
     S4.1a = case_when(S4.1a %in% c("4", "5") ~ "TRUE", T ~ "FALSE"),
     S4.2a = S4.2a > 4, S4.2b = S4.2b > 4,
     S4.2e = S4.2e >= 4, S4.2d = S4.2d >= 4, S4.1b = S4.1b > 4, S4.1c = S4.1c > 4,
     S4.8 = S4.8 > 4,
     S4.4 = relevel(as.factor(S4.4 > 4), ref = "TRUE"),
     S4.10 = S4.10 > 4, S8.4a = S8.4a > 2,
     S2.2a = S2.2a == "YES", S2.3a = S2.3a == "YES", S2.4a = S2.4a == "YES",
     S2.5a = S2.5a == "YES", S2.6a = S2.6a == "YES", S5.15d = S5.15d > 4,
     S2.2 = relevel(factor(S2.2c >= 4 | S2.3c >= 4 | S2.4c >= 4 | S2.5c >= 4 | S2.6c >= 4), ref = "FALSE")
   )


 model_df <- lasso_df <- model_df %>%
   mutate_at(
     c(
       "S5.11", "S4.9", "S5.17a", "S5.17b", "S5.17c", "S5.17d", "S3.4", "S1.2c", "S4.2c", "S5.14b",
       "S5.1a", "S5.1b"
     ),
     function(x) {
       relevel(as.factor(x %in% c("5")), ref = "FALSE")
     }
   ) %>%
   mutate_at(
     c("S1.1e", "S1.1c", "S1.1a", "S1.1d"),
     function(x) {
       relevel(as.factor(as.character(x > 4)), ref = "FALSE")
     }
   ) %>%
   mutate(S8.1b = relevel(as.factor(S8.1b), ref = "FALSE")) %>%
   mutate(S4.4 = relevel(as.factor(S4.4), ref = "FALSE")) %>%
   mutate_at(
     paste0("S7.2", c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k")),
     function(x) {
       relevel(as.factor(as.character(x >= 6)), ref = "FALSE")
     }
   )

 get_varimp <- function(model) {
   data.frame(varimp(model)) %>%
     mutate(reduction = reduction / sum(reduction)) %>%
     as_tibble() %>%
     filter(reduction != 0) %>%
     arrange(-reduction)
 }

 model_df <- model_df %>%
   mutate(
     AF3 = (A3 == "TRUE" & F3 == "TRUE"),
     AT3 = (A3 == "TRUE" & T3 == "TRUE"),
     TF3 = (T3 == "TRUE" & F3 == "TRUE"),
     ATF3 = (A3 == "TRUE" & F3 == "TRUE" & T3 == "TRUE"),
     AF5 = (A5 == "TRUE" & F5 == "TRUE"),
     AT5 = (A5 == "TRUE" & T5 == "TRUE"),
     TF5 = (T5 == "TRUE" & F5 == "TRUE"),
     ATF5 = (A5 == "TRUE" & F5 == "TRUE" & T5 == "TRUE"),
     S8.10a = (S8.10a == 1),
     S8.1d = case_when(S8.1d <= 2 ~ "1", T ~ "3"),
     S8.3b = S8.3b > 10,
     S8.4b = S8.4b > 2,
     S8.6a = S8.6a > 7,
     S8.6b = case_when(S8.6b <= 2 ~ "FALSE", T ~ "TRUE"),
     S8.7a = S8.7a,
     S8.7b = S8.7b,
     S8.7c = S8.7c,
     S8.14 = S8.14 == 4,
     S8.11 = case_when(S8.11 == "W" ~ "W", S8.11 %in% c("R", "RW") ~ "R", T ~ "rest"),
     S4.5a = S4.5a >= 4,
     S4.5b = S4.5b > 2,
     S8.13_low = S8.13 <= 2,
     S8.13 = S8.13 >= 4,
     S8.2balternative = S8.2balternative == "FAMALL"
   ) %>%
   mutate(
     S2.2a = case_when(S2.2a ~ as.character(S2.2c), T ~ NA_character_),
     S2.3a = case_when(S2.3a ~ as.character(S2.3c), T ~ NA_character_),
     S2.4a = case_when(S2.4a ~ as.character(S2.4c), T ~ NA_character_),
     S2.5a = case_when(S2.5a ~ as.character(S2.5c), T ~ NA_character_),
     S2.6a = case_when(S2.6a ~ as.character(S2.6c), T ~ NA_character_),
     S2.2aa = case_when(!is.na(S2.2a) | !is.na(S2.3a) | !is.na(S2.4a) |
       !is.na(S2.5a) | !is.na(S2.6a) ~ as.character(S2.2), T ~ NA_character_)
   ) %>%
   mutate_at(
     c("S2.2c", "S2.3c", "S2.4c", "S2.5c", "S2.6c"),
     function(x) {
       (x >= 4)
     }
   ) %>%
   mutate(S2.2c = S2.2c | S2.3c) %>%
   mutate_at(
     c("S2.2c", "S2.3c", "S2.4c", "S2.5c", "S2.6c"),
     function(x) {
       relevel(as.factor(x),
         ref = "FALSE"
       )
     }
   )

 measures <- c("A5", "T5", "F5", "S5.12", "S5.9", "S5.8", "S5.4")
 resilience <- c("S8.13", "S8.13_low", "S4.5b")
 vul_now <- c("S2.2c", "S2.4c", "S2.5c", "S2.6c", "S2.2")
 special_vul <- c("S2.2a", "S2.3a", "S2.4a", "S2.5a", "S2.6a", "S2.2aa")
 temperature <- c("S2.1a1", "S2.1b3", "S2.1c1", "S2.1d1")
 vul_future <- c("S4.2d", "S4.2e")
 past_measures <- c("A3", "F3", "S3.4", "T3")
 outcomes <- c(vul_now, resilience, vul_future, measures, past_measures)
 group_outcomes <- c(vul_now, past_measures, measures, resilience, "S3.4", "S3.1b", "S4.1b", "S4.2d", "S4.2e")

 model_df <- model_df %>%
   mutate_at(unique(c(outcomes, group_outcomes)), function(x) {
     relevel(as.factor(x), ref = "FALSE")
   }) %>%
   mutate_at(
     colnames(df)[str_detect(colnames(df), "S1.8|S1.9")],
     function(x) {
       (x >= 4)
     }
   )

 group_df <- data.frame(group = "biophysical", index = 1, col_names = c("A3", "T3", "F3", "S8.11", "S8.6a", "S8.6e", "S8.6c", "S8.6b")) %>%
   rbind(data.frame(group = "region", index = 2, col_names = c("reg"))) %>%
   rbind(data.frame(group = "economic", index = 3, col_names = c(
     "S3.3", "S8.4b", #' S8.6d',
     "S8.4a", "S8.14"
   ))) %>%
   rbind(data.frame(group = "human", index = 4, col_names = c(
     "S8.1b", "S8.1c", "S8.1d", "S8.2a2",
     "S8.2balternative", "S8.3b", "S1.3a", "S1.3b", "S1.3d"
   ))) %>%
   rbind(data.frame(group = "social", index = 5, col_names = c(
     paste0("S1.8", c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j")),
     paste0("S1.9", c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j")),
     "S5.17a", "S5.17b", "S5.17c", "S5.17d"
   ))) %>%
   rbind(data.frame(group = "Experience", index = 6, col_names = c("S2.1a1", "S2.1b3", "S2.1c1", "S2.1d1"))) %>%
   rbind(data.frame(group = "Vulnerability", index = 7, col_names = c("S2.2c", "S2.4c", "S2.5c", "S2.6c"))) %>%
   mutate(col_names = as.character(col_names))

 past_outcome_df <- data.frame(group = "past outcome", index = 101, col_names = c(vul_now, "S3.4"))

 prepare_sgb <- function(alpha = 0.5, local_df = 1, index_df, blearner = "bols",
                         outcome_name = "y") {
   formula_frame <- index_df
   formula_frame$degf <- local_df
   formula_frame <- formula_frame %>%
     group_by(index) %>%
     mutate(degf = degf / sum(degf)) %>%
     ungroup() %>%
     mutate(degf = degf * length(index_df$index) / length(unique(index_df$index))) %>%
     mutate(
       degf = local_df * degf / (max(degf)),
       term = paste0(blearner, "(", sgl_name, ", df = ", degf * alpha, ")")
     )
   formula <- paste0(formula_frame$term, collapse = "+")
   formula_group <- formula_frame %>%
     group_by(index) %>%
     summarize(sgl_name = paste0(sgl_name, collapse = " , "), degf = mean(degf) * (1 - alpha)) %>%
     mutate(term = paste0(blearner, "(", sgl_name, ", df = ", degf, ")"))
   formula_group <- paste0(formula_group$term, collapse = " + ")
   final_formula <- paste0(formula, " + ", formula_group)
   if (alpha == 0) {
     final_formula <- formula_group
   }
   return(final_formula)
 }


 train_index <- sample(1:dim(model_df)[1], dim(model_df)[1] * 0.7)
 eval_df <- data.frame()


model_df <- model_df %>%
  mutate(S8.11w = relevel(factor(case_when(S8.11 == 'W' ~ 'TRUE', T ~ 'FALSE')), ref = 'FALSE'),
         S1.8i = relevel(factor(S1.8i), ref = 'FALSE'),
         S1.9i = relevel(factor(S1.9i), ref = 'FALSE'),
         S1.3a = relevel(factor(S1.3a), ref = 'FALSE'))
outcomes <- c('S8.13','S8.13_low'#, 'S1.8i'
              )

# Data manipulation and cleaning end


for(region in c('Tunisia', 'Chile','')){
  if(region %in% unique(model_df$Country)){vul_df <- model_df %>% filter(Country %in% region)}
  else {vul_df <- model_df}
for(outcome in outcomes){
  index_df <- group_df %>%
    filter(col_names != outcome & col_names != substring(outcome,1,5)) %>%
    mutate(sgl_name = col_names)
  ## Extended data figure 5: statistical model containing only climate variables
  glm_formula <- paste0(outcome,' ~', 'S2.1a1+S2.1b3+S2.1c1+S2.1d1+S2.2c+S2.4c+S2.5c+S2.6c')
  glm(formula = glm_formula, data = vul_df, binomial('logit')) %>%
    odds.ratio() %>% 
    as.data.frame() %>% rownames_to_column() %>%
    write.xlsx(paste0('05_Publikation/model_paper/data/', region, '_odds_',
                      outcome, '.xlsx'))
  
  ## sparse group boosting model start whole dataset
  predictors <- index_df$col_names[index_df$col_names != outcome]
  mb_formula <- paste0(paste0('bols(', predictors, ')'),collapse = '+')
  mb_formula <- as.formula(paste0(outcome, '~', mb_formula))
  sgb0.5_formula <-  prepare_sgb(alpha = 0.5, index_df = index_df, outcome_name = outcome)
  sgb0.5_formula <- paste0(outcome, '~', sgb0.5_formula) %>% as.formula()
  mb_model <- mboost(formula = mb_formula, data = vul_df, family = Binomial(link = 'logit'),
                     control = boost_control(mstop = 4000, nu = 0.3))
  
  sgb0.5_model <- mboost(formula = sgb0.5_formula, data = vul_df, family = Binomial(link = 'logit'),
                         control = boost_control(mstop = 4000, nu = 0.3))
  cl <- makeCluster(8)
  mb_model <- mb_model[mstop(cvrisk(mb_model, papply = myApply))]
  sgb0.5_model <- sgb0.5_model[mstop(cvrisk(sgb0.5_model, papply = myApply))]
  get_varimp(mb_model) %>%
    write.xlsx(paste0('05_Publikation/model_paper/data/',region,'_varimp',
                      outcome, '.xlsx'))
  get_varimp(sgb0.5_model) %>%
    write.xlsx(paste0('05_Publikation/model_paper/data/',region,'_group_varimp',
                      outcome, '.xlsx'))
  sgb0.5_model$coef() %>% lapply(function(x){x[-1]}) %>% bind_rows() %>% 
    summarize_all(function(x){exp(sum(x, na.rm = T))}) %>%
  write.xlsx(paste0('05_Publikation/model_paper/predictions/',region,'_sgb_odds',
                    outcome, '.xlsx'))
  sgb_temp <- sgb0.5_model$coef() %>% lapply(function(x){mean(x[-1])}) %>% as.vector()
  data.frame(blearner = names(sgb_temp), variable = str_replace_all(names(sgb_temp), 'bols\\(|, df.*', '')) %>% 
    mutate(direction = sgb_temp %>% bind_rows() %>% c())  %>% 
    write.xlsx(paste0('05_Publikation/model_paper/predictions/',region,'_sgb_direction',
                      outcome, '.xlsx'))
  ## sparse group boosting model end
  
  # Model training for predictions
  train_index <- sample(1:dim(vul_df)[1], dim(vul_df)[1]*0.7)
  
  ## sgb train
  
  sgb0.5_model_train <- mboost(formula = as.formula(sgb0.5_formula),
                               family = Binomial(link ="logit"), data = vul_df[train_index,],
                               control = boost_control(mstop = 20000, nu = 0.3))
  cv_folds_train <- cv(model.weights(sgb0.5_model_train), type = "kfold", B = 10)
  cv_sgb0.5_train <- cvrisk(sgb0.5_model_train, folds = cv_folds_train, papply = myApply)
  sgb0.5_model_train <- sgb0.5_model_train[mstop(cv_sgb0.5_train)]
  print('sgb')
  print(mstop(sgb0.5_model_train))
  ## mb train
  
  mb_model_train <- mboost(formula = as.formula(mb_formula),
                               family = Binomial(link ="logit"), data = vul_df[train_index,],
                               control = boost_control(mstop = 4000, nu = 0.3))
  cv_mb_model_train <- cvrisk(mb_model_train, folds = cv_folds_train, papply = myApply)
  mb_model_train <- mb_model_train[mstop(cv_mb_model_train)]
  print('mb')
  print(mstop(mb_model_train))
  
  ## glm trin
  glm_train <- glm(formula = glm_formula, data =vul_df[train_index,], binomial('logit'))
  
  ## random forest train
  rf_train <- randomForest(as.formula(paste0(outcome,'~.')), data = vul_df[train_index,] %>% select(index_df$col_names, outcome) %>% mutate_if(is.character,as.factor))
  
  ## gbm train
  gbm_train <- gbm(
    formula = as.formula('outcome_~.'), n.trees = 100, interaction.depth = 3, n.minobsinnode = 1,
    data = vul_df[train_index,] %>% 
      mutate(outcome_ = as.numeric(vul_df[[outcome]][train_index])-1) %>% 
      select(index_df$col_names, outcome_) %>%
      mutate_if(is.character,as.factor) %>% 
      mutate_if(is.logical, as.factor),
    distribution = 'bernoulli'
  )  
  
  ## svm train
  svm_train <- e1071::svm(formula = as.formula(paste0(outcome,'~.')), 
                          data = vul_df[train_index,] %>% 
                            select(index_df$col_names, outcome) %>%
                            mutate_if(is.character,as.factor), 
                           kernel = 'polynomial', degree = 3, probability = TRUE)
  
  ## nn train
  nn_train <- neuralnet(as.formula('outcome_ ==1~.'), hidden = 1,
                        data = vul_df[train_index,] %>% 
                          mutate(outcome_ = as.numeric(vul_df[[outcome]][train_index])-1) %>% 
                          select(index_df$col_names, outcome_) %>%
                          mutate_all(function(x){as.numeric(as.factor(x))}),
                        linear.output = TRUE, act.fct = "logistic"
                        )
  # interactions analysis start
  
  if(region == '' & outcome == 'S8.13'){
    mb_formula_interact <- vector()
    for(var_1 in predictors[predictors != 'S1.3d']){
      for(var_2 in predictors){
        if(var_2 != var_1){
          mb_formula_interact[paste0(var_1,var_2)] <- paste0('bols(', var_1,', by =', var_2, ', df = 1)')
        }
      }
    }
    mb_formula_interact <-
      c(mb_formula_interact, paste0('bols(', predictors, ',df=1)'))
    
    mb_formula_interact <- paste0(mb_formula_interact,collapse = '+')
    mb_formula_interact <- paste0(outcome, '~',mb_formula_interact)
    
    mb_interact_train <- mboost(formula = as.formula(mb_formula_interact),
                                family = Binomial(link ="logit"), data = vul_df[train_index,],
                                control = boost_control(mstop = 4000, nu = 0.1))
    cv_mb_interact_train <- cvrisk(mb_interact_train, folds = cv_folds_train, papply = myApply)
    mb_interact_train <- mb_interact_train[mstop(cv_mb_interact_train)]
    
    mb_interact <- mboost(formula = as.formula(mb_formula_interact),
                          family = Binomial(link ="logit"), data = vul_df,
                          control = boost_control(mstop = 4000, nu = 0.1))
    cv_mb_interact <- cvrisk(mb_interact, papply = myApply)
    mb_interact <- mb_interact[mstop(cv_mb_interact)]
    # interactions only with climate variables
    mb_formula_interact <- vector()
    for(var_1 in predictors){
      for(var_2 in c('S2.1a1', 'S2.1b3', 'S2.1c1', 'S2.1d1', 'S2.2c', 'S2.4c',
                     'S2.5c', 'S2.6c')){
        if(var_2 != var_1){
          mb_formula_interact[paste0(var_1,var_2)] <- paste0('bols(', var_1,', by =', var_2, ', df = 1)')
        }
      }
    }
    mb_formula_interact <-
      c(mb_formula_interact, paste0('bols(', predictors, ',df=1)'))
    
    mb_formula_interact <- paste0(mb_formula_interact,collapse = '+')
    mb_formula_interact <- paste0(outcome, '~',mb_formula_interact)
    
    mb_interact_exp_train <- mboost(formula = as.formula(mb_formula_interact),
                                    family = Binomial(link ="logit"), data = vul_df[train_index,],
                                    control = boost_control(mstop = 4000, nu = 0.1))
    cv_mb_interact_exp_train <- cvrisk(mb_interact_exp_train, folds = cv_folds_train, papply = myApply)
    mb_interact_exp_train <- mb_interact_exp_train[mstop(cv_mb_interact_exp_train)]
    
    mb_interact_exp <- mboost(formula = as.formula(mb_formula_interact),
                              family = Binomial(link ="logit"), data = vul_df,
                              control = boost_control(mstop = 4000, nu = 0.1))
    cv_mb_interact_exp <- cvrisk(mb_interact_exp, papply = myApply)
    mb_interact_exp <- mb_interact_exp[mstop(cv_mb_interact_exp)]
  } else {mb_interact_train <- mb_model_train
  mb_interact_exp_train <- mb_model_train}
  ## interaction analysis end
  
  # prediction:
  svm_pred <-  predict(svm_train, vul_df[-train_index,] %>% select(index_df$col_names, outcome) %>% mutate_if(is.character,as.factor), decision.values = T)
  nn_pred <- predict(nn_train, vul_df[-train_index,] %>% 
                       mutate(outcome_ = as.numeric(vul_df[[outcome]][-train_index])-1) %>% 
                       select(index_df$col_names, outcome_) %>%
                       mutate_all(function(x){as.numeric(as.factor(x))}))
  pred_df <- vul_df[-train_index,] %>%
    transmute(pred_sgb0.5 = predict(sgb0.5_model_train,vul_df[-train_index,], type = 'response'),
           pred_mb = predict(mb_model_train,vul_df[-train_index,], type = 'response'),
           pred_mb_int = predict(mb_interact_train,vul_df[-train_index,], type = 'response'),
           pred_mb_int_exp = predict(mb_interact_exp_train,vul_df[-train_index,], type = 'response'),
           pred_glm = predict(glm_train,vul_df[-train_index,], type = 'response'),
           pred_rf = predict(type = 'prob',rf_train,vul_df[-train_index,] %>% select(index_df$col_names, outcome) %>% mutate_if(is.character,as.factor))[,2],
           pred_svm = attr(svm_pred,which = 'decision.values') %>% as.numeric(),
           pred_nn =  nn_pred,
           pred_gbm = predict(type = 'response',gbm_train,vul_df[-train_index,] %>% 
                                                  mutate(outcome_ = as.numeric(vul_df[[outcome]][-train_index])-1) %>% 
                                                  select(index_df$col_names, outcome_) %>%
                                                  mutate_if(is.character,as.factor) %>% 
                                                  mutate_if(is.logical, as.factor)))
  pred_df$response <- vul_df[-train_index,][[outcome]]
  pred_df$outcome <- outcome
  write.xlsx(pred_df,  paste0('05_Publikation/model_paper/predictions/',region,'_', outcome,'pred_df.xlsx'))
  
  ## Table 1 auc data start
  auc_df <- data.frame(
    auc_sgb0.5 = auc(response = vul_df[-train_index,][[outcome]],
                             predictor = pred_df$pred_sgb0.5),
           pred_mb = auc(response = vul_df[-train_index,][[outcome]],
                         predictor = pred_df$pred_mb),
           pred_mb_int = auc(response = vul_df[-train_index,][[outcome]],
                             predictor = pred_df$pred_mb_int),
           pred_mb_int_exp = auc(response = vul_df[-train_index,][[outcome]],
                             predictor = pred_df$pred_mb_int_exp),
           pred_glm = auc(response = vul_df[-train_index,][[outcome]],
                          predictor = pred_df$pred_glm),
           pred_rf = auc(response = vul_df[-train_index,][[outcome]] ,
                         predictor = pred_df$pred_rf),
           pred_svm = auc(response = vul_df[-train_index,][[outcome]] ,
                          predictor = pred_df$pred_svm),
           pred_nn = auc(response = vul_df[-train_index,][[outcome]] ,
                         predictor = pred_df$pred_nn),
           pred_gbm = auc(response = vul_df[-train_index,][[outcome]] ,
                          predictor = pred_df$pred_gbm),
           outcome = outcome)

  write.xlsx(auc_df,  paste0('05_Publikation/model_paper/data/',region,'_', outcome,'prediction.xlsx'))
  stopCluster(cl)  
  
  ## Table 1 auc data end
  # resynthesis
  important_vars <- get_varimp(mb_model) %>% filter(reduction > 0.025) %>%
    magrittr::extract2('variable') %>% as.character()
  new_vars <- unique(c('A3','F3','T3','S2.1a1','S2.1b3','S2.1c1','S2.1d1',important_vars)) 
  new_formula <- as.formula(paste0(outcome,'~',paste0(new_vars, collapse = '+')))
  
  glm(formula = new_formula, data = vul_df, binomial('logit')) %>%
    odds.ratio() %>% 
    as.data.frame() %>% 
    mutate_all(function(x){round(x,3)}) %>% 
    rownames_to_column() %>%
    write.xlsx(paste0('05_Publikation/model_paper/data/', region, '_new_odds_',
                      outcome, '.xlsx'))
  
}
}
saveRDS(mb_interact, '05_Publikation/model_paper/predictions/mb_interact.R')
temp <- readRDS('05_Publikation/model_paper/predictions/mb_interact.R')
# groups

model_df %>%
  select(index_df$col_names) %>%
  gather('variable', 'category') %>%
  group_by(variable, category) %>% 
  tally() %>%
  left_join(index_df, by = c('variable' = 'col_names')) %>%
  filter(category != 'FALSE') %>%
  select(-index, -sgl_name) %>%
  arrange(group) %>%
  write.xlsx(paste0('05_Publikation/model_paper/data/group_summary.xlsx'))
  

# plots

## Figure 1 start
synthesis_df <- 
  read.xlsx('05_Publikation/model_paper/data/_odds_S8.13.xlsx') %>%
  mutate(Country = 'Chile & Tunisia', wellbeing = 'High wellbeing') %>%
  rbind(read.xlsx('05_Publikation/model_paper/data/Chile_odds_S8.13.xlsx') %>% 
          mutate(Country = 'Chile', wellbeing = 'High wellbeing')) %>% 
  
  rbind(read.xlsx('05_Publikation/model_paper/data/Tunisia_odds_S8.13.xlsx') %>%
          mutate(Country = 'Tunisia', wellbeing = 'High wellbeing')) %>%
  rbind(read.xlsx('05_Publikation/model_paper/data/Chile_odds_S8.13_low.xlsx') %>% 
          mutate(Country = 'Chile', wellbeing = 'Low wellbeing')) %>% 
  rbind(read.xlsx('05_Publikation/model_paper/data/Tunisia_odds_S8.13_low.xlsx') %>%
          mutate(Country = 'Tunisia', wellbeing = 'Low wellbeing')) %>%
  rbind(read.xlsx('05_Publikation/model_paper/data/_odds_S8.13_low.xlsx') %>%
          mutate(Country = 'Chile & Tunisia', wellbeing = 'Low wellbeing')) %>%
  rename('lower' = '2.5.%', upper = '97.5.%') %>%
  filter(rowname != '(Intercept)' & rowname != 'S2.3cTRUE') %>%
  mutate(weather = case_when(str_detect(rowname, 'S2.1a1|S2.2c') ~ 'Increasing temperature', T ~ 'rowname'),
         weather = case_when(str_detect(rowname, 'S2.1b3|S2.4c') ~ 'Decreasing rainfall', T ~ weather),
         weather = case_when(str_detect(rowname, 'S2.1c1|S2.5c') ~ 'Increasing drought', T ~ weather),
         weather = case_when(str_detect(rowname, 'S2.1d1|S2.6c') ~ 'Increasing extreme weather', T ~ weather),
         weather = factor(weather, levels =c('Increasing temperature', 'Decreasing rainfall',
                                             'Increasing drought', 'Increasing extreme weather')),
         group = case_when(str_detect(rowname, 'S2.2c|S2.4c|S2.5c|S2.6c') ~ 'Income damage', T ~ 'Climate change')) %>%
  mutate(OR = log(OR), lower = log(lower), upper = log(upper))

synthesis_df %>%
  ggplot(aes(x = weather, ymin=lower, ymax=upper, y = OR, color = group, fill = group)) +
  geom_errorbar(position=position_dodge(width=0.9), width = 0.6) + 
  geom_point(position=position_dodge(width=0.9), shape=21, size=1.5) +
  geom_hline(yintercept = 0, linetype='dashed', size = 0.1,
             col = 'black') + coord_flip() +
  theme_bw(#base_family = 'Arial',
    base_size = 10) + ylim(c(-2.6,2.6)) + facet_grid(Country ~ wellbeing ) + xlab('') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.text.x.bottom = element_text(angle = 0, vjust = 0.5, hjust=1),
        legend.title = element_blank(), legend.position = 'bottom') +
  scale_fill_manual(values = c('#B2114E', '#003366'))+
  scale_color_manual(values = c('#B2114E', '#003366')) + ylab('Odds ratios (log)')
ggsave('05_Publikation/model_paper/data/synthesis.png', width = 7, height = 5, dpi = 600)
ggsave('05_Publikation/model_paper/data/synthesis.pdf', width = 7, height = 5, dpi = 600)
## Figure 1 end

# varimp interactions
## Figure 3 start
plotdata <- get_varimp(mb_interact) %>%
  filter(reduction >= 0.01) %>% 
  clean_names() %>%
  mutate(variable = as.character(variable)) %>%
  group_by(variable) %>%
  summarize(reduction = sum(reduction)) %>%
  ungroup() 
  
plotdata %>%
  ggplot(aes(x= reorder(variable, reduction), y = reduction)) +
  geom_bar(stat = "identity") + coord_flip()  +
  theme_bw(base_size = 10) + ylab('Relative Importance') + xlab('Interactions') 
ggsave('05_Publikation/model_paper/data/varimp_interaction.pdf', width = 5.5, height = 3.5, dpi = 600)
ggsave('05_Publikation/model_paper/data/varimp_interaction.png', width = 5.5, height = 3.5, dpi = 600)

## Figure 3 end

plotdata %>%
  mutate(reduction = round(reduction,3)) %>%
  write.xlsx('05_Publikation/model_paper/data/varimp_interaction.xlsx')
# varimp int exp

plotdata <- get_varimp(mb_interact_exp) %>%
  filter(reduction >= 0.01) %>% 
  clean_names() %>%
  mutate(variable = as.character(variable)) %>%
  group_by(variable) %>%
  summarize(reduction = sum(reduction)) %>%
  ungroup() 

plotdata %>%
  ggplot(aes(x= reorder(variable, reduction), y = reduction)) +
  geom_bar(stat = "identity") + coord_flip()  +
  theme_bw(base_size = 11) + ylab('Relative Importance') + xlab('Interactions') +
  theme(axis.text=element_text(size=12)) 
ggsave('05_Publikation/model_paper/data/varimp_interaction_exp.pdf', width = 7, height = 5, dpi = 600)
ggsave('05_Publikation/model_paper/data/varimp_interaction_exp.png', width = 7, height = 5, dpi = 600)
plotdata %>%
  mutate(reduction = round(reduction,3)) %>%
  write.xlsx('05_Publikation/model_paper/data/varimp_interaction_exp.xlsx')



# auc curve
## Extended data figure 4 start
all_pred <- read.xlsx('05_Publikation/model_paper/predictions/Chile_S8.13pred_df.xlsx') %>%
  bind_rows(read.xlsx('05_Publikation/model_paper/predictions/Chile_S8.13_lowpred_df.xlsx')) %>%
  mutate(Country = 'Chile') %>%
    bind_rows(
      read.xlsx('05_Publikation/model_paper/predictions/Tunisia_S8.13pred_df.xlsx') %>%
    bind_rows(read.xlsx('05_Publikation/model_paper/predictions/Tunisia_S8.13_lowpred_df.xlsx')) %>%
    mutate(Country = 'Tunisia')
  ) %>%
  bind_rows(
    read.xlsx('05_Publikation/model_paper/predictions/_S8.13pred_df.xlsx') %>%
      bind_rows(read.xlsx('05_Publikation/model_paper/predictions/_S8.13_lowpred_df.xlsx')) %>%
      mutate(Country = 'Chile & Tunisia')
  ) %>%
  mutate(wellbeing = case_when(outcome == 'S8.13' ~ 'High wellbeing', T ~ 'Low wellbeing'))
all_pred <- all_pred %>%
  mutate_at(colnames(all_pred[1:9]), as.numeric) 

all_pred %>%
  group_by(Country, wellbeing) %>%
  summarize_at(colnames(all_pred[c(1:7,9)]),
               list(~round(mean(.>0.5 & response | (.<0.5 & !response)), 3) )) %>%
  write.xlsx('05_Publikation/model_paper/data/accuracy.xlsx')
all_pred %>%
  group_by(Country, wellbeing) %>%
  summarize_at(colnames(all_pred[8]),
               list(~round(mean(.>1 & response | (.<1 & !response)), 3) )) 
# all_pred %>%
#   group_by(Country, wellbeing, response) %>%
#   summarize_at(colnames(all_pred[1:9]),
#                list(~mean(.>0.5 & response | (.<0.5 & !response)))) %>%
#   write.xlsx('05_Publikation/model_paper/data/accuracy.xlsx')

# #pred_df %>%
#   mutate(pred_svm = -pred_svm,pred_nn = 1-as.numeric(pred_nn)) %>%
#   gather(value = pred, key = 'model', -response, -Country,-wellbeing) %>%
#   mutate(model = str_replace_all(model, 'pred_',''),
#          model = str_replace_all(model, '_',' '),
#          model = str_replace_all(model, '0.5', '')) %>%
#   filter(model %in% c('glm', 'sgb','mb int', 'rf', 'gbm', 'nn')) %>%
#   mutate(model = factor(model, levels = c('glm', 'sgb', 'mb int', 'rf', 'gbm', 'nn'))) %>% View()
#   ggplot(aes(m = pred, d = response, color = model)) +
#   geom_roc(labels = F, n.cuts = 0) + theme_bw(base_family = 'Arial',base_size = 18) + ylab("True positive fraction") + 
#   xlab('False positive fraction') + scale_color_brewer(palette =  'Dark2') +
#   geom_abline(slope = 1, intercept = 0, linetype='dotted') +
#   theme(legend.title=element_blank())
# ggsave('05_Publikation/model_paper/data/auc_curve.pdf', width = 7, height = 5, dpi = 600)

all_pred %>%
  mutate(pred_svm = 1-pred_svm,pred_nn = 1-as.numeric(pred_nn)) %>%
  gather(value = pred, key = 'model', -response, -Country,-wellbeing) %>%
  mutate(model = str_replace_all(model, 'pred_',''),
         model = str_replace_all(model, '_',' '),
         model = str_replace_all(model, '0.5', '')) %>%
  filter(model %in% c('glm', 'sgb','mb', 'rf', 'gbm', 'nn')) %>%
  mutate(model = factor(model, levels = c('glm', 'sgb', 'mb', 'rf', 'gbm', 'nn')),
         pred = as.numeric(pred)) %>%
  ggplot(aes(m = pred, d = response, color = model)) +
  geom_roc(labels = F, n.cuts = 0, size = 0.5) + theme_bw(base_size = 10) + ylab("True positive fraction") +
  xlab('False positive fraction') + scale_color_brewer(palette =  'Dark2') +
  geom_abline(slope = 1, intercept = 0, linetype='dotted') + facet_grid(Country~wellbeing) +
  theme(legend.title=element_blank())
ggsave('05_Publikation/model_paper/data/auc_curve_all.pdf', width = 5.5, height = 3.5, dpi = 600)
ggsave('05_Publikation/model_paper/data/auc_curve_all.png', width = 5.5, height = 3.5, dpi = 600)

## extended data figure 4 end

# group varimp

## Figure 2 start
direction_df <- read.xlsx('05_Publikation/model_paper/predictions/Chile_sgb_directionS8.13.xlsx') %>% 
  mutate(Country = 'Chile', wellbeing = 'High wellbeing') %>%
  bind_rows(
    read.xlsx('05_Publikation/model_paper/predictions/Tunisia_sgb_directionS8.13.xlsx') %>%
      mutate(Country = 'Tunisia', wellbeing = 'High wellbeing')
  ) %>% 
  bind_rows(
    read.xlsx('05_Publikation/model_paper/predictions/Chile_sgb_directionS8.13_low.xlsx') %>%
      mutate(Country = 'Chile', wellbeing = 'Low wellbeing')
  ) %>%
  bind_rows(
    read.xlsx('05_Publikation/model_paper/predictions/Tunisia_sgb_directionS8.13_low.xlsx') %>%
      mutate(Country = 'Tunisia', wellbeing = 'Low wellbeing')) %>% 
  select(-variable) %>%
  mutate(direction = as.numeric(direction))
  
group_plot <- read.xlsx('05_Publikation/model_paper/data/_group_varimpS8.13.xlsx') %>% 
  mutate(Country = 'All') %>%
  bind_rows(
    read.xlsx('05_Publikation/model_paper/data/Chile_group_varimpS8.13.xlsx') %>%
      mutate(Country = 'Chile', wellbeing = 'High wellbeing')
  ) %>%
  bind_rows(
    read.xlsx('05_Publikation/model_paper/data/Tunisia_group_varimpS8.13.xlsx') %>%
      mutate(Country = 'Tunisia', wellbeing = 'High wellbeing')
  ) %>% 
  bind_rows(
    read.xlsx('05_Publikation/model_paper/data/Chile_group_varimpS8.13_low.xlsx') %>%
      mutate(Country = 'Chile', wellbeing = 'Low wellbeing')
  ) %>%
  bind_rows(
    read.xlsx('05_Publikation/model_paper/data/Tunisia_group_varimpS8.13_low.xlsx') %>%
      mutate(Country = 'Tunisia', wellbeing = 'Low wellbeing')
  ) %>%
  filter(Country != 'All') %>% left_join(direction_df, by = c('blearner', 'Country', 'wellbeing')) %>%
  mutate(variable = case_when(str_detect(variable, 'S1.8a, S1.8b') ~ 'Social asset group',
                              str_detect(variable, 'S1.3a, S1.3b') ~ 'Human asset group',
                              str_detect(variable, 'A3, F3') ~ 'Biophysical asset group',
                              str_detect(variable, 'S2.1a1, S') ~ 'Climate experience group',
                              str_detect(variable, 'S2.2c, S2.4c') ~ 'Income damage group',
                              str_detect(variable, 'S3.3, S8.14') ~ 'Economic asset group',
                              T ~ variable),
         ) %>%
  clean_names() %>%
  group_by(variable) %>%
  mutate(order = sum(reduction)) %>%
  filter(reduction>0.01) %>%
  ungroup() %>%
  arrange(order) %>%
  group_by(variable, wellbeing) %>%
  mutate(endpoint = sum(reduction), direction = sum(direction)) %>%
  ungroup() %>%
  mutate(direction  = case_when(variable == 'Use of well as water source' | 
                                  variable == 'Years of farm management' ~ -direction, T ~ direction))
group_plot %>%
  ggplot(aes(x= reorder(variable, order), y = reduction, fill = Country)) +
            geom_bar(position = position_stack(reverse = TRUE), stat = "identity") + coord_flip() +
            scale_fill_manual(values = c("#9d43a5", "#87c9fb")) +
            theme_bw(base_size=10) + theme(legend.title = element_blank()) +
            xlab('Variable') + ylab('Relative Importance') + facet_grid(.~wellbeing) +
  geom_text(data = group_plot %>% filter(direction >= 0, !(variable %in% c('Regions', 'Human asset group'))), 
            aes(x= reorder(variable, order), y = endpoint, 
                label = sprintf('\u2191')), size = 4, position = position_identity(), hjust=-0.1, vjust=0.3) +
  geom_text(data = group_plot %>% filter(direction < 0, !(variable %in% c('Regions', 'Human asset group'))), 
            aes(x= reorder(variable, order), y = endpoint, 
                label = sprintf('\u2193')), size = 4, position = position_identity(), hjust=-0.1, vjust=0.3)
ggsave('05_Publikation/model_paper/data/country_varim.pdf', width = 5.5, height = 3.5, dpi = 600)
ggsave('05_Publikation/model_paper/data/country_varim.png', width = 5.5, height = 3.5, dpi = 600)
## Figure 2 end

model_df <-
  model_df %>%
  mutate(S8.1d = relevel(factor(case_when(S8.1d == '3' ~ 'TRUE', T~ 'FALSE')), ref = 'FALSE'))
# 3 way comparison
## helper function for figures
three_fun <- function(x_name, x_var, group_name, group_var, high_cat_name_group ='Yes',
                      low_cat_name_group = 'No', high_cat_name_x ='Yes',
                      low_cat_name_x = 'No'){
  three_model <- glm(formula = paste0('S8.13~', x_var,'*', group_var,'*Country'), data = model_df, binomial('logit'))
  three_model_all <- glm(formula = paste0('S8.13~', x_var,'*', group_var), data = model_df, binomial('logit'))
  three <- model_df %>%
    mutate('Well_being' = predict(three_model, type = 'response')) %>%
    rbind(
      model_df %>% 
        mutate('Well_being' = predict(three_model_all, type = 'response'),
               Country = 'Chile & Tunisia')
    ) %>%
    mutate(x_axis = case_when(get(x_var) == 'TRUE' ~ high_cat_name_x, T ~low_cat_name_x),
           group_axis = case_when(get(group_var) == 'TRUE' ~ high_cat_name_group, T ~low_cat_name_group),
    )
  three_model_all %>%
    odds.ratio() %>% 
    as.data.frame() %>% rownames_to_column() %>%
    mutate(region = 'All') %>%
    rbind(
      glm(formula = paste0('S8.13~', x_var,'*', group_var), 
          data = model_df %>% filter(Country == 'Tunisia'), binomial('logit')) %>%
        odds.ratio() %>% 
        as.data.frame() %>% rownames_to_column() %>%
        mutate(region = 'Tunisia')
    ) %>%
    rbind(
      glm(formula = paste0('S8.13~', x_var,'*', group_var), 
          data = model_df %>% filter(Country == 'Chile'), binomial('logit')) %>%
        odds.ratio() %>% 
        as.data.frame() %>% rownames_to_column() %>%
        mutate(region = 'Chile')
    ) %>%
    write.xlsx(paste0('05_Publikation/model_paper/data/three_',x_var,'_', group_var,'.xlsx'))

  ret <- three %>%
    ggplot(aes_string(y = 'Well_being', color = 'group_axis', x = 'x_axis', group = 'group_axis')) +
    geom_point(size = 3) + geom_line(aes_string(x = 'x_axis')) +
    theme_bw(base_size = 14) + facet_wrap(.~Country) +
    scale_color_manual(values = c("#000000", "#CC79A7")) +
    guides(color=guide_legend(group_name)) +
    ylab('Probability high wellbeing') + xlab(x_name) + theme(legend.position = 'top')
  ggsave(paste0('05_Publikation/model_paper/data/three_',x_var,'_', group_var,'.pdf'), width = 7, height = 5, dpi = 600)
  ggsave(paste0('05_Publikation/model_paper/data/three_',x_var,'_', group_var,'.png'), width = 7, height = 5, dpi = 600)
  return(ret)
}


# 3 way comparisons
## Figure 4
top_left <- three_fun(x_name = 'Increases in temperature', x_var = 'S2.1a1', group_name = 'Newspaper sources',
          group_var = 'S1.8a') + ylim(c(0, 1))
top_right <- three_fun(x_name = 'Increases in temperature', x_var = 'S2.1a1', group_name = 'Trust in industry',
                       group_var = 'S1.9i') + ylab('') + ylim(c(0, 1))
bottom_left <- three_fun(x_name = 'Reduction in percipitation', x_var = 'S2.1b3', group_name = 'Newspaper sources',
                      group_var = 'S1.8a') + ylim(c(0, 1))
bottom_right <- three_fun(x_name = 'Reduction in percipitation', x_var = 'S2.1b3', group_name = 'Trust in industry',
                       group_var = 'S1.9i') + ylab('') + ylim(c(0, 1))

three_temp <- grid.arrange(top_left, top_right, bottom_left, bottom_right, layout_matrix = rbind(c(1, 2),
                                                        c(3,4)))
ggsave('05_Publikation/model_paper/data/three_temp_percip.pdf', three_temp, width = 9, height = 9, dpi = 600)
ggsave('05_Publikation/model_paper/data/three_temp_percip.png', three_temp, width = 9, height = 9, dpi = 600)


## Figure 5
top_left <- three_fun(x_name = 'Use of industry information', x_var = 'S1.8i', group_name = 'Trust in media',
          group_var = 'S1.9a') + ylim(c(0, 1))
top_right <- three_fun(x_name = 'Trust in experts', x_var = 'S1.9f', group_name = 'Trust in industry',
                       group_var = 'S1.9i') + ylab('') + ylim(c(0, 1))
bottom_left <- three_fun(x_name = 'Trust in industry', x_var = 'S1.9i', group_name = 'Trust in government',
                                        group_var = 'S5.17a') + ylim(c(0, 1))
bottom_right <- three_fun(x_name = 'Use of media', x_var = 'S1.8c', group_name = 'Education',
                          group_var = 'S8.1d') + ylab('') + ylim(c(0, 1))

three_other <- grid.arrange(top_left, top_right, bottom_left, bottom_right, layout_matrix = rbind(c(1, 2),
                                                                                                 c(3,4)))
ggsave('05_Publikation/model_paper/data/three_other.pdf', three_other, width = 9, height = 9, dpi = 600)
ggsave('05_Publikation/model_paper/data/three_other.png', three_other, width = 9, height = 9, dpi = 600)

