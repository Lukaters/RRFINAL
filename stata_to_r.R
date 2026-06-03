library(dplyr)
library(tidyr)
library(plm)
library(panelr)
library(marginaleffects)
library(ggplot2)
library(patchwork)
library(stargazer)
library(modelsummary)

df <- read.csv("/Users/lukadecaters/Desktop/R_Project-Reproducible-Research/cleaned_data3.csv")
df <- df[df$WAVE2008 == 1 & df$WAVE2013 == 1 & df$WAVE2018 == 1, ]

df <- df[df$VM01 >= 0 & df$VM01 <= 10 & 
           df$UB04 >= 0 & df$UB04 <= 10 & 
           df$TD01 >= 0 & df$TD01 <= 10, ]

#Variables coding

recode_08 <- function(x) ifelse(x == 1, 1, ifelse(x == 2, 0, NA))
for (i in 1:38) {
  v <- sprintf("VZ02A%02d", i)
  df[[paste0("NHP", i, "_08")]] <- recode_08(df[[v]])
}

for (col in grep("^TP09A_", names(df), value = TRUE)) {
  df[[col]] <- ifelse(df[[col]] %in% c(0, 1), df[[col]], NA)
}

df$NHP_EL2008 <- df$NHP1_08*39.2 + df$NHP12_08*36.80 + df$NHP26_08*24.00
df$NHP_P2008  <- df$NHP2_08*12.91 + df$NHP4_08*19.74 + df$NHP8_08*9.99 + df$NHP19_08*11.22 + df$NHP24_08*8.96 + df$NHP28_08*20.86 + df$NHP36_08*5.83 + df$NHP38_08*10.49
df$NHP_ER2008 <- df$NHP3_08*10.47 + df$NHP6_08*9.31 + df$NHP7_08*7.22 + df$NHP16_08*7.08 + df$NHP20_08*9.76 + df$NHP23_08*13.99 + df$NHP31_08*13.95 + df$NHP32_08*16.21 + df$NHP37_08*12.01
df$NHP_S2008  <- df$NHP5_08*22.37 + df$NHP13_08*12.57 + df$NHP22_08*27.26 + df$NHP29_08*16.10 + df$NHP33_08*21.70
df$NHP_SI2008 <- df$NHP9_08*22.01 + df$NHP15_08*19.36 + df$NHP21_08*20.13 + df$NHP30_08*22.53 + df$NHP34_08*15.97
df$NHP_PA2008 <- df$NHP10_08*11.54 + df$NHP11_08*10.57 + df$NHP14_08*21.30 + df$NHP17_08*10.79 + df$NHP18_08*9.30 + df$NHP25_08*12.61 + df$NHP27_08*11.20 + df$NHP35_08*12.69
df$NHP2008    <- df$NHP_EL2008 + df$NHP_P2008 + df$NHP_ER2008 + df$NHP_S2008 + df$NHP_SI2008 + df$NHP_PA2008

recode_13 <- function(x) ifelse(x == 1, 1, ifelse(x == 0, 0, NA))
for (i in 1:38) {
  v <- sprintf("UR01A_%02d", i)
  df[[paste0("NHP", i, "_13")]] <- recode_13(df[[v]])
}

df$NHP_EL2013 <- df$NHP1_13*39.2 + df$NHP12_13*36.80 + df$NHP26_13*24.00
df$NHP_P2013  <- df$NHP2_13*12.91 + df$NHP4_13*19.74 + df$NHP8_13*9.99 + df$NHP19_13*11.22 + df$NHP24_13*8.96 + df$NHP28_13*20.86 + df$NHP36_13*5.83 + df$NHP38_13*10.49
df$NHP_ER2013 <- df$NHP3_13*10.47 + df$NHP6_13*9.31 + df$NHP7_13*7.22 + df$NHP16_13*7.08 + df$NHP20_13*9.76 + df$NHP23_13*13.99 + df$NHP31_13*13.95 + df$NHP32_13*16.21 + df$NHP37_13*12.01
df$NHP_S2013  <- df$NHP5_13*22.37 + df$NHP13_13*12.57 + df$NHP22_13*27.26 + df$NHP29_13*16.10 + df$NHP33_13*21.70
df$NHP_SI2013 <- df$NHP9_13*22.01 + df$NHP15_13*19.36 + df$NHP21_13*20.13 + df$NHP30_13*22.53 + df$NHP34_13*15.97
df$NHP_PA2013 <- df$NHP10_13*11.54 + df$NHP11_13*10.57 + df$NHP14_13*21.30 + df$NHP17_13*10.79 + df$NHP18_13*9.30 + df$NHP25_13*12.61 + df$NHP27_13*11.20 + df$NHP35_13*12.69
df$NHP2013    <- df$NHP_EL2013 + df$NHP_P2013 + df$NHP_ER2013 + df$NHP_S2013 + df$NHP_SI2013 + df$NHP_PA2013

df$NHP_EL2018 <- df$TP09A_01*39.2 + df$TP09A_12*36.80 + df$TP09A_26*24.00
df$NHP_P2018  <- df$TP09A_02*12.91 + df$TP09A_04*19.74 + df$TP09A_08*9.99 + df$TP09A_19*11.22 + df$TP09A_24*8.96 + df$TP09A_28*20.86 + df$TP09A_36*5.83 + df$TP09A_38*10.49
df$NHP_ER2018 <- df$TP09A_03*10.47 + df$TP09A_06*9.31 + df$TP09A_07*7.22 + df$TP09A_16*7.08 + df$TP09A_20*9.76 + df$TP09A_23*13.99 + df$TP09A_31*13.95 + df$TP09A_32*16.21 + df$TP09A_37*12.01
df$NHP_S2018  <- df$TP09A_05*22.37 + df$TP09A_13*12.57 + df$TP09A_22*27.26 + df$TP09A_29*16.10 + df$TP09A_33*21.70
df$NHP_SI2018 <- df$TP09A_09*22.01 + df$TP09A_15*19.36 + df$TP09A_21*20.13 + df$TP09A_30*22.53 + df$TP09A_34*15.97
df$NHP_PA2018 <- df$TP09A_10*11.54 + df$TP09A_11*10.57 + df$TP09A_14*21.30 + df$TP09A_17*10.79 + df$TP09A_18*9.30 + df$TP09A_25*12.61 + df$TP09A_27*11.20 + df$TP09A_35*12.69
df$NHP2018    <- df$NHP_EL2018 + df$NHP_P2018 + df$NHP_ER2018 + df$NHP_S2018 + df$NHP_SI2018 + df$NHP_PA2018

df$SUBJMOB2008 <- ifelse(df$VW05 %in% 1:5, 6 - df$VW05, NA)
df$SUBJMOB2013 <- ifelse(df$UM05 %in% 1:5, 6 - df$UM05, NA)
df$SUBJMOB2018 <- ifelse(df$TM31 %in% 1:5, 6 - df$TM31, NA)

df <- df %>% rename(JOB2008 = JOB2008_SCALE3, JOB2013 = JOB2013_SCALE3, JOB2018 = JOB2018_SCALE3)

df$SUBJPOSIT2008 <- ifelse(df$VM01 %in% 1:10, 11 - df$VM01, NA)
df$SUBJPOSIT2013 <- ifelse(df$UB04 %in% c(0, 1), 1, df$UB04)
df$SUBJPOSIT2018 <- ifelse(df$TD01 %in% c(0, 1), 1, df$TD01)

df$MARRIED2008 <- ifelse(df$VR01 == 2, 1, ifelse(df$VR01 %in% c(1,3,4,5), 0, NA))
df$MARRIED2013 <- ifelse(df$UK01 == 2, 1, ifelse(df$UK01 %in% c(1,3,4,5), 0, NA))
df$MARRIED2018 <- ifelse(df$TK01 == 2, 1, ifelse(df$TK01 %in% c(1,3,4,5), 0, NA))

df$STDINC2008 <- as.numeric(scale(df$VR22))
df$STDINC2013 <- as.numeric(scale(df$UK21))
df$STDINC2018 <- as.numeric(scale(df$TK21))

df$AGESQ2008 <- df$AGE2008^2
df$AGESQ2013 <- df$AGE2013^2
df$AGESQ2018 <- df$AGE2018^2

df <- df %>% select(
  ANONID, WAVE2008, WAVE2013, WAVE2018, GENDER, YRBIRTH,
  AGE2008, AGE2013, AGE2018, VOIVOD2008, VOIVOD2013, VOIVOD2018,
  SIZE2008, SIZE2013, SIZE2018, EDUC2008, EDUC2013, EDUC2018,
  JOB2008, JOB2013, JOB2018, AGESQ2008, AGESQ2013, AGESQ2018,
  STDINC2008, STDINC2013, STDINC2018, MARRIED2008, MARRIED2013, MARRIED2018,
  SUBJPOSIT2008, SUBJPOSIT2013, SUBJPOSIT2018, SUBJMOB2008, SUBJMOB2013, SUBJMOB2018,
  NHP2008, NHP2013, NHP2018,
  NHP_EL2008, NHP_ER2008, NHP_P2008, NHP_PA2008, NHP_S2008, NHP_SI2008,
  NHP_EL2013, NHP_ER2013, NHP_P2013, NHP_PA2013, NHP_S2013, NHP_SI2013,
  NHP_EL2018, NHP_ER2018, NHP_P2018, NHP_PA2018, NHP_S2018, NHP_SI2018)

# Long panel format 

df_long <- df %>%
  pivot_longer(
    cols = -c(ANONID, GENDER, YRBIRTH),
    names_to = c(".value", "YEAR"),
    names_pattern = "^(.+?)(2008|2013|2018)$"
  ) %>%
  mutate(
    YEAR = as.numeric(YEAR),
    TIME = 1 + (YEAR - 2008) / 5
  )

table(df_long$TIME)

pdata <- pdata.frame(df_long, index = c("ANONID", "TIME"))

#FE all outcomes
fe_el  <- plm(NHP_EL ~ AGE + AGESQ + EDUC + JOB + STDINC + MARRIED + SIZE + SUBJPOSIT + factor(TIME), data = pdata, model = "within")
fe_er  <- plm(NHP_ER ~ AGE + AGESQ + EDUC + JOB + STDINC + MARRIED + SIZE + SUBJPOSIT + factor(TIME), data = pdata, model = "within")
fe_p   <- plm(NHP_P  ~ AGE + AGESQ + EDUC + JOB + STDINC + MARRIED + SIZE + SUBJPOSIT + factor(TIME), data = pdata, model = "within")
fe_pa  <- plm(NHP_PA ~ AGE + AGESQ + EDUC + JOB + STDINC + MARRIED + SIZE + SUBJPOSIT + factor(TIME), data = pdata, model = "within")
fe_s   <- plm(NHP_S  ~ AGE + AGESQ + EDUC + JOB + STDINC + MARRIED + SIZE + SUBJPOSIT + factor(TIME), data = pdata, model = "within")
fe_si  <- plm(NHP_SI ~ AGE + AGESQ + EDUC + JOB + STDINC + MARRIED + SIZE + SUBJPOSIT + factor(TIME), data = pdata, model = "within")
fe_tot <- plm(NHP    ~ AGE + AGESQ + EDUC + JOB + STDINC + MARRIED + SIZE + SUBJPOSIT + factor(TIME), data = pdata, model = "within")
stargazer(fe_el, fe_er, fe_p, fe_pa, fe_s, fe_si, fe_tot, type = "text", column.sep.width = "1pt", omit.stat = c("f", "ser"))

#RE all outcomes
re_el  <- plm(NHP_EL ~ AGE + AGESQ + EDUC + JOB + STDINC + MARRIED + SIZE + SUBJPOSIT + factor(TIME), data = pdata, model = "random")
re_er  <- plm(NHP_ER ~ AGE + AGESQ + EDUC + JOB + STDINC + MARRIED + SIZE + SUBJPOSIT + factor(TIME), data = pdata, model = "random")
re_p   <- plm(NHP_P  ~ AGE + AGESQ + EDUC + JOB + STDINC + MARRIED + SIZE + SUBJPOSIT + factor(TIME), data = pdata, model = "random")
re_pa  <- plm(NHP_PA ~ AGE + AGESQ + EDUC + JOB + STDINC + MARRIED + SIZE + SUBJPOSIT + factor(TIME), data = pdata, model = "random")
re_s   <- plm(NHP_S  ~ AGE + AGESQ + EDUC + JOB + STDINC + MARRIED + SIZE + SUBJPOSIT + factor(TIME), data = pdata, model = "random")
re_si  <- plm(NHP_SI ~ AGE + AGESQ + EDUC + JOB + STDINC + MARRIED + SIZE + SUBJPOSIT + factor(TIME), data = pdata, model = "random")
re_tot <- plm(NHP    ~ AGE + AGESQ + EDUC + JOB + STDINC + MARRIED + SIZE + SUBJPOSIT + factor(TIME), data = pdata, model = "random")
stargazer(re_el, re_er, re_p, re_pa, re_s, re_si, re_tot, type = "text", column.sep.width = "1pt", omit.stat = c("f", "ser"))

#RE + Gender all outcomes
reg_el  <- plm(NHP_EL ~ AGE + AGESQ + EDUC + JOB + STDINC + MARRIED + SIZE + SUBJPOSIT + factor(TIME) + GENDER, data = pdata, model = "random")
reg_er  <- plm(NHP_ER ~ AGE + AGESQ + EDUC + JOB + STDINC + MARRIED + SIZE + SUBJPOSIT + factor(TIME) + GENDER, data = pdata, model = "random")
reg_p   <- plm(NHP_P  ~ AGE + AGESQ + EDUC + JOB + STDINC + MARRIED + SIZE + SUBJPOSIT + factor(TIME) + GENDER, data = pdata, model = "random")
reg_pa  <- plm(NHP_PA ~ AGE + AGESQ + EDUC + JOB + STDINC + MARRIED + SIZE + SUBJPOSIT + factor(TIME) + GENDER, data = pdata, model = "random")
reg_s   <- plm(NHP_S  ~ AGE + AGESQ + EDUC + JOB + STDINC + MARRIED + SIZE + SUBJPOSIT + factor(TIME) + GENDER, data = pdata, model = "random")
reg_si  <- plm(NHP_SI ~ AGE + AGESQ + EDUC + JOB + STDINC + MARRIED + SIZE + SUBJPOSIT + factor(TIME) + GENDER, data = pdata, model = "random")
reg_tot <- plm(NHP    ~ AGE + AGESQ + EDUC + JOB + STDINC + MARRIED + SIZE + SUBJPOSIT + factor(TIME) + GENDER, data = pdata, model = "random")
stargazer(reg_el, reg_er, reg_p, reg_pa, reg_s, reg_si, reg_tot, type = "text", column.sep.width = "1pt", omit.stat = c("f", "ser"))

#FE men
fem_el  <- plm(NHP_EL ~ AGE + AGESQ + EDUC + JOB + STDINC + MARRIED + SIZE + SUBJPOSIT + factor(TIME), data = pdata, subset = (GENDER == 1), model = "within")
fem_er  <- plm(NHP_ER ~ AGE + AGESQ + EDUC + JOB + STDINC + MARRIED + SIZE + SUBJPOSIT + factor(TIME), data = pdata, subset = (GENDER == 1), model = "within")
fem_p   <- plm(NHP_P  ~ AGE + AGESQ + EDUC + JOB + STDINC + MARRIED + SIZE + SUBJPOSIT + factor(TIME), data = pdata, subset = (GENDER == 1), model = "within")
fem_pa  <- plm(NHP_PA ~ AGE + AGESQ + EDUC + JOB + STDINC + MARRIED + SIZE + SUBJPOSIT + factor(TIME), data = pdata, subset = (GENDER == 1), model = "within")
fem_s   <- plm(NHP_S  ~ AGE + AGESQ + EDUC + JOB + STDINC + MARRIED + SIZE + SUBJPOSIT + factor(TIME), data = pdata, subset = (GENDER == 1), model = "within")
fem_si  <- plm(NHP_SI ~ AGE + AGESQ + EDUC + JOB + STDINC + MARRIED + SIZE + SUBJPOSIT + factor(TIME), data = pdata, subset = (GENDER == 1), model = "within")
fem_tot <- plm(NHP    ~ AGE + AGESQ + EDUC + JOB + STDINC + MARRIED + SIZE + SUBJPOSIT + factor(TIME), data = pdata, subset = (GENDER == 1), model = "within")
stargazer(fem_el, fem_er, fem_p, fem_pa, fem_s, fem_si, fem_tot, type = "text", column.sep.width = "1pt", omit.stat = c("f", "ser"))

#FE women
fef_el  <- plm(NHP_EL ~ AGE + AGESQ + EDUC + JOB + STDINC + MARRIED + SIZE + SUBJPOSIT + factor(TIME), data = pdata, subset = (GENDER == 2), model = "within")
fef_er  <- plm(NHP_ER ~ AGE + AGESQ + EDUC + JOB + STDINC + MARRIED + SIZE + SUBJPOSIT + factor(TIME), data = pdata, subset = (GENDER == 2), model = "within")
fef_p   <- plm(NHP_P  ~ AGE + AGESQ + EDUC + JOB + STDINC + MARRIED + SIZE + SUBJPOSIT + factor(TIME), data = pdata, subset = (GENDER == 2), model = "within")
fef_pa  <- plm(NHP_PA ~ AGE + AGESQ + EDUC + JOB + STDINC + MARRIED + SIZE + SUBJPOSIT + factor(TIME), data = pdata, subset = (GENDER == 2), model = "within")
fef_s   <- plm(NHP_S  ~ AGE + AGESQ + EDUC + JOB + STDINC + MARRIED + SIZE + SUBJPOSIT + factor(TIME), data = pdata, subset = (GENDER == 2), model = "within")
fef_si  <- plm(NHP_SI ~ AGE + AGESQ + EDUC + JOB + STDINC + MARRIED + SIZE + SUBJPOSIT + factor(TIME), data = pdata, subset = (GENDER == 2), model = "within")
fef_tot <- plm(NHP    ~ AGE + AGESQ + EDUC + JOB + STDINC + MARRIED + SIZE + SUBJPOSIT + factor(TIME), data = pdata, subset = (GENDER == 2), model = "within")
stargazer(fef_el, fef_er, fef_p, fef_pa, fef_s, fef_si, fef_tot, type = "text", column.sep.width = "1pt", omit.stat = c("f", "ser"))

#RE men
rem_el  <- plm(NHP_EL ~ AGE + AGESQ + EDUC + JOB + STDINC + MARRIED + SIZE + SUBJPOSIT + factor(TIME), data = pdata, subset = (GENDER == 1), model = "random")
rem_er  <- plm(NHP_ER ~ AGE + AGESQ + EDUC + JOB + STDINC + MARRIED + SIZE + SUBJPOSIT + factor(TIME), data = pdata, subset = (GENDER == 1), model = "random")
rem_p   <- plm(NHP_P  ~ AGE + AGESQ + EDUC + JOB + STDINC + MARRIED + SIZE + SUBJPOSIT + factor(TIME), data = pdata, subset = (GENDER == 1), model = "random")
rem_pa  <- plm(NHP_PA ~ AGE + AGESQ + EDUC + JOB + STDINC + MARRIED + SIZE + SUBJPOSIT + factor(TIME), data = pdata, subset = (GENDER == 1), model = "random")
rem_s   <- plm(NHP_S  ~ AGE + AGESQ + EDUC + JOB + STDINC + MARRIED + SIZE + SUBJPOSIT + factor(TIME), data = pdata, subset = (GENDER == 1), model = "random")
rem_si  <- plm(NHP_SI ~ AGE + AGESQ + EDUC + JOB + STDINC + MARRIED + SIZE + SUBJPOSIT + factor(TIME), data = pdata, subset = (GENDER == 1), model = "random")
rem_tot <- plm(NHP    ~ AGE + AGESQ + EDUC + JOB + STDINC + MARRIED + SIZE + SUBJPOSIT + factor(TIME), data = pdata, subset = (GENDER == 1), model = "random")
stargazer(rem_el, rem_er, rem_p, rem_pa, rem_s, rem_si, rem_tot, type = "text", column.sep.width = "1pt", omit.stat = c("f", "ser"))

#RE women
ref_el  <- plm(NHP_EL ~ AGE + AGESQ + EDUC + JOB + STDINC + MARRIED + SIZE + SUBJPOSIT + factor(TIME), data = pdata, subset = (GENDER == 2), model = "random")
ref_er  <- plm(NHP_ER ~ AGE + AGESQ + EDUC + JOB + STDINC + MARRIED + SIZE + SUBJPOSIT + factor(TIME), data = pdata, subset = (GENDER == 2), model = "random")
ref_p   <- plm(NHP_P  ~ AGE + AGESQ + EDUC + JOB + STDINC + MARRIED + SIZE + SUBJPOSIT + factor(TIME), data = pdata, subset = (GENDER == 2), model = "random")
ref_pa  <- plm(NHP_PA ~ AGE + AGESQ + EDUC + JOB + STDINC + MARRIED + SIZE + SUBJPOSIT + factor(TIME), data = pdata, subset = (GENDER == 2), model = "random")
ref_s   <- plm(NHP_S  ~ AGE + AGESQ + EDUC + JOB + STDINC + MARRIED + SIZE + SUBJPOSIT + factor(TIME), data = pdata, subset = (GENDER == 2), model = "random")
ref_si  <- plm(NHP_SI ~ AGE + AGESQ + EDUC + JOB + STDINC + MARRIED + SIZE + SUBJPOSIT + factor(TIME), data = pdata, subset = (GENDER == 2), model = "random")
ref_tot <- plm(NHP    ~ AGE + AGESQ + EDUC + JOB + STDINC + MARRIED + SIZE + SUBJPOSIT + factor(TIME), data = pdata, subset = (GENDER == 2), model = "random")
stargazer(ref_el, ref_er, ref_p, ref_pa, ref_s, ref_si, ref_tot, type = "text", column.sep.width = "1pt", omit.stat = c("f", "ser"))

xreg_re <- plm(NHP_EL ~ AGE + AGESQ + EDUC + JOB + STDINC + MARRIED + SIZE + SUBJPOSIT + factor(TIME), data = pdata, model = "random")
xreg_fe <- plm(NHP_EL ~ AGE + AGESQ + EDUC + JOB + STDINC + MARRIED + SIZE + SUBJPOSIT + factor(TIME), data = pdata, model = "within")
hybrid_model <- wbm(NHP_EL ~ AGE + AGESQ + EDUC + JOB + STDINC + MARRIED + SIZE + SUBJPOSIT + TIME | GENDER,
                    data = df_long, id = "ANONID", wave = "TIME")
summary(hybrid_model)

#Wave dummy
df_long$TIME1 <- ifelse(df_long$TIME == 1, 1, 0)
df_long$TIME2 <- ifelse(df_long$TIME == 2, 1, 0)
df_long$TIME3 <- ifelse(df_long$TIME == 3, 1, 0)

#Gender dummy
df_long$MALE <- ifelse(df_long$GENDER == 2, 0, df_long$GENDER)

#Cut education into different categories
df_long$EDUC_1 <- ifelse(df_long$EDUC >= 1 & df_long$EDUC <= 3, 1, ifelse(df_long$EDUC >= 1 & df_long$EDUC <= 8, 0, NA))
df_long$EDUC_2 <- ifelse(df_long$EDUC >= 4 & df_long$EDUC <= 6, 1, ifelse(df_long$EDUC >= 1 & df_long$EDUC <= 8, 0, NA))
df_long$EDUC_3 <- ifelse(df_long$EDUC >= 7 & df_long$EDUC <= 8, 1, ifelse(df_long$EDUC >= 1 & df_long$EDUC <= 8, 0, NA))

#Cut settlement size into different categories
df_long$SIZE_1 <- ifelse(df_long$SIZE == 1,                          1, ifelse(df_long$SIZE >= 1 & df_long$SIZE <= 5, 0, NA))
df_long$SIZE_2 <- ifelse(df_long$SIZE >= 2 & df_long$SIZE <= 3,      1, ifelse(df_long$SIZE >= 1 & df_long$SIZE <= 5, 0, NA))
df_long$SIZE_3 <- ifelse(df_long$SIZE >= 4 & df_long$SIZE <= 5,      1, ifelse(df_long$SIZE >= 1 & df_long$SIZE <= 5, 0, NA))

#Cut subjectif position into different categories
df_long$SUBJPOSIT_1 <- ifelse(df_long$SUBJPOSIT >= 1 & df_long$SUBJPOSIT <= 2,  1, ifelse(df_long$SUBJPOSIT >= 1 & df_long$SUBJPOSIT <= 10, 0, NA))
df_long$SUBJPOSIT_2 <- ifelse(df_long$SUBJPOSIT >= 3 & df_long$SUBJPOSIT <= 4,  1, ifelse(df_long$SUBJPOSIT >= 1 & df_long$SUBJPOSIT <= 10, 0, NA))
df_long$SUBJPOSIT_3 <- ifelse(df_long$SUBJPOSIT >= 5 & df_long$SUBJPOSIT <= 6,  1, ifelse(df_long$SUBJPOSIT >= 1 & df_long$SUBJPOSIT <= 10, 0, NA))
df_long$SUBJPOSIT_4 <- ifelse(df_long$SUBJPOSIT >= 7 & df_long$SUBJPOSIT <= 8,  1, ifelse(df_long$SUBJPOSIT >= 1 & df_long$SUBJPOSIT <= 10, 0, NA))
df_long$SUBJPOSIT_5 <- ifelse(df_long$SUBJPOSIT >= 9 & df_long$SUBJPOSIT <= 10, 1, ifelse(df_long$SUBJPOSIT >= 1 & df_long$SUBJPOSIT <= 10, 0, NA))

#Cut age into different categories
df_long$AGE_5Y <- as.integer(cut(df_long$AGE,
  breaks = c(20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 95),
  labels = 1:12, right = TRUE))

#Recreate panel data
pdata <- pdata.frame(df_long, index = c("ANONID", "TIME"))

#FE model that uses age divided into categories
fe_curv <- plm(NHP ~ factor(AGE_5Y) + AGESQ + JOB + STDINC + MARRIED + SIZE_2 + SIZE_3 + SUBJPOSIT + TIME2 + TIME3,
               data = pdata, model = "within")
summary(fe_curv)

#Same but RE
re_curv <- plm(NHP ~ factor(AGE) + JOB + STDINC + MARRIED + SIZE_2 + SIZE_3 + SUBJPOSIT + TIME2 + TIME3,
               data = pdata, model = "random")

age_margins <- predictions(re_curv, newdata = datagrid(AGE = as.character(seq(25, 75, by = 5))))
ggplot(age_margins, aes(x = AGE, y = estimate)) +
  geom_line() + geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 1)

#3 models (OLS RE FE)
m1_ols <- lm(NHP ~ AGE + AGESQ + JOB + STDINC + MARRIED + SIZE_2 + SIZE_3 + SUBJPOSIT + TIME2 + TIME3, data = df_long)
m2_re  <- plm(NHP ~ AGE + AGESQ + JOB + STDINC + MARRIED + SIZE_2 + SIZE_3 + SUBJPOSIT + TIME2 + TIME3, data = pdata, model = "random")
m3_fe  <- plm(NHP ~ AGE + AGESQ + JOB + STDINC + MARRIED + SIZE_2 + SIZE_3 + SUBJPOSIT + TIME2 + TIME3, data = pdata, model = "within")

panel_df <- panel_data(df_long, id = ANONID, wave = TIME)

#2 hybrid models
m4_hybrid <- wbm(NHP ~ AGE + AGESQ + JOB + STDINC + MARRIED + SIZE_2 + SIZE_3 + SUBJPOSIT + TIME2 + TIME3, data = panel_df)
m5_hybrid <- wbm(NHP ~ AGE + AGESQ + JOB + STDINC + MARRIED + SIZE_2 + SIZE_3 + SUBJPOSIT + TIME2 + TIME3 | MALE + EDUC_2 + EDUC_3, data = panel_df)

modelsummary(list("OLS" = m1_ols, "RE" = m2_re, "FE" = m3_fe, "Hybrid" = m4_hybrid, "Hybrid+TI" = m5_hybrid),
             statistic = "conf.int", gof_map = c("aic", "bic"))

#Hybrid model with subjposit continuous
hyb_el <- wbm(NHP_EL ~ AGE + AGESQ + JOB + STDINC + MARRIED + SIZE_2 + SIZE_3 + SUBJPOSIT + TIME2 + TIME3 | MALE + EDUC_2 + EDUC_3, data = panel_df)
hyb_er <- wbm(NHP_ER ~ AGE + AGESQ + JOB + STDINC + MARRIED + SIZE_2 + SIZE_3 + SUBJPOSIT + TIME2 + TIME3 | MALE + EDUC_2 + EDUC_3, data = panel_df)
hyb_p  <- wbm(NHP_P  ~ AGE + AGESQ + JOB + STDINC + MARRIED + SIZE_2 + SIZE_3 + SUBJPOSIT + TIME2 + TIME3 | MALE + EDUC_2 + EDUC_3, data = panel_df)
hyb_pa <- wbm(NHP_PA ~ AGE + AGESQ + JOB + STDINC + MARRIED + SIZE_2 + SIZE_3 + SUBJPOSIT + TIME2 + TIME3 | MALE + EDUC_2 + EDUC_3, data = panel_df)
hyb_s  <- wbm(NHP_S  ~ AGE + AGESQ + JOB + STDINC + MARRIED + SIZE_2 + SIZE_3 + SUBJPOSIT + TIME2 + TIME3 | MALE + EDUC_2 + EDUC_3, data = panel_df)
hyb_si <- wbm(NHP_SI ~ AGE + AGESQ + JOB + STDINC + MARRIED + SIZE_2 + SIZE_3 + SUBJPOSIT + TIME2 + TIME3 | MALE + EDUC_2 + EDUC_3, data = panel_df)
modelsummary(list(hyb_el, hyb_er, hyb_p, hyb_pa, hyb_s, hyb_si), statistic = "conf.int")

#Hybrid model with subposit in dummies
hyb_el_d <- wbm(NHP_EL ~ AGE + AGESQ + JOB + STDINC + MARRIED + SIZE_2 + SIZE_3 + SUBJPOSIT_1 + SUBJPOSIT_2 + SUBJPOSIT_4 + SUBJPOSIT_5 + TIME2 + TIME3 | MALE + EDUC_2 + EDUC_3, data = panel_df)
hyb_er_d <- wbm(NHP_ER ~ AGE + AGESQ + JOB + STDINC + MARRIED + SIZE_2 + SIZE_3 + SUBJPOSIT_1 + SUBJPOSIT_2 + SUBJPOSIT_4 + SUBJPOSIT_5 + TIME2 + TIME3 | MALE + EDUC_2 + EDUC_3, data = panel_df)
hyb_p_d  <- wbm(NHP_P  ~ AGE + AGESQ + JOB + STDINC + MARRIED + SIZE_2 + SIZE_3 + SUBJPOSIT_1 + SUBJPOSIT_2 + SUBJPOSIT_4 + SUBJPOSIT_5 + TIME2 + TIME3 | MALE + EDUC_2 + EDUC_3, data = panel_df)
hyb_pa_d <- wbm(NHP_PA ~ AGE + AGESQ + JOB + STDINC + MARRIED + SIZE_2 + SIZE_3 + SUBJPOSIT_1 + SUBJPOSIT_2 + SUBJPOSIT_4 + SUBJPOSIT_5 + TIME2 + TIME3 | MALE + EDUC_2 + EDUC_3, data = panel_df)
hyb_s_d  <- wbm(NHP_S  ~ AGE + AGESQ + JOB + STDINC + MARRIED + SIZE_2 + SIZE_3 + SUBJPOSIT_1 + SUBJPOSIT_2 + SUBJPOSIT_4 + SUBJPOSIT_5 + TIME2 + TIME3 | MALE + EDUC_2 + EDUC_3, data = panel_df)
hyb_si_d <- wbm(NHP_SI ~ AGE + AGESQ + JOB + STDINC + MARRIED + SIZE_2 + SIZE_3 + SUBJPOSIT_1 + SUBJPOSIT_2 + SUBJPOSIT_4 + SUBJPOSIT_5 + TIME2 + TIME3 | MALE + EDUC_2 + EDUC_3, data = panel_df)
modelsummary(list(hyb_el_d, hyb_er_d, hyb_p_d, hyb_pa_d, hyb_s_d, hyb_si_d), statistic = "conf.int")

df_long$SUBJPOSIT_MALE <- df_long$SUBJPOSIT * df_long$MALE
df_long$SUBJPOSIT_AGE  <- df_long$SUBJPOSIT * df_long$AGE
df_long$SUBJPOSIT_EDUC <- df_long$SUBJPOSIT * df_long$EDUC
panel_df <- panel_data(df_long, id = ANONID, wave = TIME)

#Hybrid interaction (Subjposit x male)
hyb_el_m <- wbm(NHP_EL ~ AGE + AGESQ + JOB + STDINC + MARRIED + SIZE_2 + SIZE_3 + SUBJPOSIT + SUBJPOSIT_MALE + TIME2 + TIME3 | MALE + EDUC_2 + EDUC_3, data = panel_df)
hyb_er_m <- wbm(NHP_ER ~ AGE + AGESQ + JOB + STDINC + MARRIED + SIZE_2 + SIZE_3 + SUBJPOSIT + SUBJPOSIT_MALE + TIME2 + TIME3 | MALE + EDUC_2 + EDUC_3, data = panel_df)
hyb_p_m  <- wbm(NHP_P  ~ AGE + AGESQ + JOB + STDINC + MARRIED + SIZE_2 + SIZE_3 + SUBJPOSIT + SUBJPOSIT_MALE + TIME2 + TIME3 | MALE + EDUC_2 + EDUC_3, data = panel_df)
hyb_pa_m <- wbm(NHP_PA ~ AGE + AGESQ + JOB + STDINC + MARRIED + SIZE_2 + SIZE_3 + SUBJPOSIT + SUBJPOSIT_MALE + TIME2 + TIME3 | MALE + EDUC_2 + EDUC_3, data = panel_df)
hyb_s_m  <- wbm(NHP_S  ~ AGE + AGESQ + JOB + STDINC + MARRIED + SIZE_2 + SIZE_3 + SUBJPOSIT + SUBJPOSIT_MALE + TIME2 + TIME3 | MALE + EDUC_2 + EDUC_3, data = panel_df)
hyb_si_m <- wbm(NHP_SI ~ AGE + AGESQ + JOB + STDINC + MARRIED + SIZE_2 + SIZE_3 + SUBJPOSIT + SUBJPOSIT_MALE + TIME2 + TIME3 | MALE + EDUC_2 + EDUC_3, data = panel_df)
modelsummary(list(hyb_el_m, hyb_er_m, hyb_p_m, hyb_pa_m, hyb_s_m, hyb_si_m), statistic = "conf.int")

#Hybrid interaction (Subjposit x age)
hyb_el_a <- wbm(NHP_EL ~ AGE + AGESQ + JOB + STDINC + MARRIED + SIZE_2 + SIZE_3 + SUBJPOSIT + SUBJPOSIT_AGE + TIME2 + TIME3 | MALE + EDUC_2 + EDUC_3, data = panel_df)
hyb_er_a <- wbm(NHP_ER ~ AGE + AGESQ + JOB + STDINC + MARRIED + SIZE_2 + SIZE_3 + SUBJPOSIT + SUBJPOSIT_AGE + TIME2 + TIME3 | MALE + EDUC_2 + EDUC_3, data = panel_df)
hyb_p_a  <- wbm(NHP_P  ~ AGE + AGESQ + JOB + STDINC + MARRIED + SIZE_2 + SIZE_3 + SUBJPOSIT + SUBJPOSIT_AGE + TIME2 + TIME3 | MALE + EDUC_2 + EDUC_3, data = panel_df)
hyb_pa_a <- wbm(NHP_PA ~ AGE + AGESQ + JOB + STDINC + MARRIED + SIZE_2 + SIZE_3 + SUBJPOSIT + SUBJPOSIT_AGE + TIME2 + TIME3 | MALE + EDUC_2 + EDUC_3, data = panel_df)
hyb_s_a  <- wbm(NHP_S  ~ AGE + AGESQ + JOB + STDINC + MARRIED + SIZE_2 + SIZE_3 + SUBJPOSIT + SUBJPOSIT_AGE + TIME2 + TIME3 | MALE + EDUC_2 + EDUC_3, data = panel_df)
hyb_si_a <- wbm(NHP_SI ~ AGE + AGESQ + JOB + STDINC + MARRIED + SIZE_2 + SIZE_3 + SUBJPOSIT + SUBJPOSIT_AGE + TIME2 + TIME3 | MALE + EDUC_2 + EDUC_3, data = panel_df)
modelsummary(list(hyb_el_a, hyb_er_a, hyb_p_a, hyb_pa_a, hyb_s_a, hyb_si_a), statistic = "conf.int")

#Hybrid interaction (Subposit x Educ)
hyb_el_e <- wbm(NHP_EL ~ AGE + AGESQ + JOB + STDINC + MARRIED + SIZE_2 + SIZE_3 + SUBJPOSIT + SUBJPOSIT_EDUC + TIME2 + TIME3 | MALE + EDUC, data = panel_df)
hyb_er_e <- wbm(NHP_ER ~ AGE + AGESQ + JOB + STDINC + MARRIED + SIZE_2 + SIZE_3 + SUBJPOSIT + SUBJPOSIT_EDUC + TIME2 + TIME3 | MALE + EDUC, data = panel_df)
hyb_p_e  <- wbm(NHP_P  ~ AGE + AGESQ + JOB + STDINC + MARRIED + SIZE_2 + SIZE_3 + SUBJPOSIT + SUBJPOSIT_EDUC + TIME2 + TIME3 | MALE + EDUC, data = panel_df)
hyb_pa_e <- wbm(NHP_PA ~ AGE + AGESQ + JOB + STDINC + MARRIED + SIZE_2 + SIZE_3 + SUBJPOSIT + SUBJPOSIT_EDUC + TIME2 + TIME3 | MALE + EDUC, data = panel_df)
hyb_s_e  <- wbm(NHP_S  ~ AGE + AGESQ + JOB + STDINC + MARRIED + SIZE_2 + SIZE_3 + SUBJPOSIT + SUBJPOSIT_EDUC + TIME2 + TIME3 | MALE + EDUC, data = panel_df)
hyb_si_e <- wbm(NHP_SI ~ AGE + AGESQ + JOB + STDINC + MARRIED + SIZE_2 + SIZE_3 + SUBJPOSIT + SUBJPOSIT_EDUC + TIME2 + TIME3 | MALE + EDUC, data = panel_df)
modelsummary(list(hyb_el_e, hyb_er_e, hyb_p_e, hyb_pa_e, hyb_s_e, hyb_si_e), statistic = "conf.int")

df_old    <- subset(df_long, AGE > 60)
pdata_old <- pdata.frame(df_old, index = c("ANONID", "TIME"))
panel_old <- panel_data(df_old, id = ANONID, wave = TIME)

#Models only on 60+ years old
m1_old <- lm(NHP ~ AGE + AGESQ + JOB + STDINC + MARRIED + SIZE_2 + SIZE_3 + SUBJPOSIT + TIME2 + TIME3, data = df_old)
m2_old <- plm(NHP ~ AGE + AGESQ + JOB + STDINC + MARRIED + SIZE_2 + SIZE_3 + SUBJPOSIT + TIME2 + TIME3, data = pdata_old, model = "random")
m3_old <- plm(NHP ~ AGE + AGESQ + JOB + STDINC + MARRIED + SIZE_2 + SIZE_3 + SUBJPOSIT + TIME2 + TIME3, data = pdata_old, model = "within")
m4_old <- wbm(NHP ~ AGE + AGESQ + JOB + STDINC + MARRIED + SIZE_2 + SIZE_3 + SUBJPOSIT + TIME2 + TIME3, data = panel_old)
m5_old <- wbm(NHP ~ AGE + AGESQ + JOB + STDINC + MARRIED + SIZE_2 + SIZE_3 + SUBJPOSIT + TIME2 + TIME3 | MALE + EDUC_2 + EDUC_3, data = panel_old)
modelsummary(list("OLS" = m1_old, "RE" = m2_old, "FE" = m3_old, "Hybrid" = m4_old, "Hybrid+TI" = m5_old))

df_plot <- subset(df_long, TIME >= 1 & TIME <= 3)

#Density plots
p_nhp    <- ggplot(df_plot, aes(x = NHP))    + geom_density()
p_nhp_el <- ggplot(df_plot, aes(x = NHP_EL)) + geom_density()
p_nhp_er <- ggplot(df_plot, aes(x = NHP_ER)) + geom_density()
p_nhp_p  <- ggplot(df_plot, aes(x = NHP_P))  + geom_density()
p_nhp_pa <- ggplot(df_plot, aes(x = NHP_PA)) + geom_density()
p_nhp_s  <- ggplot(df_plot, aes(x = NHP_S))  + geom_density()
p_nhp_si <- ggplot(df_plot, aes(x = NHP_SI)) + geom_density()

(p_nhp | p_nhp_el | p_nhp_er | p_nhp_p) / (p_nhp_pa | p_nhp_s | p_nhp_si)

table(df_long$TIME)

df_long$sample_full <- as.integer(complete.cases(df_long[, c("NHP","AGE","AGESQ","JOB","STDINC","MARRIED",
                                                              "SIZE_2","SIZE_3","SUBJPOSIT","TIME2","TIME3",
                                                              "MALE","EDUC_2","EDUC_3")]))
#Descriptive statistics
table(df_long$TIME[df_long$sample_full == 1])
summary(df_long[df_long$sample_full == 1, c("AGE","AGESQ","JOB","STDINC","MARRIED","SIZE_2","SIZE_3",
                                             "SUBJPOSIT","TIME2","TIME3","MALE","EDUC_2","EDUC_3")])

#Hausman
fixed_h  <- plm(NHP ~ AGE + AGESQ + JOB + STDINC + MARRIED + SIZE_2 + SIZE_3 + SUBJPOSIT + TIME2 + TIME3, data = pdata, model = "within")
random_h <- plm(NHP ~ AGE + AGESQ + JOB + STDINC + MARRIED + SIZE_2 + SIZE_3 + SUBJPOSIT + TIME2 + TIME3, data = pdata, model = "random")
phtest(fixed_h, random_h)


#Suppl

df_long$age_40_90 <- ifelse(df_long$AGE > 40, 1, 0)
df_long$age_45_90 <- ifelse(df_long$AGE > 45, 1, 0)
df_40 <- subset(df_long, age_40_90 == 1)
df_45 <- subset(df_long, age_45_90 == 1)

#Age >40
hyb_nhp_40 <- wbm(NHP    ~ AGE + AGESQ + JOB + STDINC + MARRIED + SIZE_2 + SIZE_3 + SUBJPOSIT + TIME2 + TIME3 | MALE + EDUC_2 + EDUC_3, data = df_40, id = "ANONID", wave = "TIME")
hyb_el_40  <- wbm(NHP_EL ~ AGE + AGESQ + JOB + STDINC + MARRIED + SIZE_2 + SIZE_3 + SUBJPOSIT + TIME2 + TIME3 | MALE + EDUC_2 + EDUC_3, data = df_40, id = "ANONID", wave = "TIME")
hyb_er_40  <- wbm(NHP_ER ~ AGE + AGESQ + JOB + STDINC + MARRIED + SIZE_2 + SIZE_3 + SUBJPOSIT + TIME2 + TIME3 | MALE + EDUC_2 + EDUC_3, data = df_40, id = "ANONID", wave = "TIME")
hyb_p_40   <- wbm(NHP_P  ~ AGE + AGESQ + JOB + STDINC + MARRIED + SIZE_2 + SIZE_3 + SUBJPOSIT + TIME2 + TIME3 | MALE + EDUC_2 + EDUC_3, data = df_40, id = "ANONID", wave = "TIME")
hyb_pa_40  <- wbm(NHP_PA ~ AGE + AGESQ + JOB + STDINC + MARRIED + SIZE_2 + SIZE_3 + SUBJPOSIT + TIME2 + TIME3 | MALE + EDUC_2 + EDUC_3, data = df_40, id = "ANONID", wave = "TIME")
hyb_s_40   <- wbm(NHP_S  ~ AGE + AGESQ + JOB + STDINC + MARRIED + SIZE_2 + SIZE_3 + SUBJPOSIT + TIME2 + TIME3 | MALE + EDUC_2 + EDUC_3, data = df_40, id = "ANONID", wave = "TIME")
hyb_si_40  <- wbm(NHP_SI ~ AGE + AGESQ + JOB + STDINC + MARRIED + SIZE_2 + SIZE_3 + SUBJPOSIT + TIME2 + TIME3 | MALE + EDUC_2 + EDUC_3, data = df_40, id = "ANONID", wave = "TIME")
modelsummary(list(hyb_nhp_40, hyb_el_40, hyb_er_40, hyb_p_40, hyb_pa_40, hyb_s_40, hyb_si_40), statistic = "conf.int")

#Age >45
hyb_nhp_45 <- wbm(NHP    ~ AGE + AGESQ + JOB + STDINC + MARRIED + SIZE_2 + SIZE_3 + SUBJPOSIT + TIME2 + TIME3 | MALE + EDUC_2 + EDUC_3, data = df_45, id = "ANONID", wave = "TIME")
hyb_el_45  <- wbm(NHP_EL ~ AGE + AGESQ + JOB + STDINC + MARRIED + SIZE_2 + SIZE_3 + SUBJPOSIT + TIME2 + TIME3 | MALE + EDUC_2 + EDUC_3, data = df_45, id = "ANONID", wave = "TIME")
hyb_er_45  <- wbm(NHP_ER ~ AGE + AGESQ + JOB + STDINC + MARRIED + SIZE_2 + SIZE_3 + SUBJPOSIT + TIME2 + TIME3 | MALE + EDUC_2 + EDUC_3, data = df_45, id = "ANONID", wave = "TIME")
hyb_p_45   <- wbm(NHP_P  ~ AGE + AGESQ + JOB + STDINC + MARRIED + SIZE_2 + SIZE_3 + SUBJPOSIT + TIME2 + TIME3 | MALE + EDUC_2 + EDUC_3, data = df_45, id = "ANONID", wave = "TIME")
hyb_pa_45  <- wbm(NHP_PA ~ AGE + AGESQ + JOB + STDINC + MARRIED + SIZE_2 + SIZE_3 + SUBJPOSIT + TIME2 + TIME3 | MALE + EDUC_2 + EDUC_3, data = df_45, id = "ANONID", wave = "TIME")
hyb_s_45   <- wbm(NHP_S  ~ AGE + AGESQ + JOB + STDINC + MARRIED + SIZE_2 + SIZE_3 + SUBJPOSIT + TIME2 + TIME3 | MALE + EDUC_2 + EDUC_3, data = df_45, id = "ANONID", wave = "TIME")
hyb_si_45  <- wbm(NHP_SI ~ AGE + AGESQ + JOB + STDINC + MARRIED + SIZE_2 + SIZE_3 + SUBJPOSIT + TIME2 + TIME3 | MALE + EDUC_2 + EDUC_3, data = df_45, id = "ANONID", wave = "TIME")
modelsummary(list(hyb_nhp_45, hyb_el_45, hyb_er_45, hyb_p_45, hyb_pa_45, hyb_s_45, hyb_si_45), statistic = "conf.int")

modelsummary(list("RE" = m2_re, "FE" = m3_fe, "Hybrid" = m4_hybrid, "Hybrid+TI" = m5_hybrid),
             statistic = "conf.int", gof_map = c("aic", "bic"))
