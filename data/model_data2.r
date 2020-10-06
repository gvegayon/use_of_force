library(data.table)

dat <- fread("data-raw/njdatageorge200918.csv")

dat[, date := as.IDate(date, format = "%m/%d/%Y")]

# Looking at the race variable -------------------------------------------------
sdcols <- sprintf("Subject_%i_race", 1:3)
dat[, c(sdcols) := lapply(.SD, tolower), .SDcols = sdcols]

# How many?
sort(dat[, table(
  c(Subject_1_race, Subject_2_race, Subject_3_race),
  useNA = "always"
  )], decreasing = TRUE)

# Changing to NAs
dat[, c(sdcols) := lapply(.SD, function(x) fifelse(
  test = grepl("white|asian|black|hispan", x),
  yes  = x,
  no   = NA_character_)), .SDcols = sdcols
  ]

dat[, c(sdcols) := lapply(.SD, function(x) fifelse(
  test = grepl("asian", x),
  yes  = "asian",
  no   = x)), .SDcols = sdcols
]

# How many?
sort(dat[, table(
  c(Subject_1_race, Subject_2_race, Subject_3_race),
  useNA = "always"
)], decreasing = TRUE)

# Do the reported Races match the reported number of subjects?
dat[, n_subject_races :=
      as.integer(!is.na(Subject_1_race)) +
      as.integer(!is.na(Subject_2_race)) +
      as.integer(!is.na(Subject_3_race))]
prop.table(dat[, table(nsubjects != n_subject_races)])

# Creating number of subjects
dat[, n_white :=
      fifelse(is.na(Subject_1_race), 0, as.integer(Subject_1_race == "white")) +
      fifelse(is.na(Subject_2_race), 0, as.integer(Subject_2_race == "white")) +
      fifelse(is.na(Subject_3_race), 0, as.integer(Subject_3_race == "white"))
    ]
dat[, n_black :=
      fifelse(is.na(Subject_1_race), 0, as.integer(Subject_1_race == "black")) +
      fifelse(is.na(Subject_2_race), 0, as.integer(Subject_2_race == "black")) +
      fifelse(is.na(Subject_3_race), 0, as.integer(Subject_3_race == "black"))
]
dat[, n_asian :=
      fifelse(is.na(Subject_1_race), 0, as.integer(Subject_1_race == "asian")) +
      fifelse(is.na(Subject_2_race), 0, as.integer(Subject_2_race == "asian")) +
      fifelse(is.na(Subject_3_race), 0, as.integer(Subject_3_race == "asian"))
]
dat[, n_hispanic :=
      fifelse(is.na(Subject_1_race), 0, as.integer(Subject_1_race == "hispanic")) +
      fifelse(is.na(Subject_2_race), 0, as.integer(Subject_2_race == "hispanic")) +
      fifelse(is.na(Subject_3_race), 0, as.integer(Subject_3_race == "hispanic"))
]

# Setting as NA if they don't match
dat[, n_white := ifelse(n_subject_races != nsubjects, NA_integer_, n_white)]
dat[, n_black := ifelse(n_subject_races != nsubjects, NA_integer_, n_black)]
dat[, n_hispanic := ifelse(n_subject_races != nsubjects, NA_integer_, n_hispanic)]
dat[, n_asian := ifelse(n_subject_races != nsubjects, NA_integer_, n_asian)]

# Looking at consistency within reports
dat[, consistency_white := diff(range(n_white)), by = caseid]
dat[, consistency_black := diff(range(n_black)), by = caseid]
dat[, consistency_hispanic := diff(range(n_hispanic)), by = caseid]
dat[, consistency_asian := diff(range(n_asian)), by = caseid]

dat[, consistency_race := 
    consistency_white +
    consistency_black +
    consistency_hispanic +
    consistency_asian
    ]

prop.table(dat[, table(consistency_race)])
# consistency_race About 96 percent of the data has consistent reports
#            0            1            2            3            4 
# 0.9618575995 0.0172742179 0.0186488680 0.0016230809 0.0005962338 

# Sex of the subjects ----------------------------------------------------------
sdcols <- sprintf("Subject_%i_sex", 1:3)
dat[, c(sdcols) := lapply(.SD, tolower), .SDcols = sdcols]

dat[, table(c(Subject_1_sex, Subject_2_sex, Subject_3_sex))]

# There are too many cases in which the race is set as "Not Listed"
# so we cannot use it
#             Var1  Freq
# 1          blank     2
# 2         female 13106
# 3      illegible     1
# 4            mae     1
# 5           make     1
# 6           male 56966
# 7  male & female     1
# 8    male/female     2
# 9     not listed 79934
# 10      redacted   226
# 11           unk     2

# Looking at cases in which there's a single individual
dat[nsubjects == 1, table(c(Subject_2_sex, Subject_3_sex))]
# 69984 cases with one individual have Not Listed in sex2 and 3
# so it seems that it has to do with a default value in the form
dat[nsubjects==1, Subject_2_sex := NA_character_]
dat[nsubjects==1, Subject_3_sex := NA_character_]

dat[nsubjects==2, Subject_3_sex := NA_character_]

# Tabulating again
dat[, table(c(Subject_1_sex, Subject_2_sex, Subject_3_sex))]

# Creating a count variable
sdcols <- sprintf("Subject_%d_sex", 1:3)
dat[, c(sdcols) := lapply(.SD, function(d) {
  fifelse(grepl("^(male|mae|make)$", d), "male",
          fifelse(grepl("^female$", d), d, NA_character_))
}), .SDcols = sdcols]

dat[, table(c(Subject_1_sex, Subject_2_sex, Subject_3_sex), useNA = "always")]

dat[, n_male :=
      fifelse(is.na(Subject_1_sex), 0, as.integer(Subject_1_sex == "male")) +
      fifelse(is.na(Subject_2_sex), 0, as.integer(Subject_2_sex == "male")) +
      fifelse(is.na(Subject_3_sex), 0, as.integer(Subject_3_sex == "male"))
]
dat[, n_female :=
      fifelse(is.na(Subject_1_sex), 0, as.integer(Subject_1_sex == "female")) +
      fifelse(is.na(Subject_2_sex), 0, as.integer(Subject_2_sex == "female")) +
      fifelse(is.na(Subject_3_sex), 0, as.integer(Subject_3_sex == "female"))
]

dat[, consistency_male := diff(range(n_male)), by = caseid]
dat[, consistency_female := diff(range(n_female)), by = caseid]

dat[, table(consistency_male)]
dat[, table(consistency_female)]

# Gender variable --------------------------------------------------------------
dat[, table(is.na(officer_male))] # 57282 NAs

dat[, officer_gender_range := diff(range(officer_male, na.rm = TRUE)), by = officerid_num]
dat[, table(is.na(officer_male), officer_gender_range)] # No case that can be fixed of NAs
dat[, officer_gender_range := NULL]

# Officer Race -----------------------------------------------------------------
dat[, table(is.na(officer_race))]
dat[, tmp := as.integer(factor(officer_race))]
dat[, tmp := diff(range(tmp, na.rm = TRUE)), by = officerid_num]
dat[, table(is.na(officer_race), is.finite(tmp))] # No place to comple missings
dat[, tmp := NULL]

# Years of experience ----------------------------------------------------------
dat[, years_range := diff(range(officer_nyears, na.rm = TRUE)), by = officerid_num]
dat[, table(is.na(officer_nyears), is.finite(years_range))] # 78 cases that can be imputed
setorder(dat, officerid_num, date)

# Filling using "Last Observation Carried Forward"
dat[,officer_nyears2 := nafill(officer_nyears, "nocb"), by = officerid_num]
dat[,officer_nyears2 := nafill(officer_nyears2, "locf"), by = officerid_num]

dat[, years_range := NULL]
dat[, officer_nyears := NULL]
setnames(dat, "officer_nyears2", "officer_nyears")

# Creating date variable -------------------------------------------------------

# Counting events
dat[, nofficers := .N, keyby = caseid]
dat[, table(nofficers)]

# NAs are 0s?
dat[, firearm_discharge := fcoalesce(firearm_discharge, 0L)]
dat[, firearm_pointed   := fcoalesce(firearm_pointed, 0L)]

# What proportion fired or pointed a gun
dat[, prop_fired   := sum(firearm_discharge)/.N, by = caseid]
dat[, prop_pointed := sum(firearm_pointed)/.N, by = caseid]

# Relevant for conditional logit
dat[, clog_fired   := prop_fired > 0   & prop_fired < 1,]
dat[, clog_pointed := prop_pointed > 0 & prop_pointed < 1,]

# Lagged exposure
setorder(dat, officerid_num, date, caseid)
dat[, exposure_i := sum(firearm_pointed) - firearm_pointed, by = caseid]
dat[, exposure_i := shift(exposure_i, type = "lag"), by = officerid_num]
# dat[!is.na(exposure_i), exposure_i := as.integer(exposure_i > 0)]

# Deferred exposure
dat[, cumpointed := cumsum(fcoalesce(firearm_pointed, 0L)), by = officerid_num]
dat[, exposure_d := sum(cumpointed > 0) - (cumpointed > 0), by = caseid]
# View(dat[, .(officerid_num, caseid, exposure_d, cumpointed)])
dat[, exposure_d := shift(exposure_d, type = "lag"), by = officerid_num]

# How many events?
dat[, nevents := 1:.N, by = officerid_num]
unique(dat[, list(clog_fired, caseid)])[,table(clog_fired)] # 29
unique(dat[, list(clog_pointed, caseid)])[,table(clog_pointed)] # 316

# How many records
dat[clog_fired == TRUE, .N] # 93
dat[clog_pointed == TRUE, .N] # 893

# Generating the model data pointed --------------------------------------------
data_model <- dat[clog_pointed == TRUE, .(
  officerid_num,
  town,
  # officer_rank_cat,
  firearm_pointed, 
  officer_male, 
  officer_nyears, 
  officer_po, 
  # officer_sleo,
  caseid,
  exposure_i,
  exposure_d,
  officer_race,
  nevents,
  nevents,
  # overall_race,
  town
  )]

data_model[, lapply(.SD, function(i) sum(is.na(i)))]

data_model <- data_model[complete.cases(data_model)]

# Models
library(survival)
model_0 <- firearm_pointed ~ I(as.integer(exposure_i > 0)) + officer_male +
  nevents + I(officer_nyears^2)+ officer_nyears + officer_po +
  factor(officer_race) + strata(caseid)

model_1 <- firearm_pointed ~ I(as.integer(exposure_i > 0)) + officer_male +
  nevents + officer_nyears + officer_po +
  factor(officer_race) + strata(caseid)

model_2 <- firearm_pointed ~ I(as.integer(exposure_d > 0)) + officer_male +
  nevents + I(officer_nyears^2)+ officer_nyears + officer_po +
  factor(officer_race) + strata(caseid)

model_3 <- firearm_pointed ~ I(as.integer(exposure_d > 0)) + officer_male +
  nevents + officer_nyears + officer_po +
  factor(officer_race) + strata(caseid)


ans_0 <- clogit(model_0, dat = data_model)
ans_1 <- clogit(model_1, dat = data_model)
ans_2 <- clogit(model_2, dat = data_model)
ans_3 <- clogit(model_3, dat = data_model)

texreg::screenreg(list(
  ans_0, ans_1, ans_2, ans_3
))

lmtest::lrtest(ans_0, ans_1)
lmtest::lrtest(ans_2, ans_3)

table(data_model[, length(unique(town)), by = caseid]$V1)

summary(
  glm(firearm_pointed ~
        exposure_i +
        officer_male  +
        # nevents +
        # I(overall_race != officer_race) +
        # I(overall_race == "black") +
        # I(officer_nyears^2)+
        officer_nyears+
        officer_po +
        factor(officer_race)
        , data = data_model, family = binomial()
      )
)

# Generating the model data fired ----------------------------------------------
data_model <- dat[clog_fired == TRUE, .(
  officerid_num,
  # officer_rank_cat,
  firearm_discharge, 
  officer_male, 
  officer_nyears, 
  officer_po, 
  # officer_sleo,
  caseid,
  # exposure_i,
  officer_race
  # nevents
)]

data_model <- data_model[complete.cases(data_model)]
library(survival)
ans <- clogit(
  firearm_discharge ~
    # exposure_i +
    officer_male  +
    # nevents +
    # factor(officer_rank_cat) + 
    officer_po +
    I(officer_race == "white") +
    # officer_sleo +
    officer_nyears+
    # I(officer_nyears^2)+
    strata(caseid),
  dat = data_model,
  # family = binomial("logit")
)
summary(ans)

# Model taking into account features of the event itself -----------------------
# but actually looking at features relating to interaction with the individual

library(lme4)
ans_re <- glmer(
  firearm_pointed ~ 
  officer_male  +
    # nevents +
    exposure_i + 
    factor(officer_race)+
    # I(overall_race != officer_race) +
    # I(overall_race == "black") +
    # I(overall_race == "hispanic") +
    # I(overall_race == "white") +
    nsubjects + 
    I(officer_nyears^2)+
    officer_nyears+
    officer_po +
    # factor(officer_race)  + 
    factor(county) +
    (1 | caseid),
  data = dat,
  subset  = nofficers >= 2 & grepl("^(Essex|Ocean)", county),
  family  = binomial(link="logit"),
  control = glmerControl(sparseX = TRUE)
)

summary(ans_re)
