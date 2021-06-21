library(data.table)

dat <- fread("data-raw/njdatageorge200918.csv")

dat[, date := as.IDate(date, format = "%m/%d/%Y")]

# Looking at the date variable -------------------------------------------------
dat[, missing_date := is.na(date)]
dat[, missing_date := any(missing_date) & any(!missing_date), by=caseid]
dat[, table(missing_date, useNA = "always")] # Zero cases in which we can attribute a date

# We need to drop all those observations, which are in total 768
dat <- dat[!is.na(missing_date)]
dat[, missing_date := NULL]

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

# Checking if we can impute gender
# dat[, gender := fcoalesce(
#   officer_male,
#   as.integer(mean(officer_male, na.rm = TRUE) > .5)
#   ), by = officerid_num]
# 
# dat[, table(officer_male, gender, useNA = "always")]

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

# These variables are equal to one if "yes" and NA otherwise
dat[, firearm_discharge := fcoalesce(firearm_discharge, 0L)]
dat[, firearm_used      := fcoalesce(firearm_used, 0L)]

# Lagged exposure
setorder(dat, officerid_num, date, caseid)
dat[, exposure_d := sum(firearm_used, na.rm = TRUE) - firearm_used, by = caseid]
dat[, exposure_d := shift(exposure_d, type = "lag"), by = officerid_num]

# Cumulative lagged exposure
dat[, exposure_d_cum := cumsum(fcoalesce(exposure_d, 0L)), by = officerid]
dat[, exposure_d_cum := fifelse(is.na(exposure_d), NA_integer_, exposure_d_cum)]

# Deferred exposure
# dat[, cumpointed := cumsum(fcoalesce(firearm_used, 0L)), by = officerid_num]
# dat[, exposure_i:= sum(exposure_d > 0) - (exposure_d > 0), by = caseid]
# dat[, exposure_i:= shift(exposure_i type = "lag"), by = officerid_num]
setorder(dat, officerid_num, date, caseid)
dat[, ccsum := cumsum(firearm_used), by = officerid_num]

# Sum of individuals who have previously pointed a firearm
dat[, exposure_i:= sum(ccsum > 0L, na.rm = TRUE) - (ccsum > 0L), by = caseid]

# Correcting if all observations are null
dat[, exposure_i:= fifelse(is.na(ccsum) == .N, NA_integer_, exposure_i), by = caseid]
dat[, exposure_i:= shift(exposure_i, type="lag", n=1), by = officerid_num]
# dat[, ccsum := NULL]

# Cumulative lagged exposure ---------------------------------------------------
# dat[, exposure_i_cum := cumsum(fcoalesce(exposure_i, 0L)), by = officerid]
# dat[, exposure_i_cum := fifelse(is.na(exposure_i), NA_integer_, exposure_i_cum)]
# 
# dat[, addmargins(table(exposure_d, exposure_i, useNA = "always"))]

# Avoiding double counting
setorder(dat, officerid_num, date, caseid)
dat[, officers := paste0(
  caseid, "-", officerid_num[which(ccsum > 0)], collapse = ","
  ), by = caseid]

dat[, c("officers_cum") := (function(x, ids, id) {
  d <- unlist(strsplit(x, ","))
  
  d <- data.table(
    off  = stringr::str_extract(d, "[0-9]+$"),
    case = stringr::str_extract(d, "^[0-9]+")
  )
  d <- d[complete.cases(d) & off != id[1]]
  
  if (nrow(d) == 0)
    return(integer(length(ids)))
  
  m <- matrix(0L,nrow = length(ids), ncol=length(unique(d$off)),
              dimnames = list(ids, unique(d$off)))
  
  m[cbind(d$case, d$off)] <- 1L
  # print(m)    
  m <- matrix(apply(m, 2, cumsum), ncol = length(ids), byrow = TRUE)
  m[] <- m[] > 0
# print(m)  
  colSums(m)
  
})(officers, caseid, officerid_num), by = officerid_num]

dat[, exposure_i_cum := shift(as.integer(officers_cum), n = 1, type = "lag"), by = officerid_num]
dat[, table(as.integer(exposure_i_cum), exposure_i_cum)]

# View(dat[officerid_num==1653, .(caseid, officers, exposure_i_cum2, exposure_i, exposure_i_cum, officerid_num)])

# Unique alters ---------------------------------------------------

# Avoiding double counting
setorder(dat, officerid_num, date, caseid)
dat[, officers := paste0(
  caseid, "-", officerid_num, collapse = ","
), by = caseid]

dat[, c("alters_cum") := (function(x, ids, id) {
  d <- unlist(strsplit(x, ","))
  
  d <- data.table(
    off  = stringr::str_extract(d, "[0-9]+$"),
    case = stringr::str_extract(d, "^[0-9]+")
  )
  d <- d[complete.cases(d) & off != id[1]]
  
  if (nrow(d) == 0)
    return(integer(length(ids)))
  
  m <- matrix(0L,nrow = length(ids), ncol=length(unique(d$off)),
              dimnames = list(ids, unique(d$off)))
  
  m[cbind(d$case, d$off)] <- 1L
  # print(m)    
  m <- matrix(apply(m, 2, cumsum), ncol = length(ids), byrow = TRUE)
  m[] <- m[] > 0
  # print(m)  
  colSums(m)
  
})(officers, caseid, officerid_num), by = officerid_num]

dat[, alters_cum := shift(as.integer(alters_cum), n = 1, type = "lag"), by = officerid_num]
dat[, table(as.integer(exposure_i_cum), exposure_i_cum)]

# View(dat[officerid_num==1653, .(caseid, officers, exposure_i, exposure_i_cum, alters_cum, officerid_num)])


# Number of days since the exposure --------------------------------------------
setorder(dat, officerid_num, date, caseid)

dat[, days_since_exp_d := fifelse(
  exposure_d > 0L, 
  date - shift(date, type = "lag", n = 1),
  0L
), by = officerid_num]

dat[, days_since_exp_i := fifelse(
  exposure_i > 0L, 
  date - shift(date, type = "lag", n = 1),
  0L
), by = officerid_num]

  
# dat[officerid_num==1653, .(officerid_num, caseid, date, days_since_exp_i, exposure_i, days_since_exp_d, exposure_d)]


# Subsetting data for analysis -------------------------------------------------

# How many events?
dat[, nevents := 1:.N, by = officerid_num]

data_model <- dat[, .(
  caseid,
  officerid_num,
  officer_male,
  # firearm_pointed,
  firearm_used,
  firearm_discharge,
  officer_nyears,
  officer_po,
  # officer_sleo,
  exposure_d,
  exposure_i,
  exposure_d_cum,
  exposure_i_cum,
  alters_cum,
  officer_race,
  nevents,
  # town,
  # supid,
  nofficers,
  nsubjects,
  days_since_exp_d,
  days_since_exp_i,
  county,
  town
)]

data_model[, relative_exp := officer_nyears - mean(officer_nyears), by = caseid]
data_model[, relative_exp := fcoalesce(relative_exp, NA_real_), by = caseid]
# model_data[, relative_exp := which.max(officer_nyears) == 1:.N, by = caseid]

# Imputing officer gender
merge(
  x = data_model[is.na(officer_male)],
  y = unique(dat[,.(officerid, officerid_num)]),
  by = "officerid_num", all.x = TRUE, all.y = FALSE
)[,.(officerid, officerid_num)]

# officerid officerid_num
# 1:                andres_bran_paterson           536 1
# 2:                andres_brea_paterson           536 1
# 3:              brian_dibiasi_hamilton          1677 1
# 4:                   c_provenzano_njsp          2107  
# 5:          christopher_dimeo_hamilton          2711 1
# 6:          christopher_dimeo_hamilton          2711 1
# 7:          christopher_dimeo_hamilton          2711 1
# 8:          christopher_dimeo_hamilton          2711 1
# 9:        christopher_williams_madison          3027 1
# 10:          clifford_spencer_lumberton          3064 1
# 11:        clifford_spencer_mount holly          3064 1
# 12:          corey_knoedler_cherry hill          3142 1
# 13:               david_bacsik_hamilton          3839 1
# 14:              david_belbin_paulsboro          3847 1
# 15:       david_canica_north plainfield          3869 1
# 16:      david_dzibela_north plainfield          3913 1
# 17:             david_leonardi_hamilton          3997 1
# 18:            dean_janowski_woodbridge          4190 1
# 19: dennis_richards_gloucester township          4277 1
# 20:            dennis_sullivan_hamilton          4288 1
# 21:     donald_everett_north plainfield          4448 1
# 22:              e_mcquarry_mount holly          4625  
# 23:             edmund_ansara_millville          4708 1
# 24:      edward_sinker_north plainfield          4828 1
# 25:            gavin_rossner_pennsauken          5700 1
# 26:                 harry_brock_clifton          6148 1
# 27:         james_distelcamp_woodbridge          6791 1
# 28:               james_rickey_hamilton          7023 1
# 29:     jason_smith_gloucester township          7329 1
# 30:     jason_smith_gloucester township          7329 1
# 31:             jeffrey_galant_hamilton          7509 1
# 32:          jeffrey_horvath_woodbridge          7528 1
# 33:              jhad_carter_plainfield          7802 1
# 34:            john_janowski_woodbridge          8131 1
# 35:              joseph_dixon_millville          8851 1
# 36:          joseph_marietta_pennsauken          8997 1
# 37:                joseph_wilk_hamilton          9218 1
# 38:              justin_bowman_watchung          9414 1
# 39:              justin_vallier_clifton          9493 1
# 40:           keith_rutherford_hamilton          9698 1
# 41:                   l_vasta_paulsboro         10314  
# 42:                louis_lobue_hamilton         10562 1
# 43:             marco_grossmann_hoboken         10984 1
# 44:           mark_straszewski_hamilton         11217 1
# 45:         martesse_gilliam_plainfield         11262 1
# 46:            martin_heath_cherry hill         11271 1
# 47:     michael_diguglielmo_east orange         12005 1
# 48:            michael_niven_woodbridge         12408 1
# 49:       michael_terracciano_manasquan         12644 1
# 50:          nicholas_avanzato_hamilton         12995 1
# 51:            nicole_thigpen_paulsboro         13220 0
# 52:                 ramie_nouh_paterson         14070 0
# 53:             randy_colondres_clifton         14091 1
# 54:            richard_liedtka_hamilton         14407 1
# 55:             richard_rettzo_hamilton         14467 1
# 56: robert_kropewnicki_north plainfield         14791 1
# 57:           rodney_richards_paulsboro         15090 1
# 58:           roger_fernandez_manasquan         15103 1
# 59:                 ryan_dunne_watchung         15339 1
# 60:                        s_brown_njsp         15484  
# 61:          stephen_varady_cherry hill         16320 1
# 62:         steven_ptaszynski_fairfield         16467 1
# 63:            thomas_clugsten_hamilton         16760 1
# 64:             thomas_ganci_woodbridge         16820 1
# 65:              tyseme_holmes_paterson         17413  
# 66:                w_reichert_paulsboro         17639  
# 67:         william_petrovey_woodbridge         17991 1
# officerid officerid_num

imputed_gender <- matrix(c(536L, 1L,
536L, 1L,
1677L, 1L,
# L2107,L  ,
2711L, 1L,
2711L, 1L,
2711L, 1L,
2711L, 1L,
3027L, 1L,
3064L, 1L,
3064L, 1L,
3142L, 1L,
3839L, 1L,
3847L, 1L,
3869L, 1L,
3913L, 1L,
3997L, 1L,
4190L, 1L,
4277L, 1L,
4288L, 1L,
4448L, 1L,
# L4625,L  ,
4708L, 1L,
4828L, 1L,
5700L, 1L,
6148L, 1L,
6791L, 1L,
7023L, 1L,
7329L, 1L,
7329L, 1L,
7509L, 1L,
7528L, 1L,
7802L, 1L,
8131L, 1L,
8851L, 1L,
8997L, 1L,
9218L, 1L,
9414L, 1L,
9493L, 1L,
9698L, 1L,
# L10314,L  ,
10562L, 1L,
10984L, 1L,
11217L, 1L,
11262L, 1L,
11271L, 1L,
12005L, 1L,
12408L, 1L,
12644L, 1L,
12995L, 1L,
13220L, 0L,
14070L, 0L,
14091L, 1L,
14407L, 1L,
14467L, 1L,
14791L, 1L,
15090L, 1L,
15103L, 1L,
15339L, 1L,
# L15484,L  ,
16320L, 1L,
16467L, 1L,
16760L, 1L,
16820L, 1L,
# L17413,L  ,
# L17639,L  ,
17991L, 1L), byrow = TRUE, ncol = 2L, dimnames = list(NULL, c("officerid_num", "officer_male2")))
imputed_gender <- data.table(imputed_gender)

# Merging and replacing missings
data_model <- merge(
  x = data_model,
  y = unique(imputed_gender),
  by = "officerid_num",
  all.x = TRUE, all.y = FALSE
)

data_model[, officer_male := fcoalesce(officer_male, officer_male2)]
data_model[, officer_male2 := NULL]

# Preparing for conditional logit  ---------------------------------------------
data_model <- data_model[complete.cases(data_model)]

# What proportion fired or pointed a gun
data_model[, prop_used := sum(firearm_used)/.N, by = caseid]

# Relevant for conditional logit
data_model[, clog_used := prop_used > 0 & prop_used < 1,]

# How many records
data_model <- data_model[clog_used == TRUE] # 893

# Dropping cases in which the number of officers is less than 2
# data_model[, nofficers2 := .N, by=caseid]

data_model[, prop_used := NULL]

# Decades
data_model[, officer_nyears2 := (officer_nyears/10) ^ 2]

fwrite(data_model, file = "data/model_data.csv")

dat[, table(exposure_i, exposure_d, useNA = "always")]
# exposure_d
# exposure_i    0     1     2     3     4     5     6
#          0 41123   461    71    26    24     6     0
#          1   504    49    22    14     4     2     1
#          2    37     7     2     3     3     3     0
#          3     5     0     0     0     0     0     0
