Dynamic Exposure
================

``` r
knitr::opts_chunk$set(warning = FALSE)
```

# Data loading

``` r
library(data.table)

# Reading the data and identifying individuals and their first shooting event
njforce <- data.table::fread("../data-raw/njforce_200210.csv")
reports <- subset(
  njforce,
  select = c(
    date, officerid, firearm_discharged, firearm_pointed, incidentid,
    officer_male, officer_nyears, officer_race, officer_rank, town,
    officer_po, officer_sleo, nsubjects, Incident_type
    ))
reports[, date := as.Date(date, format = "%m/%d/%Y")]
```

I don’t see many discharged reports, so we will look at pointed. Only 11
records show discharged weapon, though 764 are firearm pointed. For the
11 records, it would suffice do a qualitative analysis.

``` r
table(reports$firearm_pointed, reports$firearm_discharged)
```

    ##    
    ##        0    1
    ##   0 3270   11
    ##   1  764    0

How many officers per incident?

``` r
# How many officers per incident
tab <- table(njforce[, list(N=.N), by = incidentid]$N)
tab <- cbind(
  N = tab,
  pcent = tab/sum(tab),
  cum_pcent = cumsum(tab/sum(tab))
  )
knitr::kable(tab)
```

|   |    N |     pcent | cum\_pcent |
| :- | ---: | --------: | ---------: |
| 1 | 1580 | 0.6174287 |  0.6174287 |
| 2 |  662 | 0.2586948 |  0.8761235 |
| 3 |  202 | 0.0789371 |  0.9550606 |
| 4 |   66 | 0.0257913 |  0.9808519 |
| 5 |   32 | 0.0125049 |  0.9933568 |
| 6 |   12 | 0.0046893 |  0.9980461 |
| 7 |    3 | 0.0011723 |  0.9992184 |
| 9 |    2 | 0.0007816 |  1.0000000 |

# Calculating Dynamic Exposure

``` r
# Counting the number of guns pointed
reports[, nofficers := .N, by = "incidentid"]

# Ordering the data for some cumulative computations
reports <- reports[order(officerid, date),]

# Event number counter
reports[, nevent := 1:.N, by = "officerid"]

# Creating a cumulative counter per officer
reports[, fdischarged := cumsum(firearm_discharged), by = "officerid"]
reports[, fpointed    := cumsum(firearm_pointed), by = "officerid"]
reports[, fpointed    := cumsum(firearm_pointed), by = "officerid"]

# TRUE if the individual had ever pointed a gun in the past
reports[, ever_pointed := fpointed != 0]
```

**Immediate Exposure**: Number of officers other than \(i\) who pointed
a gun at event \(T\).

``` r
reports[, exposure_i := sum(firearm_pointed) - firearm_pointed, by = "incidentid"]
reports[, exposure_i := shift(exposure_i, type="lag"), by = "officerid"]
```

**Deferred Exposure**: Number of officers other than \(i\) who
participated in event \(T\) who have ever pointed a gun.

``` r
reports[, exposure_d := sum(ever_pointed) - ever_pointed, by = "incidentid"]
reports[, exposure_d := shift(exposure_d, type="lag"), by = "officerid"]
```

**Cumulative Exposure**: Number of officers other than \(i\) who
participated in event \(T\) who have ever pointed a gun in the presence
of \(i\).

Where \(T(i)\) is the set that represent the events in which \(i\) was
involved.

``` r
#TBD
```

``` r
# Identifying the first event with either pointed or discharged, we will
# use this later.
reports[, first := ever_pointed & !shift(ever_pointed, type="lag", fill=TRUE), by = "officerid"]

# Average peer experience (as in number of reported events)
reports[, peer_exp := (sum(nevent) - nevent)/(nofficers - 1 + 1e-20), by = "incidentid"]

# Adding other features
reports[, female := as.integer(officer_male == 0)]
reports[, police := as.integer(officer_po == 1)]
reports[, sleo   := as.integer(officer_sleo == 1)]

# Indicator variable to flag whether it is relevant or not
reports[, relevant := (1:.N > 1L) & (!ever_pointed | first), by = "officerid"]

# Violent or weapon related incident
reports[, violent_or_weapon := as.integer(grepl("armed|gun|violen|weap|shoot|knife", Incident_type))]
```

# Naive models

## Predicting the first incident

``` r
reports[, exposure_yesno := as.integer(exposure_d > 0)]
reports[, pressure := nofficers/(nsubjects + 1e-20)]

ans0 <- glm(
  firearm_pointed ~ exposure_yesno + nevent + nofficers + female +
    officer_nyears + factor(officer_race) + violent_or_weapon + factor(town),
  data   = reports,
  subset = relevant == TRUE,
  family = binomial(link="logit")
  )

ans1 <- glm(
  firearm_pointed ~ exposure_yesno + I(exposure_yesno * female) + nevent +
    nofficers + female + officer_nyears + factor(officer_race) + violent_or_weapon +
    factor(town),
  data   = reports,
  subset = relevant == TRUE,
  family = binomial(link="logit")
  )

ans2 <- glm(
  firearm_pointed ~ exposure_yesno + I(exposure_yesno * female) + nevent +
    nofficers + female + officer_nyears + factor(officer_race) + 
    police + sleo + nsubjects + violent_or_weapon + factor(town),
  data   = reports,
  subset = relevant == TRUE,
  family = binomial(link="logit")
  )

ans3 <- glm(
  firearm_pointed ~ exposure_d + I(exposure_d * female) + I(exposure_d^2) + nevent +
    nofficers + female + officer_nyears + factor(officer_race) + 
    police + sleo + nsubjects + violent_or_weapon + factor(town),
  data   = reports,
  subset = relevant == TRUE,
  family = binomial(link="logit")
  )

ans4 <- glm(
  firearm_pointed ~ exposure_d +  I(exposure_d^2) + 
    sleo + nsubjects + violent_or_weapon + factor(town),
  data   = reports,
  subset = relevant == TRUE,
  family = binomial(link="logit")
  )

ans5 <- glm(
  firearm_pointed ~ exposure_d +  I(exposure_d^2) + 
    sleo + nsubjects + factor(town),
  data   = reports,
  subset = relevant == TRUE,
  family = binomial(link="logit")
  )

ans6 <- glm(
  firearm_pointed ~ exposure_d +  I(exposure_d^2) + 
    sleo + nsubjects + factor(town) + pressure,
  data   = reports,
  subset = relevant == TRUE,
  family = binomial(link="logit")
  )

# reports$officer_race

cmap <- list(
  exposure_yesno           = "Exposure (yes/no)",
  `I(exposure_yesno * female)` = "Exposure (yes/no) x Female",
  exposure_d               = "Exposure (count)",
  `I(exposure_d^2)`        = "Exposure (count) ^2",
  `I(exposure_d * female)` = "Exposure (count) x Female",
  exposure_d_indirect      = "Exposure (indirect, count)",
  nevent                   = "Num. of Event",
  nofficers                  = "N. Officers Involved",
  officer_nyears           = "Tenure (t)",
  nsubjects                = "N. Subjects",
  peer_exp                 = "Peer experience (avg. years)",
  female                   = "Female (yes/no)",
  `factor(officer_race)1`  = "Asian (yes/no)", 
  `factor(officer_race)2`  = "Black (yes/no)", 
  `factor(officer_race)3`  = "Hispanic (yes/no)", 
  `factor(officer_race)4`  = "White (yes/no)", 
  police                   = "is police off. (yes/no)",
  sleo                     = "is law enforcement off. (yes/no)",
  violent_or_weapon        = "Violent or Weapon related (yes/no)",
  pressure                 = "N Officers/N Subjects"
)

# Officer_race
# 1 = asian
# 2 = black
# 3 = Hispanic
# 4 = white

library(texreg)
```

    ## Version:  1.37.5
    ## Date:     2020-06-17
    ## Author:   Philip Leifeld (University of Essex)
    ## 
    ## Consider submitting praise using the praise or praise_interactive functions.
    ## Please cite the JSS article in your publications -- see citation("texreg").

``` r
models <- mget(ls(pattern = "^ans[0-9]+$"))

# Computing McFadden's pseudo r2
pr2 <- sapply(models, function(m) {
  1 - with(m, deviance/null.deviance)
})

auc <- sapply(models, function(m) {
  AUC::auc(AUC::roc(predict(m, type = "response"), as.factor(m$y)))
})

htmlreg(
  models, custom.coef.map = cmap,
  custom.gof.rows = list("Pseudo R2" = pr2, "AUC" = auc)
  )
```

<table class="texreg" style="margin: 10px auto;border-collapse: collapse;border-spacing: 0px;caption-side: bottom;color: #000000;border-top: 2px solid #000000;">

<caption>

Statistical models

</caption>

<thead>

<tr>

<th style="padding-left: 5px;padding-right: 5px;">

 

</th>

<th style="padding-left: 5px;padding-right: 5px;">

ans0

</th>

<th style="padding-left: 5px;padding-right: 5px;">

ans1

</th>

<th style="padding-left: 5px;padding-right: 5px;">

ans2

</th>

<th style="padding-left: 5px;padding-right: 5px;">

ans3

</th>

<th style="padding-left: 5px;padding-right: 5px;">

ans4

</th>

<th style="padding-left: 5px;padding-right: 5px;">

ans5

</th>

<th style="padding-left: 5px;padding-right: 5px;">

ans6

</th>

</tr>

</thead>

<tbody>

<tr style="border-top: 1px solid #000000;">

<td style="padding-left: 5px;padding-right: 5px;">

Exposure (yes/no)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

0.21

</td>

<td style="padding-left: 5px;padding-right: 5px;">

0.19

</td>

<td style="padding-left: 5px;padding-right: 5px;">

0.20

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

</tr>

<tr>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.18)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.19)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.20)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

</tr>

<tr>

<td style="padding-left: 5px;padding-right: 5px;">

Exposure (yes/no) x Female

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

0.61

</td>

<td style="padding-left: 5px;padding-right: 5px;">

0.97

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

</tr>

<tr>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.92)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.97)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

</tr>

<tr>

<td style="padding-left: 5px;padding-right: 5px;">

Exposure (count)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

0.21

</td>

<td style="padding-left: 5px;padding-right: 5px;">

0.22

</td>

<td style="padding-left: 5px;padding-right: 5px;">

0.20

</td>

<td style="padding-left: 5px;padding-right: 5px;">

0.18

</td>

</tr>

<tr>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.24)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.22)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.22)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.22)

</td>

</tr>

<tr>

<td style="padding-left: 5px;padding-right: 5px;">

Exposure (count) ^2

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

\-0.06

</td>

<td style="padding-left: 5px;padding-right: 5px;">

\-0.03

</td>

<td style="padding-left: 5px;padding-right: 5px;">

\-0.02

</td>

<td style="padding-left: 5px;padding-right: 5px;">

\-0.01

</td>

</tr>

<tr>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.08)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.07)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.06)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.06)

</td>

</tr>

<tr>

<td style="padding-left: 5px;padding-right: 5px;">

Exposure (count) x Female

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

0.63

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

</tr>

<tr>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.49)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

</tr>

<tr>

<td style="padding-left: 5px;padding-right: 5px;">

Num. of Event

</td>

<td style="padding-left: 5px;padding-right: 5px;">

0.02

</td>

<td style="padding-left: 5px;padding-right: 5px;">

0.02

</td>

<td style="padding-left: 5px;padding-right: 5px;">

0.01

</td>

<td style="padding-left: 5px;padding-right: 5px;">

0.01

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

</tr>

<tr>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.03)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.03)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.03)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.03)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

</tr>

<tr>

<td style="padding-left: 5px;padding-right: 5px;">

N. Officers Involved

</td>

<td style="padding-left: 5px;padding-right: 5px;">

0.06

</td>

<td style="padding-left: 5px;padding-right: 5px;">

0.06

</td>

<td style="padding-left: 5px;padding-right: 5px;">

\-0.02

</td>

<td style="padding-left: 5px;padding-right: 5px;">

\-0.02

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

</tr>

<tr>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.06)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.06)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.07)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.07)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

</tr>

<tr>

<td style="padding-left: 5px;padding-right: 5px;">

Tenure (t)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

\-0.03<sup>\*</sup>

</td>

<td style="padding-left: 5px;padding-right: 5px;">

\-0.04<sup>\*</sup>

</td>

<td style="padding-left: 5px;padding-right: 5px;">

\-0.05<sup>\*\*</sup>

</td>

<td style="padding-left: 5px;padding-right: 5px;">

\-0.05<sup>\*\*</sup>

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

</tr>

<tr>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.01)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.01)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.02)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.02)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

</tr>

<tr>

<td style="padding-left: 5px;padding-right: 5px;">

N. Subjects

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

1.64<sup>\*\*\*</sup>

</td>

<td style="padding-left: 5px;padding-right: 5px;">

1.64<sup>\*\*\*</sup>

</td>

<td style="padding-left: 5px;padding-right: 5px;">

1.67<sup>\*\*\*</sup>

</td>

<td style="padding-left: 5px;padding-right: 5px;">

1.64<sup>\*\*\*</sup>

</td>

<td style="padding-left: 5px;padding-right: 5px;">

1.90<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.21)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.21)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.20)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.20)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.23)

</td>

</tr>

<tr>

<td style="padding-left: 5px;padding-right: 5px;">

Female (yes/no)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

\-0.31

</td>

<td style="padding-left: 5px;padding-right: 5px;">

\-0.59

</td>

<td style="padding-left: 5px;padding-right: 5px;">

\-0.92

</td>

<td style="padding-left: 5px;padding-right: 5px;">

\-0.94

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

</tr>

<tr>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.47)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.66)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.70)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.65)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

</tr>

<tr>

<td style="padding-left: 5px;padding-right: 5px;">

Black (yes/no)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

1.21

</td>

<td style="padding-left: 5px;padding-right: 5px;">

1.23

</td>

<td style="padding-left: 5px;padding-right: 5px;">

1.24

</td>

<td style="padding-left: 5px;padding-right: 5px;">

1.25

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

</tr>

<tr>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(1.16)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(1.17)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(1.24)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(1.24)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

</tr>

<tr>

<td style="padding-left: 5px;padding-right: 5px;">

Hispanic (yes/no)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

1.69

</td>

<td style="padding-left: 5px;padding-right: 5px;">

1.71

</td>

<td style="padding-left: 5px;padding-right: 5px;">

1.63

</td>

<td style="padding-left: 5px;padding-right: 5px;">

1.65

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

</tr>

<tr>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(1.34)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(1.34)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(1.42)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(1.43)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

</tr>

<tr>

<td style="padding-left: 5px;padding-right: 5px;">

White (yes/no)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

0.92

</td>

<td style="padding-left: 5px;padding-right: 5px;">

0.92

</td>

<td style="padding-left: 5px;padding-right: 5px;">

0.89

</td>

<td style="padding-left: 5px;padding-right: 5px;">

0.89

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

</tr>

<tr>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(1.11)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(1.11)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(1.18)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(1.18)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

</tr>

<tr>

<td style="padding-left: 5px;padding-right: 5px;">

is police off. (yes/no)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

\-0.29

</td>

<td style="padding-left: 5px;padding-right: 5px;">

\-0.29

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

</tr>

<tr>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.29)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.29)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

</tr>

<tr>

<td style="padding-left: 5px;padding-right: 5px;">

is law enforcement off. (yes/no)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

\-2.43<sup>\*</sup>

</td>

<td style="padding-left: 5px;padding-right: 5px;">

\-2.44<sup>\*</sup>

</td>

<td style="padding-left: 5px;padding-right: 5px;">

\-1.82

</td>

<td style="padding-left: 5px;padding-right: 5px;">

\-1.75

</td>

<td style="padding-left: 5px;padding-right: 5px;">

\-1.73

</td>

</tr>

<tr>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(1.10)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(1.11)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(1.05)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(1.03)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(1.03)

</td>

</tr>

<tr>

<td style="padding-left: 5px;padding-right: 5px;">

Violent or Weapon related (yes/no)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

2.03<sup>\*\*\*</sup>

</td>

<td style="padding-left: 5px;padding-right: 5px;">

2.05<sup>\*\*\*</sup>

</td>

<td style="padding-left: 5px;padding-right: 5px;">

2.38<sup>\*\*\*</sup>

</td>

<td style="padding-left: 5px;padding-right: 5px;">

2.42<sup>\*\*\*</sup>

</td>

<td style="padding-left: 5px;padding-right: 5px;">

2.31<sup>\*\*\*</sup>

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

</tr>

<tr>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.46)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.46)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.50)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.50)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.48)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

</tr>

<tr>

<td style="padding-left: 5px;padding-right: 5px;">

N Officers/N Subjects

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

0.00<sup>\*</sup>

</td>

</tr>

<tr>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.00)

</td>

</tr>

<tr style="border-top: 1px solid #000000;">

<td style="padding-left: 5px;padding-right: 5px;">

Pseudo R2

</td>

<td style="padding-left: 5px;padding-right: 5px;">

0.05

</td>

<td style="padding-left: 5px;padding-right: 5px;">

0.05

</td>

<td style="padding-left: 5px;padding-right: 5px;">

0.13

</td>

<td style="padding-left: 5px;padding-right: 5px;">

0.13

</td>

<td style="padding-left: 5px;padding-right: 5px;">

0.12

</td>

<td style="padding-left: 5px;padding-right: 5px;">

0.10

</td>

<td style="padding-left: 5px;padding-right: 5px;">

0.11

</td>

</tr>

<tr>

<td style="padding-left: 5px;padding-right: 5px;">

AUC

</td>

<td style="padding-left: 5px;padding-right: 5px;">

0.64

</td>

<td style="padding-left: 5px;padding-right: 5px;">

0.64

</td>

<td style="padding-left: 5px;padding-right: 5px;">

0.71

</td>

<td style="padding-left: 5px;padding-right: 5px;">

0.71

</td>

<td style="padding-left: 5px;padding-right: 5px;">

0.71

</td>

<td style="padding-left: 5px;padding-right: 5px;">

0.69

</td>

<td style="padding-left: 5px;padding-right: 5px;">

0.69

</td>

</tr>

<tr>

<td style="padding-left: 5px;padding-right: 5px;">

AIC

</td>

<td style="padding-left: 5px;padding-right: 5px;">

1036.89

</td>

<td style="padding-left: 5px;padding-right: 5px;">

1038.46

</td>

<td style="padding-left: 5px;padding-right: 5px;">

960.33

</td>

<td style="padding-left: 5px;padding-right: 5px;">

962.11

</td>

<td style="padding-left: 5px;padding-right: 5px;">

983.22

</td>

<td style="padding-left: 5px;padding-right: 5px;">

1004.20

</td>

<td style="padding-left: 5px;padding-right: 5px;">

1000.80

</td>

</tr>

<tr>

<td style="padding-left: 5px;padding-right: 5px;">

BIC

</td>

<td style="padding-left: 5px;padding-right: 5px;">

1108.95

</td>

<td style="padding-left: 5px;padding-right: 5px;">

1115.66

</td>

<td style="padding-left: 5px;padding-right: 5px;">

1052.96

</td>

<td style="padding-left: 5px;padding-right: 5px;">

1059.88

</td>

<td style="padding-left: 5px;padding-right: 5px;">

1034.97

</td>

<td style="padding-left: 5px;padding-right: 5px;">

1050.78

</td>

<td style="padding-left: 5px;padding-right: 5px;">

1052.55

</td>

</tr>

<tr>

<td style="padding-left: 5px;padding-right: 5px;">

Log Likelihood

</td>

<td style="padding-left: 5px;padding-right: 5px;">

\-504.45

</td>

<td style="padding-left: 5px;padding-right: 5px;">

\-504.23

</td>

<td style="padding-left: 5px;padding-right: 5px;">

\-462.16

</td>

<td style="padding-left: 5px;padding-right: 5px;">

\-462.05

</td>

<td style="padding-left: 5px;padding-right: 5px;">

\-481.61

</td>

<td style="padding-left: 5px;padding-right: 5px;">

\-493.10

</td>

<td style="padding-left: 5px;padding-right: 5px;">

\-490.40

</td>

</tr>

<tr>

<td style="padding-left: 5px;padding-right: 5px;">

Deviance

</td>

<td style="padding-left: 5px;padding-right: 5px;">

1008.89

</td>

<td style="padding-left: 5px;padding-right: 5px;">

1008.46

</td>

<td style="padding-left: 5px;padding-right: 5px;">

924.33

</td>

<td style="padding-left: 5px;padding-right: 5px;">

924.11

</td>

<td style="padding-left: 5px;padding-right: 5px;">

963.22

</td>

<td style="padding-left: 5px;padding-right: 5px;">

986.20

</td>

<td style="padding-left: 5px;padding-right: 5px;">

980.80

</td>

</tr>

<tr style="border-bottom: 2px solid #000000;">

<td style="padding-left: 5px;padding-right: 5px;">

Num. obs.

</td>

<td style="padding-left: 5px;padding-right: 5px;">

1270

</td>

<td style="padding-left: 5px;padding-right: 5px;">

1270

</td>

<td style="padding-left: 5px;padding-right: 5px;">

1269

</td>

<td style="padding-left: 5px;padding-right: 5px;">

1269

</td>

<td style="padding-left: 5px;padding-right: 5px;">

1307

</td>

<td style="padding-left: 5px;padding-right: 5px;">

1307

</td>

<td style="padding-left: 5px;padding-right: 5px;">

1307

</td>

</tr>

</tbody>

<tfoot>

<tr>

<td style="font-size: 0.8em;" colspan="8">

<sup>***</sup>p \< 0.001; <sup>**</sup>p \< 0.01; <sup>*</sup>p \< 0.05

</td>

</tr>

</tfoot>

</table>

``` r
# screenreg(models, custom.coef.map = cmap,custom.gof.rows = list("Pseudo R2" = pr2))
```

## Predicting any incident

``` r
# Lagging self-exposure
reports[, firearm_pointed_prev := shift(firearm_pointed, type = "lag", fill = TRUE), by = "officerid"]
reports[, firearm_pointed_prev := cumsum(firearm_pointed_prev), by = "officerid"]

reports[, relevant_any := 1:.N > 1, by = "officerid"]

reports[, exposure_yesno := as.integer(exposure_d > 0)]
ans0 <- glm(
  firearm_pointed ~ exposure_yesno + nevent + nofficers + female +
    officer_nyears + factor(officer_race) + firearm_pointed_prev + violent_or_weapon +
    factor(town),
  data   = reports,
  subset = relevant_any == TRUE,
  family = binomial(link="logit")
  )

ans1 <- glm(
  firearm_pointed ~ exposure_yesno + I(exposure_yesno * female) + nevent +
    nofficers + female + officer_nyears + factor(officer_race) + firearm_pointed_prev +
    violent_or_weapon + factor(town),
  data   = reports,
  subset = relevant_any == TRUE,
  family = binomial(link="logit")
  )

ans2 <- glm(
  firearm_pointed ~ exposure_yesno + I(exposure_yesno * female) + nevent +
    nofficers + female + officer_nyears + factor(officer_race) + 
    police + sleo + nsubjects + firearm_pointed_prev + violent_or_weapon +
    factor(town),
  data   = reports,
  subset = relevant_any == TRUE,
  family = binomial(link="logit")
  )

ans3 <- glm(
  firearm_pointed ~ exposure_d + I(exposure_d * female) + I(exposure_d^2) + nevent +
    nofficers + female + officer_nyears + factor(officer_race) + 
    police + sleo + nsubjects + firearm_pointed_prev + violent_or_weapon +
    factor(town),
  data   = reports,
  subset = relevant_any == TRUE,
  family = binomial(link="logit")
  )

ans4 <- glm(
  firearm_pointed ~ exposure_d + I(exposure_d * female) + I(exposure_d^2) + nevent +
    nofficers + female + officer_nyears + factor(officer_race) + 
    police + sleo + nsubjects + firearm_pointed_prev + violent_or_weapon +
    factor(town),
  data   = reports,
  subset = relevant_any == TRUE,
  family = binomial(link="logit")
  )

ans5 <- glm(
  firearm_pointed ~ exposure_d + I(exposure_d * female) + I(exposure_d^2) + nevent +
    nofficers + female + officer_nyears + factor(officer_race) + 
    police + sleo + nsubjects + firearm_pointed_prev + violent_or_weapon +
    factor(town) + peer_exp,
  data   = reports,
  subset = relevant_any == TRUE,
  family = binomial(link="logit")
  )

ans6 <- glm(
  firearm_pointed ~ exposure_d + I(exposure_d^2) + nevent +
    officer_nyears + 
    sleo + nsubjects + firearm_pointed_prev + violent_or_weapon +
    factor(town),
  data   = reports,
  subset = relevant_any == TRUE,
  family = binomial(link="logit")
  )

ans7 <- glm(
  firearm_pointed ~ exposure_d + I(exposure_d^2) + nevent +
    officer_nyears + 
    sleo + nsubjects + firearm_pointed_prev + factor(town),
  data   = reports,
  subset = relevant_any == TRUE,
  family = binomial(link="logit")
  )

ans8 <- glm(
  firearm_pointed ~ exposure_d + I(exposure_d^2) + nevent +
    officer_nyears + pressure +
    sleo + nsubjects + firearm_pointed_prev + factor(town),
  data   = reports,
  subset = relevant_any == TRUE,
  family = binomial(link="logit")
  )

cmap <- c(cmap, list(firearm_pointed_prev = "N Previous Pointed"))

# Officer_race
# 1 = asian
# 2 = black
# 3 = Hispanic
# 4 = white

library(texreg)
models <- mget(ls(pattern = "^ans[0-9]+$"))

# Computing McFadden's pseudo r2
pr2 <- sapply(models, function(m) {
  1 - with(m, deviance/null.deviance)
})

auc <- sapply(models, function(m) {
  AUC::auc(AUC::roc(predict(m, type = "response"), as.factor(m$y)))
})

htmlreg(
  models, custom.coef.map = cmap,
  custom.gof.rows = list("Pseudo R2" = pr2, "AUC" = auc)
  )
```

<table class="texreg" style="margin: 10px auto;border-collapse: collapse;border-spacing: 0px;caption-side: bottom;color: #000000;border-top: 2px solid #000000;">

<caption>

Statistical models

</caption>

<thead>

<tr>

<th style="padding-left: 5px;padding-right: 5px;">

 

</th>

<th style="padding-left: 5px;padding-right: 5px;">

ans0

</th>

<th style="padding-left: 5px;padding-right: 5px;">

ans1

</th>

<th style="padding-left: 5px;padding-right: 5px;">

ans2

</th>

<th style="padding-left: 5px;padding-right: 5px;">

ans3

</th>

<th style="padding-left: 5px;padding-right: 5px;">

ans4

</th>

<th style="padding-left: 5px;padding-right: 5px;">

ans5

</th>

<th style="padding-left: 5px;padding-right: 5px;">

ans6

</th>

<th style="padding-left: 5px;padding-right: 5px;">

ans7

</th>

<th style="padding-left: 5px;padding-right: 5px;">

ans8

</th>

</tr>

</thead>

<tbody>

<tr style="border-top: 1px solid #000000;">

<td style="padding-left: 5px;padding-right: 5px;">

Exposure (yes/no)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

0.17

</td>

<td style="padding-left: 5px;padding-right: 5px;">

0.17

</td>

<td style="padding-left: 5px;padding-right: 5px;">

0.20

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

</tr>

<tr>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.10)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.10)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.10)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

</tr>

<tr>

<td style="padding-left: 5px;padding-right: 5px;">

Exposure (yes/no) x Female

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

0.10

</td>

<td style="padding-left: 5px;padding-right: 5px;">

0.06

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

</tr>

<tr>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.55)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.59)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

</tr>

<tr>

<td style="padding-left: 5px;padding-right: 5px;">

Exposure (count)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

0.28<sup>\*</sup>

</td>

<td style="padding-left: 5px;padding-right: 5px;">

0.28<sup>\*</sup>

</td>

<td style="padding-left: 5px;padding-right: 5px;">

0.28<sup>\*</sup>

</td>

<td style="padding-left: 5px;padding-right: 5px;">

0.27<sup>\*</sup>

</td>

<td style="padding-left: 5px;padding-right: 5px;">

0.28<sup>\*</sup>

</td>

<td style="padding-left: 5px;padding-right: 5px;">

0.29<sup>\*\*</sup>

</td>

</tr>

<tr>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.11)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.11)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.11)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.11)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.11)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.11)

</td>

</tr>

<tr>

<td style="padding-left: 5px;padding-right: 5px;">

Exposure (count) ^2

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

\-0.08<sup>\*</sup>

</td>

<td style="padding-left: 5px;padding-right: 5px;">

\-0.08<sup>\*</sup>

</td>

<td style="padding-left: 5px;padding-right: 5px;">

\-0.08<sup>\*</sup>

</td>

<td style="padding-left: 5px;padding-right: 5px;">

\-0.08<sup>\*</sup>

</td>

<td style="padding-left: 5px;padding-right: 5px;">

\-0.08<sup>\*</sup>

</td>

<td style="padding-left: 5px;padding-right: 5px;">

\-0.09<sup>\*\*</sup>

</td>

</tr>

<tr>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.03)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.03)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.03)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.03)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.03)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.03)

</td>

</tr>

<tr>

<td style="padding-left: 5px;padding-right: 5px;">

Exposure (count) x Female

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

0.21

</td>

<td style="padding-left: 5px;padding-right: 5px;">

0.21

</td>

<td style="padding-left: 5px;padding-right: 5px;">

0.21

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

</tr>

<tr>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.32)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.32)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.32)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

</tr>

<tr>

<td style="padding-left: 5px;padding-right: 5px;">

Num. of Event

</td>

<td style="padding-left: 5px;padding-right: 5px;">

\-0.05<sup>\*\*\*</sup>

</td>

<td style="padding-left: 5px;padding-right: 5px;">

\-0.05<sup>\*\*\*</sup>

</td>

<td style="padding-left: 5px;padding-right: 5px;">

\-0.05<sup>\*\*\*</sup>

</td>

<td style="padding-left: 5px;padding-right: 5px;">

\-0.05<sup>\*\*\*</sup>

</td>

<td style="padding-left: 5px;padding-right: 5px;">

\-0.05<sup>\*\*\*</sup>

</td>

<td style="padding-left: 5px;padding-right: 5px;">

\-0.05<sup>\*\*\*</sup>

</td>

<td style="padding-left: 5px;padding-right: 5px;">

\-0.05<sup>\*\*\*</sup>

</td>

<td style="padding-left: 5px;padding-right: 5px;">

\-0.05<sup>\*\*\*</sup>

</td>

<td style="padding-left: 5px;padding-right: 5px;">

\-0.05<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.01)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.01)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.01)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.01)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.01)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.01)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.01)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.01)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.01)

</td>

</tr>

<tr>

<td style="padding-left: 5px;padding-right: 5px;">

N. Officers Involved

</td>

<td style="padding-left: 5px;padding-right: 5px;">

\-0.00

</td>

<td style="padding-left: 5px;padding-right: 5px;">

\-0.00

</td>

<td style="padding-left: 5px;padding-right: 5px;">

\-0.04

</td>

<td style="padding-left: 5px;padding-right: 5px;">

\-0.04

</td>

<td style="padding-left: 5px;padding-right: 5px;">

\-0.04

</td>

<td style="padding-left: 5px;padding-right: 5px;">

\-0.05

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

</tr>

<tr>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.04)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.04)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.04)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.04)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.04)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.04)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

</tr>

<tr>

<td style="padding-left: 5px;padding-right: 5px;">

Tenure (t)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

\-0.02<sup>\*</sup>

</td>

<td style="padding-left: 5px;padding-right: 5px;">

\-0.02<sup>\*</sup>

</td>

<td style="padding-left: 5px;padding-right: 5px;">

\-0.02

</td>

<td style="padding-left: 5px;padding-right: 5px;">

\-0.02

</td>

<td style="padding-left: 5px;padding-right: 5px;">

\-0.02

</td>

<td style="padding-left: 5px;padding-right: 5px;">

\-0.02

</td>

<td style="padding-left: 5px;padding-right: 5px;">

\-0.02<sup>\*</sup>

</td>

<td style="padding-left: 5px;padding-right: 5px;">

\-0.02<sup>\*</sup>

</td>

<td style="padding-left: 5px;padding-right: 5px;">

\-0.02

</td>

</tr>

<tr>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.01)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.01)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.01)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.01)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.01)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.01)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.01)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.01)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.01)

</td>

</tr>

<tr>

<td style="padding-left: 5px;padding-right: 5px;">

N. Subjects

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

1.58<sup>\*\*\*</sup>

</td>

<td style="padding-left: 5px;padding-right: 5px;">

1.58<sup>\*\*\*</sup>

</td>

<td style="padding-left: 5px;padding-right: 5px;">

1.58<sup>\*\*\*</sup>

</td>

<td style="padding-left: 5px;padding-right: 5px;">

1.58<sup>\*\*\*</sup>

</td>

<td style="padding-left: 5px;padding-right: 5px;">

1.57<sup>\*\*\*</sup>

</td>

<td style="padding-left: 5px;padding-right: 5px;">

1.50<sup>\*\*\*</sup>

</td>

<td style="padding-left: 5px;padding-right: 5px;">

2.06<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.12)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.12)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.12)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.12)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.12)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.12)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.15)

</td>

</tr>

<tr>

<td style="padding-left: 5px;padding-right: 5px;">

Peer experience (avg. years)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

0.00

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

</tr>

<tr>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.01)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

</tr>

<tr>

<td style="padding-left: 5px;padding-right: 5px;">

Female (yes/no)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

\-0.26

</td>

<td style="padding-left: 5px;padding-right: 5px;">

\-0.31

</td>

<td style="padding-left: 5px;padding-right: 5px;">

\-0.38

</td>

<td style="padding-left: 5px;padding-right: 5px;">

\-0.51

</td>

<td style="padding-left: 5px;padding-right: 5px;">

\-0.51

</td>

<td style="padding-left: 5px;padding-right: 5px;">

\-0.52

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

</tr>

<tr>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.28)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.41)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.44)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.40)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.40)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.40)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

</tr>

<tr>

<td style="padding-left: 5px;padding-right: 5px;">

Black (yes/no)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

1.24

</td>

<td style="padding-left: 5px;padding-right: 5px;">

1.24

</td>

<td style="padding-left: 5px;padding-right: 5px;">

1.48

</td>

<td style="padding-left: 5px;padding-right: 5px;">

1.49

</td>

<td style="padding-left: 5px;padding-right: 5px;">

1.49

</td>

<td style="padding-left: 5px;padding-right: 5px;">

1.49

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

</tr>

<tr>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(1.09)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(1.09)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(1.13)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(1.14)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(1.14)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(1.14)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

</tr>

<tr>

<td style="padding-left: 5px;padding-right: 5px;">

Hispanic (yes/no)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

1.44

</td>

<td style="padding-left: 5px;padding-right: 5px;">

1.44

</td>

<td style="padding-left: 5px;padding-right: 5px;">

1.59

</td>

<td style="padding-left: 5px;padding-right: 5px;">

1.61

</td>

<td style="padding-left: 5px;padding-right: 5px;">

1.61

</td>

<td style="padding-left: 5px;padding-right: 5px;">

1.61

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

</tr>

<tr>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(1.17)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(1.17)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(1.22)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(1.22)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(1.22)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(1.22)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

</tr>

<tr>

<td style="padding-left: 5px;padding-right: 5px;">

White (yes/no)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

1.38

</td>

<td style="padding-left: 5px;padding-right: 5px;">

1.38

</td>

<td style="padding-left: 5px;padding-right: 5px;">

1.59

</td>

<td style="padding-left: 5px;padding-right: 5px;">

1.60

</td>

<td style="padding-left: 5px;padding-right: 5px;">

1.60

</td>

<td style="padding-left: 5px;padding-right: 5px;">

1.60

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

</tr>

<tr>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(1.06)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(1.06)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(1.11)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(1.11)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(1.11)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(1.11)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

</tr>

<tr>

<td style="padding-left: 5px;padding-right: 5px;">

is police off. (yes/no)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

0.12

</td>

<td style="padding-left: 5px;padding-right: 5px;">

0.13

</td>

<td style="padding-left: 5px;padding-right: 5px;">

0.13

</td>

<td style="padding-left: 5px;padding-right: 5px;">

0.13

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

</tr>

<tr>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.16)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.16)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.16)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.16)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

</tr>

<tr>

<td style="padding-left: 5px;padding-right: 5px;">

is law enforcement off. (yes/no)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

\-2.41<sup>\*</sup>

</td>

<td style="padding-left: 5px;padding-right: 5px;">

\-2.40<sup>\*</sup>

</td>

<td style="padding-left: 5px;padding-right: 5px;">

\-2.40<sup>\*</sup>

</td>

<td style="padding-left: 5px;padding-right: 5px;">

\-2.42<sup>\*</sup>

</td>

<td style="padding-left: 5px;padding-right: 5px;">

\-2.49<sup>\*</sup>

</td>

<td style="padding-left: 5px;padding-right: 5px;">

\-2.31<sup>\*</sup>

</td>

<td style="padding-left: 5px;padding-right: 5px;">

\-2.20<sup>\*</sup>

</td>

</tr>

<tr>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(1.07)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(1.07)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(1.07)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(1.07)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(1.05)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(1.02)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(1.02)

</td>

</tr>

<tr>

<td style="padding-left: 5px;padding-right: 5px;">

Violent or Weapon related (yes/no)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

2.20<sup>\*\*\*</sup>

</td>

<td style="padding-left: 5px;padding-right: 5px;">

2.20<sup>\*\*\*</sup>

</td>

<td style="padding-left: 5px;padding-right: 5px;">

2.60<sup>\*\*\*</sup>

</td>

<td style="padding-left: 5px;padding-right: 5px;">

2.61<sup>\*\*\*</sup>

</td>

<td style="padding-left: 5px;padding-right: 5px;">

2.61<sup>\*\*\*</sup>

</td>

<td style="padding-left: 5px;padding-right: 5px;">

2.61<sup>\*\*\*</sup>

</td>

<td style="padding-left: 5px;padding-right: 5px;">

2.59<sup>\*\*\*</sup>

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

</tr>

<tr>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.29)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.29)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.32)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.32)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.32)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.32)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.32)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

</tr>

<tr>

<td style="padding-left: 5px;padding-right: 5px;">

N Officers/N Subjects

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

0.00<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.00)

</td>

</tr>

<tr>

<td style="padding-left: 5px;padding-right: 5px;">

N Previous Pointed

</td>

<td style="padding-left: 5px;padding-right: 5px;">

0.21<sup>\*\*\*</sup>

</td>

<td style="padding-left: 5px;padding-right: 5px;">

0.21<sup>\*\*\*</sup>

</td>

<td style="padding-left: 5px;padding-right: 5px;">

0.22<sup>\*\*\*</sup>

</td>

<td style="padding-left: 5px;padding-right: 5px;">

0.22<sup>\*\*\*</sup>

</td>

<td style="padding-left: 5px;padding-right: 5px;">

0.22<sup>\*\*\*</sup>

</td>

<td style="padding-left: 5px;padding-right: 5px;">

0.22<sup>\*\*\*</sup>

</td>

<td style="padding-left: 5px;padding-right: 5px;">

0.22<sup>\*\*\*</sup>

</td>

<td style="padding-left: 5px;padding-right: 5px;">

0.22<sup>\*\*\*</sup>

</td>

<td style="padding-left: 5px;padding-right: 5px;">

0.22<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.03)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.03)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.03)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.03)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.03)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.03)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.03)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.03)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.03)

</td>

</tr>

<tr style="border-top: 1px solid #000000;">

<td style="padding-left: 5px;padding-right: 5px;">

Pseudo R2

</td>

<td style="padding-left: 5px;padding-right: 5px;">

0.08

</td>

<td style="padding-left: 5px;padding-right: 5px;">

0.08

</td>

<td style="padding-left: 5px;padding-right: 5px;">

0.15

</td>

<td style="padding-left: 5px;padding-right: 5px;">

0.15

</td>

<td style="padding-left: 5px;padding-right: 5px;">

0.15

</td>

<td style="padding-left: 5px;padding-right: 5px;">

0.15

</td>

<td style="padding-left: 5px;padding-right: 5px;">

0.15

</td>

<td style="padding-left: 5px;padding-right: 5px;">

0.12

</td>

<td style="padding-left: 5px;padding-right: 5px;">

0.14

</td>

</tr>

<tr>

<td style="padding-left: 5px;padding-right: 5px;">

AUC

</td>

<td style="padding-left: 5px;padding-right: 5px;">

0.69

</td>

<td style="padding-left: 5px;padding-right: 5px;">

0.69

</td>

<td style="padding-left: 5px;padding-right: 5px;">

0.74

</td>

<td style="padding-left: 5px;padding-right: 5px;">

0.74

</td>

<td style="padding-left: 5px;padding-right: 5px;">

0.74

</td>

<td style="padding-left: 5px;padding-right: 5px;">

0.74

</td>

<td style="padding-left: 5px;padding-right: 5px;">

0.74

</td>

<td style="padding-left: 5px;padding-right: 5px;">

0.73

</td>

<td style="padding-left: 5px;padding-right: 5px;">

0.74

</td>

</tr>

<tr>

<td style="padding-left: 5px;padding-right: 5px;">

AIC

</td>

<td style="padding-left: 5px;padding-right: 5px;">

3065.62

</td>

<td style="padding-left: 5px;padding-right: 5px;">

3067.59

</td>

<td style="padding-left: 5px;padding-right: 5px;">

2855.87

</td>

<td style="padding-left: 5px;padding-right: 5px;">

2853.83

</td>

<td style="padding-left: 5px;padding-right: 5px;">

2853.83

</td>

<td style="padding-left: 5px;padding-right: 5px;">

2855.57

</td>

<td style="padding-left: 5px;padding-right: 5px;">

2851.50

</td>

<td style="padding-left: 5px;padding-right: 5px;">

2926.16

</td>

<td style="padding-left: 5px;padding-right: 5px;">

2881.37

</td>

</tr>

<tr>

<td style="padding-left: 5px;padding-right: 5px;">

BIC

</td>

<td style="padding-left: 5px;padding-right: 5px;">

3157.66

</td>

<td style="padding-left: 5px;padding-right: 5px;">

3165.77

</td>

<td style="padding-left: 5px;padding-right: 5px;">

2972.44

</td>

<td style="padding-left: 5px;padding-right: 5px;">

2976.55

</td>

<td style="padding-left: 5px;padding-right: 5px;">

2976.55

</td>

<td style="padding-left: 5px;padding-right: 5px;">

2984.42

</td>

<td style="padding-left: 5px;padding-right: 5px;">

2931.29

</td>

<td style="padding-left: 5px;padding-right: 5px;">

2999.81

</td>

<td style="padding-left: 5px;padding-right: 5px;">

2961.16

</td>

</tr>

<tr>

<td style="padding-left: 5px;padding-right: 5px;">

Log Likelihood

</td>

<td style="padding-left: 5px;padding-right: 5px;">

\-1517.81

</td>

<td style="padding-left: 5px;padding-right: 5px;">

\-1517.80

</td>

<td style="padding-left: 5px;padding-right: 5px;">

\-1408.93

</td>

<td style="padding-left: 5px;padding-right: 5px;">

\-1406.92

</td>

<td style="padding-left: 5px;padding-right: 5px;">

\-1406.92

</td>

<td style="padding-left: 5px;padding-right: 5px;">

\-1406.78

</td>

<td style="padding-left: 5px;padding-right: 5px;">

\-1412.75

</td>

<td style="padding-left: 5px;padding-right: 5px;">

\-1451.08

</td>

<td style="padding-left: 5px;padding-right: 5px;">

\-1427.69

</td>

</tr>

<tr>

<td style="padding-left: 5px;padding-right: 5px;">

Deviance

</td>

<td style="padding-left: 5px;padding-right: 5px;">

3035.62

</td>

<td style="padding-left: 5px;padding-right: 5px;">

3035.59

</td>

<td style="padding-left: 5px;padding-right: 5px;">

2817.87

</td>

<td style="padding-left: 5px;padding-right: 5px;">

2813.83

</td>

<td style="padding-left: 5px;padding-right: 5px;">

2813.83

</td>

<td style="padding-left: 5px;padding-right: 5px;">

2813.57

</td>

<td style="padding-left: 5px;padding-right: 5px;">

2825.50

</td>

<td style="padding-left: 5px;padding-right: 5px;">

2902.16

</td>

<td style="padding-left: 5px;padding-right: 5px;">

2855.37

</td>

</tr>

<tr style="border-bottom: 2px solid #000000;">

<td style="padding-left: 5px;padding-right: 5px;">

Num. obs.

</td>

<td style="padding-left: 5px;padding-right: 5px;">

3415

</td>

<td style="padding-left: 5px;padding-right: 5px;">

3415

</td>

<td style="padding-left: 5px;padding-right: 5px;">

3414

</td>

<td style="padding-left: 5px;padding-right: 5px;">

3414

</td>

<td style="padding-left: 5px;padding-right: 5px;">

3414

</td>

<td style="padding-left: 5px;padding-right: 5px;">

3414

</td>

<td style="padding-left: 5px;padding-right: 5px;">

3420

</td>

<td style="padding-left: 5px;padding-right: 5px;">

3420

</td>

<td style="padding-left: 5px;padding-right: 5px;">

3420

</td>

</tr>

</tbody>

<tfoot>

<tr>

<td style="font-size: 0.8em;" colspan="10">

<sup>***</sup>p \< 0.001; <sup>**</sup>p \< 0.01; <sup>*</sup>p \< 0.05

</td>

</tr>

</tfoot>

</table>

``` r
# screenreg(models, custom.coef.map = cmap,custom.gof.rows = list("Pseudo R2" = pr2))
```

Throw in time of the day.

Time of the year.

See if we can get info about the area.

Broker in misconduct network.
