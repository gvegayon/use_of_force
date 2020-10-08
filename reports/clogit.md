Conditional Logit
================

Descriptive statistics

``` r
desc <- readRDS("../models/clogit_descriptive_stats.rds")
knitr::kable(desc, format = "html")
```

<table>

<tbody>

<tr>

<td style="text-align:left;">

Firearm pointed

</td>

<td style="text-align:left;">

Yes

</td>

<td style="text-align:left;">

No

</td>

</tr>

<tr>

<td style="text-align:left;">

Sex = Male

</td>

<td style="text-align:left;">

98.31% (0.13)

</td>

<td style="text-align:left;">

97.70% (0.15)

</td>

</tr>

<tr>

<td style="text-align:left;">

Years of Exp.

</td>

<td style="text-align:left;">

8.30 (6.43)

</td>

<td style="text-align:left;">

8.28 (5.85)

</td>

</tr>

<tr>

<td style="text-align:left;">

Exposure (direct)

</td>

<td style="text-align:left;">

6.00% (0.24)

</td>

<td style="text-align:left;">

8.07% (0.27)

</td>

</tr>

<tr>

<td style="text-align:left;">

Exposure (indirect)

</td>

<td style="text-align:left;">

17.25% (0.38)

</td>

<td style="text-align:left;">

25.61% (0.44)

</td>

</tr>

<tr>

<td style="text-align:left;">

Race = White

</td>

<td style="text-align:left;">

0.71 (0.45)

</td>

<td style="text-align:left;">

0.74 (0.44)

</td>

</tr>

<tr>

<td style="text-align:left;">

Police Officer

</td>

<td style="text-align:left;">

350.00 (0.33)

</td>

<td style="text-align:left;">

255.00 (0.31)

</td>

</tr>

<tr>

<td style="text-align:left;">

N Officers

</td>

<td style="text-align:left;">

3.44 (1.52)

</td>

<td style="text-align:left;">

3.22 (1.59)

</td>

</tr>

<tr>

<td style="text-align:left;">

N Events

</td>

<td style="text-align:left;">

3.44 (1.52)

</td>

<td style="text-align:left;">

3.22 (1.59)

</td>

</tr>

<tr>

<td style="text-align:left;">

N observations

</td>

<td style="text-align:left;">

400

</td>

<td style="text-align:left;">

285

</td>

</tr>

</tbody>

</table>

Models

``` r
models <- readRDS("../models/clogit.rds")
htmlreg(
  l = models$models,
  custom.coef.map = models$labels,
  stars = c(.01, .05, .1),
  caption = paste(
    "Conditional Logit Estimates. Standard errors may be invalid due to",
    "possible contagion (or anti-contagion) effects.",
    "The last column corresponds to a logistic regression."
  ))
```

<table class="texreg" style="margin: 10px auto;border-collapse: collapse;border-spacing: 0px;caption-side: bottom;color: #000000;border-top: 2px solid #000000;">

<caption>

Conditional Logit Estimates. Standard errors may be invalid due to
possible contagion (or anti-contagion) effects. The last column
corresponds to a logistic regression.

</caption>

<thead>

<tr>

<th style="padding-left: 5px;padding-right: 5px;">

 

</th>

<th style="padding-left: 5px;padding-right: 5px;">

Model 1

</th>

<th style="padding-left: 5px;padding-right: 5px;">

Model 2

</th>

<th style="padding-left: 5px;padding-right: 5px;">

Model 3

</th>

<th style="padding-left: 5px;padding-right: 5px;">

Model 4

</th>

<th style="padding-left: 5px;padding-right: 5px;">

Model 5

</th>

<th style="padding-left: 5px;padding-right: 5px;">

Model 6

</th>

<th style="padding-left: 5px;padding-right: 5px;">

Model 7

</th>

</tr>

</thead>

<tbody>

<tr style="border-top: 1px solid #000000;">

<td style="padding-left: 5px;padding-right: 5px;">

Exposure (direct)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

0.04

</td>

<td style="padding-left: 5px;padding-right: 5px;">

\-0.01

</td>

<td style="padding-left: 5px;padding-right: 5px;">

\-0.00

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

(0.39)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.39)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.39)

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

Exposure (indirect)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

0.52<sup>\*</sup>

</td>

<td style="padding-left: 5px;padding-right: 5px;">

0.53<sup>\*</sup>

</td>

<td style="padding-left: 5px;padding-right: 5px;">

0.54<sup>\*\*</sup>

</td>

<td style="padding-left: 5px;padding-right: 5px;">

0.87<sup>\*\*</sup>

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

(0.27)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.27)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.27)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.35)

</td>

</tr>

<tr>

<td style="padding-left: 5px;padding-right: 5px;">

Sex = Male

</td>

<td style="padding-left: 5px;padding-right: 5px;">

\-0.42

</td>

<td style="padding-left: 5px;padding-right: 5px;">

\-0.44

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

\-0.30

</td>

<td style="padding-left: 5px;padding-right: 5px;">

\-0.29

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

(0.72)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.75)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.71)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.74)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

</tr>

<tr>

<td style="padding-left: 5px;padding-right: 5px;">

N of Events

</td>

<td style="padding-left: 5px;padding-right: 5px;">

\-0.02

</td>

<td style="padding-left: 5px;padding-right: 5px;">

\-0.03<sup>\*</sup>

</td>

<td style="padding-left: 5px;padding-right: 5px;">

\-0.04<sup>\*</sup>

</td>

<td style="padding-left: 5px;padding-right: 5px;">

\-0.02

</td>

<td style="padding-left: 5px;padding-right: 5px;">

\-0.03

</td>

<td style="padding-left: 5px;padding-right: 5px;">

\-0.03<sup>\*</sup>

</td>

<td style="padding-left: 5px;padding-right: 5px;">

\-0.05<sup>\*\*</sup>

</td>

</tr>

<tr>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.02)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.02)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.02)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.02)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.02)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.02)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.02)

</td>

</tr>

<tr>

<td style="padding-left: 5px;padding-right: 5px;">

Years of Exp.

</td>

<td style="padding-left: 5px;padding-right: 5px;">

0.01

</td>

<td style="padding-left: 5px;padding-right: 5px;">

0.15<sup>\*\*\*</sup>

</td>

<td style="padding-left: 5px;padding-right: 5px;">

0.15<sup>\*\*\*</sup>

</td>

<td style="padding-left: 5px;padding-right: 5px;">

0.01

</td>

<td style="padding-left: 5px;padding-right: 5px;">

0.15<sup>\*\*\*</sup>

</td>

<td style="padding-left: 5px;padding-right: 5px;">

0.16<sup>\*\*\*</sup>

</td>

<td style="padding-left: 5px;padding-right: 5px;">

0.26<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.02)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.06)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.06)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.02)

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

</tr>

<tr>

<td style="padding-left: 5px;padding-right: 5px;">

Years of Exp.^2

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

\-0.01<sup>\*\*</sup>

</td>

<td style="padding-left: 5px;padding-right: 5px;">

\-0.01<sup>\*\*\*</sup>

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

\-0.01<sup>\*\*</sup>

</td>

<td style="padding-left: 5px;padding-right: 5px;">

\-0.01<sup>\*\*\*</sup>

</td>

<td style="padding-left: 5px;padding-right: 5px;">

\-0.01<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.00)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.00)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.00)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.00)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.00)

</td>

</tr>

<tr>

<td style="padding-left: 5px;padding-right: 5px;">

Police Officer

</td>

<td style="padding-left: 5px;padding-right: 5px;">

0.18

</td>

<td style="padding-left: 5px;padding-right: 5px;">

0.11

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

0.11

</td>

<td style="padding-left: 5px;padding-right: 5px;">

0.03

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

(0.34)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.34)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.34)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

(0.34)

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

</tr>

<tr style="border-top: 1px solid #000000;">

<td style="padding-left: 5px;padding-right: 5px;">

AIC

</td>

<td style="padding-left: 5px;padding-right: 5px;">

363.00

</td>

<td style="padding-left: 5px;padding-right: 5px;">

358.24

</td>

<td style="padding-left: 5px;padding-right: 5px;">

354.49

</td>

<td style="padding-left: 5px;padding-right: 5px;">

359.36

</td>

<td style="padding-left: 5px;padding-right: 5px;">

354.43

</td>

<td style="padding-left: 5px;padding-right: 5px;">

350.50

</td>

<td style="padding-left: 5px;padding-right: 5px;">

1137.02

</td>

</tr>

<tr>

<td style="padding-left: 5px;padding-right: 5px;">

R<sup>2</sup>

</td>

<td style="padding-left: 5px;padding-right: 5px;">

0.01

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

0.02

</td>

<td style="padding-left: 5px;padding-right: 5px;">

0.02

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

</tr>

<tr>

<td style="padding-left: 5px;padding-right: 5px;">

Max. R<sup>2</sup>

</td>

<td style="padding-left: 5px;padding-right: 5px;">

0.45

</td>

<td style="padding-left: 5px;padding-right: 5px;">

0.45

</td>

<td style="padding-left: 5px;padding-right: 5px;">

0.45

</td>

<td style="padding-left: 5px;padding-right: 5px;">

0.45

</td>

<td style="padding-left: 5px;padding-right: 5px;">

0.45

</td>

<td style="padding-left: 5px;padding-right: 5px;">

0.45

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

</tr>

<tr>

<td style="padding-left: 5px;padding-right: 5px;">

Num. events

</td>

<td style="padding-left: 5px;padding-right: 5px;">

254

</td>

<td style="padding-left: 5px;padding-right: 5px;">

254

</td>

<td style="padding-left: 5px;padding-right: 5px;">

254

</td>

<td style="padding-left: 5px;padding-right: 5px;">

254

</td>

<td style="padding-left: 5px;padding-right: 5px;">

254

</td>

<td style="padding-left: 5px;padding-right: 5px;">

254

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

</tr>

<tr>

<td style="padding-left: 5px;padding-right: 5px;">

Num. obs.

</td>

<td style="padding-left: 5px;padding-right: 5px;">

597

</td>

<td style="padding-left: 5px;padding-right: 5px;">

597

</td>

<td style="padding-left: 5px;padding-right: 5px;">

598

</td>

<td style="padding-left: 5px;padding-right: 5px;">

597

</td>

<td style="padding-left: 5px;padding-right: 5px;">

597

</td>

<td style="padding-left: 5px;padding-right: 5px;">

598

</td>

<td style="padding-left: 5px;padding-right: 5px;">

598

</td>

</tr>

<tr>

<td style="padding-left: 5px;padding-right: 5px;">

Missings

</td>

<td style="padding-left: 5px;padding-right: 5px;">

88

</td>

<td style="padding-left: 5px;padding-right: 5px;">

88

</td>

<td style="padding-left: 5px;padding-right: 5px;">

87

</td>

<td style="padding-left: 5px;padding-right: 5px;">

88

</td>

<td style="padding-left: 5px;padding-right: 5px;">

88

</td>

<td style="padding-left: 5px;padding-right: 5px;">

87

</td>

<td style="padding-left: 5px;padding-right: 5px;">

 

</td>

</tr>

<tr>

<td style="padding-left: 5px;padding-right: 5px;">

BIC

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

2279.35

</td>

</tr>

<tr>

<td style="padding-left: 5px;padding-right: 5px;">

Log Likelihood

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

\-308.51

</td>

</tr>

<tr style="border-bottom: 2px solid #000000;">

<td style="padding-left: 5px;padding-right: 5px;">

Deviance

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

617.02

</td>

</tr>

</tbody>

<tfoot>

<tr>

<td style="font-size: 0.8em;" colspan="8">

<sup>***</sup>p \< 0.01; <sup>**</sup>p \< 0.05; <sup>*</sup>p \< 0.1

</td>

</tr>

</tfoot>

</table>
