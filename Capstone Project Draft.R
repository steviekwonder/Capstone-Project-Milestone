college_info <- read.csv("~/Desktop/GRE Vocab Data Analytics/Most-Recent-Cohorts-Scorecard-Elements.csv")

# PREDDEG- type of degree that institution primarily awards
# CONTROL - whether school is public, private, private non profit, private for profit
# HBCU - historically black college or university
# PBI - Predominantly black institutions
# ANNHI - Alaska native American serving populations
# TRIBAL
# AANAPI
# HSI - Hispanic serving institutions
# NANTI - Native american non- tribal institutions. With the exception of a small number of HBCU’s, identified
#   institutions are classified by their eligibility to apply for the
#   Department’s minority-serving institution programgrants. 
# RELAFFIL - religious affiliation
# SATVR25 - 25th percentile of SAT reading, also SATVR75
# SATMT... - math
# SATWR - writing
# SATMID - midpoint
# however, note that institutions do not report
#   those midpoints to IPEDS. SAT and ACT data are available from 2001-02
#   on; however, SAT writing scores are available only from 2006-07 on, and
#   ACT writing data are available only from 2008-09 on.
# SAT_AVG - probably SAT average? SAT_AVG_ALL ...?
# PCIP...The Classification of Instructional Programs (CIP) provides a structure in
#   which to track and report in fields of study. Two types of program data
#   are included in these data. The first set (PCIP[01-54]) provide the
#   percentage of degrees awarded in each two-digit CIP code field of study.
#   The second set (CIP[01-54][CERT1/CERT2/ASSOC/BACHL/CERT4])
#   identifies whether the institution offers the program, at what level, and
#   whether the institution offers the full program and level through
#   distance education12. It is calculated from counts of awards made in
#   each CIP. CIP elements within each Scorecard data file are derived from
#   the IPEDS Completions component. Reported awards cover the 12
#   month period ending June 30 prior to the IPEDS collection year. 
# UGDS - number of degree/certificate seeking undergraduates enrolled in the fall, as reported in IPEDS Fall enrollment component
# UGDS_2MOR - two or more races
# UGDS_NRA - non-residential aliens
# UGDS_UNKN - race unknown
# PPTUG_EF - proportion of degree seeking students enrolled in part time job in fall term
# NPT4_PUB- average net price for public colleges of institutions PRIV for private
# NPT41_PUB - average net price for lower quintile income bracket 0-$30000, 2,3,4,5
# DISTANCEONLY
# PCTPELL - pulled from IPEDS Student Fianancial Aid, shows the share of undergraduate students who received Pell grants
# RET_FT4, RET_PT4, RET_FTL4, RET_PTL4 - Available through the IPEDS Fall Enrollment component, retention rate
#   identifies (separately) the share of full-time and part-time studentsin
#   the prior year, at four-year (RET_FT4 and RET_PT4) and less-than-fouryear
#   institutions (RET_FTL4 and RET_PTL4), who return to the institution
#   after the first year. For four-year institutions, the retention rate covers
#   bachelor’s degree-seeking students only; at less-than-four-year
#   institutions, it covers all degree/certificate-seeking students. Data are
#   not available prior to 2004-05.
# PCTFLOAN - This element (PCTFLOAN), as reported in the IPEDS Student Financial
#   Aid (SFA) component, shows the share of undergraduate students who
#   received federal loans in a given year. 
# UG25ABV - student ages 25 and older
# MD_EARN_WNE_P10 - Mean (MN_EARN_WNE_P*) and median (MD_EARN_WNE_P*)
#   earnings are for the institutional aggregate of all federally aided
#   students who enroll in an institution each year and who are
#   employed but not enrolled.
# GT_25K_P6 - This measure describes the fraction of former students earning
#   over $25,000 (gt_25k_p*). It is available for each year from six
#   to 10 years after entering the institution. 
# GRAD_DEBT_MDN_SUPP, GRAD_DEBT_MDN10YR_SUPP- At institutions where large numbers of students withdraw before
#   completion, a lower median debt level could simply reflect the lack of
#   time that a typical student spends at the institution. Therefore, the
#   Department usesthe typical debt level for students who complete
#   (GRAD_DEBT_MDN_SUPP or GRAD_DEBT_MDN10YR_SUPP for the debt
#   level expressed in monthly payments27) on the consumer website
# RPY_3YR_RT_SUPP-  The repayment rates are produced in
#   rolling two-year averages to reduce variability from year to
#   year; and the three-year repayment rate, which isincluded on
#   the consumer tool, is suppressed for institutions with fewer
#   than 30 borrowers in the two cohorts to produce more stable
#   measures (RPY_3YR_RT_SUPP).
# C150_L4_POOLED_SUPP, C150_4_POOLED_SUPP- 

## 1) Cut down data frame to variables you need for exploratory analysis. 
  library(dplyr)
  college_edit <- college_info %>%
    select(INSTNM, STABBR, CONTROL, PCTFLOAN, RELAFFIL, GT_25K_P6, NPT4_PUB, NPT41_PUB, NPT4_PRIV, NPT41_PRIV)

  college_edit$CONTROL <- factor(college_edit$CONTROL)
  levels(college_edit$CONTROL) <- c("public", "non_priv", "for_priv")
  
  
## 2) How well do schools offer loans based on whether it is a public or private university? 
  library(ggplot2)
  college_edit$PCTFLOAN <- as.numeric(as.character(college_edit$PCTFLOAN))
  ggplot(college_edit, aes(x = INSTNM, y = PCTFLOAN, col = CONTROL)) + geom_jitter(alpha = 0.5) + facet_grid(.~CONTROL) 

  # Average percentage of undergraduates at certain university recieving aid (public, private for-profit, private non-profit)
    college_public <- subset(college_edit, CONTROL == "public")
    loan <- college_public$PCTFLOAN
    loan <- as.numeric(as.character(loan))
    avgLoan_public <- mean(loan, na.rm = TRUE)  # around 32% of an undergrad population of a public university has financial aid

    college_forPriv <- subset(college_edit, CONTROL == "for_priv")
    loanTwo <- college_forPriv$PCTFLOAN
    loanTwo <- as.numeric(as.character(loanTwo))
    avgLoan_for_priv <- mean(loanTwo, na.rm = TRUE) # around 62% of an undergrad population of a private for-profit university has financial aid

    college_nonPriv <- subset(college_edit, CONTROL == "non_priv")
    loanThree <- college_nonPriv$PCTFLOAN
    loanThree <- as.numeric(as.character(loanThree))
    avgLoan_non_priv <- mean(loanThree, na.rm = TRUE) # around 32% of an undergrad population of a private non-profit university has financial aid
    
## 3) The average net price of institutions for those of lower income bracket of 0-$30000. Let's observe the frequency  
## distributions and the average net price for public, private non-profit and private for-profit universities.
  college_public$NPT4_PUB <- as.numeric(as.character(college_public$NPT4_PUB))
  college_public$NPT41_PUB <- as.numeric(as.character(college_public$NPT41_PUB))
  qplot(x = NPT4_PUB, data = college_public, geom = "freqpoly") # net price frequency distribution for public universities
  netPricePublic <- mean(college_public$NPT4_PUB, na.rm = TRUE) # average net price for public universities
    
  college_nonPriv$NPT4_PRIV <- as.numeric(as.character(college_nonPriv$NPT4_PRIV))
  college_nonPriv$NPT41_PRIV <- as.numeric(as.character(college_nonPriv$NPT41_PRIV))
  qplot(x = NPT4_PRIV, data = college_nonPriv, geom = "freqpoly") # net price frequency distribution for private nonprofit universities
  netPriceNonPriv <- mean(college_nonPriv$NPT4_PRIV, na.rm = TRUE) # average net price for private nonprofit universities
    
  college_forPriv$NPT4_PRIV <- as.numeric(as.character(college_forPriv$NPT4_PRIV))
  college_forPriv$NPT41_PRIV <- as.numeric(as.character(college_forPriv$NPT41_PRIV))
  qplot(x = NPT4_PRIV, data = college_forPriv, geom = "freqpoly") # net price frequency distribution for private for-profit universities
  netPriceForPriv <- mean(college_forPriv$NPT4_PRIV, na.rm = TRUE)  # average net price for private for-profit universities

## 4) What about earning annual salaries above $25,000 after graduation?  geom_freqpoly() 
  college_edit$GT_25K_P6 <- as.numeric(as.character(college_edit$GT_25K_P6))
  ggplot(college_edit, aes(x = GT_25K_P6)) + geom_freqpoly() + facet_grid(.~CONTROL)

## 5) Does religious affiliation have anything to do with whether a higher percentage of students get aid? NOT VERY USEFUL
  college_edit$RELAFFIL <- as.numeric(as.character(college_edit$RELAFFIL))
  ggplot(college_edit, aes(x = RELAFFIL, y = PCTFLOAN, col = CONTROL)) + geom_jitter(alpha = 0.5) + facet_grid(.~CONTROL)
  ggplot(college_edit, aes(x = RELAFFIL, y = GT_25K_P6, col = CONTROL)) + geom_jitter() + facet_grid(.~CONTROL)



