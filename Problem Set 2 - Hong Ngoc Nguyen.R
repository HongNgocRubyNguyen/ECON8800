# this code is for the problem set of topic in macro and Labor course
# code is beased on the following two papers
# 1. Acemoglu, D. and Autor, D. (2011). Handbook of Labor Economics, 4b:1043–1171
# 2. Katz, L. F. and Murphy, K. M. (1992). The Quarterly Journal of Economics
# clear environment
rm(list = ls())
# laad libaries
install.packages("lubridate")
library(tidyverse)
library(lubridate)
# read the downloaded
data_00 <- read_fwf(file="data_00.dat",                     
                    fwf_cols(year      = c(1, 4),
                             serial    = c(5,9),
                             month     = c(10,11),
                            # hwtfinl   = c(12,21),
                             cpsid     = c(12,25),
                             asecflag  = c(26,26),
                             hflag     = c(27,27),
                             asecwth   = c(28,37),
                             pernum    = c(38,39),
                            # wtfinl    = c(50,63),
                             cpsidp    = c(40,53),
                             asecwt    = c(54,63),
                             age       = c(64,65),
                             sex       = c(66,66),
                             race      = c(67,69),
                             educ      = c(70,72),
                             schlcoll  = c(73,73),
                             indly     = c(74,77),
                             classwly  = c(78,79),
                             wkswork1  = c(80,81),
                             wkswork2  = c(82,82),
                             fullpart  = c(83,83),
                             incwage   = c(84,90)),
                    col_types = cols(year       = "i",
                                     serial     = "n",
                                     month      = "i",
                                   #  hwtfinl    = "d",
                                     cpsid      = "d",
                                     asecflag   = "i",
                                     hflag      = "i",
                                     asecwth    = "d",
                                     pernum     = "i",
                                    # wtfinl     = "d",
                                     cpsidp     = "d",
                                     asecwt     = "d",                    
                                     age        = "i",
                                     sex        = "i",
                                     race       = "i",
                                     educ       = "i",
                                     schlcoll   = "i",
                                     indly      = "i",
                                     classwly   = "i",
                                     wkswork1   = "i",
                                     wkswork2   = "i",
                                     fullpart   = "i",
                                     incwage    = "n"))
#data_00$hwtfinl = data_00$hwtfinl/10000
#data_00$wtfinl = data_00$wtfinl/10000
data_00$asecwt = data_00$asecwt/10000
# merge cpi data (see Acemoglu and Autor's Data Appendix)
data_cpi <- read_csv(file = "data_cpi.csv", col_names = c("year","cpi"), col_types=cols(year = "D", cpi = "d"), skip = 1)
data_cpi$year <- year(data_cpi$year)
data_cpi <- data_cpi %>%
  mutate(price_1982 = ifelse(year == 1982, cpi, 0)) %>% # the base year is 1982 (see Acemoglu and Autor's Data Appendix)
  mutate(price_1982 = max(price_1982)) %>%
  mutate(cpi = cpi/price_1982) %>%
  select(year, cpi)
data_00 <- data_00 %>%
  left_join(data_cpi, by = "year")
# replace missing values
data_00 <- data_00 %>%
  mutate(educ = ifelse(educ == 999, NA, educ)) %>%
  mutate(classwly = ifelse(classwly == 99, NA, classwly)) %>%  
  mutate(wkswork2 = ifelse(wkswork2 == 999, NA, wkswork2)) %>%  
  mutate(incwage = ifelse(incwage == 9999999 | incwage == 9999998, NA, incwage)) %>%
  mutate(race = ifelse(race == 999, NA, race))
# create wrkswork variable: worked weeks are in brackets before 1976 see Katz and Murphy (1992)
data_00 <- data_00 %>%
  mutate(wkswork = ifelse(year >= 1976, wkswork1, NA)) %>%
  mutate(wkswork = ifelse(year < 1976 & wkswork2 == 1, 7, wkswork)) %>%
  mutate(wkswork = ifelse(year < 1976 & wkswork2 == 2, 20, wkswork)) %>%
  mutate(wkswork = ifelse(year < 1976 & wkswork2 == 3, 33, wkswork)) %>%
  mutate(wkswork = ifelse(year < 1976 & wkswork2 == 4, 43.5, wkswork)) %>%
  mutate(wkswork = ifelse(year < 1976 & wkswork2 == 5, 48.5, wkswork)) %>%
  mutate(wkswork = ifelse(year < 1976 & wkswork2 == 6, 51, wkswork))
# handle the top coding issue for income see Katz and Murphy (1992)'s Data section
data_00 <- data_00 %>%
  group_by(year) %>%
  mutate(top_incwage = max(incwage, na.rm = TRUE)) %>%
  mutate(incwage = ifelse(incwage == top_incwage, 1.45*incwage, incwage)) %>%
  ungroup()
# calculate log real wages
data_00 <- data_00 %>%
  mutate(rwage = incwage/cpi/wkswork) %>%
  mutate(lrwage = log(rwage))
# create education duammies
data_00 <- data_00 %>%
  mutate(dfemale = (sex == 2)) # female
data_00 <- data_00 %>%      
  mutate(deduc_1 = ifelse(educ < 70, 1, 0)) %>%                # highshool dropout
  mutate(deduc_2 = ifelse(educ >= 80 & educ < 110, 1, 0)) %>%  # some college
  mutate(deduc_3 = ifelse(educ >= 110 & educ < 123, 1, 0)) %>% # 4 years college 
  mutate(deduc_4 = ifelse(educ >= 123, 1, 0))                  # more than college
data_00 <- data_00 %>%
  mutate(drace_1 = (race == 200)) %>% # black
  mutate(drace_2 = (race > 200)) # nonwhite other
# create experience variable: check the IPUMS website for variable definition
data_00 <- data_00 %>%
  mutate(exp = ifelse(educ == 10, age - 8.5, NA)) %>%
  mutate(exp = ifelse(educ == 11, age - 7, exp)) %>%
  mutate(exp = ifelse(educ == 12, age - 8, exp)) %>%
  mutate(exp = ifelse(educ == 13, age - 9, exp)) %>%
  mutate(exp = ifelse(educ == 14, age - 10, exp)) %>%
  mutate(exp = ifelse(educ == 20, age - 11.5, exp)) %>%
  mutate(exp = ifelse(educ == 21, age - 11, exp)) %>%
  mutate(exp = ifelse(educ == 22, age - 12, exp)) %>%
  mutate(exp = ifelse(educ == 30, age - 13.5, exp)) %>%
  mutate(exp = ifelse(educ == 31, age - 13, exp)) %>%
  mutate(exp = ifelse(educ == 32, age - 14, exp)) %>%
  mutate(exp = ifelse(educ == 40, age - 15, exp)) %>%
  mutate(exp = ifelse(educ == 50, age - 16, exp)) %>%
  mutate(exp = ifelse(educ == 60, age - 17, exp)) %>%
  mutate(exp = ifelse(educ == 70, age - 18, exp)) %>%
  mutate(exp = ifelse(educ == 71, age - 18, exp)) %>%
  mutate(exp = ifelse(educ == 72, age - 18, exp)) %>%
  mutate(exp = ifelse(educ == 73, age - 18, exp)) %>%
  mutate(exp = ifelse(educ == 80, age - 19, exp)) %>%
  mutate(exp = ifelse(educ == 81, age - 19, exp)) %>%
  mutate(exp = ifelse(educ == 90, age - 20, exp)) %>%
  mutate(exp = ifelse(educ == 91, age - 20, exp)) %>%
  mutate(exp = ifelse(educ == 92, age - 20, exp)) %>%
  mutate(exp = ifelse(educ == 100, age - 21, exp)) %>%
  mutate(exp = ifelse(educ == 110, age - 22, exp)) %>%
  mutate(exp = ifelse(educ == 111, age - 22, exp)) %>%
  mutate(exp = ifelse(educ == 120, age - 23.5, exp)) %>%
  mutate(exp = ifelse(educ == 121, age - 23, exp)) %>%
  mutate(exp = ifelse(educ == 122, age - 24, exp)) %>%
  mutate(exp = ifelse(educ == 123, age - 23, exp)) %>%
  mutate(exp = ifelse(educ == 124, age - 23, exp)) %>%
  mutate(exp = ifelse(educ == 125, age - 27, exp))
# sample selection (see Katz and Murphy (1992) and Acemoglu and Autor (2011)'s Data Appendix)
data_00 <- data_00 %>%
  filter(rwage >= 67) %>%                                                       # real wage more than 67 dollars in the 1987 dollar
  filter(age >= 16 & age <= 64) %>%                                             # age equal or above 16 and equal or less than 64
  filter(fullpart == 1) %>%                                                     # work more than 35 hours
  filter(wkswork >= 40) %>%                                                     # work more than 40 weeks
  filter(classwly != 10 | classwly != 13 | classwly != 14) %>%                  # not self-employed
  filter(!((year >= 1992 & year <= 2002) & (indly >= 940 & indly <= 960))) %>%  # not in military
  filter(!(year >= 2003 & indly == 9890)) %>%
  filter(schlcoll == 5 | year < 1986) %>%                                       # no school attendance
  filter(exp >= 0)                                                              # get rid of negative experience
# graphing figures from HERE

data_00 <- data_00 %>%
  mutate(drace_1 = ifelse(race == 200, 1, 0)) %>% # black
  mutate(drace_2 = ifelse(race > 200, 1, 0)) # nonwhite other


write.csv(data_00, "data_01.csv")



# FIGURE 1


exp_level <- c(5, 15, 25, 35, 45) # experience level (5, 15, 25, 35, 45 years)

preds11 <- matrix(0,46,5) 
preds12 <- matrix(0,46,5)
preds13 <- matrix(0,46,5)
preds14 <- matrix(0,46,5)
preds15 <- matrix(0,46,5)
preds21 <- matrix(0,46,5)
preds22 <- matrix(0,46,5)
preds23 <- matrix(0,46,5)
preds24 <- matrix(0,46,5)
preds25 <- matrix(0,46,5)

male <- matrix(0,46,5)
female <- matrix(0,46,5)

coll <- matrix(0,46,1)
hischl <- matrix(0,46,1)
ratio <- matrix(0,46,1)

for (i in 1:46)
{
  
  #data_00 <- data_00 %>%
  #filter(year==1962+i) %>%  # for each year
  
  # REGRESSION
  
  # For male (wages are regressed separately by sex)
  #filter(sex==1) 
  
  
  # Log weekly wages for full-time, full-year workers (lrwage) are regressed separately by sex in each year 
  # on four education dummies (deduc_1 to deduc_4), a quartic in experience (poly(exp_level[j],4,raw=TRUE), 
  # interactions of the education dummies and experience quartic, two race categories (black drace_1, non-white other drace_2), 
  # and full set of interactions between education, experience, and sex.
  #m1[i] <- lm(lrwage ~ deduc_1 + deduc_2 + deduc_3 + deduc_4 + poly(exp,4,raw=TRUE) + poly(exp,4,raw=TRUE):(deduc_1 + deduc_2 + deduc_3 + deduc_4) + drace_1 + drace_2 + poly(exp,4,raw=TRUE):drace_1:(deduc_1 + deduc_2 + deduc_3 + deduc_4) + poly(exp,4,raw=TRUE):drace_2:(deduc_1 + deduc_2 + deduc_3 + deduc_4) )
  attach(data_00)  
  m1 <- lm(lrwage ~ deduc_1 + deduc_2 + deduc_3 + deduc_4 + poly(exp,4,raw=TRUE) + 
             poly(exp,4,raw=TRUE):(deduc_1 + deduc_2 + deduc_3 + deduc_4) + drace_1 + drace_2 + 
             poly(exp,4,raw=TRUE):drace_1:(deduc_1 + deduc_2 + deduc_3 + deduc_4) + 
             poly(exp,4,raw=TRUE):drace_2:(deduc_1 + deduc_2 + deduc_3 + deduc_4), subset = (sex==1 & year==1963+i) )
  
  
  
  # Calculate the composition-adjusted mean log wage for each group in each year
  
  for (j in 1:5) # for each experience level
  {
    # (high school graduate)
    pleaseforecast11 <- data.frame(exp = exp_level[j], drace_1 = 0, drace_2 = 0, deduc_1 = 0, deduc_2 = 0, deduc_3 = 0, deduc_4 = 0)
    #predict(m1[i], newdata=preds11[i,j])
    preds11[i,j] = predict(m1, pleaseforecast11)
    #poly(exp_level[j],4,raw=TRUE)
    
    # (high school dropout)
    pleaseforecast12 <- data.frame(exp = exp_level[j],drace_1 = 0, drace_2 = 0, deduc_1 = 1, deduc_2 = 0, deduc_3 = 0, deduc_4 = 0)
    preds12[i,j] = predict(m1, pleaseforecast12)
    
    # (some college)
    pleaseforecast13 <- data.frame(exp = exp_level[j],drace_1 = 0, drace_2 = 0, deduc_1 = 0, deduc_2 = 1, deduc_3 = 0, deduc_4 = 0)
    preds13[i,j] = predict(m1, pleaseforecast13)
    
    # (college graduate)
    pleaseforecast14 <- data.frame(exp = exp_level[j],drace_1 = 0, drace_2 = 0, deduc_1 = 0, deduc_2 = 0, deduc_3 = 1, deduc_4 = 0)
    preds14[i,j] = predict(m1, pleaseforecast14)
    
    # (greater than college)
    pleaseforecast15 <- data.frame(exp = exp_level[j],drace_1 = 0, drace_2 = 0, deduc_1 = 0, deduc_2 = 0, deduc_3 = 0, deduc_4 = 1)
    preds15[i,j] = predict(m1, pleaseforecast15)
    
    # The mean log wage for college and high school is the weighted average of the relevant composition adjusted cells 
    # using a fixed set of weights equal to the average employment share of each sex by potential experience group.
    
    #male[i,j] <- sum(wkswork, subset = (sex==1 & year==1963+i & exp == exp_level[j])) / sum(wkswork, subset = (sex==1 & year==1963+i & exp %in% exp_level)) 
    male[i,j] <- sum(wkswork, subset = (sex==1 & year==1963+i & exp == exp_level[j])) / sum(wkswork, subset = (sex==1 & exp == exp_level[j])) 
  }
  
  
  # For female (wages are regressed separately by sex)
  #data_00 <- data_00 %>%
  #filter(sex==2)
  
  # REGRESSION
  
  # Log weekly wages for full-time, full-year workers (lrwage) are regressed separately by sex in each year 
  # on four education dummies (deduc_1 to deduc_4), a quartic in experience (poly(exp_level[j],4,raw=TRUE), 
  # interactions of the education dummies and experience quartic, two race categories (black drace_1, non-white other drace_2), 
  # and full set of interactions between education, experience, and sex.
  attach(data_00)    
  m2 <- lm(lrwage ~ deduc_1 + deduc_2 + deduc_3 + deduc_4 + poly(exp,4,raw=TRUE) + 
             poly(exp,4,raw=TRUE):(deduc_1 + deduc_2 + deduc_3 + deduc_4) + drace_1 + drace_2 + 
             poly(exp,4,raw=TRUE):drace_1:(deduc_1 + deduc_2 + deduc_3 + deduc_4) + 
             poly(exp,4,raw=TRUE):drace_2:(deduc_1 + deduc_2 + deduc_3 + deduc_4), subset = (sex==2 & year==1963+i) ) 
  
  
  # Calculate the composition-adjusted mean log wage for each group in each year
  
  for (j in 1:5) # for each experience level
  {
    # (high school graduate)
    pleaseforecast21 <- data.frame(exp = exp_level[j],drace_1 = 0, drace_2 = 0, deduc_1 = 0, deduc_2 = 0, deduc_3 = 0, deduc_4 = 0)
    preds21[i,j] = predict(m2, pleaseforecast21)
    
    # (high school dropout)
    pleaseforecast22 <- data.frame(exp = exp_level[j],drace_1 = 0, drace_2 = 0, deduc_1 = 1, deduc_2 = 0, deduc_3 = 0, deduc_4 = 0)
    preds22[i,j] = predict(m2, pleaseforecast22)
    
    # (some college)
    pleaseforecast23 <- data.frame(exp = exp_level[j],drace_1 = 0, drace_2 = 0, deduc_1 = 0, deduc_2 = 1, deduc_3 = 0, deduc_4 = 0)
    preds23[i,j] = predict(m2, pleaseforecast23)
    
    # (college graduate)
    pleaseforecast24 <- data.frame(exp = exp_level[j],drace_1 = 0, drace_2 = 0, deduc_1 = 0, deduc_2 = 0, deduc_3 = 1, deduc_4 = 0)
    preds24[i,j] = predict(m2, pleaseforecast24)
    
    # (greater than college)
    pleaseforecast25 <- data.frame(exp = exp_level[j],drace_1 = 0, drace_2 = 0, deduc_1 = 0, deduc_2 = 0, deduc_3 = 0, deduc_4 = 1)
    preds25[i,j] = predict(m2, pleaseforecast25)
    
    # The mean log wage for college and high school is the weighted average of the relevant composition adjusted cells 
    # using a fixed set of weights equal to the average employment share of each sex by potential experience group.
    
    #female[i,j] <- sum(wkswork, subset = (sex==2 & year==1963+i & exp == exp_level[j])) / sum(wkswork, subset = (sex==2 & year==1963+i & exp %in% exp_level)) 
    female[i,j] <- sum(wkswork, subset = (sex==2 & year==1963+i & exp == exp_level[j])) / sum(wkswork, subset = (sex==2 & exp == exp_level[j])) 
    
  }
  
}



# The ratio of mean log wages for college and high school graduates for each year
for (i in 1:46)
{
  coll[i] <- preds14[i,1] * male[i,1] + preds24[i,1] * female[i,1] + preds14[i,2] * male[i,2] + preds24[i,2] * female[i,2] + preds14[i,3] * male[i,3] + preds24[i,3] * female[i,3] + preds14[i,4] * male[i,4] + preds24[i,4] * female[i,4] + preds14[i,5] * male[i,5] + preds24[i,5] * female[i,5] 
  hischl[i] <- preds11[i,1] * male[i,1] + preds21[i,1] * female[i,1] + preds11[i,2] * male[i,2] + preds21[i,2] * female[i,2] + preds11[i,3] * male[i,3] + preds21[i,3] * female[i,3] + preds11[i,4] * male[i,4] + preds21[i,4] * female[i,4] + preds11[i,5] * male[i,5] + preds21[i,5] * female[i,5] 
  ratio[i] <- coll[i]/ hischl[i]
}

# Plots
dyear = seq.int(1964,2009,1)
figure1 = data.frame(log_wage_gap = ratio, year = dyear)

#ggplot(data = figure1) + 
#  geom_point(mapping = aes(x = year, y = ratio))

ggplot(data = figure1) + 
  geom_point(mapping = aes(x = year, y = log_wage_gap)) +
  geom_line(mapping = aes(x = year, y = log_wage_gap))+
  ggtitle('Compositiion adjusted college/high-school log weekly wage ratio, 1963-2008')



# FIGURE 2


# Create the categorical variable of education level (edlvl)
data_00 <- data_00 %>%      
  mutate(edlvl = ifelse(deduc_1==1,1,ifelse(deduc_2==1,3,ifelse(deduc_3==1,4,ifelse(deduc_4==1,5,2)))))
attach(data_00)



# Calculate the labor supply for college/high school groups by experience level
lsup<-data_00 %>%
  group_by(year,sex,exp,edlvl) %>%
  summarise(labsup = log(mean(wkswork)))

avgwage <-data_00%>%
  group_by(year,sex,exp,edlvl) %>%
  summarise(wwage = mean(rwage))

relw <-avgwage%>%
  group_by(year)%>%
  summarise(ywage = mean(wwage))

avgwage <- merge(x= avgwage, y = relw, by = c("year"))

avgwage <- avgwage%>%
  mutate(relwage = wwage/ywage)

lsupavgwage <- merge(x=lsup,y=avgwage,by=c("year","sex","exp","edlvl"))

lsupavgwage <-lsupavgwage%>%
  mutate(sindex = lsupavgwage$labsup * lsupavgwage$relwage)

supindex<- lsupavgwage%>%
  group_by(year)%>%
  summarise(index = log(mean(sindex)))

ggplot(data = supindex)+ 
  geom_point(mapping = aes(x = year, y = log(index))) +
  geom_line(mapping = aes(x = year, y = log(index)))+
  ggtitle('College/high-school log relative supply')

