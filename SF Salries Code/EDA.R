######### Exploratory Data Analysis  ###########


####################
#  Simple statistics
####################
mean(test$BasePay, na.rm=TRUE)
mean(SalariesClean$BasePay,na.rm=TRUE)
mean(overtime,na.rm=TRUE)
mean(totalpay,na.rm=TRUE)
sd(totalpay)
t.test(totalpay,overtime,na.rm=TRUE)

plot(Salaries$OvertimePay, Salaries$Year, main="Plot of Overtime pay to Year")


##job title frequency
Salaries %>%
  group_by(JobTitle) %>%
  summarize(Frequency = n()) %>%
  arrange(desc(Frequency)) %>%
  head

##how many job titles occur only once
Salaries %>%
  group_by(JobTitle) %>%
  summarize(Frequency = n()) %>%
  mutate(OccursOnce = Frequency == 1) %>%
  %group_by(OccursOnce) %>%
summarize(Total = n())

##which jobs occur only once
Salaries %>% 
  filter(!duplicated(JobTitle)) %>%
  head()

##how does pay compared to people who share a title
Salaries %>%
  mutate(SharesTitle = duplicated(JobTitle)) %>%
  ggplot(aes(x = TotalPay)) + 
  geom_density(aes(fill = SharesTitle), alpha = 0.6)

##Compensation graph all employees
Salaries %>%
  ggplot(aes(x = BasePay)) +
  geom_density(fill = "grey40")


##creating dataset for full time employees
ft <- Salaries %>% filter(Status == "FT")
dim(ft)

##creating dataset for part time employees
pt <- Salaries %>% filter(Status == "PT")
dim(pt)

ptBene <- pt %>% filter(Benefits > 0)



##breakdown by year of FT employees
ft %>%
  group_by(Year) %>%
  summarize(Frequency = n())

## all from 2014


##Compensation graph FT employees
ft %>%
  ggplot(aes(x = BasePay)) +
  geom_density(fill = "grey40") 


#Benefits graph FT employees
ft %>%
  ggplot(aes(x = Benefits)) +
  geom_density(fill = "grey40")

## who has pay less than 0
Salaries %>%
  filter(TotalPay == 0)