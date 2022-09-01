rm(list=ls())
library(readxl)
library(tibble)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(data.table)

setwd("/Users/nguyenmui/Library/CloudStorage/OneDrive-VictoriaUniversityofWellington-STAFF/Roy Mckenzie Centre/MSD Wellbeing/chart")
getwd()

###########################
### NOT ENGOUGH MONEY #####
##########################
benefit <- read_excel("test.xlsx",1)
#--------------------------#
#### Draw line chart 1 #####
#--------------------------#
chart_1 <- subset(benefit, benefit == "Main Benefit" & 
                    type == "Benefit in the month of survey" & 
                    choice == "Yes" & family_structure != "Total") %>% 
  pivot_longer(c(`2008`, `2010`,`2012`,`2014`,`2016`,`2018`,`2020`), names_to = "year", values_to = "percentage")

#chart_1$year <- as.numeric(chart_1$year)
a <- ggplot(data = chart_1, aes(year, percentage, color = family_structure, group = family_structure)) +
  geom_line(size=0.7) + geom_point(size=1.7)
a + labs(x = NULL, y = "Percent",
         title = "% with not enough money to meet everyday needs: received a main benefit in the past month",
         caption = "Data Source: IDI data") + theme_bw() + theme(plot.title = element_text(hjust = 0.5)) + 
  theme(legend.position="bottom")+ theme(legend.title = element_blank()) + 
  guides(color = guide_legend(nrow = 2, byrow = TRUE))

#--------------------------#
#### Draw line chart 2 #####
#--------------------------#
table_1 <- subset(benefit, benefit == "Main Benefit" & 
                    type == "Benefit in the month of survey" & 
                    choice == "Yes" & family_structure != "Total") %>% 
  pivot_longer(c(`2008`, `2010`,`2012`,`2014`,`2016`,`2018`,`2020`), names_to = "year", values_to = "percentage")
table_2 <- subset(benefit, benefit == "Main Benefit" & 
                    type == "Benefit in the month of survey" & 
                    choice == "No" & family_structure != "Total") %>% 
  pivot_longer(c(`2008`, `2010`,`2012`,`2014`,`2016`,`2018`,`2020`), names_to = "year", values_to = "percentage")

diff_table <- table_1 %>% 
  left_join(table_2, by = c("year"="year", "family_structure" = "family_structure")) %>% 
  mutate(diff = percentage.x - percentage.y)

b <- ggplot(data = diff_table, aes(year, diff, color = family_structure, group = family_structure)) +
  geom_line(size=0.7) + geom_point(size=1.7) +  ylim(-15,15) + geom_hline(aes(yintercept = 0))
b + labs(x = NULL, y = "Percent",
         title = "Difference in % with not enough money to meet everyday needs \n between those receiving a main benefit and those not receiving a main benefit in the past month",
         caption = "Data Source: IDI data") + theme_bw() + theme(plot.title = element_text(hjust = 0.5)) + 
  theme(legend.position="bottom")+ theme(legend.title = element_blank()) + 
  guides(color = guide_legend(nrow = 2, byrow = TRUE))

#--------------------------#
#### Draw line chart 3 #####
#--------------------------#
chart_3 <- subset(benefit, benefit == "Main Benefit" &
                               family_structure == "Total") %>% 
  pivot_longer(c(`2008`, `2010`,`2012`,`2014`,`2016`,`2018`,`2020`), names_to = "year", values_to = "percentage")

type.labs <- c("Benefit receipt in the past month", "Benefit receipt in the past year", "Benefit receipt 30 of past 36 months")
names(type.labs) <- c("Benefit in the month of survey", "Benefit in the year of survey","Benefit for 30+ months in last three years from survey")

chart_3 <- chart_3 %>% 
  mutate(type = fct_relevel(type, c("Benefit in the month of survey", "Benefit in the year of survey","Benefit for 30+ months in last three years from survey")),
         label = toupper(type))
c3 <- ggplot(data = chart_3, aes(year, percentage, color = factor(choice,labels=c("No benefit receipt","Benefit receipt")), group = choice)) +
  geom_line(size=0.7) + geom_point(size=1.7) + facet_grid(~ type, labeller = labeller(type = type.labs)) + ylim(0, 22)
c3 + labs(x = NULL, y = "Percent",
         title = "% with not enough money to meet everyday needs by main benefit time period receipt: Total",
         caption = "Data Source: IDI data") + theme_bw() + theme(plot.title = element_text(hjust = 0.5)) + 
  theme(legend.position="bottom")+ theme(legend.title = element_blank())
  

#--------------------------#
#### Draw line chart 4 #####
#--------------------------#
chart_4 <- subset(benefit, benefit == "Main Benefit" &
                    family_structure == "Couples with dependent children") %>% 
  pivot_longer(c(`2008`, `2010`,`2012`,`2014`,`2016`,`2018`,`2020`), names_to = "year", values_to = "percentage")

type.labs <- c("Benefit receipt in the past month", "Benefit receipt in the past year", "Benefit receipt 30 of past 36 months")
names(type.labs) <- c("Benefit in the month of survey", "Benefit in the year of survey","Benefit for 30+ months in last three years from survey")

chart_4 <- chart_4 %>% 
  mutate(type = fct_relevel(type, c("Benefit in the month of survey", "Benefit in the year of survey","Benefit for 30+ months in last three years from survey")),
         label = toupper(type))
c4 <- ggplot(data = chart_4, aes(year, percentage, color = factor(choice,labels=c("No benefit receipt","Benefit receipt")), group = choice)) +
  geom_line(size=0.7) + geom_point(size=1.7) + facet_grid(~ type, labeller = labeller(type = type.labs)) + ylim(0, 22)
c4 + labs(x = NULL, y = "Percent",
          title = "% with not enough money to meet everyday needs by main benefit time period receipt:\n Couples with dependent children",
          caption = "Data Source: IDI data") + theme_bw() + theme(plot.title = element_text(hjust = 0.5)) + 
  theme(legend.position="bottom")+ theme(legend.title = element_blank())

#--------------------------#
#### Draw line chart 5 #####
#--------------------------#
chart_5 <- subset(benefit, benefit == "Main Benefit" &
                    family_structure == "Couples without dependent children") %>% 
  pivot_longer(c(`2008`, `2010`,`2012`,`2014`,`2016`,`2018`,`2020`), names_to = "year", values_to = "percentage")

type.labs <- c("Benefit receipt in the past month", "Benefit receipt in the past year", "Benefit receipt 30 of past 36 months")
names(type.labs) <- c("Benefit in the month of survey", "Benefit in the year of survey","Benefit for 30+ months in last three years from survey")

chart_5 <- chart_5 %>% 
  mutate(type = fct_relevel(type, c("Benefit in the month of survey", "Benefit in the year of survey","Benefit for 30+ months in last three years from survey")),
         label = toupper(type))
c5 <- ggplot(data = chart_5, aes(year, percentage, color = factor(choice,labels=c("No benefit receipt","Benefit receipt")), group = choice)) +
  geom_line(size=0.7) + geom_point(size=1.7) + facet_grid(~ type, labeller = labeller(type = type.labs)) + ylim(0, 15)
c5 + labs(x = NULL, y = "Percent",
          title = "% with not enough money to meet everyday needs by main benefit time period receipt:\n Couples without dependent children",
          caption = "Data Source: IDI data") + theme_bw() + theme(plot.title = element_text(hjust = 0.5)) + 
  theme(legend.position="bottom")+ theme(legend.title = element_blank())

#--------------------------#
#### Draw line chart 6 #####
#--------------------------#
chart_6 <- subset(benefit, benefit == "Main Benefit" &
                    family_structure == "Sole parents with dependent children") %>% 
  pivot_longer(c(`2008`, `2010`,`2012`,`2014`,`2016`,`2018`,`2020`), names_to = "year", values_to = "percentage")

type.labs <- c("Benefit receipt in the past month", "Benefit receipt in the past year", "Benefit receipt 30 of past 36 months")
names(type.labs) <- c("Benefit in the month of survey", "Benefit in the year of survey","Benefit for 30+ months in last three years from survey")

chart_6 <- chart_6 %>% 
  mutate(type = fct_relevel(type, c("Benefit in the month of survey", "Benefit in the year of survey","Benefit for 30+ months in last three years from survey")),
         label = toupper(type))
c6 <- ggplot(data = chart_6, aes(year, percentage, color = factor(choice,labels=c("No benefit receipt","Benefit receipt")), group = choice)) +
  geom_line(size=0.7) + geom_point(size=1.7) + facet_grid(~ type, labeller = labeller(type = type.labs)) + ylim(0, 65)
c6 + labs(x = NULL, y = "Percent",
          title = "% with not enough money to meet everyday needs by main benefit time period receipt:\n Sole parents with dependent children",
          caption = "Data Source: IDI data") + theme_bw() + theme(plot.title = element_text(hjust = 0.5)) + 
  theme(legend.position="bottom")+ theme(legend.title = element_blank())

#--------------------------#
#### Draw line chart 8 #####
#--------------------------#
chart_8 <- subset(benefit, benefit == "Main Benefit" &
                    family_structure == "Single people without dependent children") %>% 
  pivot_longer(c(`2008`, `2010`,`2012`,`2014`,`2016`,`2018`,`2020`), names_to = "year", values_to = "percentage")

type.labs <- c("Benefit receipt in the past month", "Benefit receipt in the past year", "Benefit receipt 30 of past 36 months")
names(type.labs) <- c("Benefit in the month of survey", "Benefit in the year of survey","Benefit for 30+ months in last three years from survey")

chart_8 <- chart_8 %>% 
  mutate(type = fct_relevel(type, c("Benefit in the month of survey", "Benefit in the year of survey","Benefit for 30+ months in last three years from survey")),
         label = toupper(type))
c8 <- ggplot(data = chart_8, aes(year, percentage, color = factor(choice,labels=c("No benefit receipt","Benefit receipt")), group = choice)) +
  geom_line(size=0.7) + geom_point(size=1.7) + facet_grid(~ type, labeller = labeller(type = type.labs)) + ylim(0, 30)
c8 + labs(x = NULL, y = "Percent",
          title = "% with not enough money to meet everyday needs by main benefit time period receipt:\n Single people without dependent children",
          caption = "Data Source: IDI data") + theme_bw() + theme(plot.title = element_text(hjust = 0.5)) + 
  theme(legend.position="bottom")+ theme(legend.title = element_blank())
#--------------------------#
#### Draw line chart 7 #####
#--------------------------#
chart_7 <- subset(benefit, type == "Benefit in the month of survey" & family_structure != "Total" &
                    choice == "Yes") %>% 
  pivot_longer(c(`2008`, `2010`,`2012`,`2014`,`2016`,`2018`,`2020`), names_to = "year", values_to = "percentage")

c7 <- ggplot(data = chart_7, aes(year, percentage, color = benefit, group = benefit)) +
  geom_line(size=0.7) + geom_point(size=1.7) + facet_wrap(~ family_structure)
c7 + labs(x = NULL, y = "Percent",
          title = "% with not enough money to meet everyday needs \n by main versus MSD benefit receipt in the past month",
          subtitle = "Unit: %",
          caption = "Data Source: IDI data") + theme_bw() + theme(plot.title = element_text(hjust = 0.5)) + 
  theme(legend.position="bottom")+ theme(legend.title = element_blank())


###########################
### EMPLOYMENT #####
##########################
rm(list=ls())
benefit <- read_excel("employment.xlsx",1)
#--------------------------#
#### Draw line chart 1 #####
#--------------------------#
chart_1 <- subset(benefit, benefit == "Main Benefit" & 
                    type == "Benefit in the month of survey" & 
                    choice == "Yes" & family_structure != "Total") %>% 
  pivot_longer(c(`2008`, `2010`,`2012`,`2014`,`2016`,`2018`,`2020`), names_to = "year", values_to = "percentage")

#chart_1$year <- as.numeric(chart_1$year)
a <- ggplot(data = chart_1, aes(year, percentage, color = family_structure, group = family_structure)) +
  geom_line(size=0.7) + geom_point(size=1.7) + ylim (10,100)
a + labs(x = NULL, y = "Percent",
         title = "% employed: received a main benefit in the past month",
         caption = "Data Source: IDI data") + theme_bw() + theme(plot.title = element_text(hjust = 0.5)) + 
  theme(legend.position="bottom")+ theme(legend.title = element_blank()) + 
  guides(color = guide_legend(nrow = 2, byrow = TRUE))

#--------------------------#
#### Draw line chart 2 #####
#--------------------------#
table_1 <- subset(benefit, benefit == "Main Benefit" & 
                    type == "Benefit in the month of survey" & 
                    choice == "Yes" & family_structure != "Total") %>% 
  pivot_longer(c(`2008`, `2010`,`2012`,`2014`,`2016`,`2018`,`2020`), names_to = "year", values_to = "percentage")
table_2 <- subset(benefit, benefit == "Main Benefit" & 
                    type == "Benefit in the month of survey" & 
                    choice == "No" & family_structure != "Total") %>% 
  pivot_longer(c(`2008`, `2010`,`2012`,`2014`,`2016`,`2018`,`2020`), names_to = "year", values_to = "percentage")

diff_table <- table_1 %>% 
  left_join(table_2, by = c("year"="year", "family_structure" = "family_structure")) %>% 
  mutate(diff = percentage.x - percentage.y)

b <- ggplot(data = diff_table, aes(year, diff, color = family_structure, group = family_structure)) +
  geom_line(size=0.7) + geom_point(size=1.7) +  ylim(-15,15) + geom_hline(aes(yintercept = 0))
b + labs(x = NULL, y = "Percent",
         title = "Difference in % employed between those receiving a main benefit \n and those not receiving a main benefit in the past month",
         caption = "Data Source: IDI data") + theme_bw() + theme(plot.title = element_text(hjust = 0.5)) + 
  theme(legend.position="bottom")+ theme(legend.title = element_blank()) + 
  guides(color = guide_legend(nrow = 2, byrow = TRUE))

#--------------------------#
#### Draw line chart 3 #####
#--------------------------#
chart_3 <- subset(benefit, benefit == "Main Benefit" &
                    family_structure == "Total") %>% 
  pivot_longer(c(`2008`, `2010`,`2012`,`2014`,`2016`,`2018`,`2020`), names_to = "year", values_to = "percentage")

type.labs <- c("Benefit receipt in the past month", "Benefit receipt in the past year", "Benefit receipt 30 of past 36 months")
names(type.labs) <- c("Benefit in the month of survey", "Benefit in the year of survey","Benefit for 30+ months in last three years from survey")

chart_3 <- chart_3 %>% 
  mutate(type = fct_relevel(type, c("Benefit in the month of survey", "Benefit in the year of survey","Benefit for 30+ months in last three years from survey")),
         label = toupper(type))
c3 <- ggplot(data = chart_3, aes(year, percentage, color = factor(choice,labels=c("No benefit receipt","Benefit receipt")), group = choice)) +
  geom_line(size=0.7) + geom_point(size=1.7) + facet_grid(~ type, labeller = labeller(type = type.labs)) + ylim(75, 85)
c3 + labs(x = NULL, y = "Percent",
          title = "% employed by main benefit time period receipt: Total",
          caption = "Data Source: IDI data") + theme_bw() + theme(plot.title = element_text(hjust = 0.5)) + 
  theme(legend.position="bottom")+ theme(legend.title = element_blank())


#--------------------------#
#### Draw line chart 4 #####
#--------------------------#
chart_4 <- subset(benefit, benefit == "Main Benefit" &
                    family_structure == "Couples with dependent children") %>% 
  pivot_longer(c(`2008`, `2010`,`2012`,`2014`,`2016`,`2018`,`2020`), names_to = "year", values_to = "percentage")

type.labs <- c("Benefit receipt in the past month", "Benefit receipt in the past year", "Benefit receipt 30 of past 36 months")
names(type.labs) <- c("Benefit in the month of survey", "Benefit in the year of survey","Benefit for 30+ months in last three years from survey")

chart_4 <- chart_4 %>% 
  mutate(type = fct_relevel(type, c("Benefit in the month of survey", "Benefit in the year of survey","Benefit for 30+ months in last three years from survey")),
         label = toupper(type))
c4 <- ggplot(data = chart_4, aes(year, percentage, color = factor(choice,labels=c("No benefit receipt","Benefit receipt")), group = choice)) +
  geom_line(size=0.7) + geom_point(size=1.7) + facet_grid(~ type, labeller = labeller(type = type.labs)) + ylim(75, 95)
c4 + labs(x = NULL, y = "Percent",
          title = "% employed by main benefit time period receipt:\n Couples with dependent children",
          caption = "Data Source: IDI data") + theme_bw() + theme(plot.title = element_text(hjust = 0.5)) + 
  theme(legend.position="bottom")+ theme(legend.title = element_blank())

#--------------------------#
#### Draw line chart 5 #####
#--------------------------#
chart_5 <- subset(benefit, benefit == "Main Benefit" &
                    family_structure == "Couples without dependent children") %>% 
  pivot_longer(c(`2008`, `2010`,`2012`,`2014`,`2016`,`2018`,`2020`), names_to = "year", values_to = "percentage")

type.labs <- c("Benefit receipt in the past month", "Benefit receipt in the past year", "Benefit receipt 30 of past 36 months")
names(type.labs) <- c("Benefit in the month of survey", "Benefit in the year of survey","Benefit for 30+ months in last three years from survey")

chart_5 <- chart_5 %>% 
  mutate(type = fct_relevel(type, c("Benefit in the month of survey", "Benefit in the year of survey","Benefit for 30+ months in last three years from survey")),
         label = toupper(type))
c5 <- ggplot(data = chart_5, aes(year, percentage, color = factor(choice,labels=c("No benefit receipt","Benefit receipt")), group = choice)) +
  geom_line(size=0.7) + geom_point(size=1.7) + facet_grid(~ type, labeller = labeller(type = type.labs)) + ylim(80, 90)
c5 + labs(x = NULL, y = "Percent",
          title = "% employed by main benefit time period receipt:\n Couples without dependent children",
          caption = "Data Source: IDI data") + theme_bw() + theme(plot.title = element_text(hjust = 0.5)) + 
  theme(legend.position="bottom")+ theme(legend.title = element_blank())

#--------------------------#
#### Draw line chart 6 #####
#--------------------------#
chart_6 <- subset(benefit, benefit == "Main Benefit" &
                    family_structure == "Sole parents with dependent children") %>% 
  pivot_longer(c(`2008`, `2010`,`2012`,`2014`,`2016`,`2018`,`2020`), names_to = "year", values_to = "percentage")

type.labs <- c("Benefit receipt in the past month", "Benefit receipt in the past year", "Benefit receipt 30 of past 36 months")
names(type.labs) <- c("Benefit in the month of survey", "Benefit in the year of survey","Benefit for 30+ months in last three years from survey")

chart_6 <- chart_6 %>% 
  mutate(type = fct_relevel(type, c("Benefit in the month of survey", "Benefit in the year of survey","Benefit for 30+ months in last three years from survey")),
         label = toupper(type))
c6 <- ggplot(data = chart_6, aes(year, percentage, color = factor(choice,labels=c("No benefit receipt","Benefit receipt")), group = choice)) +
  geom_line(size=0.7) + geom_point(size=1.7) + facet_grid(~ type, labeller = labeller(type = type.labs)) + ylim(40, 80)
c6 + labs(x = NULL, y = "Percent",
          title = "% employed by main benefit time period receipt:\n Sole parents with dependent children",
          caption = "Data Source: IDI data") + theme_bw() + theme(plot.title = element_text(hjust = 0.5)) + 
  theme(legend.position="bottom")+ theme(legend.title = element_blank())

#--------------------------#
#### Draw line chart 7 #####
#--------------------------#
chart_7 <- subset(benefit, type == "Benefit in the month of survey" & family_structure != "Total" &
                    choice == "Yes") %>% 
  pivot_longer(c(`2008`, `2010`,`2012`,`2014`,`2016`,`2018`,`2020`), names_to = "year", values_to = "percentage")

c7 <- ggplot(data = chart_7, aes(year, percentage, color = benefit, group = benefit)) +
  geom_line(size=0.7) + geom_point(size=1.7) + facet_wrap(~ family_structure)
c7 + labs(x = NULL, y = "Percent",
          title = "% employed by main versus MSD benefit receipt in the past month",
          subtitle = "Unit: %",
          caption = "Data Source: IDI data") + theme_bw() + theme(plot.title = element_text(hjust = 0.5)) + 
  theme(legend.position="bottom")+ theme(legend.title = element_blank())

#--------------------------#
#### Draw line chart 8 #####
#--------------------------#
chart_8 <- subset(benefit, benefit == "Main Benefit" &
                    family_structure == "Single people without dependent children") %>% 
  pivot_longer(c(`2008`, `2010`,`2012`,`2014`,`2016`,`2018`,`2020`), names_to = "year", values_to = "percentage")

type.labs <- c("Benefit receipt in the past month", "Benefit receipt in the past year", "Benefit receipt 30 of past 36 months")
names(type.labs) <- c("Benefit in the month of survey", "Benefit in the year of survey","Benefit for 30+ months in last three years from survey")

chart_8 <- chart_8 %>% 
  mutate(type = fct_relevel(type, c("Benefit in the month of survey", "Benefit in the year of survey","Benefit for 30+ months in last three years from survey")),
         label = toupper(type))
c8 <- ggplot(data = chart_8, aes(year, percentage, color = factor(choice,labels=c("No benefit receipt","Benefit receipt")), group = choice)) +
  geom_line(size=0.7) + geom_point(size=1.7) + facet_grid(~ type, labeller = labeller(type = type.labs)) + ylim(60, 80)
c8 + labs(x = NULL, y = "Percent",
          title = "% employed by main benefit time period receipt:\n Single people without dependent children",
          caption = "Data Source: IDI data") + theme_bw() + theme(plot.title = element_text(hjust = 0.5)) + 
  theme(legend.position="bottom")+ theme(legend.title = element_blank())
