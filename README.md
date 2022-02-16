# Popupation pyramids updated
## Upload the relevant packages and dataset. You can find the data on github here

library(tidyverse)
options(scipen = 9)
setwd("/myworkingdirectory/")
mydt % filter(iso=='UGA')
## The dataset includes population estimates at subnational level for Uganda.

### reformat the dataset using tidy
 
newdf % gather(variable, value,6:761) %>% separate(variable,c('year','sex', 'age'), sep='_') %>% mutate(sex=if_else(sex=='F','female','male')) %>%
spread(year, value) %>%
mutate(age2=recode(age, '1'='0-4', '4'='0-4', '5'='5-9','10'='10-14','15'='15-19', '20'='20-24', '25'= '25-29', '30'='30-34', '35'='35-39', '40'='40-44', '45'='45-49', '50'='50-54', '55'='55-59', '60'='60-64', '65'='65-69', '70'='70-74', '75'='75-79', '80'='80+')) %>%
mutate(age=recode(age, '1'='0', '4'='0'))
 
newdf$age %
gather(key = year, value = pop, 10:14) %>%
filter(iso == "UGA"&adm_id==c("UGMIS2014452022"), year %in% c(2000, 2005, 2010, 2015, 2020))
 
newdf4 %
group_by(iso, adm_id, id, year, sex, age, age2, ageno) %>%
summarise(pop= sum(pop)) %>%
mutate(ageno = ageno + 1)
 
library(ggthemes)
ggplot(data = newdf4, aes(x = age, y = pop/1000, fill = year)) +
#bars for all but 2100
geom_bar(data = newdf4 %>% filter(sex == "female", year != 2100) %>% arrange(rev(year)),
stat = "identity",
position = "identity", width = 4.5) +
geom_bar(data = newdf4 %>% filter(sex == "male", year != 2100) %>% arrange(rev(year)),
stat = "identity",
position = "identity",
mapping = aes(y = -pop/1000)) +
coord_flip() +
scale_y_continuous(labels = abs, breaks = seq(-600, 600, 250)) +
geom_hline(yintercept = 0) +
theme_economist_white(horizontal = FALSE) +
scale_fill_economist() +
labs(fill = "", x = "", y = "")

