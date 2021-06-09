## script to look at pheno plants progression
##Script by Alison Haddad


library(neonUtilities)
library(tidyverse)
library(lubridate)

setwd("C:/Users/haddad/Documents/R/R-enthusiasts group/Alisons stuff")

#download CPER PHE data from the portal
phe <- loadByProduct(
  dpID = "DP1.10055.001",
  site = "CPER",
  startdate = "2017-01",
  enddate = "2020-12",
  check.size = F
)
View(phe)
 
#simplify the names of dataframes I will need
a<-phe$phe_perindividual
b<-phe$phe_statusintensity

#merge the two data frames by the column they have the same
phe.data <- merge(a,b, by = 'individualID')
View(phe.data)

#remove all the useless columns
phe.data.clean <- phe.data %>%  select(dayOfYear, date.y, taxonID, individualID, phenophaseName, phenophaseStatus, phenophaseIntensity)
View(phe.data.clean)

#filter to only look at when plants are active
phe.data.yes <- filter(phe.data.clean, phenophaseStatus == 'yes')
View(phe.data.yes)
     

#use lubridate to split date function into separate year, month, and day column. 
#There's probably a cleaner way to do this, but I copied code from the internet and it worked sooooo
phe.data.yes <- phe.data.yes %>% mutate(date.y = ymd(date.y)) %>%
  mutate_at(vars(date.y),funs(year, month, day))
 View(phe.data.yes) 
 
#split the individual ID column so I can label my graphs with the TagID of the plant. 
#Remove = F keeps the original column, default is to remove the originial column
#Initially tried to split the individual ID into all its separate parts using '.' as the separator, but it kept throwing me errors
phe.data.new <- phe.data.yes %>% separate(individualID, c('Junk', 'TagID'), sep = '.CPER.0', remove = F)
View(phe.data.new) 


#change the NAs to 0s
phe.data.new[is.na(phe.data.new)]<-0

#create separate data frames for each plant
phe.BOGR <- filter(phe.data.new, taxonID == 'BOGR2')
phe.HECO <- filter(phe.data.new, taxonID == 'HECO26')
phe.THFI <- filter(phe.data.new, taxonID == 'THFI')
View(phe.THFI)

#plot out THFI, and change the colors! ooooh so fancy
ggplot(phe.THFI,
       aes(x= dayOfYear, y = year))+
         geom_point(aes(color = factor(phenophaseName)))+
         scale_color_manual(values = c('green','darkgreen','pink'))+
         facet_wrap(~TagID)






