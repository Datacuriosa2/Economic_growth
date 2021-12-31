################################################################################
'Project : 1 - Historic comparison of Latam GDP per capita
Data: GDP per capita, PPP (constant 2017 international $)
Source: https://data.worldbank.org/indicator/NY.GDP.PCAP.PP.KD
Author: Marygracia Aquino
Contact: datacuriosa2@gmail.com'
################################################################################

#packages
#install.packages("dplyr")
library(dplyr)
library(readxl)
library(ggplot2)

################################################################################

#import data dgp per capita
gdp_per_capita <- as.data.frame(read_xls("gdp_per_capita_ppp_constant2017.xls", 
                                         skip = 3))

################################################################################

#drop unnecessary variables
bd1 <- gdp_per_capita[,-c(2:34)]

#creating ID for latam countries

##English
country <- c("Argentina", "Bolivia", "Brazil", "Chile", "Colombia", "Costa Rica",
             "Cuba", "Dominican Republic", "Ecuador", "El Salvador", "Guatemala",
             "Haiti", "Honduras", "Mexico", "Nicaragua", "Panama","Paraguay", 
             "Peru", "Uruguay", "Venezuela")
latam_en <- data.frame(country)
latam_en$latam <- 1

##Spanish
latam_es <- c("Argentina", "Bolivia", "Brasil", "Chile", "Colombia", "Costa Rica",
              "Cuba", "República Dominicana", "Ecuador", "El Salvador", "Guatemala",
              "Haití", "Honduras", "México", "Nicaragua", "Panamá","Paraguay", 
              "Perú", "Uruguay", "Venezuela")

#change name variables of gdp per capita
names(bd1) <- c("country", "y1990", "y1991", "y1992", "y1993", "y1994", "y1995",
                "y1996", "y1997", "y1998", "y1999", "y2000", "y2001", "y2002",
                "y2003", "y2004", "y2005", "y2006", "y2007", "y2008", "y2009", 
                "y2010", "y2011", "y2012", "y2013", "y2014", "y2015", "y2016", 
                "y2017", "y2018", "y2019", "y2020")

#keeping latam countries of gdp per country
bd2 <- left_join(latam_en,bd1,by="country")
pais <- data.frame(latam_es)
bd2 <- bind_cols(bd2,pais)

#calculating growth
bd2$g_1990_2020 <- (bd2$y2020 - bd2$y1990)/bd2$y1990
bd2$g_1990_2019 <- (bd2$y2019 - bd2$y1990)/bd2$y1990
bd2$g_2010_2020 <- (bd2$y2020 - bd2$y2010)/bd2$y2010
bd2$g_2010_2019 <- (bd2$y2019 - bd2$y2010)/bd2$y2010

#average growth
mean_g_1990_2020 <- mean(bd2$g_1990_2020, na.rm = T)
mean_g_1990_2019 <- mean(bd2$g_1990_2019, na.rm = T)
mean_g_2010_2020 <- mean(bd2$g_2010_2020, na.rm = T)
mean_g_2010_2019 <- mean(bd2$g_2010_2019, na.rm = T)

#average gdp per capita
mean_gdp_per_capita_1990 <- mean(bd2$y1990, na.rm = T)
mean_gdp_per_capita_2020 <- mean(bd2$y2020, na.rm = T)
mean_gdp_per_capita_2019 <- mean(bd2$y2019, na.rm = T)

#growth average gdp per capita
g1990_2020 <-(mean_gdp_per_capita_2020/mean_gdp_per_capita_1990)-1
g1990_2019 <-(mean_gdp_per_capita_2019/mean_gdp_per_capita_1990)-1

################################################################################

#export data
writexl::write_xlsx(bd2,"growth_latam_1990-2020.xlsx")
