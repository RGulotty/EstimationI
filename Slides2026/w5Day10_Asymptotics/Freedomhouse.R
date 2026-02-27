library(tidyverse)
library(readxl)

load("~/Downloads/fh_pmm.rda")

df<-read_csv("~/Downloads/qjps_18132_supp/DATA/backsliding.csv")
df%>%arrange(year)
fh_pmm

#Calculate backsliding
fh_pmm %>%group_by(pmm_country) %>% arrange(year)%>% 
mutate(lfh=lag(pmm_freedomhouse), pmmdiff= pmm_freedomhouse-lfh)%>%
filter(pmmdiff<0)

fhdata <- read_excel("~/Downloads/Country_and_Territory_Ratings_and_Statuses_FIW1973-2021.xlsx", sheet = 2, skip = 3, col_names = FALSE)
nYears <- (ncol(fhdata) - 1)/3
 var_years <- expand.grid(x = c("pr", "cl", "status"), y = c(1972:1980,1982:(1972 + nYears)))
 
 names(fhdata) <- c("country", paste(var_years$x, var_years$y,  sep = "_"))
 
fhdata$pr_1972 <- as.double(fhdata$pr_1972)
fhdata$cl_1972 <- as.double(fhdata$cl_1972)
    data <- fhdata %>% tidyr::pivot_longer(names_to = "indicator", values_to = "value", dplyr::matches("[12][0-9]{3}"), 
        values_transform = list(value = as.character)) %>% 
        tidyr::separate(indicator, into = c("status", "year"), sep = "_") %>% 
        filter(!is.na(value)) %>% 
        distinct() %>% tidyr::pivot_wider(names_from = "status", values_from = "value") %>% 
        mutate(across(year:cl, as.numeric)) %>% 
        mutate(cl = ifelse(country == "South Africa" & year == 
            1972, 5, cl), pr = ifelse(country == "South Africa" & 
            year == 1972, 6, pr), status = ifelse(country == 
            "South Africa" & year == 1972, "NF", status), status = as.factor(status), 
            fh_total = pr + cl, fh_total_reversed = 14 - fh_total, 
            country = plyr::mapvalues(country, from = c("Yemen, S.", 
                "Vietnam, S.", "Germany, E."), to = c("South Yemen", 
                "South Vietnam", "East Germany"), warn_missing = FALSE))
                

names(data)<-c("country", "year", "Political_Rights", "Civil_Rights", "status", "fh_total", "fh_total_reversed")


write_csv(data, "freedomhouse.csv")