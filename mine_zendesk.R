#! /usr/bin/Rscript

library(dplyr)
library(plyr)
library(stringr)
library(tidyr)

##############################
## Read in data
##############################
usr <- readLines("users.txt")
sat <- readLines("satisfaction_ratings.txt")
org <- readLines("organizations.txt")

##############################
## structure user
##############################
## requester
requester <- usr[str_detect(usr, "requester")] %>%
    lapply(function(t)t <- str_split(t, ":")[[1]][2]) %>%
    unlist() %>% str_replace(",","") %>% str_trim()

## name
name <- usr[str_detect(usr, "u_name:")] %>%
    lapply(function(t)t <- str_split(t, ":")[[1]][2]) %>%
    unlist() %>% str_replace(",","") %>% str_trim()

## role
role <- usr[str_detect(usr, "role:")] %>%
    lapply(function(t)t <- str_split(t, ":")[[1]][2]) %>%
    unlist() %>% str_replace(",","") %>% str_trim()

## org_usr
org_usr <- usr[str_detect(usr, "org:")] %>%
    lapply(function(t)t <- str_split(t, ":")[[1]][2]) %>%
    unlist() %>% str_replace(",","") %>% str_trim()

## email
email <- usr[str_detect(usr, "email:")] %>%
    lapply(function(t)t <- str_split(t, ":")[[1]][2]) %>%
    unlist() %>% str_replace(",","") %>% str_trim()

## datauser
data_usr <- data.frame(requester = requester,
                       name = name,
                       role = role,
                       id_org = org_usr,
                       email = email)

##############################
## structure satisfaction
##############################
## requester
sat_requester <- sat[str_detect(sat, "requester_id:")] %>%
    lapply(function(t)t <- str_split(t, ":")[[1]][2]) %>%
    unlist() %>% str_replace(",","") %>% str_trim()

## score
score <- sat[str_detect(sat, "score:")] %>%
    lapply(function(t)t <- str_split(t, ":")[[1]][2]) %>%
    unlist() %>% str_replace(",","") %>% str_trim()

## score
comment <- sat[str_detect(sat, "comment:")] %>%
    lapply(function(t)t <- str_split(t, ":")[[1]][2]) %>%
    unlist() %>% str_replace(",","") %>% str_trim()

## date
date <- sat[str_detect(sat, "date:")] %>%
    lapply(function(t)t <- str_split(t, ":")[[1]][2]) %>%
    unlist() %>% str_replace(",","") %>% str_trim() %>% as.Date()

data_sat <- data.frame(requester = sat_requester,
                      score = score,
                      comment = comment,
                      date = date)

##############################
## structure org
##############################
## id_org
id_org <- org[str_detect(org, "id:")] %>%
    lapply(function(t)t <- str_split(t, ":")[[1]][2]) %>%
    unlist() %>% str_replace(",","") %>% str_trim()

## org_name
org_name <- org[str_detect(org, "org_name:")] %>%
    lapply(function(t)t <- str_split(t, ":")[[1]][2]) %>%
    unlist() %>% str_replace(",","") %>% str_trim()

data_org <- data.frame(id_org = id_org,
                      org_name = org_name)

##############################
## putting all together
##############################
## satisfaction usr
sat_usr <- merge(data_sat, data_usr, by = "requester")

## satisfaction usr org
sat_usr_org <- merge(sat_usr, data_org, by = "id_org")

## final data
final_data <- sat_usr_org[,-c(1,2)]
final_data <- final_data[,c(4,5,6,7,1,2)]

## Write results
write.csv(final_data, "zendesk.csv", row.names = FALSE)
