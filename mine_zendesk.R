library(RJSONIO)
library(plyr)
library(RCurl)
library(dplyr)

## Lectura de datos
users         <- RJSONIO::fromJSON("users.json")
organizations <- RJSONIO::fromJSON("organizations.json")
satisfaction_ratings <- RJSONIO::fromJSON("satisfaction_ratings.json")

## Minar satisfaccion
requester   <- laply(satisfaction_ratings, function(t) t <- t$requester)
assignee    <- laply(satisfaction_ratings, function(t) t <- t$requester)
score       <- laply(satisfaction_ratings, function(t) t <- t$score)
comment     <- laply(satisfaction_ratings, function(t) t <- {
    if(is.null(t$comment)){
        NA
    }else{
        t$comment
    }
})
create_date <- laply(satisfaction_ratings, function(t) t <- t$created_at)

## dataset
satis_data  <-
    data.frame(requester = requester,
               score = score,
               comment = comment,
               create_date = create_date)

## Minar usuarios
requester    <- laply(users, function(t) t <- t$id)
url          <- laply(users, function(t) t <- t$url)
u_name       <- laply(users, function(t) t <- t$name)
email        <- laply(users, function(t) t <- t$email)
role         <- laply(users, function(t) t <- t$role)
organization <- laply(users, function(t) t <- {
    if(is.null(t$organization_id)){
        NA
    }else{
        t$organization_id
    }
})

## dataset
users_data   <- data.frame(requester = requester,
                          name = u_name, email = email,
                          role = role, organization = organization)
users_data   <- dplyr::filter(users_data, role == "end-user")

## Minar organizaciones
id    <- laply(organizations, function(t) t <- t$id)
org_name <- laply(organizations, function(t) t <- t$details)

## dataset
org_data <- data.frame(organization = id, org = org_name)

## dataset
satis_u <- merge(satis_data, users_data, by = "requester",    all = TRUE)
org_u   <- merge(satis_u,      org_data, by = "organization", all = TRUE)

## final data
final_data <- org_u[,3:ncol(satis_u)]
final_data$role <- NULL

## Escribir resultados
write.csv(final_data, "data_zen.csv", row.names = FALSE)
