## ------------------------------------ ##
## Datan käsittelyä                     ##
## ------------------------------------ ##
load("data/ess_hyvinvointi.rda")
df <- ess_hyvinvointi

library(plyr)

relevel_likert5 <- function(x) {
  x <- revalue(x, c("Agree strongly"=5, 
               "Agree"=4,
               "Neither agree nor disagree"=3,
               "Disagree"=2,
               "Disagree strongly"=1))
  x <- as.character(x)
   x[x %in% "Refusal"] <- NA
   x[x %in% "Don't know"] <- NA
   x[x %in% "No answer"] <- NA
  x <- factor(x, levels=c("5","4","3","2","1"))
}

df$dngval <- relevel_likert5(df$dngval)
df$nhpftr <- relevel_likert5(df$nhpftr)
df$lotsgot <- relevel_likert5(df$lotsgot)
df$lfwrs <- relevel_likert5(df$lfwrs)
df$flclpla <- relevel_likert5(df$flclpla)


## påalvelujärjestelm'

df$stfedu <- revalue(df$stfedu, c("Extremely bad"="0",
                                    "Extremely good"="10"))  

df$stfhlth <- revalue(df$stfhlth, c("Extremely bad"="0",
                                  "Extremely good"="10"))  

# onnellinen

df$happy <- revalue(df$happy, c("Extremely unhappy"="0",
                                    "Extremely happy"="10"))  

# terveys

relevel_health <- function(x) {
  x <- revalue(x, c("Very good"=5, 
                    "Good"=4,
                    "Fair"=3,
                    "Bad"=2,
                    "Very bad"=1))
  x <- as.character(x)
  x[x %in% "Refusal"] <- NA
  x[x %in% "Don't know"] <- NA
  x[x %in% "No answer"] <- NA
  x <- factor(x, levels=c("5","4","3","2","1"))
}

df$health <- relevel_health(df$health)


relevel_satisfied <- function(x) {
   revalue(x, c("Extremely dissatisfied"="0", 
                "Extremely satisfied"="10"))  
}

df$stflife <- relevel_satisfied(df$stflife)
df$stfeco <- relevel_satisfied(df$stfeco)
df$stfgov <- relevel_satisfied(df$stfgov)
df$stfdem <- relevel_satisfied(df$stfdem)

# # luottamus-muuttujat
# df$trstprl <- relevel_trust(df$trstprl)
# df$trstlgl <- relevel_trust(df$trstlgl)
# df$trstplc <- relevel_trust(df$trstplc)
# df$trstplt <- relevel_trust(df$trstplt)
# df$trstprt <- relevel_trust(df$trstprt)
# df$trstep <- relevel_trust(df$trstep)
# df$trstun <- relevel_trust(df$trstun)
# # ladataan graafifunktio
# 

