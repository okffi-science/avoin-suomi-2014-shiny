## ------------------------------------ ##
## Datan käsittelyä                     ##
## ------------------------------------ ##
load("data/ess_arvot.rda")
df <- ess_arvot

library(plyr)


relevel_likert4 <- function(x) {
  x <- revalue(x, c("Allow many to come and live here"=4,
               "Allow some"=3,
               "Allow a few"=2,
               "Allow none"=1))
  x <- as.character(x)
   x[x %in% "Refusal"] <- NA
   x[x %in% "Don't know"] <- NA
   x[x %in% "No answer"] <- NA
  x <- factor(x, levels=c("4","3","2","1"))
}

df$imsmetn <- relevel_likert4(df$imsmetn)
df$imdfetn <- relevel_likert4(df$imdfetn)
df$impcntr <- relevel_likert4(df$impcntr)

# homo lesbo

relevel_likert5 <- function(x) {
  library(plyr)
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


df$freehms <- relevel_likert5(df$freehms)


# 



# 
# ## påalvelujärjestelm'
# 
# df$stfedu <- revalue(df$stfedu, c("Extremely bad"="0",
#                                     "Extremely good"="10"))  
# 
# df$stfhlth <- revalue(df$stfhlth, c("Extremely bad"="0",
#                                   "Extremely good"="10"))  
# 
# # onnellinen
# 
# df$happy <- revalue(df$happy, c("Extremely unhappy"="0",
#                                     "Extremely happy"="10"))  
# 
# # terveys
# 
# relevel_health <- function(x) {
#   x <- revalue(x, c("Very good"=5, 
#                     "Good"=4,
#                     "Fair"=3,
#                     "Bad"=2,
#                     "Very bad"=1))
#   x <- as.character(x)
#   x[x %in% "Refusal"] <- NA
#   x[x %in% "Don't know"] <- NA
#   x[x %in% "No answer"] <- NA
#   x <- factor(x, levels=c("5","4","3","2","1"))
# }
# 
# df$health <- relevel_health(df$health)
# 
# 
# relevel_satisfied <- function(x) {
#    revalue(x, c("Extremely dissatisfied"="0", 
#                 "Extremely satisfied"="10"))  
# }
# 
# df$stflife <- relevel_satisfied(df$stflife)
# df$stfeco <- relevel_satisfied(df$stfeco)
# df$stfgov <- relevel_satisfied(df$stfgov)
# df$stfdem <- relevel_satisfied(df$stfdem)
# 
# # # luottamus-muuttujat
# # df$trstprl <- relevel_trust(df$trstprl)
# # df$trstlgl <- relevel_trust(df$trstlgl)
# # df$trstplc <- relevel_trust(df$trstplc)
# # df$trstplt <- relevel_trust(df$trstplt)
# # df$trstprt <- relevel_trust(df$trstprt)
# # df$trstep <- relevel_trust(df$trstep)
# # df$trstun <- relevel_trust(df$trstun)
# # # ladataan graafifunktio
# # 
# 
