## ------------------------------------ ##
## Datan käsittelyä                     ##
## ------------------------------------ ##
load("data/ess_luottamus.rda")
df <- ess_luottamus

library(plyr)
relevel_trust <- function(x) {
  revalue(x, c("No trust at all"="0", 
               "Complete trust"="10"))  
}

# luottamus-muuttujat
df$trstprl <- relevel_trust(df$trstprl)
df$trstlgl <- relevel_trust(df$trstlgl)
df$trstplc <- relevel_trust(df$trstplc)
df$trstplt <- relevel_trust(df$trstplt)
df$trstprt <- relevel_trust(df$trstprt)
df$trstep <- relevel_trust(df$trstep)
df$trstun <- relevel_trust(df$trstun)
# ladataan graafifunktio

df$ppltrst <- revalue(df$ppltrst, c("You can't be too careful"="0",
                                    "Most people can be trusted"="10"))  

df$pplfair <- revalue(df$pplfair, c("Most people try to take advantage of me"="0",
                                    "Most people try to be fair"="10"))  

df$pplhlp <- revalue(df$pplhlp, c("People mostly look out for themselves"="0",
                                    "People mostly try to be helpful"="10")) 
