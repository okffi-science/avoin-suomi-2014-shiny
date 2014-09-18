# # # library(foreign)
# # # ess <- read.dta("~/data/ess/integr_ed20_round6/ESS6e02.dta")
# # # save(ess, file="~/data/ess/integr_ed20_round6/ESS6e02.rda")
# 
# # Load Ess data
# load("~/data/ess/integr_ed20_round6/ESS6e02.rda")
# # 
# # tee GB's UK (jotta kartat pelaa)
# ess$cntry[ess$cntry == "GB"] <- "UK"
# 
# #liitetään maiden suomenkieliset nimet ja regiimien nimet
# library(RCurl)
# GHurl <- getURL("https://raw.githubusercontent.com/muuankarski/data/master/world/countries_continents.csv")
# dat <- read.csv(text = GHurl)
# dat <- dat[!duplicated(dat[c("cCode2")]),]
# # tee GB's UK (jotta kartat pelaa)
# dat$cCode2 <- as.character(dat$cCode2)
# dat$cCode2[dat$cCode2 == "GB"] <- "UK"
# ess <- merge(ess,dat,
#             by.x="cntry",
#             by.y="cCode2",
#             all.x=TRUE)
# 
# 
# # luottamus shinyn muuttujat
# ess_luottamus <- ess[c("cntry","idno","pweight",
#              "trstprl","trstlgl","trstplc","trstplt","trstprt","trstep","trstun", # valtaan
#              "ppltrst","pplfair","pplhlp", # toiseen ihmiseen
#              "maa","ryhma1")]
# 
# save(ess_luottamus, file="data/ess_luottamus.rda")
# 
# # hyvinvointi shinyn muuttujat
# ess_hyvinvointi <- ess[c("cntry","idno","pweight","maa","ryhma1",
#                          "dngval","nhpftr","lotsgot","lfwrs","flclpla", # mikä fiilis asioiden tolasta
#                          "deaimpp", # Kuinka vaikeata tai helppoa Teidän on selvitä elä
#                          "health", # terveydentyila
#                          "happy", # kuinka onnellinen olette
#                          "stflife","stfeco","stfgov","stfdem", # tyytyväinen
#                          "stfedu","stfhlth", # palvelut
#                          "gincdif", # Valtiovallan pitäisi ryhtyä toimenpiteisiin tuloerojen vähentämiseksi?
#                          "plinsoc")] # huippua
# 
# 
# save(ess_hyvinvointi, file="data/ess_hyvinvointi.rda")
# 
# arvot shinyn muuttujat
# ess_arvot <- ess[c("cntry","idno","pweight","maa","ryhma1",
#                    "imsmetn","imdfetn","impcntr", # mamu kuinka paljon
#                    "imbgeco",
#                    "imueclt",
#                    "imwbcnt",
#                    "freehms")] # homo
# 
# save(ess_arvot, file="data/ess_arvot.rda")
# 
# 
# 
# ## Maatason shapefilet!
# 
# #load EU country shapefile from GISco
# download.file("http://epp.eurostat.ec.europa.eu/cache/GISCO/geodatafiles/CNTR_2010_60M_SH.zip",
#                          destfile="zipfile")
# unzip("zipfile")
# library(rgdal)
# # read into SpatialPolygonsDataFrame
# map <- readOGR(dsn = "./CNTR_2010_60M_SH/data", layer = "CNTR_RG_60M_2010")
# # shapen yksinkertaistus
# library(rgdal)
# sdata <- as.data.frame(map)
# sdata <- sdata[,1:7]
# 
# shapefileSimple <- rgeos::gSimplify(spgeom=map, tol=0.0060)
# map <- SpatialPolygonsDataFrame(shapefileSimple, data=sdata)
# 
# library(ggplot2)
# library(rgeos)
# map <- gBuffer(map, width=0, byid=TRUE)
# map$id <- rownames(map@data)
# map.points <- fortify(map, region = "id")
# map.df <- merge(map.points, map, by = "id")
# # valitse vaan ne maat, jotka ovat ESS datassa
# ess_maat <- as.character(levels(factor(ess$cntry)))
# map.df <- map.df[map.df$CNTR_ID %in% ess_maat,]
# 
# save(map.df, file="data/map.df.rda")
# 
# 
# ## Aluetason shapefile
# 
# 
# #load EU country shapefile from GISco
# download.file("http://epp.eurostat.ec.europa.eu/cache/GISCO/geodatafiles/NUTS_2010_60M_SH.zip",
#               destfile="zipfile")
# unzip("zipfile")
# library(rgdal)
# # read into SpatialPolygonsDataFrame
# map <- readOGR(dsn = "./NUTS_2010_60M_SH/data", layer = "NUTS_RG_60M_2010")
# # shapen yksinkertaistus
# library(rgdal)
# sdata <- as.data.frame(map)
# #sdata <- sdata[,1:7]
# 
# shapefileSimple <- rgeos::gSimplify(spgeom=map, tol=0.0055)
# map <- SpatialPolygonsDataFrame(shapefileSimple, data=sdata)
# 
# library(ggplot2)
# library(rgeos)
# map <- gBuffer(map, width=0, byid=TRUE)
# map$id <- rownames(map@data)
# map.points <- fortify(map, region = "id")
# map.df <- merge(map.points, map, by = "id")
# # valitse vaan ne maat, jotka ovat ESS datassa
# #ess_maat <- as.character(levels(factor(ess$cntry)))
# map.df <- map.df[map.df$CNTR_ID %in% ess_maat,]
# 
# save(map.df, file="data/map.df.nuts.rda")
