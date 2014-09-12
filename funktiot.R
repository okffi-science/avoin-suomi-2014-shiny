


create_dfplot <- function(var) {
  # load data
  # munge key variable
  bar <- function(x) as.character(eval(parse(text=x)) )
  variable <- as.factor(bar(var))
  cntry <- df$maa
  code <- df$cntry
  regime_fi <- df$ryhma1
  pweight <- df$pweight # painotukset
  idno <- df$idno # painotukset
  dat <- data.frame(cntry,variable,regime_fi,pweight,idno,code)
  
  df.plot <- dat[dat$variable %in% 0:10,]
  df.plot[[2]] <- as.numeric(levels(df.plot[[2]]))[df.plot[[2]]]
  df.plot
}




ess_jakauma <- function(var, value) {

  df.plot <- create_dfplot(var)
    
    # order countries 
    library(plyr)
    library(grid)
    order.data <- ddply(df.plot, .(cntry), summarise, 
                        mean    = mean(variable, na.rm=TRUE))
    
    order.data <- order.data[order(order.data$mean), ]
    df.plot$cntry <- factor(df.plot$cntry,
                            levels = order.data$cntry)  

  df.plot$cntry <- factor(df.plot$cntry)
  library(survey)
  d.df <- svydesign(id = ~idno, 
                    weights = ~pweight, 
                    data = df.plot)
  df.plot2 <- data.frame(prop.table(svytable(~variable+cntry, d.df),2)*100)
  names(df.plot2)[3] <- "rel"
  df.plot2$variable <- as.numeric(levels(df.plot2$variable))[df.plot2$variable]
  
  library(ggplot2)
  ggplot(data=df.plot2,
         aes(x=variable,y=rel,
             fill=variable)) + geom_bar(stat="identity") +
    facet_wrap(~cntry) +
    scale_fill_gradient(low="red", high="green") +
    annotate("segment", x = value, xend = value,
             y=0, yend = 30,
             color = "red",
             size=0.4,
             linetype="dashed") +
    theme_minimal() +
    theme(legend.position="none") +
    theme(axis.title.x = element_blank())
}

ess_keskiarvo <- function(var, value) {

  df.plot <- create_dfplot(var)
  
  library(survey)
  d.df <- svydesign(id = ~idno, 
                    weights = ~pweight, 
                    data = df.plot)
  data_mean <- as.numeric(svymean(~as.numeric(variable),d.df)[1])
  
  dat.plot <- data.frame(svyby(~as.numeric(variable), ~cntry+regime_fi, design=d.df, svymean, na.rm=T))
  # New names for data file
  names(dat.plot) <- c("maa","regime_fi","value","se")
  # order countries by median income
  
  if (max(dat.plot$value) > 5) maksiimi <- 10
  if (max(dat.plot$value) < 5 & max(dat.plot$value) > 4) maksiimi <- 5
  if (max(dat.plot$value) < 4.01) maksiimi <- 4
  
  library(ggplot2)
  ggplot(data=dat.plot,
         aes(x=reorder(maa, value, max),
             y=value, fill=regime_fi)) + 
    geom_bar(stat="identity") +
    # vastaaja
    annotate("segment", x = 0, xend = 29,
             y=value, yend = value,
             color = "red",
             size=1,
             linetype="dashed") +
    annotate("text", x = 5, y=value+0.5, 
             color = "red",
             size=5,label="Sinä!") +
    # datan keskiarvo
    annotate("segment", x = 0, xend = 29,
             y=data_mean, yend = data_mean,
             color = "black",
             size=1,
             linetype="dashed") +
    annotate("text", x = 20, y=data_mean-0.5, 
             color = "black",
             size=5,label=paste0("Euroopan keskiarvo = ",round(data_mean,1))) +
    theme_minimal() +
    scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9", 
                               "#009E73","#D55E00", "#CC79A7","#0072B2","#F0E442")) +
    theme(axis.text.x  = element_text(angle=90, vjust= 0.5)) +
    theme(legend.position = "top") +
    theme(legend.title = element_blank()) +
    theme(legend.direction = "horizontal") +
    theme(axis.title = element_blank()) +
    coord_cartesian(ylim=c(0,maksiimi))
  
}


ess_kartta <- function(var, value) {
  
  load("data/map.df.rda")
  df.plot <- create_dfplot(var)
  
  # calculate means for plotting
  library(plyr)
  library(grid)
  df.mean <- ddply(df.plot, .(code), summarise, 
                   mean    = mean(variable, na.rm=TRUE))
  map.df.l <- merge(map.df,df.mean,by.x="CNTR_ID",by.y="code")
  
  map.df.l <- map.df.l[order(map.df.l$order), ]
  
  
  library(ggplot2)
   library(mapproj)
   library(maps)
  ggplot(data=map.df.l,
         aes(long,lat,group=group)) +
    geom_polygon(aes(fill = mean),
                 colour="white",
                 size=.2,
                 alpha=.7) +
    geom_polygon(data = map.df.l, aes(long,lat),
                 fill=NA,
                 colour="white",
                 size = 0.1) + 
    scale_fill_gradient(low="red",
                         high="green") +
     coord_cartesian(xlim=c(-25,45),
                     ylim=c(15,70)) +
    #coord_map(xlim=c(-45,65),ylim=c(25,70)) +
    coord_map(project="orthographic", xlim=c(-15,34),ylim=c(25,70)) +
    theme_minimal() +
    theme(axis.title = element_blank())
}
