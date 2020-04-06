# ////////////////////////////////////////////////
#
#   IPE Term Paper Code
#   ---------------------------------------------
#   Jessica Draper
#   University of Mannheim
#   Spring 2019
#
# ////////////////////////////////////////////////

# ===============================================
#  Packages
# ===============================================

p_needed <- c("readxl", "tidyverse", "grid", "viridis", "dplyr", "lubridate", "maps","mapdata","maptools", "mapview", "rgdal","ggmap","leaflet","tigris", "sp", "ggplot2","animation","gridExtra","psych","rstudioapi","data.table", "cshapes", "htmltools", "rgeos", "raster", "scales", "spData", "shiny", "pracma", "geosphere", "spatialEco", "merTools", "countrycode", "knitr", "kableExtra", "zeligverse", "MASS", "Hmisc", "foreign", "reshape2","stargazer","ggrepel")
packages <- rownames(installed.packages())
p_to_install <- p_needed[!(p_needed %in% packages)]
if (length(p_to_install) > 0) {
  install.packages(p_to_install)
}
sapply(p_needed, require, character.only = TRUE)

select <- dplyr::select
group_by <- dplyr::group_by
summarise <- dplyr::summarise

options(scipen=999)

# ===============================================
#  Load Data
# ===============================================

getwd()
setwd("/Users/Jess/Documents/School/Spring 2019/IPE/Term Paper/Data")


# -----------------------------------------------
#  Prelimiary data
# -----------------------------------------------

# ----- Trade over time -----

trade <- read_csv("trade.csv")
trade <- melt(trade) %>% select(variable, value) %>% group_by(variable) %>% summarise(value = mean(value, na.rm = TRUE)) %>% transmute(year = variable, trade = value)
trade$year <- as.numeric(as.character(trade$year))
year.breaks <- seq(min(trade$year), max(trade$year),5)

trade.plot <- ggplot(trade, aes(year, trade)) + geom_line() + theme_light() + labs(title = "", x="Year", y="Trade (% of GDP)") + scale_x_continuous(breaks = year.breaks)

ggsave("trade.jpg", trade.plot, width = 7, height = 3.5)

# Most variables are 5-year means

# ----- UN Members -----

un_memb <- read_xlsx("un-members.xlsx")
un_memb <- un_memb %>% mutate(ccode = countrycode(un_memb$country, "country.name", "genc3c"))
un_memb <- un_memb %>% mutate(cname = countrycode(un_memb$ccode, "genc3c", "country.name"))
193-178


# -----------------------------------------------
#  Model data
# -----------------------------------------------

# ----- ATT Status ----- #
# https://treaties.un.org/pages/ViewDetails.aspx?src=TREATY&mtdsg_no=XXVI-8&chapter=26&clang=_en
# https://unoda-web.s3-accelerate.amazonaws.com/wp-content/uploads/assets/ATT/docs/ATT_info_kit.pdf

att <- read_xlsx("att_status.xlsx")
table(att$weak_support)
att$att_support <- NA
for (i in 1:nrow(att)) {
  if (!is.na(att$ratification[i])) {
    att$att_support[i] <- "Strong Support"
  }
}

for (i in 1:nrow(att)) {
  if (!is.na(att$signatory[i]) & is.na(att$ratification[i])) {
    att$att_support[i] <- "Weak Support"
  }
}

att$att_support[is.na(att$att_support)] <- "No support"

table(att$att_support)
table(att$ratification)
table(att$signatory)

att <- att %>% mutate(cname = countrycode(att$cname, "country.name", "country.name")) %>% select(cname, att_support)

# ----- Arms Exporter -----

export <- read_csv("country-tiv.csv")
export <- export[c(1:66,68),]
export <- export %>% mutate(cname = countrycode(export$country, "country.name", "country.name")) %>% select(cname, arms_export = Average)

summary(tiv$arms_export) # 1st quant = 1.3

# ----- Arms Impoter -----

import <- read_csv("arms_importers.csv")
import_rm <- c("Syria rebels*", "Libya GNC", "PKK (Turkey)*", "Ukraine Rebels*", "Huthi rebels (Yemen)*")
import <- import %>% filter(!X1 %in% import_rm)
import <- import %>% mutate(cname = countrycode(X1, "country.name", "country.name")) %>% select(cname, arms_import = mean) %>% drop_na(cname)

# ----- Conflict -----

conflict <- read_csv("gedconflict.csv")
conflict <- conflict %>% select(year, type_of_violence, country, deaths_civilians) %>% filter(year >= 2013) 

conflict.count <- conflict %>% group_by(country, year) %>% tally() %>% group_by(country) %>% summarise(conflict = sum(n)) %>% transmute(cname = countrycode(country, "country.name", "country.name"), conflict = conflict/5) # average over 5-year period
conflict.count$cname[conflict.count$cname == "Yemen Arab Republic"] <- "Yemen"

severity <- conflict %>% group_by(country) %>% summarise(deaths = sum(deaths_civilians)) %>% transmute(cname = countrycode(country, "country.name", "country.name"), deaths) # divide by pop?
severity$cname[severity$cname == "Yemen Arab Republic"] <- "Yemen"

conflict.data <- full_join(conflict.count, severity, by = "cname")

# compute lower and upper whiskers
deaths.lim <- boxplot.stats(conflict.data$deaths)$stats[c(1, 5)]
conflict.lim <- boxplot.stats(conflict.data$conflict)$stats[c(1, 5)]
conflict.data <- conflict.data %>% filter(conflict > conflict.lim[1] & conflict < conflict.lim[2]) %>% filter(deaths > deaths.lim[1] & deaths < deaths.lim[2]) 

cor(conflict.data$conflict, conflict.data$deaths)

ggplot(conflict.data, aes(conflict, deaths)) + geom_point() + geom_smooth(method = "lm")

# average conflict occurence count is highly correlated with same period sum of civilian deaths, so just going to opt for using conflict occurence

# dummifying this variable because only a few states have experienced this kind of armed conflict
conflict <- conflict.count %>% mutate(conflict_dummy = 0)
conflict$conflict_dummy[conflict$conflict >= 1] <- 1
conflict <- conflict %>% select(cname, conflict = conflict_dummy)


# Alternative measure: QoG data, perception of political instability/violence
threat <- read_xlsx("capacity.xlsx")
threat <- threat %>% transmute(cname = countrycode(cname, "country.name", "country.name"), threat = wbgi_pve)
threat$threat <- -(threat$threat)

# ----- CINC (state capacity) -----

cinc <- read_csv("cinc.csv")
cinc <- cinc %>% filter(year == 2012) %>% mutate(cname = countrycode(stateabb, "cowc", "country.name")) %>% select(cname,cinc)
cinc$cname[cinc$cname == "Yugoslavia"] <- "Serbia"
cinc$cname[175] <- "Vietnam"

# security apparatus: https://fragilestatesindex.org/indicators/c1/

# Get the files names, then read as csv file, then rbind to combine them all
setwd("/Users/Jess/Documents/School/Spring 2019/IPE/Term Paper/Data/fsi")
files <- list.files(pattern="*.xlsx")
fsi <- do.call(rbind, lapply(files, function(x) read_xlsx(x)))
fsi$Year <- year(fsi$Year)
setwd("/Users/Jess/Documents/School/Spring 2019/IPE/Term Paper/Data")

security <- fsi %>% select(cname = Country, year = Year, security_apparatus = `C1: Security Apparatus`) %>% group_by(cname) %>% summarise(security_apparatus = mean(security_apparatus)) %>% transmute(cname = countrycode(cname, "country.name", "country.name"), security_apparatus)
security$security_apparatus <- -(security$security_apparatus) + 10 # reversing index so high = strong security
security[79,1] <- "Israel"

# fragile states; includes: Security Apparatus, Factionalized Elites, Group Grievance, Economy, Economic Inequality, Human Flight and Brain Drain, State Legitimacy, Public Services, Human Rights, Demographic Pressures, Refugees and IDPs, External Intervention

fragile <- read_xlsx("fragile.xlsx")
fragile <- fragile %>% transmute(cname = countrycode(cname, "country.name", "country.name"), fragile)
fragile[79,1] <- "Israel"
fragile[106,1] <- "Micronesia (Federated States of)"

# government effectiveness: https://www.qogdata.pol.gu.se/data/qog_bas_jan19.pdf
gov_eff <- read_xlsx("capacity.xlsx")
gov_eff <- gov_eff %>% transmute(cname = countrycode(cname, "country.name", "country.name"), gov_eff = wbgi_gee)

# ----- Democracy -----

polity <- read_xls("polity.xls")
polity <- polity %>% select(cname = country, year, polity2) %>% filter(year >= 2013) %>% drop_na() %>% group_by(cname) %>% summarise(polity = mean(polity2)) %>% transmute(cname = countrycode(cname, "country.name", "country.name"), polity)

summary(polity$polity)

# ----- Refugees ----
# http://popstats.unhcr.org/en/time_series

refugee <- read.csv("refugee.csv")
refugee <- refugee %>% select(Year,Origin,Value)
refugee$Value[refugee$Value == "*"] <- "1"
refugee$Value <- as.numeric(as.character(refugee$Value))

refugee$Origin <- as.character(refugee$Origin)
refugee$Origin[refugee$Origin == "Central African Rep."] <- "Central African Republic"
refugee$Origin[refugee$Origin == "Serbia and Kosovo (S/RES/1244 (1999))"] <- "Serbia"

refugee <- refugee %>% group_by(Origin) %>% summarise(mean = mean(Value))

refugee <- refugee %>% mutate(cname = countrycode(refugee$Origin, "country.name", "country.name")) %>% select(cname, refugee = mean) %>% drop_na(cname)

# ----- Homicides ----- # Rate, per 100,000 people https://dataunodc.un.org/crime/intentional-homicide-victims

wb_hom <- read_csv("worldbank-homicides.csv")
wb_hom$avg <- NA

for (i in 1:nrow(wb_hom)) {
  rates.vector <- as.numeric(unname(wb_hom[i,3:24]))
  rates.vector <- na.omit(rates.vector)
  wb_hom$avg[i] <- mean(tail(rates.vector, 5))
}

wb_hom$avg[is.nan(wb_hom$avg)] <- NA

homicide <- wb_hom %>% transmute(cname = countrycode(`Country Name`, "country.name", "country.name"), homicide = avg)


# homicides from https://qog.pol.gu.se/data/datadownloads/qogbasicdata
homicide <- read_xlsx("capacity.xlsx")
homicide <- homicide %>% transmute(cname = countrycode(cname, "country.name", "country.name"), homicide = wdi_homicides)

# un data
homicide2 <- read_excel("homicide.xlsx")
homicide2 <- homicide2 %>% ungroup() %>% mutate(mean = rowMeans(homicide[,2:6])) %>% mutate(ccode = countrycode(homicide$Country, "country.name", "country.name")) %>% select(ccode, homicide = mean) %>% group_by(ccode) %>% summarise(homicide = mean(homicide))

sms_data <- read_excel("sms_violence.xlsx") %>% select(ccode, cname, violentdeaths_avg = vd_avg, homicide_2016, gundeaths_2016 = firearm_deaths_2016)

guns <- select(sms_data, ccode, gundeaths_2016)

# ----- GDP ----- # logged per capita # https://data.worldbank.org/indicator/NY.GDP.MKTP.PP.CD?end=2017&start=2013

gdp <- read.csv("gdp2.csv")
gdp <- gdp %>% ungroup() %>% mutate(mean = rowMeans(gdp[,3:7])) %>% mutate(cname = countrycode(gdp$Country.Code, "genc3c", "country.name")) %>% select(cname, gdp = mean) %>% drop_na()
gdp$gdp <- log(gdp$gdp)

# ----- Humaniarian Aid ----- # % of gross national incom # https://data.oecd.org/oda/net-oda.htm

aid <- read.csv("aid.csv")
aid <- aid %>% group_by(country) %>% summarise(mean = mean(value)) %>% transmute(cname = countrycode(country, "genc3c", "country.name"), aid = mean)

# ----- Common Law -----

commonlaw <- read_xlsx("commonlaw.xlsx")
commonlaw$commonlaw <- 0
commonlaw$commonlaw[commonlaw$`LEGAL SYSTEM`=="Common law"] <- 1
commonlaw <- commonlaw %>% mutate(cname = countrycode(`MEMBER STATES`, "country.name", "country.name"))
commonlaw$cname[commonlaw$`MEMBER STATES` == "BOLIVIE"] <- "Bolivia"
commonlaw$cname[commonlaw$`MEMBER STATES` == "CENTRAL AFRICA REPUBLIC"] <- "Central African Republic"
commonlaw$cname[commonlaw$`MEMBER STATES` == "ISRAËL"] <- "Israel"
commonlaw$cname[commonlaw$`MEMBER STATES` == "MICRONESIA"] <- "Micronesia (Federated States of)"
commonlaw$cname[commonlaw$`MEMBER STATES` == "MOLDAVIA"] <- "Moldova"
commonlaw <- commonlaw %>% select(cname, commonlaw) %>% drop_na

# ----- Prior UN Activity ----- https://treaties.un.org/Pages/AdvanceSearch.aspx?isBack=true&tab=UNTS&clang=_en

treaties <- read_xlsx("treaties.xlsx")
treaties <- treaties %>% transmute(cname = countrycode(country, "country.name", "country.name"), treaties)

# ----- COMBINE ALL -----

df <- att %>% full_join(export, by="cname") %>% full_join(import, by = "cname") %>% full_join(conflict.count, by = "cname") %>% full_join(security, by = "cname") %>% full_join(aid, by = "cname") %>% full_join(commonlaw, by = "cname") %>% full_join(treaties, by = "cname") %>% full_join(polity, by = "cname") %>% full_join(refugee, by ="cname") %>% full_join(cinc, by = "cname") %>% full_join(threat, by = "cname") %>% full_join(gdp, by="cname") %>% full_join(gov_eff, by="cname")

df <- df[which(df$cname %in% att$cname),]
table(df$cname)

# ordering IV
df$att_support <- factor(df$att_support, ordered = TRUE,
                         levels = c("No support", "Weak Support", "Strong Support"))

# Assuming no data = 0 or close to it
df$arms_export[is.na(df$arms_export)] <- 0 
df$arms_import[is.na(df$arms_import)] <- 0 
df$aid[is.na(df$aid)] <- 0
df$conflict[is.na(df$conflict)] <- 0
#df$conflict <- base::scale(df$conflict)

# alternative dfs for testing models
df1 <- df %>% select(cname, att_support, arms_export, arms_import, conflict, security_apparatus, aid, commonlaw, treaties) %>% mutate(conflict = base::scale(conflict))
df2 <- df %>% select(cname, att_support, arms_export, arms_import, refugee, gov_eff, aid, commonlaw, treaties) %>% mutate(refugee = base::scale(refugee))
df3 <- df %>% select(cname, att_support, arms_export, arms_import, conflict, security_apparatus, aid, commonlaw, treaties, polity, gdp) %>% mutate(conflict = base::scale(conflict))

# Dropping NAs of irrelevant members
df1 <- drop_na(df1)
df2 <- drop_na(df2)
df3 <- drop_na(df3)

summary(df)
write.csv(df, "ipe-df.csv")

# ===============================================
#  Figures, Visual Aids
# ===============================================

theme_jess <- function () {
  
  theme_light() +
  theme(axis.title.x = element_text(margin = margin(t = 10))) +
  theme(axis.title.y = element_text(margin = margin(r = 10))) +
  
  theme(strip.text = element_text(color = "#222222"),
        strip.background = element_rect(fill = "#e5e5e5"))
}

viz <- df %>% select(cname, att_support, arms_export, arms_import, conflict, security_apparatus, aid, commonlaw, treaties)

# Dependent Variable

dv <- ggplot(viz, aes(att_support)) + geom_bar(stat="count", width = .7) + theme_jess() + labs(x = "Level of support for the Arms Trade Treaty", y = "Number of Countries") + geom_text(stat="count", aes(label = paste("n =",..count..)), position = position_stack(vjust = .5), size = 3, color = "white")
ggsave("dv.jpg", dv, width = 6, height = 3)

# Independent Variables

# average TIV, 2014-2018
arms <- ggplot(viz, aes(x=arms_import, y=arms_export, label=cname)) + geom_point(alpha =.4, size = 2) + theme_jess() + labs(y = "Arms Exports (TIV)", x = "Arms Imports (TIV)", title = "Arms Imports and Exports") + geom_text_repel(aes(label=ifelse(arms_export>1200, as.character(cname), "")), size = 3) + geom_text_repel(aes(label=ifelse(arms_import>1200, as.character(cname), "")), size = 3)

# Average Conflict Frequency (2013 - 2017)
negext <- ggplot(viz, aes(x=security_apparatus, y=conflict, label=cname)) + geom_point(alpha =.4, size = 2) + theme_jess() + labs(y = "Conflict Frequency", x = "Security Apparatus", title = "Conflict and State Capacity") + geom_text_repel(aes(label=ifelse(security_apparatus<.57, as.character(cname), "")), size = 3)

ivs <- grid.arrange(arms, negext, ncol = 2)
ggsave("ivs.jpg", ivs, width = 12, height = 4)

# ===============================================
#  Ordered Logit Model
# ===============================================

# main model
m1 <- polr(att_support ~ arms_export + arms_import + conflict*security_apparatus + aid + commonlaw + treaties, data = df1, start = c(rep(0, 8), -2, 0), Hess = TRUE)
summary(m1)

# diff operationalization
m2 <- polr(att_support ~ arms_export + arms_import + refugee*gov_eff + aid + commonlaw + treaties, data = df2, start = c(rep(0, 8), -2, 0), Hess = TRUE)
summary(m2)

# added controls: polity + gdp
m3 <- polr(att_support ~ arms_export + arms_import + conflict*security_apparatus + aid + commonlaw + treaties + polity + gdp, data = df3, start = c(rep(0, 10), -2, 0), Hess = TRUE)
summary(m3)

stargazer(m1, m2, m3, keep.stat = c("n", "res.dev", "aic"))

# ----- Robustness -----

# Model 1
predict_results1 <- data.frame(y =df1$att_support, y.hat = predict(m1))
predict_results1 <- predict_results1 %>% mutate_if(is.factor, as.character)
predict_results1$correct <- 0
predict_results1$correct[predict_results1$y == predict_results1$y.hat] <- 1
table(predict_results1$correct)[[2]]/nrow(df1) # 66.9%

# Model 2
predict_results2 <- data.frame(y =df2$att_support, y.hat = predict(m2))
predict_results2 <- predict_results2 %>% mutate_if(is.factor, as.character)
predict_results2$correct <- 0
predict_results2$correct[predict_results2$y == predict_results2$y.hat] <- 1
table(predict_results2$correct)[[2]]/nrow(df2) # 66.3%

# Model 3
predict_results3 <- data.frame(y =df3$att_support, y.hat = predict(m3))
predict_results3 <- predict_results3 %>% mutate_if(is.factor, as.character)
predict_results3$correct <- 0
predict_results3$correct[predict_results3$y == predict_results3$y.hat] <- 1
table(predict_results3$correct)[[2]]/nrow(df3) # 67.7%

# -----------------------------------
# Predicted Probabilities Graphs
# -----------------------------------

# Setting Xs

X1 <- cbind(df1$arms_export, df1$arms_import, df1$conflict, df1$security_apparatus, df1$aid, df1$commonlaw, df1$treaties, df1$conflict*df1$security_apparatus)
X2 <- cbind(df2$arms_export, df2$arms_import, df2$refugee, df2$gov_eff, df2$aid, df2$commonlaw, df2$treaties, df2$refugee*df2$gov_eff)
X3 <- cbind(df3$arms_export, df3$arms_import, df3$conflict, df3$security_apparatus, df3$aid, df3$commonlaw, df3$treaties, df3$polity, df3$gdp, df3$conflict*df3$security_apparatus)

cats <- c("No support", "Weak Support", "Strong Support")
J <- length(cats) # number of categories we have


# ----- Category function, observed value approach -----

cat.values <- function(scenario, model, sel, X) {
  
  cases <- array(NA, c(dim(X), length(scenario)))
  cases[, , ] <- X
  sel <- sel # The column of X that contains variable of interest
  
  for (i in 1:length(scenario)){
    cases[, sel, i] <- scenario[i]  
  }
  
  U <- matrix(nrow = nrow(X), ncol = length(scenario))
  
  for(i in 1:length(scenario)){
    U[, i] <- cases[,,i] %*% unname(model$coefficients)
  }
  
  probs <- array(NA, c(dim(Z), length(scenario)))
  
  # get thresholds
  tau <- sort(unname(model$zeta))
  
  for(i in 1:length(scenario)){
    # calculate probabilities
    probs[, 1, i] <- exp(tau[1] - U[, i])/(1 + exp(tau[1] - U[, i]))
    
    for (j in 2:(ncol(Z) - 1)){
      probs[, j, i] <- exp(tau[j] - U[, i])/(1 + exp(tau[j] - U[, i])) - 
        exp(tau[j-1] - U[, i])/(1 + exp(tau[j - 1] - U[, i]))
    } 
    
    probs[, ncol(Z), i] <- 1 - exp(tau[J - 1] - U[, i])/(1 + exp(tau[J - 1] - U[, i])) 
  }
  
  # Now the means of the observed values
  
  ovaP <- matrix(ncol = ncol(Z), nrow = length(scenario))
  
  for(i in 1:length(scenario)){
    ovaP[i,] <- apply(probs[, , i], 2, mean)
  }
  
  plotdf <- data.frame(ovaP)
  names(plotdf) <- c("No support", "Weak support", "Strong support")
  
  plotdf <- cbind(melt(plotdf), scenario)
  return(plotdf)
}

# --- Specifying scenarios ----

cat_scenarios <- function(X, model, title3, title4) {
  
  export.scenario <- seq(min(X[,1]), max(X[,1]) ,length.out = 100)
  import.scenario <- seq(min(X[,2]), max(X[,2]) ,length.out = 100)
  conflict.scenario <- seq(min(X[,3]), max(X[,3]) ,length.out = 100)
  capacity.scenario <- seq(min(X[,4]), max(X[,4]) ,length.out = 100)
  aid.scenario <- seq(min(X[,5]), max(X[,5]) ,length.out = 100)
  treaties.scenario <- seq(min(X[,7]), max(X[,7]) ,length.out = 100)
  
  export.qoi <- cat.values(export.scenario, model, 1, X)
  import.qoi <- cat.values(import.scenario, model, 2, X)
  #conflict.qoi <- cat.values(conflict.scenario, model, 3, X)
  #capacity.qoi <- cat.values(capacity.scenario, model, 4, X)
  aid.qoi <- cat.values(aid.scenario, model, 5, X)
  treaties.qoi <- cat.values(treaties.scenario, model, 7, X)
  
  conflict.low_sa <- int.sim(conflict.scenario, model, 3, 4, 0, X)
  conflict.high_sa <- int.sim(conflict.scenario, model, 3, 4, max(X[,4]), X)
  
  gg1 <- ggplot(export.qoi, aes(x = scenario, y = value, fill = variable, group = variable)) + 
    geom_area(position = "stack", colour = "black", alpha = 0.8) + 
    theme_bw() + scale_fill_brewer(palette = "Greys") + labs(title = "Arms Exports", x = "Arms Exports", y = "Predicted Probabilities", fill = "ATT Support") + theme(legend.position = "bottom")
  
  gg2 <- ggplot(import.qoi, aes(x = scenario, y = value, fill = variable, group = variable)) + 
    geom_area(position = "stack", colour = "black", alpha = 0.8) + 
    theme_bw() + scale_fill_brewer(palette = "Greys") + labs(title = "Arms Imports", x = "Arms Imports", y = "Predicted Probabilities", fill = "ATT Support") + theme(legend.position = "bottom")
  
  #gg3 <- ggplot(conflict.qoi, aes(x = scenario, y = value, fill = variable, group = variable)) + 
  #  geom_area(position = "stack", colour = "black", alpha = 0.8) + 
  #  theme_bw() + scale_fill_brewer(palette = "Greys") + labs(title = "Conflict", x = "Conflict", y = "Predicted Probabilities", #fill = "ATT Support") + theme(legend.position = "bottom")
  
  #gg4 <- ggplot(capacity.qoi, aes(x = scenario, y = value, fill = variable, group = variable)) + 
  #  geom_area(position = "stack", colour = "black", alpha = 0.8) + 
  #  theme_bw() + scale_fill_brewer(palette = "Greys") + labs(title = "Security Apparatus", x = "Security Apparatus", y = #"Predicted Probabilities", fill = "ATT Support") + theme(legend.position = "bottom")
  
  low.sa <- ggplot(conflict.low_sa, aes(x = scenario, y = value, fill = variable, group = variable)) + 
    geom_area(position = "stack", colour = "black", alpha = 0.8) + 
    theme_bw() + scale_fill_brewer(palette = "Greys") + labs(title = paste(title3," (weak ",title4,")", sep = ""), x = "Conflict", y = "Predicted Probabilities", fill = "ATT Support") + theme(legend.position = "bottom")
  
  high.sa <- ggplot(conflict.high_sa, aes(x = scenario, y = value, fill = variable, group = variable)) + 
    geom_area(position = "stack", colour = "black", alpha = 0.8) + 
    theme_bw() + scale_fill_brewer(palette = "Greys") + labs(title = paste(title3," (strong ",title4,")", sep = ""), x = "Conflict", y = "Predicted Probabilities", fill = "ATT Support") + theme(legend.position = "bottom")
  
  gg5 <- ggplot(aid.qoi, aes(x = scenario, y = value, fill = variable, group = variable)) + 
    geom_area(position = "stack", colour = "black", alpha = 0.8) + 
    theme_bw() + scale_fill_brewer(palette = "Greys") + labs(title = "Humanitarian Aid", x = "Humanitarian Aid", y = "Predicted Probabilities", fill = "ATT Support") + theme(legend.position = "bottom")
  
  gg6 <- ggplot(treaties.qoi, aes(x = scenario, y = value, fill = variable, group = variable)) + 
    geom_area(position = "stack", colour = "black", alpha = 0.8) + 
    theme_bw() + scale_fill_brewer(palette = "Greys") + labs(title = "Prior Treaties", x = "Number of Treaties", y = "Predicted Probabilities", fill = "ATT Support") + theme(legend.position = "bottom")
  
  return(grid.arrange(gg1, gg2, low.sa, high.sa, gg5, gg6, nrow = 3))
}

# with controls

cat_scenarios_controls <- function(X, model, title3, title4) {
  
  export.scenario <- seq(min(X[,1]), max(X[,1]) ,length.out = 100)
  import.scenario <- seq(min(X[,2]), max(X[,2]) ,length.out = 100)
  conflict.scenario <- seq(min(X[,3]), max(X[,3]) ,length.out = 100)
  #capacity.scenario <- seq(min(X[,4]), max(X[,4]) ,length.out = 100)
  aid.scenario <- seq(min(X[,5]), max(X[,5]) ,length.out = 100)
  #commonlaw.scenario <- seq(min(df$commonlaw), max(df$commonlaw) ,length.out = 100)
  treaties.scenario <- seq(min(X[,7]), max(X[,7]) ,length.out = 100)
  polity.scenario <- seq(min(X[,8]), max(X[,8]) ,length.out = 100)
  gdp.scenario <- seq(min(X[,9]), max(X[,9]) ,length.out = 100)
  
  export.qoi <- cat.values(export.scenario, model, 1, X)
  import.qoi <- cat.values(import.scenario, model, 2, X)
  #conflict.qoi <- cat.values(conflict.scenario, model, 3, X)
  #capacity.qoi <- cat.values(capacity.scenario, model, 4, X)
  aid.qoi <- cat.values(aid.scenario, model, 5, X)
  treaties.qoi <- cat.values(treaties.scenario, model, 7, X)
  polity.qoi <- cat.values(polity.scenario, model, 8, X)
  gdp.qoi <- cat.values(gdp.scenario, model, 8, X)
  
  conflict.low_sa <- int.sim(conflict.scenario, model, 3, 4, 0, X)
  conflict.high_sa <- int.sim(conflict.scenario, model, 3, 4, max(X[,4]), X)
  
  gg1 <- ggplot(export.qoi, aes(x = scenario, y = value, fill = variable, group = variable)) + 
    geom_area(position = "stack", colour = "black", alpha = 0.8) + 
    theme_bw() + scale_fill_brewer(palette = "Greys") + labs(title = "Arms Exports", x = "Arms Exports", y = "Predicted Probabilities", fill = "ATT Support") + theme(legend.position = "bottom")
  
  gg2 <- ggplot(import.qoi, aes(x = scenario, y = value, fill = variable, group = variable)) + 
    geom_area(position = "stack", colour = "black", alpha = 0.8) + 
    theme_bw() + scale_fill_brewer(palette = "Greys") + labs(title = "Arms Imports", x = "Arms Imports", y = "Predicted Probabilities", fill = "ATT Support") + theme(legend.position = "bottom")
  
  #  gg3 <- ggplot(conflict.qoi, aes(x = scenario, y = value, fill = variable, group = variable)) + 
  #    geom_area(position = "stack", colour = "black", alpha = 0.8) + 
  #    theme_bw() + scale_fill_brewer(palette = "Greys") + labs(title = "Conflict", x = "Conflict", y = "Predicted Probabilities", #fill = "ATT Support") + theme(legend.position = "bottom")
  
  #  gg4 <- ggplot(capacity.qoi, aes(x = scenario, y = value, fill = variable, group = variable)) + 
  #    geom_area(position = "stack", colour = "black", alpha = 0.8) + 
  #    theme_bw() + scale_fill_brewer(palette = "Greys") + labs(title = "Security Apparatus", x = "Security Apparatus", y = #"Predicted Probabilities", fill = "ATT Support") + theme(legend.position = "bottom")
  
  low.sa <- ggplot(conflict.low_sa, aes(x = scenario, y = value, fill = variable, group = variable)) + 
    geom_area(position = "stack", colour = "black", alpha = 0.8) + 
    theme_bw() + scale_fill_brewer(palette = "Greys") + labs(title = paste(title3," (weak ",title4,")", sep = ""), x = "Conflict", y = "Predicted Probabilities", fill = "ATT Support") + theme(legend.position = "bottom")
  
  high.sa <- ggplot(conflict.high_sa, aes(x = scenario, y = value, fill = variable, group = variable)) + 
    geom_area(position = "stack", colour = "black", alpha = 0.8) + 
    theme_bw() + scale_fill_brewer(palette = "Greys") + labs(title = paste(title3," (strong ", title4,")", sep = ""), x = "Conflict", y = "Predicted Probabilities", fill = "ATT Support") + theme(legend.position = "bottom")
  
  gg5 <- ggplot(aid.qoi, aes(x = scenario, y = value, fill = variable, group = variable)) + 
    geom_area(position = "stack", colour = "black", alpha = 0.8) + 
    theme_bw() + scale_fill_brewer(palette = "Greys") + labs(title = "Humanitarian Aid", x = "Humanitarian Aid", y = "Predicted Probabilities", fill = "ATT Support") + theme(legend.position = "bottom")
  
  gg6 <- ggplot(treaties.qoi, aes(x = scenario, y = value, fill = variable, group = variable)) + 
    geom_area(position = "stack", colour = "black", alpha = 0.8) + 
    theme_bw() + scale_fill_brewer(palette = "Greys") + labs(title = "Prior Treaties", x = "Number of Treaties", y = "Predicted Probabilities", fill = "ATT Support") + theme(legend.position = "bottom")
  
  gg7 <- ggplot(polity.qoi, aes(x = scenario, y = value, fill = variable, group = variable)) + 
    geom_area(position = "stack", colour = "black", alpha = 0.8) + 
    theme_bw() + scale_fill_brewer(palette = "Greys") + labs(title = "Regime Type", x = "Polity", y = "Predicted Probabilities", fill = "ATT Support") + theme(legend.position = "bottom")
  
  gg8 <- ggplot(gdp.qoi, aes(x = scenario, y = value, fill = variable, group = variable)) + 
    geom_area(position = "stack", colour = "black", alpha = 0.8) + 
    theme_bw() + scale_fill_brewer(palette = "Greys") + labs(title = "GDP", x = "GDP", y = "Predicted Probabilities", fill = "ATT Support") + theme(legend.position = "bottom")
  
  return(grid.arrange(gg1, gg2, low.sa,high.sa, gg5, gg6, gg7, gg8, nrow = 4))
}

# ----- calling functions for each model -----

# Model 1
Z <- matrix(NA, nrow = nrow(df1), ncol = J)# our response matrix that will feed into ll function later
for(j in 1:J) {
  Z[, j] <- df1$att_support == cats[j] # evaluate T or F, logical values instead of numbers
}
m1.qoi <- cat_scenarios(X1, m1, "Conflict", "security apparatus")

# Model 2
Z <- matrix(NA, nrow = nrow(df2), ncol = J)# our response matrix that will feed into ll function later
for(j in 1:J) {
  Z[, j] <- df2$att_support == cats[j] # evaluate T or F, logical values instead of numbers
}
m2.qoi <- cat_scenarios(X2, m2, "Refugees", "government effectiveness")

# Model 3
Z <- matrix(NA, nrow = nrow(df3), ncol = J)# our response matrix that will feed into ll function later
for(j in 1:J) {
  Z[, j] <- df3$att_support == cats[j] # evaluate T or F, logical values instead of numbers
}
m3.qoi <- cat_scenarios_controls(X3, m3, "Conflict", "security apparatus")

# Saving graphs
ggsave("m1qoi.jpg", plot = m1.qoi, height = 10, width = 10)
ggsave("m2qoi.jpg", plot = m2.qoi, height = 10, width = 10)
ggsave("m3qoi.jpg", plot = m3.qoi, height = 13, width = 10)

# ---- Interaction term QoI -----

int.sim <- function(scenario, model, sel, int, value, X) {
  
  cases <- array(NA, c(dim(X), length(scenario)))
  cases[, , ] <- X
  sel <- sel # The column of X that contains variable of interest
  
  for (i in 1:length(scenario)){
    cases[, sel, i] <- scenario[i] 
    cases[, int, i] <- value
  }
  
  U <- matrix(nrow = nrow(X), ncol = length(scenario))
  
  for(i in 1:length(scenario)){
    U[, i] <- cases[,,i] %*% unname(model$coefficients)
  }
  
  probs <- array(NA, c(dim(Z), length(scenario)))
  
  # get thresholds
  tau <- sort(unname(model$zeta))
  
  for(i in 1:length(scenario)){
    # calculate probabilities
    probs[, 1, i] <- exp(tau[1] - U[, i])/(1 + exp(tau[1] - U[, i]))
    
    for (j in 2:(ncol(Z) - 1)){
      probs[, j, i] <- exp(tau[j] - U[, i])/(1 + exp(tau[j] - U[, i])) - 
        exp(tau[j-1] - U[, i])/(1 + exp(tau[j - 1] - U[, i]))
    } 
    
    probs[, ncol(Z), i] <- 1 - exp(tau[J - 1] - U[, i])/(1 + exp(tau[J - 1] - U[, i])) 
  }
  
  # Now the means of the observed values
  
  ovaP <- matrix(ncol = ncol(Z), nrow = length(scenario))
  
  for(i in 1:length(scenario)){
    ovaP[i,] <- apply(probs[, , i], 2, mean)
  }
  
  plotdf <- data.frame(ovaP)
  names(plotdf) <- c("No support", "Weak support", "Strong support")
  
  plotdf <- cbind(melt(plotdf), scenario)
  return(plotdf)
}

Z <- matrix(NA, nrow = nrow(df1), ncol = J)# our response matrix that will feed into ll function later
for(j in 1:J) {
  Z[, j] <- df1$att_support == cats[j] # evaluate T or F, logical values instead of numbers
}

conflict.scenario <- seq(min(df$conflict), max(df$conflict), length.out = 100)
conflict.low_sa <- int.sim(conflict.scenario, m1, 3, 4, 0, X1)
conflict.high_sa <- int.sim(conflict.scenario, m1, 3, 4, max(df1$security_apparatus), X1)

low.sa <- ggplot(conflict.low_sa, aes(x = scenario, y = value, fill = variable, group = variable)) + 
  geom_area(position = "stack", colour = "black", alpha = 0.8) + 
  theme_bw() + scale_fill_brewer(palette = "Greys") + labs(title = "Conflict (weak security apparatus)", x = "Conflict", y = "Predicted Probabilities", fill = "ATT Support") + theme(legend.position = "bottom")

high.sa <- ggplot(conflict.high_sa, aes(x = scenario, y = value, fill = variable, group = variable)) + 
  geom_area(position = "stack", colour = "black", alpha = 0.8) + 
  theme_bw() + scale_fill_brewer(palette = "Greys") + labs(title = "Conflict (strong security apparatus)", x = "Conflict", y = "Predicted Probabilities", fill = "ATT Support") + theme(legend.position = "bottom")

grid.arrange(low.sa, high.sa, ncol = 2)

# Difference in STRONG SUPPORT

fd <- conflict.high_sa$value[conflict.high_sa$variable == "Strong support"] - conflict.low_sa$value[conflict.low_sa$variable == "Strong support"]
fd.ci <- quantile(fd, probs = c(0.975, 0.5, 0.025))
fd.data <- list(fd, fd.ci)

hist(fd, main = "First Difference in Strong Support Between Conflict with Low and High SA", xlab = "Difference in Strong Support for ATT", col = "gray", border = "white")
abline(v=fd.ci[[3]], lty = "dashed", col = "maroon4")
abline(v=fd.ci[[1]], lty = "dashed", col = "maroon4")
abline(v=fd.ci[[2]], col = "maroon4", lwd = 2)

# Difference in NO SUPPORT
fd <- conflict.high_sa$value[conflict.high_sa$variable == "No support"] - conflict.low_sa$value[conflict.low_sa$variable == "No support"]
fd.ci <- quantile(fd, probs = c(0.975, 0.5, 0.025))
fd.data <- list(fd, fd.ci)

hist(fd, main = "First Difference in Weak Support Between Conflict with Low and High SA", xlab = "Difference in Strong Support for ATT", col = "gray", border = "white")
abline(v=fd.ci[[3]], lty = "dashed", col = "maroon4")
abline(v=fd.ci[[1]], lty = "dashed", col = "maroon4")
abline(v=fd.ci[[2]], col = "maroon4", lwd = 2)

# -----------------------------------
# First Differences (all variables)
# -----------------------------------

# df1 as primary dataframe
df <- df1

Z <- matrix(NA, nrow = nrow(df1), ncol = J)# our response matrix that will feed into ll function later
for(j in 1:J) {
  Z[, j] <- df1$att_support == cats[j] # evaluate T or F, logical values instead of numbers
}

fd_fun <- function(scenario, model, sel, X) {
  
  cases <- array(NA, c(dim(X), length(scenario)))
  cases[, , ] <- X
  sel <- sel # The column of X that contains variable of interest
  
  for (i in 1:length(scenario)){
    cases[, sel, i] <- scenario[i]  
  }
  
  U <- matrix(nrow = nrow(X), ncol = length(scenario))
  
  for(i in 1:length(scenario)){
    U[, i] <- cases[,,i] %*% unname(model$coefficients)
  }
  
  probs <- array(NA, c(dim(Z), length(scenario)))
  
  # get thresholds
  tau <- sort(unname(model$zeta))
  
  for(i in 1:length(scenario)){
    # calculate probabilities
    probs[, 1, i] <- exp(tau[1] - U[, i])/(1 + exp(tau[1] - U[, i]))
    
    for (j in 2:(ncol(Z) - 1)){
      probs[, j, i] <- exp(tau[j] - U[, i])/(1 + exp(tau[j] - U[, i])) - 
        exp(tau[j-1] - U[, i])/(1 + exp(tau[j - 1] - U[, i]))
    } 
    
    probs[, ncol(Z), i] <- 1 - exp(tau[J - 1] - U[, i])/(1 + exp(tau[J - 1] - U[, i])) 
  }
  
  # Now the means of the observed values
  
  ovaP <- matrix(ncol = ncol(Z), nrow = length(scenario))
  
  for(i in 1:length(scenario)){
    ovaP[i,] <- apply(probs[, , i], 2, mean)
  }
  
  plotdf <- data.frame(ovaP)
  names(plotdf) <- c("No support", "Weak support", "Strong support")
  
  plotdf <- cbind(melt(plotdf), scenario)
  return(plotdf)
  #return(probs)
}

conflict.scenario <- seq(min(X1[,3]), max(X1[,3]) ,length.out = 100)
conflict.low_sa <- int.sim(conflict.scenario, m1, 3, 4, 0, X1)
conflict.high_sa <- int.sim(conflict.scenario, m1, 3, 4, max(df1$security_apparatus), X1)

# Arms Exports
fd1 <- fd_fun(export.scenario, m1, 1, X1)
fdtest <- fd_fun(rep(0, 100), m1, 1, X1)

# ---- Arms Importer and Regime Type ----

import_regime <- full_join(polity, import, by = "cname")
import_regime <- drop_na(import_regime)
plot(import_regime$polity, import_regime$arms_import)

ggplot(import_regime, aes(polity, arms_import)) + geom_point() + geom_smooth(method = "lm")
# shows that polity is not a great indicator for demand of ars
cor(import_regime[[2,]], import_regime[[3,]]) # -0.22

# ---- Deciding Cutoff Point for Arms Dummies ----

# NOT MAKING CUTOFFS: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC1458573/

# Exporters 
cutoff <- full_join(export, gdp, by = "cname") %>% drop_na()
cutoff$ex_share <- cutoff$arms_export/cutoff$gdp
cut <- quantile(cutoff$ex_share)[[3]]
cutoff$dummy <- 0
cutoff$dummy[cutoff$arms_export > 10] <- 1
table(cutoff$dummy)

quantile(cutoff$arms_export)

x <- cutoff$cname[cutoff$dummy == 1]
y <- cutoff$cname[cutoff$dummy == 0]
max.len<-max(length(x),length(y))
x = c(x, rep(NA, max.len - length(x)))
y = c(y, rep(NA, max.len - length(y))) 
export_cname <- cbind(exporter = x, nonexporter = y)

# Importers 
cutoff <- full_join(import, gdp, by = "cname") %>% drop_na()
cutoff$im_share <- cutoff$arms_import/cutoff$gdp
cut <- quantile(cutoff$im_share)[[3]]
cutoff$dummy <- 0
cutoff$dummy[cutoff$arms_import > 10] <- 1
table(cutoff$dummy)

x <- cutoff$cname[cutoff$dummy == 1]
y <- cutoff$cname[cutoff$dummy == 0]
max.len<-max(length(x),length(y))
x = c(x, rep(NA, max.len - length(x)))
y = c(y, rep(NA, max.len - length(y))) 
import_cname <- cbind(importer = x, nonimporter = y)
