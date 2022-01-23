library(StatDA)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(GGally)
library(mclust)
library(corrplot)

source("./functions.R")
data("ohorizon")

df<-data.frame(ohorizon)
names(df)<- tolower(names(df))
summary(df) # to see how the data looks like

all_cols<- colnames(df)    # get all column names
factor_cols =  names(Filter(is.factor,df))  # get categorical columns

df$lito <- as.factor(df$lito)
####################### EXPLORATORY DATA ANALYSIS #########################
## some factor variables visuals
# check country wise records
p<- get_bar_plot(df, "coun")
plot(p)

# check vegzone wise records
p<- get_bar_plot(df, "veg_zone")
plot(p)

# check x.asp wise records
p<- get_bar_plot(df, "x.asp")
plot(p)


#random elements to analyze
cols_to_analyze<- c( 'sr', 's', 'pt', 'mg' , 'cd' , 'al' , 'bi', 'zn' , 'mn', 'mo')

#Comparison of element concentrations , using barplots/box and whisker for mean conc.
##by country 
###figure set 1

plots<- list()
i =1
for (col in cols_to_analyze ) {
  plots[[i]]<- ggplot(df, aes_string("coun", as.name(col) ))+
    geom_boxplot(shape=1, outlier.colour = "red", outlier.shape = 1, outlier.alpha = 0.3)+
    labs ( y=paste0(c(col," in (mg/Kg)"), collapse=""))
  paste(col," distribution by ","country")
  
  #ggsave(paste(col," distribution by ","country.png"), last_plot(), )
  
  #plots[i]= p 
  i=i+1
  
}
multiplot(plotlist = (plots), cols = 2)

## by vegetation zone
###figure set 2
plots<- list()
i =1
for (col in cols_to_analyze ) {
  plots[[i]]<- ggplot(df, aes_string("veg_zone", as.name(col) )) + 
    geom_boxplot(shape=1, outlier.colour = "red", outlier.shape = 1, outlier.alpha = 0.3)+ 
    labs ( y=paste0(c(col," in (mg/Kg)"), collapse=""))
paste(col," distribution by ","veg_zone")

#ggsave(paste(col," distribution by ","veg_zone.png"), last_plot(), )
  #plots[i]= p 
  i=i+1
  
}
multiplot(plotlist = (plots), cols = 2)

## by Lito and country 
### figure set 3
plots<- list()
i =1
for (col in cols_to_analyze ) {
  plots[[i]]<- ggplot(df,aes_string(x="coun", y=as.name(col), colour="lito")) +
    geom_jitter(aes(text=paste("country: ", coun)), width=0.25, alpha=0.5, ) +
    theme(axis.text.x = element_text(angle = -30, hjust = 0.1)) +
    labs(title = "Conc distribution by LITO in each country",
         x = "Country",
         y = paste("Conc of ",col, " in mg/kg"))
  
  #plots[i]= p 
  i=i+1
  ggsave(paste(col," distribution by ","country_lito.png"), last_plot(), )
}
multiplot(plotlist = (plots),  cols = 2)

## by groundveg
### figure set 4
plots<- list()
i =1
for (col in cols_to_analyze ) {
  
  plots[[i]] <- ggplot(df, aes_string("groundveg", as.name(col) ))+ 
    geom_boxplot(shape=1, outlier.colour = "red", outlier.shape = 1, outlier.alpha = 0.3)+ 
    labs ( y=paste0(c(col," in (mg/Kg)"), collapse=""))
  paste(col," distribution by ","groundveg")
  i=i+1
  ggsave(paste(col," distribution by ","groundveg.png"), last_plot(), )
}
multiplot(plotlist = (plots),  cols = 2)
 
 
metallic_elements<-c("sr","cu" ,"al","mg","na", "fe","hg","k")
non_metallic_elements <- c( "bi","s","no3","po4","so4")

## Ph vs metallic elements scatter plot
ggpairs(df%>% select(eval(c(metallic_elements, "ph"))), title="correlogram metallic conc and ph") 
## Ph vs non metallic elements scatter plot
ggpairs(df%>% select(eval(c(non_metallic_elements, "ph"))), title="correlogram non-metallic conc and ph") 

ggpairs(df%>% select(eval(c(metallic_elements,non_metallic_elements, "ph"))), title="correlogram conc and ph") 



#  Analysis for Bi
## correlation with each elements (done)
## occurrence in different type 
## scatter plot
## figureset 5
plots<- list()
i =1
for (col in cols_to_analyze){
  plots[[i]] <- ggplot(df, aes_string(x=as.name(col), y="bi" , colour='lito')) +
    geom_point() +
    geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) 
  i=i+1
  ggsave(paste(col," scatterplot ","bi vs ",col," .png"), last_plot(), )
}
multiplot(plotlist = (plots), cols = 2)

..
# Step 1:Data cleaning
## Treating nulls
### for numeric -element columns , imputing with mean
### for numeric other  -9999
###  factor cols = missing
### for blank factors= missing

lapply(df,function(x) { length(which(is.na(x)))})
#lito
#humno
#humthi
# ph  
# Imputing for LITO by mode of LITO in country 


df_FIN<- df%>% filter(coun=='FIN')
df_NOR<- df%>% filter(coun=='NOR')
df_RUS <- df %>% filter(coun=='RUS')

#replacing null in lito with each coutry's mode Lito
fin_lito_mode <- df_FIN%>% group_by(lito) %>% summarize(count= n()) %>% filter(count==max(count)) %>% select(lito) 
print(fin_lito_mode)
df_FIN<- df_FIN%>%replace_na(list(lito= fin_lito_mode))

rus_lito_mode <- df_RUS%>% filter(!is.na(lito)) %>% group_by(lito) %>% summarize(count= n()) %>% filter(count==max(count)) %>% select(lito) 
print(rus_lito_mode)
df_RUS<- df_RUS%>%replace_na(list(lito= rus_lito_mode))

nor_lito_mode <- df_NOR%>% group_by(lito) %>% summarize(count= n()) %>% filter(count==max(count)) %>% select(lito) 
print(nor_lito_mode)
df_NOR<- df_NOR%>%replace_na(list(lito= nor_lito_mode))
df<- rbind(df_FIN, df_NOR, df_RUS)
# replace nA in other cols with mean  
df$ph[is.na(df$ph)]<-mean(df$ph,na.rm=TRUE)
df$au[is.na(df$au)]<-mean(df$au,na.rm=TRUE)
df$pd[is.na(df$pd)]<-mean(df$pd,na.rm=TRUE)
df$pt[is.na(df$pt)]<-mean(df$pt,na.rm=TRUE)

## Standardize

cols_to_standardize<-unique(c(names(Filter(is.numeric,df)), 
    names(Filter(is.integer,df))))
cols_to_standardize <-cols_to_standardize[!cols_to_standardize %in% c("id"  ,   "xcoo" ,  "ycoo" )]
cols_to_transform <-cols_to_standardize[!cols_to_standardize %in% c("elev", "aomean","humno","humthi","ph")]
cols <- paste0(cols_to_transform ,"_log")
df[cols] <- log(df[cols_to_transform])
df<-  df%>% mutate_at(c(cols_to_standardize,cols), ~ (scale(.) %>% as.vector))

# Step 2: 
## create rock type variable
## barplot for count by each type of rock

df_extended <- df %>% mutate( rock_type = case_when(
  lito=='9' | lito=='10'  ~ "caledonian", 
  lito=='51' | lito =='52' ~ "palaeozoic",
  lito=='81'| lito=='82'| lito=='83' ~ "alkaline",
  lito=='7'~ "granites",
  TRUE ~ 'others'
))  
#df_extended %>% select(lito , rock_type) %>% View()
p<- get_bar_plot(df_extended %>%filter(rock_type!="others"), "rock_type")
plot(p)
#further check rock type for each lito 
g <- ggplot(df_extended %>%filter(rock_type!="others"), aes(rock_type))
g + geom_bar(aes(fill=lito) , width = 0.5) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="Bar Plot of rock type distribution", 
       subtitle="Rocktype across Lito") 

 
## Density plots 


#Distribution of BI conc in rock type
g <- ggplot(df_extended, aes(bi))
g + geom_density(aes(fill=factor(rock_type)), alpha=0.8) + 
  labs(title="Density plot", 
       subtitle="Bi conc distribution with rock type",
       x="conc in mg/Kg",
       fill="# rocktype")
#figureset6
plots<- list()
i =1
for (col in cols_to_analyze){
  plots[[i]] <- ggplot(df_extended, aes_string(as.name(col))) + geom_density(aes(fill=factor(rock_type))) + 
    labs( x=paste("Conc of ",col ),
         fill="# rocktype")
  i=i+1
  ggsave(paste(col," distribution ","rocktype vs ",col," .png"), last_plot(), )
}
multiplot(plotlist = (plots),  cols = 2)
## all are right tailed, but some are slight less skewed, need to do log transformation


################### PART 2 CLUSTERING ###########################

cols<- cols[!cols %in% c("loi_log","cond_log")]
df_filter<- df_extended %>% filter(rock_type != 'others') %>% select(c(cols, rock_type))
train<-NULL
X<-df_filter%>%select(!rock_type)
Y<-df_filter%>%select(rock_type)
## checking highly correlated variables
M= cor(X)
corrplot(M) 
corrplot(M, method = 'square', order = 'FPC', type = 'lower', diag = FALSE)

M[upper.tri(M)] <- 0
diag(M) <- 0
## ammonium acetate extraction is highly correlated to element concentrations
cols<- cols[! cols %in% c('al_aa_log','ba_aa_log', 'ca_aa_log', 'cd_aa_log', 'co_aa_log', 'cr_aa_log', 'cu_aa_log', 'fe_aa_log',
                          'k_aa_log', 'mg_aa_log' , 'mn_aa_log', 'na_aa_log', 'p_aa_log', 'pb_aa_log','s_aa_log' ,'si_aa_log' ,'sr_aa_log' , 
                          'ti_aa_log' , 'v_aa_log', 'zn_aa_log')]
#  -------- K-Means ----------
numberOfClusters <- 4
## 75% of the sample size


model <- kmeans(X, numberOfClusters ,iter.max = 300)
model$cluster  #check cluster of each record
model$size    #observations in each clusters
train$rock_type= Y
train$cluster<- model$cluster
train<-as.data.frame(train)
confusion_matrix<-train%>% group_by(rock_type,cluster) %>% summarise(count=n())
View(confusion_matrix)
#clustering quality
adjustedRandIndex(train$rock_type,train$cluster)  # 0.368 , improved after removing correlated columns


# --------- Hierarchical ------

distance <- dist(X, method="euclidean") 
hc <- hclust(distance, method="average")
model <- cutree(hc, numberOfClusters)
train<-NULL
train$rock_type= Y
train$cluster<- model
train<-as.data.frame(train)
confusion_matrix<-train%>% group_by(rock_type,cluster) %>% summarise(count=n())
View(confusion_matrix)
#clustering quality
adjustedRandIndex(train$rock_type,train$cluster)  # 0.0077 
#kmeans performed better, but there's more room for improvement. Can try different distance method on herarichal




#################################PART 3 Regression #############################

## Correlation with BI for each of the element has been identified above

model<-lm(bi_log ~ ., data= X)
summary(model)
bi_log.hat <- fitted.values(model)
head(bi_log.hat)
mean(X$bi_log)==mean(bi_log.hat)
model_residual <- residuals(model)
mean(model_residual) #close to zero , very insignificant e-18
sum(model_residual)#close to zero , very insignificant e-16

residual.df <- as.data.frame(model_residual)
ggplot(residual.df,aes(model_residual)) +  geom_histogram(bins = 12, fill='blue')
plot(model)   # checking model diagnostics

mse <- mean((X$bi_log - bi_log.hat)^2)
rmse <- sqrt(mse)
rmse   #0.32 
pred_error_rate <- rmse/mean(X$bi_log)
pred_error_rate   # prediction error rate is 10.53 %
 