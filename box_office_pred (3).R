
library(dplyr)
library(mice)
library(caret)
library(stringr)
library(gbm)
library(randomForest)

#Note; I have modified the orginal excel spreadsheets as some manipulations were easier in
# in Excel than R (extracting the year, month and day from the release date)

tmdb_train<-read.csv("tmdb-box-office-prediction\\train.csv")
tmdb_test<-read.csv("tmdb-box-office-prediction\\test.csv")


#We are going to remove categories which are very specific to each movie
# this will include "id", "imdb_id", "original_title", "overview", "poster_page","status"
#"title", "keywords" and "spoken languages" (as it seems original language is a better indicator)
#we also remove release date, as it is superflous

#Creating a function to pre-process the data
prProc<-function(tmdb_tr,d){
  #Vector of the indicies of the variables we want to remove
  rmv<-c(1,6,8,9,11,14,19,20,21,22,23)
  tmdb_rmv<-tmdb_tr[,-rmv]
  
  #There are only two NA variables (in the runtime variabe) 
  #and we just impute the mean runtime
  
  tmdb_rmv[1336,12]<-mean(tmdb_rmv[,12][!tmdb_rmv[,12]==0],na.rm=T)
  tmdb_rmv[2303,12]<-mean(tmdb_rmv[,12][!tmdb_rmv[,12]==0],na.rm=T)
  tmdb_rmv[,12][tmdb_rmv[,12]==0]<-tmdb_rmv[2303,12]
  
  #The variable homepage is also specific to each movie, however whether or not a movie 
  # has a homepage could be a factor in determing box office sales, thus
  # we will turn this variable into a yes no variable
  
  
  yn<-as.character(tmdb_rmv$homepage)
  yn[!(yn=="")]<-"Y"
  yn[(yn=="")]<-"N"
  yn<-as.factor(yn)
  
  tmdb_rmv<-mutate(tmdb_rmv,homepage=yn)
  
  # We are going to turn the genre variable into several variables
  # so there can be common genres between each movie, we choose 
  #the first 3 genres, as most movies are classified using less than 3 genres
  
  genre_mat<-str_extract_all(tmdb_rmv$genres,"\\{[^\\}\\{}]{1,}\\}",simplify = T)
  genre_df<-data.frame(genre_mat[,1:3])
  names(genre_df)<-c("gen1","gen2","gen3")
  
  # We turn the production company into several variables too
  
  pc_mat<-str_extract_all(tmdb_rmv$production_companies,"\\{[^\\}\\{}]{1,}\\}",simplify = T)
  pc_df<-data.frame(pc_mat[,1:3])
  names(pc_df)<-c("pc1","pc2","pc3")
  
  # We extract the first 5 leading actors
  
  la_mat<-str_extract_all(tmdb_rmv$cast,"\\{[^\\}]{1,}\\}",simplify = T)
  la_name_mat<-apply(la_mat[,1:5],1:2, str_extract,pattern="name':.{1,}',")
  la_df<-as.data.frame(la_name_mat)
  names(la_df)<-c("la1","la2","la3","la4","la5")
  
  #Extract Genders
  # Gender of the first 5 leading actors
  la_gen_mat<-apply(la_mat[,1:5],1:2, str_extract,pattern="'gender': [0-9],")
  la_gen_df<-as.data.frame(la_gen_mat)
  names(la_gen_df)<-c("lag1","lag2","lag3","lag4","lag5")
  
  #Extract Director
  crew<-str_extract_all(tmdb_rmv$crew,"\\{[^\\}]{1,}'Director'[^\\{]{1,}\\}",simplify = T)
  dr<-str_extract_all(crew[,1],"'Director', 'name':[^,]{1,}',",simplify=T)
  dr<-as.data.frame(dr[,1])
  names(dr)<-"dir"
  
  #Extract gender of director
  la_gen_dir<- str_extract_all(crew[,1],"'gender': [0-9],",simplify=T)
  gdir<-as.data.frame(la_gen_dir[,1])
  names(gdir)<-"gdir"
  
  #Extract first 3 Producers
  pr<-str_extract_all(tmdb_rmv$crew,"'job': 'Producer', 'name':[^,]{1,}',",simplify=T)
  pr<-as.data.frame(pr[,1:3])
  names(pr)<-c("pr1","pr2","pr3")
  
  #Extract first 3 Executive Producers
  epr<-str_extract_all(tmdb_rmv$crew,"'job': 'Executive Producer', 'name':[^,]{1,}',",simplify=T)
  epr<-as.data.frame(epr[,1:3])
  names(epr)<-c("epr1","epr2","epr3")
  
  # Extract production countries
  
  pco_mat<-str_extract_all(tmdb_rmv$production_countries,"\\{[^\\}\\{}]{1,}\\}",simplify = T)
  pco_df<-data.frame(pco_mat[,1:2])
  names(pco_df)<-c("pco1","pco2")
  
  # We also split the data into Year, month and day so movie release dates can be compared
  #We will turn month and day into factor variables
  tmdb_rmv$month<-as.factor(tmdb_rmv$month)
  tmdb_rmv$day<-as.factor(tmdb_rmv$day)
  
  #date<-as.Date(tmdb_rmv$release_date,"%m/%d/%y")
  #dm<-format(date,"%m")
  #dd<-format(date,"%d")
  #We let the year be numeric as there could be a trend between year and ticket pricetm
  #dy<-as.data.frame(dy)
  #names(dy)<-"year"
  #dm<-as.factor(dm)
  #dm<-as.data.frame(dm)
  #names(dm)<-"month"
  #dd<-as.factor(dd)
  #dd<-as.data.frame(dd)
  #names(dd)<-"day"
  
  #Now lets remove the variables we will replace with the new dataframes
  
  rmv2<-c(3,7,8,13,14)
  
  tmdb_rmv2<-tmdb_rmv[,-rmv2]
  
  #For budget several of the movies have a budget of zero, we will use mice to impute 
  # We only impute on numeric data and the date variables as imputation uses regression
  # and having too many factor variable may cause problems
  
  
  #The if statement is just whether to have revenue at the end of the data set
  #(the training data set) or not
  
  if(d==1){
  #Also removing the first factor variable and the language factor variable
  tmdb_imp<-tmdb_rmv2[,c(-1,-4)]
  tmdb_imp$budget[tmdb_imp$budget==0]<-NA
  imputed_Data <- mice(tmdb_imp, m=1, maxit = 20, method="cart" ,seed = 500,remove_collinear=T)
  
  #imputing the budget data
  complete_Data<-complete(imputed_Data)
  
  
  #Converting the reveue into log revenue for the competition
  complete_Data<-mutate(complete_Data,revenue=log(revenue))
  
  #Let us now combine all the data sets, to get the final training data
  
  final_data<-cbind(complete_Data[,-8],genre_df,pc_df,la_df,la_gen_df,dr,gdir,pr,epr,pco_df,complete_Data$revenue)
  
  for (i in setdiff(1:34,c(1,3,4,7,34))){
    final_data[[i]]<-as.character(final_data[[i]])}
  
  #Turn all remaining blank and NA values into "None"
  final_data[is.na(final_data)]<-"None"
  final_data[final_data==""]<-"None"
  
  for (i in setdiff(1:34,c(1,3,4,7,34))){
    final_data[[i]]<-as.factor(final_data[[i]])}
  }
  
  else {
    tmdb_imp<-tmdb_rmv2[,c(-1,-4)]
    tmdb_imp$budget[tmdb_imp$budget==0]<-NA
    imputed_Data <- mice(tmdb_imp, m=1, maxit = 20, method="cart" ,seed = 500,remove_collinear=T)
    
    complete_Data<-complete(imputed_Data)
    
    
    #Let us now combine all the data sets, to get the final training data
    
    final_data<-cbind(complete_Data,genre_df,pc_df,la_df,la_gen_df,dr,gdir,pr,epr,pco_df)
    
    
  for (i in setdiff(1:33,c(1,3,4,7))){
    final_data[[i]]<-as.character(final_data[[i]])}
  
  final_data[is.na(final_data)]<-"None"
  final_data[final_data==""]<-"None"
  
  for (i in setdiff(1:33,c(1,3,4,7))){
    final_data[[i]]<-as.factor(final_data[[i]])}}
  
  return(final_data)
}


#Creating the final test and training data sets
final_data_tr<-prProc(tmdb_train,d=1)
names(final_data_tr)[34]<-"revenue"
final_data_te<-prProc(tmdb_test,d=2)

# It was also found that normalizing the continous vairables help give better predicition 
# at least for linear regression, thus we will use a box cox transfromation

for (i in c(1,3,7)){
 bc<-BoxCoxTrans(final_data_tr[[i]])
  final_data_tr[[i]]<-predict(bc,final_data_tr[[i]])
 final_data_te[[i]]<-predict(bc,final_data_te[[i]])
}

#Actors, prodcuers and directors are too unqiue to use for training
# to deal with this I categorize each, actor, director, production company, producer and
# executive producer by how much money the film they were in made
#Each movie was sorted into 20 different percentiles, and this percentile
# is attibuted to the cast, producers and company of the movie
# This reduces the number of levels for each variables from hundreds to just 20

q<-quantile(final_data_tr$revenue,prob=seq(0,1,length=21))

#This function returns a data frame for actors, producers ect. and their corresponding
#percentile rank

name_list<-function(dfl){
# Reducing the category of actor
act_list<-data.frame(actor=NULL,rank=NULL)
act_temp<-c(NULL)
rank_temp<-c()
temp_names<-c("a","b","c","d","e")
for (i in 1:length(names(dfl)))
{temp_names[i]<-names(dfl)[i]}
  
la1<-as.character(dfl[[temp_names[1]]])
la2<-as.character(dfl[[temp_names[2]]])
la3<-as.character(dfl[[temp_names[3]]])
la4<-as.character(dfl[[temp_names[4]]])
la5<-as.character(dfl[[temp_names[5]]])

act_len<-length(c(la1[1],la2[1],la3[1],la4[1],la5[5])[!is.na(c(la1[1],la2[1],la3[1],la4[1],la5[5]))])

for (i in 1:dim(final_data_tr)[1]){
  act_temp<-c(act_temp,la1[i],la2[i],la3[i],la4[i],la5[i])
  act_temp<-act_temp[!is.na(act_temp)]
  revq<-case_when(
    ((q[1]<=final_data_tr$revenue[i])&(final_data_tr$revenue[i]<q[2]))~1,
    ((q[2]<=final_data_tr$revenue[i])&(final_data_tr$revenue[i]<q[3]))~2,
    ((q[3]<=final_data_tr$revenue[i])&(final_data_tr$revenue[i]<q[4]))~3,
    ((q[4]<=final_data_tr$revenue[i])&(final_data_tr$revenue[i]<q[5]))~4,
    ((q[5]<=final_data_tr$revenue[i])&(final_data_tr$revenue[i]<q[6]))~5,
    ((q[6]<=final_data_tr$revenue[i])&(final_data_tr$revenue[i]<q[7]))~6,
    ((q[7]<=final_data_tr$revenue[i])&(final_data_tr$revenue[i]<q[8]))~7,
    ((q[8]<=final_data_tr$revenue[i])&(final_data_tr$revenue[i]<q[9]))~8,
    ((q[9]<=final_data_tr$revenue[i])&(final_data_tr$revenue[i]<q[10]))~9,
    ((q[10]<=final_data_tr$revenue[i])&(final_data_tr$revenue[i]<q[11]))~10,
    ((q[11]<=final_data_tr$revenue[i])&(final_data_tr$revenue[i]<q[12]))~11,
    ((q[12]<=final_data_tr$revenue[i])&(final_data_tr$revenue[i]<q[13]))~12,
    ((q[13]<=final_data_tr$revenue[i])&(final_data_tr$revenue[i]<q[14]))~13,
    ((q[14]<=final_data_tr$revenue[i])&(final_data_tr$revenue[i]<q[15]))~14,
    ((q[15]<=final_data_tr$revenue[i])&(final_data_tr$revenue[i]<q[16]))~15,
    ((q[16]<=final_data_tr$revenue[i])&(final_data_tr$revenue[i]<q[17]))~16,
    ((q[17]<=final_data_tr$revenue[i])&(final_data_tr$revenue[i]<q[18]))~17,
    ((q[18]<=final_data_tr$revenue[i])&(final_data_tr$revenue[i]<q[19]))~18,
    ((q[19]<=final_data_tr$revenue[i])&(final_data_tr$revenue[i]<q[20]))~19,
    ((q[20]<=final_data_tr$revenue[i])&(final_data_tr$revenue[i]<=q[21]))~20)
  rank_temp<-c(rank_temp,rep(revq,times=act_len))}

act_list<-data.frame(actor=act_temp,rank=rank_temp)
act_list<-act_list[act_list$actor!="None",]
#act_list$actor<-as.factor(as.character(act_list$actor))


#Removing duplicates from list
rem<-NULL
for (i in 1:dim(act_list)[1]){
  ind<-act_list$actor %in% act_list$actor[i]
  wind<-which(ind)[-which(which(ind)==i)]
  if (length(wind!=0)){
    for (j in wind){
      if (act_list$rank[i]>act_list$rank[j]){
        rem<-c(rem,-j)}
      }
    }}

act_list<-act_list[rem,]
act_list<-unique.data.frame(act_list)

return(act_list)
}

#Creating lists of  for actors, director, production companies, produces,e.producers
# The list contains the number correspoding to their percentile for each actor ect.
act_list<-name_list(final_data_tr[,14:18])
pc_list<-name_list(final_data_tr[,11:13])
dir_ex<-as.data.frame(final_data_tr[,24])
dir_list<-name_list(dir_ex)
pr_list<-name_list(final_data_tr[,26:28])
epr_list<-name_list(final_data_tr[,29:31])

act_list$rank<-as.character(act_list$rank)
pc_list$rank<-as.character(pc_list$rank)
dir_list$rank<-as.character(dir_list$rank)
pr_list$rank<-as.character(pr_list$rank)
epr_list$rank<-as.character(epr_list$rank)

#Now lets replace all the factors in the dataset with their rankings


#A function to replace the actors, producers, directors ect. with their percentile rank
rankrep<-function(var_list,vec_list,fd){
for (i in vec_list){
  dfr<-c()
  for (j in 1:dim(final_data_tr)[1]){
    vt<-which(var_list[[1]] %in% as.character(fd[,i][j]))
    if(length(vt)==1){
      dfr<-c(dfr,var_list[[2]][vt])}
  else{dfr<-c(dfr,as.character(fd[,i][j]))}}
  fd[[i]]<-dfr}
return(fd)}

varlist<-list(a=act_list,b=pc_list,c=dir_list,d=pr_list,e=epr_list)
veclist<-list(a=14:18,b=11:13,c=24,d=26:28,e=29:31)

fd<-final_data_tr

#Implementing the function for the training set
for (i in 1:5){
  fd<-rankrep(varlist[[i]],veclist[[i]],fd)}

#We will also cut down the number o dlevels in production countries, simply
# by only conisdering the top 25 countries in terms of production

pco_rank<-table(fd$pco1)[order(table(fd$pco1),decreasing=T)]

pco_rankt25<-names(pco_rank)[1:25]

fd$pco1<-as.character(fd$pco1)
fd$pco2<-as.character(fd$pco2)

for (i in 1:dim(final_data_tr)[1]){
  if(!(fd$pco1[i] %in% pco_rankt25)){
    fd$pco1[i]<-"None"}
  if(!(fd$pco2[i] %in% pco_rankt25)){
    fd$pco2[i]<-"None"}}
    


rankrep<-function(var_list,vec_list,fd){
  for (i in vec_list){
    dfr<-c()
    for (j in 1:dim(final_data_te)[1]){
      vt<-which(var_list[[1]] %in% as.character(fd[,i][j]))
      if(length(vt)==1){
        dfr<-c(dfr,var_list[[2]][vt])}
      else{dfr<-c(dfr,"None")}}
    fd[[i]]<-dfr}
  return(fd)}

fdt<-final_data_te

#Implementing the function for the test set
for (i in 1:5){
  fdt<-rankrep(varlist[[i]],veclist[[i]],fdt)}    

for (i in 1:dim(final_data_te)[1]){
  if(!(fdt$pco1[i] %in% pco_rankt25)){
    fdt$pco1[i]<-"None"}
  if(!(fdt$pco2[i] %in% pco_rankt25)){
    fdt$pco2[i]<-"None"}}

fd$pco1<-as.factor(as.character(fd$pco1))
fd$pco2<-as.factor(as.character(fd$pco2))

fdt$pco1<-as.factor(as.character(fdt$pco1))
fdt$pco2<-as.factor(as.character(fdt$pco2))

#COnverting all the categorial variables from character to numeric (so they can be used my the learning alogirthms)
for (i in 1:33){
  if(is.character(fd[[i]])){
    fd[[i]]<-as.factor(fd[[i]])}
  if(is.character(fdt[[i]])){
    fdt[[i]]<-as.factor(fdt[[i]])}}

#There is an anomaly where the ep3 of the test set does not contain 2, we will just add a 2 so the facotr levels match
fdt$epr3<-as.character(fdt$epr3)
fdt$epr3[1]<-"2"
fdt$epr3<-as.factor(fdt$epr3)
fdt$epr3[1]<-"None"

#This also occurs for genre 1
d<-fdt$gen1
sdiff<-setdiff(levels(fd$gen1),levels(fdt$gen1))
fdt$gen1<-as.character(fdt$gen1)
temp<-fdt$gen1[1]
fdt$gen1[1]<-sdiff
fdt$gen1<-as.factor(fdt$gen1)
fdt$gen1[1]<-temp

#Upon examining the data, we see there are a lot of values in the test set
# that don't show up in the training set, we will recalssifiy these values
#as "None".  

for (i in 1:dim(fdt)[2]){
  if (is.factor(fdt[[i]])){
    fdt[[i]]<-as.character(fdt[[i]])
  fdt[[i]][!(fdt[[i]] %in% levels( fd[[i]]))]<-"None"
  fdt[[i]]<-as.factor(fdt[[i]])}}

#We create data frames calculating the fraction of "None" in each variable for
# the test and training set

namesc<-c()
f<-c()
for (i in 1:dim(fd)[2]){
if (is.factor(fd[[i]])){
t<-table(fd[[i]]=="None")
fr<-t[2]/(t[1]+t[2])
namesc<-c(namesc,names(fd)[i])
f<-c(f,fr)}}

tr_tr<-data.frame(namesc,f)

namesc<-c()
f<-c()
for (i in 1:dim(fdt)[2]){
  if (is.factor(fdt[[i]])){
    t<-table(fdt[[i]]=="None")
    fr<-t[2]/(t[1]+t[2])
    namesc<-c(namesc,names(fdt)[i])
    f<-c(f,fr)}}

tr_te<-data.frame(namesc,f)

#Preliminary algorithms ran on the training and test sets, showed a large amount of 
#overfitting, this could be because there are far more "None" variables in the 
#test set as we had to remove values that did not appear in the training set
# Thus we randomly impute variables in the training set to "None" to match
# the fraction of "None"s in the test set

diff<-tr_te$f-tr_tr$f
diff<-diff[!is.na(diff)]
diff[diff<0]<-0
j=1
for (i in 1:dim(fd)[2]){
  if (is.factor(fd[[i]])&((i!=5)&(i!=6))&(i!=2)){
    if(diff[j]!=0){
    fd[[i]]<-as.character(fd[[i]])
    fd[[i]][sample(1:dim(fd)[1],round(diff[j]*dim(fd)[1]))]<-"None"
    fd[[i]]<-as.factor(fd[[i]])
    }
    j<-j+1
    print(j)}}

#Now we will create a stacked model to predict the data, we will use random forest
# gradient boosting and elastic net regression as they all gave reasonable estimates
# by themselves on the data (not shown in script, done through trial and error in workspace)

#split the data into 3-folds

cvst<-createFolds(fd[,34],k=3)

#We create 9 models (3 for each technique), using different hyperparameter
# for each model, trained on the entire training set

rf1<-randomForest(x=fd[,-34],y=fd[,34],mtry=10)
rf2<-randomForest(x=fd[,-34],y=fd[,34],mtry=20)
rf3<-randomForest(x=fd[,-34],y=fd[,34],mtry=30)
gbm1<-gbm(revenue~.,data=fd,n.trees=50,interaction.depth=3)
gbm2<-gbm(revenue~.,data=fd,n.trees=100,interaction.depth=2)
gbm3<-gbm(revenue~.,data=fd,n.trees=150,interaction.depth=1)
lr1<-train(revenue~.,data=fd,method="glmnet",trControl=trainControl(method="cv", number = 2),tuneGrid=data.frame(alpha=1,lambda=0.377366))
lr2<-train(revenue~.,data=fd,method="glmnet",trControl=trainControl(method="cv", number = 2),tuneGrid=data.frame(alpha=0.1,lambda=0.00377366))
lr3<-train(revenue~.,data=fd,method="glmnet",trControl=trainControl(method="cv", number = 2),tuneGrid=data.frame(alpha=0.55,lambda=0.0377366))

p1<-NULL
p2<-NULL
p3<-NULL
p4<-NULL
p5<-NULL
p6<-NULL
p7<-NULL
p8<-NULL
p8<-NULL
p9<-NULL

#In this loop we train each model on 2/3 of the data and then predict on the remaining 
# holdout, this is done for each fold, thus for the entire training set, we get
# predictions for each data sample (where the model has been trained on two-thirds)
# of the training data)
# We use out of fold preidction for the stacked data, as the final model
#should account for the fact that the prediction are made on test data, despite being
# trained on training data

for (i in 1:3){
  rf_cv1<-randomForest(x=fd[-cvst[[i]],-34],y=fd[-cvst[[i]],34],mtry=10)
  rf_cv2<-randomForest(x=fd[-cvst[[i]],-34],y=fd[-cvst[[i]],34],mtry=20)
  rf_cv3<-randomForest(x=fd[-cvst[[i]],-34],y=fd[-cvst[[i]],34],mtry=30)
  
  gbm_cv1<-gbm(revenue~.,data=fd[-cvst[[i]],],n.trees=50,interaction.depth=3)
  gbm_cv2<-gbm(revenue~.,data=fd[-cvst[[i]],],n.trees=100,interaction.depth=2)
  gbm_cv3<-gbm(revenue~.,data=fd[-cvst[[i]],],n.trees=150,interaction.depth=1)
  
  lr_cv1<-train(revenue~.,data=fd[-cvst[[i]],],method="glmnet",trControl=trainControl(method="cv", number = 2),tuneGrid=data.frame(alpha=1,lambda=0.377366))
  lr_cv2<-train(revenue~.,data=fd[-cvst[[i]],],method="glmnet",trControl=trainControl(method="cv", number = 2),tuneGrid=data.frame(alpha=0.1,lambda=0.00377366))
  lr_cv3<-train(revenue~.,data=fd[-cvst[[i]],],method="glmnet",trControl=trainControl(method="cv", number = 2),tuneGrid=data.frame(alpha=0.55,lambda=0.0377366))
  
  p1<-c(p1,predict(rf_cv1,newdata=fd[cvst[[i]],-34]))
  p2<-c(p2,predict(rf_cv2,newdata=fd[cvst[[i]],-34]))
  p3<-c(p3,predict(rf_cv3,newdata=fd[cvst[[i]],-34]))
  p4<-c(p4,predict(gbm_cv1,newdata=fd[cvst[[i]],-34],n.trees=50))
  p5<-c(p5,predict(gbm_cv2,newdata=fd[cvst[[i]],-34],n.trees=100))
  p6<-c(p6,predict(gbm_cv3,newdata=fd[cvst[[i]],-34],n.trees=150))
  p7<-c(p7,predict.train(lr_cv1,newdata=fd[cvst[[i]],]))
  p8<-c(p8,predict.train(lr_cv1,newdata=fd[cvst[[i]],]))
  p9<-c(p9,predict.train(lr_cv1,newdata=fd[cvst[[i]],]))
}

#To do the training of the stacked model, we use the original dataset, plus the
#predicted values

total_df=fd[c(cvst[[1]],cvst[[2]],cvst[[3]]),]
total_df$rf1=p1
total_df$rf2=p2
total_df$rf3=p3
total_df$gbm1=p4
total_df$gbm2=p5
total_df$gbm3=p6
total_df$lr1=p7
total_df$lr2=p8
total_df$lr3=p9

# Now we train a random forest on the stacked training set ( we use random forest as it gave 
# us the best performance on the stacked data compared to gradient boosting and regression)
rf_stack<-train(revenue ~.,data=total_df,method="rf",trControl=trainControl(method="cv", number = 5) )


# Now we build the stacked testing set, we get predicted values on the test set from
# the models trained on the entire training set (not the models trained on only 2/3 of the data)
pt1<-predict(rf1,newdata=fdt)
pt2<-predict(rf2,newdata=fdt)
pt3<-predict(rf3,newdata=fdt)
pt4<-predict(gbm1,newdata=fdt,n.trees=50)
pt5<-predict(gbm2,newdata=fdt,n.trees=100)
pt6<-predict(gbm3,newdata=fdt,n.trees=150)
pt7<-predict.train(lr1,newdata=fdt)
pt8<-predict.train(lr1,newdata=fdt)
pt9<-predict.train(lr1,newdata=fdt)

fdt_stack<-mutate(fdt,rf1=pt1,rf2=pt2,rf2=pt2,rf3=pt3,gbm1=pt4,gbm1=pt5,gbm1=pt6,lr1=pt7,lr1=pt8,lr1=pt9)

#And now we used the randomForest model trained on the data to make the final prediction
#on the testing data

final_p<-predict.train(rf_stack,fdt_stack)

# We find the final stacked model has a smaller RMSE than any of the individual models

 

