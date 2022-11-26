library(dplyr)
library(ggplot2)
library(truncnorm)
traindata_pre<-read.csv("C:/Users/ISCYI/Desktop/S12/BP/Team_scores/predi/12.12.-splits/train/new.csv")
traindata<-select(traindata_pre,gameId,gameDuration,teamVictory,
                  Player_1_ban,	Player_1_pick,Player_1_position,
                  Player_2_ban,	Player_2_pick,Player_2_position,
                  Player_3_ban,	Player_3_pick,Player_3_position,
                  Player_4_ban,	Player_4_pick,Player_4_position,
                  Player_5_ban,	Player_5_pick,Player_5_position,
                  Player_6_ban,	Player_6_pick,Player_6_position,
                  Player_7_ban,	Player_7_pick,Player_7_position,
                  Player_8_ban,	Player_8_pick,Player_8_position,
                  Player_9_ban,	Player_9_pick,Player_9_position,
                  Player_10_ban,	Player_10_pick,Player_10_position)
traindata$gameDuration<-as.numeric(traindata$gameDuration)
traindata1<-filter(traindata,gameDuration >= 1000)

traindata_team1<-data.frame(
  gameID=c(traindata1$gameId),
  team_victory=c(traindata1$teamVictory),
  team1_Top_pick=c(1:length(traindata1$gameId)),
  team1_Jun_pick=c(1:length(traindata1$gameId)),
  team1_Mid_pick=c(1:length(traindata1$gameId)),
  team1_Adc_pick=c(1:length(traindata1$gameId)),
  team1_Sup_pick=c(1:length(traindata1$gameId)),
  team1_ban_1=c(1:length(traindata1$gameId)),
  team1_ban_2=c(1:length(traindata1$gameId)),
  team1_ban_3=c(1:length(traindata1$gameId)),
  team1_ban_4=c(1:length(traindata1$gameId)),
  team1_ban_5=c(1:length(traindata1$gameId))
)
traindata_team2<-data.frame(
  gameID=c(traindata1$gameId),
  team_victory=c(traindata1$teamVictory),
  team2_Top_pick=c(1:length(traindata1$gameId)),
  team2_Jun_pick=c(1:length(traindata1$gameId)),
  team2_Mid_pick=c(1:length(traindata1$gameId)),
  team2_Adc_pick=c(1:length(traindata1$gameId)),
  team2_Sup_pick=c(1:length(traindata1$gameId)),
  team2_ban_1=c(1:length(traindata1$gameId)),
  team2_ban_2=c(1:length(traindata1$gameId)),
  team2_ban_3=c(1:length(traindata1$gameId)),
  team2_ban_4=c(1:length(traindata1$gameId)),
  team2_ban_5=c(1:length(traindata1$gameId))
)


#transform position data to traindata_team1

for (i in 1:length(traindata1$gameId)) {
  for (j in 3:18) {
    if(traindata1[i,j]=="TOP"|traindata1[i,j]=="MIDDLE"|traindata1[i,j]=="JUNGLE"|traindata1[i,j]=="BOTTOM"
       |traindata1[i,j]=="UTILITY")
      if(traindata1[i,j]=="TOP"){
        traindata_team1$team1_Top_pick[i]<-traindata1[i,j-1]
        traindata_team1$team1_ban_1[i]<-traindata1[i,j-2]
      }else if(traindata1[i,j]=="MIDDLE"){
        traindata_team1$team1_Mid_pick[i]<-traindata1[i,j-1]
        traindata_team1$team1_ban_2[i]<-traindata1[i,j-2]
      }else if(traindata1[i,j]=="JUNGLE"){
        traindata_team1$team1_Jun_pick[i]<-traindata1[i,j-1]
        traindata_team1$team1_ban_3[i]<-traindata1[i,j-2]
      }else if(traindata1[i,j]=="BOTTOM"){
        traindata_team1$team1_Adc_pick[i]<-traindata1[i,j-1]
        traindata_team1$team1_ban_4[i]<-traindata1[i,j-2]
      }else {
        traindata_team1$team1_Sup_pick[i]<-traindata1[i,j-1]
        traindata_team1$team1_ban_5[i]<-traindata1[i,j-2]
      }
  }
} 



#transform position data to traindata_team2 

for (i in 1:length(traindata1$gameId)) {
  for (j in 19:33) {
    if(traindata1[i,j]=="TOP"|traindata1[i,j]=="MIDDLE"|
       traindata1[i,j]=="JUNGLE"|traindata1[i,j]=="BOTTOM"
       |traindata1[i,j]=="UTILITY")
      if(traindata1[i,j]=="TOP"){
        traindata_team2$team2_Top_pick[i]<-traindata1[i,j-1]
        traindata_team2$team2_ban_1[i]<-traindata1[i,j-2]
      }else if(traindata1[i,j]=="MIDDLE"){
        traindata_team2$team2_Mid_pick[i]<-traindata1[i,j-1]
        traindata_team2$team2_ban_2[i]<-traindata1[i,j-2]
      }else if(traindata1[i,j]=="JUNGLE"){
        traindata_team2$team2_Jun_pick[i]<-traindata1[i,j-1]
        traindata_team2$team2_ban_3[i]<-traindata1[i,j-2]
      }else if(traindata1[i,j]=="BOTTOM"){
        traindata_team2$team2_Adc_pick[i]<-traindata1[i,j-1]
        traindata_team2$team2_ban_4[i]<-traindata1[i,j-2]
      }else {
        traindata_team2$team2_Sup_pick[i]<-traindata1[i,j-1]
        traindata_team2$team2_ban_5[i]<-traindata1[i,j-2]
      }
  }
} 

#whether pick OP champions
#team1
n1<-rep(0,length(traindata_team1$gameID))

for (i in 1:length(traindata_team1$gameID)) {
  for (j in 3:7) {
    if(traindata_team1[i,j]=="266"|traindata_team1[i,j]=="114"|
       traindata_team1[i,j]=="113"|traindata_team1[i,j]=="104"|
       traindata_team1[i,j]=="57"|traindata_team1[i,j]=="517"|
       traindata_team1[i,j]=="268"|traindata_team1[i,j]=="84"|
       traindata_team1[i,j]=="112"|traindata_team1[i,j]=="51"|
       traindata_team1[i,j]=="429"|traindata_team1[i,j]=="523"|
       traindata_team1[i,j]=="236"|traindata_team1[i,j]=="350"|
       traindata_team1[i,j]=="888")n1[i]=n1[i]+1
    
  }
}
traindata_team1<-mutate(traindata_team1,n1)


#team2
n2<-rep(0,length(traindata_team1$gameID))
for (i in 1:length(traindata_team1$gameID)) {
  for (j in 3:7) {
    if(traindata_team2[i,j]=="266"|traindata_team2[i,j]=="114"|
       traindata_team2[i,j]=="113"|traindata_team2[i,j]=="104"|
       traindata_team2[i,j]=="57"|traindata_team2[i,j]=="517"|
       traindata_team2[i,j]=="268"|traindata_team2[i,j]=="84"|
       traindata_team2[i,j]=="112"|traindata_team2[i,j]=="51"|
       traindata_team2[i,j]=="429"|traindata_team2[i,j]=="523"|
       traindata_team2[i,j]=="236"|traindata_team2[i,j]=="350"|
       traindata_team2[i,j]=="888")n2[i]=n2[i]+1
    
  }
}

traindata_team2 <-mutate(traindata_team2,n2)
#double bottom
#team1
a1<-rep(0,length(traindata_team1$gameID))

for (i in 1:length(traindata_team1$gameID)) {
  if(traindata_team1$team1_Adc_pick[i]== "145" && traindata_team1$team1_Sup_pick[i]=="526")a1[i]=a1[i]+1
}
for (i in 1:length(traindata_team1$gameID)) {
  if(traindata_team1$team1_Adc_pick[i]== "15" && traindata_team1$team1_Sup_pick[i]=="350")a1[i]=a1[i]+1
}
for (i in 1:length(traindata_team1$gameID)) {
  if(traindata_team1$team1_Adc_pick[i]== "523" && traindata_team1$team1_Sup_pick[i]=="117")a1[i]=a1[i]+1
}
for (i in 1:length(traindata_team1$gameID)) {
  if(traindata_team1$team1_Adc_pick[i]== "110" && traindata_team1$team1_Sup_pick[i]=="223")a1[i]=a1[i]+1
}
for (i in 1:length(traindata_team1$gameID)) {
  if(traindata_team1$team1_Adc_pick[i]== "110" && traindata_team1$team1_Sup_pick[i]=="888")a1[i]=a1[i]+1
}
for (i in 1:length(traindata_team1$gameID)) {
  if(traindata_team1$team1_Adc_pick[i]== "81" && traindata_team1$team1_Sup_pick[i]=="43")a1[i]=a1[i]+1
}
for (i in 1:length(traindata_team1$gameID)) {
  if(traindata_team1$team1_Adc_pick[i]== "18" && traindata_team1$team1_Sup_pick[i]=="526")a1[i]=a1[i]+1
}
for (i in 1:length(traindata_team1$gameID)) {
  if(traindata_team1$team1_Adc_pick[i]== "51" && traindata_team1$team1_Sup_pick[i]=="99")a1[i]=a1[i]+2
}
for (i in 1:length(traindata_team1$gameID)) {
  if(traindata_team1$team1_Adc_pick[i]== "236" && traindata_team1$team1_Sup_pick[i]=="267")a1[i]=a1[i]+2
}
for (i in 1:length(traindata_team1$gameID)) {
  if(traindata_team1$team1_Adc_pick[i]== "22" && traindata_team1$team1_Sup_pick[i]=="74")a1[i]=a1[i]+2
}
for (i in 1:length(traindata_team1$gameID)) {
  if(traindata_team1$team1_Adc_pick[i]== "429" && traindata_team1$team1_Sup_pick[i]=="888")a1[i]=a1[i]+2
}
traindata_team1<- mutate(traindata_team1,a1)

#team2


a2<-rep(0,length(traindata_team1$gameID))

for (i in 1:length(traindata_team1$gameID)) {
  if(traindata_team2$team2_Adc_pick[i]== "145" && traindata_team2$team2_Sup_pick[i]=="526")a2[i]=a2[i]+1
}
for (i in 1:length(traindata_team1$gameID)) {
  if(traindata_team2$team2_Adc_pick[i]== "15" && traindata_team2$team2_Sup_pick[i]=="350")a2[i]=a2[i]+1
}
for (i in 1:length(traindata_team1$gameID)) {
  if(traindata_team2$team2_Adc_pick[i]== "523" && traindata_team2$team2_Sup_pick[i]=="117")a2[i]=a2[i]+1
}
for (i in 1:length(traindata_team1$gameID)) {
  if(traindata_team2$team2_Adc_pick[i]== "110" && traindata_team2$team2_Sup_pick[i]=="223")a2[i]=a2[i]+1
}
for (i in 1:length(traindata_team1$gameID)) {
  if(traindata_team2$team2_Adc_pick[i]== "110" && traindata_team2$team2_Sup_pick[i]=="888")a2[i]=a2[i]+1
}
for (i in 1:length(traindata_team1$gameID)) {
  if(traindata_team2$team2_Adc_pick[i]== "81" && traindata_team2$team2_Sup_pick[i]=="43")a2[i]=a2[i]+1
}
for (i in 1:length(traindata_team1$gameID)) {
  if(traindata_team2$team2_Adc_pick[i]== "18" && traindata_team2$team2_Sup_pick[i]=="526")a2[i]=a2[i]+1
}
for (i in 1:length(traindata_team1$gameID)) {
  if(traindata_team2$team2_Adc_pick[i]== "51" && traindata_team2$team2_Sup_pick[i]=="99")a2[i]=a2[i]+2
}
for (i in 1:length(traindata_team1$gameID)) {
  if(traindata_team2$team2_Adc_pick[i]== "236" && traindata_team2$team2_Sup_pick[i]=="267")a2[i]=a2[i]+2
}
for (i in 1:length(traindata_team1$gameID)) {
  if(traindata_team2$team2_Adc_pick[i]== "22" && traindata_team2$team2_Sup_pick[i]=="74")a2[i]=a2[i]+2
}
for (i in 1:length(traindata_team1$gameID)) {
  if(traindata_team2$team2_Adc_pick[i]== "429" && traindata_team2$team2_Sup_pick[i]=="888")a2[i]=a2[i]+2
}
traindata_team2<- mutate(traindata_team2,a2)


#CSM
#team2
b2<-rep(0,length(traindata_team1$gameID))
for (i in 1:length(traindata_team1$gameID)) {
  if(traindata_team2$team2_Adc_pick[i]== "145")b2[i]=b2[i]+10.8
}
for (i in 1:length(traindata_team1$gameID)) {
  if(traindata_team2$team2_Adc_pick[i]== "81")b2[i]=b2[i]+10.73
}      
for (i in 1:length(traindata_team1$gameID)) {
  if(traindata_team2$team2_Adc_pick[i]== "15")b2[i]=b2[i]+10.5
}       
for (i in 1:length(traindata_team1$gameID)) {
  if(traindata_team2$team2_Adc_pick[i]== "51")b2[i]=b2[i]+10.32
}       
for (i in 1:length(traindata_team1$gameID)) {
  if(traindata_team2$team2_Adc_pick[i]== "110")b2[i]=b2[i]+10.22
}       
for (i in 1:length(traindata_team1$gameID)) {
  if(traindata_team2$team2_Adc_pick[i]== "18")b2[i]=b2[i]+10.05
}      
for (i in 1:length(traindata_team1$gameID)) {
  if(traindata_team2$team2_Adc_pick[i]== "236")b2[i]=b2[i]+9.95
}       
for (i in 1:length(traindata_team1$gameID)) {
  if(traindata_team2$team2_Adc_pick[i]== "22")b2[i]=b2[i]+9.9
}       
for (i in 1:length(traindata_team1$gameID)) {
  if(traindata_team2$team2_Adc_pick[i]== "523")b2[i]=b2[i]+9.63
}       
for (i in 1:length(traindata_team1$gameID)) {
  if(traindata_team2$team2_Adc_pick[i]== "429")b2[i]=b2[i]+9.4
}      
traindata_team2<- mutate(traindata_team2,b2)      


#team1
b1<-rep(0,length(traindata_team1$gameID))
for (i in 1:length(traindata_team1$gameID)) {
  if(traindata_team1$team1_Adc_pick[i]== "145")b1[i]=b1[i]+10.8
}
for (i in 1:length(traindata_team1$gameID)) {
  if(traindata_team1$team1_Adc_pick[i]== "81")b1[i]=b1[i]+10.73
}      
for (i in 1:length(traindata_team1$gameID)) {
  if(traindata_team1$team1_Adc_pick[i]== "15")b1[i]=b1[i]+10.5
}       
for (i in 1:length(traindata_team1$gameID)) {
  if(traindata_team1$team1_Adc_pick[i]== "51")b1[i]=b1[i]+10.32
}       
for (i in 1:length(traindata_team1$gameID)) {
  if(traindata_team1$team1_Adc_pick[i]== "110")b1[i]=b1[i]+10.22
}       
for (i in 1:length(traindata_team1$gameID)) {
  if(traindata_team1$team1_Adc_pick[i]== "18")b1[i]=b1[i]+10.05
}      
for (i in 1:length(traindata_team1$gameID)) {
  if(traindata_team1$team1_Adc_pick[i]== "236")b1[i]=b1[i]+9.95
}       
for (i in 1:length(traindata_team1$gameID)) {
  if(traindata_team1$team1_Adc_pick[i]== "22")b1[i]=b1[i]+9.9
}       
for (i in 1:length(traindata_team1$gameID)) {
  if(traindata_team1$team1_Adc_pick[i]== "523")b1[i]=b1[i]+9.63
}       
for (i in 1:length(traindata_team1$gameID)) {
  if(traindata_team1$team1_Adc_pick[i]== "429")b1[i]=b1[i]+9.4
}      
traindata_team1<- mutate(traindata_team1,b1)      




#ban OP champions
#team1
c1<-rep(0,length(traindata_team1$gameID))
for (i in 1:length(traindata_team1$gameID)) {
  for (j in 8:12) {
    if(traindata_team1[i,j]=="266"|traindata_team1[i,j]=="114"|
       traindata_team1[i,j]=="113"|traindata_team1[i,j]=="104"|
       traindata_team1[i,j]=="57"|traindata_team1[i,j]=="517"|
       traindata_team1[i,j]=="268"|traindata_team1[i,j]=="84"|
       traindata_team1[i,j]=="112"|traindata_team1[i,j]=="51"|
       traindata_team1[i,j]=="429"|traindata_team1[i,j]=="523"|
       traindata_team1[i,j]=="236"|traindata_team1[i,j]=="350"|
       traindata_team1[i,j]=="888")c1[i]=c1[i]+1
    
  }
}
traindata_team1<-mutate(traindata_team1,c1)


#team2
c2<-rep(0,length(traindata_team1$gameID))
for (i in 1:length(traindata_team1$gameID)) {
  for (j in 8:12) {
    if(traindata_team2[i,j]=="266"|traindata_team2[i,j]=="114"|
       traindata_team2[i,j]=="113"|traindata_team2[i,j]=="104"|
       traindata_team2[i,j]=="57"|traindata_team2[i,j]=="517"|
       traindata_team2[i,j]=="268"|traindata_team2[i,j]=="84"|
       traindata_team2[i,j]=="112"|traindata_team2[i,j]=="51"|
       traindata_team2[i,j]=="429"|traindata_team2[i,j]=="523"|
       traindata_team2[i,j]=="236"|traindata_team2[i,j]=="350"|
       traindata_team2[i,j]=="888")c2[i]=c2[i]+1
    
  }
}

traindata_team2 <-mutate(traindata_team2,c2)



#whether follow the rules
#rule1
x1<-rep(0,length(traindata_team1$gameID))
for (i in 1:length(traindata_team1$gameID)) {
  for (j in 3:7) {
    if(traindata_team1[i,j]=="223"|traindata_team1[i,j]=="412"|
       traindata_team1[i,j]=="12"|traindata_team1[i,j]=="201"|
       traindata_team1[i,j]=="32"|traindata_team1[i,j]=="526"|
       traindata_team1[i,j]=="79"|traindata_team1[i,j]=="516"|
       traindata_team1[i,j]=="150"|traindata_team1[i,j]=="57")x1[i]=x1[i]+1
    
  }
}

x2<-rep(0,length(traindata_team1$gameID))
for (i in 1:length(traindata_team1$gameID)) {
  for (j in 3:7) {
    if(traindata_team2[i,j]=="223"|traindata_team2[i,j]=="412"|
       traindata_team2[i,j]=="12"|traindata_team2[i,j]=="201"|
       traindata_team2[i,j]=="32"|traindata_team2[i,j]=="526"|
       traindata_team2[i,j]=="79"|traindata_team2[i,j]=="516"|
       traindata_team2[i,j]=="150"|traindata_team2[i,j]=="57")x2[i]=x2[i]+1
    
  }
}
#rule2
for (i in 1:length(traindata_team1$gameID)) {
  if(traindata_team1$team1_Mid_pick[i] != "777")x1[i]=x1[i]+1
}
for (i in 1:length(traindata_team1$gameID)) {
  if(traindata_team2$team2_Mid_pick[i] != "777")x2[i]=x2[i]+1
}
traindata_team1 <-mutate(traindata_team1,x1)
traindata_team2 <-mutate(traindata_team2,x2)



#transform all values in a new dataform
traindata_value1<-select(traindata_team1,x1,n1,a1,b1,c1)


#generate random data which obey normal distribution
y1<-rtruncnorm(n = traindata_team1$gameID, a =0 , b = 2, mean = 1, sd = 0.1)
z1<-rtruncnorm(n = traindata_team1$gameID, a =0 , b = 2, mean = 1, sd = 0.1)
m1<-rtruncnorm(n = traindata_team1$gameID, a =0 , b = 13, mean = 6.5, sd = 0.65)
d1<-rtruncnorm(n = traindata_team1$gameID, a =0 , b = 5, mean = 2.5, sd = 0.25)

y2<-rtruncnorm(n = traindata_team2$gameID, a =0 , b = 2, mean = 1, sd = 0.1)
z2<-rtruncnorm(n = traindata_team2$gameID, a =0 , b = 2, mean = 1, sd = 0.1)
m2<-rtruncnorm(n = traindata_team2$gameID, a =0 , b = 13, mean = 6.5, sd = 0.65)
d2<-rtruncnorm(n = traindata_team2$gameID, a =0 , b = 5, mean = 2.5, sd = 0.25)

traindata_use<-mutate(traindata_value1,y1,z1,m1,d1)
traindata_use2<-mutate(traindata_value2,y2,z2,m2,d2)
#uniformization
min.max.norm <- function(x){
  (x-min(x))/(max(x)-min(x))
}

traindata_use1 <- apply(traindata_use[,-c(0,10)],2,min.max.norm)  #positive factors

#Find the total contribution of all samples to factor Xj

first1 <- function(data)
{
  x <- c(data)
  for(i in 1:length(data))
    x[i] = data[i]/sum(data[])
  return(x)
}
dataframe <- apply(traindata_use1,2,first1)
#Turn each element of the matrix generated in the previous step into the product of each element with the natural log (element) and calculate the information entropy

first2 <- function(data)
{
  x <- c(data)
  for(i in 1:length(data)){
    if(data[i] == 0){
      x[i] = 0
    }else{
      x[i] = data[i] * log(data[i])
    }
  }
  return(x)
}

dataframe1 <- apply(dataframe,2,first2)

k <- 1/log(length(dataframe1[,1]))
d <- -k * colSums(dataframe1)
#Calculate the redundancy
d <- 1-d
#Calculate the weights of each factor
w <- d/sum(d)
w

w<-as.data.frame(w)
#marking function
#x
mark_function<-function(x1,y1,z1,m1,n1,a1,b1,c1,d1){
vec.x1 =rep(0,length(traindata_team1$gameID))

for (i in 1:length(traindata_team1$gameID)) {
  if (x1[i]==0) {
    vec.x1[i]=0
  }else if(x1[i]==1){
    vec.x1[i]=5
  }else{
    vec.x1[i]=10}
}

#y

vec.y1 =rep(0,length(traindata_team1$gameID))

for (i in 1:length(traindata_team1$gameID)) {
  if (y1[i]>=0 &y1[i]<0.66) {
    vec.y1[i]=0
  }else if(y1[i]>=0.66 &y1[i]<1.32){
    vec.y1[i]=5
  }else{
    vec.y1[i]=10}
}


#z
vec.z1 =rep(0,length(traindata_team1$gameID))

for (i in 1:length(traindata_team1$gameID)) {
  if (z1[i]>=0 &z1[i]<0.66) {
    vec.z1[i]=0
  }else if(z1[i]>=0.66 &z1[i]<1.32){
    vec.z1[i]=5
  }else{
    vec.z1[i]=10}
}
#m
vec.m1 =rep(0,length(traindata_team1$gameID))
for (i in 1:length(traindata_team1$gameID)) {
  if (m1[i]>=0 &m1[i]<=2.17) {
    vec.m1[i]=0
  }else if(m1[i]>2.17 &m1[i]<=4.34){
    vec.m1[i]=2
  }else if(m1[i]>4.34 &m1[i]<=6.51){
    vec.m1[i]=4
  }else if(m1[i]>6.51 &m1[i]<=8.68){
    vec.m1[i]=6}
  else if(m1[i]>8.68 &m1[i]<=10.85){
    vec.m1[i]=8}else{vec.m1[i]=10}
}
#n
vec.n1 =rep(0,length(traindata_team1$gameID))

for (i in 1:length(traindata_team1$gameID)) {
  if (n1[i]==0) {
    vec.n1[i]=0
  }else {vec.n1[i]=10}
}
#a

vec.a1 =rep(0,length(traindata_team1$gameID))

for (i in 1:length(traindata_team1$gameID)) {
  if (a1[i]==0) {
    vec.a1[i]=0
  }else if(a1[i]==1){
    vec.a1[i]=5
  }else{
    vec.a1[i]=10}
}

#b

vec.b1 =rep(0,length(traindata_team1$gameID))
for (i in 1:length(traindata_team1$gameID)) {
  if (b1[i]>=9.4 &b1[i]<=9.63) {
    vec.b1[i]=0
  }else if(b1[i]>9.63 &b1[i]<=9.86){
    vec.b1[i]=2
  }else if(b1[i]>9.86 &b1[i]<=10.09){
    vec.b1[i]=4
  }else if(b1[i]>10.09 &b1[i]<=10.32){
    vec.b1[i]=6}
  else if(b1[i]>10.32 &b1[i]<=10.55){
    vec.b1[i]=8}else{vec.b1[i]=10}
}
#c
vec.c1 =rep(0,length(traindata_team1$gameID))
for (i in 1:length(traindata_team1$gameID)) {
  if (c1[i]==0) {
    vec.c1[i]=0
  }else if(c1[i]==1){
    vec.c1[i]=2
  }else if(c1[i]==2){
    vec.c1[i]=4
  }else if(c1[i]==3){
    vec.c1[i]=6}
  else if(c1[i]==4){
    vec.c1[i]=8}else{vec.c1[i]=10}
}

#d
vec.d1 =rep(0,length(traindata_team1$gameID))
for (i in 1:length(traindata_team1$gameID)) {
  if (d1[i]==0) {
    vec.d1[i]=0
  }else if(d1[i]==1){
    vec.d1[i]=2
  }else if(d1[i]==2){
    vec.d1[i]=4
  }else if(d1[i]==3){
    vec.d1[i]=6}
  else if(d1[i]==4){
    vec.d1[i]=8}else{vec.d1[i]=10}
}
return(vec.x1*w[1,1]+vec.y1*w[6,1]+vec.z1*w[7,1]+vec.m1*w[8,1]
       +vec.n1*w[2,1]+vec.a1*w[3,1]+vec.b1*w[4,1]+vec.c1*w[5,1]+vec.d1*w[9,1])
}


#calculate the accuracy rate of the marking model









