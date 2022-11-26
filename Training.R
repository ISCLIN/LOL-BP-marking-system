library(dplyr)
library(ggplot2)
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
traindata_value1<-select(traindata_team1,team_victory,x1,n1,a1,b1,c1)
traindata_value2<-select(traindata_team2,x2,n2,a2,b2,c2)
traindata_value<-mutate(traindata_value1,traindata_value2)



#PCA
com1 <- prcomp(traindata_value[,2:11], center = TRUE,scale. = TRUE)
summary(com1)


df1<-com1$x
head(df1)


df1<-data.frame(df1,traindata_team1$team_victory)
head(df1)

library(ggplot2)

#Extract the variance contribution rate of the principal component and generate the axis title
summ<-summary(com1)
xlab<-paste0("PC1(",round(summ$importance[2,1]*100,2),"%)")
ylab<-paste0("PC2(",round(summ$importance[2,2]*100,2),"%)")
p2<-ggplot(data = df1,aes(x=PC1,y=PC2,color=traindata_team1.team_victory))+
  stat_ellipse(aes(fill=traindata_team1.team_victory),
               type = "norm", geom ="polygon",alpha=0.2,color=NA)+
  geom_point()+labs(x=xlab,y=ylab,color="")+
  guides(fill=F)
p2+scale_fill_manual(values = c("purple","orange","blue"))+
  scale_colour_manual(values = c("purple","orange","blue"))





