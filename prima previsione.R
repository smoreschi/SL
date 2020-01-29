# MOdelli 
library(MASS)
library(caret)

# ripeti prima di ogni passaggio 
set.seed(123)
cv <- trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = 2
)
grid <- expand.grid(
  n.trees = c(500, 1000, 1500),  # 3 scelte per n, lambda e d (rispettivamente)
  shrinkage = c(0.001, 0.005,0.01),
  interaction.depth = c(2,3,4,5,6), 
  n.minobsinnode = c(5,10)
)
#
boost15 <- train(
  post~., nc15,
  method = "gbm",
  tuneGrid = grid,
  verbose = FALSE,
  trControl=cv)
boost16 <- train(
  post~., nc16,
  method = "gbm",
  tuneGrid = grid,
  verbose = FALSE,
  trControl=cv)
boost17 <- train(
  post~., nc17,
  method = "gbm",
  tuneGrid = grid,
  verbose = FALSE,
  trControl=cv)
boost18 <- train(
  post~., nc18,
  method = "gbm",
  tuneGrid = grid,
  verbose = FALSE,
  trControl=cv)
boost19 <- train(
  post~., nc19,
  method = "gbm",
  tuneGrid = grid,
  verbose = FALSE,
  trControl=cv)


# uso 15 come modello
library(pROC)
phat.15_16 = predict(boost15, nc16, type = "prob")[2]
phat.15_16=as.vector(as.matrix(phat.15_16)) 
roc(response = nc16$post, predictor = phat.15_16, levels = c("yes","no"))$auc 
phat.15_16=factor(ifelse(phat.15_16>=0.5, "yes", "no"))
confusionMatrix(phat.15_16, nc16$post, positive='yes')
 

phat.15_17 = predict(boost15, nc17, type = "prob")[2]
phat.15_17=as.vector(as.matrix(phat.15_17)) 
roc(response = nc17$post, predictor = phat.15_17, levels = c("yes","no"))$auc 
phat.15_17=factor(ifelse(phat.15_17>=0.5, "yes", "no"))
confusionMatrix(phat.15_17, nc17$post, positive='yes')


phat.15_18 = predict(boost15, nc18, type = "prob")[2]
phat.15_18=as.vector(as.matrix(phat.15_18)) 
roc(response = nc18$post, predictor = phat.15_18, levels = c("yes","no"))$auc 
phat.15_18=factor(ifelse(phat.15_18>=0.5, "yes", "no"))
confusionMatrix(phat.15_18, nc18$post, positive='yes')
 

phat.15_19 = predict(boost15, nc19, type = "prob")[2]
phat.15_19=as.vector(as.matrix(phat.15_19)) 
roc(response = nc19$post, predictor = phat.15_19, levels = c("yes","no"))$auc 
phat.15_19=factor(ifelse(phat.15_19>=0.5, "yes", "no"))
confusionMatrix(phat.15_19, nc19$post, positive='yes')


# uso 16 come modello ..................................................................................................
phat.16_15 = predict(boost16, nc15, type = "prob")[2]
phat.16_15=as.vector(as.matrix(phat.16_15)) 
roc(response = nc15$post, predictor = phat.16_15, levels = c("yes","no"))$auc  
phat.16_15=factor(ifelse(phat.16_15>=0.5, "yes", "no"))
confusionMatrix(phat.16_15, nc15$post, positive='yes')



phat.16_17 = predict(boost16, nc17, type = "prob")[2]
phat.16_17=as.vector(as.matrix(phat.16_17)) 
roc(response = nc17$post, predictor = phat.16_17, levels = c("yes","no"))$auc 
phat.16_17=factor(ifelse(phat.16_17>=0.5, "yes", "no"))
confusionMatrix(phat.16_17, nc17$post, positive='yes')


phat.16_18 = predict(boost16, nc18, type = "prob")[2]
phat.16_18=as.vector(as.matrix(phat.16_18)) 
roc(response = nc18$post, predictor = phat.16_18, levels = c("yes","no"))$auc  
phat.16_18=factor(ifelse(phat.16_18>=0.5, "yes", "no"))
confusionMatrix(phat.16_18, nc18$post, positive='yes')


phat.16_19 = predict(boost16, nc19, type = "prob")[2]
phat.16_19=as.vector(as.matrix(phat.16_19)) 
roc(response = nc19$post, predictor = phat.16_19, levels = c("yes","no"))$auc  
phat.16_19=factor(ifelse(phat.16_19>=0.5, "yes", "no"))
confusionMatrix(phat.16_19, nc19$post, positive='yes')


# uso 17 come modello ................................................................................................
phat.17_15 = predict(boost17, nc15, type = "prob")[2]
phat.17_15=as.vector(as.matrix(phat.17_15)) 
roc(response = nc15$post, predictor = phat.17_15, levels = c("yes","no"))$auc 
phat.17_15=factor(ifelse(phat.17_15>=0.5, "yes", "no"))
confusionMatrix(phat.17_15, nc15$post, positive='yes')


phat.17_16 = predict(boost17, nc16, type = "prob")[2]
phat.17_16=as.vector(as.matrix(phat.17_16)) 
roc(response = nc16$post, predictor = phat.17_16, levels = c("yes","no"))$auc  
phat.17_16=factor(ifelse(phat.17_16>=0.5, "yes", "no"))
confusionMatrix(phat.17_16, nc16$post, positive='yes')


phat.17_18 = predict(boost17, nc18, type = "prob")[2]
phat.17_18=as.vector(as.matrix(phat.17_18)) 
roc(response = nc18$post, predictor = phat.17_18, levels = c("yes","no"))$auc 
phat.17_18=factor(ifelse(phat.17_18>=0.5, "yes", "no"))
confusionMatrix(phat.17_18, nc18$post, positive='yes')


phat.17_19 = predict(boost17, nc19, type = "prob")[2]
phat.17_19=as.vector(as.matrix(phat.17_19)) 
roc(response = nc19$post, predictor = phat.17_19, levels = c("yes","no"))$auc 
phat.17_19=factor(ifelse(phat.17_19>=0.5, "yes", "no"))
confusionMatrix(phat.17_19, nc19$post, positive='yes')


# uso 18 come modello ................................................................................................
phat.18_15 = predict(boost18, nc15, type = "prob")[2]
phat.18_15=as.vector(as.matrix(phat.18_15)) 
roc(response = nc15$post, predictor = phat.18_15, levels = c("yes","no"))$auc 
phat.18_15=factor(ifelse(phat.18_15>=0.5, "yes", "no"))
confusionMatrix(phat.18_15, nc15$post, positive='yes')


phat.18_16 = predict(boost18, nc16, type = "prob")[2]
phat.18_16=as.vector(as.matrix(phat.18_16)) 
roc(response = nc16$post, predictor = phat.18_16, levels = c("yes","no"))$auc
phat.18_16=factor(ifelse(phat.18_16>=0.5, "yes", "no"))
confusionMatrix(phat.18_16, nc16$post, positive='yes')


phat.18_17 = predict(boost18, nc17, type = "prob")[2]
phat.18_17=as.vector(as.matrix(phat.18_17)) 
roc(response = nc17$post, predictor = phat.18_17, levels = c("yes","no"))$auc  
phat.18_17=factor(ifelse(phat.18_17>=0.5, "yes", "no"))
confusionMatrix(phat.18_17, nc17$post, positive='yes')


phat.18_19 = predict(boost18, nc19, type = "prob")[2]
phat.18_19=as.vector(as.matrix(phat.18_19)) 
roc(response = nc19$post, predictor = phat.18_19, levels = c("yes","no"))$auc  
phat.18_19=factor(ifelse(phat.18_19>=0.5, "yes", "no"))
confusionMatrix(phat.18_19, nc19$post, positive='yes')



# uso 19 come modello ................................................................................................
phat.19_15 = predict(boost19, nc15, type = "prob")[2]
phat.19_15=as.vector(as.matrix(phat.19_15)) 
roc(response = nc15$post, predictor = phat.19_15, levels = c("yes","no"))$auc  
phat.19_15=factor(ifelse(phat.19_15>=0.5, "yes", "no"))
confusionMatrix(phat.19_15, nc15$post, positive='yes')


phat.19_16 = predict(boost19, nc16, type = "prob")[2]
phat.19_16=as.vector(as.matrix(phat.19_16)) 
roc(response = nc16$post, predictor = phat.19_16, levels = c("yes","no"))$auc  
phat.19_16=factor(ifelse(phat.19_16>=0.5, "yes", "no"))
confusionMatrix(phat.19_16, nc16$post, positive='yes')


phat.19_17 = predict(boost19, nc17, type = "prob")[2]
phat.19_17=as.vector(as.matrix(phat.19_17)) 
roc(response = nc17$post, predictor = phat.19_17, levels = c("yes","no"))$auc 
phat.19_17=factor(ifelse(phat.19_17>=0.5, "yes", "no"))
confusionMatrix(phat.19_17, nc17$post, positive='yes')


phat.19_18 = predict(boost19, nc18, type = "prob")[2]
phat.19_18=as.vector(as.matrix(phat.19_18)) 
roc(response = nc18$post, predictor = phat.19_18, levels = c("yes","no"))$auc  
phat.19_18=factor(ifelse(phat.19_18>=0.5, "yes", "no"))
confusionMatrix(phat.19_18, nc18$post, positive='yes')








# ################ previsioni su anno 2020 .................
phat.15_20 = predict(boost15, nc20, type = "prob")[2]
phat.15_20=as.vector(as.matrix(phat.15_20)) 
tot.15_20=which(phat.15_20>0.5);tot.15_20
prev15=cbind(nc20[c(1,2)],phat.15_20)
prev15$post=ifelse(prev15$phat.15_20>=0.4965, "yes", "no")


phat.16_20 = predict(boost16, nc20, type = "prob")[2]
phat.16_20=as.vector(as.matrix(phat.16_20))
tot.16_20=which(phat.16_20>0.5);tot.16_20
prev16=cbind(nc20[c(1,2)],phat.16_20)
prev16$post=ifelse(prev16$phat.16_20>=0.5, "yes", "no")


phat.17_20 = predict(boost17, nc20, type = "prob")[2]
phat.17_20=as.vector(as.matrix(phat.17_20)) 
tot.17_20=which(phat.17_20>0.5)
prev17=cbind(nc20[c(1,2)],phat.17_20)
prev17$post=ifelse(prev17$phat.17_20>=0.445, "yes", "no")


phat.18_20 = predict(boost18, nc20, type = "prob")[2]
phat.18_20=as.vector(as.matrix(phat.18_20)) 
tot.18_20=which(phat.18_20>0.5)
prev18=cbind(nc20[c(1,2)],phat.18_20)
prev18$post=ifelse(prev18$phat.18_20>=0.4965, "yes", "no")


phat.19_20 = predict(boost19, nc20, type = "prob")[2]
phat.19_20=as.vector(as.matrix(phat.19_20)) 
tot.19_20=which(phat.19_20>0.5)
prev19=cbind(nc20[c(1,2)],phat.19_20)
prev19$post=ifelse(prev19$phat.19_20>0.5, "yes", "no")


#creo dataset con previsioni su 20 per ogni modello boosting
tot=cbind(phat.15_20,phat.16_20,phat.17_20,phat.18_20, phat.19_20)
tot=as.data.frame(tot)
tot$media=(tot[,1]+tot[,2]+tot[,3]+tot[,4]+tot[,5])/5
vet=c(rownames(nc20))
vet=as.data.frame(vet)
vet2=matrix(NA,347,1)
vet2<-nc20$CONF
tot=cbind(vet,vet2,tot)
colnames(tot)[c(1,2)]=c("TEAM","CONF")
tot$post="no"
vettore=c(which(tot$media==(max(tot$media[which(tot$CONF=="A10")]))),which(tot$media==(max(tot$media[which(tot$CONF=="ACC")]))),
which(tot$media==(max(tot$media[which(tot$CONF=="AE")]))),which(tot$media==(max(tot$media[which(tot$CONF=="Amer")]))),
which(tot$media==(max(tot$media[which(tot$CONF=="ASun")]))),which(tot$media==(max(tot$media[which(tot$CONF=="B10")]))),
which(tot$media==(max(tot$media[which(tot$CONF=="B12")]))),which(tot$media==(max(tot$media[which(tot$CONF=="BE")]))),
which(tot$media==(max(tot$media[which(tot$CONF=="BSky")]))),which(tot$media==(max(tot$media[which(tot$CONF=="BSth")]))),
which(tot$media==(max(tot$media[which(tot$CONF=="BW")]))),which(tot$media==(max(tot$media[which(tot$CONF=="CAA")]))),
which(tot$media==(max(tot$media[which(tot$CONF=="CUSA")]))),which(tot$media==(max(tot$media[which(tot$CONF=="Horz")]))),
which(tot$media==(max(tot$media[which(tot$CONF=="Ivy")]))),which(tot$media==(max(tot$media[which(tot$CONF=="MAAC")]))),
which(tot$media==(max(tot$media[which(tot$CONF=="MAC")]))),which(tot$media==(max(tot$media[which(tot$CONF=="MEAC")]))),
which(tot$media==(max(tot$media[which(tot$CONF=="MVC")]))),which(tot$media==(max(tot$media[which(tot$CONF=="MWC")]))),
which(tot$media==(max(tot$media[which(tot$CONF=="NEC")]))),which(tot$media==(max(tot$media[which(tot$CONF=="OVC")]))),
which(tot$media==(max(tot$media[which(tot$CONF=="P12")]))),which(tot$media==(max(tot$media[which(tot$CONF=="Pat")]))),
which(tot$media==(max(tot$media[which(tot$CONF=="SB")]))),which(tot$media==(max(tot$media[which(tot$CONF=="SC")]))),
which(tot$media==(max(tot$media[which(tot$CONF=="SEC")]))),which(tot$media==(max(tot$media[which(tot$CONF=="Slnd")]))),
which(tot$media==(max(tot$media[which(tot$CONF=="Sum")]))),which(tot$media==(max(tot$media[which(tot$CONF=="SWAC")]))),
which(tot$media==(max(tot$media[which(tot$CONF=="WAC")]))),which(tot$media==(max(tot$media[which(tot$CONF=="WCC")]))))
tot$post[vettore]="yes"
tot[-vettore,9]=ifelse(tot[-vettore,8]>=0.60, "yes", "no")
table(tot$post)
table(tot$CONF,tot$post=="yes")
tot$post=as.factor(tot$post)
# facendo così prendo almeno 1 squadra per tutti 
previsti=tot[which(tot$post=="yes"),]
previsti20=nc20[which(tot$post=="yes"),]


vet=c("Champions", "Second", "F4", "E8", "S16",  "R32", "R64", "R68", "does_not_go_to _the_postseason")
vet1=c("Champions", "Second", "F4", "E8", "S16",  "R32", "R64", "R68")
previsti20$post=factor(previsti20,levels = vet1)