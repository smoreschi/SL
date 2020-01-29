nc15=read.csv("C:/Users/simo2/Documents/Università/ESAMI/ALDO/statistical learning/progetto/dati_completi/nc15.csv", sep=" ")
nc16=read.csv("C:/Users/simo2/Documents/Università/ESAMI/ALDO/statistical learning/progetto/dati_completi/nc16.csv", sep=" ")
nc17=read.csv("C:/Users/simo2/Documents/Università/ESAMI/ALDO/statistical learning/progetto/dati_completi/nc17.csv", sep=" ")
nc18=read.csv("C:/Users/simo2/Documents/Università/ESAMI/ALDO/statistical learning/progetto/dati_completi/nc18.csv", sep=" ")
nc19=read.csv("C:/Users/simo2/Documents/Università/ESAMI/ALDO/statistical learning/progetto/dati_completi/nc19.csv", sep=" ")
nc20=read.csv("C:/Users/simo2/Documents/Università/ESAMI/ALDO/statistical learning/progetto/dati_completi/nc20.csv", sep=" ")
p19=nc19[which(tot$post=="yes"),]
p18=nc18[which(tot$post=="yes"),]
p17=nc17[which(tot$post=="yes"),] 
p16=nc16[which(tot$post=="yes"),] 
p15=nc15[which(tot$post=="yes"),]

#19
table(p19$POSTSEASON) # POSSO LAVORARE SOLO SU DI LUI
p19$POSTSEASON=ifelse(p19$POSTSEASON %in% c("Champions" , "Second", "F4", "E8", "S16"), "S16",  "does_not_go_to_S16")
p19$POSTSEASON=factor(p19$POSTSEASON, levels=c("S16", "does_not_go_to_S16"))
table(p19$POSTSEASON)
rownames(p19)=p19[,1]
p19=p19[,-c(1,2,36)]
# 18
table(p18$POSTSEASON) # POSSO LAVORARE SOLO SU DI LUI
p18$POSTSEASON=ifelse(p18$POSTSEASON %in% c("Champions" , "Second", "F4", "E8", "S16"), "S16",  "does_not_go_to_S16")
p18$POSTSEASON=factor(p18$POSTSEASON, levels=c("S16", "does_not_go_to_S16"))
table(p18$POSTSEASON)
rownames(p18)=p18[,1]
p18=p18[,-c(1,2,36)]
# 17
table(p17$POSTSEASON) # POSSO LAVORARE SOLO SU DI LUI
p17$POSTSEASON=ifelse(p17$POSTSEASON %in% c("Champions" , "Second", "F4", "E8", "S16"), "S16",  "does_not_go_to_S16")
p17$POSTSEASON=factor(p17$POSTSEASON, levels=c("S16", "does_not_go_to_S16"))
table(p17$POSTSEASON)
rownames(p17)=p17[,1]
p17=p17[,-c(1,2,36)]
# 16
table(p16$POSTSEASON)
p16$POSTSEASON=ifelse(p16$POSTSEASON %in% c("Champions" , "Second", "F4", "E8", "S16"), "S16",  "does_not_go_to_S16")
p16$POSTSEASON=factor(p16$POSTSEASON, levels=c("S16", "does_not_go_to_S16"))
table(p16$POSTSEASON)
rownames(p16)=p16[,1]
p16=p16[,-c(1,2,36)]
# 15
table(p15$POSTSEASON)
p15$POSTSEASON=ifelse(p15$POSTSEASON %in% c("Champions" , "Second", "F4", "E8", "S16"), "S16",  "does_not_go_to_S16")
p15$POSTSEASON=factor(p15$POSTSEASON, levels=c("S16", "does_not_go_to_S16"))
table(p15$POSTSEASON)
rownames(p15)=p15[,1]
p15=p15[,-c(1,2,36)]


library(MASS)
library(caret)

# ripeti passaggi tutte le volte
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
bst19 <- train(
  POSTSEASON~., p19,
  method = "gbm",
  tuneGrid = grid,
  verbose = FALSE,
  trControl=cv)

bst18 <- train(
  POSTSEASON~., p18,
  method = "gbm",
  tuneGrid = grid,
  verbose = FALSE,
  trControl=cv)

bst17 <- train(
  POSTSEASON~., p17,
  method = "gbm",
  tuneGrid = grid,
  verbose = FALSE,
  trControl=cv)

bst16 <- train(
  POSTSEASON~., p16,
  method = "gbm",
  tuneGrid = grid,
  verbose = FALSE,
  trControl=cv)

bst15 <- train(
  POSTSEASON~., p15,
  method = "gbm",
  tuneGrid = grid,
  verbose = FALSE,
  trControl=cv)
summary(bst15) 



###############################################################################ààààààààààààààààààààààààààààààààààààààààù

##########################################################################################ààààààààààààààààààààààààààààààààààààà
previsti20=read.csv("C:/Users/simo2/Documents/Università/ESAMI/ALDO/statistical learning/progetto/dati_completi/previsti20.csv", sep=" ")
rownames(previsti20)=previsti20[,1]
previsti20=previsti20[,-c(1,2,35)]  #tolgo: team conf e post
previsti20$POSTSEASON=NA
previsti20$POSTSEASON=factor(previsti20$POSTSEASON, levels=c("S16", "does_not_go_to_S16"))

pv19prob.su.p20=predict(bst19, type="prob", newdata=previsti20)[,1]
pv18prob.su.p20=predict(bst18, type="prob", newdata=previsti20)[,1]
pv17prob.su.p20=predict(bst17, type="prob", newdata=previsti20)[,1]
pv16prob.su.p20=predict(bst16, type="prob", newdata=previsti20)[,1]
pv15prob.su.p20=predict(bst15, type="prob", newdata=previsti20)[,1]

# stacking
pv=cbind(pv15prob.su.p20, pv16prob.su.p20, pv17prob.su.p20, pv18prob.su.p20 ,pv19prob.su.p20)
pv=as.data.frame(pv)
rownames(pv) <- rownames(p19)
pv$medie=(pv[,1] + pv[,2])/2
pv.prese=pv[which(pv[,3]>0.5),]
pv.prese$percentuali=(pv.prese$medie/sum(pv.prese$medie))*100
pv.prese$TEAM=c("Arizona", "Baylor", "Butler", "Dayton", "Duke", "Kansas", "Lousville", "Michigan St.", 
                "Ohio St.", "San Diego St,", "West Virginia")

TEAM=c("Arizona", "Baylor", "Butler", "Dayton", "Duke", "Kansas", "Lousville", "Michigan St.", 
                "Ohio St.", "San Diego St,", "West Virginia")
previsti20$POSTSEASON=ifelse(rownames(previsti20) %in% TEAM, "S16", "no_S16")
