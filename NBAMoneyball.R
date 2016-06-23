setwd("C:/Users/DELL/Desktop/ISSMTECHASSIGNMENTS/Kaggle/Basketball/")
NBA=read.csv("NBA_train.csv")
str(NBA)
with(NBA,plot(W,col=factor(Playoffs)))
abline(h=42,col="blue",lwd=2,lty=2)
NBA$PTSdiff=NBA$PTS-NBA$oppPTS
cor(NBA$PTSdiff,NBA$W)
plot(NBA$PTSdiff,NBA$W)

winsReg=lm(W~PTSdiff,NBA)
summary(winsReg)

# Thus the number of wins can be defines as
# W=41+0,0326*PTSdiff
# So, if we need to win greater than 42 mathes to reach playoff, 
# We need (42-41)/0,0326 Points diff (PTSDiff)
# Thus we need to achieve 30.67 Ptsdiff to reach playoff

# We now predict PTS and OppPTS
PtsReg1=lm(PTS~X2PA+X3PA+FTA+ORB+DRB+AST+STL+BLK+TOV,NBA)
summary(PtsReg1)

PtsReg2=lm(PTS~X2PA+X3PA+FTA+ORB+AST+STL,NBA)
summary(PtsReg1)

OppPtsReg1=lm(oppPTS~X2PA+X3PA+FTA+ORB+DRB+AST+STL+BLK+TOV,NBA)
summary(OppPtsReg1)

OppPtsReg2=lm(oppPTS~X2PA+X3PA+FTA+ORB+DRB+STL+TOV,NBA)
summary(OppPtsReg2)

# Use model on test set
NBATest=read.csv("NBA_test.csv")
Pts=predict(PTSreg2,NBATest)
OppPts=predict(OppPtsReg2,NBATest)

# Calculate R2 error of Pts and OppPts Predicted
PtsSSE=sum((Pts-NBATest$PTS)^2)
PtsSST=sum((NBATest$PTS-mean(NBA$PTS))^2)
PtsR2=1-(PtsSSE/PtsSST)
PtsRMSE=sqrt(PtsSSE/nrow(NBATest))

OppPtsSSE=sum((OppPts-NBATest$oppPTS)^2)
OppPtsSST=sum((NBATest$oppPTS-mean(NBA$oppPTS))^2)
OppPtsR2=1-(OppPtsSSE/OppPtsSST)
OppPtsRMSE=sqrt(OppPtsSSE/nrow(NBATest))


# Now Using these Pts nand OppPts predicted we calculate the PTSDiff
PtsDiff=Pts-OppPts
playoffs=ifelse(PtsDiff>=42,1,0)
table(NBATest$Playoffs,playoffs)

# We see that our model produces an accuracy of 78.6%