#build the predicted line
yr.pred<-seq(2010,2018,length=100)
tran_length.pred<-seq(0,100,length=100)
site.pred<-factor(rep(c("O"),each=100))
#site.pred<-factor(rep(c("N"),each=100))
#site.pred<-factor(rep(c("I"),each=100))
treatment.pred<-factor(rep(c("rocks"),each=100))
#treatment.pred<-factor(rep(c("no rocks"),each=100))

nwdata=data.frame(yr.pred,tran_length.pred,site.pred,treatment.pred)


pred_LC=predict(mod1, data=nwdata,list(site=site.pred, treatment=treatment.pred,
                                       tran_length=tran_length.pred),
                type="response",
                se.fit=TRUE)

