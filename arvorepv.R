pc2<-quantile(treino$V2,seq(from=0,to=1,by=1/8))
pc2[1]<-pc2[1]-0.0000000001
treino$V2<-cut(treino$V2,pc2,labels=FALSE)
sum(table(treino$V2))

pc3<-quantile(treino$V3,seq(from=0,to=1,by=1/8))
pc3[1]<-pc3[1]-0.1
pc3[4]<-pc3[4]-0.1
pc3[2]<-pc3[2]-0.1
treino$V3<-cut(treino$V3,pc3,labels=FALSE)
sum(table(treino$V3))
treino$V3

pc7<-quantile(treino$V7,seq(from=0,to=1,by=1/8),names=FALSE)
pc7[1]<-pc7[1]-0.00001
pc7[2]<-pc7[2]-0.00001
pc7[3]<-pc7[3]-0.00001
pc7[5]<-pc7[5]-0.00001
treino$V7<-cut(treino$V7,pc7,labels=FALSE)
sum(table(treino$V7))
treino$V7

#--------------------------------------------------------------------------

treino$V5[treino$V5<=7]<-1
treino$V5[treino$V5>7]<-2
treino$V5
table(treino$V5)

treino$V10[treino$V10>=1]<-2
treino$V10[treino$V10==0]<-1
treino$V10
table(treino$V10)

treino$V13[treino$V13<=160]<-1
treino$V13[treino$V13>160]<-2
treino$V13
table(treino$V13)

treino$V14[treino$V14<=3]<-1
treino$V14[treino$V14>3]<-2
treino$V14
table(treino$V14)