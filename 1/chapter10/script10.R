#シミュレーション1,2,3を実行するためのスクリプト
#criterion="MFI",method="EAP", priorDist="unif", priorPar=c(-4,4)は，
#項目選択基準はMFI，推定法はEAP，事前分布は-4から4の一様分布を示している
#これらを変更すれば様々な設定でシミュレーションを行うことができる
#シードの問題があるため，本書と同じ結果にはならない。
library(catR)
#繰り返し数
nrep<-50
#項目プールのサイズ
pool<-300
#初期提示項目数
firstitem<-5
#提示項目数
nitems<-40
#被験者母数の真値
truevalue<-0
#シミュレーション1と3の場合は1，それ以外は0
simu13<-1

estresults<-matrix(0,nrow<-7,ncol<-1)
biasresults<-matrix(0,nrow<-7,ncol<-1)
mseresults<-matrix(0,nrow<-7,ncol<-1)
Items<-matrix(0,nrow<-nrep,ncol<-nitems)
apara_mat<-matrix(0,nrow<-nrep,ncol<-nitems)
bpara_mat<-matrix(0,nrow<-nrep,ncol<-nitems)
respattern<-matrix(0,nrow<-nrep,ncol<-nitems)
Finalest<-matrix(0,nrow<-nrep,ncol<-1)
Finalse<-matrix(0,nrow<-nrep,ncol<-1)

#データの発生
size<-nrep*pool
bparaT<-runif(size, -4,4)
bpara<-bparaT+rnorm(size,mean=0,sd=0.05)
aparaT<-runif(size, 0.5,1.5)*1.702
apara<-aparaT+rnorm(size,0,0.3)*1.702
for(i in 1:size)
{
if(apara[i]<0) {apara[i]<-0}
}
cpara<-matrix(0,nrow<-size,ncol<-1)
dpara<-matrix(1,nrow<-size,ncol<-1)
simudata_all<-cbind(apara,bpara,cpara,dpara)
simudataT_all<-cbind(aparaT,bparaT,cpara,dpara)

for(i in 1:nrep)
{
bank<-createItemBank(simudata_all[(1+(i-1)*(pool)):(i*pool),])
bankT<-createItemBank(simudataT_all[(1+(i-1)*(pool)):(i*pool),])
if(simu13==1) {bank<-bankT}

start <- list(nrItems=firstitem, theta=0, halfRange=2)
test <- list(method="EAP", priorDist="unif", priorPar=c(-4,4))
final <- list(method="EAP", priorDist="unif", priorPar=c(-4,4))
stop <- list(rule="length", thr=firstitem)
#上記の設定の下でCATを行う。
res <- randomCAT(trueTheta=truevalue, bank, start=start, test=test, stop=stop,
final=final)
Items[i,1:firstitem]<-res$testItems[1:firstitem]
apara_mat[i,1:firstitem]<-res$itemPar[1:firstitem,1]
bpara_mat[i,1:firstitem]<-res$itemPar[1:firstitem,2]
respattern[i,1:firstitem]<-res$pattern[1:firstitem]
Finalest[i,]<-res$thFinal
Finalse[i,]<-res$seFinal
}

#CATの実行
for(i in 1:nrep)
{
bank<-createItemBank(simudata_all[(1+(i-1)*(pool)):(i*pool),])
bankT<-createItemBank(simudataT_all[(1+(i-1)*(pool)):(i*pool),])
if(simu13==1) {bank<-bankT}

#1人につきnitems-firstitem回分
for(rep in 1:(nitems-firstitem))
{

outitems<-Items[i,1:(firstitem+rep-1)]
#項目選択基準，事前分布，推定法を変更することができる
selected<-nextItem(bank,Finalest[i,],out<-outitems,criterion="MFI",method="EAP", priorDist="unif", priorPar=c(-4,4),parInt=c(-4,4,21),x<-respattern[i,1:(firstitem+rep-1)])
expectres <- rbinom(1, 1, Pi(th=truevalue, as.data.frame(bankT$itemPar)[selected$item,1:4])$Pi)
respattern[i,(firstitem+rep)]<-expectres
Items[i,(firstitem+rep)]<-selected$item
apara_mat[i,(firstitem+rep)]<-selected$par[1]
bpara_mat[i,(firstitem+rep)]<-selected$par[2]

Finalest[i,]<-eapEst(as.data.frame(bank$itemPar)[Items[i,1:(firstitem+rep)],1:4],respattern[i,1:(firstitem+rep)], priorDist="unif", priorPar=c(-4,4));
Finalse[i,]<-eapSem(Finalest[i,],as.data.frame(bank$itemPar)[Items[i,1:(firstitem+rep)],1:4],respattern[i,1:(firstitem+rep)], priorDist="unif", priorPar=c(-4,4));

if(rep%%5==0) {estresults[rep/5]<-mean(Finalest);mseresults[rep/5]<-mean((Finalest-truevalue)^2);biasresults[rep/5]<-mean(Finalest)-truevalue;}
}
}

#推定値
estresults
#バイアス
biasresults
#mse
mseresults
#提示回ごとの識別力母数の平均
apply(apara_mat,2,mean)


#シミュレーション4を実行するためのスクリプト
#80通りのうちの1通り（300項目，真値1.0，候補20，バランス考慮）
#のシミュレーション
#シードの問題があるため，本書と同じ結果にはならない。

#繰り返し数
nrep<-1000
#項目プールのサイズ(300の場合のみ示した)
pool<-300
#判定基準
judge<-0
#候補の項目数
candidate<-20
#95%信頼区間の場合
jvalue<-1.96
#被験者母数の真値
truevalue<-1.0
#項目内容のバランス
cbList <- list(names=c("A","B","C"), props=c(1/3,1/3,1/3))
#バランスをとるか否か（とる場合は1）
balance<-0

library(catR)
firstitem<-5
nitems<-50+firstitem

#データ（項目パラメタ）の発生
aparaT<-runif(pool, 0.5,1.5)*1.702
bparaT<-runif(pool, -4,4)
cpara<-matrix(0,nrow<-pool,ncol<-1)
dpara<-matrix(1,nrow<-pool,ncol<-1)
content<-c(rep('A', pool/3),rep('B', pool/3),rep('C', pool/3))
simudata<-data.frame(aparaT,bparaT,cpara,dpara,content)
bank <- createItemBank(simudata, cb=TRUE);

#はじめにfirstitems項目提示する
Items<-matrix(0,nrow<-nrep,ncol<-nitems)
apara_mat<-matrix(0,nrow<-nrep,ncol<-nitems)
bpara_mat<-matrix(0,nrow<-nrep,ncol<-nitems)
respattern<-matrix(0,nrow<-nrep,ncol<-nitems)
Finalest<-matrix(0,nrow<-nrep,ncol<-1)
Finalse<-matrix(0,nrow<-nrep,ncol<-1)

for(i in 1:nrep)
{
#はじめにtheta=-1から±2の範囲の困難度を持つ5項目を提示する。
start <- list(nrItems=firstitem, theta=0, halfRange=2)
test <- list(method="EAP", priorDist="unif", priorPar=c(-4,4), itemSelect="MFI")
final <- list(method="EAP", priorDist="unif", priorPar=c(-4,4))
stop <- list(rule="length", thr=firstitem)
#上記の設定の下でCATを行う。
res <- randomCAT(truevalue, bank, start=start, test=test, stop=stop,final=final)
Items[i,1:firstitem]<-res$testItems[1:firstitem]
apara_mat[i,1:firstitem]<-res$itemPar[1:firstitem,1]
bpara_mat[i,1:firstitem]<-res$itemPar[1:firstitem,2]
respattern[i,1:firstitem]<-res$pattern[1:firstitem]
Finalest[i,]<-res$thFinal
Finalse[i,]<-res$seFinal
}

PItems<-matrix(0,nrow<-nrep,ncol<-nitems)
provideditems<-matrix(0,nrow<-nrep,ncol<-1)

for(i in 1:nrep)
{
for(rep in 1:50)
{
outitems<-Items[i,1:(firstitem+rep-1)]
if(balance==1) {selected<-nextItem(bank,Finalest[i,],out<-outitems,criterion="MFI",cbControl=cbList, randomesque=candidate)}
else {selected<-nextItem(bank,Finalest[i,],out<-outitems,criterion="MFI",randomesque=candidate)}

expectres <- rbinom(1, 1, Pi(th=truevalue, as.data.frame(bank$itemPar)[selected$item,1:4])$Pi)
respattern[i,(firstitem+rep)]<-expectres
Items[i,(firstitem+rep)]<-selected$item
apara_mat[i,(firstitem+rep)]<-selected$par[1]
bpara_mat[i,(firstitem+rep)]<-selected$par[2]

#EAP推定値とSE
Finalest[i,]<-eapEst(as.data.frame(bank$itemPar)[Items[i,1:(firstitem+rep)],1:4],respattern[i,1:(firstitem+rep)], priorDist="unif", priorPar=c(-4,4));
Finalse[i,]<-eapSem(Finalest[i,],as.data.frame(bank$itemPar)[Items[i,1:(firstitem+rep)],1:4],respattern[i,1:(firstitem+rep)], priorDist="unif", priorPar=c(-4,4));
if(Finalest[i,]-jvalue*Finalse[i,]>judge) {break;}
if(Finalest[i,]+jvalue*Finalse[i,]<judge) {break;}
if(rep==50) {break;}
}
provideditems[i]<-rep
PItems[i,]<-Items[i,]
}

#クラス分けの結果
class1<-0;class2<-0;class3<-0;class4<-0;
for(i in 1:nrep)
{
if(Finalest[i,]>judge && provideditems[i]<50) {class1<-class1+1}
if(Finalest[i,]<judge && provideditems[i]<50) {class2<-class2+1}
if(Finalest[i,]>judge && provideditems[i]==50) {class3<-class3+1}
if(Finalest[i,]<judge && provideditems[i]==50) {class4<-class4+1}
}

#項目内容の分布
contentclass<-matrix(0,nrow<-nrep,ncol<-3)
for(i in 1:nrep)
{
for(j in 1:50)
{
if(0<PItems[i,j] && PItems[i,j]<101) {contentclass[i,1]<-contentclass[i,1]+1}
if(100<PItems[i,j] && PItems[i,j]<201) {contentclass[i,2]<-contentclass[i,2]+1}
if(200<PItems[i,j] && PItems[i,j]<301) {contentclass[i,3]<-contentclass[i,3]+1}
}
}

#各項目の出現回数
provided<-matrix(0,nrow<-1,ncol<-301)
for(i in 1:nrep)
{
for(j in 1:50)
{
a<-PItems[i,j]
provided[1,(a+1)]<-provided[1,(a+1)]+1
}
}

#1000回の繰り返しにおける項目提示回数の分布
#5000回の繰り返しにおける分布については，
#truevalueを0.2から1まで0.2刻みで変化させて求めればよい。
p0<-0;p10<-0;p20<-0;p50<-0;p100<-0;p200<-0;p500<-0;
p999<-0;p1000<-0;
for(i in 2:301)
{
if((t(provided))[i]==0) {p0<-p0+1}
if((t(provided))[i]>0 && (t(provided))[i]<11) {p10<-p10+1}
if((t(provided))[i]>10 && (t(provided))[i]<21) {p20<-p20+1}
if((t(provided))[i]>20 && (t(provided))[i]<51) {p50<-p50+1}
if((t(provided))[i]>50 && (t(provided))[i]<101) {p100<-p100+1}
if((t(provided))[i]>100 && (t(provided))[i]<201) {p200<-p200+1}
if((t(provided))[i]>200 && (t(provided))[i]<500) {p500<-p500+1}
if((t(provided))[i]>500 && (t(provided))[i]<999) {p999<-p999+1}
if((t(provided))[i]==1000) {p1000<-p1000+1}
}

round(c(class1,class2,class3,class4,
summary((contentclass/apply(contentclass,1,sum))[,1]),
summary((contentclass/apply(contentclass,1,sum))[,2]),
summary((contentclass/apply(contentclass,1,sum))[,3]),
p0,p10,p20,p50,p100,p200,p500,p999,p1000-5,
(sum(apply(contentclass,1,sum))-(class3+class4)*50)/(1000-class3-class4)),7)


