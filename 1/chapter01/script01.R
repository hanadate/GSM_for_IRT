###第1章
###ラッシュモデルと多値ラッシュモデル

#パッケージ読み込み
library(eRm)

###1.1 ラッシュモデル

############################
###  ラッシュモデル実行  ###
############################

###小規模データの分析
#データセット読み込み
#「空間回転データ」
data1 <- read.csv("rmexam.csv")

###項目母数の推定―関数RM()―
### 関数の主な引数
## RM(X, se, sum0, ...)
## X:0,1のデータ行列
## se:標準誤差を計算するかどうか(既定値はTRUE)
## sum0:困難度母数に関する制約。TRUEならば母数の和が0となる制約を，FALSEならば1番目の項目が0となる制約(既定値はTRUE)。
#分析(分析結果は容易度を出力するため，符号を逆にすることに注意)
rmres1 <- RM(X=data1, se=TRUE, sum0=TRUE)
summary(rmres1)

#表1.3
#項目母数の推定値
estimate1 <- data.frame(beta = -rmres1$beta,beta.se=rmres1$se.beta)
round(estimate1,3)

###被験者母数の推定―関数person.parameter()―
### 関数の主な引数
## person.parameter(object, ...)
## object:クラスeRmのオブジェクト
#分析
#被験者母数の推定
personpar1 <- person.parameter(object=rmres1)

#表1.4
#被験者母数の推定値
personpar1

###項目反応関数の描画―関数plotjointICC()―
### 関数の主な引数
## plotjointICC(object, item.subset, legend, ...)
## object:クラスeRmのオブジェクト
## item.subset:プロットする項目を指定する。既定値は"all"で，すべての項目のIRFを描く。
## legend:凡例を掲載するかいなか。
#図1.3
#「空間回転データ」のIRF
plotjointICC(rmres1,legend=FALSE,col="black",xlab="latent trait θ",ylab="probability p(θ)",
cex=1,lwd=1,cex.axis=1,las=1)


###中規模データの分析
#データセット読み込み
#「学力テスト1」(入門編で利用したデータ)
test1 <- read.csv("test1.csv")

###項目母数の推定
rmres2 <- RM(X=test1, se=TRUE, sum0=TRUE)
summary(rmres2)

###被験者母数の推定
personpar2 <- person.parameter(object=rmres2)
personpar2

###項目特性曲線の描画
plotjointICC(rmres2,legend=FALSE,col="black",xlab="latent trait θ",ylab="probability p(θ)",
cex=1,lwd=1,cex.axis=1,las=1)

###AndersenのLR検定―関数LRtest()―
### 関数の主な引数
## LRtest(object, splitcr, ...)
## object:クラスeRmのオブジェクト
## splitcr:被験者を下位集団に分割する際の基準を指定する。"median"の場合は中央値で，"mean"の場合は平均値で分割する。
##         自分でベクトルを指定することも可能
#LR検定の実行
lrres <- LRtest(object=rmres2, splitcr="median")
lrres

###Wald検定―関数Waldtest()―
### 関数の主な引数
## Waldtest(object, splitcr, ...)
## object:クラスeRmのオブジェクト
## splitcr:被験者を下位集団に分割する際の基準を指定する。"median"の場合は中央値で，"mean"の場合は平均値で分割する。
##         自分でベクトルを指定することも可能
#Wald検定の実行
wdres <- Waldtest(object=rmres2,splitcr="median")
#表1.5
#Wald検定
wdres

###Martin-Lof検定―関数MLoef()―
### 関数の主な引数
## MLoef(robj, splitcr, ...)
## robj:クラスeRmのオブジェクト
## splitcr:項目を下位集団に分割する際の基準を指定する。"median"の場合は中央値で，"mean"の場合は平均値で分割する。
##         自分でベクトルを指定することも可能
#Martin-Loef検定
mlres <- MLoef(robj=rmres2,splitcr="median")
mlres
summary(mlres)

###項目適合度指標―関数itemfit()―
###被験者適合度指標―関数personfit()―
### 関数の主な引数
## itemfit(object, ...)
## personfit(object, ...)
## object:クラスeRmのオブジェクト

#表1.7
#項目適合度指標
itemfit(object=personpar2)

#表1.8
#被験者適合度指標の分布
pfit <- personfit(object=personpar2)
outfit_f <- cut(pfit$p.outfitMSQ,breaks=c(-Inf,0.7,1.3,Inf))
infit_f <- cut(pfit$p.infitMSQ,breaks=c(-Inf,0.7,1.3,Inf))
table(outfit_f);table(infit_f)

###項目削除―関数stepwiseIt()―
### 関数の主な引数
## stepwiseIt(object, criterion, alpha=0.05, ...)
## object:クラスeRmのオブジェクト
## criterion:項目削除の基準を指定する。"itemfit","Waldtest","LRtest"のいずれかをリストとして指定する。
##           "Waldtest","LRtest"に関しては，2番目の要素として検定に利用する分割基準を指定する。
## alpha:有意水準
#項目削除の実行
Step <- stepwiseIt(object=rmres2,criterion=list("Waldtest","median"),alpha=0.05)

###Person-Item Map―関数plotPImap()―
### 関数の主な引数
## plotPImap(object, item.subset, sorted, ...)
## object:クラスeRmのオブジェクト
## item.subset:プロットする項目を指定する。既定値は"all"で，すべての項目を示す。
## sorted:項目母数の値に従って昇順に示すかを指定する（既定値はFALSE）。
#Person-Item Mapの描画
#図1.4
plotPImap(object=rmres2,item.subset="all",sorted=TRUE)

###Pathway Map―関数plotPWmap()―
### 関数の主な引数
## plotPWmap(object, imap, item.subject, pmap,person.subset, pp, ...)
## object:クラスeRmのオブジェクト
## imap:項目の適合度指標をプロットするかを指定する（既定値はTRUE）。
## item.subset:プロットする項目を指定する。既定値は"all"で，すべての項目を示す。
## pmap:被験者の適合度指標をプロットするかを指定する（既定値はFALSE）。
## person.subset:プロットする被験者を指定する。既定値は"all"で，すべての被験者を示す。
## pp:被験者母数の推定結果を指定する。
#Pathway Mapの描画
#項目に関するPathway Map
#図1.5
plotPWmap(object=rmres2,pp=personpar2,imap=TRUE,pmap=FALSE)
#被験者に関するPathway Map
plotPWmap(object=rmres2,pp=personpar2,person.subset=c(1:nrow(test1)),imap=FALSE,pmap=TRUE)

###項目情報曲線・テスト情報曲線―関数plotINFO()―
### 関数の主な引数
## plotINFO(ermobject, type, theta, ...)
## ermobject:クラスeRmのオブジェクト
## type:項目情報曲線を描く場合には"item"，テスト情報曲線を描く場合には"test"，両方書く場合には"both"を指定する。
## theta:尺度値の範囲を指定する。

#項目情報曲線の描画
plotINFO(ermobject=rmres2,type="both")
#テスト情報曲線の描画
#図1.6
#「学力テスト1」のテスト情報曲線
plotINFO(ermobject=rmres2,type="test")

#中規模データの分析（等化）
#データセット読み込み
#「学力テスト2」(入門編で利用したデータ)
test2 <- read.csv("test2.csv")

###項目母数の推定
rmres3 <- RM(X=test2, se=TRUE, sum0=TRUE)
summary(rmres3)
###被験者母数の推定
personpar3 <- person.parameter(object=rmres3)
personpar3

#項目選択
test1.com.item<-c( 5,11,15,28,29)
test2.com.item<-c( 3, 8,10,20,21)
#「学力テスト1」と「学力テスト2」の推定結果
test1.estimate <- data.frame(beta = -rmres2$beta,beta.se=rmres2$se.beta)
test2.estimate <- data.frame(beta = -rmres3$beta,beta.se=rmres3$se.beta)

###項目の等化
#共通項目の選択
com.item1 <- c(4,6,8,14,15)
com.item2 <- c(2,4,6,9,10)
equt1 <- test1.estimate[com.item1,1]
equt2 <- test2.estimate[com.item2,1]

#等化係数の計算
beta12 <- mean(equt1) - mean(equt2)

#表1.9
#テスト間の共通項目の推定値
c.test <- data.frame(c.test1=c(equt1,mean(equt1)),c.test2=c(equt2,mean(equt2)))
rownames(c.test) <- c("V11","V18","V22","V44","V46","平均")
colnames(c.test) <- c("学力テスト1","学力テスト2")
round(t(c.test),3)


###1.2 多値ラッシュモデル

########################
###  評定尺度モデル  ###
########################
#評定尺度モデルによる「態度データ」の分析
#データ読み込み
Sciencedat <- read.csv("sciencedat.csv")
#表1.10
#「態度データ」の一部
head(Sciencedat)

###項目母数の推定―関数RSM()―
### 関数の主な引数
## RSM(X, se, sum0, ...)
## X:項目反応データ。カテゴリは0から始まる。欠測値はNAとする。
## se:標準誤差を計算するかどうか(既定値はTRUE)
## sum0:困難度母数に関する制約。TRUEならば母数の和が0となる制約を，FALSEならば1番目の項目が0となる制約(既定値はTRUE)。
#分析(分析結果は容易度を出力するため，符号を逆にすることに注意)
rsmres <- RSM(X=Sciencedat,se=TRUE,sum0=TRUE)
summary(rsmres)

#推定結果のまとめ
#項目母数b_j
rsm.beta_j <- c(-sum(rsmres$etapar[1:3]),rsmres$etapar[1:3])
names(rsm.beta_j) <- colnames(Sciencedat)
#カテゴリ係数omega_c
omega_c <- -rsmres$etapar[4:5]

#表1.12
#項目母数と閾値母数
estimate2 <- c(rsm.beta_j,omega_c)
round(estimate2,3)

#被験者母数の推定
perparRSM <- person.parameter(object=rsmres)
perparRSM

#閾値の計算
thresholds(rsmres)

###項目特性曲線の描画―関数plotICC()―
### 関数の主な引数
## plotICC(object, item.subset, legpos=FALSE, ...)
## object:クラスeRmのオブジェクト
## item.subset:プロットする項目を指定する。
## legend:凡例を掲載するかいなか。
#項目特性曲線の描画
#図1.9，図1.10
plotICC(object=rsmres,item.subset=c(1),lty=1,col="black",legpos=FALSE,las=1,
xlab="latent trait θ",ylab="probability f(θ)",cex=1,lwd=1,cex.axis=1)
plotICC(object=rsmres,item.subset=c(2),lty=1,col="black",legpos=FALSE,las=1,
xlab="latent trait θ",ylab="probability f(θ)",cex=1,lwd=1,cex.axis=1)
plotICC(object=rsmres,item.subset=c(3),lty=1,col="black",legpos=FALSE,las=1,
xlab="latent trait θ",ylab="probability f(θ)",cex=1,lwd=1,cex.axis=1)
plotICC(object=rsmres,item.subset=c(4),lty=1,col="black",legpos=FALSE,las=1,
xlab="latent trait θ",ylab="probability f(θ)",cex=1,lwd=1,cex.axis=1)

#Person-Item Mapの描画
plotPImap(object=rsmres,sorted=TRUE)

#Pathway Mapの描画
plotPWmap(object=rsmres,pp=perparRSM)


########################
###  部分採点モデル  ###
########################

###項目母数の推定―関数PCM()―
### 関数の主な引数
## PCM(X, se, sum0, ...)
## X:項目反応データ。カテゴリは0から始まる。欠測値はNAとする。
## se:標準誤差を計算するかどうか(既定値はTRUE)
## sum0:困難度母数に関する制約。TRUEならば母数の和が0となる制約を，FALSEならば1番目の項目が0となる制約(既定値はTRUE)。
#分析(分析結果は容易度を出力するため，符号を逆にすることに注意)
pcmres <- PCM(X=Sciencedat,se=TRUE,sum0=FALSE)
summary(pcmres)

#被験者母数の推定
perparPCM <- person.parameter(object=pcmres)
perparPCM

#閾値の計算
#閾値はステップ母数に対応
#Locationは項目母数b_jに対応
Thresh <- thresholds(pcmres)
ThreshM <- Thresh$threshtable$'1'
round(ThreshM,3)

#閾値母数の計算
d_jc <- cbind((ThreshM[,2:3] - ThreshM[,1]),(ThreshM[,4] - ThreshM[,1]))

#表1.13
#項目母数と閾値母数とステップ母数
estimate3 <- cbind(ThreshM[,1],d_jc,ThreshM[,2:4])
colnames(estimate3) <- c("b_j","d_j1","d_j2","d_j3","b_j1","b_j2","b_j3")
round(estimate3,3)


#項目特性曲線の描画
#図1.11，図1.12
plotICC(object=pcmres,item.subset=c(1),lty=1,col="black",legpos=FALSE,
xlab="latent trait θ",ylab="probability f(θ)",cex=1,lwd=1,cex.axis=1)
plotICC(object=pcmres,item.subset=c(2),lty=1,col="black",legpos=FALSE,
xlab="latent trait θ",ylab="probability f(θ)",cex=1,lwd=1,cex.axis=1)
plotICC(object=pcmres,item.subset=c(3),lty=1,col="black",legpos=FALSE,
xlab="latent trait θ",ylab="probability f(θ)",cex=1,lwd=1,cex.axis=1)
plotICC(object=pcmres,item.subset=c(4),lty=1,col="black",legpos=FALSE,
xlab="latent trait θ",ylab="probability f(θ)",cex=1,lwd=1,cex.axis=1)

#Person-Item Mapの描画
plotPImap(object=pcmres,pp=perparPCM,sorted=T)

#Pathway Mapの描画
plotPWmap(object=pcmres,pp=perparPCM)

