## 第3章スクリプト
### 動作確認環境
## R 2.15.2
## package mirt 0.5.0

library(mirt)

##########################################
## 項目特性曲線を描く関数（尺度値D=1.0）
# mirt::coef()によって算出したIRT母数のオブジェクト
# number = 項目番号
# degree = 図の角度，省略時は45
# D = 尺度因子，省略時は1.702
# main = 図の名称。省略時は Item + 項目の順番 となる。

# 補償型モデル用IRS
irs<-function(coef,number,degree=45,D=1.702,gue=g,upp=u,main=paste("Item",number,sep="")){
	x <- seq(-4, 4, length= 100); y <- x
	par(oma = c(0, 0, 1, 0),mar=c(0,2,1,0))
	a1 <- coef[[number]][1,1]
	a2 <- coef[[number]][1,2]
	d  <- coef[[number]][1,3]
	g  <- coef[[number]][1,4]
	u  <- coef[[number]][1,5]
	f <- function(x,y) { p <- g+(u-g) * (1/(1+(exp(-D*((a1)*x + (a2)*y + d)))))}
	z <- outer(x, y, f);
	persp(x, y, z, theta = degree, phi = 30, expand = 0.6, zlim=c(0,1), 
	      main=main, xlab="θ1", ylab="θ2", zlab="p(u=1)",ticktype="detailed")
}

## 部分的補償モデル用IRS関数
# mirt::coef()したオブジェクトと項目番号を渡す
# degreeで回す
irspc<-function(coef,number,degree=45,D=1.702,main=paste("Item",number,sep="")){
	x <- seq(-4, 4, length= 100); y <- x
	par(oma = c(0, 0, 1, 0),mar=c(0,2,1,0))
	a1 <- coef[[number]][1,1]
	a2 <- coef[[number]][1,2]
	d1 <- coef[[number]][1,3]
	d2 <- coef[[number]][1,4]
	g  <- coef[[number]][1,5]
	u  <- coef[[number]][1,6]
	f <- function(x,y) { 
		p <- g+(u-g) * (1/(1+(exp(-D*(a1*x + d1)))) * 1/(1+(exp(-D*(a2*y + d2)))))
	}
	z <- outer(x, y, f);
	persp(x, y, z, theta = degree, phi = 30, expand = 0.6, zlim=c(0,1), #shade=0.1,
	      main=main, xlab="θ1", ylab="θ2", zlab="p(u=1)",ticktype="detailed")
}

# 探索的モデル用IRS
irsexp<-function(coef,number,degree=45,D=1.702,gue=g,upp=u){
	x <- seq(-4, 4, length= 100); y <- x
	par(oma = c(0, 1, 1, 0))
	a1 <- coef[[number]][1]
	a2 <- coef[[number]][2]
	d  <- coef[[number]][3]
	g  <- coef[[number]][4]
	u  <- coef[[number]][5]
	f <- function(x,y) { p <- g+(u-g) * (1/(1+(exp(-D*((a1)*x + (a2)*y + d)))))}
	z <- outer(x, y, f);
	persp(x, y, z, theta = degree, phi = 30, expand = 0.6, zlim=c(0,1), 
	      main=paste("Item",number,sep=""), xlab="θ1", ylab="θ2", zlab="p(u=1)",ticktype="detailed")
}



##########################################
## データの準備
##########################################
# 「NAEPデータ」の読み込み
naep <- read.csv("naep.csv",header=T)
head(naep)

#「IQテストデータ」の読み込み
iqtf <- read.csv("iqtf.csv",header=T)
head(iqtf)

##########################################
## 分析例
##########################################
## 2値データの補償的モデルによる分析
# confmirt.model()によるモデルの記述
# 次元名 = 項目の形式で記述する．項目はカンマで区切る．
# 項目が連続して配されている場合はハイフンで繋ぐことも可能．
# 例えば項目1から項目5までの場合，「1,2,3,4,5」とも「1-5」とも記述可能．

# confmirt()によって母数推定を実行する．
# 引数
# model = モデルを記述したオブジェクト名
# data = データの指定
# pars = 項目母数の確認や，制約を課す．
# D = 尺度因子．デフォルトでは1.702となる．

## 母数の制約
# 引数pars="values"とすることで，項目母数の一覧が示される．
# ここで，例えば項目1の2次元目の識別力を0に固定したい場合，項目母数の一覧のオブジェクトに関して，母数の番号"parnum"と，推定するか否かを示す"est"を確認し，
# mod2[2,5] <- 0; mod2[2,6] <- FALSE;
# のように指定する．これは値をゼロとし，推定を行わない，即ち固定するという指定を表している．
# 母数の制約指定を完了した後には，改めて関数confmirt()を実行する．この際引数"pars"には母数制約を課した状態の項目母数一覧オブジェクトを指定することに注意すること．

modelnaep <- confmirt.model()
f1 = 1-8
f2 = 1-8

mod2 <- confmirt(data=naep,model=modelnaep, pars="values", D=1.702)
fitnaep <- confmirt(data=naep, model=modelnaep, pars=mod2, D=1.702)
fitnaep #適合度指標（表3.6）

# 因子負荷量の出力
summary(fitnaep) #表3.3
# 関数coef()によってIRT母数を出力する．
coef(fitnaep) #表3.4

naepcoefmat <- t(as.data.frame(coef(fitnaep)[1:8]))
# 多次元識別力
MAnaep_comp <- sqrt(rowSums(cbind(naepcoefmat[seq(1,40,5),1],naepcoefmat[seq(2,40,5),1])^2))
# 項目3についての多次元識別力の検算
round(sqrt(0.446^2 + 0.474^2),3)

# 多次元困難度
MDnaep_comp <- -naepcoefmat[seq(3,40,by=5),1]/MAnaep_comp
# 項目3についての多次元困難度の検算
round(-0.347/.651,3)
#全項目の多次元識別力と多次元困難度（表3.4）
round(cbind(MAnaep_comp,MDnaep_comp),3)

# alphaの算出 #
library(aspace)
MDISC<-c(0.651,0.521,0.552,1.016,0.980,0.788,0.729,0.536)
a1<-c(0.446 ,0.189 ,0.511 ,0.750 ,0.758 ,0.451 ,0.665 ,0.476)
a2<-c(0.474, 0.486, 0.208, 0.686, 0.621, 0.646, 0.298, 0.246)
cbind(round(acos_d(a1/MDISC)),
round(acos_d(a2/MDISC)))

# 補償型モデル IRSの描画(項目番号と順番の違いに注意)
naepcoef<-coef(fitnaep)
irs(naepcoef,1, degree=315, main="Item3") #Item3
irs(naepcoef,2, degree=315, main="Item5") #Item5
irs(naepcoef,3, degree=315, main="Item7") #Item7
irs(naepcoef,4, degree=315, main="Item8") #Item8
irs(naepcoef,5, degree=315, main="Item9") #Item9
irs(naepcoef,6, degree=315, main="Item10") #Item10
irs(naepcoef,7, degree=315, main="Item11") #Item11
irs(naepcoef,8, degree=315, main="Item12") #Item12

# IRSは関数 itemplot()を用いて描画することも可能
# 項目1のIRSを描画する
itemplot(fitnaep,1)


##########################################
## 部分的補償型モデル（非補償型モデル）

# 部分補償型モデルを実行するには，関数 confmirt()の引数 itemtypeに，"PC2PL"を指定する

partdat <- naep
partnaep <- confmirt.model()
f1 = 1-8
f2 = 1-8

partmod <- confmirt(data=partdat,model=partnaep,pars="values",itemtype="PC2PL",D=1.702)
fitpart <- confmirt(data=partdat,model=partnaep,itemtype="PC2PL",pars=partmod,D=1.702)
fitpart #適合度指標（表3.6）
summary(fitpart)#表3.5因子負荷量
t(as.data.frame(coef(fitpart))) #表3.5項目母数

# 部分補償型モデル IRSの描画
fitpartcoef<-coef(fitpart)
irspc(fitpartcoef,1, degree=315, main="Item3") #Item3
irspc(fitpartcoef,2, degree=315, main="Item5") #Item5
irspc(fitpartcoef,3, degree=315, main="Item7") #Item7
irspc(fitpartcoef,4, degree=315, main="Item8") #Item8
irspc(fitpartcoef,5, degree=315, main="Item9") #Item9
irspc(fitpartcoef,6, degree=315, main="Item10") #Item10
irspc(fitpartcoef,7, degree=315, main="Item11") #Item11
irspc(fitpartcoef,8, degree=315, main="Item12") #Item12


##########################################
## 探索的多次元項目反応モデル：「IQテストデータ」の分析
# 引数"rotate"で因子軸の回転方法を指定可能である．デフォルトでは"varimax"となる．
# この他に"promax","biquartimin","geominT","geominQ"等が指定可能である．
# より詳しい回転方法の名称指定方法はRパッケージ"GPArotation"のマニュアルを参照せよ（http://cran.r-project.org/web/packages/GPArotation/index.html）．

# 探索的モデルの場合，引数"model"には次元数を指定する．

iqtfmirt2 <- mirt(data=iqtf, model=2, rotate="varimax",itemtype="2PL",D=1.702)
summary(iqtfmirt2) #表3.10
t(as.data.frame(coef(iqtfmirt2)[1:16])) #項目母数（表3.9）

mdis<-t(as.data.frame(coef(iqtfmirt2)[1:16])) 
#MDISCとMDIFF算出（表3.9）
round(cbind(sqrt(mdis[,1]^2 + mdis[,2]^2),-(mdis[,3])/(sqrt(mdis[,1]^2 + mdis[,2]^2))),3)

# IRSを描く
iqtfmirt2coef<-coef(iqtfmirt2)
#例えば第1項目の場合
irsexp(iqtfmirt2coef,1,D=1.702)

##########################################
## FIFA（完全情報項目因子分析）を行う。（「IQテストデータ」を使用）
iq4mod <- confmirt.model()
f1 = 1,2,3,4
f2 = 5,6,7,8
f3 = 9,10,11,12
f4 = 13,14,15,16

confmirtFitIQ1 <- confmirt(data=iqtf, model=iq4mod, D=1.702)
(confmirtFitIQ1) #適合度指標
summary(confmirtFitIQ1) #表3.11


# 因子スコアの推定
fscoreIQ1 <- mirt::fscores(confmirtFitIQ1, full.scores=T)#EAP推定値
nrow(fscoreIQ1)
length(which((duplicated(fscoreIQ1)==T)))
table(rowSums(iqtf))
table(round(fscoreIQ1[,17:20],3))
# 20名分の得点と因子スコアを取得
tscore <- rowSums(fscoreIQ1[1:20, 1:16])
cbind(fscoreIQ1[1:20, 1:16], tscore,round(fscoreIQ1[1:20, 17:20],3),
      rowMeans(round(fscoreIQ1[1:20, 17:20],3))) #表3.12

##########################################
## 一般因子を仮定したモデル（双因子モデル）
iq4mod_bifac <- confmirt.model()
f1 = 1,2,3,4
f2 = 5,6,7,8
f3 = 9,10,11,12
f4 = 13,14,15,16
G = 1-16

iq4mod_bifac.2 <- confmirt(data=iqtf, model=iq4mod_bifac, pars="values",D=1.702)
head(iq4mod_bifac.2)
confmirtFitIQ2 <- confmirt(data=iqtf, model=iq4mod_bifac, D=1.702)
confmirtFitIQ2 #適合度指標
summary(confmirtFitIQ2) #表3.13

