################
### 分析の準備 ###
################
#パッケージ読み込み
library(difR)
#データセット読み込み
Anxiety<-read.csv("Anxiety01data.csv",header=TRUE)
#グループ変数の抜き出し
age<-Anxiety$age#年齢
gender<-Anxiety$gender#性別
education<-Anxiety$education#学歴
#年齢×学歴のグループ変数作成
ageeducation<-numeric(nrow(Anxiety))
ageeducation[(age==1)&(education==1)]<-3;ageeducation[(age==1)&(education==0)]<-2
ageeducation[(age==0)&(education==1)]<-1;ageeducation[(age==0)&(education==0)]<-0
#ジェンダー×学歴のグループ変数作成
gendereducation<-numeric(nrow(Anxiety))
gendereducation[(gender==1)&(education==1)]<-3;gendereducation[(gender==1)&(education==0)]<-2
gendereducation[(gender==0)&(education==1)]<-1;gendereducation[(gender==0)&(education==0)]<-0
#反応行列のみを取り出す
unness<-is.element(names(Anxiety),c("age","gender", "education"))
Anxiety<-Anxiety[,!unness]#反応行列

########################
### 6.1 MH法およびGMH法 ###
########################
###### MH法（関数difMH）
#### 関数の主な引数
## Data: データ行列のみ，もしくはデータ行列にグループメンバーシップのベクトルを加えたもの
## group: グループメンバーシップのベクトル，もしくはグループメンバーシップを表す（データ内の）列の指示子
## focal.name: 焦点グループに対応するグループの水準を示すもの
## purify: 当該手法を繰り返し用いて係留項目（anchor items）を純化(purify)するべきかどうか（既定値はFALSE）
#### 分析
R_MH<-difMH(Data=Anxiety, group=gender, focal.name=1, purify=FALSE)
#図6.1
{plot(R_MH$MH, cex=0.7, ylab="MH統計量",xlab="項目番号",pch="", cex.lab=1.2)
text(x=1:29,y=R_MH$MH,labels=as.character(1:29))
segments(-0.5,3.8414, 30.5, 3.8414)}
#plot(R_MH)#これでもよい

###### GMH法（関数difGMH ）
#### 関数の主な引数 
## Data: データ行列のみ，もしくはデータ行列にグループメンバーシップのベクトルを加えたもの
## group: グループメンバーシップのベクトル，もしくはグループメンバーシップを表す（データ内の）列の指示子
## focal.names: 焦点グループに対応するグループの水準を示すもの
## purify: 当該手法を繰り返し用いて係留項目（anchor items）を純化(purify)するべきかどうか（既定値はFALSE）
#### 分析
R_GMH<-difGMH(Data=Anxiety, group=gendereducation, focal.names=c(1,2,3), purify=FALSE)
#図6.2
qchisq(p=0.95, df=3, ncp = 0, lower.tail = TRUE, log.p = FALSE)
{plot(R_GMH$GMH, cex=0.7, ylab="GMH統計量",xlab="項目番号",pch="", cex.lab=1.2)
text(x=1:29,y=R_GMH$GMH,labels=as.character(1:29))
segments(-0.5,7.8147, 30.5, 7.8147)}
#plot(R_GMH)#これでもよい

########################################
### 6.2 ロジスティック法および一般化ロジスティック法 ###
########################################
###### 均一DIF（図6.3）
beta<-(-2);beta_0<-(0.15);beta_1<-(-1);beta_2<-0
S<-seq(0,50,by=0.01)
G0<-rep(0,length(S));G1<-rep(1,length(S))
z0<-beta+beta_0*S+beta_1*G0+beta_2*S*G0
p0<-exp(z0)/(1+exp(z0))
plot(S,p0,type="l",xlim=c(0,50),ylim=c(0,1),xlab="合計得点",
	ylab="正答確率",cex.lab=1.2,cex.axis=1.2)
z1<-beta+beta_0*S+beta_1*G1+beta_2*S*G1;p1<-exp(z1)/(1+exp(z1))
par(new=TRUE)
plot(S,p1,type="l",xlim=c(0,50),ylim=c(0,1),lty="dashed",
	xlab="", ylab="",axes=FALSE)
legend("topleft",lty=c(1,2),legend=c("G=0","G=1"),box.lty=0,cex=1.4);par(new=FALSE)

###### 不均一DIF（図6.4）
beta<-(-2);beta_0<-(0.15);beta_1<-(0);beta_2<-(0.1)
S<-seq(0,50,by=0.01)
G0<-rep(0,length(S));G1<-rep(1,length(S))
z0<-beta+beta_0*S+beta_1*G0+beta_2*S*G0
p0<-exp(z0)/(1+exp(z0))
plot(S,p0,type="l",xlim=c(0,50),ylim=c(0,1),xlab="合計得点",
	ylab="正答確率",cex.lab=1.2,cex.axis=1.2)
z1<-beta+beta_0*S+beta_1*G1+beta_2*S*G1;p1<-exp(z1)/(1+exp(z1))
par(new=TRUE)
plot(S,p1,type="l",xlim=c(0,50),ylim=c(0,1),lty="dashed",
	xlab="", ylab="",axes=FALSE)
legend("topleft",lty=c(1,2),legend=c("G=0","G=1"),box.lty=0,cex=1.4);par(new=FALSE)

###### DIF（図6.5）
beta<-(-2);beta_0<-(0.15);beta_1<-(-1);beta_2<-(0.1)
S<-seq(0,50,by=0.01)
G0<-rep(0,length(S));G1<-rep(1,length(S))
z0<-beta+beta_0*S+beta_1*G0+beta_2*S*G0
p0<-exp(z0)/(1+exp(z0))
plot(S,p0,type="l",xlim=c(0,50),ylim=c(0,1),xlab="合計得点",
	ylab="正答確率",cex.lab=1.2,cex.axis=1.2)
z1<-beta+beta_0*S+beta_1*G1+beta_2*S*G1;p1<-exp(z1)/(1+exp(z1))
par(new=TRUE)
plot(S,p1,type="l",xlim=c(0,50),ylim=c(0,1),lty="dashed",
	xlab="", ylab="",axes=FALSE)
legend("topleft",lty=c(1,2),legend=c("G=0","G=1"),box.lty=0,cex=1.4);par(new=FALSE)

###### ロジスティック法（関数difLogistic）
#### 関数の主な引数
## Data: データ行列のみか，データ行列に下位集団を表すベクトルを加えたもの
## group: 下位集団を表すベクトル，もしくは下位集団を表す（データ内の）列の指示子
## focal.name: 焦点グループに対応するグループの水準を示す
## type: どのDIF効果を検定すべきかを特定する。可能な値は"both"（デフォルト），"udif"，"nudif"
## criterion: どのDIF統計量を計算するかを特定する。可能な値は"LRT"（デフォルト）もしくは"Wald"
## purify: 当該手法を繰り返し用いてアンカー項目を純化すべきかどうか（デフォルトは"FALSE"）
#### 分析
R_Logi<-difLogistic(Data=Anxiety, group=age, focal.name=1, purify=FALSE, type="both",criterion="LRT")
#図6.6
{plot(R_Logi$Logistik, cex=0.7, ylab="LRT統計量",xlab="項目番号",pch="", cex.lab=1.2)
text(x=1:29,y=R_Logi$Logistik,labels=as.character(1:29))
segments(-0.5,5.991465, 30.5, 5.991465)}
#plot(R_Logi)#これでもよい


#図6.7
{beta<-R_Logi$logitPar[1,1];beta_0<-R_Logi$logitPar[1,2]
beta_1<-R_Logi$logitPar[1,3];beta_2<-R_Logi$logitPar[1,4]
S<-seq(0,30,by=0.01)
G0<-rep(0,length(S));G1<-rep(1,length(S))
z0<-beta+beta_0*S+beta_1*G0+beta_2*S*G0
p0<-exp(z0)/(1+exp(z0))
plot(S,p0,type="l",xlim=c(0,30),ylim=c(0,1),xlab="合計得点",
	ylab="正答確率",cex.lab=1.2,cex.axis=1.2)
z1<-beta+beta_0*S+beta_1*G1+beta_2*S*G1;p1<-exp(z1)/(1+exp(z1))
par(new=TRUE)
plot(S,p1,type="l",xlim=c(0,30),ylim=c(0,1),lty="dashed",
	xlab="", ylab="",axes=FALSE)
legend("topleft",lty=c(1,2),legend=c("Low","High"),box.lty=0,cex=1.4);par(new=FALSE)}
#plot(R_Logi, plot="itemCurve",item=1, itemFit="best", group.names=c("Low","High"))#これでもよい

#表6.5内の項目R24
S<-apply(Anxiety,1,sum)#合計得点
Item<-24;G<-(age)
result0<-glm(Anxiety[,Item]~S+G+S*G,family=binomial)#モデル0
result2<-glm(Anxiety[,Item]~S,family=binomial)#モデル2
(LL0<-(-1*logLik(result0)[1]))#最小対数尤度を最大対数尤度へ変換
(LL2<-(-1*logLik(result2)[1]))#最小対数尤度を最大対数尤度へ変換
(Lambda02<-(-2*(LL0-LL2)))### LRT統計量

###### 一般化ロジスティック法（関数difGenLogistic）
#### 関数の主な引数
## Data: データ行列のみ，もしくはデータ行列にグループメンバーシップのベクトルを加えたもの
## group: グループメンバーシップのベクトル，もしくはグループメンバーシップを表す（データ内の）列の指示子
## focal.names: 焦点グループに対応するグループの水準を示すもの
## type: どのDIF効果を検定すべきかを特定する。可能な値は"both"（デフォルト），"udif"，"nudif"
## criterion: どのDIF統計量を計算するかを特定する。可能な値は"LRT"（デフォルト）もしくは"Wald"
## purify: 当該手法を繰り返し用いて係留項目（anchor items）を純化(purify)するべきかどうか（既定値はFALSE）
#### 分析
#グループのラベル付け
ageeducation[ageeducation==0]<-"LowUniv";ageeducation[ageeducation==1]<-"LowHS"
ageeducation[ageeducation==2]<-"HighUniv";ageeducation[ageeducation==3]<-"HighHS"
R_Glogi<-difGenLogistic(Data=Anxiety,group=ageeducation,
	focal.names=c("LowHS","HighUniv","HighHS"), purify=FALSE,criterion="Wald", type="udif")#
#図6.8
{plot(R_Glogi$genLogistik, cex=0.7, ylab="Wald統計量",xlab="項目番号",pch="", cex.lab=1.2)
text(x=1:29,y=R_Glogi$genLogistik,labels=as.character(1:29))
segments(-0.5,7.814728, 30.5, 7.814728)}
#plot(R_Glogi)#これでもよい
#図6.9
{beta<-R_Glogi$logitPar[6,1]#切片
beta_0<-R_Glogi$logitPar[6,2]#合計得点の係数
S<-seq(0,30,by=0.01)
beta_1<-0#グループ0の切片
z0<-beta+beta_0*S+beta_1
p0<-exp(z0)/(1+exp(z0))
plot(S,p0,type="l",xlim=c(0,30),ylim=c(0,1),xlab="合計得点",
	ylab="正答確率",cex.lab=1.2,cex.axis=1.2)
beta_1<-R_Glogi$logitPar[6,3]#グループ1の切片
z1<-beta+beta_0*S+beta_1;p1<-exp(z1)/(1+exp(z1))
par(new=TRUE)
plot(S,p1,type="l",xlim=c(0,30),ylim=c(0,1),lty="dashed",
	xlab="", ylab="",axes=FALSE)
beta_1<-R_Glogi$logitPar[6,4]#グループ1の切片
z1<-beta+beta_0*S+beta_1;p1<-exp(z1)/(1+exp(z1))
par(new=TRUE)
plot(S,p1,type="l",xlim=c(0,30),ylim=c(0,1),lty="dotted",
	xlab="", ylab="",axes=FALSE)
beta_1<-R_Glogi$logitPar[6,5]#グループ1の切片
z1<-beta+beta_0*S+beta_1;p1<-exp(z1)/(1+exp(z1))
par(new=TRUE)
plot(S,p1,type="l",xlim=c(0,30),ylim=c(0,1),lty="dotdash",
	xlab="", ylab="",axes=FALSE)
legend("topleft",lty=c("solid","dashed","dotted","dotdash"),
	legend=c("LowUniv","LowHS","HighUniv","HighHS"),box.lty=0,cex=1.4);par(new=FALSE)}
#plot(R_Glogi, plot="itemCurve",item=6, ref.name=c("U65Univ"))#これでもよい

#表6.6内の項目R26
S<-apply(Anxiety,1,sum)#合計得点
Item<-26;G<-factor(ageeducation)
result<-glm(Anxiety[,Item]~S+G,family=binomial)
summary(result)
P<-matrix(c(
  1,0,0,0,0,
  0,0,1,0,0,
  0,0,0,1,0,
  0,0,0,0,1,
  0,1,0,0,0
),5,5,byrow=TRUE)#推定量の順番を本文に合わせるために使う行列
tauhat<-P%*%coefficients(result)#推定値
(Cov<-P%*%vcov(result)%*%t(P))#推定量の共分散行列
C<-cbind(rep(0,3),diag(3),rep(0,3))#定数行列C
(Q<-t(C%*%tauhat)%*%solve(C%*%Cov%*%t(C))%*%(C%*%tauhat))#Wald統計量

##################
### 6.3 項目純化 ###
##################
R_MH_nonP<-difMH(Data=Anxiety, group=age, focal.name=1, purify=FALSE)
#図6.10
{plot(R_MH_nonP$MH, cex=0.7, ylab="MH統計量",xlab="項目番号",pch="", cex.lab=1.2)
text(x=1:29,y=R_MH_nonP$MH,labels=as.character(1:29))
segments(-0.5,3.8414, 30.5, 3.8414)}
#plot(R_MH_nonP)#これでもよい
R_MH_P<-difMH(Data=Anxiety, group=age, focal.name=1, purify=TRUE)
#図6.10
{plot(R_MH_P$MH, cex=0.7, ylab="MH統計量",xlab="項目番号",pch="", cex.lab=1.2)
text(x=1:29,y=R_MH_P$MH,labels=as.character(1:29))
segments(-0.5,3.8414, 30.5, 3.8414)}
#plot(R_MH_P)#これでもよい

