####　第5章　特異項目機能（DIF）　―IRTに基づく方法―

## パッケージの読み込み
library(difR)

## 「Anxietyデータ」の読み込み
Anx<-read.csv("1/chapter05/PROMIS.csv")
head(Anx)


######################################################
### 5.2.2　Lordの方法
### 均一DIFの検出（1母数ロジスティックモデル）・年齢
######################################################

##### データの準備　#####
## グループ変数として年齢のみを残す
Anx.a<-Anx[,-c(2,3)]
head(Anx.a)

## データを年齢低群（0=65歳未満）と年齢高群（1=65歳以上）に分割
nH<-sum(Anx.a$age)
nL<-nrow(Anx.a)-nH
data.Low<-Anx.a[,2:30][order(Anx.a$age),][1:nL,]
data.High<-Anx.a[,2:30][order(Anx.a$age),][(nL+1):(nL+nH),]

##### 項目母数の推定と等化　#####

#####　項目母数を推定する関数　#####
##　　関数　itemParEst
##  itemParEst(data, model, ...)
##　data：データ行列
##　model：IRTモデルの種類（"1PL","2PL","3PL"のいずれか）
#####################################

## グループごとに項目母数を推定（表5.2）
(mL<-itemParEst(data.Low, model="1PL")) ## 年齢低群の困難度
(mH<-itemParEst(data.High, model="1PL"))## 年齢高群の困難度（等化前）
colMeans(mL);colMeans(mH) ##困難度の平均

#####　項目母数の等化　#####
##　　関数　itemRescale
##  itemRescale(mR, mF, ...)
##　mR：等化の基準となる項目母数の推定値行列
##　mF：等化される項目母数の推定値行列
#####################################

## 項目母数の等化（表5.2）
(mH.new<-itemRescale(mL,mH))## 年齢高群の困難度（等化後）
colMeans(mH.new) ##困難度の平均

## 2グループの困難度の散布図（図5.3）
plot(mL[,1], mH.new[,1], xlab="b(low)", ylab="b(high)",xlim=c(-1.5,4),ylim=c(-1.5,4),type="n")
x<-as.character(seq(from=1,to=29)) 
text(mL[,1], mH.new[,1], x ) 
abline(a=0,b=1)

#####　Lordの方法によるDIFの検出　#####
##　　関数　difLord
##　 difLord(Data, group, focal.name, model, 
##           irtParam=NULL, same.scale=TRUE, alpha=0.05,...)
##　Data：データ行列もしくはデータ行列に下位グループを示すベクトルを加えた行列
##　group：下位グループを表すベクトル，もしくは下位グループを表すデータ内の列の指示子
##　focal.name：比較の基準とならない方のグループの水準
##　model：IRTモデルの種類（"1PL","2PL","3PL"のいずれか）
##　irtParam：項目母数の行列（項目数×２行）
##　same.scale：irtParam行列の項目母数が同一尺度上にあるか否か（デフォルトは"TRUE"）
##　alpha：有意水準（デフォルトは0.05）
#####################################

###　注） difR内の関数を用いると出力結果に「Output was not captured!」「The plot was not captured!」
###      と表示されるが，これは結果を外部ファイルに保存していないことについての忠告．
###      外部ファイルに保存する場合は，関数内に以下を追加すれば良い．
###      DIFを実行する関数の場合：save.output = TRUE, output = c("ファイル名","default")
###      plot関数の場合：save.plot = TRUE, save.options = c("ファイル名", "default", "pdf"（もしくは"jpeg"）)


## 等化後の項目母数を用いる場合（表5.3）
item.1PL<-rbind(mL,mH.new)
(lord.a<-difLord(irtParam=item.1PL,same.scale=TRUE))

## 困難度の推定値を得ずにDIF分析の結果のみ出力する場合（データを分けて推定する必要がない）
(lord.a<-difLord(Anx.a, group="age", focal.name=1, model="1PL"))

## DIF分析の結果を図で表わす（図5.4）
plot(lord.a)

##　R25のIRF（図5.5）
plot(lord.a, plot="itemCurve",item=25,group.names=c("Low","High"))
# group.namesは等化の際に基準としたグループを先に示す。

##　R21, R24, R28のIRF
plot(lord.a, plot="itemCurve",item=21,group.names=c("Low","High"))
plot(lord.a, plot="itemCurve",item=24,group.names=c("Low","High"))
plot(lord.a, plot="itemCurve",item=28,group.names=c("Low","High"))


######################################################
### 5.2.2　Lordの方法
### 不均一DIFの検出（2母数ロジスティックモデル）・性別
######################################################

##### データの準備　#####
## グループ変数として性別のみを残す
Anx.g<-Anx[,-c(1,3)]
head(Anx.g)

##　データを男性・女性に分割
nF<-sum(Anx.g$gender)
nM<-nrow(Anx.g)-nF
data.Man<-Anx.g[,2:30][order(Anx.g$gender),][1:nM,]
data.Woman<-Anx.g[,2:30][order(Anx.g$gender),][(nM+1):(nM+nF),]

##### 項目母数の推定と等化　#####
##　グループごとに項目母数を推定（表5.4）
(mM.2PL<-itemParEst(data.Man, model="2PL"))  ## 男性の項目母数
(mW.2PL<-itemParEst(data.Woman, model="2PL"))## 女性の項目母数（等化前）
colMeans(mM.2PL);colMeans(mW.2PL) ##困難度・識別力の平均
sd(mM.2PL[,"b"]);sd(mW.2PL[,"b"]) ##困難度の標準偏差

## 項目母数の等化（表5.4）
(mW.2PL.new<-itemRescale(mM.2PL, mW.2PL)) ## 女性の項目母数（等化後）
item.2PL<-rbind(mM.2PL, mW.2PL.new)
colMeans(mW.2PL.new) ##困難度・識別力の平均

#####　Lordの方法によるDIFの検出　#####
(lord.g.2PL<-difLord(Anx.g,group="gender",focal.name=1,model="2PL"))

##　DIF分析の結果を図で表わす（図5.6）
plot(lord.g.2PL)

##　R3のIRF（図5.7）
plot(lord.g.2PL, plot="itemCurve",item=3,group.names=c("Man","Woman"))

## R6, R7, R10, R29のIRF
plot(lord.g.2PL, plot="itemCurve",item=6,group.names=c("Man","Woman"))
plot(lord.g.2PL, plot="itemCurve",item=7,group.names=c("Man","Woman"))
plot(lord.g.2PL, plot="itemCurve",item=10,group.names=c("Man","Woman"))


##########################################
### 5.3.2　一般化Lordの方法
### 均一DIFの検出（1母数ロジスティックモデル）・年齢×性別
##########################################

##### データの準備　#####
##　グループ（年齢低・男性，年齢高・男性，年齢低・女性，年齢低・女性）の作成
anx<-Anx
anx$group<-rep("group",nrow(Anx))
head(anx)
anx$group[anx$age==0 & anx$gender==0]<-"ManLow"
anx$group[anx$age==0 & anx$gender==1]<-"WomanLow"
anx$group[anx$age==1 & anx$gender==0]<-"ManHigh"
anx$group[anx$age==1 & anx$gender==1]<-"WomanHigh"
head(anx)
table(anx$group)　##　グループごとの受験者数を確認

##　グループ変数として新たに作成したもののみ残す
Anx.ag<-anx[,-c(1:3)]
head(Anx.ag)

## データをそれぞれのグループに分割
data0<-data1<-data2<-data3<-NULL
for (i in 1:nrow(anx)){
if (anx$group[i]=="WomanLow") data0<-rbind(data0,anx[i,4:32])
if (anx$group[i]=="WomanHigh") data1<-rbind(data1,anx[i,4:32])
if (anx$group[i]=="ManLow") data2<-rbind(data2,anx[i,4:32])
if (anx$group[i]=="ManHigh") data3<-rbind(data3,anx[i,4:32])
}

##### 項目母数の推定と等化　#####
##　グループごとに項目母数を推定
(m0.1PL<-itemParEst(data0, model="1PL"))#WomanLow
(m1.1PL<-itemParEst(data1, model="1PL"))#WomanHigh
(m2.1PL<-itemParEst(data2, model="1PL"))#ManLow
(m3.1PL<-itemParEst(data3, model="1PL"))#ManHigh

## 項目母数の等化（表5.5）
(irt.scale<-rbind(m0.1PL, itemRescale(m0.1PL,m1.1PL),itemRescale(m0.1PL,m2.1PL) ,itemRescale(m0.1PL,m3.1PL)))
# 等化の基準となるグループ以外のグループ名
names<-c("WomanHigh","ManLow","ManHigh")

#####　一般化Lordの方法によるDIFの検出　#####
##　　関数　difGenLord
##　 diGenfLord(Data, group, focal.names, model, 
##              irtParam=NULL, nrFocal=2, same.scale=TRUE, alpha=0.05,...)
##　Data：データ行列もしくはデータ行列に下位グループを示すベクトルを加えた行列
##　group：下位グループを表すベクトル，もしくは下位グループを表すデータ内の列の指示子
##　focal.names：比較の基準とならない方のグループの水準
##　model：IRTモデルの種類（"1PL","2PL","3PL"のいずれか）
##　irtParam：項目母数の行列（項目数×２行）
##　nrFocal：グループ数-1
##　same.scale：irtParam行列の項目母数が同一尺度上にあるか否か（デフォルトは"TRUE"）
##　alpha：有意水準（デフォルトは0.05）
#####################################

#####　一般化Lordの方法によるDIFの検出（表5.5）　#####
(glord<-difGenLord(irtParam=irt.scale, nrFocal=3,  same.scale=TRUE, focal.name=names))

## DIF分析の結果を図で表わす（図5.8）
plot(glord)

##　R13のIRF（図5.9）
plot(glord, plot="itemCurve",item=13,ref.name="WomanLow")

##　R21, R25のIRF
plot(glord, plot="itemCurve",item=21,ref.name="WomanLow")
plot(glord, plot="itemCurve",item=25,ref.name="WomanLow")


##########################################
### 5.4.2　Rajuの方法による均一DIFの検出・性別
##########################################

#####　データの準備　#####
## グループ変数として性別のみを残す
Anx.g<-Anx[,-c(1,3)]
head(Anx.g)

##　データを男性・女性に分割
nF<-sum(Anx.g$gender)
nM<-nrow(Anx.g)-nF
data.Man<-Anx.g[,2:30][order(Anx.g$gender),][1:nM,]
data.Woman<-Anx.g[,2:30][order(Anx.g$gender),][(nM+1):(nM+nF),]

##### 項目母数の推定と等化　#####
##　グループごとに項目母数を推定
(mM<-itemParEst(data.Man, model="1PL"))  ## 男性の項目母数（表5.6）
(mW<-itemParEst(data.Woman, model="1PL"))## 女性の項目母数（等化前）

## 項目母数の等化
(mW.new<-itemRescale(mM,mW)) ## 女性の項目母数（等化後）（表5.6）

#####　Rajuの方法によるDIFの検出　#####
##　　関数　difRaju
##　 difRaju(Data, group, focal.name, model, 
##           irtParam=NULL, same.scale=TRUE, alpha=0.05, signed=FALSE,...)
##　Data：データ行列もしくはデータ行列に下位グループを示すベクトルを加えた行列
##　group：下位グループを表すベクトル，もしくは下位グループを表すデータ内の列の指示子
##　focal.names：比較の基準とならない方のグループの水準
##　model：IRTモデルの種類（"1PL","2PL","3PL"のいずれか）
##　irtParam：項目母数の行列（項目数×２行）
##　nrFocal：グループ数-1
##　same.scale：irtParam行列の項目母数が同一尺度上にあるか否か（デフォルトは"TRUE"）
##　alpha：有意水準（デフォルトは0.05）
##　signed：Rajuの統計量をsigned area(TRUE)で計算するかunsigned area(FALSE, デフォルト)で計算するか
#####################################

#####　Rajuの方法によるDIFの検出（表5.6）　#####
(raju.g<-difRaju(Anx.g,group="gender",focal.name=1,model="1PL"))

## DIF分析の結果を図で表わす（図5.10）
plot(raju.g)

##　R20のIRF（図5.11）
lord.g<-difLord(Anx.g,group="gender",focal.name=1,model="1PL")
plot(lord.g, plot="itemCurve",item=20,group.names=c("Man","Woman"))
## difRaju関数ではICCがそのままプロットされないため
## 項目母数の推定値が同じLord関数を便宜上，利用している


##########################################
### 5.4.3　LR検定による均一DIFの検出・学歴
##########################################

##### データの準備　#####
### グループ変数として学歴のみを残す
Anx.e<-Anx[,-c(1,2)]
head(Anx.e)

#####　LR検定によるDIFの検出　#####
##　　関数　difLRT
##　 difLRT(Data, group, focal.name, model, 
##           irtParam=NULL, same.scale=TRUE, alpha=0.05,...)
##　Data：データ行列もしくはデータ行列に下位グループを示すベクトルを加えた行列
##　group：下位グループを表すベクトル，もしくは下位グループを表すデータ内の列の指示子
##　focal.names：比較の基準とならない方のグループの水準
##　model：IRTモデルの種類（"1PL","2PL","3PL"のいずれか）
##　irtParam：項目母数の行列（項目数×２行）
##　nrFocal：グループ数-1
##　same.scale：irtParam行列の項目母数が同一尺度上にあるか否か（デフォルトは"TRUE"）
##　alpha：有意水準（デフォルトは0.05）
#####################################

#####　LR検定によるDIFの検出（表5.7）　#####
##　LR検定によるDIFの検出（数時間かかることがある）
t<-proc.time()
(LRT.e<-difLRT(Anx.e,group="education",focal.name=1, p.adjust.method="BH"))
proc.time()-t # 3.1 hours
saveRDS(LRT.e, "LRT_e.rds")
(LRT.e <- readRDS("1/chapter05/LRT_e.rds"))
##　DIF分析の結果を図で表わす（図5.12）
plot(LRT.e)

