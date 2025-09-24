#####################################
#Chapter13. マルチレベルIRTモデル
#####################################

#ライブラリのロード
#パッケージmlirtはFox, J.P.のホームページ
#(http://www.jean-paulfox.com/)からダウンロードする．
#mlirtの使い方の詳細についてはパッケージ付録のマニュアル
#を参照されたい
library(mlirt)

#パッケージcodaはcranからダウンロード可能
#(http://cran.r-project.org/web/packages/coda/index.html)
library(coda)

#項目反応パタン(行列指定)
Y <- as.matrix(read.table("Y.csv",sep=","))

#性別：レベル1変数
woman <- as.matrix(read.table("woman.csv",sep=","))

#公私:レベル2変数
public <- as.matrix(read.table("public.csv",sep=","))

#学校毎の人数
school <- as.matrix(read.table("school.csv",sep=","))

#提案モデル
S <- c(1,0,1,1) 
S2 <- matrix(
c(1,0),ncol=1,byrow=T)

#各学校のサンプルサイズ
nll <- school

#提案モデルの実行
#固定効果導入モデル推定
#XG=MCMC反復
out <- estmlirt(Y=Y,S=S,S2=S2,XF=woman,
W=public,nll=nll,XG=20000,scaling1=1)

#EAP推定値，SD，確信区間の表示
#estmlirtでは乱数生成へ渡すseedが指定できないので，テキスト中の
#数値は厳密に再現できない
mlirtout(10000,out)

#burnin期間の指定
burnin <-10000
last <-20000

#レベル2の誤差分散のマルコフ連鎖の収束判定
#Gewekeの指標
geweke.diag(out[[35]][burnin:last])
#heidelberger & Weltchの指標
heidel.diag(out[[35]][burnin:last])
#トレース図の描画
plot(out[[35]][burnin:last],type="l",col="black",
ylab="",xlab="iteration")

#レベル1の誤差分散のマルコフ連鎖の収束判定
geweke.diag(out[[31]][burnin:last])
heidel.diag(out[[31]][burnin:last])
plot(out[[31]][burnin:last],type="l",col="black",
ylab="",xlab="iteration")

#レベル2の固定効果(公私の効果)のマルコフ連鎖の収束判定
refix2<-matrix(out[[30]],ncol=2,byrow=F)
geweke.diag(refix2[burnin:last,])
heidel.diag(refix2[burnin:last,])
plot(refix2[burnin:last,1],type="l",col="black",
ylab="",xlab="iteration")
plot(refix2[burnin:last,2],type="l",col="black",
ylab="",xlab="iteration")


#レベル1の固定効果(性別の効果)のマルコフ連鎖の収束判定
geweke.diag(out[[32]][burnin:last])
heidel.diag(out[[32]][burnin:last])
plot(out[[32]][burnin:last],type="l",col="black",
xlab="iteration",ylab="")


#ランダム切片に対するランダム効果のEAP推定値
hist(out[[34]],breaks=30,col="grey",xlab="u",
main="",xlim=c(-2,2),ylim=c(0,20))

#ItemparameterのMCMC標本
resparam<-matrix(out[[17]],ncol=36,byrow=F)
geweke.diag(resparam[burnin:last,])
heidel.diag(resparam[burnin:last,])

#識別力と困難度のEAP推定値
EAPa<-out[[13]]
EAPb<-out[[16]]


#最小モデルの指定
S <- c(1,0,0,0) 
S2 <- 0

#最小モデルの推定
out2 <- estmlirt(Y=Y,S=S,S2=S2,XF=woman,W=public,
nll=nll,XG=20000,scaling1=1)

#EAP推定値，SD，確信区間の表示
mlirtout(10000,out2)

