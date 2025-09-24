#####################################
#Chapter13. �}���`���x��IRT���f��
#####################################

#���C�u�����̃��[�h
#�p�b�P�[�Wmlirt��Fox, J.P.�̃z�[���y�[�W
#(http://www.jean-paulfox.com/)����_�E�����[�h����D
#mlirt�̎g�����̏ڍׂɂ��Ă̓p�b�P�[�W�t�^�̃}�j���A��
#���Q�Ƃ��ꂽ��
library(mlirt)

#�p�b�P�[�Wcoda��cran����_�E�����[�h�\
#(http://cran.r-project.org/web/packages/coda/index.html)
library(coda)

#���ڔ����p�^��(�s��w��)
Y <- as.matrix(read.table("Y.csv",sep=","))

#���ʁF���x��1�ϐ�
woman <- as.matrix(read.table("woman.csv",sep=","))

#����:���x��2�ϐ�
public <- as.matrix(read.table("public.csv",sep=","))

#�w�Z���̐l��
school <- as.matrix(read.table("school.csv",sep=","))

#��ă��f��
S <- c(1,0,1,1) 
S2 <- matrix(
c(1,0),ncol=1,byrow=T)

#�e�w�Z�̃T���v���T�C�Y
nll <- school

#��ă��f���̎��s
#�Œ���ʓ������f������
#XG=MCMC����
out <- estmlirt(Y=Y,S=S,S2=S2,XF=woman,
W=public,nll=nll,XG=20000,scaling1=1)

#EAP����l�CSD�C�m�M��Ԃ̕\��
#estmlirt�ł͗��������֓n��seed���w��ł��Ȃ��̂ŁC�e�L�X�g����
#���l�͌����ɍČ��ł��Ȃ�
mlirtout(10000,out)

#burnin���Ԃ̎w��
burnin <-10000
last <-20000

#���x��2�̌덷���U�̃}���R�t�A���̎�������
#Geweke�̎w�W
geweke.diag(out[[35]][burnin:last])
#heidelberger & Weltch�̎w�W
heidel.diag(out[[35]][burnin:last])
#�g���[�X�}�̕`��
plot(out[[35]][burnin:last],type="l",col="black",
ylab="",xlab="iteration")

#���x��1�̌덷���U�̃}���R�t�A���̎�������
geweke.diag(out[[31]][burnin:last])
heidel.diag(out[[31]][burnin:last])
plot(out[[31]][burnin:last],type="l",col="black",
ylab="",xlab="iteration")

#���x��2�̌Œ����(�����̌���)�̃}���R�t�A���̎�������
refix2<-matrix(out[[30]],ncol=2,byrow=F)
geweke.diag(refix2[burnin:last,])
heidel.diag(refix2[burnin:last,])
plot(refix2[burnin:last,1],type="l",col="black",
ylab="",xlab="iteration")
plot(refix2[burnin:last,2],type="l",col="black",
ylab="",xlab="iteration")


#���x��1�̌Œ����(���ʂ̌���)�̃}���R�t�A���̎�������
geweke.diag(out[[32]][burnin:last])
heidel.diag(out[[32]][burnin:last])
plot(out[[32]][burnin:last],type="l",col="black",
xlab="iteration",ylab="")


#�����_���ؕЂɑ΂��郉���_�����ʂ�EAP����l
hist(out[[34]],breaks=30,col="grey",xlab="u",
main="",xlim=c(-2,2),ylim=c(0,20))

#Itemparameter��MCMC�W�{
resparam<-matrix(out[[17]],ncol=36,byrow=F)
geweke.diag(resparam[burnin:last,])
heidel.diag(resparam[burnin:last,])

#���ʗ͂ƍ���x��EAP����l
EAPa<-out[[13]]
EAPb<-out[[16]]


#�ŏ����f���̎w��
S <- c(1,0,0,0) 
S2 <- 0

#�ŏ����f���̐���
out2 <- estmlirt(Y=Y,S=S,S2=S2,XF=woman,W=public,
nll=nll,XG=20000,scaling1=1)

#EAP����l�CSD�C�m�M��Ԃ̕\��
mlirtout(10000,out2)

