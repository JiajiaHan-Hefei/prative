##��дCox-Staut�����Լ��麯��
coxs.test=function(x,alpha)
{
  n=length(x)
  D=c()
  if(n%%2==0) {c=n/2}
  else {c=(n+1)/2}
  for(i in 1:(c-1)) {D[i]=x[i]-x[i+c]}
  splus=sum(D>0)
  sminus=sum(D<0)
  n.new=splus+sminus
  K=min(splus,sminus)
  p=pbinom(K,n.new,0.5)
  if(p<alpha)
  {return(paste("�ܾ�ԭ����,pֵΪ",p))}
  else
  {return(paste("���ܾ�ԭ���裬pֵΪ",p))}
}



#���ݼ���չʾ
library(ks)
data(faithful)
H <- Hpi(x=faithful)
fhat <- kde(x=faithful,H=H)
plot(fhat,display="filled.contour2")
points(faithful,cex=0.5,pch=16)