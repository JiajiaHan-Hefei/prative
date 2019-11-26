Ufun <- function(df,n)
{ ##����Uͳ������Ϊ����ͳ����
  ##h(x1,x2,x3)=1/3*[sgn(median(xi(1),xi(2),xi(3))-mean(xi(1),xi(2),xi(3)))]
  USTAT = NULL;
  for (mu in 1:n)
  {
    x=rt(20,df);
    n1=length(x);
    H=NULL;
    for (i in 1:(n-2))
      for (j in (i+1):(n1-1))
        for (k in (j+1):n1)
        {
          a1 = sign(2*x[i]-x[j]-x[k]);
          a2 = sign(2*x[j]-x[i]-x[k]);
          a3 = sign(2*x[k]-x[i]-x[j]);
          h=1/3*(a1+a2+a3);
          H=c(H,h);
        }
    Ustat=(1/choose(length(x),3))*sum(H);##choose��������ļ���
    USTAT=c(USTAT,Ustat)
  }
hist(USTAT,border = F,col = "gray1");
list(Umean = mean(USTAT),Uvar = var(USTAT));
}