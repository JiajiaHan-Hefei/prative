#����Boston���������и�����������λ��
#Ȼ���÷��ż����Լ������ȼ���������õ�����λ���Ƿ�����
library(MASS)
data(boston)
colnames(boston)=toupper(colnames(boston))#����������Сд��Ϊ��д����������
tax.median = median(boston$TAX)#TAX��������λ��
splus=sum(boston$TAX>tax.median)
sminus=sum(boston$TAX<tax.median)
k=min(sminus,splus)
n=splus+sminus
binom.test(k,n,0.5)#���ż���
wilcox.test(boston$TAX-tax.median)#wilcoxon�����ȼ���
plot(density(boston$TAX))#��ʾTAX����˫��ֲ�
ks.test(boston$TAX,pnorm,mean(boston$TAX),sd(boston$TAX))#Liliefor��̬�ֲ�����