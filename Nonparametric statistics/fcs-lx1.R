#�ĵ�һ
library(MASS)
data(geyser)
duration = geyser[,2]
duration.sort = sort(duration)
duration.rank = rank(duration.sort)
duration.cdf = duration.rank/length(duration)
plot(duration.sort,duration.cdf)
N = length(duration)
segments(duration.sort[1:(N-1)],duration.cdf[1:(N-1)],duration.sort[2:N],duration.cdf[1:(N-1)])
duration.cdf[1:(N-1)]
alpha = 0.5
band = sqrt(1/(2*N))*log(2/alpha)
lower.95 = duration.cdf - band
upper.95 = duration.cdf + band
lines(duration.sort,lower.95,lty=2)
lines(duration.sort,upper.95)#���ƾ���ֲ�����������
alpha = seq(0.01,0.1,by = 0.001)
band = sqrt(1/(2*N))*log(2/alpha)
#�Ƚϲ�ͬalpha�µ������������
#�ɼ�����alpha�����������
plot(alpha,band)


#�ĵ���
Ufun(3,100)