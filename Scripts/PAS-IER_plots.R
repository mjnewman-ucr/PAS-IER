#APA Style Simple Slope Plots

#See https://sakaluk.wordpress.com/2015/08/27/6-make-it-pretty-plotting-2-way-interactions-with-ggplot2/
#Will need to manually add in slope equations in a word doc after inserting PDF of plot
#Slope info can be found using the this tool: http://www.quantpsy.org/interact/mlr2.htm
    #Make sure to save the output from here in a word doc in case you need it later

#Make sure to run analyses in main script first, since you will need to have
#the data in your environment

library(psych)
library(ggplot2)

apatheme=theme_bw()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border=element_blank(),
        axis.line=element_line(),
        text=element_text(family='Times'))



# Responsiveness X PAS

#Part 1
xx <- c(-5,3)   #  <-- change to alter plot dims
yy <- c(2.2342,6.919)   #  <-- change to alter plot dims
leg <- c(-5,2.8198)   #  <-- change to alter legend location
x <- c(-5,3)   #  <-- x-coords for lines
y1 <- c(3.015,6.919)
y2 <- c(3.18,6.38)
y3 <- c(3.345,5.841)
plot(xx,yy,type='n',font=2,font.lab=2,xlab='X',ylab='Y',main='MLR 2-Way Interaction Plot')
lines(x,y1,lwd=3,lty=1,col=1)
lines(x,y2,lwd=3,lty=5,col=2)
lines(x,y3,lwd=3,lty=6,col=3)
points(x,y1,col=1,pch=16)
points(x,y2,col=1,pch=16)
points(x,y3,col=1,pch=16)
legend(leg[1],leg[2],legend=c('CVz1(1)','CVz1(2)','CVz1(3)'),lwd=c(3,3,3),lty=c(1,5,6),col=c(1,2,3))


z1=-10  #supply lower bound for z
z2=10   #supply upper bound for z
z <- seq(z1,z2,length=1000)
fz <- c(z,z)
y1 <- (0.4+0.08*z)+(1.9725*sqrt(0.002+(2*z*0.0003)+((z^2)*0.001)))
y2 <- (0.4+0.08*z)-(1.9725*sqrt(0.002+(2*z*0.0003)+((z^2)*0.001)))
fy <- c(y1,y2)
fline <- (0.4+0.08*z)
plot(fz,fy,type='p',pch='.',font=2,font.lab=2,col=2,xlab='Moderator',ylab='Simple Slope',main='Confidence Bands')
lines(z,fline)
f0 <- array(0,c(1000))
lines(z,f0,col=8)
abline(v=-21.7932,col=4,lty=2)
abline(v=-2.7838,col=4,lty=2)

#Part 2

respXPAS_ss=ggplot(ds, aes(x = iris_r_centered, y = eff_comp)) +
  geom_point(alpha = 0.00) +
  labs(x = 'Emotional Responsiveness', y = 'IER Effectiveness') +
  scale_size_continuous(guide = "none") +
  geom_abline(aes(intercept=5.455, slope=0.488, linetype='+1SD PAS')) +
  geom_abline(aes(intercept=5.18, slope=0.4, linetype='Mean PAS')) +
  geom_abline(aes(intercept=4.905, slope=0.312, linetype='-1SD PAS')) +
  scale_linetype_manual(values=c('solid','dashed','dotted'), breaks=c('+1SD PAS','Mean PAS','-1SD PAS'),name=NULL)+ apatheme

respXPAS_ss + expand_limits(x = c(-5, 3), y = c(2, 7))

ggsave(file = "respXPAS_ss.pdf")   

#--------------------------------------------------------------------------------------------------

### Cog X PAS 

#Part 1

xx <- c(-5,3)   #  <-- change to alter plot dims
yy <- c(2.5906,6.8248)   #  <-- change to alter plot dims
leg <- c(-5,3.1199)   #  <-- change to alter legend location
x <- c(-5,3)   #  <-- x-coords for lines
y1 <- c(3.4884,6.8248)
y2 <- c(3.3924,6.3409)
y3 <- c(3.2963,5.8571)
plot(xx,yy,type='n',font=2,font.lab=2,xlab='X',ylab='Y',main='MLR 2-Way Interaction Plot')
lines(x,y1,lwd=3,lty=1,col=1)
lines(x,y2,lwd=3,lty=5,col=2)
lines(x,y3,lwd=3,lty=6,col=3)
points(x,y1,col=1,pch=16)
points(x,y2,col=1,pch=16)
points(x,y3,col=1,pch=16)
legend(leg[1],leg[2],legend=c('CVz1(1)','CVz1(2)','CVz1(3)'),lwd=c(3,3,3),lty=c(1,5,6),col=c(1,2,3))

z1=-10  #supply lower bound for z
z2=10   #supply upper bound for z
z <- seq(z1,z2,length=1000)
fz <- c(z,z)
y1 <- (0.36857+0.04407*z)+(1.9725*sqrt(0.0015938342+(2*z*0.0002193848)+((z^2)*0.0006529323)))
y2 <- (0.36857+0.04407*z)-(1.9725*sqrt(0.0015938342+(2*z*0.0002193848)+((z^2)*0.0006529323)))
fy <- c(y1,y2)
fline <- (0.36857+0.04407*z)
plot(fz,fy,type='p',pch='.',font=2,font.lab=2,col=2,xlab='Moderator',ylab='Simple Slope',main='Confidence Bands')
lines(z,fline)
f0 <- array(0,c(1000))
lines(z,f0,col=8)
abline(v=-3.9143,col=4,lty=2)
abline(v=55.3539,col=4,lty=2)

#Part 2

cogXPAS_ss=ggplot(ds, aes(x = iris_cs_centered, y = eff_comp)) +
  geom_point(alpha = 0.00) +
  labs(x = 'Cognitive Support', y = 'IER Effectiveness') +
  scale_size_continuous(guide = "none") +
  geom_abline(aes(intercept=5.5737, slope=0.417, linetype='+1SD PAS')) +
  geom_abline(aes(intercept=5.2352, slope=0.3686, linetype='Mean PAS')) +
  geom_abline(aes(intercept=4.8968, slope=0.3201, linetype='-1SD PAS')) +
  scale_linetype_manual(values=c('solid','dashed','dotted'), breaks=c('+1SD PAS','Mean PAS','-1SD PAS'),name=NULL)+ apatheme

cogXPAS_ss

cogXPAS_ss + expand_limits(x = c(-5, 3), y = c(2, 7))

ggsave(file = "cogXPAS_ss.pdf")   

#--------------------------------------------------------------------------------------------------

# Solicitation X PAS 

#Part 1
xx <- c(1,2)   #  <-- change to alter plot dims
yy <- c(4.698,6.2087)   #  <-- change to alter plot dims
leg <- c(1,4.8869)   #  <-- change to alter legend location
x <- c(1,2)   #  <-- x-coords for lines
y1 <- c(6.0581,6.2087)
y2 <- c(5.504,5.9144)
y3 <- c(4.9498,5.6201)
plot(xx,yy,type='n',font=2,font.lab=2,xlab='X',ylab='Y',main='MLR 2-Way Interaction Plot')
lines(x,y1,lwd=3,lty=1,col=1)
lines(x,y2,lwd=3,lty=5,col=2)
lines(x,y3,lwd=3,lty=6,col=3)
points(x,y1,col=1,pch=16)
points(x,y2,col=1,pch=16)
points(x,y3,col=1,pch=16)
legend(leg[1],leg[2],legend=c('CVz1(1)','CVz1(2)','CVz1(3)'),lwd=c(3,3,3),lty=c(1,5,6),col=c(1,2,3))

z1=-10  #supply lower bound for z
z2=10   #supply upper bound for z
z <- seq(z1,z2,length=1000)
fz <- c(z,z)
y1 <- (0.41045+-0.23618*z)+(1.9725*sqrt(0.01728333+(2*z*0.00004213929)+((z^2)*0.01327744)))
y2 <- (0.41045+-0.23618*z)-(1.9725*sqrt(0.01728333+(2*z*0.00004213929)+((z^2)*0.01327744)))
fy <- c(y1,y2)
fline <- (0.41045+-0.23618*z)
plot(fz,fy,type='p',pch='.',font=2,font.lab=2,col=2,xlab='Moderator',ylab='Simple Slope',main='Confidence Bands')
lines(z,fline)
f0 <- array(0,c(1000))
lines(z,f0,col=8)
abline(v=0.5271,col=4,lty=2)
abline(v=46.6163,col=4,lty=2)

#Part 2

seekingXPAS_ss=ggplot(ds, aes(x = seeking, y = eff_comp)) +
  geom_point(alpha = 0.00) +
  labs(x = 'Youth-Solicited IER', y = 'IER Effectiveness') +
  scale_size_continuous(guide = "none") +
  geom_abline(aes(intercept=5.9074, slope=0.1507, linetype='+1SD PAS')) +
  geom_abline(aes(intercept=5.0935, slope=0.4105, linetype='Mean PAS')) +
  geom_abline(aes(intercept=4.2796, slope=0.6702, linetype='-1SD PAS')) +
  scale_linetype_manual(values=c('solid','dashed','dotted'), breaks=c('+1SD PAS','Mean PAS','-1SD PAS'),name=NULL)+ apatheme

seekingXPAS_ss

seekingXPAS_ss + ylim(3, 7)

ggsave(file = "seekingXPAS_ss.pdf")   
