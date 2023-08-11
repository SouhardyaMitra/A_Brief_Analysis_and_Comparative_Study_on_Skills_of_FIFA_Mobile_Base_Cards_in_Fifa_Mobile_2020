#call the data

x=matrix(0,544,6)
x[,1]=football$REFLEX
x[,2]=football$PACE
x[,3]=football$ACCURACY
x[,4]=football$PASSING
x[,5]=football$DEFENDING
x[,6]=football$PHYSICAL

#r1j
for(j in 2:6)
print(cor(x[,1],x[,j]))							#considering x6

#r1j.6
m1=resid(lm(x[,1] ~ x[,6]))
for(j in 2:5)
{
m2=resid(lm(x[,j] ~ x[,6]))
print(cor(m1,m2))
}										#considering x3

#r1j.63				
m1=resid(lm(x[,1] ~ x[,6] + x[,3]))
for(j in c(2,4,5))
{
m2=resid(lm(x[,j] ~ x[,6] + x[,3]))
print(cor(m1,m2))
}										#considering x5

#r1j.635
m1=resid(lm(x[,1] ~ x[,6] + x[,3] + x[,5]))
for(j in c(2,4))
{
m2=resid(lm(x[,j] ~ x[,6] + x[,3] + x[,5]))
print(cor(m1,m2))
}										#x2 and x4, both have values close to 0.2, proceeding to multiple once considering x4, and then not considering it

#r1j.6354									#no need
m1=resid(lm(x[,1] ~ x[,6] + x[,3] + x[,5] + x[,4]))
m2=resid(lm(x[,2] ~ x[,6] + x[,3] + x[,5] + x[,4]))
print(cor(m1,m2))

summary(lm(x[,1] ~ x[,6] + x[,3] + x[,5] + x[,4] + x[,2]))	#no need
summary(lm(x[,1] ~ x[,6] + x[,3] + x[,5] + x[,4]))		
summary(lm(x[,1] ~ x[,6] + x[,3] + x[,5]))			#this model is okay

#there is not much difference in the 2 values of multiple R sq when x4 is considered and when it is not. Thus we may ignore x4.
#we can ignore PACE and PASSING
