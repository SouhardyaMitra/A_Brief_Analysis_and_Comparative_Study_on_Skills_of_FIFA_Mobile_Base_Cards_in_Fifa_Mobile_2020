#call the data

y1=football[football$POSITION=="FORWARD",]		#change position in each case
dim(y1)

x=matrix(0,131,8)		#based on dimension change the nrow
x[,1]=y1$PACE
x[,2]=y1$ACCURACY
x[,3]=y1$PASSING
x[,4]=y1$REFLEX
x[,5]=y1$DEFENDING
x[,6]=y1$PHYSICAL
x[,7]=y1$HEIGHT.IN.CM
x[,8]=y1$WEIGHT.IN.KG

skill=c("PACE","ACCURACY","PASSING","REFLEX","DEFENDING","PHYSICAL")

for(i in 1:6)
{
print(skill[i])

print(paste(skill[i]," vs height"))
print(cor(x[,i],x[,7]))

print(paste(skill[i]," vs weight"))
print(cor(x[,i],x[,8]))

print("height partial")
m1=resid(lm(x[,i] ~ x[,8]))
m2=resid(lm(x[,7] ~ x[,8]))
print(cor(m1,m2))

print("weight partial")
m3=resid(lm(x[,i] ~ x[,7]))
m4=resid(lm(x[,8] ~ x[,7]))
print(cor(m3,m4))
}
