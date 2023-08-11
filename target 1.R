rm(list=ls())
football=read.csv(file="C:/Users/dell/Desktop/dissertation/FOOTBALL DATA.csv")
y1=football[football$POSITION=="GK",]
y2=football[football$POSITION=="DEF",]
y3=football[football$POSITION=="MID",]
y4=football[football$POSITION=="FORWARD",]

#GK

y11=y1$PACE
y12=y1$ACCURACY
y13=y1$PASSING
y14=y1$REFLEX
y15=y1$DEFENDING
y16=y1$PHYSICAL
x11=y1$HEIGHT.IN.CM
x12=y1$WEIGHT.IN.KG

#GK HEIGHT AND SKILLS

GK_pace_height=lm(y11~x11)
GK_accuracy_height=lm(y12~x11)
GK_passing_height=lm(y13~x11)
GK_reflex_height=lm(y14~x11)
GK_defending_height=lm(y15~x11)
GK_physical_height=lm(y16~x11)

#GK WEIGHT AND SKILLS

GK_pace_weight=lm(y11~x12)
GK_accuracy_weight=lm(y12~x12)
GK_passing_weight=lm(y13~x12)
GK_reflex_weight=lm(y14~x12)
GK_defending_weight=lm(y15~x12)
GK_physical_weight=lm(y16~x12)


##DEF

y21=y2$PACE
y22=y2$ACCURACY
y23=y2$PASSING
y24=y2$REFLEX
y25=y2$DEFENDING
y26=y2$PHYSICAL
x21=y2$HEIGHT.IN.CM
x22=y2$WEIGHT.IN.KG

#DEF HEIGHT AND SKILLS


DEF_pace_height=lm(y21~x21)
DEF_accuracy_height=lm(y22~x21)
DEF_passing_height=lm(y23~x21)
DEF_reflex_height=lm(y24~x21)
DEF_defending_height=lm(y25~x21)
DEF_physical_height=lm(y26~x21)

#DEF WEIGHT AND SKILLS

DEF_pace_weight=lm(y21~x22)
DEF_accuracy_weight=lm(y22~x22)
DEF_passing_weight=lm(y23~x22)
DEF_reflex_weight=lm(y24~x22)
DEF_defending_weight=lm(y25~x22)
DEF_physical_weight=lm(y26~x22)



##MID

y31=y3$PACE
y32=y3$ACCURACY
y33=y3$PASSING
y34=y3$REFLEX
y35=y3$DEFENDING
y36=y3$PHYSICAL
x31=y3$HEIGHT.IN.CM
x32=y3$WEIGHT.IN.KG

## MID HEIGHT AND SKILLS


MID_pace_height=lm(y31~x31)
MID_accuracy_height=lm(y32~x31)
MID_passing_height=lm(y33~x31)
MID_reflex_height=lm(y34~x31)
MID_defending_height=lm(y35~x31)
MID_physical_height=lm(y36~x31)


## MID WEIGHT AND SKILLS


MID_pace_weight=lm(y31~x32)
MID_accuracy_weight=lm(y32~x32)
MID_passing_weight=lm(y33~x32)
MID_reflex_weight=lm(y34~x32)
MID_defending_weight=lm(y35~x32)
MID_physical_weight=lm(y36~x32)


##FORWARD


y41=y4$PACE
y42=y4$ACCURACY
y43=y4$PASSING
y44=y4$REFLEX
y45=y4$DEFENDING
y46=y4$PHYSICAL
x41=y4$HEIGHT.IN.CM
x42=y4$WEIGHT.IN.KG

##FORWARD HEIGHT AND SKILLS


FWD_pace_height=lm(y41~x41)
FWD_accuracy_height=lm(y42~x41)
FWD_passing_height=lm(y43~x41)
FWD_reflex_height=lm(y44~x41)
FWD_defending_height=lm(y45~x41)
FWD_physical_height=lm(y46~x41)

##FORWARD WEIGHT AND SKILLS

FWD_pace_weight=lm(y41~x42)
FWD_accuracy_weight=lm(y42~x42)
FWD_passing_weight=lm(y43~x42)
FWD_reflex_weight=lm(y44~x42)
FWD_defending_weight=lm(y45~x42)
FWD_physical_weight=lm(y46~x42)


##TARGET 2









