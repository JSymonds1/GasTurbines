# To perform the iterative
Conditions <- NULL

array=c(228, 238, 248, 258)

for (i in 1:length(array)){
  x=engineCondi(T0=array[i])
  Conditions <- rbind(Conditions, x)
  
} #end of for loop

ggplot(Conditions, aes(x=1:NROW(Conditions)),xlab("Variable"),ylab("Performance"))+
  theme(legend.position="right")+
  geom_line(aes(y=pt0),color="aliceblue")+
  geom_line(aes(y=Tt0),color="antiquewhite2")+
  geom_line(aes(y=s0),color="aquamarine")+
  geom_line(aes(y=pt2),color="azure3")+
  geom_line(aes(y=Tt2),color="bisque2")+
  geom_line(aes(y=s2),color="blue")+
  geom_line(aes(y=pt3),color="blueviolet")+
  geom_line(aes(y=Tt3),color="brown2")+
  geom_line(aes(y=s3),color="burlywood4")+
  geom_line(aes(y=pt4),color="cadetblue3")+
  geom_line(aes(y=Tt4),color="chartreuse2")+
  geom_line(aes(y=s4),color="chocolate4")+
  geom_line(aes(y=pt5),color="coral2")+
  geom_line(aes(y=Tt5),color="cornflowerblue")+
  geom_line(aes(y=s5),color="cyan3")+
  geom_line(aes(y=pt7),color="darkblue")+
  geom_line(aes(y=Tt7),color="goldenrod1")+
  geom_line(aes(y=s7),color="darkgreen")+
  geom_line(aes(y=pt9),color="darkolivegreen4")+
  geom_line(aes(y=Tt9),color="darkorange1")+
  geom_line(aes(y=s9),color="darkorchid1")+
  geom_line(aes(y=FuelAir),color="gold")+
  geom_line(aes(y=FuelAirAfterburn),color="firebrick")+
  geom_line(aes(y=NondimensionalSpecificThrust),color="deepskyblue4")+
  geom_line(aes(y=TSFC),color="darkseagreen2")+
  geom_line(aes(y=ThermalEfficiency),color="darkviolet")+
  geom_line(aes(y=PropulsiveEfficiency),color="dodgerblue4")