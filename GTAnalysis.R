# To perform the iterative analysis of a GTE.
require(ggplot2)
require(reshape2)

Conditions <- NULL

array=c(228, 238, 248, 258)

for (i in 1:length(array)){
  x=engineCondi(T0=array[i])
  Conditions <- rbind(Conditions, x)
  
} #end of for loop
Conditions$id=rownames(Conditions)
condi_long=melt(Conditions, id="id")

Condi_pressure=condi_long[condi_long$variable %in% c("pt0","pt2","pt3","pt4",
                                                     "pt5","pt7","pt9"),]
Condi_temp=condi_long[condi_long$variable %in% c("Tt0","Tt2","Tt3","Tt4","Tt5",
                                                 "Tt7","Tt9"),]
Condi_entropy=condi_long[condi_long$variable %in% c("s0","s2","s3","s4","s5","s7","s9"),]

Condi_perf=condi_long[condi_long$variable %in% c("FuelAir","FuelAirAfterburn",
                                                 "NondimensionalSpecificThrust",
                                                 "ThermalEfficiency",
                                                 "PropulsiveEfficiency"),]
Condi_TSFC=condi_long[condi_long$variable=="TSFC",]

ggplot(Conditions_long, aes(x=id,y=value,group=variable,color=variable))+
         geom_line()+ggtitle("All Values")
ggplot(Condi_pressure, aes(x=id,y=value,group=variable,color=variable))+
  geom_line()+ggtitle("Total Pressure")
ggplot(Condi_temp, aes(x=id,y=value,group=variable,color=variable))+
  geom_line()+ggtitle("Total Temperature")
ggplot(Condi_entropy, aes(x=id,y=value,group=variable,color=variable))+
  geom_line()+ggtitle("Entropy(Relative)")
ggplot(Condi_perf, aes(x=id,y=value,group=variable,color=variable))+
  geom_line()+ggtitle("Performance")
ggplot(Condi_TSFC, aes(x=id,y=value,group=variable,color=variable))+
  geom_line()+ggtitle("TSFC")
