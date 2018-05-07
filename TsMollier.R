# Program to create a Mollier diagram from a chosen row from the Conditions data frame.

TsMollier = function (row){
 Tt=c(Conditions[row,'Tt0'],Conditions[row,'Tt2'],Conditions[row,'Tt3'],Conditions[row,'Tt4'],
          Conditions[row,'Tt5'],Conditions[row,'Tt7'],Conditions[row,'Tt9'])
 s=c(Conditions[row,'s0'],Conditions[row,'s2'],Conditions[row,'s3'],Conditions[row,'s4'],
          Conditions[row,'s5'],Conditions[row,'s7'],Conditions[row,'s9'])
 MollierData = data.frame(s,Tt)

 ggplot(MollierData,aes(x=s))+geom_line(aes(y=Tt))+geom_point(y=Tt)
  
}