getMetaInfo <-
function(filename){
  msdata=.jcall("XCMS", "Ljava/util/ArrayList;","getMSData", filename);
  xcms=.jnew("XCMS")
  RT=xcms$callRT(msdata)/60
  ms1=.jcall(xcms, "Ljava/util/ArrayList;", "getMSn", msdata, 1L)
  mz=.jcall(xcms, "[D", "callMZ", ms1)  
  mslevels=.jcall(xcms, "[I", "callMSLevels", msdata)   
  polarity=.jcall("XCMS", "[S", "callPolarity", msdata)
  ms2=.jcall(xcms, "Ljava/util/ArrayList;", "getMSn", msdata, 2L)
  cid=.jcall(xcms, "[D", "callCid", ms2)
  
  cat("--- General information on the experiment: \n")
  cat("**********************************************\n")
  cat("RT range <min>: ", round(range(RT)), "\n") 
  cat("mz range:", round(range(mz)), "\n")  
  cat("Total scans: ", .jcall(msdata, "I", "size"), "\n")
  cat("MSn scans: \n")
  print(table(mslevels))
  cat("\n")
  cat("Polarity:", unique(polarity), "\n")
  cat("Collision energy:", unique(cid), "\n")
  cat("---------------------------------------------\n")
  
  MSXMLParser="org/systemsbiology/jrap/stax/MSXMLParser"
  xml=.jnew(MSXMLParser, filename);
  instrument=xml$rapFileHeader()$getInstrumentInfo()
  
  cat("Manufacturer:", instrument$getManufacturer(),"\n") 
  cat("MS Model:", instrument$getModel(),"\n") 
  cat("Ionisation:", instrument$getIonization(),"\n") 
  cat("MS Analyser:", instrument$getMassAnalyzer(),"\n") 
  cat("MS Detecotr:", instrument$getDetector(),"\n")
  cat("Software:",  instrument$getSoftwareInfo()$name,
                    instrument$getSoftwareInfo()$version,  "\n")
  
  dataPro=xml$rapFileHeader()$getDataProcessing()  
  centroid=dataPro$getCentroided()
  cat("Centroid:", ifelse(centroid==1, "Yes", "No"), "\n") 
  cat("***********************************************\n")
}#>>>

