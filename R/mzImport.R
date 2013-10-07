mzImport <-
function(iontree, dbname="mzDB.db", exp.id){
  if(class(iontree)!="iontree") stop("class of iontree is required")
  #format MS2 spectrum
  ion.ms2=formatSpec(iontree@MS2, fromTo="mat2str")

  #ion ms3 is a list
  if(length(iontree@MS3)==0){
   ion.ms3=""
  }else{
   ms3=xmlNode("ms3")
   node.ms3=lapply(iontree@MS3, function(x) xmlNode("ms3",
                                      attrs=c(premz=as.character(x$premz2)),
                                      formatSpec(x$sp3, "mat2str")))
   tmp=addChildren(ms3, kids=node.ms3)
   ion.ms3=toString(tmp)    #xmlParse(s1)
  }
  
  #only information to a few columns, rt=0 for DIMS, exp.id=1
  #exp.id=1
  record.1=data.frame(exp_id=exp.id, 
                      mz=mean(iontree@mz), 
                      rt=0, 
                      ms2=ion.ms2, 
                      ms3=ion.ms3)
  sql.insert="INSERT INTO mz(exp_id, mz, rt, ms2, ms3) VALUES($exp_id, $mz, $rt, $ms2, $ms3)"

  #transaction, make sure right path
  db=dbConnect(dbDriver("SQLite"), dbname)
  
  #!!check table "experiment", output _id, species etc
  q1=dbSendQuery(db, "SELECT _id, species FROM experiment") 
  rs=fetch(q1, n=-1) 
  dbClearResult(q1)
  cat("current experiments in the database\n")
  print(rs)
  if(!exp.id %in% rs[,1]) stop("exp id not existed in table 'experiement'")
  
  #"mz" %in% dbListTables(db)
  dbBeginTransaction(db)
  dbGetPreparedQuery(db, sql.insert, bind.data=record.1)
  dbCommit(db)

  invisible(dbDisconnect(db)) 
 }#>>>mzImport

