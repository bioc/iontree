metaDataImport <-
function(dbname="mzDB.db"){
  if(.Platform$OS.type!="widows"){
    stop("Please use sqlitebroswer for data entry.\n A Java-based editor may be provided in the later version of iontree package.")
  }
  #!!!!!!!!!!!!!!!!!!!!!!!!!!
  tablename="experiment"
  #!!!!!!!!!!!!!!!!!!!!!!!!!!
  db=dbConnect(dbDriver("SQLite"), dbname)
  if(!dbExistsTable(db, tablename)) stop("table does not exist!")
 
  #presume table "experiment" exists
  #sql.1=paste("SELECT * FROM", tablename)
  sql.1=paste("SELECT * FROM", tablename, "limit 1")
  rs=dbSendQuery(db, sql.1) 
  fetch(rs, n=-1)   
  tableDef=dbColumnInfo(rs)$name
  invisible(dbClearResult(rs))
  
  #primary key added automatically 
  tableDef=tableDef[-1]
  entry=data.frame(name=tableDef, value=rep("", length(tableDef)), 
                   stringsAsFactors=F)
  cat("*****************************************************************\n")
  cat("provide info on sample origin and MS experimental conditions\n\n")
  cat("'Patience is a virtue!' at least in this circumstance :-) \n")
  cat("*****************************************************************\n")
  flush.console()
  entry=fix(entry)
  print(entry)
  
  #construct SQL commend insert
  field.name=paste(as.character(entry$name), collapse=",")
  field.value=paste("$", as.character(entry$name), sep="")
  field.value=paste(as.character(field.value), collapse=",")

  sql.insert=paste("INSERT INTO experiment(",field.name, ")",
                 " VALUES(", field.value, ")", sep="")

  #!! must be data frame
  tmp=as.data.frame(t(entry$value))
  names(tmp)=entry$name

  dbBeginTransaction(db)
  dbGetPreparedQuery(db, sql.insert, bind.data=tmp)
  dbCommit(db)
  
  invisible(dbDisconnect(db))
}#>>>

