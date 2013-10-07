createDB <-
function(dbname="mzDB.db", sql="mzDBSchema.sql"){
  if(file.exists(dbname)) stop("the database already exists!")
  
  if(missing(sql) || is.null(sql)){
    current.dir = getwd()
    setwd(system.file("sql", package = "iontree"))
    sql.def=readLines("mzDBSchema.sql")  
    setwd(current.dir)
  }else{
    if(!file.exists(sql)) stop("provide db schema to create a database")
    #file.show(system.file("sql/mzDBSchema.sql", package="MSn"))
    sql.def=readLines(sql)
  }
    
  #sql.def=readLines(sql)
  sql.def=paste(sql.def, collapse="")
  sql.def=unlist(strsplit(sql.def, ";"))
  
  db=dbConnect(dbDriver("SQLite"), dbname=dbname)
  tmp=sapply(sql.def, function(x) dbGetQuery(db, x))
  cat(dbname, "was created with the following tables:\n")
  cat(dbListTables(db), "\n")
  invisible(dbDisconnect(db))
}#>>>
