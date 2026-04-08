get_edi_table <- function(identifier, entity_seq) {
  scope <- "knb-lter-hbr"
  
  revision <- list_data_package_revisions(scope, identifier, filter = "newest")
  packageId <- paste(scope, identifier, revision, sep = ".")
  
  pc <- read_data_package(packageId)
  tableurl <- as.character(pc[entity_seq])  # coerce to plain string
  
  dt <- read.csv(tableurl)
  return(dt)
}
