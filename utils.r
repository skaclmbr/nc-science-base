#############################################################################
# Functions for Connecting to Databases



#############################################################################
# MongoDB
# this is a read only account
HOST = "cluster0-shard-00-00.rzpx8.mongodb.net:27017"
source("config.r")





#############################################################################
## WILDLIFE ACTION PLAN

## Mongo Connection Parameters
WAP_URI = sprintf(
  paste0("mongodb://%s:%s@%s/%s?authSource=admin&replicaSet=",
    "atlas-3olgg1-shard-0&readPreference=primary&ssl=true"),
  USER,
  PASS,
  HOST,
  "conservation_connections")

ncsb <- mongo(
    "nc_science_base",
    url = WAP_URI,
    options = ssl_options(weak_cert_validation = T)
)



## Functions
getEntityList <- function() {
    pipeline <- paste0(
            '[',
            ' { "$project": { "title" : 1 }},',
            ' { "$sort": { "title" : 1 }}',
            ']'
        )
    
    list <- ncsb$aggregate(pipeline)
    list$title
}

getEntity <- function(key) {

    result <- ncsb$find(
        sprintf(paste0('{"id" : "%s" }'), key),
        limit = 1
    )

    # list <- mv$aggregate(pipeline)
    # list$title
}