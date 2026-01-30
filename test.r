library(jsonlite)
library(httr)


schema_response <- GET("https://ncpif.org/ncsb.schema.json")

schema_data <- fromJSON(rawToChar(schema_response$content))
props <- schema_data$properties

get_data <- function(d){
  for (i in names(props)){
    switch(i,
      type = {

      },
      array = {

      },
      string = {

      },
      number = {

      },
      properties = {
        
      }

    )
    print(i)
    print(d[i])
    print(typeof(d[i]))
  }
}

get_data(schema_data["$defs"])
get_data(props)

print(schema_data$properties)