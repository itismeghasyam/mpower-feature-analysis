library(tidyverse)
library(synapser)
library(bridgeclient) # https://github.com/philerooski/bridgeclient
email_in <- ''
pass_in <- ''

## Login into Bridge (app)
bridgeclient::bridge_login('sage-mpower-2', email_in, pass_in) # mpower_2

get_all_participants_app <- function(offset_in){
  response <- bridgeclient:::bridgeGET(
    glue::glue("/v3/participants?includeTesters=true&offsetBy={offset_in}&pageSize=100"))
  return(response)
}


hybrid_study_participants <- tryCatch(get_all_participants_app(offset_in = 0),
                                      error = function(e){
                                        list()
                                      }) 

ids_tbl_new <- NULL

if(length(hybrid_study_participants)){
  
  ids_tbl <- lapply(hybrid_study_participants$items, function(x){
    x
  }) 
  
  toggle_switch = TRUE
  current_offset = 100
  while(toggle_switch){
    hybrid_study_participants <- tryCatch(get_all_participants_app(offset = current_offset),
                                          error = function(e){
                                            list()
                                          })  
    
    ids_tbl_new <- lapply(hybrid_study_participants$items, function(x){
      x
    }) 
    
    
    if(length(ids_tbl_new)){
      ids_tbl <- c(ids_tbl, ids_tbl_new)
      
      current_offset <- current_offset + 100
    }else{
      toggle_switch <- FALSE
    }
    
  }
}

## get the follwing items from ids tbl
# healthCode, externalId, studyId, enrollmentDate, withdrawnDate, dataGroups

## Get all ids first
all.ids <- lapply(ids_tbl, function(x){
  x$id
}) %>% unlist() %>% as.data.frame() %>% 
  `colnames<-`(c('userId'))

## Get all participant details using bridgeclient::get_participant()
all.participant.details <- lapply(ids_tbl, function(x){
  get_participant(user_id = x$id)
})


## Convert all participant list into something that is readable
aa <- lapply(all.participant.details, function(participant){
  # userId <- participant$id
  healthCode <- participant$healthCode
  dataGroups <- paste0((participant$dataGroups), collapse = ',')
  enrollments <- paste0(sort(names(participant$enrollments)), collapse = ',')
  createdOn <- participant$createdOn
  status <- participant$status
  
  data.frame(healthCode = healthCode,
             dataGroups = dataGroups,
             enrollments =  enrollments,
             createdOn = createdOn,
             status = status)
  
}) %>% data.table::rbindlist(fill = T) %>% 
  ungroup() %>% 
  as.data.frame()

## remove existing rows
results <- synTableQuery("select * from syn60228793")
deleted <- synDelete(results)

# update the new table
# # table <- synBuildTable("mPower app All Participants Bridge", 'syn12030321', aa)
# # table$schema
current.table.table <- synapser::Table('syn60228793', aa)
table <- synStore(current.table.table)

