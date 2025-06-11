library(bridgeclient) # https://github.com/philerooski/bridgeclient
email_in <- ''
pass_in <- ''

## Login into Bridge
bridgeclient::bridge_login('sage-mpower-2', email_in, pass_in) # mpower_2
# list of all participants given a studysd
get_all_participants <- function(study_id, offset_in){
  response <- bridgeclient:::bridgeGET(
    glue::glue("/v5/studies/{study_id}/enrollments?includeTesters=true&offsetBy={offset_in}&pageSize=100"))
  return(response)
}

## get all studies
studies_list <- c('at-home-pd',
                  'at-home-PD2',
                  'sage',
                  'stepwise',
                  'test',
                  'Udall-superusers',
                  'udall-umn')

## table to hold all ids
ids.tbl.all <- NULL

## get Ids from bridge
for(study_in in studies_list){
  print(study_in)
  
  hybrid_study_participants <- tryCatch(get_all_participants(study_in, offset_in = 0),
                                        error = function(e){
                                          list()
                                        }) 
  
  
  if(length(hybrid_study_participants)){
    
    ids_tbl <- lapply(hybrid_study_participants$items, function(x){
      
      externalId = tryCatch({x$externalId}, error = function(e){NA})
      userId = x$participant$identifier
      enrolledOn = tryCatch({x$enrolledOn}, error = function(e){NA})
      withdrawnOn = tryCatch({x$withdrawnOn}, error = function(e){NA})
      
      if(is.null(externalId)){
        externalId <- NA
      }
      
      if(is.null(enrolledOn)){
        enrolledOn <- NA
      }
      
      if(is.null(withdrawnOn)){
        withdrawnOn <- NA
      }
      
      data.frame(externalId =  externalId,
                 userId = userId,
                 enrolledOn = enrolledOn,
                 withdrawnOn = withdrawnOn)
    }) %>% data.table::rbindlist(fill = T)
    # this is just the first 100 ids, there can be more. 
    # Look at the pageSize metric in the get_all_participants function
    # 100 is the max number of records we can request at once from bridge?
    # So we have to offset everytime
    
    toggle_switch = TRUE
    current_offset = 100
    while(toggle_switch){
      hybrid_study_participants <- tryCatch(get_all_participants(study_in,
                                                                 offset = current_offset),
                                            error = function(e){
                                              list()
                                            })  
      
      ids_tbl_new <- lapply(hybrid_study_participants$items, function(x){
        
        externalId = tryCatch({x$externalId}, error = function(e){NA})
        userId = x$participant$identifier
        enrolledOn = tryCatch({x$enrolledOn}, error = function(e){NA})
        withdrawnOn = tryCatch({x$withdrawnOn}, error = function(e){NA})
        
        if(is.null(externalId)){
          externalId <- NA
        }
        
        if(is.null(enrolledOn)){
          enrolledOn <- NA
        }
        
        if(is.null(withdrawnOn)){
          withdrawnOn <- NA
        }
        
        data.frame(externalId =  externalId,
                   userId = userId,
                   enrolledOn = enrolledOn,
                   withdrawnOn = withdrawnOn)
      }) %>% data.table::rbindlist(fill = T)
      
      if(nrow(ids_tbl_new)){
        ids_tbl <- ids_tbl %>% 
          dplyr::full_join(ids_tbl_new) %>% 
          unique()
        
        current_offset <- current_offset + 100
      }else{
        toggle_switch <- FALSE
      }
      
    }
  }
  
  if(is.null(ids.tbl.all)){
    ids.tbl.all <- ids_tbl %>% 
      dplyr::mutate(study = study_in)
  }else{
    ids.tbl.all <- ids.tbl.all %>% 
      dplyr::full_join(ids_tbl %>% 
                         dplyr::mutate(study = study_in))
  }
  
}

# add dataGroups
ids.tbl.all.meta <- ids.tbl.all %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(dataGroups = tryCatch(list(get_participant(user_id = userId)$dataGroups %>% unlist()),
                                      error = function(e){NA})) %>% 
  dplyr::ungroup()

# add healthCode
ids.tbl.all.meta <- ids.tbl.all.meta %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(healthCode = tryCatch(get_participant(user_id = userId)$healthCode,
                                      error = function(e){NA})) %>% 
  dplyr::ungroup()


# reorder columns
aa <- ids.tbl.all.meta %>% 
  dplyr::select(healthCode,
                externalId,
                studyId = study,
                enrollmentDate = enrolledOn,
                withdrawnDate = withdrawnOn,
                dataGroups) %>% 
  as.data.frame()

# convert dataGroups into a string text
aa <- aa %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(dataGroups = paste0((dataGroups), collapse = ',')) %>% 
  dplyr::ungroup()

## remove existing rows
results <- synTableQuery("select * from syn60228734")
deleted <- synDelete(results)

# upload table to synapse
# table <- synBuildTable("mPower Enrollments Bridge", 'syn12030321', aa)
# table$schema
current.table.table <- synapser::Table('syn60228734', aa)
table <- synStore(current.table.table)
