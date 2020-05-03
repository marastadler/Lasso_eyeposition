#' Transform_data for data processing
  
Transform_data <- function(path = 'Data/Vid18_car_cornfield.csv') {
    
    raw_data <- read.csv(path, header = TRUE, sep = ';', dec =',',
                         col.names = c('RecordingTime [ms]', 'Time of Day [h:m:s:ms]', 'Trial',
                                       'Stimulus', 'Participant', 'Category Right', 
                                       'Point of Regard Right X [px]', 'Point of Regard Right Y [px]'))
    
   
    Rx <- which(colnames(raw_data) == 'Point.of.Regard.Right.X..px.')
    raw_data[, Rx] <- as.numeric(sub(",", ".", raw_data[, Rx], fixed = TRUE))
    Ry <- which(colnames(raw_data) == 'Point.of.Regard.Right.Y..px.')
    raw_data[, Ry] <- as.numeric(sub(",", ".", raw_data[, Ry], fixed = TRUE))
    colnames(raw_data)[c(Rx, Ry)] <- c('Rx', 'Ry')
    
    ind_na <- which(is.na(raw_data$Rx) & is.na(raw_data$Ry))
    raw_data <- raw_data[-ind_na, ]
    
    # first viewing coordinate that remains completely on the respective frame is selected:
    relev <- c(1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0)
    Relev <- c(rep(relev, ceiling(max(table(raw_data['Participant']))/
                                    length(relev))))
    
    Eye_Positions <- raw_data[, c('Participant', 'Rx', 'Ry')]
    
    Relev_ges <- c()
    tab_participants <- table(Eye_Positions[,'Participant'])
    
    for(i in names(tab_participants)){
      
      Relev_ges <- c(Relev_ges, 
                     Relev[1:nrow(Eye_Positions[Eye_Positions$Participant == i,])])
      
    }
    
    index_ges <- which(Relev_ges == 1)
    
    Eyepositions <- Eye_Positions[index_ges, ]
    
    tab_participants1 <- table(Eyepositions$Participant)
    
    # Consider only frames for which eye movements are available for all participants:
    
    Eyepositions$index <- 1:nrow(Eyepositions)
    ind_delete <- c()
    for(i in names(tab_participants1)){
      if(nrow(Eyepositions[Eyepositions$Participant == i, ]) >
         min(tab_participants1)){
        Eyepositions_i <- Eyepositions[Eyepositions$Participant == i, ]
        ind_delete <- c(ind_delete, Eyepositions_i$index[
          (min(tab_participants1) + 1) :nrow(Eyepositions_i)])
      }
    }

  if(!is.null(ind_delete)){
      Eyepositions <- Eyepositions[-ind_delete, c('Participant', 'Rx', 'Ry')]}
    
    if(is.null(ind_delete)){
      Eyepositions <- Eyepositions[ , c('Participant', 'Rx', 'Ry')]}
    
    tab <- table(Eyepositions$Participant)
    
    str(Eyepositions)
    Eyepositions <- Eyepositions[, c('Rx', 'Ry')]
    Eyepositions$Rx <- Eyepositions$Rx - 258
    Eyepositions <- t(Eyepositions)
    
    EyePos_exemple <- array(unlist(Eyepositions), dim =  c(2, tab[1], length(tab)))
    # str(EyePos_exemple)
    # save(EyePos_exemple, file = 'EyePos_example_car.corn.Rda')
    
    return(EyePos_exemple)
  }


