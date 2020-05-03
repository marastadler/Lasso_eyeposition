
  
Transform_data <- function(path = 'Hochbegabte-Critical-Stimuli-Export/Vid18_car_cornfield.csv'){
    # Zunächst wurde die txt-Datei in eine csv-Datei(;-sep) in Excel konvertiert.
    
    raw_data <- read.csv(path, header = TRUE, sep = ';', dec =',',
                         col.names = c('RecordingTime [ms]', 'Time of Day [h:m:s:ms]', 'Trial',
                                       'Stimulus', 'Participant', 'Category Right', 
                                       'Point of Regard Right X [px]', 'Point of Regard Right Y [px]'))
    
    # str(raw_data)
    # 
    # table(raw_data$Participant)
    # Koordinaten von factor zu numeric konvertieren:
    Rx <- which(colnames(raw_data) == 'Point.of.Regard.Right.X..px.')
    raw_data[, Rx] <- as.numeric(sub(",", ".", raw_data[, Rx], fixed = TRUE))
    Ry <- which(colnames(raw_data) == 'Point.of.Regard.Right.Y..px.')
    raw_data[, Ry] <- as.numeric(sub(",", ".", raw_data[, Ry], fixed = TRUE))
    colnames(raw_data)[c(Rx, Ry)] <- c('Rx', 'Ry')
    # head(raw_data)
    
    
    
    ## Anteil Blickkoordinate an Frame:
    
    # Es soll jeweils die erste Blickkoordinate, welche komplett auf dem jeweiligen
    # Frame stattfindet/liegt extrahiert werden. Die übrigen werden dann nicht mehr
    # berücksichtigt, sodass am Ende jedem Frame eine Koordinate pro Person
    # zugeordnet wird.
    
    # Die Videos sind mit jeweils 25 Hz aufgezeichnet und der Eyetracker nimmt die
    # Koordinaten mit 60 Hz auf. D.h. 2.4 Blickbewegungen pro Frame:
    
    # Entfernen der Separator-Zeilevor jedem Participant
    ind_na <- which(is.na(raw_data$Rx) & is.na(raw_data$Ry))
    raw_data <- raw_data[-ind_na, ]
    # table(raw_data$Participant)
    
    # Anteil am "ersten" Frame
    # prop <- c(1, 1, 0.4, 1, 0.8,  1, 1, 0.2, 1, 0.6, 1, 1)
    # wir sind nur an der ersten 1 eines Frames interessiert
    # d.h. man nimmt die 1., 4., 6., 9., 11. und 13. Blickkoordnate usw.
    # nach 12 wiederholt sich die Reihenfolge
    # relevante:
    relev <- c(1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0)
    Relev <- c(rep(relev, ceiling(max(table(raw_data['Participant']))/
                                    length(relev))))
    
    Eye_Positions <- raw_data[, c('Participant', 'Rx', 'Ry')]
    
    # inwieweit ist es ok zu ignorieiren, dass die Anzahl der Blickkordinaten um bis zu 4 variieren?
    Relev_ges <- c()
    tab_participants <- table(Eye_Positions[,'Participant'])
    
    for(i in names(tab_participants)){
      
      Relev_ges <- c(Relev_ges, 
                     Relev[1:nrow(Eye_Positions[Eye_Positions$Participant == i,])])
      
    }
    
    index_ges <- which(Relev_ges == 1)
    
    Eyepositions <- Eye_Positions[index_ges, ]
    #head(Eyepositions)
    
    tab_participants1 <- table(Eyepositions$Participant)
    
    # Betrachte nur Frames für welche für alle Participants Blickbewegungen vorliegen:
    
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
    # ind_delete
    if(!is.null(ind_delete)){
      Eyepositions <- Eyepositions[-ind_delete, c('Participant', 'Rx', 'Ry')]}
    
    if(is.null(ind_delete)){
      Eyepositions <- Eyepositions[ , c('Participant', 'Rx', 'Ry')]}
    
    # Nun werden jedem Probanden 169 Blickkordinaten zugeordnet:
    tab <- table(Eyepositions$Participant)
    # Löschen der Spalte $Participant$ und rechne $x-258$, da Monitorbreite 1920 und Framebreite 1350 (d.h. $(1920 - 1359)/2=258$):
    
    str(Eyepositions)
    Eyepositions <- Eyepositions[, c('Rx', 'Ry')]
    Eyepositions$Rx <- Eyepositions$Rx - 258
    Eyepositions <- t(Eyepositions)
    
    # Struktur wir Datansatz Coutrot & Guyader:
    EyePos_exemple <- array(unlist(Eyepositions), dim =  c(2, tab[1], length(tab)))
    # str(EyePos_exemple)
    #save(EyePos_exemple, file = 'EyePos_exemple_car.corn.Rda')
    
    return(EyePos_exemple)
  }


