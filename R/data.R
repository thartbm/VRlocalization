

getCurrentData <- function() {
  
  Reach::downloadOSFdata(repository = 'vzds5',
                         filelist = list('/'=c( 'hand_30.csv',
                                                'hand_60.csv', 
                                                'pen_aligned.csv',
                                                'pen_rotated.csv',
                                                'omnibus_hand.csv')),
                         folder = 'data',
                         overwrite = TRUE)

}


