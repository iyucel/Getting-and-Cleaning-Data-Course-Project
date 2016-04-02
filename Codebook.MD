This codebook is adopted from the original work as published by the authors and compilers of the UCI Har Dataset.  
The citation for this data set is as follows: 
  Davide Anguita, Alessandro Ghio, Luca Oneto, Xavier Parra and Jorge L. Reyes-Ortiz. Human Activity Recognition on Smartphones using a Multiclass Hardware-Friendly Support Vector Machine. International Workshop of Ambient Assisted Living (IWAAL 2012). Vitoria-Gasteiz, Spain. Dec 2012

**Experiment Description**

>"Carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data." 

>"The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain. See 'features_info.txt' for more details. 

The only modifications this project makes to the existing data is to rename a select number of variables after merging the following files:

- features.txt
- activity_labels.txt
- subject_train.txt
- x_train.txt
- y_train.txt
- subject_test.txt
- x_test.txt
- y_test.txt

The final list of variables, as given in the printed "TidyData.txt" file are as follows:
*  subject                                               
*  activity                                              
*  timeBodyAccelerometer reading-mean()-X                
*  timeBodyAccelerometer reading-mean()-Y                
*  timeBodyAccelerometer reading-mean()-Z                
*  timeBodyAccelerometer reading-std()-X                 
*  timeBodyAccelerometer reading-std()-Y                 
*  timeBodyAccelerometer reading-std()-Z                 
*  timeGravityAccelerometer reading-mean()-X             
*  timeGravityAccelerometer reading-mean()-Y             
*  timeGravityAccelerometer reading-mean()-Z             
*  timeGravityAccelerometer reading-std()-X              
*  timeGravityAccelerometer reading-std()-Y              
*  timeGravityAccelerometer reading-std()-Z              
*  timeBodyAccelerometer readingJerk-mean()-X            
*  timeBodyAccelerometer readingJerk-mean()-Y            
*  timeBodyAccelerometer readingJerk-mean()-Z            
*  timeBodyAccelerometer readingJerk-std()-X             
*  timeBodyAccelerometer readingJerk-std()-Y             
*  timeBodyAccelerometer readingJerk-std()-Z             
*  timeBodyGyroscope Reading-mean()-X                    
*  timeBodyGyroscope Reading-mean()-Y                    
*  timeBodyGyroscope Reading-mean()-Z                    
*  timeBodyGyroscope Reading-std()-X                     
*  timeBodyGyroscope Reading-std()-Y                     
*  timeBodyGyroscope Reading-std()-Z                     
*  timeBodyGyroscope ReadingJerk-mean()-X                
*  timeBodyGyroscope ReadingJerk-mean()-Y                
*  timeBodyGyroscope ReadingJerk-mean()-Z                
*  timeBodyGyroscope ReadingJerk-std()-X                 
*  timeBodyGyroscope ReadingJerk-std()-Y                 
*  timeBodyGyroscope ReadingJerk-std()-Z                 
*  timeBodyAccelerometer readingMagnitude-mean()         
*  timeBodyAccelerometer readingMagnitude-std()          
*  timeGravityAccelerometer readingMagnitude-mean()      
*  timeGravityAccelerometer readingMagnitude-std()       
*  timeBodyAccelerometer readingJerkMagnitude-mean()     
*  timeBodyAccelerometer readingJerkMagnitude-std()      
*  timeBodyGyroscope ReadingMagnitude-mean()             
*  timeBodyGyroscope ReadingMagnitude-std()              
*  timeBodyGyroscope ReadingJerkMagnitude-mean()         
*  timeBodyGyroscope ReadingJerkMagnitude-std()          
*  frequencyBodyAccelerometer reading-mean()-X           
*  frequencyBodyAccelerometer reading-mean()-Y           
*  frequencyBodyAccelerometer reading-mean()-Z           
*  frequencyBodyAccelerometer reading-std()-X            
*  frequencyBodyAccelerometer reading-std()-Y            
*  frequencyBodyAccelerometer reading-std()-Z            
*  frequencyBodyAccelerometer readingJerk-mean()-X       
*  frequencyBodyAccelerometer readingJerk-mean()-Y       
*  frequencyBodyAccelerometer readingJerk-mean()-Z       
*  frequencyBodyAccelerometer readingJerk-std()-X        
*  frequencyBodyAccelerometer readingJerk-std()-Y        
*  frequencyBodyAccelerometer readingJerk-std()-Z        
*  frequencyBodyGyroscope Reading-mean()-X               
*  frequencyBodyGyroscope Reading-mean()-Y               
*  frequencyBodyGyroscope Reading-mean()-Z               
*  frequencyBodyGyroscope Reading-std()-X                
*  frequencyBodyGyroscope Reading-std()-Y                
*  frequencyBodyGyroscope Reading-std()-Z                
*  frequencyBodyAccelerometer readingMagnitude-mean()    
*  frequencyBodyAccelerometer readingMagnitude-std()     
*  frequencyBodyAccelerometer readingJerkMagnitude-mean()
*  frequencyBodyAccelerometer readingJerkMagnitude-std() 
*  frequencyBodyGyroscope ReadingMagnitude-mean()        
*  frequencyBodyGyroscope ReadingMagnitude-std()         
*  frequencyBodyGyroscope ReadingJerkMagnitude-mean()    
*  frequencyBodyGyroscope ReadingJerkMagnitude-std()


If you're still confused about what's going on here, don't hesitate to contact me.  Thanks and godspeed

