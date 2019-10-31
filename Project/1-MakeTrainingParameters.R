get_params = function(sequences){
  
  require(dplyr)
  Figure2Data <- read.csv("C:\\Users\\rimcl\\OneDrive\\School\\Capstone\\github\\Figure2Data-Serizawa.csv")
  FPvector = Figure2Data[,1]
  df1 = Figure2Data[FPvector == 1,]
  df2 =  Figure2Data[FPvector == 0.9,]
  df3 =  Figure2Data[FPvector == 0.75,]
  LST = list(df1,df2,df3)
  
  PARAMS = NULL
  
  
  for (item in LST){
    
    f.p = NULL
    i.n1 = NULL
    i.n2 = NULL
    label = NULL
    s1 = NULL
    s2 = NULL
    s3 = NULL
    
    for (i in 2:dim(item)[1]){
        for (s in 1:dim(sequences)[1]){
          
            label = rbind(label, item$near_boundary[i]&item$near_boundary[i-1])
            i.n1 = rbind(i.n1, item$i_n[i-1])
            i.n2 = rbind(i.n2, item$i_n[i])
            f.p = rbind(f.p, item$f_p[i])
            s1 = rbind(s1,sequences[s,1])
            s2 = rbind(s2,sequences[s,2])
            s3 = rbind(s3,sequences[s,3])
            
        
            label = rbind(label, item$near_boundary[i]&item$near_boundary[i-1])
            i.n1 = rbind(i.n1, item$i_n[i])
            i.n2 = rbind(i.n2, item$i_n[i-1])
            f.p = rbind(f.p, item$f_p[i])
            s1 = rbind(s1,sequences[s,1])
            s2 = rbind(s2,sequences[s,2])
            s3 = rbind(s3,sequences[s,3])
        }
    }
    
    temp = cbind(f.p,i.n1,i.n2,label,s1,s2,s3)
    PARAMS = rbind(PARAMS, temp)
  }
  colnames(PARAMS) <- c("f_p","i_n1","i_n2","label","s1","s2","s3")
  PARAMS
  
}