source('/datasets/work/af-digiscapesm/work/Ross/SLGA/Shiny/HPC/HPCUtils.R')
debugPath <- '/datasets/work/af-digiscapesm/work/Ross/HPCout'

options(width=200)

args = commandArgs(trailingOnly=T)



getInfo <- function(task){
  
  if(task=='Show_Job_Log'){
   df <- getJobLog(debugPath)
   return(tail(df, 20))
  }else if(task=='Show_Queue'){
   df <- showQ('sea084')
   return(df)
  }else if(task=='Show_Jobs_Info'){
    df <- showJobInfo(ident='sea084', tnum=10, filter='ALL')
    return(df)
  }else if(task=='Show_Jobs_Info_-_Verbose'){
    df <- showVerboseJobInfo(ident='sea084', debugPath=debugPath, tnum=10, filter='ALL')
  return(df)
  }else if(task=='Show_Number_CPUs_In_Use'){
    df <- showCPUs2(ident='sea084')
    return(df)
  }else if(task=='Show_All_Users'){
    df <- showAllUsers()
    return(df)
  }else if(task=='HPC_Load'){
    df <- HPCLoad()
    return(df)
  }
  
  
}

print(getInfo(args[1]))




