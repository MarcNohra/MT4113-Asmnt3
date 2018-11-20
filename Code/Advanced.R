testFunc <- function(func, nrep, param_list, ret_vals) {
  browser()
  
  n_param<-length(param_list)                                                       # number of parameters
  dim_vec<-numeric(n_param) 
  for(i in 1:n_param){dim_vec[i]<-length(param_list[[i]])}                          # construct vector with grid dimensions
  param_names<-names(param_list)                                                    # names of parameters
  for(i in 1:n_param)(assign(paste(param_names[i],"_grid",sep=""),param_list[[i]])) # create grid for each parameter
  
  subm_param<-paste(param_names,"=",param_names,",", sep="",collapse="")
  subm_param<-substr(subm_param,1,(nchar(subm_param)-1))
  #if(mode=="longitudinal"){
  eval(parse(text=paste("func2<-function(","aux,",subm_param,"){func(",subm_param,")}")))
  #}  
  #if(mode=="cross-sectional"){func2<-function(params){eval(parse(text=paste("func(",paste(params, collapse=","),")", collapse="")))}}
  
  grid_size<-prod(dim_vec)  
  cat(paste("Grid of ",grid_size, " parameter constellations to be evaluated.","\n","\n"))
}

func<-function(n,loc,scale){
   sample<-rnorm(n, loc, scale)
   stat<-sqrt(n)*mean(sample)/sd(sample)
   decision<-abs(stat)>1.96
   return(list("decision"=decision))
}
func(3, 1, 4)

param_list=list("n"=100, "loc"=seq(0,1,0.2), "scale"=c(1,2))

param_names<-names(param_list)
test_run<-eval(parse(text=paste("func(",paste(param_names,"=param_list[[",1:length(param_names),"]][1]", sep="", collapse=","),")", collapse="", sep="")))
ret_vals<-gsub(" ", "_", names(test_run))

testFunc(func = func, nrep = 10, param_list = param_list, ret_vals = )














