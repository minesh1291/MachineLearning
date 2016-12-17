# Variable selection with glmnet (caret package)

The following code block identifies features from your data set.

```R
require(glmnet)
##returns variables from lasso variable selection, use alpha=0 for ridge
ezlasso=function(df,yvar,folds=10,trace=F,alpha=1){
  x<-model.matrix(as.formula(paste(yvar,"~.")),data=df)
  x=x[,-1] ##remove intercept

  glmnet1<-glmnet::cv.glmnet(x=x,y=df[,yvar],type.measure='mse',nfolds=folds,alpha=alpha)

  co<-coef(glmnet1,s = "lambda.1se")
  inds<-which(co!=0)
  variables<-row.names(co)[inds]
  variables<-variables[!(variables %in% '(Intercept)')];
  return( c(yvar,variables));
}
```
VIF (variable inflation factor) to be effective especially when cross-validated.

```R
require(VIF)
require(cvTools);
#returns selected variables using VIF and kfolds cross validation 
ezvif=function(df,yvar,folds=5,trace=F,ignore=c()){
  df=discard(df,ignore);
  f=cvFolds(nrow(df),K=folds);
  findings=list();
  for(v in names(df)){
    if(v==yvar)next;
    findings[[v]]=0; 
  }
  for(i in 1:folds){   
    if(trace) message("fold ",i);
    rows=f$subsets[f$which!=i] ##leave one out 
    y=df[rows,yvar];
    xdf=df[rows,names(df) != yvar]; #remove output var    
    if(trace) say("trying ",i,yvar,nrow(df),length(y)," subsize=",min(200,floor(nrow(xdf))));
    vifResult=vif(y,xdf,trace=trace,subsize=min(200,floor(nrow(xdf))))
    if(trace) print(names(xdf)[vifResult$select]);
        for(v in names(xdf)[vifResult$select]){
      findings[[v]]=findings[[v]]+1; #vote
    }
  }
  findings=(sort(unlist(findings),decreasing = T))    
  if(trace) print(findings[findings>0]); 
  return( c(yvar,names(findings[findings==findings[1]])) )  
}

```

Both of the above ez functions return an vector of variable names. The following code block converts the return values to a formula.

```R
#converts ezvif or ezlasso results into formula
ezformula=function(v,operator=' + '){
  return(as.formula(paste(v[1],'~',paste(v[-1],collapse = operator))))
}
```

[ref.](http://stats.stackexchange.com/)
