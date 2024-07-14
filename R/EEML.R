#' @title Selection of Superior Models Using MSC Algorithm
#' @param df Dataframe of predicted values of models with first column as actual values
#' @param Alpha Confidence level of MCS tests
#' @param K Resampling length
#' @import stats MCS
#' @return
#' \itemize{
#'   \item SelModel: Name of the selected models
#' }
#' @export
#' @examples
#' library("EEML")
#' Actual<- as.ts(rnorm(200,100,50))
#' Model1<- as.ts(rnorm(200,100,50))
#' Model2<- as.ts(rnorm(200,100,50))
#' Model3<- as.ts(rnorm(200,100,50))
#' Model4<- as.ts(rnorm(200,100,50))
#' Model5<- as.ts(rnorm(200,100,50))
#' DF <- cbind(Actual, Model1,Model2,Model3,Model4,Model5)
#' SelModel<-ModelSel(df=DF, Alpha=0.2, K=NULL)
#'
#' @references
#' \itemize{
#'\item Paul, R.K., Das, T. and Yeasin, M., 2023. Ensemble of time series and machine learning model for forecasting volatility in agricultural prices. National Academy Science Letters, 46(3), pp.185-188.
#'\item Yeasin, M. and Paul, R.K., 2024. OptiSembleForecasting: optimization-based ensemble forecasting using MCS algorithm and PCA-based error index. The Journal of Supercomputing, 80(2), pp.1568-1597.
#'\item  Hansen PR, Lunde A, Nason JM (2011). The model confidence set. Econometrica, 79(2), 453-497
#' }
ModelSel<-function(df, Alpha, K){
  colnames(df)<-c("Actual",colnames(df)[-1])
  Loss<-abs(df[,-1]-df[,1])
  MCS<-MCSprocedure(Loss, alpha = 0.5,B = 5000,statistic = "Tmax" )
  SelModel<-MCS@Info$model.names
  return(SelModel)
}


#' @title Selection of Superior Models Using MSC Algorithm
#' @param ModelSel Dataframe of predicted values of selected models with first column as actual values
#' @param Optim Optimisation technique
#' @import stats WeightedEnsemble
#' @return
#' \itemize{
#'   \item WeightEn: Ensemble weight of the candidate models
#' }
#' @export
#' @examples
#' \donttest{
#' library("EEML")
#' Actual<- as.ts(rnorm(200,100,50))
#' Model1<- as.ts(rnorm(200,100,50))
#' Model2<- as.ts(rnorm(200,100,50))
#' Model3<- as.ts(rnorm(200,100,50))
#' DF <- cbind(Actual, Model1,Model2,Model3)
#' SelModel<-Weight(ModelSel=DF,Optim="PSO")
#' }
#' @references
#' \itemize{
#'\item Paul, R.K., Das, T. and Yeasin, M., 2023. Ensemble of time series and machine learning model for forecasting volatility in agricultural prices. National Academy Science Letters, 46(3), pp.185-188.
#'\item Yeasin, M. and Paul, R.K., 2024. OptiSembleForecasting: optimization-based ensemble forecasting using MCS algorithm and PCA-based error index. The Journal of Supercomputing, 80(2), pp.1568-1597.
#' }
Weight<-function(ModelSel,Optim="PSO"){
  DF<-as.data.frame(ModelSel)
  WeightEn<-WeightedEnsemble(df=DF, Method = Optim)
}

#' @title Ensemble Explainable Machine Learning Models
#' @param df List of dataframes containing various explainable scores for each model
#' @param Weight Ensemble weights of the models (from weight function)
#' @import stats topsis
#' @return
#' \itemize{
#'   \item ImpScore: Final variable important score of EEML model
#' }
#' @export
#' @examples
#' \donttest{
#' library("EEML")
#' df1<- as.data.frame(matrix(rnorm(50) , nrow = 10) )
#' df2<- as.data.frame(matrix(rnorm(50) , nrow = 10) )
#' df3<- as.data.frame(matrix(rnorm(50) , nrow = 10) )
#' rownames(df1)<- rownames(df2)<-rownames(df3)<-paste0("Var", seq(1,10,1))
#' colnames(df1)<- colnames(df2)<-colnames(df3)<-paste0("Exp", seq(1,5,1))
#' DF<- list(df1, df2, df3)
#' EEML<-EEML(df=DF,Weight=NULL)
#' }
#' @references
#' \itemize{
#'\item Paul, R.K., Das, T. and Yeasin, M., 2023. Ensemble of time series and machine learning model for forecasting volatility in agricultural prices. National Academy Science Letters, 46(3), pp.185-188.
#'\item Yeasin, M. and Paul, R.K., 2024. OptiSembleForecasting: optimization-based ensemble forecasting using MCS algorithm and PCA-based error index. The Journal of Supercomputing, 80(2), pp.1568-1597.
#' }
EEML<-function(df, Weight){
  Data<-df
  normalized <- function(x) {
    x<-abs(x)
    (x - min(x)) / (max(x) - min(x))
  }
  FScore<-NULL
  for(i in 1:length(Data)){
    data<-as.data.frame(Data[[i]])
    norm_data<-lapply(Data, normalized)
    norm_data<-as.data.frame(norm_data)
    weight <- rep(1, (ncol(data)))
    d <-as.data.frame((data))
    d<-as.matrix(d)

    w <- weight
    direct <- rep("+",length(w))
    Top<-topsis(d, w, direct)
    FScore<-cbind(FScore, Top[,2])
  }

  if(is.null(Weight)){
    weight1 <- rep(1, (ncol(FScore)))
  }else{
    weight1 <- Weight
  }
  d <-as.data.frame((FScore))
  d<-as.matrix(d)
  w <- weight1
  direct <- rep("+",length(w))
  Top<-topsis(d, w, direct)
  Final<-cbind(FScore, Top[,2])
  ImpScore<-as.data.frame(Final)
  colnames(ImpScore)<-c(paste0("Model", seq(1,length(Data))), "EEML")
  rownames(ImpScore)<-rownames(Data[[1]])
  return(ImpScore)
}
