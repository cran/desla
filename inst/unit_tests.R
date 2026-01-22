test_desla <- function(){ # reference files were created by version 0.3.0 on 21/02/2024
  res_folder <- system.file("unit_test_reference_files", package="desla", mustWork = TRUE)
  Ts <- c(50,100)
  Ns <- c(50,100)
  H <- 1:10
  progress_bar <- FALSE
  tests <- c("default", "alpha001", "penalizeH", "Rq", "demean", "scale", "parallel", "threads3", "PI_constant12", "LRV_bandwidth")

  mmr <- matrix(data = NA, nrow=10, ncol=4, byrow = TRUE)
  rownames(mmr) <- tests
  colnames(mmr) <- c("T_50N_50", "T_50N_100", "T_100N_50", "T_100N_100")

  for(i in 1:length(tests)){
    set.seed(i)
    for(t in 1:length(Ts)){
      for(n in 1:length(Ns)){
        T_ <- Ts[t]
        N <- Ns[n]
        tcn <- paste0("desla_",tests[i],"_","T_",T_,"_N_",N)

        X <- matrix(rnorm(T_*N), nrow=T_)
        y <- X[,1:10] %*% 1:10 + rnorm(T_)

        if(tests[i]=="default"){
          d <- desla::desla(X=X, y=y, H=H, progress_bar=progress_bar)
        }else if(tests[i]=="alpha001"){
          d <- desla::desla(X=X, y=y, H=H, progress_bar=progress_bar, alphas = 0.01)
        }else if(tests[i]=="penalizeH"){
          d <- desla::desla(X=X, y=y, H=H, progress_bar=progress_bar, penalize_H = FALSE)
        }else if(tests[i]=="Rq"){
          d <- desla::desla(X=X, y=y, H=H, progress_bar=progress_bar, R = matrix(1/(1:10), nrow = 1, ncol = 10), q = 10)
        }else if(tests[i]=="demean"){
          d <- desla::desla(X=X, y=y, H=H, progress_bar=progress_bar, demean = FALSE)
        }else if(tests[i]=="scale"){
          d <- desla::desla(X=X, y=y, H=H, progress_bar=progress_bar, scale = FALSE)
        }else if(tests[i]=="parallel"){
          d <- desla::desla(X=X, y=y, H=H, progress_bar=progress_bar, parallel = FALSE)
        }else if(tests[i]=="threads3"){
          d <- desla::desla(X=X, y=y, H=H, progress_bar=progress_bar, threads = 3)
        }else if(tests[i]=="PI_constant12"){
          d <- desla::desla(X=X, y=y, H=H, progress_bar=progress_bar, PI_constant = 1.2)
        }else if(tests[i]=="LRV_bandwidth"){
          d <- desla::desla(X=X, y=y, H=H, progress_bar=progress_bar, LRV_bandwidth = c(1/2, 1/3))
        }else{
          stop("invalid test")
        }

        d_ref<-readRDS(file=paste0(res_folder,"/",tcn,".rds"))
        mmr[i, 2*(t-1)+n] <- identical(d, d_ref)
      }
    }
  }
  if(sum(mmr)!=40){warning("something is not matching")}
  return(mmr)
}

test_desla()
