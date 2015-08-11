```{r}

#---summarized table

library(xtable)
models <- c("observations", "P(G1|obs)", "P(G2|obs)", "P(G3|obs)", "Predicted Group")
LDA1 <- c("1st of Group1", round(football11lda$posterior[1,1], digits=4), 
          round(football11lda$posterior[1,2], digits=4), round(football11lda$posterior[1,3], 
                                                               digits=4), "Predicted Group")
LDA2 <- c( "1st of Group2", round(football11lda$posterior[31,1], digits=4), 
           round(football11lda$posterior[31,2], digits=4), round(football11lda$posterior[31,3], 
                                                                 digits=4), "Predicted Group")
LDA3 <- c( "1st of Group3", round(football11lda$posterior[61,1], digits=7), 
           round(football11lda$posterior[61,2], digits=4), round(football11lda$posterior[61,3], 
                                                                 digits=4), "Predicted Group")
QDA1 <- c( "1st of Group1", football11lda$posterior[1,1], football11lda$posterior[1,2], 
           football11lda$posterior[1,3], "Predicted Group")
QDA2 <- c( "1st of Group2", football11lda$posterior[1,1], football11lda$posterior[1,2], 
           football11lda$posterior[1,3], "Predicted Group")
QDA3 <- c( "1st of Group3", football11lda$posterior[1,1], football11lda$posterior[1,2], 
           football11lda$posterior[1,3], "Predicted Group")
KDA1 <- c( "1st of Group1", football11lda$posterior[1,1], football11lda$posterior[1,2], 
           football11lda$posterior[1,3], "Predicted Group")
KDA2 <- c( "1st of Group2", football11lda$posterior[1,1], football11lda$posterior[1,2], 
           football11lda$posterior[1,3], "Predicted Group")
KDA3 <- c( "1st of Group3", football11lda$posterior[1,1], football11lda$posterior[1,2], 
           football11lda$posterior[1,3], "Predicted Group")
nearestneighbor1 <- c("1st of Group1", football11lda$posterior[1,1], 
                      football11lda$posterior[1,2], 
                      football11lda$posterior[1,3], "Predicted Group")
nearestneighbor2 <- c( "1st of Group2", football11lda$posterior[1,1], 
                       football11lda$posterior[1,2], 
                       football11lda$posterior[1,3], "Predicted Group")
nearestneighbor3 <- c( "1st of Group3", football11lda$posterior[1,1], 
                       football11lda$posterior[1,2], 
                       football11lda$posterior[1,3], "Predicted Group")
table <-rbind(models, LDA1, LDA2, LDA3, QDA1, QDA2, QDA3, KDA1, KDA2, KDA3, nearestneighbor1, 
              nearestneighbor2, nearestneighbor3)
```