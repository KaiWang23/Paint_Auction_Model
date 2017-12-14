```{r}
load("raw_valid.Rdata")
load("paintings_test.Rdata")
load("paintings_train.Rdata")
load("raw_test.Rdata")
load("paintings_validation.Rdata")
clean.new = function(df, NA.omit=F){
   df = df %>% 
     select(-c(sale, price, count, subject, winningbidder, authorstandard, authorstyle, author, Height_in, Width_in, Surface_Rect, Diam_in, winningbiddertype, Surface_Rnd,material,materialCat, Interm)) %>%
     mutate(lot = as.numeric(lot)) %>%
     mutate(mat=as.character(mat))%>%
     mutate(mat_recode = ifelse(mat %in% c("a", "bc", "c"), "metal",
                          ifelse(mat %in% c("al", "ar", "m"), "other",
                                ifelse(mat %in% c("co", "bt", "t"), "canvas",
                                       ifelse(mat %in% c("p", "ca"), "paper",
                                              ifelse(mat %in% c("b"), "wood",
                                                     ifelse(mat %in% c("o", "e", "v"), "other",
                                                            ifelse(mat %in% c("n/a", ""), "na",
                                                                   "other")))))))) %>%
     mutate(school_pntg=as.character(school_pntg))%>%
     mutate(school_pntg_recode = ifelse(school_pntg %in% c("A", "G", "S"), "other",school_pntg)) %>%
     mutate(origin_cat=ifelse(origin_cat %in% c("X","S"),"O",origin_cat))%>%
     mutate(SurfaceNA = ifelse(is.na(df$Surface), 1, 0))%>%
     mutate(nfigures=as.numeric(nfigures))%>%
     mutate(Surface=log(Surface+1))%>%
     mutate(endbuyer=as.character(endbuyer))%>%
     mutate(endbuyer = ifelse(endbuyer == "", "NA", endbuyer)) %>% 
     mutate(Shape=as.character(Shape))%>%
   mutate(shape_recode = ifelse(Shape == "", "Other",
                               ifelse(Shape == "ovale", "oval",
                                      ifelse(Shape == "ronde", "round",
                                             ifelse(Shape %in% c("octogon","octagon"), "Other", Shape)))))%>%
     select(-c(mat,school_pntg, Shape))%>%
     mutate(type_intermed=as.character(type_intermed))%>%
     mutate(type_intermed=ifelse(type_intermed == "EB","B",type_intermed))
   
   
   factornames=colnames(df)[!colnames(df) %in% c("logprice","lot","position","Surface","nfigures")] 
   
   
   df[factornames] = lapply(df[factornames], factor)

   df$Surface[is.na(df$Surface)] = 0
  
  
   if(NA.omit==T){
      df = na.omit(df)
   }


   #df=df[vapply(df, function(x) length(unique(x)) > 1, logical(1L))]
   
  return(df) 
}

rmse <- function(y, ypred) {
rmse <- sqrt(mean((y - ypred)^2))
return(rmse)
}

train = clean.new(paintings_train) %>% select(-lot)
test = clean.new(paintings_test)%>% select(-lot)
test.true = clean.new(test.raw) %>% select(-lot)
valid.true = clean.new(valid) %>% select(-lot)
validation = clean.new(paintings_validation)%>% select(-lot)
```

```{r}
#lasso
library(glmnet)
x = model.matrix(logprice~., train)
y= train$logprice
grid = 10^(seq(10,-2, length=100))

lasso.mod= glmnet(x,y,alpha=1, lambda=grid)
#plot(lasso.mod)
set.seed(1)

cv.out = cv.glmnet(x,y, alpha=1)
#plot(cv.out)
bestlam=cv.out$lambda.min
lasso.mod2 = glmnet(x,y,alpha=1, lambda=bestlam)

test=test%>%
  mutate(logprice=1)
x.test = model.matrix(logprice~.,test)
pred=predict(lasso.mod2, x.test)

pred=exp(pred)


test.rmse = rmse(exp(test.true$logprice), as.numeric(pred))

###############


train1=train%>%
  mutate(logprice=1)
x.test = model.matrix(logprice~.,train1)
pred=predict(lasso.mod2, x.test)

pred=exp(pred)

train.rmse = rmse(exp(as.numeric(paintings_train$logprice)), as.numeric(pred))


#############




validation=validation%>%
  mutate(logprice=1)
x.test = model.matrix(logprice~.,validation)
pred=predict(lasso.mod2, x.test)

pred=exp(pred)


valid.rmse = rmse(exp(as.numeric(valid.true$logprice)), as.numeric(pred))

```




```{r}
library(caret)

cv.ctrl <- trainControl(method = "repeatedcv", repeats = 3,number = 3,
                        classProbs = TRUE,
                        allowParallel=T)

xgb.grid <- expand.grid(nrounds = 100,
                        eta = seq(0.01, 0.21, by = 0.05),
                        lambda = seq(0.1, 1, by = 0.1),
                        alpha = 1)


set.seed(45)
xgb_tune <-train(logprice~.,
                 data=train,
                 trControl = cv.ctrl,
                 method="xgbLinear",
                 tuneGrid=xgb.grid,
                 gamma = 1,
                 nthread = 8
    )

predictions.test = predict(xgb_tune, newdata = test) %>% exp()
predictions.valid = predict(xgb_tune, newdata = validation) %>% exp()

predictions.train = predict(xgb_tune) %>% exp()

train.rmse1 = rmse(exp(as.numeric(paintings_train$logprice)), as.numeric(predictions.train))


test.rmse1 = rmse(exp(test.true$logprice), as.numeric(predictions.test))

valid.rmse1 = rmse(exp(as.numeric(valid.true$logprice)), as.numeric(predictions.valid))

```


```{r}
# model1 = lm(logprice~Surface + dealer + endbuyer + diff_origin + type_intermed + engraved + mat_recode+school_pntg_recode+paired + lrgfont + lands_sc + othgenre + discauth + othartist+still_life,
#     data = train)
# 
# pred1 = predict(model1)
# 
# pred3 = predict(model1, newdata = validation)
# 
# 
# valid.rmse.before = rmse(exp(as.numeric(valid.true$logprice)), as.numeric(pred3))
# 
# 
# train.rmse.before = rmse(exp(as.numeric(paintings_train$logprice)), as.numeric(pred1))
```



```{r}
cv.ctrl2 <- trainControl(method = "repeatedcv", repeats = 3,number = 3,
                        classProbs = TRUE,
                        allowParallel=T)

xgb.grid2 <- expand.grid(nrounds = 100,
                        eta = seq(0.01, 0.21, by = 0.05),
                        lambda = seq(0.1, 1, by = 0.1),
                        alpha = 1)


set.seed(45)
xgb_tune2 <-train(logprice~.,
                 data=train,
                 trControl = cv.ctrl,
                 method="xgbLinear",
                 tuneGrid=xgb.grid,
                 gamma = 1,
                 nthread = 8,
                 metric = c("RMSE"),
                maximize = FALSE

    )

predictions.test3 = predict(xgb_tune2, newdata = test) %>% exp()
predictions.valid3 = predict(xgb_tune2, newdata = validation) %>% exp()

predictions.train3 = predict(xgb_tune2) %>% exp()



train.rmse2 = rmse(exp(as.numeric(paintings_train$logprice)), as.numeric(predictions.train3))


test.rmse2 = rmse(exp(test.true$logprice), as.numeric(predictions.test3))

valid.rmse2 = rmse(exp(as.numeric(valid.true$logprice)), as.numeric(predictions.valid3))
```


```{r}
model0 = lm(logprice ~ 1+., data = train)

model1 = step(model0, k=2, trace = 0)

predictions.test4 = predict(model1, newdata = test) %>% exp()
predictions.valid4 = predict(model1, newdata = validation) %>% exp()

predictions.train4 = predict(model1) %>% exp()



train.rmse3 = rmse(exp(as.numeric(paintings_train$logprice)), as.numeric(predictions.train4))


test.rmse3 = rmse(exp(test.true$logprice), as.numeric(predictions.test4))

valid.rmse3 = rmse(exp(as.numeric(valid.true$logprice)), as.numeric(predictions.valid4))
```


```{r}
library(lars)
library(monomvn)
yf = train[,7]
Xf = train[,7]
rbhs = blasso(Xf, yf, case="hs",
theta = 0.1, RJ=FALSE,
thin=10, T=2000,
verb=0)
y.pred = mean(rbhs$mu) +
Xf %*% apply(rbhs$beta, 2, mean)
```



```{r}

```

