suppressMessages(library(arm))
suppressMessages(library(ggplot2))
suppressMessages(library(dplyr))
suppressMessages(library(GGally))
suppressMessages(library(foreign))
suppressMessages(library(MASS))
suppressMessages(library(car))


#data cleaning
factors=c("dealer","year","origin_author","origin_cat","diff_origin","mat","endbuyer",
          "type_intermed","artistliving","engraved","original","prevcoll","othartist",
          "paired","figures","finished","lrgfont","relig","landsALL","lands_sc","lands_elem",
          "lands_figs","lands_ment","arch","mytho","peasant","othgenre","singlefig",
          "portrait","still_life","discauth","history","allegory","pastorale", "material",
          "Shape", "other", "materialCat", "Interm", "winningbidder", "winningbiddertype",
          "authorstyle", "authorstandard", "school_pntg", "lot", "sale", "author", "subject")

#drop surface_Rnd, count, price


test=paintings_train
test[factors] = lapply(test[factors], factor)
df.train.final = test %>% select(-c(Surface_Rnd, count, price,  Diam_in,sale,subject, author,
                                    lands_sc, lands_figs, lands_ment, peasant, singlefig))
df.train.final$Height_in = as.numeric(df.train.final$Height_in)
df.train.final$Width_in = as.numeric(df.train.final$Width_in)
df.train.final$Surface = as.numeric(df.train.final$Surface)
df.train.final$Surface_Rect = as.numeric(df.train.final$Surface_Rect)
df.train.final$position = as.numeric(df.train.final$position)

###########################model building###################
new.df = df.train.final[!rowSums(is.na(df.train.final)) > 0,]
library(glmnet)

f <- as.formula(logprice ~ .+year*lrgfont
                +year*origin_author
                +origin_author*type_intermed
                +type_intermed*position
                +origin_author*Width_in
                +Width_in*Surface_Rect
                +year*Surface_Rect)
y <- new.df$logprice
x <- model.matrix(f, new.df)[, -1]

grid = 10^(seq(10,-2, length=100))

library(glmnet)
lasso.mod= glmnet(x,y,alpha=1, lambda=grid)
plot(lasso.mod)
set.seed(1)

cv.out = cv.glmnet(x,y, alpha=1)
plot(cv.out)
bestlam=cv.out$lambda.min
lasso.mod2 = glmnet(x,y,alpha=1, lambda=bestlam)

#out=glmnet(x,y,alpha=1, lambda=grid)
lasso.coef=predict(lasso.mod2, type="coefficients", s=bestlam)
lasso.coef

index = which(summary(lasso.coef)[,3] > 0.01 | summary(lasso.coef)[,3] < -0.01 )

variables = row.names(lasso.coef)[index]
###########################################
df.tree = df.train.final %>% select(-c(material, winningbidder, authorstandard, lot))
library(tree)
tree.vote =tree(logprice ~ ., data= df.tree)
summary(tree.vote)
plot(tree.vote)
text(tree.vote, cex=.75)

#Pruning
set.seed(2)
cv.vote = cv.tree(tree.vote, FUN=prune.tree)
plot(cv.vote$size,cv.vote$dev, type="b")
prune.vote = prune.tree(tree.vote ,best =11)
plot(prune.vote)
text(prune.vote, cex=.75)

######################################
df.model = df.train.final[!rowSums(is.na(df.train.final)) > 0,]

model1 = step(lm(logprice~.- lot - authorstandard,data=df.model),
              k=log(nrow(df.model)),
              direction="backward",trace=0)

model2 = step(lm(logprice~.- lot - authorstandard,data=df.model),
              k=log(nrow(df.model)),
              direction="forward",trace=0)

model3 = step(lm(logprice~.- lot - authorstandard,data=df.model),
              k=log(nrow(df.model)),
              direction="both",trace=0)


model4 = step(lm(logprice~.- lot - authorstandard,data=df.model),
              k=2,
              direction="backward",trace=0)
model5 = step(lm(logprice~.- lot - authorstandard,data=df.model),
              k=2,
              direction="forward",trace=0)
model6 = step(lm(logprice~.- lot - authorstandard,data=df.model),
              k=2,
              direction="both",trace=0)

#model3

lm(formula = logprice ~ dealer + year + origin_cat + diff_origin + 
     Width_in + Surface_Rect + engraved + prevcoll + paired + 
     finished + lrgfont + othgenre + Interm, data = df.model)



(College.trans <- df.model %>% dplyr::select(c(Width_in, Surface_Rect)) %>% 
     as.matrix() %>% powerTransform() %>% coef())

lm1 <- lm(logprice ~ dealer + year + origin_cat + diff_origin + 
                  log(Width_in) + log(Surface_Rect) + engraved + prevcoll + paired + 
                  finished + lrgfont + othgenre + Interm, data = df.model)

paintings_test[factors] = lapply(paintings_test[factors], factor)


mean.surface=paintings_test[rowSums(is.na(paintings_test)) > 0,] %>% 
  dplyr::select(dealer, year, origin_cat,diff_origin, Width_in, Surface_Rect, engraved,
         prevcoll, paired, finished, lrgfont, othgenre, Interm)

mean.surface[mean.surface$dealer == "R" & is.na(mean.surface$Interm),'Interm'] = sample(c(0,1), 67, replace = T, prob = c(1- 514/622,514/622))
mean.surface[mean.surface$dealer == "L" & is.na(mean.surface$Interm),'Interm'] = sample(c(0,1), 166, replace = T, prob = c(1- 1/38,1/38))
mean.surface[mean.surface$dealer == "J" & is.na(mean.surface$Interm),'Interm'] = sample(c(0,1), 53, replace = T, prob = c(1- 7/96,7/96))
mean.surface[mean.surface$dealer == "P" & is.na(mean.surface$Interm),'Interm'] = sample(c(0,1), 2, replace = T, prob = c(1- 1/87,1/87))

library(mice)
imputed_Data <- mice(mean.surface, m=5, maxit = 50, method = 'pmm', seed = 500)
mean.surface[is.na(mean.surface$Width_in),'Width_in'] = imputed_Data$imp$Width_in[1]
mean.surface[is.na(mean.surface$Surface_Rect),'Surface_Rect'] = imputed_Data$imp$Surface_Rect[2]

mean.surface$Width_in = log(mean.surface$Width_in)
mean.surface$Surface_Rect = log(mean.surface$Surface_Rect)

predictions = as.data.frame(
  exp(predict(lm1, newdata=mean.surface, 
              interval = "pred")))
save(predictions, file="predict-test.Rdata")
############
https://cran.r-project.org/web/packages/My.stepwise/My.stepwise.pdf
https://www.rdocumentation.org/packages/randomForestSRC/versions/2.5.1/topics/find.interaction





test=paintings_train%>%
  dplyr::select(-sale,-lot,-price,-authorstyle,-author,-Diam_in,-Surface_Rect,-Surface_Rnd,-material,-materialCat,-subject,-count,-other,-winningbidder,-authorstandard,-school_pntg,-winningbiddertype,-Shape)


factors=c("dealer","year","origin_author","origin_cat","diff_origin","mat","endbuyer","type_intermed","artistliving","engraved","original","prevcoll","othartist","paired","figures","finished","lrgfont","relig","landsALL","lands_sc","lands_elem","lands_figs","lands_ment","arch","mytho","peasant","othgenre","singlefig","portrait","still_life","discauth","history","allegory","pastorale", "Interm")

test[factors] = lapply(test[factors], factor)

#test=test[test$position<1,]
#test$nfigures=log(test$nfigures)

#change na in surface to mean and transform
# mean.surface.test=mean(test$Surface[!is.na(test$Surface)])
# test$Surface[is.na(test$Surface)]=mean.surface.test

imputed_Data <- mice(test, m=5, maxit = 50, method = 'pmm', seed = 500)

test$Surface[is.na(test$Surface)] = imputed_Data$imp$Surface[1][,1]
test$Width_in[is.na(test$Width_in)] = imputed_Data$imp$Width_in[1][,1]
test$Height_in[is.na(test$Height_in)] = imputed_Data$imp$Height_in[1][,1]
test$Interm[is.na(test$Interm)] = imputed_Data$imp$Interm[1][,1]



if(FALSE){
  ##not related with postition
  ggpairs(data = test[c( 2,9)],
          mapping = ggplot2::aes(y = logprice),cardinality_threshold = 20,
          lower = list(continuous = wrap("points", alpha = 0.3), combo = wrap("dot_no_facet", alpha = 0.4))
  )
  
  #surface unrelated
  test$Surface=log(test$Surface)
  ggpairs(data = test[c( 16,9)],
          mapping = ggplot2::aes(y = logprice),cardinality_threshold = 20,
          lower = list(continuous = wrap("points", alpha = 0.3), combo = wrap("dot_no_facet", alpha = 0.4))
  )
  
  #if figure =1, seems a positive linear relationship
  test$nfigures=log(test$nfigures)
  ggpairs(data = test[c( 18,9)],
          mapping = ggplot2::aes(y = logprice),cardinality_threshold = 20,
          lower = list(continuous = wrap("points", alpha = 0.3), combo = wrap("dot_no_facet", alpha = 0.4))
  )
  
  #test
  ggpairs(data = test[c(34,9)],
          mapping = ggplot2::aes(y = logprice),cardinality_threshold = 20,
          lower = list(continuous = wrap("points", alpha = 0.3), combo = wrap("dot_no_facet", alpha = 0.4))
  )
}

```

### Build your first model

In the first model predict the auction price `price` using the transformation `logprice` using at least 10 up to 20 predictors and any interactions to build a model using linear regression.  You may use stepwise model selection to simplify the model using AIC and/or BIC.  For reference, we will fit the null model to initialize the leaderboard, but replace model1 with your recommended model.


```{r model1, echo=TRUE}
#model1 = lm(logprice ~ 1, data=paintings_train)

nrow(test)

model1 = step(lm(logprice~.,data=test),k=log(nrow(test)),direction="backward",trace=0)
results.aic=summary(model1)
print(summary(results.aic))

results.aic$coefficients
results.aic$r.squared
```


Save predictions and intervals
```{r predict-model1, echo=FALSE}
load("paintings_test.Rdata")
var.change=c("sale","dealer","year","origin_author","origin_cat","diff_origin","mat","endbuyer","type_intermed","artistliving","engraved","original","prevcoll","othartist","paired","figures","finished","lrgfont","relig","landsALL","lands_sc","lands_elem","lands_figs","lands_ment","arch","mytho","peasant","othgenre","singlefig","portrait","still_life","discauth","history","allegory","pastorale")
# test$Surface = test$Surface +1
# (College.trans <- test %>% dplyr::select(Surface) %>% 
#     as.matrix() %>% powerTransform() %>% coef())
# 
# mod1 = lm(formula = logprice ~ dealer + year + origin_author + diff_origin + 
#     endbuyer + type_intermed + log(Surface) + nfigures + engraved + 
#     prevcoll + paired + finished + lrgfont + landsALL + lands_sc + 
#     othgenre + discauth, data = test)

imputed_Data1 <- mice(paintings_test, m=5, maxit = 50, method = 'pmm', seed = 500)
paintings_test[var.change] = lapply(paintings_test[var.change], factor)

paintings_test$Width_in[is.na(paintings_test$Width_in)] = imputed_Data1$imp$Width_in[1][,1]


# mean.surface=mean(paintings_test$Surface[!is.na(paintings_test$Surface)])
# 
# paintings_test$Surface[is.na(paintings_test$Surface)]=mean.surface


#paintings_test$Surface = log(paintings_test$Surface+1)


predictions = as.data.frame(
  exp(predict(model1, newdata=paintings_test, 
              interval = "pred")))
save(predictions, file="predict-test.Rdata")
