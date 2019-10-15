## vetores, dados, matrizes e subconjuntos####

####Construir um vetor####

x = c(2,7,5)

x

y = seq(from = 4, length = 3, by = 3)
?seq

y

x+y
x/y
x^y

####[]indica a posi��o dos elementos buscados em um dado vetor#################
x[2]
x[2:3]

########[]o elemento com o sinal menos remove a  elemento  na posi��o e retorna o restante do vetor##########
x[-2]
x[-c(1,2)]


#######MAtriz################################
###A sequ�ncia com valores de 1 � 12 e, depois da v�rgula, entram o n�mero de linhas e o n�mero de coluna ####

z=matrix(seq(1,12),4,3)

z
###########Retornar as colunas 3-4 e as linhas 2-3##############################

z[3:4,2:3]

#quando pedir para retornar uma coluna, indique depois da v�rgula para que o computador entenda que � um coluna###
####coluna###
z[,1]
####linha####
z[1,]

####Quando pedimos que a m�quina no retorne uma coluna ou uma linha, os valores deixam de ser uma matriz####
####e se transformam em um vetor. Para manter o status de matriz, n� indcamos que o "drop = False"

z[,1, drop = F]

dim(z)

####Gerar dados aleat�rios e gr�ficos#######
x=runif(50)
#####Valores com a distribui��o normal######
y=rnorm(50)

####gr�fico#############
plot(x,y)

plot(x,y, xlab = "Random Uniform", ylab = "Random Normal", pch = "*", col = "blue")


#####o comando mfrow indica duas linhas de gr�fico e uma coluna#####
par(mfrow = c(2,1))
plot(x,y)
hist(y)
par(mfrow = c(1,1))

######Buscar os dados guardados####

data(mtcars)

#######Checar as principais caracter�sticas#####
names(mtcars)
dim(mtcars)
class(mtcars)
summary(mtcars)

########Gr�ficos#################

plot(mtcars$cyl,mtcars$mpg)

###Voc� pode referenciar um data frame "diretamente"#####

attach(mtcars)
search()
plot(cyl,mpg)



##########Regress�o#########################

####Pacotes instalados para o exerc�cio####

library(MASS)

install.packages("ISLR")
library(ISLR)

##########Regress�o simples################

names(Boston)
data("Boston")


fit = lm(medv~lstat, data = Boston)
summary(fit)          

###########Linha de regress�o#########
abline(fit, col = "red")
names(fit)

###########Intervalo de confian�a###########
confint(fit)

###########Predi��o com intervalo de confian�a##################
predict(fit,data.frame(lstat=c(5,10,15)),interval = "confidence")


##########Regress�o multipla################
fit2 = lm(medv~lstat+
            age,
          data = Boston)
summary(fit2)

##########Usando todos os regress�res, exclu�ndo o regressando#############3333
fit3 = lm(medv~ .,
          data = Boston)
summary(fit3)

########V�rios gr�ficos ao mesmo ao mesmo tempo (2 linhas, 2 colunas)

par(mfrow= c(2,2))
plot(fit3)


##########Regress�o com tudo, mas sem Age e Indus####################

fit4 = update(fit3, ~. 
              - age
              - indus)
summary(fit4)


########### N�oLinear#######################################


fit5 = lm(medv ~ lstat*age,
          data = Boston)

summary(fit5)

fit6 = lm(medv ~ lstat + I(lstat^2),
          data = Boston)

summary(fit6)


############attach###########################

attach(Boston)


###########retornar os gr�ficos ############

par(mfrow = c(1,1))

plot(medv~lstat)

#########gr�fico em pontos################
points(lstat, fitted(fit6),
       col = "red", pch =20)

###########fun��o polinomial direto########
fit7 = lm(medv~poly(lstat,4))

points(lstat, fitted(fit7),
       col = "blue",
       pch = 20)

#forma de gr�ficos#####################
######primeiro estabelece os dados de 1 � 20 para cada eixo e depois estabelece um formato para cada unidade (pch)#####
plot(1:20, 1:20, pch = 1:20, cex = 2)


######Preditores qualitativos##########
fix(Carseats)
names(Carseats)


summary(Carseats)

########o : indica a intera��o entre a vari�vel qualitativa e a quantitativa#################

fit1 = lm(Sales~. + Income:Advertising + Age:Price, 
          Carseats)
summary(fit1)

######Discrimina as vari�veis qualitativas###############
contrasts(Carseats$ShelveLoc)

#########Escrever Fun��es no R  "..." inclui maior vari�veis########################
regplot = function(x,y){
          fit = lm(y~x,...)
          plot(x,y)
          abline(fit,col = "red")
       
}

attach(Carseats)
regplot(Price, Sales)

regplot(Price,Sales,
        xlab = "Price",
        ylab = "Sales",
        col = "blue",
        pch = 20)



################Logit###############################

########require � analogo � library##############

require(ISLR)

names(Smarket)
summary(Smarket)

#######Smarket$Direction � uma vari�vel bin�ria#########

#######Gr�ficos com a coluna � a bin�ria###########

pairs(Smarket, 
      col = Smarket$Direction)


#######Logit#####################################

glm.fit = glm(Direction ~ 
                Lag1 +
                Lag2 + 
                Lag3 +
                Lag4 +
                Lag5 +
                Volume,
              data = Smarket,
              family = binomial)
summary(glm.fit)

#############Predi��o#################
#######o type = "response" devolve a probabilidade. No Default o resultado d� os os log-odds#####
####### o type = "terms" devolve a matriz de termos ##################


glm.probs = predict(glm.fit,
                    type = "response")

glm.probs[1:5]

#############Estabelecer uma resposta verbal � probabilidade################

glm.pred = ifelse(glm.probs > 0.5, "Up", "Down")


attach(Smarket)

#########A diagonal da tabela revela os valores verdadeiros###############
table(glm.pred,
      Direction)

##########m�dia###########################################################
mean(glm.pred==Direction)

#########Treinamento e conjunto teste###################################
#######O treino aponta uma coluna de "true" para anos anteriores e o restante � "false"

treino = Year<2005

########Aqui a gente roda o Logit com um subconjunto de anos menores que 2005###########

glm.fit = glm(Direction ~ Lag1 +
                          Lag2 +
                          Lag3 +
                          Lag4 +
                          Lag5 +
                          Volume,
              data = Smarket,
              family = binomial,
              subset = treino)


#############predi��o###################################################
glm.probs=predict(glm.fit,
                  newdata = Smarket[!treino,],
                  type = "response")

glm.pred=ifelse(glm.probs > 0.5, "Up", "Down")

##########dire��o sem o subjunto treino###############################

Direction.2005 = Smarket$Direction[!treino]
table(glm.pred, Direction.2005)
mean(glm.pred == Direction.2005)

########ficou pior#################################################


#########modelo mais enxuto######################################
glm.fit=glm(Direction ~ Lag1
                      + Lag2,
            data = Smarket,
            family = binomial,
            subset = treino)

glm.probs = predict(glm.fit,
                    newdata = Smarket[!treino,],
                    type = "response")

glm.pred = ifelse(glm.probs > 0.5, "Up", "Down")
table(glm.pred, Direction.2005)
mean(glm.pred == Direction.2005)



summary(glm.fit)



############valida��o##############################
##########O objetivo � validar a predi��o utilizando um conjunto#########
##########de dados n�o usada previamente para a predi��o###############



#############Boot gera um bootstrap que replica#########
#############uma estat�stica aplicada aos dados######### 
library(boot)



plot(mpg~horsepower, data = Auto)



########o glm tamb�m roda mqo##################
glm.fit = glm(mpg~horsepower,
              data = Auto)


##########o crossvalidation (cv.glm) substitui uma observa��o da amostra original########
#com uma observa��o da predi��o################

??cv.glm


cv.glm(Auto,glm.fit)$delta

(CV <- cv.glm(Auto,
             glm.fit)


###########Fun��o da valida��o "leave one out"##########
###########essa � a fun��o que representa o cv.glm####


loocv <- function(fit){
                        h = lm.influence(fit)$h
                        mean((residuals(fit)/(1-h))^2)
                        }

###########podemos "crossvalidar" sem o pacote#########

loocv(glm.fit)
                        

###########modelo polinomial#######################

#######primeiramente cria-se um vetor com 5 linhas #######
#######para erros do grau dos graus dos polin�mios#####


cv.error = rep(0,5)
 

grau = 1:5
for(g in grau) {
  glm.fit=glm(mpg~poly(horsepower, g),
              data = Auto)
  cv.error[g] = loocv(glm.fit)         
}

plot(grau, cv.error, type = "b")



########Valida�ao em 10################################
########divide-se a amostra em 10 partes e usa-se 1 parte para o treino#######


cv.error10 = rep(0,5)
for(g in grau) {
  glm.fit=glm(mpg~poly(horsepower, g),
              data = Auto)
  cv.error10[g] = cv.glm(Auto,
                         glm.fit,
                         K = 10)$delta[1]         
}

lines(grau, 
      cv.error10,
      type = "b",
      col="red")


##########Melhor subconjunto########################

#######pacote "leaps" para decidir o melhor subconjunto###########

install.packages("leaps")
library(leaps)


?leaps
##########"leaps() performs an exhaustive search for the best subsets###########
######of the variables in x for predicting y in linear regression,############
#######using an efficient branch-and-bound algorithm."############


########usa o dataframe pertencente ao ISLR#######################

summary(Hitters)


###########retirar os valores nulos################################
Hitters <- na.omit(Hitters)
summary(Hitters)



########usando o comando regsubsets###############################

regfit.full = regsubsets(Salary~.,
                         data = Hitters)
summary(regfit.full)


########como h� 19 vari�veis � poss�vel estender at�###### 
######## 19 subconjuntos#########################################

regfit.full = regsubsets(Salary~.,
                         data = Hitters,
                         nvmax = 19)

reg.summary <- summary(regfit.full)

########Plotando o gr�fico da estat�stica CP [Cp = SQR/MQR - N�mero de obs + 2vari�veis]
#######Contra o n�mero de vari�veis###############################

plot(reg.summary$cp,
     xlab = "Vari�veis",
     ylab = "Cp stat")

########por n�mero###############################################

which.min(reg.summary$cp)

#####pede os coeficientes dos modelo 10, o melhor################

coef(regfit.full, 10)




###########Bootstrap

###### Minimum risk investment


#####Vamos imaginar um investimento x e um investimento y 

alpha <- function(x,y) {
                         vx = var(x)
                         vy = var(y)
                         cxy = cov(x,y)
                         (vy-cxy)/(vx+vy-2*cxy)
}

######(vy-cxy)/(vx+vy-2*cxy) indica minimizar o risco [var]


alpha(Portfolio$X,
      Portfolio$Y)


######calcular o erro padr�o proveniente do alpha

alpha.fn = function(data, index){
                                 with(data[index,],
                                      alpha(X,Y))
}

alpha.fn(Portfolio, 1:100)


set.seed(1)
alpha.fn(Portfolio,sample(1:100,100,
                          replace = T))


boot.out <- boot(Portfolio, alpha.fn, R=1000)

boot.out

plot(boot.out)

  
#########�rvores de decis�o#####################################
library(ISLR)
install.packages("tree")

library(tree)

attach(Carseats)

hist(Sales)


###### Para adequar a vari�vel ao problema de################### 
##### �rvore de decis�o, transformou-se as vendas###############
##### em resposta bin�ria#######################################

High = ifelse(Sales <=8, "No", "Yes")
Carseats <- data.frame(Carseats, High)


###### Agora usa-se o modelo de �rvore, mas retira-se a######## 
###### a vari�vel de vendas, porque, gerou a bin�ria###########

tree.carseats <- tree(High ~.
                      -Sales,
                      data = Carseats)
summary(tree.carseats)

plot(tree.carseats)
text(tree.carseats)


tree.carseats


########criar um conjunto de treino e de treino################
########dividir a amos de 400 obs. usa no treino e testa#####
#######no conjunto teste######################################

#########voc� usa o set.seed para "congelar" o gerador de 
#########n�meros aleat�rios###################################

set.seed(1011)
treino = sample(1:nrow(Carseats),250)
tree.carseats = tree(High~.-Sales,Carseats,subset = treino)
plot(tree.carseats);text(tree.carseats,pretty=0)

tree.pred=predict(tree.carseats,
                  Carseats[-treino,],
                  type = "class")

##########tabela do treinamento########### 
##########cross validation################
with(Carseats[-treino,],table(tree.pred,High))
(72+33)/150

cv.carseats <- cv.tree(tree.carseats, FUN = prune.misclass)
cv.carseats


prune.carseats <- prune.misclass(tree.carseats, best =13)
plot(cv.carseats)
plot(prune.carseats);text(prune.carseats, pretty = 0)


########SUPORT VECTOR MACHINE###################


#########criar uma matriz#######################
########set seed para reproduzir a aletoriedade#

set.seed(10111)

##########estabelece a distribui��o probabil�stica(normal)########
###########com o n�mero de 40 observa��es; 20 linhas e 2 colunas#
x=matrix(rnorm(40),20,2)

##########criar uma vari�vel -1,1#######################
y=rep(c(-1,1),c(10,10))

#########Como a disttribui��o normal estabelece m�dia = 0
#########corrige-se para uma m�dia 1######################

x[y==1,]=x[y==1,]+1

plot(x,col=y+3,pch=19)

make.grid <- function(x,n=75){
               grange=apply(x,2,range)
               x1=seq(from=grange[1,1],to=grange[2,1],length=n)
               x2=seq(from=grange[1,2],to=grange[2,2],length=n)
               expand.grid(X1=xq,X2=x2)}

##############pacotes###############################
install.packages("e1071")
library(e1071)
 

#######cria o data frame e estabelece o y########## 
#######como vari�vel categ�rica#####################
dat <- data.frame(x,y=as.factor(y))
svmfit=svm(y~.,
           data=dat,
           kernel = "linear",
           cost=10,
           scale=FALSE)
print(svmfit)
plot(svmfit,dat)


##A primeira coisa a fazer � uma grade dos valores para X1 e X2###