#' Fitting Linear Models
#' 
#' @description lm is used to fit linear models, including multivariate ones. It can be used to carry out regression, single stratum analysis of variance and analysis of covariance (although aov may provide a more convenient interface for these).
#' @details 
#' \preformatted{
#' # Scheffe H0:Sumcimui=0 H1:Sumcimui!=0, sumci=0
#' # medias <- tapply(datos$Y,datos$Trata,mean); ci <- c(1, 1, 1, -3) #cambiar; cme <- anova(modelo)[4ó5,3]; L <- (sum(ci*medias)); 
#' # glT <- anova(modelo)[1,1]; #' gle <- anova(modelo)[4ó5,1]; VCS <- sqrt(glT*qf(1-alfa, glT, gle)*cme*sum(ci^2/t)) #RH0 |L|>VCS
#' # Dunnett H0:mui=muT H1:mui!=muT para todo i!=T; set.seed(123); DescTools::DunnettTest(datos$Y, datos$Trata, control = "A", conf.level = 1-alfa)
#' # Newman-Keul H0:mui=muj H1:mui!=muj para todo i!=j, i,j=1,2,...,t; agricolae::SNK.test(modelo, "Trata", alpha = alfa, console = T)
#' # Duncan H0:mui=muj H1:mui!=muj para todo i!=j, i,j=1,2,...,t; agricolae::duncan.test(modelo, "Trata", alpha = alfa, console = T); 
#' # sort(tapply(datos$Y,datos$Trata,mean))
#' # Tukey H0:mui=muj H1:mui!=muj para todo i!=j, i,j=1,2,...,t; agricolae::HSD.test(modelo,"Trata", alpha = alfa, group = F, console = T); 
#' # qtukey(1-alfa,t-1,(t-2ó3)(t-1))
#' # DLS(DMS) H0:mui=muj H1:mui!=muj para todo i!=j, i,j=1,2,...,t; agricolae::LSD.test(modelo, "Trata", alpha = alfa, p.adj = "bonferroni", console = T)
#' # Prueba t H0:mu(i)-mu(j)=mu0 H1:mu(i)-mu(j)><!=mu0; cme<-anova(modelo)[4ó5,3]; medias <- tapply(datos$Y, datos$Trata, mean); 
#' # ((Ymeani.-Ymeanj.)-mu0))/sqrt(2*cme/t); qt(1-alfa,(t-2ó3)(t-1)) #cola derecha RH0 tc>tt; qt(1-alfa/2,(t-2ó3)(t-1)) #bilateral RH0 |tc|>tt
#' # agricolae::cv.model(modelo); cme<-anova(modelo)[4ó5,3]; sqrt(cme)/mean(datos$Y)*100
#' # Ft <- qf(1-alfa,t-1,(t-2ó3)(t-1)) RH0 Fc>Ft; pvalue <- 1-pf(Fc,t-1,(t-2ó3)(t-1))
#' # anova(modelo) #H0:mu1=mu2=...=mu H1: mui != mu para al menos un i=1,2,...,t
#' # shapiro.test(modelo$residuals) #H0:errores se distribuyen normal; bartlett.test(modelo$residuals, datos$Trata) #H0:sigma1^2=...=sigma^2 
#' # H1: sigmai^2 != sigma^2 para al menos un i=1,2,...,t; 
#' # tseries::runstest(factor(modelo$residuals>0)) #H0:errores son indep con respecto a la media H1:no son
#' # modelo <- lm(Y ~ Trata + Bloquef + Bloquec, data = datos); modelo <- lm(Y ~ Trata + Bloquef + Bloquec + Factor, data = datos)
#' # str(datos); names(datos)[1] <- "Y"; names(datos)[2] <- "Trata"; names(datos)[3] <- “Bloquef”; names(datos)[4] <- “Bloquec”; str(datos) 
#' # convertir Trata y Bloques as.factor
#' # datos <- as.data.frame(readxl::read_excel(file.choose(), sheet = ?))
#' # tr = c("A", "B", "C", “D”); agricolae::design.lsd(trt = tr, seed = 20) #4 ttos (A,B,C, D), 4^2 u.exp.
#' # tr1 = c("A", "B", "C", “D”) tr2=c(“Alfa”,”Beta”,”Gamma”,”Sigma”); agricolae::design.graeco(tr1, tr2, seed = 20) #4 ttos (A,B,C, D), 4^2 u.exp.
#' # Trata = t = # tratamientos; Bloquef = bf = # bloques fila; Bloquec = bc = # bloques columna
#' # t = bf = bc = f
#' }
#' @name guia_de
#' @keywords internal
NULL