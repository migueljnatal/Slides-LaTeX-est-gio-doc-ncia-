# Estágio Docência PPGEst - Estatística Econômica - UFRGS

# Lista 5 -  Cap. 7 - Wooldridge

# Autor: Miguel Jandrey Natal 


# Questões Computacionais

install.packages('wooldridge') # Instala o pacote com os datasets do Wooldridge; se vc já tem esse pacote instalado, ignore este comando 
library(wooldridge)            # Chama o pacote para ser usado nessa sessão


# C1 - Questão 8


# "Setando" os dados

data = gpa1
data                           # Nos permite visualizar o dataset

# Executa a regressão linear (i)

model = lm(gpa1$colGPA ~ gpa1$PC + gpa1$hsGPA + gpa1$ACT + gpa1$mothcoll + gpa1$fathcoll)
summary(model)

# Executa a regressão linear (iii)

hsGPA_2 = (gpa1$hsGPA)*(gpa1$hsGPA)

hsGPA_2 == (gpa1$hsGPA)^2
  
model = lm(gpa1$colGPA ~ gpa1$PC + gpa1$hsGPA + gpa1$ACT + gpa1$mothcoll + gpa1$fathcoll + hsGPA_2)
summary(model)



# C7 - Questão 9 


# "Setando" os dados

data = wage1
data                           # Nos permite visualizar o dataset

# Executa a regressão linear (i)

I = (wage1$female*wage1$educ)

exper_2 = (wage1$exper*wage1$exper)

exper_2 == (wage1$exper)^2 

tenure_2 = (wage1$tenure*wage1$tenure)

tenure_2 == (wage1$tenure)^2

#model = lm(log(wage) ~ female + educ + (female*educ) + exper + exper_2 + tenure + tenure_2, data = wage1)
#summary(model)

model_2 = lm(log(wage1$wage) ~ wage1$female + wage1$educ + I + wage1$exper + exper_2 + wage1$tenure + tenure_2)
summary(model_2)


# Executa a regressão linear (ii)

educ_ii = (wage1$educ-12.5)

model_ii = lm(log(wage) ~ female + educ + (female*educ_ii) + exper + exper_2 + tenure + tenure_2, data = wage1)
summary(model_ii)



# C5 - Questão 10

data = ceosal1
data


# Criando a dummy rafneg 

rosneg = ceosal1$ros

for (i in length(ceosal1$ros)) {  # Podemos fazer isto através de um loop com estrutura de controle

if(ceosal1$ros[i] < 0){
  
  rosneg[i] = 1
  
} else {rosneg[i] = 0}

}  

# Executa a regressão linear

model = lm(log(salary) ~ log(sales) + roe + rosneg, data = ceosal1)
summary(model)



