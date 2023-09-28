#library(survival)
library(readxl)
library(stats)

x <- read_excel("C:/Users/research/Desktop/Cancer Cervix/dfs os (28 april )Final data (2).xlsx")
colnames(x)

curve_colors <- c("blue","hotpink","green","purple","orange")
par(bg = "azure")


summary(x$OS)

survival_analysis <- survfit(Surv(OS,vs_code_4R)~Age,data = x)
plot(survival_analysis,xlab="time",ylab="survival probability",main="k-m curve",conf.int = FALSE, mark.time = TRUE,
     col = curve_colors,lwd = 2)

#par(new = TRUE)

#survival_analysis <- survfit(Surv(DFS,`RECURRENCE/NON RECURRENCE`)~Age,data = x)
#plot(survival_analysis,xlab="time",ylab="survival probability",main="k-m curve",conf.int = FALSE, mark.time = TRUE,
# col = curve_colors,lwd = 1)


L <- factor(x$Age)
legend("bottom", legend = levels(L), col = curve_colors, lwd = 1,bg = "lightgray")

cont_table= table(x$Age,x$Comorbidities)
cont_table

chi_square <- chisq.test(cont_table)
print(chi_square)

# Fit the Cox regression model
cox_model <- coxph(Surv(OS,vs_code_4R) ~ Age , data = x)
summary(cox_model)
 my-first-repo
