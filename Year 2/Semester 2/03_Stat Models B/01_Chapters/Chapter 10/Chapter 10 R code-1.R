##########################################
## Example 10.1, cinema going by gender ##
##########################################

matcinema = matrix(c(22,20,21,12),2,2)
chisq.test(matcinema)

matcinema2 = matrix(c(110,100,105,60),2,2)
chisq.test(matcinema2)
chisq.test(matcinema2)$expected

fisher.test(matcinema)
fisher.test(matcinema2)


################################
## Example 10.2, Airline data ##
################################

airline_data = matrix(c(50,40,35,40,50,55,30,80,50),3,3)
chisq.test(airline_data)
chisq.test(airline_data)$expected


###################################
## Example 10.3, Drug vs Placebo ##
###################################

drug_data = matrix(c(15,35,4,46),2,2)
chisq.test(drug_data,correct=F)
chisq.test(drug_data,correct=T)

fisher.test(drug_data)