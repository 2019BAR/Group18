load("./期末競賽/Z.rdata")


XXX = subset(A,cut2_4_7 == 7 & keams == 4)

ZZ = merge(Z,A[c(1,12)],by = 'cust')
ZZZ = subset(ZZ,keams == 4)

group_by(ZZZ,prod) %>% summarise(
  n = n()
) %>% arrange(desc(n))

TY = as(split(ZZZ$prod, ZZZ$tid), "transactions")
itemFrequency(TY) %>% sort(decreasing=T) %>% head(100)

inspect(TY[1:10])
par(cex=0.8)
itemFrequencyPlot(TY, support = 0.025, xlim = c(0,0.3),
                  type = "relative", horiz = TRUE, col = "dark red", las = 1,
                  xlab = paste0(
                    "Proportion of Market Baskets Containing Item\n",
                    "(Item Relative Frequency or Support)"))

dfy = itemInfo(TY)
str(dfy) # levels 10, 55

second.rules <- apriori(TY, 
                        parameter = list(support = 0.0025, confidence = 0.1))

vegie.rules <- subset(second.rules, subset = rhs %pin% "4711271000014")
inspect(vegie.rules)
plot(vegie.rules, method="graph", 
     control=list(type="items", alpha=1, labelCol="blue"), 
     shading = "lift")
library(manipulate)
library(latex2exp)


manipulate({
  do.call(rbind, lapply(seq(5,40,0.5), function(c){
    p = m*plogis((10/a)*(c-b))
    B %>% mutate(
      PI = ifelse(Buy<=(1-p), p, 1-Buy) * Rev - c
    ) %>%
      group_by(cut2_4_7) %>% summarise(
        Cost = c,
        Group.Sz = n(),
        No.Target = sum(PI>0),
        AvgROI = mean(PI[PI>0]),
        TotalROI = sum(PI[PI>0])
      ) } ) ) %>% 
    ggplot(aes(x=Cost, y=TotalROI, col=as.factor(cut2_4_7))) +
    geom_line(size=1.2) +
    ggtitle("Cost Effeciency per Segment ")
},
m = slider(0.05, 0.25,  0.20, step=0.01),
a = slider(  10,   30,    20, step=1),
b = slider(   4,   20,    15, step=1)
)



