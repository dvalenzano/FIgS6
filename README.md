### FIgS6
For FigureS6 C,D,E,F follow [surv_c1.R](https://github.com/dvalenzano/FIgS6/blob/master/surv_c1.R "surv_c1.R"), which will direct you to the data files used for the analysis.  
Figure S6H is generated with:
<pre><code>c1h <- read.csv(file="/Volumes/group_dv/personal/DValenzano/month-by-month/Feb2015/cross1_het.csv", head=T, sep=',')
plot(c1h$days, c1h$Het, xlab="days", ylab="heterozygosity", pch=16)
</code></pre>
