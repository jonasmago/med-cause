mean (df.subject_V2[,3])
sqrt (var(df.subject_V2[,3]))

mean (df.subject[,3])
sqrt (var (df.subject[,3]))

t.test(df.subject_V2[,2], df.subject[,2])

V1 = c(0,5,3,0,0,0,0,0,4,27,5,0,0)
V2 = c(0,5,2,0,0,0,0,0,1,22,3,1,0)
chisq.test(V1, V2,correct=FALSE)
fisher.test(V1, V2)

V1 = c(0,5,3,0,0,4,27,5,0,0)
V2 = c(0,5,2,0,0,1,22,3,1,0)
chisq.test(V1, V2,correct=FALSE)
fisher.test(V1, V2)

