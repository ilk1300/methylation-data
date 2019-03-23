{\rtf1\ansi\ansicpg1252\cocoartf1671\cocoasubrtf200
{\fonttbl\f0\fnil\fcharset0 Menlo-Regular;\f1\fswiss\fcharset0 Helvetica;}
{\colortbl;\red255\green255\blue255;\red36\green38\blue41;\red235\green236\blue237;\red251\green0\blue7;
\red0\green0\blue0;\red251\green0\blue7;\red0\green0\blue0;}
{\*\expandedcolortbl;;\cssrgb\c18824\c20000\c21176;\cssrgb\c93725\c94118\c94510;\cssrgb\c100000\c12195\c0;
\cssrgb\c0\c1\c1;\cssrgb\c100000\c12195\c0;\cssrgb\c0\c1\c1;}
\margl1440\margr1440\vieww14120\viewh8400\viewkind0
\deftab720
\pard\pardeftab720\partightenfactor0

\f0\fs26 \cf2 \cb3 \expnd0\expndtw0\kerning0
library(dplyr)\
\pard\tx720\tx1440\tx2160\tx2880\tx3600\tx4320\tx5040\tx5760\tx6480\tx7200\tx7920\tx8640\pardirnatural\partightenfactor0

\f1\fs24 \cf0 \cb1 \kerning1\expnd0\expndtw0 \
all= read.csv("/Users/billy/Desktop/Biomi600/unknown.csv")\
mydata=data.frame(all)\
 head(mydata)\
     \
 y=log(mydata[,8])\
 summary(y)\
    \
 hist(a)\
 summary(a)\
   \
\cf4 Problem#1B\
\cf0  scaled=scale(y)\
summary(scaled)\
       V1          \
 Min.   :-6.96318  \
 1st Qu.:-0.65750  \
 Median :-0.01818  \
 Mean   : 0.00000  \
 3rd Qu.: 0.63233  \
 Max.   :12.12274  \
 hist(scaled)\
\pard\tx720\tx1440\tx2160\tx2880\tx3600\tx4320\tx5040\tx5760\tx6480\tx7200\tx7920\tx8640\pardirnatural\partightenfactor0
\cf5 \
\
\cf4 For meth and unmeth files\
\cf5  unmeth= read.csv("/Users/billy/Desktop/Biomi600/known-unmeth.csv")\
 meth= read.csv("/Users/billy/Desktop/Biomi600/known-meth.csv")\
 unmethDF=data.frame(unmeth)\
 methDF=data.frame(meth)\
 unmethIPD=df[,8]\
 head(unmethIPD)\
[1] 0.842 0.754 0.975 0.919 1.056 0.511\
unmethIPD=unmethDF[8]\
 \
head(unmethIPD)\
  ipdRatio\
1    1.123\
2    0.970\
3    1.121\
4    1.003\
5    1.018\
6    0.951\
 methIPD=methDF[8]\
 hist(unmethIPD)\
 hist(unmethDF)\
 unmethIPD=unmethDF[,8]\
 \
 hist(unmethIPD)\
 methIPD=methDF[,8]\
 hist(methIPD)\
 norm_unmethIPD=log(unmethIPD)\
 norm_methIPD=log(methIPD)\
 hist(norm_methIPD)\
 hist(norm_unmethIPD)\
hist(norm_methIPD)\
\
\cf4 1C:  filter\
\
\cf5  table <- data.frame(scaled)\
 head(table)\cf4 \
\cf5  sd1 <- subset(table, x > 0 & x < 1)\
 head(sd1)\
      \
 sd2 <- subset(table, x > 1 & x < 2)\
 sd3 <- subset(table, x > 2 & x < 3)\
 sd4 <- subset(table, x > 3 & x < 4)\
 sd5 <- subset(table, x > 4 & x < 5)\
 sd6 <- subset(table, x > 5 & x < 6)\
 sd7 <- subset(table, x > 6 & x < 7)\
 sd8 <- subset(table, x > 7 & x < 8)\
 sd9 <- subset(table, x > 8 & x < 9)\
 sd10 <- subset(table, x > 9 & x < 10)\
 sd11 <- subset(table, x > 10 & x < 11)\
sd12 <- subset(table, x > 11 & x < 1+max(table))\
\
 d1=cbind.fill(sd1,sd2,sd3,sd4,sd5,sd6,sd7,sd8,sd9,sd10,sd11,sd12)\
 head(d1)\
         SD1      SD2      SD3      SD4      SD5      SD6      SD7      SD8      SD9     SD10     SD11     SD12\
1 0.01473393 1.305925 2.060422 3.012811 4.394173 5.661180 6.365062 7.002697 8.111893 9.042473 10.02552 11.42530\
2 0.33356287 1.460103 2.499472 3.155337 4.229261 5.379279 6.491769 7.113043 8.274806 9.524553 10.05318 12.12274\
3 0.52559855 1.344297 2.121342 3.216473 4.018330 5.419730 6.069319 7.670525 8.539502 9.088398 10.27411 11.02656\
4 0.81083202 1.136284 2.178964 3.089457 4.295764 5.376068 6.585417 7.846276 8.551127 9.107379 10.76381 11.42530\
5 0.51838075 1.080190 2.259198 3.014746 4.076565 5.749919 6.299319 7.562354 8.223162 9.088398 10.07438 12.12274\
6 0.90046299 1.108336 2.028373 3.615086 4.232114 5.224387 6.112296 7.758777 8.037237 9.503268 10.21076 11.02656\
\
\cf4 1d:\cf5 \
 library("Hmisc")\
 cor_2 <- rcorr(as.matrix(d1))\
 str(cor_2)\
\
\cf6 1e: \cf7 \
\cf5 \
mydata2=data.frame(mydata)\
 m2=data.frame(mydata)\
 head(m2)\
\
v1=m2[which(m2$strand==0), ]\
v2=m2[which(m2$strand==1),]\
\
 L1=lead(v1$ipdRatio, 1)\
 tail(L1)\
\
 L2=lead(v1$ipdRatio, 2)\
 L3=lead(v1$ipdRatio, 3)\
 L4=lead(v1$ipdRatio, 4)\
 L5=lead(v1$ipdRatio, 5)\
 lag1=lag(v1$ipdRatio,1)\
 oL1=lead(v2$ipdRatio, 1)\
 head(oL1)\
 tail(oL1)\
 oL2=lead(v2$ipdRatio, 2)\
 oL3=lead(v2$ipdRatio, 3)\
 oL4=lead(v2$ipdRatio, 4)\
 oL5=lead(v2$ipdRatio, 5)\
 olag1=lag(v2$ipdRatio,1)\
 olag2=lag(v2$ipdRatio,2)\
 olag3=lag(v2$ipdRatio,3)\
 olag4=lag(v2$ipdRatio,4)\
 olag5=lag(v2$ipdRatio,5)\
\
\
}