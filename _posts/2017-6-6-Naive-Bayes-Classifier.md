---
title: "Naive Bayes Classifier for Quotes (using R Notebook)"
author: "Tim Dunbar"
date: "June 6 2017"
---
---------------

### naive bayes classifier

This will be my first blog post. It is primarily for testing purposes. My workflow basically consists of R Studio, github, and Jekyll which is a Ruby Gem. I will probably write another blog post detailing my processes after I figure out what they are.

As the title suggests, this post will be about a Naive Bayes Classifier (NBC) I wrote after attending a meetup on NBCs written in Python. This classifier is trained with male and female quotations but would work equally well classifying other categorical data (note: I am not suggesting that my NBC is accurate).

This post will primarily consist of the mechanics behind my NBC and the resources I used to put it all together. I will write future blog posts regarding accuracy and eventual improvements.

#### First we need a function to build the data frames we will use as our training data inputs:

``` r
textCleaner<-function(x){
  x<-scan(x, what="", sep="\n")
  #removes the author of the quote because I am only interested in male or female
  x<-gsub("--\\s.*", "", x)
  #removes punctiation
  x<-gsub("([-'])|[[:punct:]]", "", x)
  #splits on spaces
  x<-strsplit(x, "[[:space:]]+")
  #formats as data frame
  x<-as.data.frame(unlist(x))
  return(x)
}
```

#### Here we are using some text files that I acquired from the web and the textCleaner function we wrote earlier. I'm also going to define some other variable we will need later.

We are using the following quote from Eleanor Roosevelt: *"A woman is like a tea bag, you can't tell how strong she is until you put her in hot water."*

``` r
#These are our corpuses made from male and female quotes
men_quote <- textCleaner("/home/timothy/naive-bayes-classifier/men.txt")
women_quote <- textCleaner("/home/timothy/naive-bayes-classifier/women.txt")
quote <- textCleaner("/home/timothy/naive-bayes-classifier/quote.txt")
men_prior <- 1
women_prior <- 1
```

#### We obviously need a function that does the classification, the actuall NBC. I will go through the code line by line and explain what's going on a bit later but for now we will just write it.

``` r
bayesClassifier<-function(menClass, womenClass, document, menPrior, womenPrior){
  #gets counts of words in each class
  mCount<-nrow(menClass)
  wCount<-nrow(womenClass)
  #combines the menClass and womenClass dataframes into a vocabulary dataframe
  vocabAll<-rbind(menClass, womenClass)
  #collapses like words in vocabAll and find count of all unique words in vacabulary
  vocabAll<-as.data.frame(table(vocabAll))
  vocabCount<-nrow(vocabAll)
  #collapses menClass and womenClass data frames and finds the frequency of each word
  menClass<-as.data.frame(table(menClass))
  womenClass<-as.data.frame(table(womenClass))
  #finds intersection of document data frame and the menClass and womenClass dataframes
  intersectM<-menClass[is.element(menClass$menClass, intersect(document$`unlist(x)`, menClass$menClass)),]
  intersectW<-womenClass[is.element(womenClass$womenClass, intersect(document$`unlist(x)`, womenClass$womenClass)),]
  #conditional probabilities of each intersecting word, this would be the place to add smoothing if desired in place of the 0s
  intersectM$Freq<-(intersectM$Freq+0)/(mCount+vocabCount+0)
  intersectW$Freq<-(intersectW$Freq+0)/(wCount+vocabCount+0)
  #finds product the frequency column and multiplies by the priors
  posteriorM<-prod(intersectM$Freq)*menPrior
  posteriorW<-prod(intersectW$Freq)*womenPrior
  #test for higher posterior
  if(posteriorW>posteriorM){
    return("Female")
  }
  return("Male")
}
```

#### Finally we call our NBC function and pass in the variables we made earlier

``` r
answer <- bayesClassifier(men_quote, women_quote, quote, men_prior, women_prior)

answer
```

    ## [1] "Male"

This is clearly wrong but keep in mind I am using very small data sets.
