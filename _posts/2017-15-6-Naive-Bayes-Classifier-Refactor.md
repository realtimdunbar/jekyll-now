---
title: "Naive Bayes Classifier Refactor"
author: "Tim Dunbar"
date: "June 15 2017"
---
---------------

### Naive Bayes Classifier *Refactor*

As the title suggests this post will be a refactoring of the code from the previous post.  I'm doing this partly because I recently watched all the videos from Robert Martin's (Uncle Bob's) Clean Code series but also because I think refactoring code is a good way to learn about it.

I might try to make refactoring code a regular part of this blog.

#### First a Recap
Here is the textCleaner function
```{r textCleaner, echo=TRUE}
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
And here is the Classifier code
```{r bayesClassifier, echo=TRUE}
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
  intersectM<-menClass[is.element(menClass$menClass, intersect(quote_class$`unlist(x)`, menClass$menClass)),]
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
I will tackle the textCleaner part first.  My goal will be to make the code read like "well written prose" to quote Uncle Bob.  What this means is that all the comments I have in the code are only necessary because I did a terrible job writing the code in the first place.

First, I must write a test that the current code passes so that I know I didn't break anything while refactoring.  For that we are going to need the *testthat* library.
```{r necessary_packages, echo=TRUE}
#install.packages('testthat')
library(testthat)
```
We also need a data frame made from the original function to test the new function against.  I've assigned it to a variable for simplicities sake.
```{r}
cleaned_test_file<-textCleaner('~/naive-bayes-classifier/refactor_test_file.txt')
```
#### textCleaner unit test
```{r textCleaner unit test, echo=TRUE}
test_that('textCleaner cleans', {
  test_file<-'~/naive-bayes-classifier/refactor_test_file.txt'
  
  expect_that(textCleaner(test_file), equals(cleaned_test_file))
})
```
I ran the unit test against the original function to prove the unit test itself works.  The lack of an error means that I am ready to refactor.

#### Refactored textCleaner function
```{r text_cleaner_refactored, echo=TRUE}

# Here, I've broken out each of the seperate operations of the original code into their own function.
remove_author<-function(file){
  regex_author_pattern<-"--\\s.*"
  cleaned_file<-base::gsub(regex_author_pattern, "", file)
  return(cleaned_file)
}

remove_punctuation<-function(file){
  regex_punctuation_pattern<-"([-'])|[[:punct:]]"
  cleaned_file<-base::gsub(regex_punctuation_pattern, "", file)
  return(cleaned_file)
}

split_file<-function(file){
  regex_split_pattern<-"[[:space:]]+"
  cleaned_file<-base::strsplit(file, regex_split_pattern)
  return(cleaned_file)
}

clean<-function(file){
  cleaned_file<-remove_author(file)
  cleaned_file<-remove_punctuation(cleaned_file)
  cleaned_file<-split_file(cleaned_file)
  return(cleaned_file)
}

# An argument could be made that I didn't have to break out all the cleaning steps to their own clean function but I decided to go all out
clean_text_file_and_return_data_frame<-function(file){
  
  file<-base::scan(file, what="", sep="\n")
  
  cleaned_file<-clean(file)
  # annoyingly to get the test to pass I had to rename cleaned_file to x
  x<-cleaned_file
  x<-base::as.data.frame(unlist(x))
    
  return(x)
}
```
Now to use the test I wrote (and proved) earlier on the newly written function.
```{r}
test_that('textCleaner cleans', {
  test_file<-'~/naive-bayes-classifier/refactor_test_file.txt'
  
  expect_that(clean_text_file_and_return_data_frame(test_file), equals(cleaned_test_file))
})
```
Again, the lack of an error means that everything works.  Let's review:

* I used the original code to get a data frame into a variable
* I wrote a unit test against the original code
* I tested my unit test against the data frame variable produced by the original code
* Finally, I wrote and tested the new code.

The circle is now complete.

As noted in the comment in the above *clean_text_file_and_return_data_frame* function, to get the test to pass I had to rename my cleaned_file variable to x before I converted to a data frame and called the unlist function.

I have remedied that situation below.
```{r clean_text_file_and_return_data_frame fixed, echo=TRUE}
 clean_text_file_and_return_data_frame<-function(file){
  
  file<-base::scan(file, what="", sep="\n")
  cleaned_file<-clean(file)
    
  return(cleaned_file)
}
```
This code is much more readable and follows the single responsibility principle.  Now we need a whole new set of unit tests.

For the bayesClassifer function I am going to make lots of changes.  Not only am I going to refactor the code so that it abides by the single responsibility principle but I am also going to combine all of these functions into one call.  This means that the new bayesClassifer function will call clean_text_file_and_return_data_frame.  All the user will have to do is provide it with the text files for the male and female quotes (training data) as well as the test quote and the priors.  Let's get started.

Just as before we first need a couple of unit test that work on the current code so that we can test the new code.  I've created two unit test text files to use as training data, one has a single female quote, the other a single male quote.  I will then use those same quotes as the test quote so that we are assured that we return Male, and Female when we want to. 

#### bayesClassifier unit test
```{r bayesClassifier unit test, echo=TRUE}
# First our input data frames, using our new clean_text_file_and_return_data_frame function
menClass<-clean_text_file_and_return_data_frame("~/naive-bayes-classifier/men_unit_test.txt")
womenClass<-clean_text_file_and_return_data_frame("~/naive-bayes-classifier/women_unit_test.txt")

#then I'm going to make the classifier output the string "Male"
test_that('bayesClassifier classifies', {
  womenQuote<-clean_text_file_and_return_data_frame("~/naive-bayes-classifier/women_unit_test_quote.txt")  
  expect_that(bayesClassifier(menClass, womenClass, womenQuote, .5, .5), equals("Male"))
})

#second the string "Female"
test_that('bayesClassifier classifies', {
  menQuote<-clean_text_file_and_return_data_frame("~/naive-bayes-classifier/men_unit_test_quote.txt")
  
  expect_that(bayesClassifier(menClass, womenClass, menQuote, .5, .5), equals("Female"))
})
```
And I have passing unit tests.  A smart observer here will realize that I am using the womenQuote string to output "Male" and vice versa.  This is because of the way the Naive bayes Classifier works.  We need large training datasets for it to be accurate, such that the frequency of the words in each training data set that are also in the test data set are high.  Since that is not the case here I get the reversed output of what one would expect.  The accuracy of my Naive Bayes Classifier is beyond the scope of this blog post.  Time to refactor.

#### Refactored bayes_classifier function
```{r bayes_classifier refactored, echo=TRUE}
get_count<-function(df){
  return(nrow(df))
}

combine_dataframes_and_make_table<-function(df1, df2){
  all<-rbind(df1, df2)
  return(as.data.frame(table(all)))
}

collapse_to_table<-function(df){
  return(as.data.frame(table(df)))
}

find_intersections<-function(df1, df2){
  return(df1[is.element(df1$df, intersect(df2$`unlist(x)`, df1$df)),])
}

get_conditional_probabilities<-function(intersections, count1, count2, smoothing){
  (intersections$Freq+0)/(count1+count2+smoothing)
}

get_posterior<-function(intersects, prior){
  return(prod(intersects$Freq)*prior)
}

bayes_classifier<-function(men_train, women_train, quote_test, men_prior, women_prior, smoothing=0){
  
  men_class<-clean_text_file_and_return_data_frame(men_train)
  women_class<-clean_text_file_and_return_data_frame(women_train)
  quote_class<-clean_text_file_and_return_data_frame(quote_test)
  
  men_count<-get_count(men_class)
  women_count<-get_count(women_class)
  
  all_words<-combine_dataframes_and_make_table(men_class, women_class)
  all_words_count<-get_count(all_words)
  
  men_class<-collapse_to_table(men_class)
  women_class<-collapse_to_table(women_class)
  
  intersects_men<-find_intersections(men_class, quote_class)
  intersects_women<-find_intersections(women_class, quote_class)

  intersects_men$Freq<-get_conditional_probabilities(intersects_men, men_count, all_words_count, smoothing)
  intersects_women$Freq<-get_conditional_probabilities(intersects_women, women_count, all_words_count, smoothing)

  posterior_men<-get_posterior(intersects_men, men_prior)
  posterior_women<-get_posterior(intersects_women, women_prior)
  
  if(posterior_women>posterior_men){
    return("Female")
  }
  return("Male")
}
```
#### Now retest with the modified unit tests
```{r bayes_classifier unit test modified, echo=TRUE}
# First our input data frames, using our new clean_text_file_and_return_data_frame function
men_train<-"~/naive-bayes-classifier/men_unit_test.txt"
women_train<-"~/naive-bayes-classifier/women_unit_test.txt"

#then I'm going to make the classifier output the string "Male"
test_that('bayesClassifier classifies', {
  women_quote<-"~/naive-bayes-classifier/women_unit_test_quote.txt"  
  expect_that(bayes_classifier(men_train, women_train, women_quote, .5, .5), equals("Male"))
})

#second the string "Female"
test_that('bayesClassifier classifies', {
  men_quote<-"~/naive-bayes-classifier/men_unit_test_quote.txt"
  expect_that(bayes_classifier(men_train, women_train, men_quote, .5, .5), equals("Female"))
})
```
I have passing unit tests.  Note: I did change the unit tests a bit to take into account the new functionality of being able to take in raw text files.  This is better because now all I have to do is call the bayes_classifier function.

I wasn't able to make this code much shorter but it is much more readable with descriptive names for the functions.  I will probably go back and rework the bayes_classifier function to see what else I can do with it at a later date.

I could also now write a bunch more unit tests for all of the new functions I made but this post is already getting way too long.  Next time I promise I will have visuals, perhaps something to do with Benford's Law.
