# irem yuvali | 2017555071

set.seed(2017555071)

library(tidyverse)
library(stringr)

# Question 1

prime_numbers <- c()
non_prime_numbers<- c()
flag=0

prime_num <- function(x){
  
  if(x==2){
    flag=1 # prime
  }
  
  if(x>1 && x!=2){
    flag=1
    for (i in seq(2,x-1)) {
      if(x%%i==0){
        flag=0  # not prime
        break
      }
    }
  }
  
  if (flag==1) {
    print(paste(x," is a prime number"))
    prime_numbers <- c(x)
  }
  
  if(flag==0){
    print(paste(x,"is not a prime number"))
    non_prime_numbers <- c(x)
  }

}

prime_factor <- function(x, i=2, p_factors = NULL){
  if(x<i){
    p_factors
  } 
  else if(! x %% i){
    prime_factor(x/i, i, c(p_factors, i))
  } 
  else {
    prime_factor(x, i+1, p_factors)
  }
}


for(i in seq(1,25)){
  prime_num(i)
  }


prime_numbers
non_prime_numbers


## it should be "non_prime_numbers" instead of "nop" below.
## My function can specify the prime/non-prime numbers
## but the values do not accumulate in these variables (prime_numbers and non_prime_numbers) 
## so they are null now(I cannot figure it out)
## so I'm testing my code for second part. actually its prime_factor part is working too.

nop <- c(597,931,1083)

for(i in 1:length(nop)){
  print(prime_factor(nop[i]))
}


# Question 2

data(sentences)
 
my_sample <- sample(sentences, 5, replace=TRUE)

my_sample <- unlist(str_split(my_sample, " ", simplify = TRUE), stringr::boundary("word"))

my_sample

ordered_words <- function(x){
  
  x %>% 
    str_length() %>% 
    order() %>% 
    x[.] %>% 
    str_c(collapse = " ") %>% 
    str_to_lower()
  
}

ordered_words(my_sample)



