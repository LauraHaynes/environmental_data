n = 12345
vec_1 = sample(12, n, replace = TRUE)
head(vec_1)
vec_2 = vec_1 == 3
vec_2
length(vec_1)
sum(vec_1==3)
n = 10
vec_1 = sample(12, n, replace = TRUE)
paste0("Sum of elements with value 3: ", sum(vec_1 == 3))

n = 45
for (i in 1:n)
{
  print(
    paste0("This is loop iteration:", i))
}

n = 17
vec_1 = sample(10, n, replace=TRUE)
for (i in 1:n)
{
  print(
    paste0("The element of vec_1 at index ", i , " is ",
           vec_1[i]))
}

n=14
min = 1
max = 10

create_and_print_vec = function(n, min = 1, max = 10)
{
  vec_3 = sample(min:max, n, replace = TRUE)
   for (i in 1:n)
   {
    print(paste0("The element at index ", i , " is ",
             vec_3[i]))
   }
    
}

create_and_print_vec(14, min = 100, max = 1000)

