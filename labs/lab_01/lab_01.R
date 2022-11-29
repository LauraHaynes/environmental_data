c_1 = c(1, 2, 3)
c_2 = "c(1, 2, 3)"
c_1
my_vec <- 1:6
my_vec
mat_1 = matrix(my_vec, nrow = 3)
mat_1
mat_1[3]
mat_2 = matrix(my_vec, nrow = 2, ncol=3)
mat_2
mat_3 = matrix(my_vec, nrow=3, ncol=2)
mat_3
mat_4 = matrix(my_vec, nrow=4, ncol=5)
mat_4

my_list_1 <- list(5.2, "five point two", 0:5)
names(my_list_1) <- c("two", "one", "three")

my_list_1[[1]]
my_list_1[[as.numeric("1")]]
my_list_1[["1"]]
my_list_1[["one"]]
my_list_1$one
my_list_1$"one"
my_list_1$1
my_list_1$"1"
