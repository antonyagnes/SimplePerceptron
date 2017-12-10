#load iris dataset by deafult
data(iris)
iris_subset = iris[c(1,3,5)]
#declares setosa as -1 and virginica and virsicolor as 1
iris_subset[,4] = 1
iris_subset[iris_subset[,3] == "setosa",4] = -1
#loads x and y values
x = iris_subset[,c(1,2)]
y = iris_subset[,4]

weight_vector = function(x){
  return (rep(0,x+1))
}
step_function = function(x){
  if (x<0)
    y = -1
  else
    y = 1
  return (y)
}
relu_function = function(x){
  relu = max(0,x)
  return (relu)
}
sigmoid_function = function(x){
  y = 1/(1+exp(-x))
  return(y)
}
gaussian_function = function(x){
  y = exp((-x^2)/2)
  return (y)
}
get_activation_function = function(){
  print("1 - step function , 2 - relu ,3 - sigmoid, 4 - gaussian")
  print("choose one activation function - enter its respective number")
  at_func = readline(prompt = "activation function")
  return(as.integer(at_func))
}
#perceptron function has a deafault learning rate as 1 and default number of iterations as 10
#enter perceptron(x,y,1,10)
perceptron = function(x,y,learning_rate = 1,num_of_iterations = 10){
  n = get_activation_function()
  wt = weight_vector(dim(x)[2])
  for (num in 1:num_of_iterations){
    for(i in 1:length(y)){
      z = sum(wt[2:length(wt)]*as.numeric(x[i,]))+wt[1]
      if(n == 1){
        k = step_function(z)
      }
      if(n == 2){
        k = relu_function(z)
      }
      if(n == 3){
        k = sigmoid_function(z)
      }
      if( n == 4){
        k = gaussian_function(z)
      }
      change_in_wt = learning_rate * (y[i] - k)*c(1,as.numeric(x[i,]))
      wt = wt + change_in_wt
    }
  }
  return (wt)
}
