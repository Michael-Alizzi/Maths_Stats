linear_funct <- function(a, x, b){
  
  y = a*x+b
  
  return(y)
  
}

attr(linear_funct, "Output of a Linear Equation") <- "General linear formular using a, x and b"

linear_inverse_funct <- function(a, b){
  
  Y_inverse_str <- sprintf("Solve(y == %s*x +%s, x)", a, b)
  
  f_inv = yac_str(Y_inverse_str)
  
  Y_input <- gsub("\\{x==", "",  gsub("}", "", f_inv)) 
  
  math_exp <- parse(text = Y_input)[[1]]
  
  return(math_exp)
  
}

attr(linear_inverse_funct, "Inverse formula of Linear Equation") <- "Input the a and b in the general linear formula: y = a*x+b"

derivative_funct <- function(math_expr, y){
  
  derivative <- eval(D(math_expr, y))
  
  return(derivative)
  
}

attr(derivative_funct, "Derivative of Equation") <- "Input the math_expr parameter as expression(formula) and the y parameter as the derivative you are trying to solve (as a string) to solve for the derivative (rate of change)."