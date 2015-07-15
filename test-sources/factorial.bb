# Recursive

let factorial(number) =
  if number = 0 then
    1
  else
    number * factorial(number - 1)
  end

# Iterative

let factorial(number) =
  ref factorial = 1
  ref counter = 1
  repeat number times
    factorial = restult * counter
    counter = counter + 1
  end
  factorial
