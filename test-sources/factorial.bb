# Recursive

let factorial(number) =
  return if number = 0 then
    1
  else
    number * factorial(number - 1)
  end

# Iterative

let factorial(number) =
  ref result = 1
  ref counter = number
  repeat while counter > 0
    result = restult * counter
    counter = counter - 1
  end
  return result
