let isDivisible(dividend, divisor) =
  remainder of divident / divisor = 0

let fizzBuzz(number) =
  if isDivisible(number, 15) then
    "FizzBuzz"
  else if isDivisible(number, 3) then
    "Fizz"
  else if isDivisible(number, 5) then
    "Buzz"
  else
    toString(number)
  end

ref number = 1

repeat while number <= 100
  output(fizzBuzz(number))
  number = number + 1
end
