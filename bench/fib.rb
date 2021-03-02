def fib n
  n < 3 ? 1 : fib(n-1)+fib(n-2)
end
puts fib 32
