def ack m, n
  m < 1 ? n + 1 : ack(m - 1, n > 0 ? ack(m, n - 1) : 1)
end
puts ack 3, 9
