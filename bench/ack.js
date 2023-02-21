function ack(m, n) {
  return m < 1 ? n + 1 : ack(m - 1, n > 0 ? ack(m, n - 1) : 1);
}
console.log(ack(3, 9));
