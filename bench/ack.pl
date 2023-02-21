sub ack {
  my $m = $_[0];
  my $n = $_[1];
  if ($m == 0) {
    return $n + 1; }
  if ($n == 0) {
    return ack($m-1, 1); }
  return ack($m-1, ack($m, $n-1)); }
print ack(3, 9)
