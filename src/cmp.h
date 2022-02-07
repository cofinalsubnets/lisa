u1 eql_two(obj, obj) NoInline, eql_str(obj, obj) NoInline;
static Inline u1 eql(obj a, obj b) {
  if (a == b) return true;
  if (kind(a) != kind(b)) return false;
  if (kind(a) == Two) return eql_two(a, b);
  if (kind(a) == Str) return eql_str(a, b);
  return false; }
