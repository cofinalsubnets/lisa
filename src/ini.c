#include "i.h"


uintptr_t liprng(li v) {
  const intptr_t steele_vigna_2021 = 0xaf251af3b0f025b5;
  uintptr_t r = (steele_vigna_2021 * v->rand + 1) >> 8;
  return v->rand = r; }

static str strof(la v, const char* c) {
  size_t bs = strlen(c);
  str o = cells(v, Width(struct str) + b2w(bs));
  if (!o) return 0;
  memcpy(o->text, c, bs);
  return str_ini(o, bs); }

static sym symofs(la v, const char *s) {
  str _ = strof(v, s);
  return _ ? symof(v, _) : 0; }

static bool li_ini_env(la);

enum status vm_exit(li v, enum status r) { return v->status = r; }
enum status vm_loop(li v, enum status r) {
  return report(v, r), li_go(v); }

NoInline enum status li_ini(struct V *v) {
  memset(v, 0, sizeof(struct V));
  const size_t len = 1 << 10; // power of 2
                         //
  ob *pool = malloc(len * sizeof(ob));
  if (pool) {
    v->len = len,
    v->hp = v->pool = pool,
    v->fp = (sf) (v->sp = pool + len),
    v->rand = v->t0 = clock();
    v->yield = vm_exit;
    if (li_ini_env(v)) return Ok;
    li_fin(v); }

  return OomError; }

void li_fin(struct V *v) {
  if (v) free(v->pool), v->pool = NULL; }

static bool
  defprim(la, vm*, const char*) NoInline,
  inst(la, const char*, vm*);
static bool li_ini_stack(li v) {
  if (Avail < Width(struct frame) &&
      !please(v, Width(struct frame)))
    return false;
  ob *top = v->pool + v->len;
  v->fp = (frame) top - 1;
  v->sp = (ob*) v->fp;
  v->fp->subd = v->fp;
  v->fp->argc = 0;
  v->fp->retp = 0;
  v->fp->clos = 0;
  return true; }
#define reg_prim(go, nom) && defprim(v, go, nom)
#define reg_inst(a) && inst(v, "i-"#a, a)
static bool li_ini_vm(li v) {
  ob _; return
    (v->lex.topl = mktbl(v)) VM1(reg_inst) &&
    (v->lex.macros = mktbl(v)) &&
    (_ = (ob) symofs(v, "_ns")) &&
    tbl_set(v, v->lex.topl, _, (ob) v->lex.topl) &&
    (_ = (ob) symofs(v, "macros")) &&
    tbl_set(v, v->lex.topl, _, (ob) v->lex.macros)
    VM2(reg_prim) &&
    li_ini_stack(v); }


static bool li_ini_env(struct V* v) {
  struct sym *y; return
    (y = symofs(v, "ev"), v->lex.eval = y) &&
    (y = symofs(v, ":"), v->lex.define = y) &&
    (y = symofs(v, "?"), v->lex.cond = y) &&
    (y = symofs(v, "\\"), v->lex.lambda = y) &&
    (y = symofs(v, "`"), v->lex.quote = y) &&
    (y = symofs(v, ","), v->lex.begin = y) &&
    (y = symofs(v, "."), v->lex.splat = y) &&
    li_ini_vm(v); }

static NoInline bool
defprim(struct V *v, vm *i, const char *n) {
  mo k; sym y; return
    (y = symofs(v, n)) &&
    (k = thd(v, i, y, NULL)) &&
    tbl_set(v, v->lex.topl, (ob) GF(k), (ob) k); }

// store an instruction address under a variable in the
// toplevel namespace // FIXME use a different namespace
static NoInline bool inst(la v, const char *a, vm *b) {
  sym z; return
    (z  = symofs(v, a)) &&
    tbl_set(v, v->lex.topl, (ob) z, (ob) b); }
