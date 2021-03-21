// here is some "static data". this idea came from luajit.
#define insts(_)\
  _(tget),_(tset),_(gsym_u),\
  _(arity),  _(idnum),  _(idhom),   _(idtwo),  _(idtbl), _(lbind),\
  _(immv),   _(argn),   _(clon),    _(locn),   _(take),\
  _(prel),   _(setl),   _(pc0),     _(pc1),    _(clos),\
  _(encll),  _(encln),  _(yield),   _(ret),    _(jump),\
  _(branch), _(barnch), _(call),    _(rec),\
  _(tbind),  _(push),   _(add),     _(sub),    _(mul),\
  _(dqv),    _(mod),    _(neg),     _(lt),     _(lteq),\
  _(eq),     _(gteq),   _(gt),      _(twopp),  _(numpp),\
  _(nilpp),  _(strpp),  _(tblpp),   _(sympp),  _(hompp),\
  _(car),    _(cdr),   _(cons), _(vecpp),\
  _(add_u),  _(sub_u),  _(mul_u),   _(div_u),  _(mod_u),\
  _(lt_u),   _(lteq_u), _(eq_u),    _(gteq_u), _(gt_u),\
  _(twop_u), _(nump_u), _(homp_u),  _(tblp_u), _(strp_u),\
  _(nilp_u), _(car_u),  _(cdr_u),   _(cons_u), _(vecp_u),\
  _(strmk),  _(strg),   _(strl),_(strs),_(strc),_(hom_fin_u),\
  _(symp_u), _(emse), _(hom_u), _(pc_u),\
  _(arg0), _(arg1), _(loc0),_(loc1),_(clo0),_(clo1),\
  _(unit),_(one),_(zero),\
  _(brlt),_(brlteq),_(breq),_(brgteq),_(brgt),_(brne),\
  _(zzz),\
  _(tbll), _(tblmk),_(tblg),_(tblc),_(tbls),_(tbld),_(tblks),\
  _(hom_seek_u),_(hom_geti_u),_(emi),\
  _(fail),_(ccc_u),_(cont),_(vararg),_(tuck),_(dupl),\
  _(rd_u),\
  _(drop),_(hom_getx_u),_(emx_u),_(emi_u),_(emx),_(em_u),_(ev_u),_(ap_u)
#define prims(_)\
  _("gensym", gsym_u),\
  _("hfin", hom_fin_u),\
  _("read", rd_u),   _(".", em_u),\
  _("A", car_u),    _("B", cdr_u),\
  _("X", cons_u),   _("=", eq_u),\
  _("<", lt_u),      _("<=", lteq_u),\
  _(">", gt_u),      _(">=", gteq_u),\
  _("+", add_u),     _("-", sub_u),\
  _("*", mul_u),     _("/", div_u),\
  _("%", mod_u),     _("ap", ap_u),\
  _("ccc", ccc_u),   _("ev", ev_u),\
  _("fail", fail),   _("tbl", tblmk),\
  _("tget", tblg),   _("tset", tbls),\
  _("thas", tblc),   _("tdel", tbld),\
  _("tkeys", tblks), _("tlen", tbll),\
  _("slen", strl),   _("sget", strg),\
  _("scat", strc),   _("ssub", strs),\
  _("str", strmk),   _(".c", pc_u),\
  _("hom", hom_u),   _("hseek", hom_seek_u),\
  _("emx", emx_u),     _("hgetx", hom_getx_u),\
  _("emi", emi_u),     _("hgeti", hom_geti_u),\
  _("zzz", zzz),\
  _("vecp", vecp_u), _("nump", nump_u),\
  _("symp", symp_u), _("twop", twop_u),\
  _("tblp", tblp_u), _("strp", strp_u),\
  _("nilp", nilp_u), _("homp", homp_u)

#define ninl(x) x NoInline
terp insts(ninl);
#undef ninl
#define vm_op(n,...) NoInline obj n(vm v,hom ip,mem fp,mem sp,mem hp,obj xp,##__VA_ARGS__)
vm_op(panic, const char*, ...);
#define Pack() (Ip=Ph(ip),Sp=sp,Hp=hp,Fp=fp,Xp=xp)
#define Unpack() (fp=Fp,hp=Hp,sp=Sp,ip=Gh(Ip),xp=Xp)
#define Jump(f,...) return (f)(v,ip,fp,sp,hp,xp,##__VA_ARGS__)
#define Have(n) if (avail < n) Jump((Xp=n,gc))
#define CallC(...)(Pack(),(__VA_ARGS__),Unpack())
#define Cont(n, x) return ip+=n,xp=x,G(ip)(v,ip,fp,sp,hp,xp)
#define Ap(f,x) return G(f)(v,f,fp,sp,hp,x)
#define Go(f,x) return f(v,ip,fp,sp,hp,x)
#define Next(n) Ap(ip+n,xp)

#define ff(x)((fr)(x))
#define Locs fp[-1]
#define Clos ff(fp)->clos
#define Retp ff(fp)->retp
#define Subd ff(fp)->subd
#define Argc ff(fp)->argc
#define Argv ff(fp)->argv
typedef struct fr { obj clos, retp, subd, argc, argv[]; } *fr;
#define E_TYPE  "wrong type : %s for %s"
#define E_ARITY "wrong arity : %ld of %ld"
#define TypeCheck(x,t) if(kind(x)!=t)Jump(panic,E_TYPE, tnom(kind(x)), tnom(t))
#define Arity(n) if(n>Argc)Jump(panic,E_ARITY, getnum(Argc), getnum(n))
#define ArityCheck(n) Arity(putnum(n))
