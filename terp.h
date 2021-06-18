// this is a cool way to do "static data", i got it from luajit :)
#define insts(_)\
  _(tget),   _(tset),   _(thas),   _(tlen),   _(gsym_u),\
  _(arity),  _(idZ),    _(idH),    _(id2),    _(idT),\
  _(imm),    _(arg),    _(clo),    _(loc),    _(take),\
  _(locals), _(loc_),   _(pc0),    _(pc1),    _(clos),\
  _(encll),  _(encln),  _(yield),  _(ret),    _(jump),\
  _(branch), _(barnch), _(call),   _(rec),    _(lbind),\
  _(sar_u), _(sal_u), _(band_u), _(bor_u), _(bxor_u),\
  _(tbind),  _(push),   _(add),    _(sub),    _(mul),\
  _(dqv),    _(mod),    _(neg),    _(lt),     _(lteq),\
  _(eq),     _(gteq),   _(gt),     _(twopp),  _(numpp),\
  _(nilpp),  _(strpp),  _(tblpp),  _(sympp),  _(hompp),\
  _(car),    _(cdr),    _(cons),   _(vecpp),  _(hom_u),\
  _(add_u),  _(sub_u),  _(mul_u),  _(div_u),  _(mod_u),\
  _(lt_u),   _(lteq_u), _(eq_u),   _(gteq_u), _(gt_u),\
  _(twop_u), _(nump_u), _(homp_u), _(tblp_u), _(strp_u),\
  _(nilp_u), _(car_u),  _(cdr_u),  _(cons_u), _(vecp_u),\
  _(strmk),  _(strg),   _(strl),   _(strs),   _(strconc),\
  _(symp_u), _(unit),   _(one),    _(zero),   _(hfin_u),\
  _(arg0),   _(arg1),   _(loc0),   _(loc1),   _(clo0),\
  _(clo1),   _(brlt),   _(brlteq), _(breq),   _(brgteq),\
  _(brgt),   _(brne),   _(tbll),   _(tblmk),\
  _(tblg),   _(tblc),   _(tbls),   _(tbld),   _(tblks),\
  _(hom_seek_u), _(hom_geti_u), _(hom_getx_u),\
  _(fail),   _(ccc_u),  _(cont),   _(vararg), _(tuck),\
  _(dupl),   _(emi),    _(drop),   _(emx_u),  _(emi_u),\
  _(emx),    _(em_u),   _(ev_u),   _(ap_u), _(rnd_u)
#define prims(_)\
  _("A", car_u),     _("B", cdr_u),          _("X", cons_u),    _("=", eq_u),\
  _("<", lt_u),      _("<=", lteq_u),        _(">", gt_u),      _(">=", gteq_u),\
  _(">>", sar_u), _("<<", sal_u), _("&", band_u), _("|", bor_u), _("^", bxor_u),\
  _("+", add_u),     _("-", sub_u),          _("*", mul_u),     _("/", div_u),\
  _("%", mod_u),     _("ap", ap_u),          _("ccc", ccc_u),   _("ev", ev_u),\
  _("fail", fail),   _("tbl", tblmk),        _("tget", tblg),   _("tset", tbls),\
  _("thas", tblc),   _("tdel", tbld),        _("tkeys", tblks), _("tlen", tbll),\
  _("slen", strl),   _("sget", strg),        _("scat", strconc),   _("ssub", strs),\
  _("str", strmk),   _("gensym", gsym_u),    _(".", em_u),\
  _("vecp", vecp_u), _("nump", nump_u),      _("symp", symp_u), _("twop", twop_u),\
  _("tblp", tblp_u), _("strp", strp_u),      _("nilp", nilp_u), _("homp", homp_u),\
  _("hom", hom_u),   _("hseek", hom_seek_u), _("emx", emx_u),   _("hgetx", hom_getx_u),\
  _("emi", emi_u),   _("hgeti", hom_geti_u), _("hfin", hfin_u), _("rand", rnd_u)

#define ninl(x) x NoInline
terp insts(ninl);
