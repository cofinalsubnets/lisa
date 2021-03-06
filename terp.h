#define insts(_)\
  _(arity),  _(tcnum),  _(tchom),   _(tctwo),  _(lbind),\
  _(immv),   _(argn),   _(clon),    _(locn),   _(take),\
  _(prel),   _(setl),   _(pc0),     _(pc1),    _(clos),\
  _(encll),  _(encln),  _(yield),   _(ret),    _(jump),\
  _(branch), _(barnch), _(call),    _(recur),  _(loop),\
  _(tbind),  _(push),   _(add),     _(sub),    _(mul),\
  _(dqv),    _(mod),    _(neg),     _(lt),     _(lteq),\
  _(eq),     _(gteq),   _(gt),      _(twopp),  _(numpp),\
  _(nilpp),  _(strpp),  _(tblpp),   _(sympp),  _(hompp),\
  _(car),    _(cdr),    _(setcar),  _(setcdr), _(cons),\
  _(add_u),  _(sub_u),  _(mul_u),   _(div_u),  _(mod_u),\
  _(lt_u),   _(lteq_u), _(eq_u),    _(gteq_u), _(gt_u),\
  _(twop_u), _(nump_u), _(homp_u),  _(tblp_u), _(strp_u),\
  _(nilp_u), _(car_u),  _(cdr_u),   _(cons_u),\
  _(strmk),  _(strg),   _(strl),\
  _(setcar_u), _(setcdr_u),_(globs),\
  _(symp_u), _(emse), _(hom_u), _(pc_u),\
  _(or_u), _(and_u), _(zzz),\
  _(tbll), _(tblmk),_(tblg),_(tblc),_(tbls),_(tbld),_(tblks),\
  _(hom_seek_u),_(hom_geti_u),_(hom_seti_u),\
  _(fail),_(fail_u),_(ccc_u),_(cont),_(vararg),_(tuck),\
  _(drop),_(hom_getx_u),_(hom_setx_u),_(em_u),_(ev_u),_(ap_u)
#define ninl(x) x NoInline
terp insts(ninl);
#undef ninl
