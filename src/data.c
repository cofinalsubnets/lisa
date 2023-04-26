#include "i.h"

struct methods
  two_methods = {
    .evac = cp_two,
    .walk = wk_two,
    .emit = tx_two,
    .equi = eq_two,
  },
  str_methods = {
    .evac = cp_str,
    .walk = wk_str,
    .emit = tx_str,
    .equi = eq_str,
  };
