typedef u0 emitter(lips, FILE*, obj);
emitter emhom, emvec, emtwo, emnum, emsym, emstr, emtbl, emit;
u0 write_file(lips, const char*, const char*),
   ems(lips, FILE*, obj, char),
   emsep(lips, obj, FILE*, char);
