void
initialize_mrs() {

  extern void init_LKB_PACKAGE(cl_object);
  extern void init_MRS_PACKAGE(cl_object);
  extern void init_ECL(cl_object);
  extern void init_PET_INTERFACE(cl_object);
  extern void init_MRSGLOBALS(cl_object);
  extern void init_BASEMRS(cl_object);
  extern void init_MRSOUTPUT(cl_object);
  extern void init_MRSCORPUS(cl_object);
  extern void init_INTERFACE(cl_object);
  extern void init_MRSRESOLVE(cl_object);
  extern void init_MRSCONS(cl_object);
  extern void init_CHEAPSCOPE(cl_object);
  extern void init_BASERMRS(cl_object);
  extern void init_COMP(cl_object);
  extern void init_CONVERT(cl_object);
  extern void init_ANNLT(cl_object);
  extern void init_OUTPUT(cl_object);
  extern void init_READGRAM(cl_object);
  extern void init_READTAG(cl_object);
  extern void init_INPUT(cl_object);
  extern void init_COMPARE(cl_object);
  extern void init_DEPENDENCIES(cl_object);
  read_VV(OBJNULL, init_LKB_PACKAGE);
  read_VV(OBJNULL, init_MRS_PACKAGE);
  read_VV(OBJNULL, init_ECL);
  read_VV(OBJNULL, init_PET_INTERFACE);
  read_VV(OBJNULL, init_MRSGLOBALS);
  read_VV(OBJNULL, init_BASEMRS);
  read_VV(OBJNULL, init_MRSOUTPUT);
  read_VV(OBJNULL, init_MRSCORPUS);
  read_VV(OBJNULL, init_INTERFACE);
  read_VV(OBJNULL, init_MRSRESOLVE);
  read_VV(OBJNULL, init_MRSCONS);
  read_VV(OBJNULL, init_CHEAPSCOPE);
  read_VV(OBJNULL, init_BASERMRS);
  read_VV(OBJNULL, init_COMP);
  read_VV(OBJNULL, init_CONVERT);
  read_VV(OBJNULL, init_ANNLT);
  read_VV(OBJNULL, init_OUTPUT);
  read_VV(OBJNULL, init_READGRAM);
  read_VV(OBJNULL, init_READTAG);
  read_VV(OBJNULL, init_INPUT);
  read_VV(OBJNULL, init_COMPARE);
  read_VV(OBJNULL, init_DEPENDENCIES);

} /* initialize_mrs() */
