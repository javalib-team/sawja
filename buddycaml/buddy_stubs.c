/*
 * buddy-ocaml - OCaml bindings to buddy
 * Copyright (C) 2009 Frédéric Besson
 * 
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */
#include <limits.h>
#include <caml/bigarray.h>
#include <caml/alloc.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <assert.h>

#include <bdd.h>
#include <fdd.h>
#include <stdio.h>


#define BDD_val(v) (*((BDD *) (Data_custom_val(v))))



static void finalize_bdd(value block) {
  bdd_delref(BDD_val(block));
}

static int compare_bdd(value x, value y) {
  return BDD_val(x) - BDD_val(y) ; 
}

static long hash_bdd(value bdd) {
  return (long) (Int_val(bdd));
}

static struct custom_operations bdd_custom =
  {
    "ocaml_buddy",
    finalize_bdd,
    compare_bdd,
    hash_bdd,
    custom_serialize_default,
    custom_deserialize_default
  };

static value caml_alloc_bdd(BDD bdd) {
  value block = caml_alloc_custom(&bdd_custom, sizeof(BDD), 0, 1) ;
  * ((BDD *)(Data_custom_val(block))) = bdd ; 
  return block;
}

__inline value caml_realloc_bdd(BDD b1, value bv1, BDD b2, value bv2, BDD res){
  if (res == b1) return bv1;
  else if(res == b2) return bv2;
  else {
    bdd_addref(res);
    return caml_alloc_bdd(res);
  }
}	       

/* Information on BDDs */
CAMLprim value ocaml_bdd_var(value r) {
  CAMLparam1(r);
  CAMLreturn(Val_int(bdd_var(BDD_val(r))));
}

CAMLprim value ocaml_bdd_low(value r){
  CAMLparam1(r);
  BDD low = bdd_low(BDD_val(r));
  bdd_addref(low); 
  CAMLreturn(caml_alloc_bdd(low));
}

CAMLprim value ocaml_bdd_high(value r){
  CAMLparam1(r);
  BDD high = bdd_high(BDD_val(r));
  bdd_addref(high); 
  CAMLreturn(caml_alloc_bdd(high));
}

CAMLprim value ocaml_bdd_support(value r){
  CAMLparam1(r);
  BDD sup = bdd_support(BDD_val(r));
  bdd_addref(sup);
  CAMLreturn(caml_alloc_bdd(sup));
}

CAMLprim value ocaml_bdd_satcount(value r){
  CAMLparam1(r);
  double satcount = bdd_satcount(BDD_val(r));
  CAMLreturn(caml_copy_double(satcount));
}

CAMLprim value ocaml_bdd_satcountset(value b1 , value b2){
  CAMLparam2(b1,b2);
  CAMLreturn(caml_copy_double(bdd_satcountset(BDD_val(b1), BDD_val(b2))));
}

CAMLprim value ocaml_bdd_satcountln(value r){
  CAMLparam1(r);
  CAMLreturn(caml_copy_double(bdd_satcountln(BDD_val(r))));
}

CAMLprim value ocaml_bdd_satcountlnset(value r, value varset){
  CAMLparam2(r,varset);
  CAMLreturn(caml_copy_double(bdd_satcountlnset(BDD_val(r),BDD_val(varset))));
}

CAMLprim value ocaml_bdd_nodecount(value bdd){
  CAMLparam1(bdd);
  CAMLreturn(Val_int(bdd_nodecount(BDD_val(bdd))));
}

CAMLprim value ocaml_bdd_anodecount(value r){
  CAMLparam1(r);
  CAMLreturn(caml_copy_double(bdd_anodecount(Data_bigarray_val(r), (Bigarray_val(r))->dim[0])));
}

/* int * bdd_varprofile -- what is the size of the array ? */

CAMLprim value ocaml_bdd_pathcount(value bdd){
  CAMLparam1(bdd);
  CAMLreturn(caml_copy_double(bdd_pathcount(BDD_val(bdd))));
}


/* Kernel BDD operations and data structures */
static void my_error_hook(int errcode) {
  caml_invalid_argument(bdd_errstring(errcode));
}

CAMLprim value ocaml_bdd_init(value nodenum, value cachesize) {
  CAMLparam2(nodenum,cachesize);
  int i = bdd_init(Int_val(nodenum) , Int_val(cachesize));
  bdd_error_hook(my_error_hook);
  CAMLreturn(Val_int(i));
}

CAMLprim value ocaml_bdd_done(value unit){
  CAMLparam1(unit);
  bdd_done();
  CAMLreturn(Val_unit);
}

CAMLprim value ocaml_bdd_setvarnum(value num){
  CAMLparam1(num);
  CAMLreturn(Val_int(bdd_setvarnum(Int_val(num))));
}

CAMLprim value ocaml_bdd_extvarnum(value num){
  CAMLparam1(num);
  CAMLreturn(Val_int(bdd_extvarnum(Int_val(num))));
}

CAMLprim value ocaml_bdd_isrunning(value unit){
  CAMLparam1(unit);
  CAMLreturn(Val_bool(bdd_isrunning()));
}

CAMLprim value ocaml_bdd_setmaxnodenum(value size){
  CAMLparam1(size);
  CAMLreturn(Val_int(bdd_setmaxnodenum(Int_val(size))));
}

CAMLprim value ocaml_bdd_setcacheratio(value size){
  CAMLparam1(size);
  CAMLreturn(Val_int(bdd_setcacheratio(Int_val(size))));
}


CAMLprim value ocaml_bdd_setmaxincrease(value size){
  CAMLparam1(size);
  CAMLreturn(Val_int(bdd_setmaxincrease(Int_val(size))));
}

CAMLprim value ocaml_bdd_setminfreenodes(value mf){
  CAMLparam1(mf);
  CAMLreturn(Val_int(bdd_setminfreenodes(Int_val(mf))));
}

CAMLprim value ocaml_bdd_getnodenum(value unit){
  CAMLparam1(unit);
  CAMLreturn(Val_int(bdd_getnodenum()));
}

CAMLprim value ocaml_bdd_getallocnum(value unit){
  CAMLparam1(unit);
  CAMLreturn(Val_int(bdd_getallocnum()));
}

CAMLprim value ocaml_bdd_versionstr(value unit){
  CAMLparam1(unit);
  CAMLreturn(caml_copy_string(bdd_versionstr()));
}

CAMLprim value ocaml_bdd_versionnum(value unit){
  CAMLparam1(unit);
  CAMLreturn(Val_int(bdd_versionnum()));
}

CAMLprim value ocaml_bdd_true(value unit) {
  CAMLparam1(unit);
  BDD r = bdd_true() ;
  bdd_addref(r) ; 
  CAMLreturn(caml_alloc_bdd(r));
}

CAMLprim value ocaml_bdd_false(value unit) {
  CAMLparam1(unit);
  BDD r = bdd_false() ;
  bdd_addref(r) ; 
  CAMLreturn(caml_alloc_bdd(r));
}

CAMLprim value ocaml_bdd_varnum(value unit){
  CAMLparam1(unit);
  CAMLreturn(Val_int(bdd_varnum()));
}

CAMLprim value ocaml_bdd_ithvar(value var) {
  CAMLparam1(var);
  BDD v = bdd_ithvar(Int_val(var));
  CAMLreturn(caml_alloc_bdd(v));
}

CAMLprim value ocaml_bdd_nithvar(value var) {
  CAMLparam1(var);
  BDD v = bdd_nithvar(Int_val(var));
  CAMLreturn(caml_alloc_bdd(v));
}

CAMLprim value ocaml_bdd_gbc(value unit){
  CAMLparam1(unit);
  bdd_gbc();
  CAMLreturn(Val_unit);
}

/* BDD operators */

CAMLprim value ocaml_bdd_not(value r){
  CAMLparam1(r);
  BDD bdd = bdd_not(BDD_val(r)) ;
  bdd_addref(bdd);
  CAMLreturn(caml_alloc_bdd(bdd));
}

static int ocaml_bddop [] =  
  {bddop_and, bddop_xor , bddop_or , bddop_nand,
   bddop_nor , bddop_imp , bddop_biimp , bddop_diff,
   bddop_less , bddop_invimp};

CAMLprim value ocaml_bdd_apply(value l, value r, value op){
  CAMLparam3(l,r,op);
  BDD bddl = BDD_val(l);
  BDD bddr = BDD_val(r);

  BDD bdd = bdd_apply(bddl , bddr , ocaml_bddop[Int_val(op)]);
  CAMLreturn(caml_realloc_bdd(bddl,l,bddr,r,bdd));
}

CAMLprim value ocaml_bdd_ite(value f, value g,value h){
  CAMLparam3(f,g,h);
  BDD bdd = bdd_ite(BDD_val(f),BDD_val(g), BDD_val(h));
  bdd_addref(bdd);
  CAMLreturn(caml_alloc_bdd(bdd));
}

CAMLprim value ocaml_bdd_restrict(value r, value var){
  CAMLparam2(r,var);
  BDD bdd = bdd_restrict(BDD_val(r),BDD_val(var));
  bdd_addref(bdd);
  CAMLreturn(caml_alloc_bdd(bdd));
}

CAMLprim value ocaml_bdd_constrain(value f, value c){
  CAMLparam2(f,c);
  BDD bdd = bdd_constrain(BDD_val(f), BDD_val(c));
  bdd_addref(bdd);
  CAMLreturn(caml_alloc_bdd(bdd));
}

CAMLprim value ocaml_bdd_replace_all(value r, value oldvars, value newvars){
  CAMLparam3(r,oldvars,newvars);
  int olen = (Bigarray_val(oldvars))->dim[0] ;
  int nlen = (Bigarray_val(newvars))->dim[0] ;
  if (olen != nlen)
    caml_failwith("bdd_replace incompatible number of variables");
  bddPair* pairs = bdd_newpair() ;
  bdd_setpairs(pairs, Data_bigarray_val(oldvars) , Data_bigarray_val(newvars) , olen);
  BDD bdd = bdd_replace(BDD_val(r),pairs);
  bdd_addref(bdd);
  bdd_freepair(pairs);
  CAMLreturn(caml_alloc_bdd(bdd));
}

CAMLprim value ocaml_bdd_replace(value r, value oldvar, value newvar){
  CAMLparam3(r,oldvar,newvar);
  bddPair* pairs = bdd_newpair() ;
  bdd_setpair(pairs, Int_val(oldvar), Int_val(newvar));
  BDD bdd = bdd_replace(BDD_val(r),pairs);
  bdd_addref(bdd);
  bdd_freepair(pairs);
  CAMLreturn(caml_alloc_bdd(bdd));
}

CAMLprim value ocaml_bdd_compose(value f, value g, value v){
  CAMLparam3(f,g,v);
  BDD bdd = bdd_compose(BDD_val(f) , BDD_val(g) , BDD_val(v));
  bdd_addref(bdd);
  CAMLreturn(caml_alloc_bdd(bdd));
}

CAMLprim value ocaml_bdd_exist(value r, value var){
  CAMLparam2(r,var);
  BDD bdd = bdd_exist(BDD_val(r) , BDD_val(var) ) ;
  bdd_addref(bdd);
  CAMLreturn(caml_alloc_bdd(bdd));
}

CAMLprim value ocaml_bdd_forall(value r, value var){
  CAMLparam2(r,var);
  BDD bdd = bdd_forall(BDD_val(r) , BDD_val(var) ) ;
  bdd_addref(bdd);
  CAMLreturn(caml_alloc_bdd(bdd));
}

CAMLprim value ocaml_bdd_unique(value r, value var){
  CAMLparam2(r,var);
  BDD bdd = bdd_unique(BDD_val(r) , BDD_val(var) ) ;
  bdd_addref(bdd);
  CAMLreturn(caml_alloc_bdd(bdd));
}

CAMLprim value ocaml_bdd_appex(value l, value r, value op,value var){
  CAMLparam4(l,r,op,var);
  BDD bdd = bdd_appex(BDD_val(l) , BDD_val(r), ocaml_bddop[Int_val(op)], BDD_val(var) ) ;
  bdd_addref(bdd);
  CAMLreturn(caml_alloc_bdd(bdd));
}

CAMLprim value ocaml_bdd_appall(value l, value r, value op,value var){
  CAMLparam4(l,r,op,var);
  BDD bdd = bdd_appall(BDD_val(l) , BDD_val(r), ocaml_bddop[Int_val(op)], BDD_val(var) ) ;
  bdd_addref(bdd);
  CAMLreturn(caml_alloc_bdd(bdd));
}

/* Variable reordering */

CAMLprim value ocaml_bdd_swapvar(value v1, value v2){
  CAMLparam2(v1,v2);
  CAMLreturn(Val_int(bdd_swapvar(Int_val(v1),Int_val(v2))));
}

static int ocaml_method [] ={
  BDD_REORDER_WIN2,
  BDD_REORDER_WIN2ITE,
  BDD_REORDER_WIN3,
  BDD_REORDER_WIN3ITE,
  BDD_REORDER_SIFT,
  BDD_REORDER_SIFTITE,
  BDD_REORDER_RANDOM,
  BDD_REORDER_NONE
};

__inline int ocaml_encode_method(int method){
  switch(method){
  case BDD_REORDER_WIN2 : return 0;
  case BDD_REORDER_WIN2ITE : return 1;
  case BDD_REORDER_WIN3 : return 2;
  case BDD_REORDER_WIN3ITE : return 3;
  case BDD_REORDER_SIFT : return 4;
  case BDD_REORDER_SIFTITE : return 5;
  case BDD_REORDER_RANDOM : return 6;
  case BDD_REORDER_NONE : return 7;
 default : assert(0);
  }
}


CAMLprim value ocaml_bdd_reorder(value method){
  CAMLparam1(method);
  bdd_reorder(ocaml_method[Int_val(method)]);
  CAMLreturn(Val_unit);
}

CAMLprim value ocaml_bdd_reorder_gain(value unit){
  CAMLparam1(unit);
  CAMLreturn(Val_int(bdd_reorder_gain()));
}



CAMLprim value ocaml_bdd_clrvarblocks(value unit){
  CAMLparam1(unit);
  bdd_clrvarblocks();
  CAMLreturn(Val_unit);
}

static int ocaml_reorder_fixed[] = {BDD_REORDER_FREE,BDD_REORDER_FIXED};

CAMLprim value ocaml_bdd_addvarblock(value b,value fixed){
  CAMLparam2(b,fixed);
  CAMLreturn(Val_int(bdd_addvarblock(BDD_val(b),ocaml_reorder_fixed[Bool_val(fixed)])));
}

CAMLprim value ocaml_bdd_intaddvarblock(value first, value last,value fixed){
  CAMLparam3(first,last,fixed);
  CAMLreturn(Val_int(bdd_intaddvarblock(Int_val(first),Int_val(last),ocaml_reorder_fixed[Bool_val(fixed)])));
}

CAMLprim value ocaml_bdd_varblockall(value unit){
  CAMLparam1(unit);
  bdd_varblockall();
  CAMLreturn(Val_unit);
}

CAMLprim value ocaml_bdd_autoreorder(value method){
  CAMLparam1(method);
  CAMLreturn(Val_int(ocaml_encode_method(bdd_autoreorder(ocaml_method[Int_val(method)]))));
}


CAMLprim value ocaml_bdd_autoreorder_times(value method,value num){
  CAMLparam2(method,num);
  CAMLreturn(Val_int(ocaml_encode_method(bdd_autoreorder_times(ocaml_method[Int_val(method)], Int_val(num)))));
}

CAMLprim value ocaml_bdd_var2level(value var){
  CAMLparam1(var);
  CAMLreturn(Val_int(bdd_var2level(Int_val(var))));
}

CAMLprim value ocaml_bdd_enable_disable_reorder(value b){
  CAMLparam1(b);
  if (Bool_val(b) )
    bdd_enable_reorder();
  else bdd_disable_reorder();
  CAMLreturn(Val_unit);
}


/* Finite domain variable blocks */
CAMLprim value ocaml_fdd_overlapdomain(value b1, value b2){
  CAMLparam2(b1,b2);
  CAMLreturn(Val_int(fdd_overlapdomain(Int_val(b1),Int_val(b2))));
}

CAMLprim value ocaml_fdd_clearall(value unit){
  CAMLparam1(unit);
  fdd_clearall();
  CAMLreturn(Val_unit);
}

CAMLprim value ocaml_fdd_domainnum(value unit){
  CAMLparam1(unit);
  CAMLreturn(Val_int(fdd_domainnum()));
}

CAMLprim value ocaml_fdd_domainsize(value i){
  CAMLparam1(i);
  CAMLreturn(Val_int(fdd_domainsize(Int_val(i))));
}

CAMLprim value ocaml_fdd_equals(value b1,value b2){
  CAMLparam2(b1,b2);
  BDD bdd = fdd_equals(Int_val(b1),Int_val(b2));
  bdd_addref(bdd);
  CAMLreturn(caml_alloc_bdd(bdd));
}

CAMLprim value ocaml_fdd_extdomain(value doms){
  CAMLparam1(doms);
  CAMLreturn(Val_int(fdd_extdomain(Data_bigarray_val(doms),Bigarray_val(doms)-> dim[0])));
}

CAMLprim value ocaml_fdd_intaddvarblock(value i1,value i2,value i3){
  CAMLparam3(i1,i2,i3);
  CAMLreturn(Val_int(fdd_intaddvarblock(Int_val(i1),Int_val(i2),Int_val(i3))));
}

CAMLprim value ocaml_fdd_ithset(value i){
  CAMLparam1(i);
  BDD bdd = fdd_ithset(Int_val(i));
  bdd_addref(bdd);
  CAMLreturn(caml_alloc_bdd(bdd));
}

CAMLprim value ocaml_fdd_ithvar(value fdd,value val){
  CAMLparam2(fdd,val);
  BDD bdd = fdd_ithvar(Int_val(fdd),Int_val(val));
  bdd_addref(bdd);
  CAMLreturn(caml_alloc_bdd(bdd));
}


CAMLprim value ocaml_fdd_vars(value fdb){
  CAMLparam1(fdb);
  CAMLlocal1(arr);
   int blk = Int_val(fdb);
  int * vars = fdd_vars(blk);
  int nb = fdd_varnum(blk);
  arr = caml_alloc_tuple(nb) ; /* int = word ? */
  int i;
  for(i=0; i< nb; i++)
    Store_field(arr,i,vars[i]);
  CAMLreturn(arr);
}

