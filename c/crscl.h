/* -*- Mode: C -*- */

/* crscl.h -- "Contains a Reimplementation of a Subset of Common Lisp"
 *
 * This library contains a minimal reader sor a Lisp language. The
 * language is a subset of Common Lisp (sans READER-MACRO).
 */

/* Please see the file COPYING for licensing details. */

/* History:
 *
 * 2010 - Resurrected and updated.
 *
 * 2001 - ConVEX and VAL are complex system, hence they must contain a
 * buggy reimplementation of a subset of Common Lisp.
 *
 * 2000-08-19 Marco Antoniotti
 * The file (originally named 'lisp.h') comes from Berkeley and was
 * part of the SHIFT system (another "complex" system).  It has not
 * changed much and the Berkeley Copyright must remain.
 *
 * 1996- This file is part of the SHIFT compiler.
 *
 * Originally written by Edward Wang (edward@edcom.com)
 * Modified 1995-96 by Luigi Semenzato, Berkeley PATH
 * luigi@fiat.its.berkeley.edu
 * January 1996
 */

#ifndef _CRSCL_LISP_H
#define _CRSCL_LISP_H

/* Setting up namespace and linkage for C++.
 * Since C++ programs do not necessarily include <stdio.h> I must do
 * so here.
 */
#ifdef __cplusplus

extern "C" {
#include <stdio.h>
}

namespace crscl {
extern "C" {
#endif

/* Necessary #includes. */

#include <stdbool.h>

/* The main Lisp object data structure. */

typedef struct lispval
{
  short type;
  char flags;
  struct lispval *link;
  union
  {
    /* symbol */
    struct
    {
      char *vpname;		/* print name */
      struct lispval *vshlink;	/* hash link */
      struct lispval *vplist;	/* property list (an alist) */
      short vsindex;		/* used by hasher */
    } s;

    /* cons cell */
    struct
    {
      struct lispval *car, *cdr;
    } c;

    /* string */
    struct
    {
      char *vstr;
    } t;

    /* fixnum */
    struct
    {
      int vnum;
    } f;

    /* double */
    struct
    {
      double vdnum;
    } d;

    /* IR node */
    struct
    {
      struct lispval *vop;	/* tag name (a symbol) */
      struct lispval *vattrs;	/* attributes (an alist) */
      struct lispval *vargs;	/* arguments */
      int mark;
    } n;

    /* Other */
    struct
    {
      void *x;
    } o;
  } u;

  /* Graph Traversal support for marking
   * 
   * 2000-08-22 Marco Antoniotti
   */
  int mark;
  short color;
} lv;


/* readstream -- A union type used to abstract the input stream. The
 * readstream_kind enumeration type is here as support.
 */
typedef enum _readstream_kind
{
  file_stream = 0,
  string_stream
} readstream_kind;


typedef struct readstream
{
  readstream_kind tag;
  union 
  {
    struct {
      char *name;		/* File name. */
      FILE *fp;			/* File pointer stream. */
    } file;

    struct {			/* String stream. */
      char *buf;
      int consumed;
      int len;
    } sp;
  } u;
} rs;


/* Some useful aliases */

typedef lv* lv_t;
typedef lv* symbol_t;
typedef lv* cons_t;
typedef lv* list_t;

/* type */
#define L_FREE		0
#define L_CONS		1
#define L_STRING	2
#define L_SYMBOL	3
#define L_NODE		4
#define L_FIXNUM	5
#define L_DOUBLE        6
#define L_NULL          7

#define L_OTHER		255

  /*
typedef enum l_type {
  L_FREE = 0,
  L_CONS,
  L_STRING,
  L_SYMBOL,
  L_NODE,
  L_FIXNUM,
  L_DOUBLE,
  L_NULL,

  L_OTHER = 255
};
  */

/* flags */
#define L_STATIC	0x0001

/*
 * Encapsulation
 */

#define eq(x, y)	((x) == (y))

#define consp(x)	((x) != 0 && (x)->type == L_CONS)
  /* #define null(x)         ((x) == nil) */
#define cons_or_null_p(x) (null(x) || consp(x))

extern inline _Bool null(lv *);


#define hd(x)		((x)->u.c.car)
#define tl(x)		((x)->u.c.cdr)

#define stringp(x)	((x) != 0 && (x)->type == L_STRING)
#define str(x)		((x)->u.t.vstr)

#define fixnump(x)	((x) != 0 && (x)->type == L_FIXNUM)
#define doublep(x)	((x) != 0 && (x)->type == L_DOUBLE)
#define num(x)		(lv_type_of(x) == L_FIXNUM \
                            ? ((x)->u.f.vnum)      \
                            : ((x)->u.d.vdnum))

#define intnum(x)	((x)->u.f.vnum)
#define doublenum(x)	((x)->u.d.vdnum)

#define symbolp(x)	((x) != 0 && (x)->type == L_SYMBOL)
#define pname(x)	((x)->u.s.vpname)
#define plist(x)	((x)->u.s.vplist)
#define sindex(x)	((x)->u.s.vsindex)
#define shlink(x)	((x)->u.s.vshlink)

#define nodep(x)	((x) != 0 && (x)->type == L_NODE)
#define op(x)		((x)->u.n.vop)
#define attrs(x)	((x)->u.n.vattrs)
#define args(x)		((x)->u.n.vargs)
#define arg1(x)		first(args(x))
#define arg2(x)		second(args(x))
#define arg3(x)		third(args(x))
#define mark_attr(x)	((x)->u.n.mark)

#define otherp(x)	((x) != 0 && (x)->type == L_OTHER)
#define oth(y)		((y)->u.o.x)

/*
 * Generally useful stuff
 */

#define first(x)	hd(x)
#define second(x)	hd(tl(x))
#define third(x)	hd(tl(tl(x)))
#define fourth(x)	hd(tl(tl(tl(x))))
#define fifth(x)	hd(tl(tl(tl(tl(x)))))

#define rest(x)		tl(x)

#define list1(x)	cons(x, nil)
#define list2(x, y)	cons(x, list1(y))
#define list3(x, y, z)	cons(x, list2(y, z))
#define list4(x, y, z, w)	cons(x, list3(y, z, w))

#define push(x, l)	((l) = cons(x, l))
#define apush(x, y, l)	push(cons(x, y), l)
#define acons(x, y, l)	cons(cons(x, y), l)
#define reassoc(x, y, l)	reassoc_(x, y, &l)

#define add_attr(attr, node, value) apush(attr, value, attrs(node))

#define alist1(k1, v1)			acons(k1, v1, nil)
#define alist2(k1, v1, k2, v2)		acons(k1, v1, alist1(k2, v2))
#define alist3(k1, v1, k2, v2, k3, v3)	acons(k1, v1, alist2(k2, v2, k3, v3))

#define dolist(__crscl_x_temp__, __crscl_l_temp__)	{	\
  lv *__crscl_x_temp__, *__crscl_l_local_temp__;		\
  for (__crscl_l_local_temp__ = (__crscl_l_temp__);		\
       __crscl_l_local_temp__ != (lv*) 0;			\
       __crscl_l_local_temp__ = tl(__crscl_l_local_temp__)) {	\
     __crscl_x_temp__ = hd(__crscl_l_local_temp__); {

#define tsilod }}}
#define end_dolist }}}

#define strsave(x)	(str(string(x)))
#define strsavel(x, l)	(str(stringl(x, l)))

#define nil ((lv*) 0)

/*
 * Symbol hashing
 */

/* Old. Se comment below
#define L_HBITS		11
#define L_HSHIFT	3
#define L_HSIZE		(1 << L_HBITS)
#define L_HMASK		(L_HSIZE - 1)
*/

#ifndef _CRSCL_LISP_I

/* extern lv *shtab[];
 * This variable needs to be allocated by the programs that want to
 * use the lisp library.
 *
 * A typical declaration would be

     lv* shtab[L_HSIZE];

 * Marco Antoniotti 19971023
 *
 *
 * Better still, the two macro below can be safely used.
 *
 * 2000-08-19 Marco Antoniotti
 *
 * The presence of shtab here is not warranted.  It was needed in the
 * old SHIFT embedded implementation because of some funky setup.  But
 * the shtab is accessed solely via 'intern' and 'clear_crscl_tables',
 * hence it (and its support macros) can be removed from here.
 *
 * 2001-05-18 Marco Antoniotti
 */

/* Old
#define INIT_CRSCL_TABLES() lv* shtab[L_HSIZE]
#define USE_CRSCL_TABLES()  extern lv* shtab[L_HSIZE]
*/
/* New definitions for back compatibility. */
#define INIT_CRSCL_TABLES()
#define USE_CRSCL_TABLES()

/* Old
#define CLEAR_CRSCL_TABLES() {					\
   int __clr;							\
   for (__clr = 0; __clr < L_HSIZE ;__clr++) shtab[__clr] = 0;	\
}
*/

/* New definition for back compatibility. */
#define CLEAR_CRSCL_TABLES() clear_crscl_tables()

extern lv *intern(char *);
extern int shash(char *);
extern lv* crscl_table_ref(int t);
extern int crscl_table_size();
extern void clear_crscl_tables();

#endif /* _CRSCL_LISP_I */


/*
 * Misc functions
 */

#ifndef _CRSCL_LISP_I

extern rs *make_stream(char *str, readstream_kind typ);
extern inline rs *make_stream_stream(FILE *);
extern inline rs *make_string_stream(char *);
extern inline int stream_stream_p(rs *);
extern inline int string_stream_p(rs *);
extern inline void close_stream(rs*);

extern rs* standard_input;
extern rs* standard_output;
extern rs* standard_error;


extern int crscl_read_base;
extern lv *read_sexpr(char *);
extern lv *read_sexpr1(rs *);
extern lv *read_sexpr_list(rs *);
extern lv *list_to_attrs(lv *);
extern lv *read_sexpr_string(rs *);
extern lv *read_sexpr_atom(rs *);
extern lv *read_sexpr_symbol(rs *);
extern lv *read_c_symbol(rs *);
extern int write_sexpr(lv *, char *);
extern void print(lv *);
extern void write_sexpr1(lv *, FILE *);

extern int lv_type_of(lv*);
extern void fprint_lv_type(lv*, FILE*);
extern void print_lv_type(lv*);

extern inline lv *cons(lv *, lv *);
extern lv *string(const char *);
extern lv *stringl(char *, int);
extern inline lv *fixnum(int);
extern inline lv *double_float(double);
extern lv *symbol(char *);
extern lv *node(lv *, lv *, lv *);
extern lv *attr(lv *, lv *);
#define attr2(a1, a2, n) attr(a1, attr(a2, n))
#define attr3(a1, a2, a3, n) attr2(a1, a2, attr(a3, n))
extern void set_attr(lv *attr, lv *node, lv *value);
extern void push_attr(lv *attr, lv *node, lv *value);
extern lv *other(void *x);
extern lv *assoc(lv *, lv *);
extern int length(lv *);
extern lv *nth(int, lv *);
extern lv *nthcdr(int, lv *);

extern lv* getf(lv* plist, lv* indicator);
extern lv* getf1(lv* plist, lv* indicator, lv* default_value);
extern lv* plist_get(lv* sym, lv* indicator);
extern lv* setf_get(lv* sym, lv* indicator, lv* value);


/* The 'list' function cannot reliably work in ANSI C environments,
 * due to the specs of <stdarg.h>.
 * Hence it is not made available.
 *
 * 2000-08-21 Marco Antoniotti
 */
/* extern lv *list(lv *, ...); */

extern lv *copy_tree(lv *);
extern lv *copy_list(lv *);
extern lv *nreverse(lv *);
extern lv *nreverse2(lv *, lv *);
extern lv *nconc(lv *, lv *);
extern lv *append(lv *, lv *);
extern lv *del_el(lv *, lv *);
extern lv *mapcar(lv *(*)(lv *), lv *);
extern lv *mapcarx(lv *(*)(), lv *, lv *);
extern lv *mapcan(lv *(*)(lv *), lv *);
extern void mapc(void (*)(lv *), lv *);
extern void mapcx(void (*)(lv *, lv*), lv *, lv *);
extern lv *member_op(lv *, lv *);
extern lv *memq(lv *, lv *);
extern lv *lalloc(void);
extern void gc_set_root(lv *);
extern void gc_clear_root(void);
extern void gc();
extern void new_node_marker();
extern void mark_node(lv* node);
extern int node_marked_p(lv* node);

#endif /* _CRSCL_LISP_I */

#ifdef __cplusplus
} /* extern "C" */
} /* namespace crscl */
#endif

#endif /* _CRSCL_LISP_H */

/* end of file -- crscl.h -- */
