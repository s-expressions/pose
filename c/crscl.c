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

#ifndef _CRSCL_LISP_I
#define _CRSCL_LISP_I


#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <assert.h>
#include <limits.h>
#include <errno.h>

#ifndef WIN32
#include <unistd.h>
#endif

/* Added <ctype.h> for character conversion
 * routines.
 */
#include <ctype.h>

#include "crscl.h"		/* Was #include "lisp.h" */


/* shtab -- Symbol table support. */

#define L_HBITS		11
#define L_HSHIFT	3
#define L_HSIZE		(1 << L_HBITS)
#define L_HMASK		(L_HSIZE - 1)

lv* shtab[L_HSIZE];


/* List all the interface functions */

lv *intern(char *);
int shash(char *);
int crscl_table_size();
void clear_crscl_tables();

lv *read_sexpr(char *);
lv *read_sexpr1(rs *);

rs *make_stream(char *str, readstream_kind typ);
inline rs *make_stream_stream(FILE *str);
inline rs *make_string_stream(char *str);
inline int stream_stream_p(rs *);
inline int string_stream_p(rs *);
inline void close_stream(rs *stream);

lv *read_sexpr_list(rs *);
lv *list_to_attrs(lv *);
lv *read_sexpr_string(rs *);
lv *read_sexpr_symbol(rs *);
lv *read_sexpr_atom(rs *);
lv *read_c_symbol(rs *);

int write_sexpr(lv *, char *);
void print(lv *);
void write_sexpr1(lv *, FILE *);

inline _Bool null(lv *);
inline lv *cons(lv *, lv *);

lv *string(const char *);
lv *stringl(char *, int);
inline lv *fixnum(int);
inline lv *double_float(double);
lv *symbol(char *);
lv *node(lv *, lv *, lv *);
lv *attr(lv *, lv *);
#define attr2(a1, a2, n) attr(a1, attr(a2, n))
#define attr3(a1, a2, a3, n) attr2(a1, a2, attr(a3, n))
void set_attr(lv *attr, lv *node, lv *value);
void push_attr(lv *attr, lv *node, lv *value);
lv *other(void *x);
lv *assoc(lv *, lv *);
int length(lv *);
lv *nth(int, lv *);
lv *nthcdr(int, lv *);

lv* getf(lv* plist, lv* indicator);
lv* getf1(lv* plist, lv* indicator, lv* default_value);
lv* plist_get(lv* sym, lv* indicator);
lv* setf_get(lv* sym, lv* indicator, lv* value);

lv *list(lv *, ...);
lv *copy_tree(lv *);
lv *copy_list(lv *);
lv *nreverse(lv *);
lv *nreverse2(lv *, lv *);
lv *nconc(lv *, lv *);
lv *append(lv *, lv *);
lv *del_el(lv *, lv *);
lv *mapcar(lv *(*)(lv *), lv *);
lv *mapcarx(lv *(*)(), lv *, lv *);
lv *mapcan(lv *(*)(lv *), lv *);
void mapc(void (*)(lv *), lv *);
void mapcx(void (*)(lv *, lv*), lv *, lv *);
lv *member_op(lv *, lv *);
lv *memq(lv *, lv *);
lv *lalloc(void);
void gc_set_root(lv *);
void gc_clear_root(void);
void gc();
void new_node_marker();
void mark_node(lv* node);
int node_marked_p(lv* node);


/* Internal functions. */

static void*
crscl_malloc(size_t size, int errorp)
{
  void* chunk = malloc(size);

  if (chunk != NULL)
    {
      return chunk;
    }
  else if (errorp)
    {
      fputs("CRSCL: error: unable to allocate memory.\n", stderr);
      fflush(stderr);
      exit(-1);
    }
  else
    {
      fputs("CRSCL: warning: unable to allocate memory.\n", stderr);
      fflush(stderr);
      return (void *) 0;
    }
}

static void*
crscl_realloc(void* ptr, size_t size, int errorp)
{
  void* chunk = realloc(ptr, size);

  if (chunk != NULL)
    return chunk;
  else if (errorp)
    {
      fputs("CRSCL: error: unable to reallocate memory.\n", stderr);
      fflush(stderr);
      exit(-1);
    }
  else
    {
      fputs("CRSCL: warning: unable to reallocate memory.\n", stderr);
      fflush(stderr);
      return (void *) 0;
    }
}


/*
 * A data-structure package that provides Lisp-like features
 */

inline int
lv_type_of(lv* v)
{
  if (v != 0)
    return v->type;
  else
    return L_CONS;
}


void
fprint_lv_type(lv* v, FILE* fp)
{
  switch (lv_type_of(v))
    {
    case L_FREE:
      fputs("L_FREE", fp);
      break;
    case L_CONS:
      fputs("L_CONS", fp);
      break;
    case L_STRING:
      fputs("L_STRING", fp);
      break;
    case L_SYMBOL:
      fputs("L_SYMBOL", fp);
      break;
    case L_NODE:
      fputs("L_NODE", fp);
      break;
    case L_FIXNUM:
      fputs("L_FIXNUM", fp);
      break;
    case L_DOUBLE:
      fputs("L_DOUBLE", fp);
      break;
    default:
      fputs("L_OTHER", fp);
      break;
    }
  putc('\n', fp);
}

inline void
print_lv_type(lv* v)
{
  fprint_lv_type(v, stdout);
}


/*
 * Reader
 */

/* The reader *READ-BASE* for numbers. */

int crscl_read_base = 10;



/* Some buffered read operations.
 * Ok, they are very incomplete, since the buffer is not associated
 * with the file pointer.  Hence reading from more than one file while
 * doing 'lv_ungetc' will most likely wreak havok in your program.
 *
 * 2000-08-21 Marco Antoniotti
 */

/* A set of 64 stacks indexed by fileno(). */

#define CRSCL_MAX_OPEN_STREAMS 64

struct lv_ungetc_stack
{
  int top;
  unsigned char chars_stack[256];
};

static struct lv_ungetc_stack stacked_read_chars[CRSCL_MAX_OPEN_STREAMS];
static short lv_ungetc_stacks_initialized = 0;

static void
initialize_ungetc_stacks()
{
  int i;

  for (i = 0; i < CRSCL_MAX_OPEN_STREAMS; i++)
    stacked_read_chars[i].top = -1;
  lv_ungetc_stacks_initialized = 1;
}

static int
lv_fgetc(rs* stream)
{
  if (stream->tag == file_stream)
    {
      FILE *fp = stream->u.file.fp;
      int fp_number = fileno(fp);

      if (! lv_ungetc_stacks_initialized)
	initialize_ungetc_stacks();

      if (fp_number >= CRSCL_MAX_OPEN_STREAMS)
	{
	  fprintf(stderr,
		  "CRSCL: error: too many open streams (%d) for CRSCL.",
		  fp_number);
	  exit(1);
	}

      if (stacked_read_chars[fp_number].top == -1)
	return getc(fp);
      else
	{
	  int st_top = stacked_read_chars[fp_number].top;

	  (stacked_read_chars[fp_number].top)--;
	  return (int) stacked_read_chars[fp_number].chars_stack[st_top];
	}
    }
  else
    {
      if (stream->u.sp.consumed>=stream->u.sp.len) return EOF;
      return stream->u.sp.buf[stream->u.sp.consumed++];
    }
}

static int
lv_ungetc(int c, rs* stream)
{
  if (stream->tag == file_stream)
    {
      FILE *fp = stream->u.file.fp;

      int fp_number = fileno(fp);
      int st_top;

      /* I should really double check the fileno anyway.  I am just
       * assuming that you do not call 'lv_ungetc' before 'lv_fgetc'.
       */

      st_top = ++(stacked_read_chars[fp_number].top);
      stacked_read_chars[fp_number].chars_stack[st_top]
	= (unsigned char) c;
      return c;
    }
  else
    {
      if (stream->u.sp.consumed==0) return EOF;
      stream->u.sp.consumed--;
      return c;
    }
}


lv *
read_sexpr(char* file)
{
  FILE *fp;
  lv *s;
  rs *stream;
  
  if ((fp = fopen(file, "r")) == 0)
    {
      fprintf(stderr,
	      "CRSCL: error: cannot open file \"%s\" for reading.\n",
	      file);
      fflush(stderr);
      return 0;
    }

  stream = (rs *) crscl_malloc(sizeof(rs), 1);
  stream->tag  = file_stream;
  stream->u.file.fp = fp;
  stream->u.file.name = strdup(file);

  if (stream->u.file.name == NULL)
    {
      fputs("CRSCL: error: cannot allocate memory.\n", stderr);
      fflush(stderr);
      fclose(fp); 
      free(stream);
      return 0;
    }

  s = read_sexpr1(stream);

  fclose(fp);
  free(stream->u.file.name);
  free(stream);
  return s;
}


rs *
make_stream(char *str, readstream_kind typ)
{
  FILE *fp;
  rs *stream;
  
  if (typ == file_stream)
    {
      if ((fp = fopen(str, "r")) == 0)
	{
	  fprintf(stderr, "CRSCL: cannot open file \"%s\" for reading.\n",
		  str);
	  fflush(stderr);
	  return 0;
	}
      stream = make_stream_stream(fp);

      free(stream->u.file.name); /* 'make_stream_stream' fills this
				  * field with a dummy value.
				  */
      stream->u.file.name = strdup(str);
      if (stream->u.file.name == NULL)
	{
	  fputs("CRSCL: error: cannot allocate memory.\n", stderr);
	  fflush(stderr);
	  fclose(fp); 
	  free(stream);
	  return NULL;
	}
      return stream;
    }
  else
    return make_string_stream(str);
}


inline rs *
make_string_stream(char *str)
{
  rs *stream = (rs*) crscl_malloc(sizeof(rs), 1);

  stream->tag = string_stream;
  stream->u.sp.buf = str;
  stream->u.sp.len = strlen(str);
  stream->u.sp.consumed = 0;

  return stream;
}


inline rs *
make_stream_stream(FILE *fp)
{
  rs *stream = (rs*) crscl_malloc(sizeof(rs), 1);

  stream->tag  = file_stream;
  stream->u.file.fp = fp;
  stream->u.file.name = strdup("<file-stream>");

  return stream;
}


inline int
stream_stream_p(rs* stream)
{
  return stream->tag == file_stream;
}

inline int
string_stream_p(rs* stream)
{
  return stream->tag == string_stream;
}

inline void
close_stream(rs *stream)
{
  if (stream_stream_p(stream))
    fclose(stream->u.file.fp);
  free(stream);
}


lv *
read_sexpr1(rs* fp)
{
  int c;
  lv *s;
    
 again:
  while ((c = lv_fgetc(fp)) != EOF
	 && (c == ' ' || c == '\t' || c == '\n' || c == '\r'));	/* Empty. */
  switch (c)
    {
    case ';':
      for (; (c = lv_fgetc(fp)) != EOF && c != '\n';)
	;
      goto again;
    case EOF:
      s = 0;
      break;
    case '(':
      s = read_sexpr_list(fp);
      break;
    case '"':
      s = read_sexpr_string(fp);
      break;
    default:
      lv_ungetc(c, fp);
      /* s = read_sexpr_symbol(fp); */
      s = read_sexpr_atom(fp);
    }
  return s;
}

static int
read_dot_for_pair(rs* fp)
{
  int c, next_c;

  while ((c = lv_fgetc(fp)) != EOF && (c == ' ' || c == '\t' || c == '\n'));
  if (c == '.')
    {
      next_c = lv_fgetc(fp);
      if ((next_c != EOF)
	  && (next_c == ' '
	      || next_c == '\t'
	      || next_c == '\n'))
	{
	  /* Good. We have a dot pair. */
	  return 1;
	}
      else if (next_c == EOF)
	{
	  lv_ungetc(next_c, fp);
	  return 0;
	}
      else
	{
	  /* No dotted pair. */
	  lv_ungetc(next_c, fp);
	  lv_ungetc(c, fp);
	  return 0;
	}
    }
  else
    {
      lv_ungetc(c, fp);
      return 0;
    }
}

lv *
read_sexpr_list(rs* fp)
{
  int c;
  lv *s;
    
  while ((c = lv_fgetc(fp)) != EOF && (c == ' ' || c == '\t' || c == '\n'))
    ;
  switch (c)
    {
    case EOF:
    case ')':
      s = 0;
      break;
    default:
      lv_ungetc(c, fp);
      s = read_sexpr1(fp);
      if (read_dot_for_pair(fp))
	{
	  s = cons(s, read_sexpr1(fp));
	  /* Now consume the right parenthesis. */
	  while ((c = lv_fgetc(fp)) != EOF
		 && (c == ' ' || c == '\t' || c == '\n'))
	    ;
	  if (c != ')')
	    {
	      fprintf(stderr, "CRSCL: read error: malformed dotted pair.");
	      exit(1);
	    }
	}
      else
	s = cons(s, read_sexpr_list(fp));
      break;
    }
  return s;
}


static char *read_buf;
static int read_bufsiz;

lv *
read_sexpr_string(rs* fp)
{
  int c;
  int i = 0;
    
  if (read_buf == 0)
    {
      read_buf = crscl_malloc(read_bufsiz = 1024, 1);
      memset((void *) read_buf, 0, read_bufsiz);
      if (read_buf == 0)
	{
	  fputs("CRSCL:read_sexpr_string: unable to allocate heap memory.\n",
		stderr);
	  fflush(stderr);
	  exit(1);
	}
    }
  for (; (c = lv_fgetc(fp)) != EOF && c != '"';)
    {
      if (c == '\\' && (c = lv_fgetc(fp)) == EOF)
	break;
      read_buf[i++] = c;
      if (i >= read_bufsiz)
	{
	  read_buf = crscl_realloc((void *) read_buf, read_bufsiz *= 2, 1);
	}
    }
  read_buf[i++] = 0;
  return string(read_buf);
}


lv *
read_sexpr_symbol(rs* fp)
{
  int c;
  int i = 0;
    
  if (read_buf == 0)
    {
      read_buf = (char*) crscl_malloc(read_bufsiz = 1024, 1);
      memset((void *) read_buf, 0, read_bufsiz);
    }

  for (;;)
    {
      c = lv_fgetc(fp);
      switch (c)
	{
	case ';':
	case '(':
	case ')':
	case '"':
	case '&':
	  lv_ungetc(c, fp);
	case EOF:
	case '\n':
	case '\r':
	case '\t':
	case '\f':
	case ' ':
	  goto out;
	case '\\':
	  if ((c = lv_fgetc(fp)) == EOF)
	    goto out;
	}
      read_buf[i++] = c;
      if (i >= read_bufsiz)
	{
	  read_buf = crscl_realloc((void *) read_buf, read_bufsiz *= 2, 1);
	}
    }
 out:
  read_buf[i++] = 0;
  return intern(read_buf);
}

static int
read_buffer_as_fixnum(char* buffer, lv** fixnum_result)
{
  char *end_ptr;
  long r = strtol(buffer, &end_ptr, crscl_read_base);

  if (*end_ptr != '\0')
    {
      *fixnum_result = nil;
      return 0;
    }
  if (r > INT_MAX)
    *fixnum_result = fixnum(INT_MAX);
  else if (r < INT_MIN)
    *fixnum_result = fixnum(INT_MIN);
  else if (errno == ERANGE && r == LONG_MAX)
    *fixnum_result = fixnum(INT_MAX);
  else if (errno == ERANGE && r == LONG_MIN)
    *fixnum_result = fixnum(INT_MIN);
  else
    *fixnum_result = fixnum((int) r);
  return 1;
}


static int
read_buffer_as_double(char* buffer, lv** double_result)
{
  char *end_ptr;
  double r = strtod(buffer, &end_ptr);

  if (*end_ptr != '\0')
    {
      *double_result = nil;
      return 0;
    }
  else
    *double_result = double_float(r);
  return 1;
}



lv*
read_sexpr_atom(rs* fp)
{
  int c;
  int i = 0;
    
  if (read_buf == 0)
    {
      read_buf = crscl_malloc(read_bufsiz = 1024, 1);
      memset((void *) read_buf, 0, read_bufsiz);
    }

  for (;;)
    {
      c = lv_fgetc(fp);
      switch (c)
	{
	case ';':
	case '(':
	case ')':
	case '"':
	case '&':
	  lv_ungetc(c, fp);
	case EOF:
	case '\n':
	case '\r':
	case '\t':
	case '\f':
	case ' ':
	  goto out;
	case '\\':
	  if ((c = lv_fgetc(fp)) == EOF)
	    goto out;
	}
      read_buf[i++] = c;
      if (i >= read_bufsiz)
	{
	  read_buf = crscl_realloc((void *) read_buf, read_bufsiz *= 2, 1);
	}
    }
 out:
  {
    lv* fixnum_result;
    lv* double_result;

    read_buf[i++] = 0;
    /* if ((!strcmp(read_buf, "nil")) || (!strcmp(read_buf, "NIL")))
      return nil;
    else
    */
    if (read_buffer_as_fixnum(read_buf, &fixnum_result))
      return fixnum_result;
    else if (read_buffer_as_double(read_buf, &double_result))
      return double_result;
    else /* Assume a Symbol */
      return intern(read_buf);
  }
}


lv *
read_quoted_symbol(rs *fp)
{
  int c;
  int i = 0;
    
  if (read_buf == 0)
    {
      read_buf = crscl_malloc(read_bufsiz = 1024, 1);
      memset((void *) read_buf, 0, read_bufsiz);
    }
  for (;;)
    {
      c = lv_fgetc(fp);
      switch (c)
	{
	case '|':
	  goto out;
	case '\\':
	  if ((c = lv_fgetc(fp)) == EOF)
	    goto out;
	}
      read_buf[i++] = c;
      if (i >= read_bufsiz)
	{
	  read_buf = crscl_realloc(read_buf, read_bufsiz *= 2, 1);
	}
    }
 out:
  read_buf[i++] = 0;
  return intern(read_buf);
}


lv *
read_c_symbol2(rs *fp)
{
  int c;
  int i = 0;
    
  if (read_buf == 0)
    {
      read_buf = crscl_malloc(read_bufsiz = 1024, 1);
      memset((void *) read_buf, 0, read_bufsiz);
    }
  for (;;)
    {
      c = lv_fgetc(fp);
      switch (c)
	{
	case ',':
	case ':':
	case ';':
	case '(':
	case ')':
	case '"':
	case '&':
	case '\n':
	case '\r':
	case '\t':
	case '\f':
	case ' ':
	  lv_ungetc(c, fp);
	case EOF:
	  goto out;
	case '\\':
	  if ((c = lv_fgetc(fp)) == EOF)
	    goto out;
	}
      read_buf[i++] = c;
      if (i >= read_bufsiz)
	{
	  read_buf = crscl_realloc(read_buf, read_bufsiz *= 2, 1);
	}
    }
 out:
  read_buf[i++] = 0;
  return intern(read_buf);
}


lv *
read_c_symbol(rs* fp)
{
  int c;

  if ((c = lv_fgetc(fp)) == '"')
    {
      lv *x = read_sexpr_string(fp);
      return intern(str(x));
    }
  else
    {
      lv_ungetc(c, fp);
      return read_c_symbol2(fp);
    }
}


/*
 * Print functions
 */

int
write_sexpr(lv* s, char* file)
{
  FILE *fp;
    
  if ((fp = fopen(file, "w")) == 0)
    {
      fprintf(stderr,
	      "CRSCL: error: cannot open file \"%s\" for writing.",
	      file);
      fflush(stderr);
      return -1;
    }
  write_sexpr1(s, fp);
  fclose(fp);
  return 0;
}


void
print(lv* s)
{
  write_sexpr1(s, stdout);
}


/* For debugging.
 */
void
p(lv* s)
{
  print(s);
  fflush(stdout);
}


/* For debugging.
 */
void
f()
{
  fflush(stdout);
}

/* For debugging.
 */
void
pattr(char* as, lv* n)
{
  p(attr(intern(as), n));
}

static int dont_loop = 1;
static int print_addresses = 0;
static int print_level = 10;
short print_nil_as_list = 0;

static int sexpr_mark = 1;

void
write_sexpr1_(lv* s, FILE* fp, int level)
{
  char *p;
  lv* sub_cons = nil;
    
  if (s == 0)
    {
      if (print_nil_as_list)
	fputs("()", fp);
      else
	fputs("nil", fp);
      return;
    }
  if (print_addresses) fprintf(fp, "0x%p: ", (void *) s);
  if (level >= print_level)
    {
      switch (s->type)
	{
	case L_CONS:
	  fprintf(fp, "(...)");
	  return;
	case L_NODE:
	  fprintf(fp, "#[...]");
	  return;
	}
    }
  if (dont_loop && s->mark)
    {
      switch (s->type)
	{
	case L_CONS:
	  fprintf(fp, "#=%d()", s->mark);
	  return;
	case L_NODE:
	  fprintf(fp, "#=%d#[...]", s->mark);
	  return;
	}
    }
  if (dont_loop) s->mark = sexpr_mark++;

  switch (s->type)
    {
    case L_CONS:
      fprintf(fp, "(");
      write_sexpr1_(hd(s), fp, level + 1);

      for (sub_cons = tl(s); sub_cons != nil; sub_cons = tl(sub_cons))
	{
	  if (dont_loop && sub_cons->mark)
	    {
	      switch (sub_cons->type)
		{
		case L_CONS:
		  fprintf(fp, "#=%d()", s->mark);
		  return;
		case L_NODE:
		  fprintf(fp, "#=%d#[...]", s->mark);
		  return;
		}
	    }
	  if (dont_loop) sub_cons->mark = sexpr_mark++;

	  if (sub_cons->type == L_CONS)
	    {
	      fputc(' ', fp);
	      write_sexpr1_(hd(sub_cons), fp, level + 1);
	    }
	  else
	    {
	      fputs(" . ", fp);
	      write_sexpr1_(sub_cons, fp, level + 1);
	      break;
	    }
	}

      /*
      while ((s = tl(s)))
	{
	  if (s->type == L_CONS)
	    {
	      fprintf(fp, " ");
	      write_sexpr1_(hd(s), fp, level + 1);
	    }
	  else
	    {
	      fprintf(fp, " . ");
	      write_sexpr1_(s, fp, level + 1);
	      break;
	    }
	}
      */
      fputc(')', fp);
      break;

    case L_STRING:
      putc('"', fp);
      for (p = str(s); *p; p++)
	{
	  if (*p == '"')
	    putc('\\', fp);
	  putc(*p, fp);
	}
      putc('"', fp);
      break;

    case L_FIXNUM:
      fprintf(fp, "%d", intnum(s));
      break;

    case L_DOUBLE:
      fprintf(fp, "%f", doublenum(s));
      break;

    case L_SYMBOL:
      for (p = pname(s); *p; p++)
	{
	  switch (*p)
	    {
	    case ';':
	    case '(':
	    case ')':
	    case '"':
	    case '&':
	    case '\n':
	    case '\r':
	    case '\t':
	    case '\f':
	    case ' ':
	    case '\\':
	      putc('\\', fp);
	    }
	  putc(*p, fp);
	}
      break;

    case L_NODE:
      fprintf(fp, "#[");
      write_sexpr1_(op(s), fp, level + 1);
      if (args(s))
	{
	  fprintf(fp, " ARGUMENTS: ");
	  write_sexpr1_(args(s), fp, level + 1);
	}
      if (attrs(s))
	{
	  fprintf(fp, " ATTRIBUTES: ");
	  write_sexpr1_(attrs(s), fp, level + 1);
	}
      fputc(']', fp);
      break;

    case L_OTHER:
      fprintf(fp, "#<other 0x%p>", (void *) oth(s));
      break;
    }
}

static void markv(lv *, int);

void write_sexpr1(lv *s, FILE* fp)
{
  write_sexpr1_(s, fp, 0);
  if (dont_loop) markv(s, 0);	/* unmark, really. */
}


/*
 * Basic constructors and predicates.
 */

inline _Bool
null(lv *x) {
  return
    x == nil
    || x->type == L_NULL
    || x == intern("nil")
    || x == intern("NIL");
}


inline lv *
cons(lv *a, lv *b)
{
  lv *s;
    
  s = lalloc();
  s->type = L_CONS;
  hd(s) = a;

  /* Special case for 'nil'. */
  tl(s) = null(b) ? nil : b;
  return s;
}


lv *
string(const char* p)
{
  lv *s;
    
  s = lalloc();
  s->type = L_STRING;
  str(s) = crscl_malloc(strlen(p) + 1, 1);
  memset((void *) str(s), 0, strlen(p) + 1);
  strcpy(str(s), p);
  return s;
}


lv *
stringl(char *p, int n)
{
  lv *s;
    
  s = lalloc();
  s->type = L_STRING;
  str(s) = crscl_malloc(n + 1, 1);
  
  memcpy(str(s), p, n);
  str(s)[n] = 0;
  return s;
}


inline lv *
fixnum(int v)
{
  lv *s;
    
  s = lalloc();
  s->type = L_FIXNUM;
  intnum(s) = v;
  return s;
}


inline lv *
double_float(double d)
{
  lv *s;
    
  s = lalloc();
  s->type = L_DOUBLE;
  doublenum(s) = d;
  return s;
}


lv *
symbol(char* pname)
{
  lv *s;
    
  s = lalloc();
  s->type = L_SYMBOL;
  pname(s) = (char*) crscl_malloc((strlen(pname) + 1) * sizeof(char), 1);

  /* pname(s) = malloc(strlen(pname) + 1); */
  /* if (pname(s) == 0) */
  /*   { */
  /*     fputs("lisp.c: unable to allocate heap memory.\n", stderr); */
  /*     fflush(stderr); */
  /*     exit(1); */
  /*   } */

  strcpy(pname(s), pname);
  sindex(s) = 0;
  plist(s) = 0;
  return s;
}


lv *
node(lv* op, lv* args, lv* attrs)
{
  lv *s;
  
  s = lalloc();
  s->type = L_NODE;
  op(s) = op;
  attrs(s) = attrs;
  args(s) = args;
  mark_attr(s) = 0;
  return s;
}


lv *
other(void* x)
{
  lv *s = lalloc();
  s->type = L_OTHER;
  oth(s) = x;
  mark_attr(s) = 0;
  return s;
}


void
node_set_op(lv* s, lv* op)
{
  op(s) = op;
}


/* Plist functions. */

lv *
getf(lv* plist, lv* indicator)
{
  lv* indicator_ptr = memq(indicator, plist);

  if (indicator_ptr != nil)
    return second(indicator_ptr);
  else
    return nil;
}


lv *
getf1(lv* plist, lv* indicator, lv* default_value)
{
  lv* indicator_ptr = memq(indicator, plist);

  if (indicator_ptr != nil)
    return second(indicator_ptr);
  else
    return default_value;
}

lv*
plist_get(lv* sym, lv* indicator)
{
  if (!symbolp(sym))
    {
      fputs("CRSCL: plist_get called with a non symbol.\n", stderr);
      fflush(stderr);
      exit(1);
    }
  return getf(plist(sym), indicator);
}

lv*
setf_get(lv* sym, lv* indicator, lv* value)
{
  lv* indicator_ptr;
  if (!symbolp(sym))
    {
      fputs("CRSCL: plist_get called with a non symbol.\n", stderr);
      fflush(stderr);
      exit(1);
    }
  
  indicator_ptr = memq(indicator, plist(sym));
  if (indicator_ptr != nil)
    {
      hd(tl(indicator_ptr)) = value;
    }
  else
    {
      plist(sym) = nconc(plist(sym), list2(indicator, value));
    }
  return value;
}


/* Marking nodes.
 */

int mark_number = 0;

void
new_node_marker()
{
  mark_number %= INT_MAX;	/* Being paranoid. Marco Antoniotti 19970106 */
  mark_number++;
}

void
mark_node(lv *node)
{
  mark_attr(node) = mark_number;
}

int
node_marked_p(lv *node)
{
  return mark_attr(node) == mark_number;
}


/*
 * Intern
 */

lv *
intern(char* pname)
{
  lv *s, **sp;
  
  /* The next cast is needed for some C compilers */

  for (sp = (lv**) &shtab[shash(pname)]; (s = *sp); sp = &shlink(s))
    if (strcmp(pname(s), pname) == 0)
      return s;
  return *sp = symbol(pname);
}


int
shash(char* p)
{
  unsigned h = 0;
    
  for (; *p; p++)
    {
      h ^= *(unsigned char *)p;
      h = h >> (L_HBITS - L_HSHIFT) | h << L_HSHIFT;
      h &= L_HMASK;
    }
  return h;
}


int
crscl_table_size() { return L_HSIZE; }


lv*
crscl_table_ref(int i) { return shtab[i]; }


void
clear_crscl_tables()
{
  int __clr;

  for (__clr = 0; __clr < L_HSIZE; __clr++) shtab[__clr] = 0;
}


/*
 * Standard Lisp-like functions
 */

/* list --
 * WARNING.  This function may be buggy due to the interpretation of
 * the variable argument list.
 *
 * It needs to be fixed.
 *
 * 2000-08-20 Marco Antoniotti
 */
lv *
list(lv *a, ...)
{
  va_list ap;
  lv *s, *e, *arg;

  va_start(ap, a);    
  if (a == 0)
    return 0;
  e = s = cons(a, 0);

  while ((arg = va_arg(ap, lv *)))
    e = tl(e) = cons(arg, 0);
  va_end(ap);
  return s;
}


int
length(lv* s)
{
  int n = 0;
    
  if (s != 0)
    {
      assert(consp(s));
      for(; s; s = tl(s))
	n++;
    }
  return n;
}


lv *
nth(int n, lv* l)
{
  int i = 0;
  for(; l; l = tl(l), i++)
    if (i == n) return hd(l);
  return nil;
}

lv *
nthcdr(int n, lv* l)
{
  int i = 0;

  for(; l; l = tl(l), i++)
    if (i == n) return l;
  return nil;
}




lv *
copy_tree(lv* s)
{
  if (s == 0)
    return s;
  switch (s->type)
    {
    case L_CONS:
      return cons(copy_tree(hd(s)), copy_tree(tl(s)));
    case L_STRING:
    case L_SYMBOL:
      return s;
    case L_NODE:
      return node(op(s), copy_tree(attrs(s)), copy_tree(args(s)));
    default:
      assert(0);
    }
}


lv *
copy_list(lv* s)
{
  lv *x = 0;
  lv **p = &x;
    
  for (; s; s = tl(s))
    {
      assert(s->type == L_CONS);
      *p = cons(hd(s), 0);
      p = &tl(*p);
    }
  return x;
}


lv *
assoc(lv* k, lv* al)
{
  if (al)
    {
      if (eq(k, hd(hd(al))))
	{
	  return hd(al);
	}
      else
	return assoc(k, tl(al));
    }
  else
    return nil;
}


void
reassoc_(lv* k, lv* v, lv** lp)
{
  lv *x = assoc(k, *lp);

  if (x)
    {
      tl(x) = v;
    }
  else
    {
      apush(k, v, *lp);
    }
}


lv *
attr(lv* symbol, lv* node)
{
  lv *c = assoc(symbol, attrs(node));
  return c? tl(c) : nil;
}


void
set_attr(lv* symbol, lv* node, lv* value)
{
  reassoc(symbol, value, attrs(node));
}


/* Equivalent to set_attr(SYMBOL, NODE, cons(VALUE, attr(SYMBOL, NODE)))
 */
void
push_attr(lv* symbol, lv* node, lv* value)
{
  lv *c = assoc(symbol, attrs(node));

  if (c)
    push(value, tl(c));
  else
    apush(symbol, list1(value), attrs(node));
}


lv *
nreverse(lv* s)
{
  lv *x = 0;
    
  while (s)
    {
      lv *y;

      assert(s->type == L_CONS);
      y = tl(s);
      tl(s) = x;
      x = s;
      s = y;
    }
  return x;
}


lv *
nreverse2(lv* s, lv* e)
{
  lv *x = 0;
    
  while (s != e)
    {
      lv *y;

      assert(s->type == L_CONS);
      y = tl(s);
      tl(s) = x;
      x = s;
      s = y;
    }
  return x;
}


lv *
nconc(lv* a, lv* b)
{
  lv *x, *y;
    
  if (a == 0)
    return b;
  if (b == 0)
    return a;
  for (x = a; (y = tl(x)); x = y)
    ;
  tl(x) = b;
  return a;
}


lv *
append(lv* a, lv* b)
{
  lv *x, **p = &x;

  if (a == 0) return b;
  if (b == 0) return a;
  for (; a; a = tl(a))
    {
      *p = cons(hd(a), nil);
      p = &tl(*p);
    }
  *p = b;
  return x;
}


lv *
del_el(lv* a, lv* b)
{
  if (b == nil)
    return b;
  else if (a == first(b))
    return rest(b);
  else
    return cons(first(b), del_el(a, rest(b)));
}


lv *
mapcar(lv* (*f)(lv *), lv* s)
/* lv *(*f)(lv *);
   lv *s;
 */
{
  lv *x = 0;
  lv **p = &x;
    
  for (; s; s = tl(s))
    {
      assert(s->type == L_CONS);
      *p = cons((*f)(hd(s)), 0);
      p = &tl(*p);
    }
  return x;
}


lv *
mapcarx(lv* (*f)(lv *, lv *), lv* s, lv* a)
/* lv *(*f)(lv *, lv *);
   lv *s;
   lv *a;
 */
{
  lv *x = 0;
  lv **p = &x;
    
  for (; s; s = tl(s))
    {
      assert(s->type == L_CONS);
      *p = cons((*f)(hd(s), a), 0);
      p = &tl(*p);
    }
  return x;
}


static lv*
mapcan_tr(lv* (*f)(lv *), lv* s, lv * acc)
{
  if (s == nil)
    return acc;
  else
    return mapcan_tr(f, tl(s), nconc(acc, (*f)(hd(s))));
}


lv *
mapcan(lv* (*f)(lv *), lv* s)
{
  return mapcan_tr(f, s, nil);
}


void
mapc(void (*f)(lv *), lv* s)
/* void (*f)(lv *);
   lv *s;
 */
{
  for (; s; s = tl(s))
    (*f)(hd(s));
}


void
mapcx(void (*f)(lv *, lv*), lv* s, lv* a)
/* void (*f)(lv *, lv*);
   lv *s, *a;
 */
{
  for (; s; s = tl(s))
    (*f)(hd(s), a);
}


lv *
member_op(lv* s, lv* a)
{
  for (; s; s = tl(s))
    {
      if (nodep(hd(s)) && eq(op(hd(s)), a))
	break;
    }
  return s;
}


lv*
memq(lv* el, lv* l)
{
  if (l == nil)
    return nil;
  else if (hd(l) == el)
    return l;
  else
    return memq(el, tl(l));
}


/*
 * Allocation and collection
 */

#define CHUNK 1024
lv *freelist;
lv *alloclist;


lv *
lalloc()
{
  lv *p;
    
again:
  p = freelist;
  if (p == 0)
    {
      int i;

      p = crscl_malloc(sizeof(*p) * CHUNK, 1);
      memset(p, 0, (sizeof(*p) * CHUNK, 1));
      for (i = CHUNK; --i >= 0; p++)
	{
	  p->type = L_FREE;
	  p->link = freelist;
	  freelist = p;
	}
      goto again;
    }
  freelist = p->link;
  p->mark = 0;
  p->flags = 0;
  p->link = alloclist;
  alloclist = p;
  return p;
}


lv *rootlist;

void
gc_set_root(lv* s)
{
  rootlist = cons(s, rootlist);
}


void
gc_clear_root()
{
  rootlist = 0;
}

static void mark(lv *p);
static void sweep();
static int mark_value = 1;

void
gc()
{
  mark(rootlist);
  sweep();
}


/* mark --
 * The old (commented) version used a poor man tail recursion trick,
 * which may fool the compiler. At least I cannot understand why it
 * shouldn't work.
 * Therefore I wrote the fully recursive version which should instead
 * work a little better.
 *
 * 2000-08-21 Marco Antoniotti
 */

static void
mark(lv* p)
{
  if (p == 0 || p->mark == mark_value)
    return;

  p->mark = mark_value;
  switch (p->type)
    {
    case L_STRING:
    case L_OTHER:
    case L_FIXNUM:
    case L_DOUBLE:
      break;
    case L_SYMBOL:
      mark(plist(p));
      break;
    case L_CONS:
      mark(hd(p));
      mark(tl(p));
      break;
    case L_NODE:
      if (op(p) != 0 && op(p)->type != L_SYMBOL)
	mark(op(p));
      mark(attrs(p));
      mark(args(p));
      break;
    default:
      assert(0);
    }
}

/* Old version.
static void
mark(lv* p)
{
again:
  if (p == 0 || p->mark == mark_value) return;
  p->mark = mark_value;
  switch (p->type)
    {
    case L_STRING:
    case L_OTHER:
    case L_FIXNUM:
    case L_DOUBLE:
      break;
    case L_SYMBOL:
      p = plist(p);
      goto again;
    case L_CONS:
      mark(hd(p));
      p = tl(p);
      goto again;
    case L_NODE:
      if (op(p) != 0 && op(p)->type != L_SYMBOL)
	mark(op(p));
      mark(attrs(p));
      p = args(p);
      goto again;
    default:
      assert(0);
    }
}
*/


static void
unmark(lv* p)
{
  if (p == 0 || p->mark == 0) return;

  p->mark = 0;
  
  switch (p->type)
    {
    case L_STRING:
    case L_OTHER:
    case L_FIXNUM:
    case L_DOUBLE:
      break;
    case L_SYMBOL:
      mark(plist(p));
      break;
    case L_CONS:
      mark(hd(p));
      mark(tl(p));
      break;
    case L_NODE:
      if (op(p) != 0 && op(p)->type != L_SYMBOL)
	mark(op(p));
      mark(attrs(p));
      mark(args(p));
      break;
    default:
      assert(0);
    }
}


static void
markv(lv* p, int v)
{
  int old = mark_value;

  assert(old > 0);

  mark_value = v;
  unmark(p);
  /* mark(p); */
  mark_value = old;
}


/* For debugging. */
int
check_zero_mark(lv* p)
{
  if (p == 0) return 1;

  switch (p->type)
    {
    case L_STRING:
    case L_OTHER:
    case L_FIXNUM:
    case L_DOUBLE:
      return p->mark == 0;
      break;
    case L_SYMBOL:
      if (p->mark != 0)
	return 0;
      else
	return check_zero_mark(plist(p));
      break;
    case L_CONS:
      if (check_zero_mark(hd(p)) && check_zero_mark(tl(p)))
	return p->mark == 0;	
      else
	return 0;
      break;
    case L_NODE:
      if (op(p) != 0
	  && op(p)->type != L_SYMBOL
	  && check_zero_mark(op(p))
	  && check_zero_mark(attrs(p))
	  && check_zero_mark(args(p)))
	return p->mark == 0;
      else
	return 0;
      break;
    default:
      assert(0);
    }
  return 0;
}

static void
sweep()
{
  lv *p, *q;
  lv *a = 0;
  lv *f = freelist;
    
  for (p = alloclist; p; p = q)
    {
      q = p->link;
      if (p->flags & L_STATIC)
	;
      else if (p->mark)
	{
	  p->mark = 0;
	  p->link = a;
	  a = p;
	}
      else
	{
	  switch (p->type)
	    {
	    case L_SYMBOL:
	      free(pname(p));
	      break;
	    case L_STRING:
	      free(str(p));
	      break;
	    }
	  p->type = L_FREE;
	  p->link = f;
	  f = p;
	}
    }
  alloclist = a;
  freelist = f;
}

#endif /* _CRSCL_LISP_I */

/* end of file -- crscl.c -- */
