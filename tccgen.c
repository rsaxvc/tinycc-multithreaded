/*
 *  TCC - Tiny C Compiler
 * 
 *  Copyright (c) 2001-2004 Fabrice Bellard
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

#define USING_GLOBALS
#include "tcc.h"

/********************************************************/
/* global variables */

#define vstack(s1) (s1->_vstack + 1)

#define unevalmask 0xffff /* unevaluated subexpression */
#define NODATA_WANTED (s1->nocode_wanted > 0) /* no static data output wanted either */
#define STATIC_DATA_WANTED (s1->nocode_wanted & 0xC0000000) /* only static data output */

/* Automagical code suppression ----> */
#define CODE_OFF(s1) (s1->nocode_wanted |= 0x20000000)
#define CODE_ON(s1) (s1->nocode_wanted &= ~0x20000000)

static void tcc_tcov_block_begin(TCCState *s1);

/* Clear 'nocode_wanted' at label if it was used */
ST_FUNC void gsym(TCCState *s1, int t) { if (t) { gsym_addr(s1, t, s1->ind); CODE_ON(s1); }}
static int gind(TCCState *s1) { int t = s1->ind; CODE_ON(s1); if (s1->debug_modes) tcc_tcov_block_begin(s1); return t; }

/* Set 'nocode_wanted' after unconditional jumps */
static void gjmp_addr_acs(TCCState *s1, int t) { gjmp_addr(s1, t); CODE_OFF(s1); }
static int gjmp_acs(TCCState *s1, int t) { t = gjmp(s1, t); CODE_OFF(s1); return t; }

/* These are #undef'd at the end of this file */
#define gjmp_addr gjmp_addr_acs
#define gjmp gjmp_acs
/* <---- */

#if PTR_SIZE == 4
#define VT_SIZE_T (VT_INT | VT_UNSIGNED)
#define VT_PTRDIFF_T VT_INT
#elif LONG_SIZE == 4
#define VT_SIZE_T (VT_LLONG | VT_UNSIGNED)
#define VT_PTRDIFF_T VT_LLONG
#else
#define VT_SIZE_T (VT_LONG | VT_LLONG | VT_UNSIGNED)
#define VT_PTRDIFF_T (VT_LONG | VT_LLONG)
#endif

typedef struct {
    Section *sec;
    int local_offset;
    Sym *flex_array_ref;
} init_params;

/********************************************************/
/* stab debug support */

static const struct {
  int type;
  const char *name;
} default_debug[] = {
    {   VT_INT, "int:t1=r1;-2147483648;2147483647;" },
    {   VT_BYTE, "char:t2=r2;0;127;" },
#if LONG_SIZE == 4
    {   VT_LONG | VT_INT, "long int:t3=r3;-2147483648;2147483647;" },
#else
    {   VT_LLONG | VT_LONG, "long int:t3=r3;-9223372036854775808;9223372036854775807;" },
#endif
    {   VT_INT | VT_UNSIGNED, "unsigned int:t4=r4;0;037777777777;" },
#if LONG_SIZE == 4
    {   VT_LONG | VT_INT | VT_UNSIGNED, "long unsigned int:t5=r5;0;037777777777;" },
#else
    /* use octal instead of -1 so size_t works (-gstabs+ in gcc) */
    {   VT_LLONG | VT_LONG | VT_UNSIGNED, "long unsigned int:t5=r5;0;01777777777777777777777;" },
#endif
    {   VT_QLONG, "__int128:t6=r6;0;-1;" },
    {   VT_QLONG | VT_UNSIGNED, "__int128 unsigned:t7=r7;0;-1;" },
    {   VT_LLONG, "long long int:t8=r8;-9223372036854775808;9223372036854775807;" },
    {   VT_LLONG | VT_UNSIGNED, "long long unsigned int:t9=r9;0;01777777777777777777777;" },
    {   VT_SHORT, "short int:t10=r10;-32768;32767;" },
    {   VT_SHORT | VT_UNSIGNED, "short unsigned int:t11=r11;0;65535;" },
    {   VT_BYTE | VT_DEFSIGN, "signed char:t12=r12;-128;127;" },
    {   VT_BYTE | VT_DEFSIGN | VT_UNSIGNED, "unsigned char:t13=r13;0;255;" },
    {   VT_FLOAT, "float:t14=r1;4;0;" },
    {   VT_DOUBLE, "double:t15=r1;8;0;" },
#ifdef TCC_USING_DOUBLE_FOR_LDOUBLE
    {   VT_DOUBLE | VT_LONG, "long double:t16=r1;8;0;" },
#else
    {   VT_LDOUBLE, "long double:t16=r1;16;0;" },
#endif
    {   -1, "_Float32:t17=r1;4;0;" },
    {   -1, "_Float64:t18=r1;8;0;" },
    {   -1, "_Float128:t19=r1;16;0;" },
    {   -1, "_Float32x:t20=r1;8;0;" },
    {   -1, "_Float64x:t21=r1;16;0;" },
    {   -1, "_Decimal32:t22=r1;4;0;" },
    {   -1, "_Decimal64:t23=r1;8;0;" },
    {   -1, "_Decimal128:t24=r1;16;0;" },
    /* if default char is unsigned */
    {   VT_BYTE | VT_UNSIGNED, "unsigned char:t25=r25;0;255;" },
    /* boolean type */
    {   VT_BOOL, "bool:t26=r26;0;255;" },
    {   VT_VOID, "void:t27=27" },
};

static int debug_next_type;

static struct debug_hash {
    int debug_type;
    Sym *type;
} *debug_hash;

static int n_debug_hash;

static struct debug_info {
    int start;
    int end;
    int n_sym;
    struct debug_sym {
        int type;
        unsigned long value;
        char *str;
        Section *sec;
        int sym_index;
    } *sym;
    struct debug_info *child, *next, *last, *parent;
} *debug_info, *debug_info_root;

static struct {
    unsigned long offset;
    unsigned long last_file_name;
    unsigned long last_func_name;
    int ind;
    int line;
} tcov_data;

/********************************************************/
#if 1
#define precedence_parser
static void init_prec(void);
#endif
/********************************************************/
#ifndef CONFIG_TCC_ASM
ST_FUNC void asm_instr(TCCState *s1)
{
    tcc_error(s1, "inline asm() not supported");
}
ST_FUNC void asm_global_instr(TCCState *s1)
{
    tcc_error(s1, "inline asm() not supported");
}
#endif

/* ------------------------------------------------------------------------- */
static void gen_cast(TCCState *s1, CType *type);
static void gen_cast_s(TCCState *s1, int t);
static inline CType *pointed_type(CType *type);
static int is_compatible_types(CType *type1, CType *type2);
static int parse_btype(TCCState *s1, CType *type, AttributeDef *ad);
static CType *type_decl(TCCState *s1, CType *type, AttributeDef *ad, int *v, int td);
static void parse_expr_type(TCCState *s1, CType *type);
static void init_putv(TCCState *s1, init_params *p, CType *type, unsigned long c);
static void decl_initializer(TCCState *s1, init_params *p, CType *type, unsigned long c, int flags);
static void block(TCCState *s1, int is_expr);
static void decl_initializer_alloc(TCCState *s1, CType *type, AttributeDef *ad, int r, int has_init, int v, int scope);
static void decl(TCCState *s1, int l);
static int decl0(TCCState *s1, int l, int is_for_loop_init, Sym *);
static void expr_eq(TCCState *s1);
static void vla_runtime_type_size(TCCState *s1, CType *type, int *a);
static int is_compatible_unqualified_types(CType *type1, CType *type2);
static inline int64_t expr_const64(TCCState *s1);
static void vpush64(TCCState *s1, int ty, unsigned long long v);
static void vpush(TCCState *s1, CType *type);
static int gvtst(TCCState *s1, int inv, int t);
static void gen_inline_functions(TCCState *s);
static void free_inline_functions(TCCState *s);
static void skip_or_save_block(TCCState *s1, TokenString **str);
static void gv_dup(TCCState *s1);
static int get_temp_local_var(TCCState *s1, int size,int align);
static void clear_temp_local_var_list(TCCState *s1);
static void cast_error(TCCState *s1, CType *st, CType *dt);

ST_INLN int is_float(int t)
{
    int bt = t & VT_BTYPE;
    return bt == VT_LDOUBLE
        || bt == VT_DOUBLE
        || bt == VT_FLOAT
        || bt == VT_QFLOAT;
}

static inline int is_integer_btype(int bt)
{
    return bt == VT_BYTE
        || bt == VT_BOOL
        || bt == VT_SHORT
        || bt == VT_INT
        || bt == VT_LLONG;
}

static int btype_size(int bt)
{
    return bt == VT_BYTE || bt == VT_BOOL ? 1 :
        bt == VT_SHORT ? 2 :
        bt == VT_INT ? 4 :
        bt == VT_LLONG ? 8 :
        bt == VT_PTR ? PTR_SIZE : 0;
}

/* returns function return register from type */
static int R_RET(int t)
{
    if (!is_float(t))
        return REG_IRET;
#ifdef TCC_TARGET_X86_64
    if ((t & VT_BTYPE) == VT_LDOUBLE)
        return TREG_ST0;
#elif defined TCC_TARGET_RISCV64
    if ((t & VT_BTYPE) == VT_LDOUBLE)
        return REG_IRET;
#endif
    return REG_FRET;
}

/* returns 2nd function return register, if any */
static int R2_RET(int t)
{
    t &= VT_BTYPE;
#if PTR_SIZE == 4
    if (t == VT_LLONG)
        return REG_IRE2;
#elif defined TCC_TARGET_X86_64
    if (t == VT_QLONG)
        return REG_IRE2;
    if (t == VT_QFLOAT)
        return REG_FRE2;
#elif defined TCC_TARGET_RISCV64
    if (t == VT_LDOUBLE)
        return REG_IRE2;
#endif
    return VT_CONST;
}

/* returns true for two-word types */
#define USING_TWO_WORDS(t) (R2_RET(t) != VT_CONST)

/* put function return registers to stack value */
static void PUT_R_RET(SValue *sv, int t)
{
    sv->r = R_RET(t), sv->r2 = R2_RET(t);
}

/* returns function return register class for type t */
static int RC_RET(int t)
{
    return reg_classes[R_RET(t)] & ~(RC_FLOAT | RC_INT);
}

/* returns generic register class for type t */
static int RC_TYPE(int t)
{
    if (!is_float(t))
        return RC_INT;
#ifdef TCC_TARGET_X86_64
    if ((t & VT_BTYPE) == VT_LDOUBLE)
        return RC_ST0;
    if ((t & VT_BTYPE) == VT_QFLOAT)
        return RC_FRET;
#elif defined TCC_TARGET_RISCV64
    if ((t & VT_BTYPE) == VT_LDOUBLE)
        return RC_INT;
#endif
    return RC_FLOAT;
}

/* returns 2nd register class corresponding to t and rc */
static int RC2_TYPE(int t, int rc)
{
    if (!USING_TWO_WORDS(t))
        return 0;
#ifdef RC_IRE2
    if (rc == RC_IRET)
        return RC_IRE2;
#endif
#ifdef RC_FRE2
    if (rc == RC_FRET)
        return RC_FRE2;
#endif
    if (rc & RC_FLOAT)
        return RC_FLOAT;
    return RC_INT;
}

/* we use our own 'finite' function to avoid potential problems with
   non standard math libs */
/* XXX: endianness dependent */
ST_FUNC int ieee_finite(double d)
{
    int p[4];
    memcpy(p, &d, sizeof(double));
    return ((unsigned)((p[1] | 0x800fffff) + 1)) >> 31;
}

/* compiling intel long double natively */
#if (defined __i386__ || defined __x86_64__) \
    && (defined TCC_TARGET_I386 || defined TCC_TARGET_X86_64)
# define TCC_IS_NATIVE_387
#endif

ST_FUNC void test_lvalue(TCCState *s1)
{
    if (!(s1->vtop->r & VT_LVAL))
        expect(s1, "lvalue");
}

ST_FUNC void check_vstack(TCCState *s1)
{
    if (s1->vtop != vstack(s1) - 1)
        tcc_error(s1, "internal compiler error: vstack leak (%d)",
                  (int)(s1->vtop - vstack(s1) + 1));
}

/* ------------------------------------------------------------------------- */
/* vstack debugging aid */

#if 0
void pv (const char *lbl, int a, int b)
{
    int i;
    for (i = a; i < a + b; ++i) {
        SValue *p = &s1->vtop[-i];
        printf("%s s1->vtop[-%d] : type.t:%04x  r:%04x  r2:%04x  c.i:%d\n",
            lbl, i, p->type.t, p->r, p->r2, (int)p->c.i);
    }
}
#endif

/* ------------------------------------------------------------------------- */
/* start of translation unit info */
ST_FUNC void tcc_debug_start(TCCState *s1)
{
    if (s1->do_debug) {
        int i;
        char buf[512];

        /* file info: full path + filename */
        s1->section_sym = put_elf_sym(symtab_section, 0, 0,
                                  ELFW(ST_INFO)(STB_LOCAL, STT_SECTION), 0,
                                  text_section->sh_num, NULL);
        getcwd(buf, sizeof(buf));
#ifdef _WIN32
        normalize_slashes(buf);
#endif
        pstrcat(buf, sizeof(buf), "/");
        put_stabs_r(s1, buf, N_SO, 0, 0,
                    text_section->data_offset, text_section, s1->section_sym);
        put_stabs_r(s1, s1->file->prev ? s1->file->prev->filename : s1->file->filename,
                    N_SO, 0, 0,
                    text_section->data_offset, text_section, s1->section_sym);
        for (i = 0; i < sizeof (default_debug) / sizeof (default_debug[0]); i++)
            put_stabs(s1, default_debug[i].name, N_LSYM, 0, 0, 0);

        s1->new_file = s1->last_line_num = 0;
        s1->func_ind = -1;
        debug_next_type = sizeof(default_debug) / sizeof(default_debug[0]);
        debug_hash = NULL;
        n_debug_hash = 0;

        /* we're currently 'including' the <command line> */
        tcc_debug_bincl(s1);
    }

    /* an elf symbol of type STT_FILE must be put so that STB_LOCAL
       symbols can be safely used */
    put_elf_sym(symtab_section, 0, 0,
                ELFW(ST_INFO)(STB_LOCAL, STT_FILE), 0,
                SHN_ABS, s1->file->filename);
}

/* put end of translation unit info */
ST_FUNC void tcc_debug_end(TCCState *s1)
{
    if (!s1->do_debug)
        return;
    put_stabs_r(s1, NULL, N_SO, 0, 0,
        text_section->data_offset, text_section, s1->section_sym);
    tcc_free(s1, debug_hash);
}

static BufferedFile* put_new_file(TCCState *s1)
{
    BufferedFile *f = s1->file;
    /* use upper file if from inline ":asm:" */
    if (f->filename[0] == ':')
        f = f->prev;
    if (f && s1->new_file) {
        put_stabs_r(s1, f->filename, N_SOL, 0, 0, s1->ind, text_section, s1->section_sym);
        s1->new_file = s1->last_line_num = 0;
    }
    return f;
}

/* put alternative filename */
ST_FUNC void tcc_debug_putfile(TCCState *s1, const char *filename)
{
    if (0 == strcmp(s1->file->filename, filename))
        return;
    pstrcpy(s1->file->filename, sizeof(s1->file->filename), filename);
    s1->new_file = 1;
}

/* begin of #include */
ST_FUNC void tcc_debug_bincl(TCCState *s1)
{
    if (!s1->do_debug)
        return;
    put_stabs(s1, s1->file->filename, N_BINCL, 0, 0, 0);
    s1->new_file = 1;
}

/* end of #include */
ST_FUNC void tcc_debug_eincl(TCCState *s1)
{
    if (!s1->do_debug)
        return;
    put_stabn(s1, N_EINCL, 0, 0, 0);
    s1->new_file = 1;
}

/* generate line number info */
static void tcc_debug_line(TCCState *s1)
{
    BufferedFile *f;
    if (!s1->do_debug
        || cur_text_section != text_section
        || !(f = put_new_file(s1))
        || s1->last_line_num == f->line_num)
        return;
    if (s1->func_ind != -1) {
        put_stabn(s1, N_SLINE, 0, f->line_num, s1->ind - s1->func_ind);
    } else {
        /* from tcc_assemble */
        put_stabs_r(s1, NULL, N_SLINE, 0, f->line_num, s1->ind, text_section, s1->section_sym);
    }
    s1->last_line_num = f->line_num;
}

static void tcc_debug_stabs (TCCState *s1, const char *str, int type, unsigned long value,
                             Section *sec, int sym_index)
{
    struct debug_sym *s;

    if (debug_info) {
        debug_info->sym =
            (struct debug_sym *)tcc_realloc (s1, debug_info->sym,
                                             sizeof(struct debug_sym) *
                                             (debug_info->n_sym + 1));
        s = debug_info->sym + debug_info->n_sym++;
        s->type = type;
        s->value = value;
        s->str = tcc_strdup(s1, str);
        s->sec = sec;
        s->sym_index = sym_index;
    }
    else if (sec)
        put_stabs_r (s1, str, type, 0, 0, value, sec, sym_index);
    else
        put_stabs (s1, str, type, 0, 0, value);
}

static void tcc_debug_stabn(TCCState *s1, int type, int value)
{
    if (!s1->do_debug)
        return;
    if (type == N_LBRAC) {
        struct debug_info *info =
            (struct debug_info *) tcc_mallocz(s1, sizeof (*info));

        info->start = value;
        info->parent = debug_info;
        if (debug_info) {
            if (debug_info->child) {
                if (debug_info->child->last)
                    debug_info->child->last->next = info;
                else
                    debug_info->child->next = info;
                debug_info->child->last = info;
            }
            else
                debug_info->child = info;
        }
        else
            debug_info_root = info;
        debug_info = info;
    }
    else {
        debug_info->end = value;
        debug_info = debug_info->parent;
    }
}

static void tcc_get_debug_info(TCCState *s1, Sym *s, CString *result)
{
    int type;
    int n = 0;
    int debug_type = -1;
    Sym *t = s;
    CString str;

    for (;;) {
        type = t->type.t & ~(VT_STORAGE | VT_CONSTANT | VT_VOLATILE);
        if ((type & VT_BTYPE) != VT_BYTE)
            type &= ~VT_DEFSIGN;
        if (type == VT_PTR || type == (VT_PTR | VT_ARRAY))
            n++, t = t->type.ref;
        else
            break;
    }
    if ((type & VT_BTYPE) == VT_STRUCT) {
        int i;

        t = t->type.ref;
        for (i = 0; i < n_debug_hash; i++) {
            if (t == debug_hash[i].type) {
                debug_type = debug_hash[i].debug_type;
                break;
            }
        }
        if (debug_type == -1) {
            debug_type = ++debug_next_type;
            debug_hash = (struct debug_hash *)
                tcc_realloc (s1, debug_hash,
                             (n_debug_hash + 1) * sizeof(*debug_hash));
            debug_hash[n_debug_hash].debug_type = debug_type;
            debug_hash[n_debug_hash++].type = t;
            cstr_new (&str);
            cstr_printf (s1, &str, "%s:T%d=%c%d",
                         (t->v & ~SYM_STRUCT) >= SYM_FIRST_ANOM
                         ? "" : get_tok_str(s1, t->v & ~SYM_STRUCT, NULL),
                         debug_type,
                         IS_UNION (t->type.t) ? 'u' : 's',
                         t->c);
            while (t->next) {
                int pos, size, align;

                t = t->next;
                cstr_printf (s1, &str, "%s:",
                             (t->v & ~SYM_FIELD) >= SYM_FIRST_ANOM
                             ? "" : get_tok_str(s1, t->v & ~SYM_FIELD, NULL));
                tcc_get_debug_info (s1, t, &str);
                if (t->type.t & VT_BITFIELD) {
                    pos = t->c * 8 + BIT_POS(t->type.t);
                    size = BIT_SIZE(t->type.t);
                }
                else {
                    pos = t->c * 8;
                    size = type_size(s1, &t->type, &align) * 8;
                }
                cstr_printf (s1, &str, ",%d,%d;", pos, size);
            }
            cstr_printf (s1, &str, ";");
            tcc_debug_stabs(s1, str.data, N_LSYM, 0, NULL, 0);
            cstr_free (s1, &str);
        }
    }
    else if (IS_ENUM(type)) {
        Sym *e = t = t->type.ref;

        debug_type = ++debug_next_type;
        cstr_new (&str);
        cstr_printf(s1, &str, "%s:T%d=e",
                     (t->v & ~SYM_STRUCT) >= SYM_FIRST_ANOM
                     ? "" : get_tok_str(s1, t->v & ~SYM_STRUCT, NULL),
                     debug_type);
        while (t->next) {
            t = t->next;
            cstr_printf(s1, &str, "%s:",
                         (t->v & ~SYM_FIELD) >= SYM_FIRST_ANOM
                         ? "" : get_tok_str(s1, t->v & ~SYM_FIELD, NULL));
            cstr_printf(s1, &str, e->type.t & VT_UNSIGNED ? "%u," : "%d,",
                         (int)t->enum_val);
        }
        cstr_printf(s1, &str, ";");
        tcc_debug_stabs(s1, str.data, N_LSYM, 0, NULL, 0);
        cstr_free (s1, &str);
    }
    else if ((type & VT_BTYPE) != VT_FUNC) {
        type &= ~VT_STRUCT_MASK;
        for (debug_type = 1;
             debug_type <= sizeof(default_debug) / sizeof(default_debug[0]);
             debug_type++)
            if (default_debug[debug_type - 1].type == type)
                break;
        if (debug_type > sizeof(default_debug) / sizeof(default_debug[0]))
            return;
    }
    if (n > 0)
        cstr_printf (s1, result, "%d=", ++debug_next_type);
    t = s;
    for (;;) {
        type = t->type.t & ~(VT_STORAGE | VT_CONSTANT | VT_VOLATILE);
        if ((type & VT_BTYPE) != VT_BYTE)
            type &= ~VT_DEFSIGN;
        if (type == VT_PTR)
            cstr_printf (s1, result, "%d=*", ++debug_next_type);
        else if (type == (VT_PTR | VT_ARRAY))
            cstr_printf (s1, result, "%d=ar1;0;%d;",
                         ++debug_next_type, t->type.ref->c - 1);
        else if (type == VT_FUNC) {
            cstr_printf (s1, result, "%d=f", ++debug_next_type);
            tcc_get_debug_info (s1, t->type.ref, result);
            return;
        }
        else
            break;
        t = t->type.ref;
    }
    cstr_printf (s1, result, "%d", debug_type);
}

static void tcc_debug_finish (TCCState *s1, struct debug_info *cur)
{
    while (cur) {
        int i;
        struct debug_info *next = cur->next;

        for (i = 0; i < cur->n_sym; i++) {
            struct debug_sym *s = &cur->sym[i];

            if (s->sec)
                put_stabs_r(s1, s->str, s->type, 0, 0, s->value,
                            s->sec, s->sym_index);
            else
                put_stabs(s1, s->str, s->type, 0, 0, s->value);
            tcc_free (s1, s->str);
        }
        tcc_free (s1, cur->sym);
        put_stabn(s1, N_LBRAC, 0, 0, cur->start);
        tcc_debug_finish (s1, cur->child);
        put_stabn(s1, N_RBRAC, 0, 0, cur->end);
        tcc_free (s1, cur);
        cur = next;
    }
}

static void tcc_add_debug_info(TCCState *s1, int param, Sym *s, Sym *e)
{
    CString debug_str;
    if (!s1->do_debug)
        return;
    cstr_new (&debug_str);
    for (; s != e; s = s->prev) {
        if (!s->v || (s->r & VT_VALMASK) != VT_LOCAL)
            continue;
        cstr_reset (&debug_str);
        cstr_printf(s1, &debug_str, "%s:%s", get_tok_str(s1, s->v, NULL), param ? "p" : "");
        tcc_get_debug_info(s1, s, &debug_str);
        tcc_debug_stabs(s1, debug_str.data, param ? N_PSYM : N_LSYM, s->c, NULL, 0);
    }
    cstr_free (s1, &debug_str);
}

/* put function symbol */
static void tcc_debug_funcstart(TCCState *s1, Sym *sym)
{
    CString debug_str;
    BufferedFile *f;
    if (!s1->do_debug)
        return;
    debug_info_root = NULL;
    debug_info = NULL;
    tcc_debug_stabn(s1, N_LBRAC, s1->ind - s1->func_ind);
    if (!(f = put_new_file(s1)))
        return;
    cstr_new (&debug_str);
    cstr_printf(s1, &debug_str, "%s:%c", s1->funcname, sym->type.t & VT_STATIC ? 'f' : 'F');
    tcc_get_debug_info(s1, sym->type.ref, &debug_str);
    put_stabs_r(s1, debug_str.data, N_FUN, 0, f->line_num, 0, cur_text_section, sym->c);
    cstr_free (s1, &debug_str);

    tcc_debug_line(s1);
}

/* put function size */
static void tcc_debug_funcend(TCCState *s1, int size)
{
    if (!s1->do_debug)
        return;
    tcc_debug_stabn(s1, N_RBRAC, size);
    tcc_debug_finish (s1, debug_info_root);
}


static void tcc_debug_extern_sym(TCCState *s1, Sym *sym, int sh_num, int sym_bind, int sym_type)
{
    Section *s;
    CString str;

    if (!s1->do_debug)
        return;
    if (sym_type == STT_FUNC || sym->v >= SYM_FIRST_ANOM)
        return;
    s = s1->sections[sh_num];

    cstr_new (&str);
    cstr_printf(s1, &str, "%s:%c",
        get_tok_str(s1, sym->v, NULL),
        sym_bind == STB_GLOBAL ? 'G' : s1->local_scope ? 'V' : 'S'
        );
    tcc_get_debug_info(s1, sym, &str);
    if (sym_bind == STB_GLOBAL)
        tcc_debug_stabs(s1, str.data, N_GSYM, 0, NULL, 0);
    else
        tcc_debug_stabs(s1, str.data,
            (sym->type.t & VT_STATIC) && data_section == s
            ? N_STSYM : N_LCSYM, 0, s, sym->c);
    cstr_free (s1, &str);
}

static void tcc_debug_typedef(TCCState *s1, Sym *sym)
{
    CString str;

    if (!s1->do_debug)
        return;
    cstr_new (&str);
    cstr_printf(s1, &str, "%s:t",
                 (sym->v & ~SYM_FIELD) >= SYM_FIRST_ANOM
                 ? "" : get_tok_str(s1, sym->v & ~SYM_FIELD, NULL));
    tcc_get_debug_info(s1, sym, &str);
    tcc_debug_stabs(s1, str.data, N_LSYM, 0, NULL, 0);
    cstr_free (s1, &str);
}

/* ------------------------------------------------------------------------- */
/* for section layout see lib/tcov.c */

static void tcc_tcov_block_end(TCCState *s1, int line);

static void tcc_tcov_block_begin(TCCState *s1)
{
    SValue sv;
    void *ptr;
    unsigned long last_offset = tcov_data.offset;

    tcc_tcov_block_end (s1, 0);
    if (s1->test_coverage == 0 || s1->nocode_wanted)
	return;

    if (tcov_data.last_file_name == 0 ||
	strcmp ((const char *)(tcov_section->data + tcov_data.last_file_name),
		s1->file->true_filename) != 0) {
	char wd[1024];
	CString cstr;

	if (tcov_data.last_func_name)
	    section_ptr_add(tcov_section, 1);
	if (tcov_data.last_file_name)
	    section_ptr_add(tcov_section, 1);
	tcov_data.last_func_name = 0;
	cstr_new (&cstr);
	if (s1->file->true_filename[0] == '/') {
	    tcov_data.last_file_name = tcov_section->data_offset;
	    cstr_printf(s1, &cstr, "%s", s1->file->true_filename);
	}
	else {
	    getcwd (wd, sizeof(wd));
	    tcov_data.last_file_name = tcov_section->data_offset + strlen(wd) + 1;
	    cstr_printf(s1, &cstr, "%s/%s", wd, s1->file->true_filename);
	}
	ptr = section_ptr_add(tcov_section, cstr.size + 1);
	strncpy((char *)ptr, cstr.data, cstr.size);
#ifdef _WIN32
        normalize_slashes((char *)ptr);
#endif
	cstr_free (s1, &cstr);
    }
    if (tcov_data.last_func_name == 0 ||
	strcmp ((const char *)(tcov_section->data + tcov_data.last_func_name),
		s1->funcname) != 0) {
	size_t len;

	if (tcov_data.last_func_name)
	    section_ptr_add(tcov_section, 1);
	tcov_data.last_func_name = tcov_section->data_offset;
	len = strlen (s1->funcname);
	ptr = section_ptr_add(tcov_section, len + 1);
	strncpy((char *)ptr, s1->funcname, len);
	section_ptr_add(tcov_section, -tcov_section->data_offset & 7);
	ptr = section_ptr_add(tcov_section, 8);
	write64le (ptr, s1->file->line_num);
    }
    if (s1->ind == tcov_data.ind && tcov_data.line == s1->file->line_num)
        tcov_data.offset = last_offset;
    else {
        Sym label = {0};
        label.type.t = VT_LLONG | VT_STATIC;

        ptr = section_ptr_add(tcov_section, 16);
        tcov_data.line = s1->file->line_num;
        write64le (ptr, (tcov_data.line << 8) | 0xff);
        put_extern_sym(s1, &label, tcov_section,
		       ((unsigned char *)ptr - tcov_section->data) + 8, 0);
        sv.type = label.type;
        sv.r = VT_SYM | VT_LVAL | VT_CONST;
        sv.r2 = VT_CONST;
        sv.c.i = 0;
        sv.sym = &label;
#if defined TCC_TARGET_I386 || defined TCC_TARGET_X86_64 || \
    defined TCC_TARGET_ARM || defined TCC_TARGET_ARM64 || \
    defined TCC_TARGET_RISCV64
        gen_increment_tcov (s1, &sv);
#else
        vpushv(s1, &sv);
        inc(s1, 0, TOK_INC);
        vpop(s1);
#endif
        tcov_data.offset = (unsigned char *)ptr - tcov_section->data;
        tcov_data.ind = s1->ind;
    }
}

static void tcc_tcov_block_end(TCCState *s1, int line)
{
    if (s1->test_coverage == 0)
	return;
    if (tcov_data.offset) {
	void *ptr = tcov_section->data + tcov_data.offset;
	unsigned long long nline = line ? line : s1->file->line_num;

	write64le (ptr, (read64le (ptr) & 0xfffffffffull) | (nline << 36));
	tcov_data.offset = 0;
    }
}

static void tcc_tcov_check_line(TCCState *s1, int start)
{
    if (s1->test_coverage == 0)
	return;
    if (tcov_data.line != s1->file->line_num) {
        if ((tcov_data.line + 1) != s1->file->line_num) {
	    tcc_tcov_block_end (s1, tcov_data.line);
	    if (start)
                tcc_tcov_block_begin (s1);
	}
	else
	    tcov_data.line = s1->file->line_num;
    }
}

static void tcc_tcov_start(TCCState *s1)
{
    if (s1->test_coverage == 0)
	return;
    memset (&tcov_data, 0, sizeof (tcov_data));
    if (tcov_section == NULL) {
        tcov_section = new_section(s1, ".tcov", SHT_PROGBITS,
				   SHF_ALLOC | SHF_WRITE);
	section_ptr_add(tcov_section, 4); // pointer to executable name
    }
}

static void tcc_tcov_end(TCCState *s1)
{
    if (s1->test_coverage == 0)
	return;
    if (tcov_data.last_func_name)
        section_ptr_add(tcov_section, 1);
    if (tcov_data.last_file_name)
        section_ptr_add(tcov_section, 1);
}

/* ------------------------------------------------------------------------- */
/* initialize vstack and types.  This must be done also for tcc -E */
ST_FUNC void tccgen_init(TCCState *s1)
{
    s1->vtop = vstack(s1) - 1;
    memset(s1->vtop, 0, sizeof *s1->vtop);

    /* define some often used types */
    s1->int_type.t = VT_INT;

    s1->char_type.t = VT_BYTE;
    if (s1->char_is_unsigned)
        s1->char_type.t |= VT_UNSIGNED;
    s1->char_pointer_type = s1->char_type;
    mk_pointer(s1, &s1->char_pointer_type);

    s1->func_old_type.t = VT_FUNC;
    s1->func_old_type.ref = sym_push(s1, SYM_FIELD, &s1->int_type, 0, 0);
    s1->func_old_type.ref->f.func_call = FUNC_CDECL;
    s1->func_old_type.ref->f.func_type = FUNC_OLD;
#ifdef precedence_parser
    init_prec();
#endif
    cstr_new(&s1->initstr);
}

ST_FUNC int tccgen_compile(TCCState *s1)
{
    cur_text_section = NULL;
    s1->funcname = "";
    s1->anon_sym = SYM_FIRST_ANOM;
    s1->section_sym = 0;
    s1->const_wanted = 0;
    s1->nocode_wanted = 0x80000000;
    s1->local_scope = 0;
    s1->debug_modes = s1->do_debug | s1->test_coverage << 1;

    tcc_debug_start(s1);
    tcc_tcov_start (s1);
#ifdef TCC_TARGET_ARM
    arm_init(s1);
#endif
#ifdef INC_DEBUG
    printf("%s: **** new file\n", s1->file->filename);
#endif
    s1->parse_flags = PARSE_FLAG_PREPROCESS | PARSE_FLAG_TOK_NUM | PARSE_FLAG_TOK_STR;
    next(s1);
    decl(s1, VT_CONST);
    gen_inline_functions(s1);
    check_vstack(s1);
    /* end of translation unit info */
    tcc_debug_end(s1);
    tcc_tcov_end (s1);
    return 0;
}

ST_FUNC void tccgen_finish(TCCState *s1)
{
    cstr_free(s1, &s1->initstr);
    free_inline_functions(s1);
    sym_pop(s1, &s1->global_stack, NULL, 0);
    sym_pop(s1, &s1->local_stack, NULL, 0);
    /* free preprocessor macros */
    free_defines(s1, NULL);
    /* free sym_pools */
    dynarray_reset(s1, &s1->sym_pools, &s1->nb_sym_pools);
    s1->sym_free_first = NULL;
}

/* ------------------------------------------------------------------------- */
ST_FUNC ElfSym *elfsym(TCCState *s1, Sym *s)
{
  if (!s || !s->c)
    return NULL;
  return &((ElfSym *)symtab_section->data)[s->c];
}

/* apply storage attributes to Elf symbol */
ST_FUNC void update_storage(TCCState *s1, Sym *sym)
{
    ElfSym *esym;
    int sym_bind, old_sym_bind;

    esym = elfsym(s1, sym);
    if (!esym)
        return;

    if (sym->a.visibility)
        esym->st_other = (esym->st_other & ~ELFW(ST_VISIBILITY)(-1))
            | sym->a.visibility;

    if (sym->type.t & (VT_STATIC | VT_INLINE))
        sym_bind = STB_LOCAL;
    else if (sym->a.weak)
        sym_bind = STB_WEAK;
    else
        sym_bind = STB_GLOBAL;
    old_sym_bind = ELFW(ST_BIND)(esym->st_info);
    if (sym_bind != old_sym_bind) {
        esym->st_info = ELFW(ST_INFO)(sym_bind, ELFW(ST_TYPE)(esym->st_info));
    }

#ifdef TCC_TARGET_PE
    if (sym->a.dllimport)
        esym->st_other |= ST_PE_IMPORT;
    if (sym->a.dllexport)
        esym->st_other |= ST_PE_EXPORT;
#endif

#if 0
    printf("storage %s: bind=%c vis=%d exp=%d imp=%d\n",
        get_tok_str(sym->v, NULL),
        sym_bind == STB_WEAK ? 'w' : sym_bind == STB_LOCAL ? 'l' : 'g',
        sym->a.visibility,
        sym->a.dllexport,
        sym->a.dllimport
        );
#endif
}

/* ------------------------------------------------------------------------- */
/* update sym->c so that it points to an external symbol in section
   'section' with value 'value' */

ST_FUNC void put_extern_sym2(TCCState *s1, Sym *sym, int sh_num,
                            addr_t value, unsigned long size,
                            int can_add_underscore)
{
    int sym_type, sym_bind, info, other, t;
    ElfSym *esym;
    const char *name;
    char buf1[256];

    if (!sym->c) {
        name = get_tok_str(s1, sym->v, NULL);
        t = sym->type.t;
        if ((t & VT_BTYPE) == VT_FUNC) {
            sym_type = STT_FUNC;
        } else if ((t & VT_BTYPE) == VT_VOID) {
            sym_type = STT_NOTYPE;
            if ((t & (VT_BTYPE|VT_ASM_FUNC)) == VT_ASM_FUNC)
                sym_type = STT_FUNC;
        } else {
            sym_type = STT_OBJECT;
        }
        if (t & (VT_STATIC | VT_INLINE))
            sym_bind = STB_LOCAL;
        else
            sym_bind = STB_GLOBAL;
        other = 0;

#ifdef TCC_TARGET_PE
        if (sym_type == STT_FUNC && sym->type.ref) {
            Sym *ref = sym->type.ref;
            if (ref->a.nodecorate) {
                can_add_underscore = 0;
            }
            if (ref->f.func_call == FUNC_STDCALL && can_add_underscore) {
                sprintf(buf1, "_%s@%d", name, ref->f.func_args * PTR_SIZE);
                name = buf1;
                other |= ST_PE_STDCALL;
                can_add_underscore = 0;
            }
        }
#endif

        if (sym->asm_label) {
            name = get_tok_str(s1, sym->asm_label, NULL);
            can_add_underscore = 0;
        }

        if (s1->leading_underscore && can_add_underscore) {
            buf1[0] = '_';
            pstrcpy(buf1 + 1, sizeof(buf1) - 1, name);
            name = buf1;
        }

        info = ELFW(ST_INFO)(sym_bind, sym_type);
        sym->c = put_elf_sym(symtab_section, value, size, info, other, sh_num, name);

        if (s1->debug_modes)
            tcc_debug_extern_sym(s1, sym, sh_num, sym_bind, sym_type);

    } else {
        esym = elfsym(s1, sym);
        esym->st_value = value;
        esym->st_size = size;
        esym->st_shndx = sh_num;
    }
    update_storage(s1, sym);
}

ST_FUNC void put_extern_sym(TCCState *s1, Sym *sym, Section *section,
                           addr_t value, unsigned long size)
{
    int sh_num = section ? section->sh_num : SHN_UNDEF;
    put_extern_sym2(s1, sym, sh_num, value, size, 1);
}

/* add a new relocation entry to symbol 'sym' in section 's' */
ST_FUNC void greloca(TCCState *s1, Section *s, Sym *sym, unsigned long offset, int type,
                     addr_t addend)
{
    int c = 0;

    if (s1->nocode_wanted && s == cur_text_section)
        return;

    if (sym) {
        if (0 == sym->c)
            put_extern_sym(s1, sym, NULL, 0, 0);
        c = sym->c;
    }

    /* now we can add ELF relocation info */
    put_elf_reloca(symtab_section, s, offset, type, c, addend);
}

#if PTR_SIZE == 4
ST_FUNC void greloc(TCCState *s1, Section *s, Sym *sym, unsigned long offset, int type)
{
    greloca(s1, s, sym, offset, type, 0);
}
#endif

/* ------------------------------------------------------------------------- */
/* symbol allocator */
static Sym *__sym_malloc(TCCState *s1)
{
    Sym *sym_pool, *sym, *last_sym;
    int i;

    sym_pool = tcc_malloc(s1, SYM_POOL_NB * sizeof(Sym));
    dynarray_add(s1, &s1->sym_pools, &s1->nb_sym_pools, sym_pool);

    last_sym = s1->sym_free_first;
    sym = sym_pool;
    for(i = 0; i < SYM_POOL_NB; i++) {
        sym->next = last_sym;
        last_sym = sym;
        sym++;
    }
    s1->sym_free_first = last_sym;
    return last_sym;
}

static inline Sym *sym_malloc(TCCState *s1)
{
    Sym *sym;
#ifndef SYM_DEBUG
    sym = s1->sym_free_first;
    if (!sym)
        sym = __sym_malloc(s1);
    s1->sym_free_first = sym->next;
    return sym;
#else
    sym = tcc_malloc(s1, sizeof(Sym));
    return sym;
#endif
}

ST_INLN void sym_free(TCCState *s1, Sym *sym)
{
#ifndef SYM_DEBUG
    sym->next = s1->sym_free_first;
    s1->sym_free_first = sym;
#else
    tcc_free(s1, sym);
#endif
}

/* push, without hashing */
ST_FUNC Sym *sym_push2(TCCState *s1, Sym **ps, int v, int t, int c)
{
    Sym *s;

    s = sym_malloc(s1);
    memset(s, 0, sizeof *s);
    s->v = v;
    s->type.t = t;
    s->c = c;
    /* add in stack */
    s->prev = *ps;
    *ps = s;
    return s;
}

/* find a symbol and return its associated structure. 's' is the top
   of the symbol stack */
ST_FUNC Sym *sym_find2(TCCState *s1, Sym *s, int v)
{
    while (s) {
        if (s->v == v)
            return s;
        else if (s->v == -1)
            return NULL;
        s = s->prev;
    }
    return NULL;
}

/* structure lookup */
ST_INLN Sym *struct_find(TCCState *s1, int v)
{
    v -= TOK_IDENT;
    if ((unsigned)v >= (unsigned)(s1->tok_ident - TOK_IDENT))
        return NULL;
    return s1->table_ident[v]->sym_struct;
}

/* find an identifier */
ST_INLN Sym *sym_find(TCCState *s1, int v)
{
    v -= TOK_IDENT;
    if ((unsigned)v >= (unsigned)(s1->tok_ident - TOK_IDENT))
        return NULL;
    return s1->table_ident[v]->sym_identifier;
}

static int sym_scope(TCCState *s1, Sym *s)
{
  if (IS_ENUM_VAL (s->type.t))
    return s->type.ref->sym_scope;
  else
    return s->sym_scope;
}

/* push a given symbol on the symbol stack */
ST_FUNC Sym *sym_push(TCCState *s1, int v, CType *type, int r, int c)
{
    Sym *s, **ps;
    TokenSym *ts;

    if (s1->local_stack)
        ps = &s1->local_stack;
    else
        ps = &s1->global_stack;
    s = sym_push2(s1, ps, v, type->t, c);
    s->type.ref = type->ref;
    s->r = r;
    /* don't record fields or anonymous symbols */
    /* XXX: simplify */
    if (!(v & SYM_FIELD) && (v & ~SYM_STRUCT) < SYM_FIRST_ANOM) {
        /* record symbol in token array */
        ts = s1->table_ident[(v & ~SYM_STRUCT) - TOK_IDENT];
        if (v & SYM_STRUCT)
            ps = &ts->sym_struct;
        else
            ps = &ts->sym_identifier;
        s->prev_tok = *ps;
        *ps = s;
        s->sym_scope = s1->local_scope;
        if (s->prev_tok && sym_scope(s1, s->prev_tok) == s->sym_scope)
            tcc_error(s1, "redeclaration of '%s'",
                get_tok_str(s1, v & ~SYM_STRUCT, NULL));
    }
    return s;
}

/* push a global identifier */
ST_FUNC Sym *global_identifier_push(TCCState *s1, int v, int t, int c)
{
    Sym *s, **ps;
    s = sym_push2(s1, &s1->global_stack, v, t, c);
    s->r = VT_CONST | VT_SYM;
    /* don't record anonymous symbol */
    if (v < SYM_FIRST_ANOM) {
        ps = &s1->table_ident[v - TOK_IDENT]->sym_identifier;
        /* modify the top most local identifier, so that sym_identifier will
           point to 's' when popped; happens when called from inline asm */
        while (*ps != NULL && (*ps)->sym_scope)
            ps = &(*ps)->prev_tok;
        s->prev_tok = *ps;
        *ps = s;
    }
    return s;
}

/* pop symbols until top reaches 'b'.  If KEEP is non-zero don't really
   pop them yet from the list, but do remove them from the token array.  */
ST_FUNC void sym_pop(TCCState *s1, Sym **ptop, Sym *b, int keep)
{
    Sym *s, *ss, **ps;
    TokenSym *ts;
    int v;

    s = *ptop;
    while(s != b) {
        ss = s->prev;
        v = s->v;
        /* remove symbol in token array */
        /* XXX: simplify */
        if (!(v & SYM_FIELD) && (v & ~SYM_STRUCT) < SYM_FIRST_ANOM) {
            ts = s1->table_ident[(v & ~SYM_STRUCT) - TOK_IDENT];
            if (v & SYM_STRUCT)
                ps = &ts->sym_struct;
            else
                ps = &ts->sym_identifier;
            *ps = s->prev_tok;
        }
	if (!keep)
	    sym_free(s1, s);
        s = ss;
    }
    if (!keep)
	*ptop = b;
}

/* ------------------------------------------------------------------------- */
static void vcheck_cmp(TCCState *s1)
{
    /* cannot let cpu flags if other instruction are generated. Also
       avoid leaving VT_JMP anywhere except on the top of the stack
       because it would complicate the code generator.

       Don't do this when nocode_wanted.  s1->vtop might come from
       !nocode_wanted regions (see 88_codeopt.c) and transforming
       it to a register without actually generating code is wrong
       as their value might still be used for real.  All values
       we push under nocode_wanted will eventually be popped
       again, so that the VT_CMP/VT_JMP value will be in s1->vtop
       when code is unsuppressed again. */

    if (s1->vtop->r == VT_CMP && !s1->nocode_wanted)
        gv(s1, RC_INT);
}

static void vsetc(TCCState *s1, CType *type, int r, CValue *vc)
{
    if (s1->vtop >= vstack(s1) + (VSTACK_SIZE - 1))
        tcc_error(s1, "memory full (vstack)");
    vcheck_cmp(s1);
    s1->vtop++;
    s1->vtop->type = *type;
    s1->vtop->r = r;
    s1->vtop->r2 = VT_CONST;
    s1->vtop->c = *vc;
    s1->vtop->sym = NULL;
}

ST_FUNC void vswap(TCCState *s1)
{
    SValue tmp;

    vcheck_cmp(s1);
    tmp = s1->vtop[0];
    s1->vtop[0] = s1->vtop[-1];
    s1->vtop[-1] = tmp;
}

/* pop stack value */
ST_FUNC void vpop(TCCState *s1)
{
    int v;
    v = s1->vtop->r & VT_VALMASK;
#if defined(TCC_TARGET_I386) || defined(TCC_TARGET_X86_64)
    /* for x86, we need to pop the FP stack */
    if (v == TREG_ST0) {
        o(s1, 0xd8dd); /* fstp %st(0) */
    } else
#endif
    if (v == VT_CMP) {
        /* need to put correct jump if && or || without test */
        gsym(s1, s1->vtop->jtrue);
        gsym(s1, s1->vtop->jfalse);
    }
    s1->vtop--;
}

/* push constant of type "type" with useless value */
static void vpush(TCCState *s1, CType *type)
{
    vset(s1, type, VT_CONST, 0);
}

/* push arbitrary 64bit constant */
static void vpush64(TCCState *s1, int ty, unsigned long long v)
{
    CValue cval;
    CType ctype;
    ctype.t = ty;
    ctype.ref = NULL;
    cval.i = v;
    vsetc(s1, &ctype, VT_CONST, &cval);
}

/* push integer constant */
ST_FUNC void vpushi(TCCState *s1, int v)
{
    vpush64(s1, VT_INT, v);
}

/* push a pointer sized constant */
static void vpushs(TCCState *s1, addr_t v)
{
    vpush64(s1, VT_SIZE_T, v);
}

/* push long long constant */
static inline void vpushll(TCCState *s1, long long v)
{
    vpush64(s1, VT_LLONG, v);
}

ST_FUNC void vset(TCCState *s1, CType *type, int r, int v)
{
    CValue cval;
    cval.i = v;
    vsetc(s1, type, r, &cval);
}

static void vseti(TCCState *s1, int r, int v)
{
    CType type;
    type.t = VT_INT;
    type.ref = NULL;
    vset(s1, &type, r, v);
}

ST_FUNC void vpushv(TCCState *s1, SValue *v)
{
    if (s1->vtop >= vstack(s1) + (VSTACK_SIZE - 1))
        tcc_error(s1, "memory full (vstack)");
    s1->vtop++;
    *s1->vtop = *v;
}

static void vdup(TCCState *s1)
{
    vpushv(s1, s1->vtop);
}

/* rotate n first stack elements to the bottom
   I1 ... In -> I2 ... In I1 [top is right]
*/
ST_FUNC void vrotb(TCCState *s1, int n)
{
    int i;
    SValue tmp;

    vcheck_cmp(s1);
    tmp = s1->vtop[-n + 1];
    for(i=-n+1;i!=0;i++)
        s1->vtop[i] = s1->vtop[i+1];
    s1->vtop[0] = tmp;
}

/* rotate the n elements before entry e towards the top
   I1 ... In ... -> In I1 ... I(n-1) ... [top is right]
 */
ST_FUNC void vrote(TCCState *s1, SValue *e, int n)
{
    int i;
    SValue tmp;

    vcheck_cmp(s1);
    tmp = *e;
    for(i = 0;i < n - 1; i++)
        e[-i] = e[-i - 1];
    e[-n + 1] = tmp;
}

/* rotate n first stack elements to the top
   I1 ... In -> In I1 ... I(n-1)  [top is right]
 */
ST_FUNC void vrott(TCCState *s1, int n)
{
    vrote(s1, s1->vtop, n);
}

/* ------------------------------------------------------------------------- */
/* s1->vtop->r = VT_CMP means CPU-flags have been set from comparison or test. */

/* called from generators to set the result from relational ops  */
ST_FUNC void vset_VT_CMP(TCCState *s1, int op)
{
    s1->vtop->r = VT_CMP;
    s1->vtop->cmp_op = op;
    s1->vtop->jfalse = 0;
    s1->vtop->jtrue = 0;
}

/* called once before asking generators to load VT_CMP to a register */
static void vset_VT_JMP(TCCState *s1)
{
    int op = s1->vtop->cmp_op;

    if (s1->vtop->jtrue || s1->vtop->jfalse) {
        /* we need to jump to 'mov $0,%R' or 'mov $1,%R' */
        int inv = op & (op < 2); /* small optimization */
        vseti(s1, VT_JMP+inv, gvtst(s1, inv, 0));
    } else {
        /* otherwise convert flags (rsp. 0/1) to register */
        s1->vtop->c.i = op;
        if (op < 2) /* doesn't seem to happen */
            s1->vtop->r = VT_CONST;
    }
}

/* Set CPU Flags, doesn't yet jump */
static void gvtst_set(TCCState *s1, int inv, int t)
{
    int *p;

    if (s1->vtop->r != VT_CMP) {
        vpushi(s1, 0);
        gen_op(s1, TOK_NE);
        if (s1->vtop->r != VT_CMP) /* must be VT_CONST then */
            vset_VT_CMP(s1, s1->vtop->c.i != 0);
    }

    p = inv ? &s1->vtop->jfalse : &s1->vtop->jtrue;
    *p = gjmp_append(s1, *p, t);
}

/* Generate value test
 *
 * Generate a test for any value (jump, comparison and integers) */
static int gvtst(TCCState *s1, int inv, int t)
{
    int op, x, u;

    gvtst_set(s1, inv, t);
    t = s1->vtop->jtrue, u = s1->vtop->jfalse;
    if (inv)
        x = u, u = t, t = x;
    op = s1->vtop->cmp_op;

    /* jump to the wanted target */
    if (op > 1)
        t = gjmp_cond(s1, op ^ inv, t);
    else if (op != inv)
        t = gjmp(s1, t);
    /* resolve complementary jumps to here */
    gsym(s1, u);

    s1->vtop--;
    return t;
}

/* generate a zero or nozero test */
static void gen_test_zero(TCCState *s1, int op)
{
    if (s1->vtop->r == VT_CMP) {
        int j;
        if (op == TOK_EQ) {
            j = s1->vtop->jfalse;
            s1->vtop->jfalse = s1->vtop->jtrue;
            s1->vtop->jtrue = j;
            s1->vtop->cmp_op ^= 1;
        }
    } else {
        vpushi(s1, 0);
        gen_op(s1, op);
    }
}

/* ------------------------------------------------------------------------- */
/* push a symbol value of TYPE */
ST_FUNC void vpushsym(TCCState *s1, CType *type, Sym *sym)
{
    CValue cval;
    cval.i = 0;
    vsetc(s1, type, VT_CONST | VT_SYM, &cval);
    s1->vtop->sym = sym;
}

/* Return a static symbol pointing to a section */
ST_FUNC Sym *get_sym_ref(TCCState *s1, CType *type, Section *sec, unsigned long offset, unsigned long size)
{
    int v;
    Sym *sym;

    v = s1->anon_sym++;
    sym = sym_push(s1, v, type, VT_CONST | VT_SYM, 0);
    sym->type.t |= VT_STATIC;
    put_extern_sym(s1, sym, sec, offset, size);
    return sym;
}

/* push a reference to a section offset by adding a dummy symbol */
static void vpush_ref(TCCState *s1, CType *type, Section *sec, unsigned long offset, unsigned long size)
{
    vpushsym(s1, type, get_sym_ref(s1, type, sec, offset, size));  
}

/* define a new external reference to a symbol 'v' of type 'u' */
ST_FUNC Sym *external_global_sym(TCCState *s1, int v, CType *type)
{
    Sym *s;

    s = sym_find(s1, v);
    if (!s) {
        /* push forward reference */
        s = global_identifier_push(s1, v, type->t | VT_EXTERN, 0);
        s->type.ref = type->ref;
    } else if (IS_ASM_SYM(s)) {
        s->type.t = type->t | (s->type.t & VT_EXTERN);
        s->type.ref = type->ref;
        update_storage(s1, s);
    }
    return s;
}

/* create an external reference with no specific type similar to asm labels.
   This avoids type conflicts if the symbol is used from C too */
ST_FUNC Sym *external_helper_sym(TCCState *s1, int v)
{
    CType ct = { VT_ASM_FUNC, NULL };
    return external_global_sym(s1, v, &ct);
}

/* push a reference to an helper function (such as memmove) */
ST_FUNC void vpush_helper_func(TCCState *s1, int v)
{
    vpushsym(s1, &s1->func_old_type, external_helper_sym(s1, v));
}

/* Merge symbol attributes.  */
static void merge_symattr(TCCState *s1, struct SymAttr *sa, struct SymAttr *sa1)
{
    if (sa1->aligned && !sa->aligned)
      sa->aligned = sa1->aligned;
    sa->packed |= sa1->packed;
    sa->weak |= sa1->weak;
    if (sa1->visibility != STV_DEFAULT) {
	int vis = sa->visibility;
	if (vis == STV_DEFAULT
	    || vis > sa1->visibility)
	  vis = sa1->visibility;
	sa->visibility = vis;
    }
    sa->dllexport |= sa1->dllexport;
    sa->nodecorate |= sa1->nodecorate;
    sa->dllimport |= sa1->dllimport;
}

/* Merge function attributes.  */
static void merge_funcattr(TCCState *s1, struct FuncAttr *fa, struct FuncAttr *fa1)
{
    if (fa1->func_call && !fa->func_call)
      fa->func_call = fa1->func_call;
    if (fa1->func_type && !fa->func_type)
      fa->func_type = fa1->func_type;
    if (fa1->func_args && !fa->func_args)
      fa->func_args = fa1->func_args;
    if (fa1->func_noreturn)
      fa->func_noreturn = 1;
    if (fa1->func_ctor)
      fa->func_ctor = 1;
    if (fa1->func_dtor)
      fa->func_dtor = 1;
}

/* Merge attributes.  */
static void merge_attr(TCCState *s1, AttributeDef *ad, AttributeDef *ad1)
{
    merge_symattr(s1, &ad->a, &ad1->a);
    merge_funcattr(s1, &ad->f, &ad1->f);

    if (ad1->section)
      ad->section = ad1->section;
    if (ad1->alias_target)
      ad->alias_target = ad1->alias_target;
    if (ad1->asm_label)
      ad->asm_label = ad1->asm_label;
    if (ad1->attr_mode)
      ad->attr_mode = ad1->attr_mode;
}

/* Merge some type attributes.  */
static void patch_type(TCCState *s1, Sym *sym, CType *type)
{
    if (!(type->t & VT_EXTERN) || IS_ENUM_VAL(sym->type.t)) {
        if (!(sym->type.t & VT_EXTERN))
            tcc_error(s1, "redefinition of '%s'", get_tok_str(s1, sym->v, NULL));
        sym->type.t &= ~VT_EXTERN;
    }

    if (IS_ASM_SYM(sym)) {
        /* stay static if both are static */
        sym->type.t = type->t & (sym->type.t | ~VT_STATIC);
        sym->type.ref = type->ref;
    }

    if (!is_compatible_types(&sym->type, type)) {
        tcc_error(s1, "incompatible types for redefinition of '%s'",
                  get_tok_str(s1, sym->v, NULL));

    } else if ((sym->type.t & VT_BTYPE) == VT_FUNC) {
        int static_proto = sym->type.t & VT_STATIC;
        /* warn if static follows non-static function declaration */
        if ((type->t & VT_STATIC) && !static_proto
            /* XXX this test for inline shouldn't be here.  Until we
               implement gnu-inline mode again it silences a warning for
               mingw caused by our workarounds.  */
            && !((type->t | sym->type.t) & VT_INLINE))
            tcc_warning(s1, "static storage ignored for redefinition of '%s'",
                get_tok_str(s1, sym->v, NULL));

        /* set 'inline' if both agree or if one has static */
        if ((type->t | sym->type.t) & VT_INLINE) {
            if (!((type->t ^ sym->type.t) & VT_INLINE)
             || ((type->t | sym->type.t) & VT_STATIC))
                static_proto |= VT_INLINE;
        }

        if (0 == (type->t & VT_EXTERN)) {
            struct FuncAttr f = sym->type.ref->f;
            /* put complete type, use static from prototype */
            sym->type.t = (type->t & ~(VT_STATIC|VT_INLINE)) | static_proto;
            sym->type.ref = type->ref;
            merge_funcattr(s1, &sym->type.ref->f, &f);
        } else {
            sym->type.t &= ~VT_INLINE | static_proto;
        }

        if (sym->type.ref->f.func_type == FUNC_OLD
             && type->ref->f.func_type != FUNC_OLD) {
            sym->type.ref = type->ref;
        }

    } else {
        if ((sym->type.t & VT_ARRAY) && type->ref->c >= 0) {
            /* set array size if it was omitted in extern declaration */
            sym->type.ref->c = type->ref->c;
        }
        if ((type->t ^ sym->type.t) & VT_STATIC)
            tcc_warning(s1, "storage mismatch for redefinition of '%s'",
                get_tok_str(s1, sym->v, NULL));
    }
}

/* Merge some storage attributes.  */
static void patch_storage(TCCState *s1, Sym *sym, AttributeDef *ad, CType *type)
{
    if (type)
        patch_type(s1, sym, type);

#ifdef TCC_TARGET_PE
    if (sym->a.dllimport != ad->a.dllimport)
        tcc_error(s1, "incompatible dll linkage for redefinition of '%s'",
            get_tok_str(s1, sym->v, NULL));
#endif
    merge_symattr(s1, &sym->a, &ad->a);
    if (ad->asm_label)
        sym->asm_label = ad->asm_label;
    update_storage(s1, sym);
}

/* copy sym to other stack */
static Sym *sym_copy(TCCState *s1, Sym *s0, Sym **ps)
{
    Sym *s;
    s = sym_malloc(s1), *s = *s0;
    s->prev = *ps, *ps = s;
    if (s->v < SYM_FIRST_ANOM) {
        ps = &s1->table_ident[s->v - TOK_IDENT]->sym_identifier;
        s->prev_tok = *ps, *ps = s;
    }
    return s;
}

/* copy s->type.ref to stack 'ps' for VT_FUNC and VT_PTR */
static void sym_copy_ref(TCCState *s1, Sym *s, Sym **ps)
{
    int bt = s->type.t & VT_BTYPE;
    if (bt == VT_FUNC || bt == VT_PTR || (bt == VT_STRUCT && s->sym_scope)) {
        Sym **sp = &s->type.ref;
        for (s = *sp, *sp = NULL; s; s = s->next) {
            Sym *s2 = sym_copy(s1, s, ps);
            sp = &(*sp = s2)->next;
            sym_copy_ref(s1, s2, ps);
        }
    }
}

/* define a new external reference to a symbol 'v' */
static Sym *external_sym(TCCState *s1, int v, CType *type, int r, AttributeDef *ad)
{
    Sym *s;

    /* look for global symbol */
    s = sym_find(s1, v);
    while (s && s->sym_scope)
        s = s->prev_tok;

    if (!s) {
        /* push forward reference */
        s = global_identifier_push(s1, v, type->t, 0);
        s->r |= r;
        s->a = ad->a;
        s->asm_label = ad->asm_label;
        s->type.ref = type->ref;
        /* copy type to the global stack */
        if (s1->local_stack)
            sym_copy_ref(s1, s, &s1->global_stack);
    } else {
        patch_storage(s1, s, ad, type);
    }
    /* push variables on local_stack if any */
    if (s1->local_stack && (s->type.t & VT_BTYPE) != VT_FUNC)
        s = sym_copy(s1, s, &s1->local_stack);
    return s;
}

/* save registers up to (s1->vtop - n) stack entry */
ST_FUNC void save_regs(TCCState *s1, int n)
{
    SValue *p, *p1;
    for(p = vstack(s1), p1 = s1->vtop - n; p <= p1; p++)
        save_reg(s1, p->r);
}

/* save r to the memory stack, and mark it as being free */
ST_FUNC void save_reg(TCCState *s1, int r)
{
    save_reg_upstack(s1, r, 0);
}

/* save r to the memory stack, and mark it as being free,
   if seen up to (s1->vtop - n) stack entry */
ST_FUNC void save_reg_upstack(TCCState *s1, int r, int n)
{
    int l, size, align, bt;
    SValue *p, *p1, sv;

    if ((r &= VT_VALMASK) >= VT_CONST)
        return;
    if (s1->nocode_wanted)
        return;
    l = 0;
    for(p = vstack(s1), p1 = s1->vtop - n; p <= p1; p++) {
        if ((p->r & VT_VALMASK) == r || p->r2 == r) {
            /* must save value on stack if not already done */
            if (!l) {
                bt = p->type.t & VT_BTYPE;
                if (bt == VT_VOID)
                    continue;
                if ((p->r & VT_LVAL) || bt == VT_FUNC)
                    bt = VT_PTR;
                sv.type.t = bt;
                size = type_size(s1, &sv.type, &align);
                l = get_temp_local_var(s1, size,align);
                sv.r = VT_LOCAL | VT_LVAL;
                sv.c.i = l;
                store(s1, p->r & VT_VALMASK, &sv);
#if defined(TCC_TARGET_I386) || defined(TCC_TARGET_X86_64)
                /* x86 specific: need to pop fp register ST0 if saved */
                if (r == TREG_ST0) {
                    o(s1, 0xd8dd); /* fstp %st(0) */
                }
#endif
                /* special long long case */
                if (p->r2 < VT_CONST && USING_TWO_WORDS(bt)) {
                    sv.c.i += PTR_SIZE;
                    store(s1, p->r2, &sv);
                }
            }
            /* mark that stack entry as being saved on the stack */
            if (p->r & VT_LVAL) {
                /* also clear the bounded flag because the
                   relocation address of the function was stored in
                   p->c.i */
                p->r = (p->r & ~(VT_VALMASK | VT_BOUNDED)) | VT_LLOCAL;
            } else {
                p->r = VT_LVAL | VT_LOCAL;
            }
            p->sym = NULL;
            p->r2 = VT_CONST;
            p->c.i = l;
        }
    }
}

#ifdef TCC_TARGET_ARM
/* find a register of class 'rc2' with at most one reference on stack.
 * If none, call get_reg(rc) */
ST_FUNC int get_reg_ex(TCCState *s1, int rc, int rc2)
{
    int r;
    SValue *p;
    
    for(r=0;r<NB_REGS;r++) {
        if (reg_classes[r] & rc2) {
            int n;
            n=0;
            for(p = vstack(s1); p <= s1->vtop; p++) {
                if ((p->r & VT_VALMASK) == r ||
                    p->r2 == r)
                    n++;
            }
            if (n <= 1)
                return r;
        }
    }
    return get_reg(s1, rc);
}
#endif

/* find a free register of class 'rc'. If none, save one register */
ST_FUNC int get_reg(TCCState *s1, int rc)
{
    int r;
    SValue *p;

    /* find a free register */
    for(r=0;r<NB_REGS;r++) {
        if (reg_classes[r] & rc) {
            if (s1->nocode_wanted)
                return r;
            for(p=vstack(s1);p<=s1->vtop;p++) {
                if ((p->r & VT_VALMASK) == r ||
                    p->r2 == r)
                    goto notfound;
            }
            return r;
        }
    notfound: ;
    }
    
    /* no register left : free the first one on the stack (VERY
       IMPORTANT to start from the bottom to ensure that we don't
       spill registers used in gen_opi()) */
    for(p=vstack(s1);p<=s1->vtop;p++) {
        /* look at second register (if long long) */
        r = p->r2;
        if (r < VT_CONST && (reg_classes[r] & rc))
            goto save_found;
        r = p->r & VT_VALMASK;
        if (r < VT_CONST && (reg_classes[r] & rc)) {
        save_found:
            save_reg(s1, r);
            return r;
        }
    }
    /* Should never comes here */
    return -1;
}

/* find a free temporary local variable (return the offset on stack) match the size and align. If none, add new temporary stack variable*/
static int get_temp_local_var(TCCState *s1, int size,int align){
	int i;
	struct temp_local_variable *temp_var;
	int found_var;
	SValue *p;
	int r;
	char free;
	char found;
	found=0;
	for(i=0;i<s1->nb_temp_local_vars;i++){
		temp_var=&s1->arr_temp_local_vars[i];
		if(temp_var->size<size||align!=temp_var->align){
			continue;
		}
		/*check if temp_var is free*/
		free=1;
		for(p=vstack(s1);p<=s1->vtop;p++) {
			r=p->r&VT_VALMASK;
			if(r==VT_LOCAL||r==VT_LLOCAL){
				if(p->c.i==temp_var->location){
					free=0;
					break;
				}
			}
		}
		if(free){
			found_var=temp_var->location;
			found=1;
			break;
		}
	}
	if(!found){
		s1->loc = (s1->loc - size) & -align;
		if(s1->nb_temp_local_vars<MAX_TEMP_LOCAL_VARIABLE_NUMBER){
			temp_var=&s1->arr_temp_local_vars[i];
			temp_var->location=s1->loc;
			temp_var->size=size;
			temp_var->align=align;
			s1->nb_temp_local_vars++;
		}
		found_var=s1->loc;
	}
	return found_var;
}

static void clear_temp_local_var_list(TCCState *s1){
	s1->nb_temp_local_vars=0;
}

/* move register 's' (of type 't') to 'r', and flush previous value of r to memory
   if needed */
static void move_reg(TCCState *s1, int r, int s, int t)
{
    SValue sv;

    if (r != s) {
        save_reg(s1, r);
        sv.type.t = t;
        sv.type.ref = NULL;
        sv.r = s;
        sv.c.i = 0;
        load(s1, r, &sv);
    }
}

/* get address of s1->vtop (s1->vtop MUST BE an lvalue) */
ST_FUNC void gaddrof(TCCState *s1)
{
    s1->vtop->r &= ~VT_LVAL;
    /* tricky: if saved lvalue, then we can go back to lvalue */
    if ((s1->vtop->r & VT_VALMASK) == VT_LLOCAL)
        s1->vtop->r = (s1->vtop->r & ~VT_VALMASK) | VT_LOCAL | VT_LVAL;
}

#ifdef CONFIG_TCC_BCHECK
/* generate a bounded pointer addition */
static void gen_bounded_ptr_add(TCCState *s1)
{
    int save = (s1->vtop[-1].r & VT_VALMASK) == VT_LOCAL;
    if (save) {
      vpushv(s1, &s1->vtop[-1]);
      vrott(s1, 3);
    }
    vpush_helper_func(s1, TOK___bound_ptr_add);
    vrott(s1, 3);
    gfunc_call(s1, 2);
    s1->vtop -= save;
    vpushi(s1, 0);
    /* returned pointer is in REG_IRET */
    s1->vtop->r = REG_IRET | VT_BOUNDED;
    if (s1->nocode_wanted)
        return;
    /* relocation offset of the bounding function call point */
    s1->vtop->c.i = (cur_text_section->reloc->data_offset - sizeof(ElfW_Rel));
}

/* patch pointer addition in s1->vtop so that pointer dereferencing is
   also tested */
static void gen_bounded_ptr_deref(TCCState *s1)
{
    addr_t func;
    int size, align;
    ElfW_Rel *rel;
    Sym *sym;

    if (s1->nocode_wanted)
        return;

    size = type_size(s1, &s1->vtop->type, &align);
    switch(size) {
    case  1: func = TOK___bound_ptr_indir1; break;
    case  2: func = TOK___bound_ptr_indir2; break;
    case  4: func = TOK___bound_ptr_indir4; break;
    case  8: func = TOK___bound_ptr_indir8; break;
    case 12: func = TOK___bound_ptr_indir12; break;
    case 16: func = TOK___bound_ptr_indir16; break;
    default:
        /* may happen with struct member access */
        return;
    }
    sym = external_helper_sym(s1, func);
    if (!sym->c)
        put_extern_sym(s1, sym, NULL, 0, 0);
    /* patch relocation */
    /* XXX: find a better solution ? */
    rel = (ElfW_Rel *)(cur_text_section->reloc->data + s1->vtop->c.i);
    rel->r_info = ELFW(R_INFO)(sym->c, ELFW(R_TYPE)(rel->r_info));
}

/* generate lvalue bound code */
static void gbound(TCCState *s1)
{
    CType type1;

    s1->vtop->r &= ~VT_MUSTBOUND;
    /* if lvalue, then use checking code before dereferencing */
    if (s1->vtop->r & VT_LVAL) {
        /* if not VT_BOUNDED value, then make one */
        if (!(s1->vtop->r & VT_BOUNDED)) {
            /* must save type because we must set it to int to get pointer */
            type1 = s1->vtop->type;
            s1->vtop->type.t = VT_PTR;
            gaddrof(s1);
            vpushi(s1, 0);
            gen_bounded_ptr_add(s1);
            s1->vtop->r |= VT_LVAL;
            s1->vtop->type = type1;
        }
        /* then check for dereferencing */
        gen_bounded_ptr_deref(s1);
    }
}

/* we need to call __bound_ptr_add before we start to load function
   args into registers */
ST_FUNC void gbound_args(TCCState *s1, int nb_args)
{
    int i, v;
    SValue *sv;

    for (i = 1; i <= nb_args; ++i)
        if (s1->vtop[1 - i].r & VT_MUSTBOUND) {
            vrotb(s1, i);
            gbound(s1);
            vrott(s1, i);
        }

    sv = s1->vtop - nb_args;
    if (sv->r & VT_SYM) {
        v = sv->sym->v;
        if (v == TOK_setjmp
          || v == TOK__setjmp
#ifndef TCC_TARGET_PE
          || v == TOK_sigsetjmp
          || v == TOK___sigsetjmp
#endif
          ) {
            vpush_helper_func(s1, TOK___bound_setjmp);
            vpushv(s1, sv + 1);
            gfunc_call(s1, 1);
            s1->func_bound_add_epilog = 1;
        }
#if defined TCC_TARGET_I386 || defined TCC_TARGET_X86_64
        if (v == TOK_alloca)
            s1->func_bound_add_epilog = 1;
#endif
#if TARGETOS_NetBSD
        if (v == TOK_longjmp) /* undo rename to __longjmp14 */
            sv->sym->asm_label = TOK___bound_longjmp;
#endif
    }
}

/* Add bounds for local symbols from S to E (via ->prev) */
static void add_local_bounds(TCCState *s1, Sym *s, Sym *e)
{
    for (; s != e; s = s->prev) {
        if (!s->v || (s->r & VT_VALMASK) != VT_LOCAL)
          continue;
        /* Add arrays/structs/unions because we always take address */
        if ((s->type.t & VT_ARRAY)
            || (s->type.t & VT_BTYPE) == VT_STRUCT
            || s->a.addrtaken) {
            /* add local bound info */
            int align, size = type_size(s1, &s->type, &align);
            addr_t *bounds_ptr = section_ptr_add(lbounds_section,
                                                 2 * sizeof(addr_t));
            bounds_ptr[0] = s->c;
            bounds_ptr[1] = size;
        }
    }
}
#endif

/* Wrapper around sym_pop, that potentially also registers local bounds.  */
static void pop_local_syms(TCCState *s1, Sym *b, int keep)
{
#ifdef CONFIG_TCC_BCHECK
    if (s1->do_bounds_check && !keep && (s1->local_scope || !s1->func_var))
        add_local_bounds(s1, s1->local_stack, b);
#endif
    if (s1->debug_modes)
        tcc_add_debug_info (s1, !s1->local_scope, s1->local_stack, b);
    sym_pop(s1, &s1->local_stack, b, keep);
}

static void incr_bf_adr(TCCState *s1, int o)
{
    s1->vtop->type = s1->char_pointer_type;
    gaddrof(s1);
    vpushs(s1, o);
    gen_op(s1, '+');
    s1->vtop->type.t = VT_BYTE | VT_UNSIGNED;
    s1->vtop->r |= VT_LVAL;
}

/* single-byte load mode for packed or otherwise unaligned bitfields */
static void load_packed_bf(TCCState *s1, CType *type, int bit_pos, int bit_size)
{
    int n, o, bits;
    save_reg_upstack(s1, s1->vtop->r, 1);
    vpush64(s1, type->t & VT_BTYPE, 0); // B X
    bits = 0, o = bit_pos >> 3, bit_pos &= 7;
    do {
        vswap(s1); // X B
        incr_bf_adr(s1, o);
        vdup(s1); // X B B
        n = 8 - bit_pos;
        if (n > bit_size)
            n = bit_size;
        if (bit_pos)
            vpushi(s1, bit_pos), gen_op(s1, TOK_SHR), bit_pos = 0; // X B Y
        if (n < 8)
            vpushi(s1, (1 << n) - 1), gen_op(s1, '&');
        gen_cast(s1, type);
        if (bits)
            vpushi(s1, bits), gen_op(s1, TOK_SHL);
        vrotb(s1, 3); // B Y X
        gen_op(s1, '|'); // B X
        bits += n, bit_size -= n, o = 1;
    } while (bit_size);
    vswap(s1), vpop(s1);
    if (!(type->t & VT_UNSIGNED)) {
        n = ((type->t & VT_BTYPE) == VT_LLONG ? 64 : 32) - bits;
        vpushi(s1, n), gen_op(s1, TOK_SHL);
        vpushi(s1, n), gen_op(s1, TOK_SAR);
    }
}

/* single-byte store mode for packed or otherwise unaligned bitfields */
static void store_packed_bf(TCCState *s1, int bit_pos, int bit_size)
{
    int bits, n, o, m, c;
    c = (s1->vtop->r & (VT_VALMASK | VT_LVAL | VT_SYM)) == VT_CONST;
    vswap(s1); // X B
    save_reg_upstack(s1, s1->vtop->r, 1);
    bits = 0, o = bit_pos >> 3, bit_pos &= 7;
    do {
        incr_bf_adr(s1, o); // X B
        vswap(s1); //B X
        c ? vdup(s1) : gv_dup(s1); // B V X
        vrott(s1, 3); // X B V
        if (bits)
            vpushi(s1, bits), gen_op(s1, TOK_SHR);
        if (bit_pos)
            vpushi(s1, bit_pos), gen_op(s1, TOK_SHL);
        n = 8 - bit_pos;
        if (n > bit_size)
            n = bit_size;
        if (n < 8) {
            m = ((1 << n) - 1) << bit_pos;
            vpushi(s1, m), gen_op(s1, '&'); // X B V1
            vpushv(s1, s1->vtop-1); // X B V1 B
            vpushi(s1, m & 0x80 ? ~m & 0x7f : ~m);
            gen_op(s1, '&'); // X B V1 B1
            gen_op(s1, '|'); // X B V2
        }
        vdup(s1), s1->vtop[-1] = s1->vtop[-2]; // X B B V2
        vstore(s1), vpop(s1); // X B
        bits += n, bit_size -= n, bit_pos = 0, o = 1;
    } while (bit_size);
    vpop(s1), vpop(s1);
}

static int adjust_bf(SValue *sv, int bit_pos, int bit_size)
{
    int t;
    if (0 == sv->type.ref)
        return 0;
    t = sv->type.ref->auxtype;
    if (t != -1 && t != VT_STRUCT) {
        sv->type.t = (sv->type.t & ~(VT_BTYPE | VT_LONG)) | t;
        sv->r |= VT_LVAL;
    }
    return t;
}

/* store s1->vtop a register belonging to class 'rc'. lvalues are
   converted to values. Cannot be used if cannot be converted to
   register value (such as structures). */
ST_FUNC int gv(TCCState *s1, int rc)
{
    int r, r2, r_ok, r2_ok, rc2, bt;
    int bit_pos, bit_size, size, align;

    /* NOTE: get_reg can modify vstack[] */
    if (s1->vtop->type.t & VT_BITFIELD) {
        CType type;

        bit_pos = BIT_POS(s1->vtop->type.t);
        bit_size = BIT_SIZE(s1->vtop->type.t);
        /* remove bit field info to avoid loops */
        s1->vtop->type.t &= ~VT_STRUCT_MASK;

        type.ref = NULL;
        type.t = s1->vtop->type.t & VT_UNSIGNED;
        if ((s1->vtop->type.t & VT_BTYPE) == VT_BOOL)
            type.t |= VT_UNSIGNED;

        r = adjust_bf(s1->vtop, bit_pos, bit_size);

        if ((s1->vtop->type.t & VT_BTYPE) == VT_LLONG)
            type.t |= VT_LLONG;
        else
            type.t |= VT_INT;

        if (r == VT_STRUCT) {
            load_packed_bf(s1, &type, bit_pos, bit_size);
        } else {
            int bits = (type.t & VT_BTYPE) == VT_LLONG ? 64 : 32;
            /* cast to int to propagate signedness in following ops */
            gen_cast(s1, &type);
            /* generate shifts */
            vpushi(s1, bits - (bit_pos + bit_size));
            gen_op(s1, TOK_SHL);
            vpushi(s1, bits - bit_size);
            /* NOTE: transformed to SHR if unsigned */
            gen_op(s1, TOK_SAR);
        }
        r = gv(s1, rc);
    } else {
        if (is_float(s1->vtop->type.t) && 
            (s1->vtop->r & (VT_VALMASK | VT_LVAL)) == VT_CONST) {
            /* CPUs usually cannot use float constants, so we store them
               generically in data segment */
            init_params p = { rodata_section };
            unsigned long offset;
            size = type_size(s1, &s1->vtop->type, &align);
            if (NODATA_WANTED)
                size = 0, align = 1;
            offset = section_add(p.sec, size, align);
            vpush_ref(s1, &s1->vtop->type, p.sec, offset, size);
	    vswap(s1);
	    init_putv(s1, &p, &s1->vtop->type, offset);
	    s1->vtop->r |= VT_LVAL;
        }
#ifdef CONFIG_TCC_BCHECK
        if (s1->vtop->r & VT_MUSTBOUND) 
            gbound(s1);
#endif

        bt = s1->vtop->type.t & VT_BTYPE;

#ifdef TCC_TARGET_RISCV64
        /* XXX mega hack */
        if (bt == VT_LDOUBLE && rc == RC_FLOAT)
          rc = RC_INT;
#endif
        rc2 = RC2_TYPE(bt, rc);

        /* need to reload if:
           - constant
           - lvalue (need to dereference pointer)
           - already a register, but not in the right class */
        r = s1->vtop->r & VT_VALMASK;
        r_ok = !(s1->vtop->r & VT_LVAL) && (r < VT_CONST) && (reg_classes[r] & rc);
        r2_ok = !rc2 || ((s1->vtop->r2 < VT_CONST) && (reg_classes[s1->vtop->r2] & rc2));

        if (!r_ok || !r2_ok) {
            if (!r_ok)
                r = get_reg(s1, rc);
            if (rc2) {
                int load_type = (bt == VT_QFLOAT) ? VT_DOUBLE : VT_PTRDIFF_T;
                int original_type = s1->vtop->type.t;

                /* two register type load :
                   expand to two words temporarily */
                if ((s1->vtop->r & (VT_VALMASK | VT_LVAL)) == VT_CONST) {
                    /* load constant */
                    unsigned long long ll = s1->vtop->c.i;
                    s1->vtop->c.i = ll; /* first word */
                    load(s1, r, s1->vtop);
                    s1->vtop->r = r; /* save register value */
                    vpushi(s1, ll >> 32); /* second word */
                } else if (s1->vtop->r & VT_LVAL) {
                    /* We do not want to modifier the long long pointer here.
                       So we save any other instances down the stack */
                    save_reg_upstack(s1, s1->vtop->r, 1);
                    /* load from memory */
                    s1->vtop->type.t = load_type;
                    load(s1, r, s1->vtop);
                    vdup(s1);
                    s1->vtop[-1].r = r; /* save register value */
                    /* increment pointer to get second word */
                    s1->vtop->type.t = VT_PTRDIFF_T;
                    gaddrof(s1);
                    vpushs(s1, PTR_SIZE);
                    gen_op(s1, '+');
                    s1->vtop->r |= VT_LVAL;
                    s1->vtop->type.t = load_type;
                } else {
                    /* move registers */
                    if (!r_ok)
                        load(s1, r, s1->vtop);
                    if (r2_ok && s1->vtop->r2 < VT_CONST)
                        goto done;
                    vdup(s1);
                    s1->vtop[-1].r = r; /* save register value */
                    s1->vtop->r = s1->vtop[-1].r2;
                }
                /* Allocate second register. Here we rely on the fact that
                   get_reg() tries first to free r2 of an SValue. */
                r2 = get_reg(s1, rc2);
                load(s1, r2, s1->vtop);
                vpop(s1);
                /* write second register */
                s1->vtop->r2 = r2;
            done:
                s1->vtop->type.t = original_type;
            } else {
                if (s1->vtop->r == VT_CMP)
                    vset_VT_JMP(s1);
                /* one register type load */
                load(s1, r, s1->vtop);
            }
        }
        s1->vtop->r = r;
#ifdef TCC_TARGET_C67
        /* uses register pairs for doubles */
        if (bt == VT_DOUBLE)
            s1->vtop->r2 = r+1;
#endif
    }
    return r;
}

/* generate s1->vtop[-1] and s1->vtop[0] in resp. classes rc1 and rc2 */
ST_FUNC void gv2(TCCState *s1, int rc1, int rc2)
{
    /* generate more generic register first. But VT_JMP or VT_CMP
       values must be generated first in all cases to avoid possible
       reload errors */
    if (s1->vtop->r != VT_CMP && rc1 <= rc2) {
        vswap(s1);
        gv(s1, rc1);
        vswap(s1);
        gv(s1, rc2);
        /* test if reload is needed for first register */
        if ((s1->vtop[-1].r & VT_VALMASK) >= VT_CONST) {
            vswap(s1);
            gv(s1, rc1);
            vswap(s1);
        }
    } else {
        gv(s1, rc2);
        vswap(s1);
        gv(s1, rc1);
        vswap(s1);
        /* test if reload is needed for first register */
        if ((s1->vtop[0].r & VT_VALMASK) >= VT_CONST) {
            gv(s1, rc2);
        }
    }
}

#if PTR_SIZE == 4
/* expand 64bit on stack in two ints */
ST_FUNC void lexpand(TCCState *s1)
{
    int u, v;
    u = s1->vtop->type.t & (VT_DEFSIGN | VT_UNSIGNED);
    v = s1->vtop->r & (VT_VALMASK | VT_LVAL);
    if (v == VT_CONST) {
        vdup(s1);
        s1->vtop[0].c.i >>= 32;
    } else if (v == (VT_LVAL|VT_CONST) || v == (VT_LVAL|VT_LOCAL)) {
        vdup(s1);
        s1->vtop[0].c.i += 4;
    } else {
        gv(s1, RC_INT);
        vdup(s1);
        s1->vtop[0].r = s1->vtop[-1].r2;
        s1->vtop[0].r2 = s1->vtop[-1].r2 = VT_CONST;
    }
    s1->vtop[0].type.t = s1->vtop[-1].type.t = VT_INT | u;
}
#endif

#if PTR_SIZE == 4
/* build a long long from two ints */
static void lbuild(TCCState *s1, int t)
{
    gv2(s1, RC_INT, RC_INT);
    s1->vtop[-1].r2 = s1->vtop[0].r;
    s1->vtop[-1].type.t = t;
    vpop(s1);
}
#endif

/* convert stack entry to register and duplicate its value in another
   register */
static void gv_dup(TCCState *s1)
{
    int t, rc, r;

    t = s1->vtop->type.t;
#if PTR_SIZE == 4
    if ((t & VT_BTYPE) == VT_LLONG) {
        if (t & VT_BITFIELD) {
            gv(s1, RC_INT);
            t = s1->vtop->type.t;
        }
        lexpand(s1);
        gv_dup(s1);
        vswap(s1);
        vrotb(s1, 3);
        gv_dup(s1);
        vrotb(s1, 4);
        /* stack: H L L1 H1 */
        lbuild(s1, t);
        vrotb(s1, 3);
        vrotb(s1, 3);
        vswap(s1);
        lbuild(s1, t);
        vswap(s1);
        return;
    }
#endif
    /* duplicate value */
    rc = RC_TYPE(t);
    gv(s1, rc);
    r = get_reg(s1, rc);
    vdup(s1);
    load(s1, r, s1->vtop);
    s1->vtop->r = r;
}

#if PTR_SIZE == 4
/* generate CPU independent (unsigned) long long operations */
static void gen_opl(TCCState *s1, int op)
{
    int t, a, b, op1, c, i;
    int func;
    unsigned short reg_iret = REG_IRET;
    unsigned short reg_lret = REG_IRE2;
    SValue tmp;

    switch(op) {
    case '/':
    case TOK_PDIV:
        func = TOK___divdi3;
        goto gen_func;
    case TOK_UDIV:
        func = TOK___udivdi3;
        goto gen_func;
    case '%':
        func = TOK___moddi3;
        goto gen_mod_func;
    case TOK_UMOD:
        func = TOK___umoddi3;
    gen_mod_func:
#ifdef TCC_ARM_EABI
        reg_iret = TREG_R2;
        reg_lret = TREG_R3;
#endif
    gen_func:
        /* call generic long long function */
        vpush_helper_func(s1, func);
        vrott(s1, 3);
        gfunc_call(s1, 2);
        vpushi(s1, 0);
        s1->vtop->r = reg_iret;
        s1->vtop->r2 = reg_lret;
        break;
    case '^':
    case '&':
    case '|':
    case '*':
    case '+':
    case '-':
        //pv("gen_opl A",0,2);
        t = s1->vtop->type.t;
        vswap(s1);
        lexpand(s1);
        vrotb(s1, 3);
        lexpand(s1);
        /* stack: L1 H1 L2 H2 */
        tmp = s1->vtop[0];
        s1->vtop[0] = s1->vtop[-3];
        s1->vtop[-3] = tmp;
        tmp = s1->vtop[-2];
        s1->vtop[-2] = s1->vtop[-3];
        s1->vtop[-3] = tmp;
        vswap(s1);
        /* stack: H1 H2 L1 L2 */
        //pv("gen_opl B",0,4);
        if (op == '*') {
            vpushv(s1, s1->vtop - 1);
            vpushv(s1, s1->vtop - 1);
            gen_op(s1, TOK_UMULL);
            lexpand(s1);
            /* stack: H1 H2 L1 L2 ML MH */
            for(i=0;i<4;i++)
                vrotb(s1, 6);
            /* stack: ML MH H1 H2 L1 L2 */
            tmp = s1->vtop[0];
            s1->vtop[0] = s1->vtop[-2];
            s1->vtop[-2] = tmp;
            /* stack: ML MH H1 L2 H2 L1 */
            gen_op(s1, '*');
            vrotb(s1, 3);
            vrotb(s1, 3);
            gen_op(s1, '*');
            /* stack: ML MH M1 M2 */
            gen_op(s1, '+');
            gen_op(s1, '+');
        } else if (op == '+' || op == '-') {
            /* XXX: add non carry method too (for MIPS or alpha) */
            if (op == '+')
                op1 = TOK_ADDC1;
            else
                op1 = TOK_SUBC1;
            gen_op(s1, op1);
            /* stack: H1 H2 (L1 op L2) */
            vrotb(s1, 3);
            vrotb(s1, 3);
            gen_op(s1, op1 + 1); /* TOK_xxxC2 */
        } else {
            gen_op(s1, op);
            /* stack: H1 H2 (L1 op L2) */
            vrotb(s1, 3);
            vrotb(s1, 3);
            /* stack: (L1 op L2) H1 H2 */
            gen_op(s1, op);
            /* stack: (L1 op L2) (H1 op H2) */
        }
        /* stack: L H */
        lbuild(s1, t);
        break;
    case TOK_SAR:
    case TOK_SHR:
    case TOK_SHL:
        if ((s1->vtop->r & (VT_VALMASK | VT_LVAL | VT_SYM)) == VT_CONST) {
            t = s1->vtop[-1].type.t;
            vswap(s1);
            lexpand(s1);
            vrotb(s1, 3);
            /* stack: L H shift */
            c = (int)s1->vtop->c.i;
            /* constant: simpler */
            /* NOTE: all comments are for SHL. the other cases are
               done by swapping words */
            vpop(s1);
            if (op != TOK_SHL)
                vswap(s1);
            if (c >= 32) {
                /* stack: L H */
                vpop(s1);
                if (c > 32) {
                    vpushi(s1, c - 32);
                    gen_op(s1, op);
                }
                if (op != TOK_SAR) {
                    vpushi(s1, 0);
                } else {
                    gv_dup(s1);
                    vpushi(s1, 31);
                    gen_op(s1, TOK_SAR);
                }
                vswap(s1);
            } else {
                vswap(s1);
                gv_dup(s1);
                /* stack: H L L */
                vpushi(s1, c);
                gen_op(s1, op);
                vswap(s1);
                vpushi(s1, 32 - c);
                if (op == TOK_SHL)
                    gen_op(s1, TOK_SHR);
                else
                    gen_op(s1, TOK_SHL);
                vrotb(s1, 3);
                /* stack: L L H */
                vpushi(s1, c);
                if (op == TOK_SHL)
                    gen_op(s1, TOK_SHL);
                else
                    gen_op(s1, TOK_SHR);
                gen_op(s1, '|');
            }
            if (op != TOK_SHL)
                vswap(s1);
            lbuild(s1, t);
        } else {
            /* XXX: should provide a faster fallback on x86 ? */
            switch(op) {
            case TOK_SAR:
                func = TOK___ashrdi3;
                goto gen_func;
            case TOK_SHR:
                func = TOK___lshrdi3;
                goto gen_func;
            case TOK_SHL:
                func = TOK___ashldi3;
                goto gen_func;
            }
        }
        break;
    default:
        /* compare operations */
        t = s1->vtop->type.t;
        vswap(s1);
        lexpand(s1);
        vrotb(s1, 3);
        lexpand(s1);
        /* stack: L1 H1 L2 H2 */
        tmp = s1->vtop[-1];
        s1->vtop[-1] = s1->vtop[-2];
        s1->vtop[-2] = tmp;
        /* stack: L1 L2 H1 H2 */
        save_regs(s1, 4);
        /* compare high */
        op1 = op;
        /* when values are equal, we need to compare low words. since
           the jump is inverted, we invert the test too. */
        if (op1 == TOK_LT)
            op1 = TOK_LE;
        else if (op1 == TOK_GT)
            op1 = TOK_GE;
        else if (op1 == TOK_ULT)
            op1 = TOK_ULE;
        else if (op1 == TOK_UGT)
            op1 = TOK_UGE;
        a = 0;
        b = 0;
        gen_op(s1, op1);
        if (op == TOK_NE) {
            b = gvtst(s1, 0, 0);
        } else {
            a = gvtst(s1, 1, 0);
            if (op != TOK_EQ) {
                /* generate non equal test */
                vpushi(s1, 0);
                vset_VT_CMP(s1, TOK_NE);
                b = gvtst(s1, 0, 0);
            }
        }
        /* compare low. Always unsigned */
        op1 = op;
        if (op1 == TOK_LT)
            op1 = TOK_ULT;
        else if (op1 == TOK_LE)
            op1 = TOK_ULE;
        else if (op1 == TOK_GT)
            op1 = TOK_UGT;
        else if (op1 == TOK_GE)
            op1 = TOK_UGE;
        gen_op(s1, op1);
#if 0//def TCC_TARGET_I386
        if (op == TOK_NE) { gsym(s1, b); break; }
        if (op == TOK_EQ) { gsym(s1, a); break; }
#endif
        gvtst_set(s1, 1, a);
        gvtst_set(s1, 0, b);
        break;
    }
}
#endif

static uint64_t gen_opic_sdiv(uint64_t a, uint64_t b)
{
    uint64_t x = (a >> 63 ? -a : a) / (b >> 63 ? -b : b);
    return (a ^ b) >> 63 ? -x : x;
}

static int gen_opic_lt(uint64_t a, uint64_t b)
{
    return (a ^ (uint64_t)1 << 63) < (b ^ (uint64_t)1 << 63);
}

/* handle integer constant optimizations and various machine
   independent opt */
static void gen_opic(TCCState *s1, int op)
{
    SValue *v1 = s1->vtop - 1;
    SValue *v2 = s1->vtop;
    int t1 = v1->type.t & VT_BTYPE;
    int t2 = v2->type.t & VT_BTYPE;
    int c1 = (v1->r & (VT_VALMASK | VT_LVAL | VT_SYM)) == VT_CONST;
    int c2 = (v2->r & (VT_VALMASK | VT_LVAL | VT_SYM)) == VT_CONST;
    uint64_t l1 = c1 ? v1->c.i : 0;
    uint64_t l2 = c2 ? v2->c.i : 0;
    int shm = (t1 == VT_LLONG) ? 63 : 31;

    if (t1 != VT_LLONG && (PTR_SIZE != 8 || t1 != VT_PTR))
        l1 = ((uint32_t)l1 |
              (v1->type.t & VT_UNSIGNED ? 0 : -(l1 & 0x80000000)));
    if (t2 != VT_LLONG && (PTR_SIZE != 8 || t2 != VT_PTR))
        l2 = ((uint32_t)l2 |
              (v2->type.t & VT_UNSIGNED ? 0 : -(l2 & 0x80000000)));

    if (c1 && c2) {
        switch(op) {
        case '+': l1 += l2; break;
        case '-': l1 -= l2; break;
        case '&': l1 &= l2; break;
        case '^': l1 ^= l2; break;
        case '|': l1 |= l2; break;
        case '*': l1 *= l2; break;

        case TOK_PDIV:
        case '/':
        case '%':
        case TOK_UDIV:
        case TOK_UMOD:
            /* if division by zero, generate explicit division */
            if (l2 == 0) {
                if (s1->const_wanted && !(s1->nocode_wanted & unevalmask))
                    tcc_error(s1, "division by zero in constant");
                goto general_case;
            }
            switch(op) {
            default: l1 = gen_opic_sdiv(l1, l2); break;
            case '%': l1 = l1 - l2 * gen_opic_sdiv(l1, l2); break;
            case TOK_UDIV: l1 = l1 / l2; break;
            case TOK_UMOD: l1 = l1 % l2; break;
            }
            break;
        case TOK_SHL: l1 <<= (l2 & shm); break;
        case TOK_SHR: l1 >>= (l2 & shm); break;
        case TOK_SAR:
            l1 = (l1 >> 63) ? ~(~l1 >> (l2 & shm)) : l1 >> (l2 & shm);
            break;
            /* tests */
        case TOK_ULT: l1 = l1 < l2; break;
        case TOK_UGE: l1 = l1 >= l2; break;
        case TOK_EQ: l1 = l1 == l2; break;
        case TOK_NE: l1 = l1 != l2; break;
        case TOK_ULE: l1 = l1 <= l2; break;
        case TOK_UGT: l1 = l1 > l2; break;
        case TOK_LT: l1 = gen_opic_lt(l1, l2); break;
        case TOK_GE: l1 = !gen_opic_lt(l1, l2); break;
        case TOK_LE: l1 = !gen_opic_lt(l2, l1); break;
        case TOK_GT: l1 = gen_opic_lt(l2, l1); break;
            /* logical */
        case TOK_LAND: l1 = l1 && l2; break;
        case TOK_LOR: l1 = l1 || l2; break;
        default:
            goto general_case;
        }
	if (t1 != VT_LLONG && (PTR_SIZE != 8 || t1 != VT_PTR))
	    l1 = ((uint32_t)l1 |
		(v1->type.t & VT_UNSIGNED ? 0 : -(l1 & 0x80000000)));
        v1->c.i = l1;
        s1->vtop--;
    } else {
        /* if commutative ops, put c2 as constant */
        if (c1 && (op == '+' || op == '&' || op == '^' || 
                   op == '|' || op == '*' || op == TOK_EQ || op == TOK_NE)) {
            vswap(s1);
            c2 = c1; //c = c1, c1 = c2, c2 = c;
            l2 = l1; //l = l1, l1 = l2, l2 = l;
        }
        if (!s1->const_wanted &&
            c1 && ((l1 == 0 &&
                    (op == TOK_SHL || op == TOK_SHR || op == TOK_SAR)) ||
                   (l1 == -1 && op == TOK_SAR))) {
            /* treat (0 << x), (0 >> x) and (-1 >> x) as constant */
            s1->vtop--;
        } else if (!s1->const_wanted &&
                   c2 && ((l2 == 0 && (op == '&' || op == '*')) ||
                          (op == '|' &&
                            (l2 == -1 || (l2 == 0xFFFFFFFF && t2 != VT_LLONG))) ||
                          (l2 == 1 && (op == '%' || op == TOK_UMOD)))) {
            /* treat (x & 0), (x * 0), (x | -1) and (x % 1) as constant */
            if (l2 == 1)
                s1->vtop->c.i = 0;
            vswap(s1);
            s1->vtop--;
        } else if (c2 && (((op == '*' || op == '/' || op == TOK_UDIV ||
                          op == TOK_PDIV) &&
                           l2 == 1) ||
                          ((op == '+' || op == '-' || op == '|' || op == '^' ||
                            op == TOK_SHL || op == TOK_SHR || op == TOK_SAR) &&
                           l2 == 0) ||
                          (op == '&' &&
                            (l2 == -1 || (l2 == 0xFFFFFFFF && t2 != VT_LLONG))))) {
            /* filter out NOP operations like x*1, x-0, x&-1... */
            s1->vtop--;
        } else if (c2 && (op == '*' || op == TOK_PDIV || op == TOK_UDIV)) {
            /* try to use shifts instead of muls or divs */
            if (l2 > 0 && (l2 & (l2 - 1)) == 0) {
                int n = -1;
                while (l2) {
                    l2 >>= 1;
                    n++;
                }
                s1->vtop->c.i = n;
                if (op == '*')
                    op = TOK_SHL;
                else if (op == TOK_PDIV)
                    op = TOK_SAR;
                else
                    op = TOK_SHR;
            }
            goto general_case;
        } else if (c2 && (op == '+' || op == '-') &&
                   (((s1->vtop[-1].r & (VT_VALMASK | VT_LVAL | VT_SYM)) == (VT_CONST | VT_SYM))
                    || (s1->vtop[-1].r & (VT_VALMASK | VT_LVAL)) == VT_LOCAL)) {
            /* symbol + constant case */
            if (op == '-')
                l2 = -l2;
	    l2 += s1->vtop[-1].c.i;
	    /* The backends can't always deal with addends to symbols
	       larger than +-1<<31.  Don't construct such.  */
	    if ((int)l2 != l2)
	        goto general_case;
            s1->vtop--;
            s1->vtop->c.i = l2;
        } else {
        general_case:
                /* call low level op generator */
                if (t1 == VT_LLONG || t2 == VT_LLONG ||
                    (PTR_SIZE == 8 && (t1 == VT_PTR || t2 == VT_PTR)))
                    gen_opl(s1, op);
                else
                    gen_opi(s1, op);
        }
    }
}

#if defined TCC_TARGET_X86_64 || defined TCC_TARGET_I386
# define gen_negf gen_opf
#elif defined TCC_TARGET_ARM
void gen_negf(TCCState *s1, int op)
{
    /* arm will detect 0-x and replace by vneg */
    vpushi(s1, 0), vswap(s1), gen_op(s1, '-');
}
#else
/* XXX: implement in gen_opf() for other backends too */
void gen_negf(TCCState *s1, int op)
{
    /* In IEEE negate(x) isn't subtract(0,x).  Without NaNs it's
       subtract(-0, x), but with them it's really a sign flip
       operation.  We implement this with bit manipulation and have
       to do some type reinterpretation for this, which TCC can do
       only via memory.  */

    int align, size, bt;

    size = type_size(s1, &s1->vtop->type, &align);
    bt = s1->vtop->type.t & VT_BTYPE;
    save_reg(s1, gv(s1, RC_TYPE(bt)));
    vdup(s1);
    incr_bf_adr(s1, size - 1);
    vdup(s1);
    vpushi(s1, 0x80); /* flip sign */
    gen_op(s1, '^');
    vstore(s1);
    vpop(s1);
}
#endif

/* generate a floating point operation with constant propagation */
static void gen_opif(TCCState *s1, int op)
{
    int c1, c2;
    SValue *v1, *v2;
#if defined _MSC_VER && defined __x86_64__
    /* avoid bad optimization with f1 -= f2 for f1:-0.0, f2:0.0 */
    volatile
#endif
    long double f1, f2;

    v1 = s1->vtop - 1;
    v2 = s1->vtop;
    if (op == TOK_NEG)
        v1 = v2;

    /* currently, we cannot do computations with forward symbols */
    c1 = (v1->r & (VT_VALMASK | VT_LVAL | VT_SYM)) == VT_CONST;
    c2 = (v2->r & (VT_VALMASK | VT_LVAL | VT_SYM)) == VT_CONST;
    if (c1 && c2) {
        if (v1->type.t == VT_FLOAT) {
            f1 = v1->c.f;
            f2 = v2->c.f;
        } else if (v1->type.t == VT_DOUBLE) {
            f1 = v1->c.d;
            f2 = v2->c.d;
        } else {
            f1 = v1->c.ld;
            f2 = v2->c.ld;
        }
        /* NOTE: we only do constant propagation if finite number (not
           NaN or infinity) (ANSI spec) */
        if (!(ieee_finite(f1) || !ieee_finite(f2)) && !s1->const_wanted)
            goto general_case;
        switch(op) {
        case '+': f1 += f2; break;
        case '-': f1 -= f2; break;
        case '*': f1 *= f2; break;
        case '/': 
            if (f2 == 0.0) {
                union { float f; unsigned u; } x1, x2, y;
		/* If not in initializer we need to potentially generate
		   FP exceptions at runtime, otherwise we want to fold.  */
                if (!s1->const_wanted)
                    goto general_case;
                /* the run-time result of 0.0/0.0 on x87, also of other compilers
                   when used to compile the f1 /= f2 below, would be -nan */
                x1.f = f1, x2.f = f2;
                if (f1 == 0.0)
                    y.u = 0x7fc00000; /* nan */
                else
                    y.u = 0x7f800000; /* infinity */
                y.u |= (x1.u ^ x2.u) & 0x80000000; /* set sign */
                f1 = y.f;
                break;
            }
            f1 /= f2;
            break;
        case TOK_NEG:
            f1 = -f1;
            goto unary_result;
            /* XXX: also handles tests ? */
        default:
            goto general_case;
        }
        s1->vtop--;
    unary_result:
        /* XXX: overflow test ? */
        if (v1->type.t == VT_FLOAT) {
            v1->c.f = f1;
        } else if (v1->type.t == VT_DOUBLE) {
            v1->c.d = f1;
        } else {
            v1->c.ld = f1;
        }
    } else {
    general_case:
        if (op == TOK_NEG) {
            gen_negf(s1, op);
        } else {
            gen_opf(s1, op);
        }
    }
}

/* print a type. If 'varstr' is not NULL, then the variable is also
   printed in the type */
/* XXX: union */
/* XXX: add array and function pointers */
static void type_to_str(TCCState *s1, char *buf, int buf_size,
                 CType *type, const char *varstr)
{
    int bt, v, t;
    Sym *s, *sa;
    char buf1[256];
    const char *tstr;

    t = type->t;
    bt = t & VT_BTYPE;
    buf[0] = '\0';

    if (t & VT_EXTERN)
        pstrcat(buf, buf_size, "extern ");
    if (t & VT_STATIC)
        pstrcat(buf, buf_size, "static ");
    if (t & VT_TYPEDEF)
        pstrcat(buf, buf_size, "typedef ");
    if (t & VT_INLINE)
        pstrcat(buf, buf_size, "inline ");
    if (bt != VT_PTR) {
        if (t & VT_VOLATILE)
            pstrcat(buf, buf_size, "volatile ");
        if (t & VT_CONSTANT)
            pstrcat(buf, buf_size, "const ");
    }
    if (((t & VT_DEFSIGN) && bt == VT_BYTE)
        || ((t & VT_UNSIGNED)
            && (bt == VT_SHORT || bt == VT_INT || bt == VT_LLONG)
            && !IS_ENUM(t)
            ))
        pstrcat(buf, buf_size, (t & VT_UNSIGNED) ? "unsigned " : "signed ");

    buf_size -= strlen(buf);
    buf += strlen(buf);

    switch(bt) {
    case VT_VOID:
        tstr = "void";
        goto add_tstr;
    case VT_BOOL:
        tstr = "_Bool";
        goto add_tstr;
    case VT_BYTE:
        tstr = "char";
        goto add_tstr;
    case VT_SHORT:
        tstr = "short";
        goto add_tstr;
    case VT_INT:
        tstr = "int";
        goto maybe_long;
    case VT_LLONG:
        tstr = "long long";
    maybe_long:
        if (t & VT_LONG)
            tstr = "long";
        if (!IS_ENUM(t))
            goto add_tstr;
        tstr = "enum ";
        goto tstruct;
    case VT_FLOAT:
        tstr = "float";
        goto add_tstr;
    case VT_DOUBLE:
        tstr = "double";
        if (!(t & VT_LONG))
            goto add_tstr;
    case VT_LDOUBLE:
        tstr = "long double";
    add_tstr:
        pstrcat(buf, buf_size, tstr);
        break;
    case VT_STRUCT:
        tstr = "struct ";
        if (IS_UNION(t))
            tstr = "union ";
    tstruct:
        pstrcat(buf, buf_size, tstr);
        v = type->ref->v & ~SYM_STRUCT;
        if (v >= SYM_FIRST_ANOM)
            pstrcat(buf, buf_size, "<anonymous>");
        else
            pstrcat(buf, buf_size, get_tok_str(s1, v, NULL));
        break;
    case VT_FUNC:
        s = type->ref;
        buf1[0]=0;
        if (varstr && '*' == *varstr) {
            pstrcat(buf1, sizeof(buf1), "(");
            pstrcat(buf1, sizeof(buf1), varstr);
            pstrcat(buf1, sizeof(buf1), ")");
        }
        pstrcat(buf1, buf_size, "(");
        sa = s->next;
        while (sa != NULL) {
            char buf2[256];
            type_to_str(s1, buf2, sizeof(buf2), &sa->type, NULL);
            pstrcat(buf1, sizeof(buf1), buf2);
            sa = sa->next;
            if (sa)
                pstrcat(buf1, sizeof(buf1), ", ");
        }
        if (s->f.func_type == FUNC_ELLIPSIS)
            pstrcat(buf1, sizeof(buf1), ", ...");
        pstrcat(buf1, sizeof(buf1), ")");
        type_to_str(s1, buf, buf_size, &s->type, buf1);
        goto no_var;
    case VT_PTR:
        s = type->ref;
        if (t & VT_ARRAY) {
            if (varstr && '*' == *varstr)
                snprintf(buf1, sizeof(buf1), "(%s)[%d]", varstr, s->c);
            else
                snprintf(buf1, sizeof(buf1), "%s[%d]", varstr ? varstr : "", s->c);
            type_to_str(s1, buf, buf_size, &s->type, buf1);
            goto no_var;
        }
        pstrcpy(buf1, sizeof(buf1), "*");
        if (t & VT_CONSTANT)
            pstrcat(buf1, buf_size, "const ");
        if (t & VT_VOLATILE)
            pstrcat(buf1, buf_size, "volatile ");
        if (varstr)
            pstrcat(buf1, sizeof(buf1), varstr);
        type_to_str(s1, buf, buf_size, &s->type, buf1);
        goto no_var;
    }
    if (varstr) {
        pstrcat(buf, buf_size, " ");
        pstrcat(buf, buf_size, varstr);
    }
 no_var: ;
}

static void type_incompatibility_error(TCCState *s1, CType* st, CType* dt, const char* fmt)
{
    char buf1[256], buf2[256];
    type_to_str(s1, buf1, sizeof(buf1), st, NULL);
    type_to_str(s1, buf2, sizeof(buf2), dt, NULL);
    tcc_error(s1, fmt, buf1, buf2);
}

static void type_incompatibility_warning(TCCState *s1, CType* st, CType* dt, const char* fmt)
{
    char buf1[256], buf2[256];
    type_to_str(s1, buf1, sizeof(buf1), st, NULL);
    type_to_str(s1, buf2, sizeof(buf2), dt, NULL);
    tcc_warning(s1, fmt, buf1, buf2);
}

static int pointed_size(TCCState *s1, CType *type)
{
    int align;
    return type_size(s1, pointed_type(type), &align);
}

static void vla_runtime_pointed_size(TCCState *s1, CType *type)
{
    int align;
    vla_runtime_type_size(s1, pointed_type(type), &align);
}

static inline int is_null_pointer(SValue *p)
{
    if ((p->r & (VT_VALMASK | VT_LVAL | VT_SYM)) != VT_CONST)
        return 0;
    return ((p->type.t & VT_BTYPE) == VT_INT && (uint32_t)p->c.i == 0) ||
        ((p->type.t & VT_BTYPE) == VT_LLONG && p->c.i == 0) ||
        ((p->type.t & VT_BTYPE) == VT_PTR &&
         (PTR_SIZE == 4 ? (uint32_t)p->c.i == 0 : p->c.i == 0) &&
         ((pointed_type(&p->type)->t & VT_BTYPE) == VT_VOID) &&
         0 == (pointed_type(&p->type)->t & (VT_CONSTANT | VT_VOLATILE))
         );
}

/* compare function types. OLD functions match any new functions */
static int is_compatible_func(CType *type1, CType *type2)
{
    Sym *s1, *s2;

    s1 = type1->ref;
    s2 = type2->ref;
    if (s1->f.func_call != s2->f.func_call)
        return 0;
    if (s1->f.func_type != s2->f.func_type
        && s1->f.func_type != FUNC_OLD
        && s2->f.func_type != FUNC_OLD)
        return 0;
    for (;;) {
        if (!is_compatible_unqualified_types(&s1->type, &s2->type))
            return 0;
        if (s1->f.func_type == FUNC_OLD || s2->f.func_type == FUNC_OLD )
            return 1;
        s1 = s1->next;
        s2 = s2->next;
        if (!s1)
            return !s2;
        if (!s2)
            return 0;
    }
}

/* return true if type1 and type2 are the same.  If unqualified is
   true, qualifiers on the types are ignored.
 */
static int compare_types(CType *type1, CType *type2, int unqualified)
{
    int bt1, t1, t2;

    t1 = type1->t & VT_TYPE;
    t2 = type2->t & VT_TYPE;
    if (unqualified) {
        /* strip qualifiers before comparing */
        t1 &= ~(VT_CONSTANT | VT_VOLATILE);
        t2 &= ~(VT_CONSTANT | VT_VOLATILE);
    }

    /* Default Vs explicit signedness only matters for char */
    if ((t1 & VT_BTYPE) != VT_BYTE) {
        t1 &= ~VT_DEFSIGN;
        t2 &= ~VT_DEFSIGN;
    }
    /* XXX: bitfields ? */
    if (t1 != t2)
        return 0;

    if ((t1 & VT_ARRAY)
        && !(type1->ref->c < 0
          || type2->ref->c < 0
          || type1->ref->c == type2->ref->c))
            return 0;

    /* test more complicated cases */
    bt1 = t1 & VT_BTYPE;
    if (bt1 == VT_PTR) {
        type1 = pointed_type(type1);
        type2 = pointed_type(type2);
        return is_compatible_types(type1, type2);
    } else if (bt1 == VT_STRUCT) {
        return (type1->ref == type2->ref);
    } else if (bt1 == VT_FUNC) {
        return is_compatible_func(type1, type2);
    } else if (IS_ENUM(type1->t) && IS_ENUM(type2->t)) {
        /* If both are enums then they must be the same, if only one is then
           t1 and t2 must be equal, which was checked above already.  */
        return type1->ref == type2->ref;
    } else {
        return 1;
    }
}

/* Check if OP1 and OP2 can be "combined" with operation OP, the combined
   type is stored in DEST if non-null (except for pointer plus/minus) . */
static int combine_types(TCCState *s1, CType *dest, SValue *op1, SValue *op2, int op)
{
    CType *type1 = &op1->type, *type2 = &op2->type, type;
    int t1 = type1->t, t2 = type2->t, bt1 = t1 & VT_BTYPE, bt2 = t2 & VT_BTYPE;
    int ret = 1;

    type.t = VT_VOID;
    type.ref = NULL;

    if (bt1 == VT_VOID || bt2 == VT_VOID) {
        ret = op == '?' ? 1 : 0;
        /* NOTE: as an extension, we accept void on only one side */
        type.t = VT_VOID;
    } else if (bt1 == VT_PTR || bt2 == VT_PTR) {
        if (op == '+') ; /* Handled in caller */
        /* http://port70.net/~nsz/c/c99/n1256.html#6.5.15p6 */
        /* If one is a null ptr constant the result type is the other.  */
        else if (is_null_pointer (op2)) type = *type1;
        else if (is_null_pointer (op1)) type = *type2;
        else if (bt1 != bt2) {
            /* accept comparison or cond-expr between pointer and integer
               with a warning */
            if ((op == '?' || TOK_ISCOND(op))
                && (is_integer_btype(bt1) || is_integer_btype(bt2)))
              tcc_warning(s1, "pointer/integer mismatch in %s",
                          op == '?' ? "conditional expression" : "comparison");
            else if (op != '-' || !is_integer_btype(bt2))
              ret = 0;
            type = *(bt1 == VT_PTR ? type1 : type2);
        } else {
            CType *pt1 = pointed_type(type1);
            CType *pt2 = pointed_type(type2);
            int pbt1 = pt1->t & VT_BTYPE;
            int pbt2 = pt2->t & VT_BTYPE;
            int newquals, copied = 0;
            if (pbt1 != VT_VOID && pbt2 != VT_VOID
                && !compare_types(pt1, pt2, 1/*unqualif*/)) {
                if (op != '?' && !TOK_ISCOND(op))
                  ret = 0;
                else
                  type_incompatibility_warning(s1, type1, type2,
                    op == '?'
                     ? "pointer type mismatch in conditional expression ('%s' and '%s')"
                     : "pointer type mismatch in comparison('%s' and '%s')");
            }
            if (op == '?') {
                /* pointers to void get preferred, otherwise the
                   pointed to types minus qualifs should be compatible */
                type = *((pbt1 == VT_VOID) ? type1 : type2);
                /* combine qualifs */
                newquals = ((pt1->t | pt2->t) & (VT_CONSTANT | VT_VOLATILE));
                if ((~pointed_type(&type)->t & (VT_CONSTANT | VT_VOLATILE))
                    & newquals)
                  {
                    /* copy the pointer target symbol */
                    type.ref = sym_push(s1, SYM_FIELD, &type.ref->type,
                                        0, type.ref->c);
                    copied = 1;
                    pointed_type(&type)->t |= newquals;
                  }
                /* pointers to incomplete arrays get converted to
                   pointers to completed ones if possible */
                if (pt1->t & VT_ARRAY
                    && pt2->t & VT_ARRAY
                    && pointed_type(&type)->ref->c < 0
                    && (pt1->ref->c > 0 || pt2->ref->c > 0))
                  {
                    if (!copied)
                      type.ref = sym_push(s1, SYM_FIELD, &type.ref->type,
                                          0, type.ref->c);
                    pointed_type(&type)->ref =
                        sym_push(s1, SYM_FIELD, &pointed_type(&type)->ref->type,
                                 0, pointed_type(&type)->ref->c);
                    pointed_type(&type)->ref->c =
                        0 < pt1->ref->c ? pt1->ref->c : pt2->ref->c;
                  }
            }
        }
        if (TOK_ISCOND(op))
          type.t = VT_SIZE_T;
    } else if (bt1 == VT_STRUCT || bt2 == VT_STRUCT) {
        if (op != '?' || !compare_types(type1, type2, 1))
          ret = 0;
        type = *type1;
    } else if (is_float(bt1) || is_float(bt2)) {
        if (bt1 == VT_LDOUBLE || bt2 == VT_LDOUBLE) {
            type.t = VT_LDOUBLE;
        } else if (bt1 == VT_DOUBLE || bt2 == VT_DOUBLE) {
            type.t = VT_DOUBLE;
        } else {
            type.t = VT_FLOAT;
        }
    } else if (bt1 == VT_LLONG || bt2 == VT_LLONG) {
        /* cast to biggest op */
        type.t = VT_LLONG | VT_LONG;
        if (bt1 == VT_LLONG)
          type.t &= t1;
        if (bt2 == VT_LLONG)
          type.t &= t2;
        /* convert to unsigned if it does not fit in a long long */
        if ((t1 & (VT_BTYPE | VT_UNSIGNED | VT_BITFIELD)) == (VT_LLONG | VT_UNSIGNED) ||
            (t2 & (VT_BTYPE | VT_UNSIGNED | VT_BITFIELD)) == (VT_LLONG | VT_UNSIGNED))
          type.t |= VT_UNSIGNED;
    } else {
        /* integer operations */
        type.t = VT_INT | (VT_LONG & (t1 | t2));
        /* convert to unsigned if it does not fit in an integer */
        if ((t1 & (VT_BTYPE | VT_UNSIGNED | VT_BITFIELD)) == (VT_INT | VT_UNSIGNED) ||
            (t2 & (VT_BTYPE | VT_UNSIGNED | VT_BITFIELD)) == (VT_INT | VT_UNSIGNED))
          type.t |= VT_UNSIGNED;
    }
    if (dest)
      *dest = type;
    return ret;
}

/* generic gen_op: handles types problems */
ST_FUNC void gen_op(TCCState *s1, int op)
{
    int u, t1, t2, bt1, bt2, t;
    CType type1, combtype;

redo:
    t1 = s1->vtop[-1].type.t;
    t2 = s1->vtop[0].type.t;
    bt1 = t1 & VT_BTYPE;
    bt2 = t2 & VT_BTYPE;
        
    if (bt1 == VT_FUNC || bt2 == VT_FUNC) {
	if (bt2 == VT_FUNC) {
	    mk_pointer(s1, &s1->vtop->type);
	    gaddrof(s1);
	}
	if (bt1 == VT_FUNC) {
	    vswap(s1);
	    mk_pointer(s1, &s1->vtop->type);
	    gaddrof(s1);
	    vswap(s1);
	}
	goto redo;
    } else if (!combine_types(s1, &combtype, s1->vtop - 1, s1->vtop, op)) {
        tcc_error_noabort(s1, "invalid operand types for binary operation");
        vpop(s1);
    } else if (bt1 == VT_PTR || bt2 == VT_PTR) {
        /* at least one operand is a pointer */
        /* relational op: must be both pointers */
        if (TOK_ISCOND(op))
            goto std_op;
        /* if both pointers, then it must be the '-' op */
        if (bt1 == VT_PTR && bt2 == VT_PTR) {
            if (op != '-')
                tcc_error(s1, "cannot use pointers here");
            if (s1->vtop[-1].type.t & VT_VLA) {
                vla_runtime_pointed_size(s1, &s1->vtop[-1].type);
            } else {
                vpushi(s1, pointed_size(s1, &s1->vtop[-1].type));
            }
            vrott(s1, 3);
            gen_opic(s1, op);
            s1->vtop->type.t = VT_PTRDIFF_T;
            vswap(s1);
            gen_op(s1, TOK_PDIV);
        } else {
            /* exactly one pointer : must be '+' or '-'. */
            if (op != '-' && op != '+')
                tcc_error(s1, "cannot use pointers here");
            /* Put pointer as first operand */
            if (bt2 == VT_PTR) {
                vswap(s1);
                t = t1, t1 = t2, t2 = t;
            }
#if PTR_SIZE == 4
            if ((s1->vtop[0].type.t & VT_BTYPE) == VT_LLONG)
                /* XXX: truncate here because gen_opl can't handle ptr + long long */
                gen_cast_s(s1, VT_INT);
#endif
            type1 = s1->vtop[-1].type;
            if (s1->vtop[-1].type.t & VT_VLA)
                vla_runtime_pointed_size(s1, &s1->vtop[-1].type);
            else {
                u = pointed_size(s1, &s1->vtop[-1].type);
                if (u < 0)
                    tcc_error(s1, "unknown array element size");
#if PTR_SIZE == 8
                vpushll(s1, u);
#else
                /* XXX: cast to int ? (long long case) */
                vpushi(s1, u);
#endif
            }
            gen_op(s1, '*');
#ifdef CONFIG_TCC_BCHECK
            if (s1->do_bounds_check && !s1->const_wanted) {
                /* if bounded pointers, we generate a special code to
                   test bounds */
                if (op == '-') {
                    vpushi(s1, 0);
                    vswap(s1);
                    gen_op(s1, '-');
                }
                gen_bounded_ptr_add(s1);
            } else
#endif
            {
                gen_opic(s1, op);
            }
            type1.t &= ~VT_ARRAY;
            /* put again type if gen_opic() swaped operands */
            s1->vtop->type = type1;
        }
    } else {
        /* floats can only be used for a few operations */
        if (is_float(combtype.t)
            && op != '+' && op != '-' && op != '*' && op != '/'
            && !TOK_ISCOND(op))
            tcc_error(s1, "invalid operands for binary operation");
        else if (op == TOK_SHR || op == TOK_SAR || op == TOK_SHL) {
            t = bt1 == VT_LLONG ? VT_LLONG : VT_INT;
            if ((t1 & (VT_BTYPE | VT_UNSIGNED | VT_BITFIELD)) == (t | VT_UNSIGNED))
              t |= VT_UNSIGNED;
            t |= (VT_LONG & t1);
            combtype.t = t;
        }
    std_op:
        t = t2 = combtype.t;
        /* XXX: currently, some unsigned operations are explicit, so
           we modify them here */
        if (t & VT_UNSIGNED) {
            if (op == TOK_SAR)
                op = TOK_SHR;
            else if (op == '/')
                op = TOK_UDIV;
            else if (op == '%')
                op = TOK_UMOD;
            else if (op == TOK_LT)
                op = TOK_ULT;
            else if (op == TOK_GT)
                op = TOK_UGT;
            else if (op == TOK_LE)
                op = TOK_ULE;
            else if (op == TOK_GE)
                op = TOK_UGE;
        }
        vswap(s1);
        gen_cast_s(s1, t);
        vswap(s1);
        /* special case for shifts and long long: we keep the shift as
           an integer */
        if (op == TOK_SHR || op == TOK_SAR || op == TOK_SHL)
            t2 = VT_INT;
        gen_cast_s(s1, t2);
        if (is_float(t))
            gen_opif(s1, op);
        else
            gen_opic(s1, op);
        if (TOK_ISCOND(op)) {
            /* relational op: the result is an int */
            s1->vtop->type.t = VT_INT;
        } else {
            s1->vtop->type.t = t;
        }
    }
    // Make sure that we have converted to an rvalue:
    if (s1->vtop->r & VT_LVAL)
        gv(s1, is_float(s1->vtop->type.t & VT_BTYPE) ? RC_FLOAT : RC_INT);
}

#if defined TCC_TARGET_ARM64 || defined TCC_TARGET_RISCV64 || defined TCC_TARGET_ARM
#define gen_cvt_itof1 gen_cvt_itof
#else
/* generic itof for unsigned long long case */
static void gen_cvt_itof1(TCCState *s1, int t)
{
    if ((s1->vtop->type.t & (VT_BTYPE | VT_UNSIGNED)) == 
        (VT_LLONG | VT_UNSIGNED)) {

        if (t == VT_FLOAT)
            vpush_helper_func(s1, TOK___floatundisf);
#if LDOUBLE_SIZE != 8
        else if (t == VT_LDOUBLE)
            vpush_helper_func(s1, TOK___floatundixf);
#endif
        else
            vpush_helper_func(s1, TOK___floatundidf);
        vrott(s1, 2);
        gfunc_call(s1, 1);
        vpushi(s1, 0);
        PUT_R_RET(s1->vtop, t);
    } else {
        gen_cvt_itof(s1, t);
    }
}
#endif

#if defined TCC_TARGET_ARM64 || defined TCC_TARGET_RISCV64
#define gen_cvt_ftoi1 gen_cvt_ftoi
#else
/* generic ftoi for unsigned long long case */
static void gen_cvt_ftoi1(TCCState *s1, int t)
{
    int st;
    if (t == (VT_LLONG | VT_UNSIGNED)) {
        /* not handled natively */
        st = s1->vtop->type.t & VT_BTYPE;
        if (st == VT_FLOAT)
            vpush_helper_func(s1, TOK___fixunssfdi);
#if LDOUBLE_SIZE != 8
        else if (st == VT_LDOUBLE)
            vpush_helper_func(s1, TOK___fixunsxfdi);
#endif
        else
            vpush_helper_func(s1, TOK___fixunsdfdi);
        vrott(s1, 2);
        gfunc_call(s1, 1);
        vpushi(s1, 0);
        PUT_R_RET(s1->vtop, t);
    } else {
        gen_cvt_ftoi(s1, t);
    }
}
#endif

/* special delayed cast for char/short */
static void force_charshort_cast(TCCState *s1)
{
    int sbt = BFGET(s1->vtop->r, VT_MUSTCAST) == 2 ? VT_LLONG : VT_INT;
    int dbt = s1->vtop->type.t;
    s1->vtop->r &= ~VT_MUSTCAST;
    s1->vtop->type.t = sbt;
    gen_cast_s(s1, dbt == VT_BOOL ? VT_BYTE|VT_UNSIGNED : dbt);
    s1->vtop->type.t = dbt;
}

static void gen_cast_s(TCCState *s1, int t)
{
    CType type;
    type.t = t;
    type.ref = NULL;
    gen_cast(s1, &type);
}

/* cast 'vtop' to 'type'. Casting to bitfields is forbidden. */
static void gen_cast(TCCState *s1, CType *type)
{
    int sbt, dbt, sf, df, c;
    int dbt_bt, sbt_bt, ds, ss, bits, trunc;

    /* special delayed cast for char/short */
    if (s1->vtop->r & VT_MUSTCAST)
        force_charshort_cast(s1);

    /* bitfields first get cast to ints */
    if (s1->vtop->type.t & VT_BITFIELD)
        gv(s1, RC_INT);

    dbt = type->t & (VT_BTYPE | VT_UNSIGNED);
    sbt = s1->vtop->type.t & (VT_BTYPE | VT_UNSIGNED);
    if (sbt == VT_FUNC)
        sbt = VT_PTR;

again:
    if (sbt != dbt) {
        sf = is_float(sbt);
        df = is_float(dbt);
        dbt_bt = dbt & VT_BTYPE;
        sbt_bt = sbt & VT_BTYPE;
        if (dbt_bt == VT_VOID)
            goto done;
        if (sbt_bt == VT_VOID) {
error:
            cast_error(s1, &s1->vtop->type, type);
        }

        c = (s1->vtop->r & (VT_VALMASK | VT_LVAL | VT_SYM)) == VT_CONST;
#if !defined TCC_IS_NATIVE && !defined TCC_IS_NATIVE_387
        c &= (dbt != VT_LDOUBLE) | !!s1->nocode_wanted;
#endif
        if (c) {
            /* constant case: we can do it now */
            /* XXX: in ISOC, cannot do it if error in convert */
            if (sbt == VT_FLOAT)
                s1->vtop->c.ld = s1->vtop->c.f;
            else if (sbt == VT_DOUBLE)
                s1->vtop->c.ld = s1->vtop->c.d;

            if (df) {
                if (sbt_bt == VT_LLONG) {
                    if ((sbt & VT_UNSIGNED) || !(s1->vtop->c.i >> 63))
                        s1->vtop->c.ld = s1->vtop->c.i;
                    else
                        s1->vtop->c.ld = -(long double)-s1->vtop->c.i;
                } else if(!sf) {
                    if ((sbt & VT_UNSIGNED) || !(s1->vtop->c.i >> 31))
                        s1->vtop->c.ld = (uint32_t)s1->vtop->c.i;
                    else
                        s1->vtop->c.ld = -(long double)-(uint32_t)s1->vtop->c.i;
                }

                if (dbt == VT_FLOAT)
                    s1->vtop->c.f = (float)s1->vtop->c.ld;
                else if (dbt == VT_DOUBLE)
                    s1->vtop->c.d = (double)s1->vtop->c.ld;
            } else if (sf && dbt == VT_BOOL) {
                s1->vtop->c.i = (s1->vtop->c.ld != 0);
            } else {
                if(sf)
                    s1->vtop->c.i = s1->vtop->c.ld;
                else if (sbt_bt == VT_LLONG || (PTR_SIZE == 8 && sbt == VT_PTR))
                    ;
                else if (sbt & VT_UNSIGNED)
                    s1->vtop->c.i = (uint32_t)s1->vtop->c.i;
                else
                    s1->vtop->c.i = ((uint32_t)s1->vtop->c.i | -(s1->vtop->c.i & 0x80000000));

                if (dbt_bt == VT_LLONG || (PTR_SIZE == 8 && dbt == VT_PTR))
                    ;
                else if (dbt == VT_BOOL)
                    s1->vtop->c.i = (s1->vtop->c.i != 0);
                else {
                    uint32_t m = dbt_bt == VT_BYTE ? 0xff :
                                 dbt_bt == VT_SHORT ? 0xffff :
                                  0xffffffff;
                    s1->vtop->c.i &= m;
                    if (!(dbt & VT_UNSIGNED))
                        s1->vtop->c.i |= -(s1->vtop->c.i & ((m >> 1) + 1));
                }
            }
            goto done;

        } else if (dbt == VT_BOOL
            && (s1->vtop->r & (VT_VALMASK | VT_LVAL | VT_SYM))
                == (VT_CONST | VT_SYM)) {
            /* addresses are considered non-zero (see tcctest.c:sinit23) */
            s1->vtop->r = VT_CONST;
            s1->vtop->c.i = 1;
            goto done;
        }

        /* cannot generate code for global or static initializers */
        if (STATIC_DATA_WANTED)
            goto done;

        /* non constant case: generate code */
        if (dbt == VT_BOOL) {
            gen_test_zero(s1, TOK_NE);
            goto done;
        }

        if (sf || df) {
            if (sf && df) {
                /* convert from fp to fp */
                gen_cvt_ftof(s1, dbt);
            } else if (df) {
                /* convert int to fp */
                gen_cvt_itof1(s1, dbt);
            } else {
                /* convert fp to int */
                sbt = dbt;
                if (dbt_bt != VT_LLONG && dbt_bt != VT_INT)
                    sbt = VT_INT;
                gen_cvt_ftoi1(s1, sbt);
                goto again; /* may need char/short cast */
            }
            goto done;
        }

        ds = btype_size(dbt_bt);
        ss = btype_size(sbt_bt);
        if (ds == 0 || ss == 0)
            goto error;

        if (IS_ENUM(type->t) && type->ref->c < 0)
            tcc_error(s1, "cast to incomplete type");

        /* same size and no sign conversion needed */
        if (ds == ss && ds >= 4)
            goto done;
        if (dbt_bt == VT_PTR || sbt_bt == VT_PTR) {
            tcc_warning(s1, "cast between pointer and integer of different size");
            if (sbt_bt == VT_PTR) {
                /* put integer type to allow logical operations below */
                s1->vtop->type.t = (PTR_SIZE == 8 ? VT_LLONG : VT_INT);
            }
        }

        /* processor allows { int a = 0, b = *(char*)&a; }
           That means that if we cast to less width, we can just
           change the type and read it still later. */
        #define ALLOW_SUBTYPE_ACCESS 1

        if (ALLOW_SUBTYPE_ACCESS && (s1->vtop->r & VT_LVAL)) {
            /* value still in memory */
            if (ds <= ss)
                goto done;
            /* ss <= 4 here */
            if (ds <= 4 && !(dbt == (VT_SHORT | VT_UNSIGNED) && sbt == VT_BYTE)) {
                gv(s1, RC_INT);
                goto done; /* no 64bit envolved */
            }
        }
        gv(s1, RC_INT);

        trunc = 0;
#if PTR_SIZE == 4
        if (ds == 8) {
            /* generate high word */
            if (sbt & VT_UNSIGNED) {
                vpushi(s1, 0);
                gv(s1, RC_INT);
            } else {
                gv_dup(s1);
                vpushi(s1, 31);
                gen_op(s1, TOK_SAR);
            }
            lbuild(s1, dbt);
        } else if (ss == 8) {
            /* from long long: just take low order word */
            lexpand(s1);
            vpop(s1);
        }
        ss = 4;

#elif PTR_SIZE == 8
        if (ds == 8) {
            /* need to convert from 32bit to 64bit */
            if (sbt & VT_UNSIGNED) {
#if defined(TCC_TARGET_RISCV64)
                /* RISC-V keeps 32bit vals in registers sign-extended.
                   So here we need a zero-extension.  */
                trunc = 32;
#else
                goto done;
#endif
            } else {
                gen_cvt_sxtw(s1);
                goto done;
            }
            ss = ds, ds = 4, dbt = sbt;
        } else if (ss == 8) {
            /* RISC-V keeps 32bit vals in registers sign-extended.
               So here we need a sign-extension for signed types and
               zero-extension. for unsigned types. */
#if !defined(TCC_TARGET_RISCV64)
            trunc = 32; /* zero upper 32 bits for non RISC-V targets */
#endif
        } else {
            ss = 4;
        }
#endif

        if (ds >= ss)
            goto done;
#if defined TCC_TARGET_I386 || defined TCC_TARGET_X86_64 || defined TCC_TARGET_ARM64
        if (ss == 4) {
            gen_cvt_csti(s1, dbt);
            goto done;
        }
#endif
        bits = (ss - ds) * 8;
        /* for unsigned, gen_op will convert SAR to SHR */
        s1->vtop->type.t = (ss == 8 ? VT_LLONG : VT_INT) | (dbt & VT_UNSIGNED);
        vpushi(s1, bits);
        gen_op(s1, TOK_SHL);
        vpushi(s1, bits - trunc);
        gen_op(s1, TOK_SAR);
        vpushi(s1, trunc);
        gen_op(s1, TOK_SHR);
    }
done:
    s1->vtop->type = *type;
    s1->vtop->type.t &= ~ ( VT_CONSTANT | VT_VOLATILE | VT_ARRAY );
}

/* return type size as known at compile time. Put alignment at 'a' */
ST_FUNC int type_size(TCCState *s1, CType *type, int *a)
{
    Sym *s;
    int bt;

    bt = type->t & VT_BTYPE;
    if (bt == VT_STRUCT) {
        /* struct/union */
        s = type->ref;
        *a = s->r;
        return s->c;
    } else if (bt == VT_PTR) {
        if (type->t & VT_ARRAY) {
            int ts;

            s = type->ref;
            ts = type_size(s1, &s->type, a);

            if (ts < 0 && s->c < 0)
                ts = -ts;

            return ts * s->c;
        } else {
            *a = PTR_SIZE;
            return PTR_SIZE;
        }
    } else if (IS_ENUM(type->t) && type->ref->c < 0) {
        return -1; /* incomplete enum */
    } else if (bt == VT_LDOUBLE) {
        *a = LDOUBLE_ALIGN;
        return LDOUBLE_SIZE;
    } else if (bt == VT_DOUBLE || bt == VT_LLONG) {
#ifdef TCC_TARGET_I386
#ifdef TCC_TARGET_PE
        *a = 8;
#else
        *a = 4;
#endif
#elif defined(TCC_TARGET_ARM)
#ifdef TCC_ARM_EABI
        *a = 8; 
#else
        *a = 4;
#endif
#else
        *a = 8;
#endif
        return 8;
    } else if (bt == VT_INT || bt == VT_FLOAT) {
        *a = 4;
        return 4;
    } else if (bt == VT_SHORT) {
        *a = 2;
        return 2;
    } else if (bt == VT_QLONG || bt == VT_QFLOAT) {
        *a = 8;
        return 16;
    } else {
        /* char, void, function, _Bool */
        *a = 1;
        return 1;
    }
}

/* push type size as known at runtime time on top of value stack. Put
   alignment at 'a' */
ST_FUNC void vla_runtime_type_size(TCCState *s1, CType *type, int *a)
{
    if (type->t & VT_VLA) {
        type_size(s1, &type->ref->type, a);
        vset(s1, &s1->int_type, VT_LOCAL|VT_LVAL, type->ref->c);
    } else {
        vpushi(s1, type_size(s1, type, a));
    }
}

/* return the pointed type of t */
static inline CType *pointed_type(CType *type)
{
    return &type->ref->type;
}

/* modify type so that its it is a pointer to type. */
ST_FUNC void mk_pointer(TCCState *s1, CType *type)
{
    Sym *s;
    s = sym_push(s1, SYM_FIELD, type, 0, -1);
    type->t = VT_PTR | (type->t & VT_STORAGE);
    type->ref = s;
}

/* return true if type1 and type2 are exactly the same (including
   qualifiers). 
*/
static int is_compatible_types(CType *type1, CType *type2)
{
    return compare_types(type1,type2,0);
}

/* return true if type1 and type2 are the same (ignoring qualifiers).
*/
static int is_compatible_unqualified_types(CType *type1, CType *type2)
{
    return compare_types(type1,type2,1);
}

static void cast_error(TCCState *s1, CType *st, CType *dt)
{
    type_incompatibility_error(s1, st, dt, "cannot convert '%s' to '%s'");
}

/* verify type compatibility to store s1->vtop in 'dt' type */
static void verify_assign_cast(TCCState *s1, CType *dt)
{
    CType *st, *type1, *type2;
    int dbt, sbt, qualwarn, lvl;

    st = &s1->vtop->type; /* source type */
    dbt = dt->t & VT_BTYPE;
    sbt = st->t & VT_BTYPE;
    if (dt->t & VT_CONSTANT)
        tcc_warning(s1, "assignment of read-only location");
    switch(dbt) {
    case VT_VOID:
        if (sbt != dbt)
            tcc_error(s1, "assignment to void expression");
        break;
    case VT_PTR:
        /* special cases for pointers */
        /* '0' can also be a pointer */
        if (is_null_pointer(s1->vtop))
            break;
        /* accept implicit pointer to integer cast with warning */
        if (is_integer_btype(sbt)) {
            tcc_warning(s1, "assignment makes pointer from integer without a cast");
            break;
        }
        type1 = pointed_type(dt);
        if (sbt == VT_PTR)
            type2 = pointed_type(st);
        else if (sbt == VT_FUNC)
            type2 = st; /* a function is implicitly a function pointer */
        else
            goto error;
        if (is_compatible_types(type1, type2))
            break;
        for (qualwarn = lvl = 0;; ++lvl) {
            if (((type2->t & VT_CONSTANT) && !(type1->t & VT_CONSTANT)) ||
                ((type2->t & VT_VOLATILE) && !(type1->t & VT_VOLATILE)))
                qualwarn = 1;
            dbt = type1->t & (VT_BTYPE|VT_LONG);
            sbt = type2->t & (VT_BTYPE|VT_LONG);
            if (dbt != VT_PTR || sbt != VT_PTR)
                break;
            type1 = pointed_type(type1);
            type2 = pointed_type(type2);
        }
        if (!is_compatible_unqualified_types(type1, type2)) {
            if ((dbt == VT_VOID || sbt == VT_VOID) && lvl == 0) {
                /* void * can match anything */
            } else if (dbt == sbt
                && is_integer_btype(sbt & VT_BTYPE)
                && IS_ENUM(type1->t) + IS_ENUM(type2->t)
                    + !!((type1->t ^ type2->t) & VT_UNSIGNED) < 2) {
		/* Like GCC don't warn by default for merely changes
		   in pointer target signedness.  Do warn for different
		   base types, though, in particular for unsigned enums
		   and signed int targets.  */
            } else {
                tcc_warning(s1, "assignment from incompatible pointer type");
                break;
            }
        }
        if (qualwarn)
            tcc_warning(s1, "assignment discards qualifiers from pointer target type");
        break;
    case VT_BYTE:
    case VT_SHORT:
    case VT_INT:
    case VT_LLONG:
        if (sbt == VT_PTR || sbt == VT_FUNC) {
            tcc_warning(s1, "assignment makes integer from pointer without a cast");
        } else if (sbt == VT_STRUCT) {
            goto case_VT_STRUCT;
        }
        /* XXX: more tests */
        break;
    case VT_STRUCT:
    case_VT_STRUCT:
        if (!is_compatible_unqualified_types(dt, st)) {
    error:
            cast_error(s1, st, dt);
        }
        break;
    }
}

static void gen_assign_cast(TCCState *s1, CType *dt)
{
    verify_assign_cast(s1, dt);
    gen_cast(s1, dt);
}

/* store s1->vtop in lvalue pushed on stack */
ST_FUNC void vstore(TCCState *s1)
{
    int sbt, dbt, ft, r, size, align, bit_size, bit_pos, delayed_cast;

    ft = s1->vtop[-1].type.t;
    sbt = s1->vtop->type.t & VT_BTYPE;
    dbt = ft & VT_BTYPE;

    verify_assign_cast(s1, &s1->vtop[-1].type);

    if (sbt == VT_STRUCT) {
        /* if structure, only generate pointer */
        /* structure assignment : generate memcpy */
        /* XXX: optimize if small size */
            size = type_size(s1, &s1->vtop->type, &align);

            /* destination */
            vswap(s1);
#ifdef CONFIG_TCC_BCHECK
            if (s1->vtop->r & VT_MUSTBOUND)
                gbound(s1); /* check would be wrong after gaddrof() */
#endif
            s1->vtop->type.t = VT_PTR;
            gaddrof(s1);

            /* address of memcpy() */
#ifdef TCC_ARM_EABI
            if(!(align & 7))
                vpush_helper_func(s1, TOK_memmove8);
            else if(!(align & 3))
                vpush_helper_func(s1, TOK_memmove4);
            else
#endif
            /* Use memmove, rather than memcpy, as dest and src may be same: */
            vpush_helper_func(s1, TOK_memmove);

            vswap(s1);
            /* source */
            vpushv(s1, s1->vtop - 2);
#ifdef CONFIG_TCC_BCHECK
            if (s1->vtop->r & VT_MUSTBOUND)
                gbound(s1);
#endif
            s1->vtop->type.t = VT_PTR;
            gaddrof(s1);
            /* type size */
            vpushi(s1, size);
            gfunc_call(s1, 3);
        /* leave source on stack */

    } else if (ft & VT_BITFIELD) {
        /* bitfield store handling */

        /* save lvalue as expression result (example: s.b = s.a = n;) */
        vdup(s1), s1->vtop[-1] = s1->vtop[-2];

        bit_pos = BIT_POS(ft);
        bit_size = BIT_SIZE(ft);
        /* remove bit field info to avoid loops */
        s1->vtop[-1].type.t = ft & ~VT_STRUCT_MASK;

        if (dbt == VT_BOOL) {
            gen_cast(s1, &s1->vtop[-1].type);
            s1->vtop[-1].type.t = (s1->vtop[-1].type.t & ~VT_BTYPE) | (VT_BYTE | VT_UNSIGNED);
        }
        r = adjust_bf(s1->vtop - 1, bit_pos, bit_size);
        if (dbt != VT_BOOL) {
            gen_cast(s1, &s1->vtop[-1].type);
            dbt = s1->vtop[-1].type.t & VT_BTYPE;
        }
        if (r == VT_STRUCT) {
            store_packed_bf(s1, bit_pos, bit_size);
        } else {
            unsigned long long mask = (1ULL << bit_size) - 1;
            if (dbt != VT_BOOL) {
                /* mask source */
                if (dbt == VT_LLONG)
                    vpushll(s1, mask);
                else
                    vpushi(s1, (unsigned)mask);
                gen_op(s1, '&');
            }
            /* shift source */
            vpushi(s1, bit_pos);
            gen_op(s1, TOK_SHL);
            vswap(s1);
            /* duplicate destination */
            vdup(s1);
            vrott(s1, 3);
            /* load destination, mask and or with source */
            if (dbt == VT_LLONG)
                vpushll(s1, ~(mask << bit_pos));
            else
                vpushi(s1, ~((unsigned)mask << bit_pos));
            gen_op(s1, '&');
            gen_op(s1, '|');
            /* store result */
            vstore(s1);
            /* ... and discard */
            vpop(s1);
        }
    } else if (dbt == VT_VOID) {
        --s1->vtop;
    } else {
            /* optimize char/short casts */
            delayed_cast = 0;
            if ((dbt == VT_BYTE || dbt == VT_SHORT)
                && is_integer_btype(sbt)
                ) {
                if ((s1->vtop->r & VT_MUSTCAST)
                    && btype_size(dbt) > btype_size(sbt)
                    )
                    force_charshort_cast(s1);
                delayed_cast = 1;
            } else {
                gen_cast(s1, &s1->vtop[-1].type);
            }

#ifdef CONFIG_TCC_BCHECK
            /* bound check case */
            if (s1->vtop[-1].r & VT_MUSTBOUND) {
                vswap(s1);
                gbound(s1);
                vswap(s1);
            }
#endif
            gv(s1, RC_TYPE(dbt)); /* generate value */

            if (delayed_cast) {
                s1->vtop->r |= BFVAL(VT_MUSTCAST, (sbt == VT_LLONG) + 1);
                //tcc_warning(s1, "deley cast %x -> %x", sbt, dbt);
                s1->vtop->type.t = ft & VT_TYPE;
            }

            /* if lvalue was saved on stack, must read it */
            if ((s1->vtop[-1].r & VT_VALMASK) == VT_LLOCAL) {
                SValue sv;
                r = get_reg(s1, RC_INT);
                sv.type.t = VT_PTRDIFF_T;
                sv.r = VT_LOCAL | VT_LVAL;
                sv.c.i = s1->vtop[-1].c.i;
                load(s1, r, &sv);
                s1->vtop[-1].r = r | VT_LVAL;
            }

            r = s1->vtop->r & VT_VALMASK;
            /* two word case handling :
               store second register at word + 4 (or +8 for x86-64)  */
            if (USING_TWO_WORDS(dbt)) {
                int load_type = (dbt == VT_QFLOAT) ? VT_DOUBLE : VT_PTRDIFF_T;
                s1->vtop[-1].type.t = load_type;
                store(s1, r, s1->vtop - 1);
                vswap(s1);
                /* convert to int to increment easily */
                s1->vtop->type.t = VT_PTRDIFF_T;
                gaddrof(s1);
                vpushs(s1, PTR_SIZE);
                gen_op(s1, '+');
                s1->vtop->r |= VT_LVAL;
                vswap(s1);
                s1->vtop[-1].type.t = load_type;
                /* XXX: it works because r2 is spilled last ! */
                store(s1, s1->vtop->r2, s1->vtop - 1);
            } else {
                /* single word */
                store(s1, r, s1->vtop - 1);
            }
        vswap(s1);
        s1->vtop--; /* NOT vpop() because on x86 it would flush the fp stack */
    }
}

/* post defines POST/PRE add. c is the token ++ or -- */
ST_FUNC void inc(TCCState *s1, int post, int c)
{
    test_lvalue(s1);
    vdup(s1); /* save lvalue */
    if (post) {
        gv_dup(s1); /* duplicate value */
        vrotb(s1, 3);
        vrotb(s1, 3);
    }
    /* add constant */
    vpushi(s1, c - TOK_MID); 
    gen_op(s1, '+');
    vstore(s1); /* store value */
    if (post)
        vpop(s1); /* if post op, return saved value */
}

ST_FUNC void parse_mult_str (TCCState *s1, CString *astr, const char *msg)
{
    /* read the string */
    if (s1->tok != TOK_STR)
        expect(s1, msg);
    cstr_new(astr);
    while (s1->tok == TOK_STR) {
        /* XXX: add \0 handling too ? */
        cstr_cat(s1, astr, s1->tokc.str.data, -1);
        next(s1);
    }
    cstr_ccat(s1, astr, '\0');
}

/* If I is >= 1 and a power of two, returns log2(i)+1.
   If I is 0 returns 0.  */
ST_FUNC int exact_log2p1(int i)
{
  int ret;
  if (!i)
    return 0;
  for (ret = 1; i >= 1 << 8; ret += 8)
    i >>= 8;
  if (i >= 1 << 4)
    ret += 4, i >>= 4;
  if (i >= 1 << 2)
    ret += 2, i >>= 2;
  if (i >= 1 << 1)
    ret++;
  return ret;
}

/* Parse __attribute__((...)) GNUC extension. */
static void parse_attribute(TCCState *s1, AttributeDef *ad)
{
    int t, n;
    CString astr;
    
redo:
    if (s1->tok != TOK_ATTRIBUTE1 && s1->tok != TOK_ATTRIBUTE2)
        return;
    next(s1);
    skip(s1, '(');
    skip(s1, '(');
    while (s1->tok != ')') {
        if (s1->tok < TOK_IDENT)
            expect(s1, "attribute name");
        t = s1->tok;
        next(s1);
        switch(t) {
	case TOK_CLEANUP1:
	case TOK_CLEANUP2:
	{
	    Sym *s;

	    skip(s1, '(');
	    s = sym_find(s1, s1->tok);
	    if (!s) {
	      tcc_warning(s1, "implicit declaration of function '%s'",
			  get_tok_str(s1, s1->tok, &s1->tokc));
	      s = external_global_sym(s1, s1->tok, &s1->func_old_type);
            } else if ((s->type.t & VT_BTYPE) != VT_FUNC)
                tcc_error(s1, "'%s' is not declared as function", get_tok_str(s1, s1->tok, &s1->tokc));
	    ad->cleanup_func = s;
	    next(s1);
            skip(s1, ')');
	    break;
	}
        case TOK_CONSTRUCTOR1:
        case TOK_CONSTRUCTOR2:
            ad->f.func_ctor = 1;
            break;
        case TOK_DESTRUCTOR1:
        case TOK_DESTRUCTOR2:
            ad->f.func_dtor = 1;
            break;
        case TOK_ALWAYS_INLINE1:
        case TOK_ALWAYS_INLINE2:
            ad->f.func_alwinl = 1;
            break;
        case TOK_SECTION1:
        case TOK_SECTION2:
            skip(s1, '(');
	    parse_mult_str(s1, &astr, "section name");
            ad->section = find_section(s1, (char *)astr.data);
            skip(s1, ')');
	    cstr_free(s1, &astr);
            break;
        case TOK_ALIAS1:
        case TOK_ALIAS2:
            skip(s1, '(');
	    parse_mult_str(s1, &astr, "alias(\"target\")");
            ad->alias_target = /* save string as token, for later */
                tok_alloc(s1, (char*)astr.data, astr.size-1)->tok;
            skip(s1, ')');
	    cstr_free(s1, &astr);
            break;
	case TOK_VISIBILITY1:
	case TOK_VISIBILITY2:
            skip(s1, '(');
	    parse_mult_str(s1, &astr,
			   "visibility(\"default|hidden|internal|protected\")");
	    if (!strcmp (astr.data, "default"))
	        ad->a.visibility = STV_DEFAULT;
	    else if (!strcmp (astr.data, "hidden"))
	        ad->a.visibility = STV_HIDDEN;
	    else if (!strcmp (astr.data, "internal"))
	        ad->a.visibility = STV_INTERNAL;
	    else if (!strcmp (astr.data, "protected"))
	        ad->a.visibility = STV_PROTECTED;
	    else
                expect(s1, "visibility(\"default|hidden|internal|protected\")");
            skip(s1, ')');
	    cstr_free(s1, &astr);
            break;
        case TOK_ALIGNED1:
        case TOK_ALIGNED2:
            if (s1->tok == '(') {
                next(s1);
                n = expr_const(s1);
                if (n <= 0 || (n & (n - 1)) != 0) 
                    tcc_error(s1, "alignment must be a positive power of two");
                skip(s1, ')');
            } else {
                n = MAX_ALIGN;
            }
            ad->a.aligned = exact_log2p1(n);
	    if (n != 1 << (ad->a.aligned - 1))
	      tcc_error(s1, "alignment of %d is larger than implemented", n);
            break;
        case TOK_PACKED1:
        case TOK_PACKED2:
            ad->a.packed = 1;
            break;
        case TOK_WEAK1:
        case TOK_WEAK2:
            ad->a.weak = 1;
            break;
        case TOK_UNUSED1:
        case TOK_UNUSED2:
            /* currently, no need to handle it because tcc does not
               track unused objects */
            break;
        case TOK_NORETURN1:
        case TOK_NORETURN2:
            ad->f.func_noreturn = 1;
            break;
        case TOK_CDECL1:
        case TOK_CDECL2:
        case TOK_CDECL3:
            ad->f.func_call = FUNC_CDECL;
            break;
        case TOK_STDCALL1:
        case TOK_STDCALL2:
        case TOK_STDCALL3:
            ad->f.func_call = FUNC_STDCALL;
            break;
#ifdef TCC_TARGET_I386
        case TOK_REGPARM1:
        case TOK_REGPARM2:
            skip(s1, '(');
            n = expr_const(s1);
            if (n > 3) 
                n = 3;
            else if (n < 0)
                n = 0;
            if (n > 0)
                ad->f.func_call = FUNC_FASTCALL1 + n - 1;
            skip(s1, ')');
            break;
        case TOK_FASTCALL1:
        case TOK_FASTCALL2:
        case TOK_FASTCALL3:
            ad->f.func_call = FUNC_FASTCALLW;
            break;            
#endif
        case TOK_MODE:
            skip(s1, '(');
            switch(s1->tok) {
                case TOK_MODE_DI:
                    ad->attr_mode = VT_LLONG + 1;
                    break;
                case TOK_MODE_QI:
                    ad->attr_mode = VT_BYTE + 1;
                    break;
                case TOK_MODE_HI:
                    ad->attr_mode = VT_SHORT + 1;
                    break;
                case TOK_MODE_SI:
                case TOK_MODE_word:
                    ad->attr_mode = VT_INT + 1;
                    break;
                default:
                    tcc_warning(s1, "__mode__(%s) not supported\n", get_tok_str(s1, s1->tok, NULL));
                    break;
            }
            next(s1);
            skip(s1, ')');
            break;
        case TOK_DLLEXPORT:
            ad->a.dllexport = 1;
            break;
        case TOK_NODECORATE:
            ad->a.nodecorate = 1;
            break;
        case TOK_DLLIMPORT:
            ad->a.dllimport = 1;
            break;
        default:
            if (s1->warn_unsupported)
                tcc_warning(s1, "'%s' attribute ignored", get_tok_str(s1, t, NULL));
            /* skip parameters */
            if (s1->tok == '(') {
                int parenthesis = 0;
                do {
                    if (s1->tok == '(') 
                        parenthesis++;
                    else if (s1->tok == ')') 
                        parenthesis--;
                    next(s1);
                } while (parenthesis && s1->tok != -1);
            }
            break;
        }
        if (s1->tok != ',')
            break;
        next(s1);
    }
    skip(s1, ')');
    skip(s1, ')');
    goto redo;
}

static Sym * find_field (CType *type, int v, int *cumofs)
{
    Sym *s = type->ref;
    v |= SYM_FIELD;
    while ((s = s->next) != NULL) {
	if ((s->v & SYM_FIELD) &&
	    (s->type.t & VT_BTYPE) == VT_STRUCT &&
	    (s->v & ~SYM_FIELD) >= SYM_FIRST_ANOM) {
	    Sym *ret = find_field (&s->type, v, cumofs);
	    if (ret) {
                *cumofs += s->c;
	        return ret;
            }
	}
	if (s->v == v)
	  break;
    }
    return s;
}

static void check_fields (TCCState *s1, CType *type, int check)
{
    Sym *s = type->ref;

    while ((s = s->next) != NULL) {
        int v = s->v & ~SYM_FIELD;
        if (v < SYM_FIRST_ANOM) {
            TokenSym *ts = s1->table_ident[v - TOK_IDENT];
            if (check && (ts->tok & SYM_FIELD))
                tcc_error(s1, "duplicate member '%s'", get_tok_str(s1, v, NULL));
            ts->tok ^= SYM_FIELD;
        } else if ((s->type.t & VT_BTYPE) == VT_STRUCT)
            check_fields (s1, &s->type, check);
    }
}

static void struct_layout(TCCState *s1, CType *type, AttributeDef *ad)
{
    int size, align, maxalign, offset, c, bit_pos, bit_size;
    int packed, a, bt, prevbt, prev_bit_size;
    int pcc = !s1->ms_bitfields;
    int pragma_pack = *s1->pack_stack_ptr;
    Sym *f;

    maxalign = 1;
    offset = 0;
    c = 0;
    bit_pos = 0;
    prevbt = VT_STRUCT; /* make it never match */
    prev_bit_size = 0;

//#define BF_DEBUG

    for (f = type->ref->next; f; f = f->next) {
        if (f->type.t & VT_BITFIELD)
            bit_size = BIT_SIZE(f->type.t);
        else
            bit_size = -1;
        size = type_size(s1, &f->type, &align);
        a = f->a.aligned ? 1 << (f->a.aligned - 1) : 0;
        packed = 0;

        if (pcc && bit_size == 0) {
            /* in pcc mode, packing does not affect zero-width bitfields */

        } else {
            /* in pcc mode, attribute packed overrides if set. */
            if (pcc && (f->a.packed || ad->a.packed))
                align = packed = 1;

            /* pragma pack overrides align if lesser and packs bitfields always */
            if (pragma_pack) {
                packed = 1;
                if (pragma_pack < align)
                    align = pragma_pack;
                /* in pcc mode pragma pack also overrides individual align */
                if (pcc && pragma_pack < a)
                    a = 0;
            }
        }
        /* some individual align was specified */
        if (a)
            align = a;

        if (type->ref->type.t == VT_UNION) {
	    if (pcc && bit_size >= 0)
	        size = (bit_size + 7) >> 3;
	    offset = 0;
	    if (size > c)
	        c = size;

	} else if (bit_size < 0) {
            if (pcc)
                c += (bit_pos + 7) >> 3;
	    c = (c + align - 1) & -align;
	    offset = c;
	    if (size > 0)
	        c += size;
	    bit_pos = 0;
	    prevbt = VT_STRUCT;
	    prev_bit_size = 0;

	} else {
	    /* A bit-field.  Layout is more complicated.  There are two
	       options: PCC (GCC) compatible and MS compatible */
            if (pcc) {
		/* In PCC layout a bit-field is placed adjacent to the
                   preceding bit-fields, except if:
                   - it has zero-width
                   - an individual alignment was given
                   - it would overflow its base type container and
                     there is no packing */
                if (bit_size == 0) {
            new_field:
		    c = (c + ((bit_pos + 7) >> 3) + align - 1) & -align;
		    bit_pos = 0;
                } else if (f->a.aligned) {
                    goto new_field;
                } else if (!packed) {
                    int a8 = align * 8;
	            int ofs = ((c * 8 + bit_pos) % a8 + bit_size + a8 - 1) / a8;
                    if (ofs > size / align)
                        goto new_field;
                }

                /* in pcc mode, long long bitfields have type int if they fit */
                if (size == 8 && bit_size <= 32)
                    f->type.t = (f->type.t & ~VT_BTYPE) | VT_INT, size = 4;

                while (bit_pos >= align * 8)
                    c += align, bit_pos -= align * 8;
                offset = c;

		/* In PCC layout named bit-fields influence the alignment
		   of the containing struct using the base types alignment,
		   except for packed fields (which here have correct align).  */
		if (f->v & SYM_FIRST_ANOM
                    // && bit_size // ??? gcc on ARM/rpi does that
                    )
		    align = 1;

	    } else {
		bt = f->type.t & VT_BTYPE;
		if ((bit_pos + bit_size > size * 8)
                    || (bit_size > 0) == (bt != prevbt)
                    ) {
		    c = (c + align - 1) & -align;
		    offset = c;
		    bit_pos = 0;
		    /* In MS bitfield mode a bit-field run always uses
		       at least as many bits as the underlying type.
		       To start a new run it's also required that this
		       or the last bit-field had non-zero width.  */
		    if (bit_size || prev_bit_size)
		        c += size;
		}
		/* In MS layout the records alignment is normally
		   influenced by the field, except for a zero-width
		   field at the start of a run (but by further zero-width
		   fields it is again).  */
		if (bit_size == 0 && prevbt != bt)
		    align = 1;
		prevbt = bt;
                prev_bit_size = bit_size;
	    }

	    f->type.t = (f->type.t & ~(0x3f << VT_STRUCT_SHIFT))
		        | (bit_pos << VT_STRUCT_SHIFT);
	    bit_pos += bit_size;
	}
	if (align > maxalign)
	    maxalign = align;

#ifdef BF_DEBUG
	printf("set field %s offset %-2d size %-2d align %-2d",
	       get_tok_str(f->v & ~SYM_FIELD, NULL), offset, size, align);
	if (f->type.t & VT_BITFIELD) {
	    printf(" pos %-2d bits %-2d",
                    BIT_POS(f->type.t),
                    BIT_SIZE(f->type.t)
                    );
	}
	printf("\n");
#endif

        f->c = offset;
	f->r = 0;
    }

    if (pcc)
        c += (bit_pos + 7) >> 3;

    /* store size and alignment */
    a = bt = ad->a.aligned ? 1 << (ad->a.aligned - 1) : 1;
    if (a < maxalign)
        a = maxalign;
    type->ref->r = a;
    if (pragma_pack && pragma_pack < maxalign && 0 == pcc) {
        /* can happen if individual align for some member was given.  In
           this case MSVC ignores maxalign when aligning the size */
        a = pragma_pack;
        if (a < bt)
            a = bt;
    }
    c = (c + a - 1) & -a;
    type->ref->c = c;

#ifdef BF_DEBUG
    printf("struct size %-2d align %-2d\n\n", c, a), fflush(stdout);
#endif

    /* check whether we can access bitfields by their type */
    for (f = type->ref->next; f; f = f->next) {
        int s, px, cx, c0;
        CType t;

        if (0 == (f->type.t & VT_BITFIELD))
            continue;
        f->type.ref = f;
        f->auxtype = -1;
        bit_size = BIT_SIZE(f->type.t);
        if (bit_size == 0)
            continue;
        bit_pos = BIT_POS(f->type.t);
        size = type_size(s1, &f->type, &align);

        if (bit_pos + bit_size <= size * 8 && f->c + size <= c
#ifdef TCC_TARGET_ARM
            && !(f->c & (align - 1))
#endif
            )
            continue;

        /* try to access the field using a different type */
        c0 = -1, s = align = 1;
        t.t = VT_BYTE;
        for (;;) {
            px = f->c * 8 + bit_pos;
            cx = (px >> 3) & -align;
            px = px - (cx << 3);
            if (c0 == cx)
                break;
            s = (px + bit_size + 7) >> 3;
            if (s > 4) {
                t.t = VT_LLONG;
            } else if (s > 2) {
                t.t = VT_INT;
            } else if (s > 1) {
                t.t = VT_SHORT;
            } else {
                t.t = VT_BYTE;
            }
            s = type_size(s1, &t, &align);
            c0 = cx;
        }

        if (px + bit_size <= s * 8 && cx + s <= c
#ifdef TCC_TARGET_ARM
            && !(cx & (align - 1))
#endif
            ) {
            /* update offset and bit position */
            f->c = cx;
            bit_pos = px;
	    f->type.t = (f->type.t & ~(0x3f << VT_STRUCT_SHIFT))
		        | (bit_pos << VT_STRUCT_SHIFT);
            if (s != size)
                f->auxtype = t.t;
#ifdef BF_DEBUG
            printf("FIX field %s offset %-2d size %-2d align %-2d "
                "pos %-2d bits %-2d\n",
                get_tok_str(s1, f->v & ~SYM_FIELD, NULL),
                cx, s, align, px, bit_size);
#endif
        } else {
            /* fall back to load/store single-byte wise */
            f->auxtype = VT_STRUCT;
#ifdef BF_DEBUG
            printf("FIX field %s : load byte-wise\n",
                 get_tok_str(f->v & ~SYM_FIELD, NULL));
#endif
        }
    }
}

/* enum/struct/union declaration. u is VT_ENUM/VT_STRUCT/VT_UNION */
static void struct_decl(TCCState *s1, CType *type, int u)
{
    int v, c, size, align, flexible;
    int bit_size, bsize, bt;
    Sym *s, *ss, **ps;
    AttributeDef ad, ad1;
    CType type1, btype;

    memset(&ad, 0, sizeof ad);
    next(s1);
    parse_attribute(s1, &ad);
    if (s1->tok != '{') {
        v = s1->tok;
        next(s1);
        /* struct already defined ? return it */
        if (v < TOK_IDENT)
            expect(s1, "struct/union/enum name");
        s = struct_find(s1, v);
        if (s && (s->sym_scope == s1->local_scope || s1->tok != '{')) {
            if (u == s->type.t)
                goto do_decl;
            if (u == VT_ENUM && IS_ENUM(s->type.t))
                goto do_decl;
            tcc_error(s1, "redefinition of '%s'", get_tok_str(s1, v, NULL));
        }
    } else {
        v = s1->anon_sym++;
    }
    /* Record the original enum/struct/union token.  */
    type1.t = u == VT_ENUM ? u | VT_INT | VT_UNSIGNED : u;
    type1.ref = NULL;
    /* we put an undefined size for struct/union */
    s = sym_push(s1, v | SYM_STRUCT, &type1, 0, -1);
    s->r = 0; /* default alignment is zero as gcc */
do_decl:
    type->t = s->type.t;
    type->ref = s;

    if (s1->tok == '{') {
        next(s1);
        if (s->c != -1)
            tcc_error(s1, "struct/union/enum already defined");
        s->c = -2;
        /* cannot be empty */
        /* non empty enums are not allowed */
        ps = &s->next;
        if (u == VT_ENUM) {
            long long ll = 0, pl = 0, nl = 0;
	    CType t;
            t.ref = s;
            /* enum symbols have static storage */
            t.t = VT_INT|VT_STATIC|VT_ENUM_VAL;
            for(;;) {
                v = s1->tok;
                if (v < TOK_UIDENT)
                    expect(s1, "identifier");
                ss = sym_find(s1, v);
                if (ss && !s1->local_stack)
                    tcc_error(s1, "redefinition of enumerator '%s'",
                              get_tok_str(s1, v, NULL));
                next(s1);
                if (s1->tok == '=') {
                    next(s1);
		    ll = expr_const64(s1);
                }
                ss = sym_push(s1, v, &t, VT_CONST, 0);
                ss->enum_val = ll;
                *ps = ss, ps = &ss->next;
                if (ll < nl)
                    nl = ll;
                if (ll > pl)
                    pl = ll;
                if (s1->tok != ',')
                    break;
                next(s1);
                ll++;
                /* NOTE: we accept a trailing comma */
                if (s1->tok == '}')
                    break;
            }
            skip(s1, '}');
            /* set integral type of the enum */
            t.t = VT_INT;
            if (nl >= 0) {
                if (pl != (unsigned)pl)
                    t.t = (LONG_SIZE==8 ? VT_LLONG|VT_LONG : VT_LLONG);
                t.t |= VT_UNSIGNED;
            } else if (pl != (int)pl || nl != (int)nl)
                t.t = (LONG_SIZE==8 ? VT_LLONG|VT_LONG : VT_LLONG);
            s->type.t = type->t = t.t | VT_ENUM;
            s->c = 0;
            /* set type for enum members */
            for (ss = s->next; ss; ss = ss->next) {
                ll = ss->enum_val;
                if (ll == (int)ll) /* default is int if it fits */
                    continue;
                if (t.t & VT_UNSIGNED) {
                    ss->type.t |= VT_UNSIGNED;
                    if (ll == (unsigned)ll)
                        continue;
                }
                ss->type.t = (ss->type.t & ~VT_BTYPE)
                    | (LONG_SIZE==8 ? VT_LLONG|VT_LONG : VT_LLONG);
            }
        } else {
            c = 0;
            flexible = 0;
            while (s1->tok != '}') {
                if (!parse_btype(s1, &btype, &ad1)) {
		    skip(s1, ';');
		    continue;
		}
                while (1) {
		    if (flexible)
		        tcc_error(s1, "flexible array member '%s' not at the end of struct",
                              get_tok_str(s1, v, NULL));
                    bit_size = -1;
                    v = 0;
                    type1 = btype;
                    if (s1->tok != ':') {
			if (s1->tok != ';')
                            type_decl(s1, &type1, &ad1, &v, TYPE_DIRECT);
                        if (v == 0) {
                    	    if ((type1.t & VT_BTYPE) != VT_STRUCT)
                        	expect(s1, "identifier");
                    	    else {
				int v = btype.ref->v;
				if (!(v & SYM_FIELD) && (v & ~SYM_STRUCT) < SYM_FIRST_ANOM) {
				    if (s1->ms_extensions == 0)
                        		expect(s1, "identifier");
				}
                    	    }
                        }
                        if (type_size(s1, &type1, &align) < 0) {
			    if ((u == VT_STRUCT) && (type1.t & VT_ARRAY) && c)
			        flexible = 1;
			    else
			        tcc_error(s1, "field '%s' has incomplete type",
                                      get_tok_str(s1, v, NULL));
                        }
                        if ((type1.t & VT_BTYPE) == VT_FUNC ||
			    (type1.t & VT_BTYPE) == VT_VOID ||
                            (type1.t & VT_STORAGE))
                            tcc_error(s1, "invalid type for '%s'", 
                                  get_tok_str(s1, v, NULL));
                    }
                    if (s1->tok == ':') {
                        next(s1);
                        bit_size = expr_const(s1);
                        /* XXX: handle v = 0 case for messages */
                        if (bit_size < 0)
                            tcc_error(s1, "negative width in bit-field '%s'", 
                                  get_tok_str(s1, v, NULL));
                        if (v && bit_size == 0)
                            tcc_error(s1, "zero width for bit-field '%s'", 
                                  get_tok_str(s1, v, NULL));
			parse_attribute(s1, &ad1);
                    }
                    size = type_size(s1, &type1, &align);
                    if (bit_size >= 0) {
                        bt = type1.t & VT_BTYPE;
                        if (bt != VT_INT && 
                            bt != VT_BYTE && 
                            bt != VT_SHORT &&
                            bt != VT_BOOL &&
                            bt != VT_LLONG)
                            tcc_error(s1, "bitfields must have scalar type");
                        bsize = size * 8;
                        if (bit_size > bsize) {
                            tcc_error(s1, "width of '%s' exceeds its type",
                                  get_tok_str(s1, v, NULL));
                        } else if (bit_size == bsize
                                    && !ad.a.packed && !ad1.a.packed) {
                            /* no need for bit fields */
                            ;
                        } else if (bit_size == 64) {
                            tcc_error(s1, "field width 64 not implemented");
                        } else {
                            type1.t = (type1.t & ~VT_STRUCT_MASK)
                                | VT_BITFIELD
                                | (bit_size << (VT_STRUCT_SHIFT + 6));
                        }
                    }
                    if (v != 0 || (type1.t & VT_BTYPE) == VT_STRUCT) {
                        /* Remember we've seen a real field to check
			   for placement of flexible array member. */
			c = 1;
                    }
		    /* If member is a struct or bit-field, enforce
		       placing into the struct (as anonymous).  */
                    if (v == 0 &&
			((type1.t & VT_BTYPE) == VT_STRUCT ||
			 bit_size >= 0)) {
		        v = s1->anon_sym++;
		    }
                    if (v) {
                        ss = sym_push(s1, v | SYM_FIELD, &type1, 0, 0);
                        ss->a = ad1.a;
                        *ps = ss;
                        ps = &ss->next;
                    }
                    if (s1->tok == ';' || s1->tok == TOK_EOF)
                        break;
                    skip(s1, ',');
                }
                skip(s1, ';');
            }
            skip(s1, '}');
	    parse_attribute(s1, &ad);
            if (ad.cleanup_func) {
                tcc_warning(s1, "attribute '__cleanup__' ignored on type");
            }
	    check_fields(s1, type, 1);
	    check_fields(s1, type, 0);
            struct_layout(s1, type, &ad);
        }
    }
}

static void sym_to_attr(TCCState *s1, AttributeDef *ad, Sym *s)
{
    merge_symattr(s1, &ad->a, &s->a);
    merge_funcattr(s1, &ad->f, &s->f);
}

/* Add type qualifiers to a type. If the type is an array then the qualifiers
   are added to the element type, copied because it could be a typedef. */
static void parse_btype_qualify(TCCState *s1, CType *type, int qualifiers)
{
    while (type->t & VT_ARRAY) {
        type->ref = sym_push(s1, SYM_FIELD, &type->ref->type, 0, type->ref->c);
        type = &type->ref->type;
    }
    type->t |= qualifiers;
}

/* return 0 if no type declaration. otherwise, return the basic type
   and skip it. 
 */
static int parse_btype(TCCState *s1, CType *type, AttributeDef *ad)
{
    int t, u, bt, st, type_found, typespec_found, g, n;
    Sym *s;
    CType type1;

    memset(ad, 0, sizeof(AttributeDef));
    type_found = 0;
    typespec_found = 0;
    t = VT_INT;
    bt = st = -1;
    type->ref = NULL;

    while(1) {
        switch(s1->tok) {
        case TOK_EXTENSION:
            /* currently, we really ignore extension */
            next(s1);
            continue;

            /* basic types */
        case TOK_CHAR:
            u = VT_BYTE;
        basic_type:
            next(s1);
        basic_type1:
            if (u == VT_SHORT || u == VT_LONG) {
                if (st != -1 || (bt != -1 && bt != VT_INT))
                    tmbt: tcc_error(s1, "too many basic types");
                st = u;
            } else {
                if (bt != -1 || (st != -1 && u != VT_INT))
                    goto tmbt;
                bt = u;
            }
            if (u != VT_INT)
                t = (t & ~(VT_BTYPE|VT_LONG)) | u;
            typespec_found = 1;
            break;
        case TOK_VOID:
            u = VT_VOID;
            goto basic_type;
        case TOK_SHORT:
            u = VT_SHORT;
            goto basic_type;
        case TOK_INT:
            u = VT_INT;
            goto basic_type;
        case TOK_ALIGNAS:
            { int n;
              AttributeDef ad1;
              next(s1);
              skip(s1, '(');
              memset(&ad1, 0, sizeof(AttributeDef));
              if (parse_btype(s1, &type1, &ad1)) {
                  type_decl(s1, &type1, &ad1, &n, TYPE_ABSTRACT);
                  if (ad1.a.aligned)
                    n = 1 << (ad1.a.aligned - 1);
                  else
                    type_size(s1, &type1, &n);
              } else {
                  n = expr_const(s1);
                  if (n <= 0 || (n & (n - 1)) != 0)
                    tcc_error(s1, "alignment must be a positive power of two");
              }
              skip(s1, ')');
              ad->a.aligned = exact_log2p1(n);
            }
            continue;
        case TOK_LONG:
            if ((t & VT_BTYPE) == VT_DOUBLE) {
                t = (t & ~(VT_BTYPE|VT_LONG)) | VT_LDOUBLE;
            } else if ((t & (VT_BTYPE|VT_LONG)) == VT_LONG) {
                t = (t & ~(VT_BTYPE|VT_LONG)) | VT_LLONG;
            } else {
                u = VT_LONG;
                goto basic_type;
            }
            next(s1);
            break;
#ifdef TCC_TARGET_ARM64
        case TOK_UINT128:
            /* GCC's __uint128_t appears in some Linux header files. Make it a
               synonym for long double to get the size and alignment right. */
            u = VT_LDOUBLE;
            goto basic_type;
#endif
        case TOK_BOOL:
            u = VT_BOOL;
            goto basic_type;
        case TOK_FLOAT:
            u = VT_FLOAT;
            goto basic_type;
        case TOK_DOUBLE:
            if ((t & (VT_BTYPE|VT_LONG)) == VT_LONG) {
                t = (t & ~(VT_BTYPE|VT_LONG)) | VT_LDOUBLE;
            } else {
                u = VT_DOUBLE;
                goto basic_type;
            }
            next(s1);
            break;
        case TOK_ENUM:
            struct_decl(s1, &type1, VT_ENUM);
        basic_type2:
            u = type1.t;
            type->ref = type1.ref;
            goto basic_type1;
        case TOK_STRUCT:
            struct_decl(s1, &type1, VT_STRUCT);
            goto basic_type2;
        case TOK_UNION:
            struct_decl(s1, &type1, VT_UNION);
            goto basic_type2;

            /* type modifiers */
        case TOK__Atomic:
            next(s1);
            type->t = t;
            parse_btype_qualify(s1, type, VT_ATOMIC);
            t = type->t;
            if (s1->tok == '(') {
                parse_expr_type(s1, &type1);
                /* remove all storage modifiers except typedef */
                type1.t &= ~(VT_STORAGE&~VT_TYPEDEF);
                if (type1.ref)
                    sym_to_attr(s1, ad, type1.ref);
                goto basic_type2;
            }
            break;
        case TOK_CONST1:
        case TOK_CONST2:
        case TOK_CONST3:
            type->t = t;
            parse_btype_qualify(s1, type, VT_CONSTANT);
            t = type->t;
            next(s1);
            break;
        case TOK_VOLATILE1:
        case TOK_VOLATILE2:
        case TOK_VOLATILE3:
            type->t = t;
            parse_btype_qualify(s1, type, VT_VOLATILE);
            t = type->t;
            next(s1);
            break;
        case TOK_SIGNED1:
        case TOK_SIGNED2:
        case TOK_SIGNED3:
            if ((t & (VT_DEFSIGN|VT_UNSIGNED)) == (VT_DEFSIGN|VT_UNSIGNED))
                tcc_error(s1, "signed and unsigned modifier");
            t |= VT_DEFSIGN;
            next(s1);
            typespec_found = 1;
            break;
        case TOK_REGISTER:
        case TOK_AUTO:
        case TOK_RESTRICT1:
        case TOK_RESTRICT2:
        case TOK_RESTRICT3:
            next(s1);
            break;
        case TOK_UNSIGNED:
            if ((t & (VT_DEFSIGN|VT_UNSIGNED)) == VT_DEFSIGN)
                tcc_error(s1, "signed and unsigned modifier");
            t |= VT_DEFSIGN | VT_UNSIGNED;
            next(s1);
            typespec_found = 1;
            break;

            /* storage */
        case TOK_EXTERN:
            g = VT_EXTERN;
            goto storage;
        case TOK_STATIC:
            g = VT_STATIC;
            goto storage;
        case TOK_TYPEDEF:
            g = VT_TYPEDEF;
            goto storage;
       storage:
            if (t & (VT_EXTERN|VT_STATIC|VT_TYPEDEF) & ~g)
                tcc_error(s1, "multiple storage classes");
            t |= g;
            next(s1);
            break;
        case TOK_INLINE1:
        case TOK_INLINE2:
        case TOK_INLINE3:
            t |= VT_INLINE;
            next(s1);
            break;
        case TOK_NORETURN3:
            next(s1);
            ad->f.func_noreturn = 1;
            break;
            /* GNUC attribute */
        case TOK_ATTRIBUTE1:
        case TOK_ATTRIBUTE2:
            parse_attribute(s1, ad);
            if (ad->attr_mode) {
                u = ad->attr_mode -1;
                t = (t & ~(VT_BTYPE|VT_LONG)) | u;
            }
            continue;
            /* GNUC typeof */
        case TOK_TYPEOF1:
        case TOK_TYPEOF2:
        case TOK_TYPEOF3:
            next(s1);
            parse_expr_type(s1, &type1);
            /* remove all storage modifiers except typedef */
            type1.t &= ~(VT_STORAGE&~VT_TYPEDEF);
	    if (type1.ref)
                sym_to_attr(s1, ad, type1.ref);
            goto basic_type2;
        default:
            if (typespec_found)
                goto the_end;
            s = sym_find(s1, s1->tok);
            if (!s || !(s->type.t & VT_TYPEDEF))
                goto the_end;

            n = s1->tok, next(s1);
            if (s1->tok == ':' && !s1->in_generic) {
                /* ignore if it's a label */
                unget_tok(s1, n);
                goto the_end;
            }

            t &= ~(VT_BTYPE|VT_LONG);
            u = t & ~(VT_CONSTANT | VT_VOLATILE), t ^= u;
            type->t = (s->type.t & ~VT_TYPEDEF) | u;
            type->ref = s->type.ref;
            if (t)
                parse_btype_qualify(s1, type, t);
            t = type->t;
            /* get attributes from typedef */
            sym_to_attr(s1, ad, s);
            typespec_found = 1;
            st = bt = -2;
            break;
        }
        type_found = 1;
    }
the_end:
    if (s1->char_is_unsigned) {
        if ((t & (VT_DEFSIGN|VT_BTYPE)) == VT_BYTE)
            t |= VT_UNSIGNED;
    }
    /* VT_LONG is used just as a modifier for VT_INT / VT_LLONG */
    bt = t & (VT_BTYPE|VT_LONG);
    if (bt == VT_LONG)
        t |= LONG_SIZE == 8 ? VT_LLONG : VT_INT;
#ifdef TCC_USING_DOUBLE_FOR_LDOUBLE
    if (bt == VT_LDOUBLE)
        t = (t & ~(VT_BTYPE|VT_LONG)) | (VT_DOUBLE|VT_LONG);
#endif
    type->t = t;
    return type_found;
}

/* convert a function parameter type (array to pointer and function to
   function pointer) */
static inline void convert_parameter_type(TCCState *s1, CType *pt)
{
    /* remove const and volatile qualifiers (XXX: const could be used
       to indicate a const function parameter */
    pt->t &= ~(VT_CONSTANT | VT_VOLATILE);
    /* array must be transformed to pointer according to ANSI C */
    pt->t &= ~VT_ARRAY;
    if ((pt->t & VT_BTYPE) == VT_FUNC) {
        mk_pointer(s1, pt);
    }
}

ST_FUNC void parse_asm_str(TCCState *s1, CString *astr)
{
    skip(s1, '(');
    parse_mult_str(s1, astr, "string constant");
}

/* Parse an asm label and return the token */
static int asm_label_instr(TCCState *s1)
{
    int v;
    CString astr;

    next(s1);
    parse_asm_str(s1, &astr);
    skip(s1, ')');
#ifdef ASM_DEBUG
    printf("asm_alias: \"%s\"\n", (char *)astr.data);
#endif
    v = tok_alloc(s1, astr.data, astr.size - 1)->tok;
    cstr_free(s1, &astr);
    return v;
}

static int post_type(TCCState *s1, CType *type, AttributeDef *ad, int storage, int td)
{
    int n, l, t1, arg_size, align, unused_align;
    Sym **plast, *s, *first;
    AttributeDef ad1;
    CType pt;

    if (s1->tok == '(') {
        /* function type, or recursive declarator (return if so) */
        next(s1);
	if (td && !(td & TYPE_ABSTRACT))
	  return 0;
	if (s1->tok == ')')
	  l = 0;
	else if (parse_btype(s1, &pt, &ad1))
	  l = FUNC_NEW;
	else if (td) {
	    merge_attr (s1, ad, &ad1);
	    return 0;
	} else
	  l = FUNC_OLD;
        first = NULL;
        plast = &first;
        arg_size = 0;
        if (l) {
            for(;;) {
                /* read param name and compute offset */
                if (l != FUNC_OLD) {
                    if ((pt.t & VT_BTYPE) == VT_VOID && s1->tok == ')')
                        break;
                    type_decl(s1, &pt, &ad1, &n, TYPE_DIRECT | TYPE_ABSTRACT);
                    if ((pt.t & VT_BTYPE) == VT_VOID)
                        tcc_error(s1, "parameter declared as void");
                } else {
                    n = s1->tok;
                    if (n < TOK_UIDENT)
                        expect(s1, "identifier");
                    pt.t = VT_VOID; /* invalid type */
                    pt.ref = NULL;
                    next(s1);
                }
                convert_parameter_type(s1, &pt);
                arg_size += (type_size(s1, &pt, &align) + PTR_SIZE - 1) / PTR_SIZE;
                s = sym_push(s1, n | SYM_FIELD, &pt, 0, 0);
                *plast = s;
                plast = &s->next;
                if (s1->tok == ')')
                    break;
                skip(s1, ',');
                if (l == FUNC_NEW && s1->tok == TOK_DOTS) {
                    l = FUNC_ELLIPSIS;
                    next(s1);
                    break;
                }
		if (l == FUNC_NEW && !parse_btype(s1, &pt, &ad1))
		    tcc_error(s1, "invalid type");
            }
        } else
            /* if no parameters, then old type prototype */
            l = FUNC_OLD;
        skip(s1, ')');
        /* NOTE: const is ignored in returned type as it has a special
           meaning in gcc / C++ */
        type->t &= ~VT_CONSTANT; 
        /* some ancient pre-K&R C allows a function to return an array
           and the array brackets to be put after the arguments, such 
           that "int c()[]" means something like "int[] c()" */
        if (s1->tok == '[') {
            next(s1);
            skip(s1, ']'); /* only handle simple "[]" */
            mk_pointer(s1, type);
        }
        /* we push a anonymous symbol which will contain the function prototype */
        ad->f.func_args = arg_size;
        ad->f.func_type = l;
        s = sym_push(s1, SYM_FIELD, type, 0, 0);
        s->a = ad->a;
        s->f = ad->f;
        s->next = first;
        type->t = VT_FUNC;
        type->ref = s;
    } else if (s1->tok == '[') {
	int saved_nocode_wanted = s1->nocode_wanted;
        /* array definition */
        next(s1);
	while (1) {
	    /* XXX The optional type-quals and static should only be accepted
	       in parameter decls.  The '*' as well, and then even only
	       in prototypes (not function defs).  */
	    switch (s1->tok) {
	    case TOK_RESTRICT1: case TOK_RESTRICT2: case TOK_RESTRICT3:
	    case TOK_CONST1:
	    case TOK_VOLATILE1:
	    case TOK_STATIC:
	    case '*':
		next(s1);
		continue;
	    default:
		break;
	    }
	    break;
	}
        n = -1;
        t1 = 0;
        if (s1->tok != ']') {
            if (!s1->local_stack || (storage & VT_STATIC))
                vpushi(s1, expr_const(s1));
            else {
		/* VLAs (which can only happen with local_stack && !VT_STATIC)
		   length must always be evaluated, even under nocode_wanted,
		   so that its size slot is initialized (e.g. under sizeof
		   or typeof).  */
		s1->nocode_wanted = 0;
		gexpr(s1);
	    }
            if ((s1->vtop->r & (VT_VALMASK | VT_LVAL | VT_SYM)) == VT_CONST) {
                n = s1->vtop->c.i;
                if (n < 0)
                    tcc_error(s1, "invalid array size");
            } else {
                if (!is_integer_btype(s1->vtop->type.t & VT_BTYPE))
                    tcc_error(s1, "size of variable length array should be an integer");
                n = 0;
                t1 = VT_VLA;
            }
        }
        skip(s1, ']');
        /* parse next post type */
        post_type(s1, type, ad, storage, 0);

        if ((type->t & VT_BTYPE) == VT_FUNC)
            tcc_error(s1, "declaration of an array of functions");
        if ((type->t & VT_BTYPE) == VT_VOID
            || type_size(s1, type, &unused_align) < 0)
            tcc_error(s1, "declaration of an array of incomplete type elements");

        t1 |= type->t & VT_VLA;

        if (t1 & VT_VLA) {
            if (n < 0)
              tcc_error(s1, "need explicit inner array size in VLAs");
            s1->loc -= type_size(s1, &s1->int_type, &align);
            s1->loc &= -align;
            n = s1->loc;

            vla_runtime_type_size(s1, type, &align);
            gen_op(s1, '*');
            vset(s1, &s1->int_type, VT_LOCAL|VT_LVAL, n);
            vswap(s1);
            vstore(s1);
        }
        if (n != -1)
            vpop(s1);
	s1->nocode_wanted = saved_nocode_wanted;
                
        /* we push an anonymous symbol which will contain the array
           element type */
        s = sym_push(s1, SYM_FIELD, type, 0, n);
        type->t = (t1 ? VT_VLA : VT_ARRAY) | VT_PTR;
        type->ref = s;
    }
    return 1;
}

/* Parse a type declarator (except basic type), and return the type
   in 'type'. 'td' is a bitmask indicating which kind of type decl is
   expected. 'type' should contain the basic type. 'ad' is the
   attribute definition of the basic type. It can be modified by
   type_decl().  If this (possibly abstract) declarator is a pointer chain
   it returns the innermost pointed to type (equals *type, but is a different
   pointer), otherwise returns type itself, that's used for recursive calls.  */
static CType *type_decl(TCCState *s1, CType *type, AttributeDef *ad, int *v, int td)
{
    CType *post, *ret;
    int qualifiers, storage;

    /* recursive type, remove storage bits first, apply them later again */
    storage = type->t & VT_STORAGE;
    type->t &= ~VT_STORAGE;
    post = ret = type;

    while (s1->tok == '*') {
        qualifiers = 0;
    redo:
        next(s1);
        switch(s1->tok) {
        case TOK__Atomic:
            qualifiers |= VT_ATOMIC;
            goto redo;
        case TOK_CONST1:
        case TOK_CONST2:
        case TOK_CONST3:
            qualifiers |= VT_CONSTANT;
            goto redo;
        case TOK_VOLATILE1:
        case TOK_VOLATILE2:
        case TOK_VOLATILE3:
            qualifiers |= VT_VOLATILE;
            goto redo;
        case TOK_RESTRICT1:
        case TOK_RESTRICT2:
        case TOK_RESTRICT3:
            goto redo;
	/* XXX: clarify attribute handling */
	case TOK_ATTRIBUTE1:
	case TOK_ATTRIBUTE2:
	    parse_attribute(s1, ad);
	    break;
        }
        mk_pointer(s1, type);
        type->t |= qualifiers;
	if (ret == type)
	    /* innermost pointed to type is the one for the first derivation */
	    ret = pointed_type(type);
    }

    if (s1->tok == '(') {
	/* This is possibly a parameter type list for abstract declarators
	   ('int ()'), use post_type for testing this.  */
	if (!post_type(s1, type, ad, 0, td)) {
	    /* It's not, so it's a nested declarator, and the post operations
	       apply to the innermost pointed to type (if any).  */
	    /* XXX: this is not correct to modify 'ad' at this point, but
	       the syntax is not clear */
	    parse_attribute(s1,ad);
	    post = type_decl(s1, type, ad, v, td);
	    skip(s1, ')');
	} else
	  goto abstract;
    } else if (s1->tok >= TOK_IDENT && (td & TYPE_DIRECT)) {
	/* type identifier */
	*v = s1->tok;
	next(s1);
    } else {
  abstract:
	if (!(td & TYPE_ABSTRACT))
	  expect(s1, "identifier");
	*v = 0;
    }
    post_type(s1, post, ad, storage, 0);
    parse_attribute(s1,ad);
    type->t |= storage;
    return ret;
}

/* indirection with full error checking and bound check */
ST_FUNC void indir(TCCState *s1)
{
    if ((s1->vtop->type.t & VT_BTYPE) != VT_PTR) {
        if ((s1->vtop->type.t & VT_BTYPE) == VT_FUNC)
            return;
        expect(s1, "pointer");
    }
    if (s1->vtop->r & VT_LVAL)
        gv(s1, RC_INT);
    s1->vtop->type = *pointed_type(&s1->vtop->type);
    /* Arrays and functions are never lvalues */
    if (!(s1->vtop->type.t & (VT_ARRAY | VT_VLA))
        && (s1->vtop->type.t & VT_BTYPE) != VT_FUNC) {
        s1->vtop->r |= VT_LVAL;
        /* if bound checking, the referenced pointer must be checked */
#ifdef CONFIG_TCC_BCHECK
        if (s1->do_bounds_check)
            s1->vtop->r |= VT_MUSTBOUND;
#endif
    }
}

/* pass a parameter to a function and do type checking and casting */
static void gfunc_param_typed(TCCState *s1, Sym *func, Sym *arg)
{
    int func_type;
    CType type;

    func_type = func->f.func_type;
    if (func_type == FUNC_OLD ||
        (func_type == FUNC_ELLIPSIS && arg == NULL)) {
        /* default casting : only need to convert float to double */
        if ((s1->vtop->type.t & VT_BTYPE) == VT_FLOAT) {
            gen_cast_s(s1, VT_DOUBLE);
        } else if (s1->vtop->type.t & VT_BITFIELD) {
            type.t = s1->vtop->type.t & (VT_BTYPE | VT_UNSIGNED);
	    type.ref = s1->vtop->type.ref;
            gen_cast(s1, &type);
        } else if (s1->vtop->r & VT_MUSTCAST) {
            force_charshort_cast(s1);
        }
    } else if (arg == NULL) {
        tcc_error(s1, "too many arguments to function");
    } else {
        type = arg->type;
        type.t &= ~VT_CONSTANT; /* need to do that to avoid false warning */
        gen_assign_cast(s1, &type);
    }
}

/* parse an expression and return its type without any side effect. */
static void expr_type(TCCState *s1, CType *type, void (*expr_fn)(TCCState *))
{
    s1->nocode_wanted++;
    expr_fn(s1);
    *type = s1->vtop->type;
    vpop(s1);
    s1->nocode_wanted--;
}

/* parse an expression of the form '(type)' or '(expr)' and return its
   type */
static void parse_expr_type(TCCState *s1, CType *type)
{
    int n;
    AttributeDef ad;

    skip(s1, '(');
    if (parse_btype(s1, type, &ad)) {
        type_decl(s1, type, &ad, &n, TYPE_ABSTRACT);
    } else {
        expr_type(s1, type, gexpr);
    }
    skip(s1, ')');
}

static void parse_type(TCCState *s1, CType *type)
{
    AttributeDef ad;
    int n;

    if (!parse_btype(s1, type, &ad)) {
        expect(s1, "type");
    }
    type_decl(s1, type, &ad, &n, TYPE_ABSTRACT);
}

static void parse_builtin_params(TCCState *s1, int nc, const char *args)
{
    char c, sep = '(';
    CType type;
    if (nc)
        s1->nocode_wanted++;
    next(s1);
    if (*args == 0)
	skip(s1, sep);
    while ((c = *args++)) {
	skip(s1, sep);
	sep = ',';
        if (c == 't') {
            parse_type(s1, &type);
	    vpush(s1, &type);
	    continue;
        }
        expr_eq(s1);
        type.ref = NULL;
        type.t = 0;
	switch (c) {
	    case 'e':
		continue;
	    case 'V':
                type.t = VT_CONSTANT;
	    case 'v':
                type.t |= VT_VOID;
                mk_pointer (s1, &type);
                break;
	    case 'S':
                type.t = VT_CONSTANT;
	    case 's':
                type.t |= s1->char_type.t;
                mk_pointer (s1, &type);
                break;
	    case 'i':
                type.t = VT_INT;
                break;
	    case 'l':
                type.t = VT_SIZE_T;
                break;
	    default:
                break;
	}
        gen_assign_cast(s1, &type);
    }
    skip(s1, ')');
    if (nc)
        s1->nocode_wanted--;
}

static inline int is_memory_model(const SValue *sv)
{
    /*
     * FIXME
     * The memory models should better be backed by an enumeration.
     *
     *    const int t = sv->type.t;
     *
     *    if (!IS_ENUM_VAL(t))
     *        return 0;
     *
     *    if (!(t & VT_STATIC))
     *        return 0;
     *
     * Ideally we should check whether the model matches 1:1.
     * If it is possible, we should check by the name of the value.
     */
    return 1;
}

#define ATOMIC_ID(ATOK) \
    (ATOK - TOK___atomic_store)

static void parse_atomic(TCCState *s1, int atok)
{
    int mode;
    size_t arg;
    SValue *call;
    CType atom;
    static const char *const templates[] = {
        /*
         * Each entry consists of callback and function template.
         * The template represents argument types and return type.
         *
         * ? void (return-only)
         * b bool
         * a atomic
         * A read-only atomic
         * p pointer to memory
         * v value
         * m memory model
         */
        [ATOMIC_ID(TOK___atomic_store)] = "avm?",
        [ATOMIC_ID(TOK___atomic_load)] = "Amv",
        [ATOMIC_ID(TOK___atomic_exchange)] = "avmv",
        [ATOMIC_ID(TOK___atomic_compare_exchange)] = "apvbmmb",
        [ATOMIC_ID(TOK___atomic_fetch_add)] = "avmv",
        [ATOMIC_ID(TOK___atomic_fetch_sub)] = "avmv",
        [ATOMIC_ID(TOK___atomic_fetch_or)] = "avmv",
        [ATOMIC_ID(TOK___atomic_fetch_xor)] = "avmv",
        [ATOMIC_ID(TOK___atomic_fetch_and)] = "avmv",
    };
    const char *template = templates[ATOMIC_ID(atok)];
    const size_t argc = (strlen(template) - 1);

    next(s1);

    mode = 0; /* pacify compiler */
    vpush_helper_func(s1, atok);
    call = s1->vtop;

    skip(s1, '(');
    if ((*template != 'a') && (*template != 'A'))
        expect_arg(s1, "pointer to atomic", 0);
    for (arg = 0; arg < argc; ++arg) {
        expr_eq(s1);

        switch (template[arg]) {
        case '?':
            /* void makes sense only for return value. */
            if (arg != (argc - 1))
                tcc_error(s1, "illegal atomic built-in template");
            break;

        case 'b':
            break;

        case 'a':
        case 'A':
            if ((s1->vtop->type.t & VT_BTYPE) != VT_PTR)
                expect_arg(s1, "pointer to atomic value", arg);
            memcpy(&atom, pointed_type(&s1->vtop->type), sizeof(CType));
            if (!(atom.t & VT_ATOMIC))
                expect_arg(s1, "qualified pointer to atomic value", arg);
            if ((template[arg] == 'a') && (atom.t & VT_CONSTANT))
                expect_arg(s1, "pointer to writable atomic", arg);
            switch (btype_size(atom.t & VT_BTYPE)) {
            case 8: mode = 4; break;
            case 4: mode = 3; break;
            case 2: mode = 2; break;
            case 1: mode = 1; break;
            default: tcc_error(s1, "only integer-sized types are supported");
            }
            break;

        case 'p':
            if (((s1->vtop->type.t & VT_BTYPE) != VT_PTR)
                || !is_compatible_unqualified_types(&atom, pointed_type(&s1->vtop->type)))
                expect_arg(s1, "pointer to compatible type", arg);
            break;

        case 'v':
            if (!is_integer_btype(s1->vtop->type.t & VT_BTYPE))
                expect_arg(s1, "integer type", arg);
            break;

        case 'm':
            if (!is_memory_model(s1->vtop))
                expect_arg(s1, "memory model", arg);
            s1->vtop->type.t &= ~VT_MEMMODEL;
            break;

        default:
            tcc_error(s1, "unknown parameter type");
        }
        if (s1->tok == ')')
            break;
        skip(s1, ',');
    }
    if (arg < (argc - 1))
        expect(s1, "more parameters");
    if (arg > (argc - 1))
        expect(s1, "less parameters");
    skip(s1, ')');

    call->sym = external_helper_sym(s1, atok + mode);
    gfunc_call(s1, argc);
    vpushi(s1, 0);

    switch (template[argc]) {
    case 'b': PUT_R_RET(s1->vtop, VT_BOOL); break;
    case 'v': PUT_R_RET(s1->vtop, atom.t); break;
    case 'p': PUT_R_RET(s1->vtop, VT_SIZE_T); break;
    case '?': PUT_R_RET(s1->vtop, VT_VOID); break;
    default: tcc_error(s1, "incorrect atomic template");
    }
}

ST_FUNC void unary(TCCState *s1)
{
    int n, t, align, size, r, sizeof_caller;
    CType type;
    Sym *s;
    AttributeDef ad;

    /* generate line number info */
    if (s1->debug_modes)
        tcc_debug_line(s1), tcc_tcov_check_line (s1, 1);

    sizeof_caller = s1->in_sizeof;
    s1->in_sizeof = 0;
    type.ref = NULL;
    /* XXX: GCC 2.95.3 does not generate a table although it should be
       better here */
 tok_next:
    switch(s1->tok) {
    case TOK_EXTENSION:
        next(s1);
        goto tok_next;
    case TOK_LCHAR:
#ifdef TCC_TARGET_PE
        t = VT_SHORT|VT_UNSIGNED;
        goto push_tokc;
#endif
    case TOK_CINT:
    case TOK_CCHAR: 
	t = VT_INT;
 push_tokc:
	type.t = t;
	vsetc(s1, &type, VT_CONST, &s1->tokc);
        next(s1);
        break;
    case TOK_CUINT:
        t = VT_INT | VT_UNSIGNED;
        goto push_tokc;
    case TOK_CLLONG:
        t = VT_LLONG;
	goto push_tokc;
    case TOK_CULLONG:
        t = VT_LLONG | VT_UNSIGNED;
	goto push_tokc;
    case TOK_CFLOAT:
        t = VT_FLOAT;
	goto push_tokc;
    case TOK_CDOUBLE:
        t = VT_DOUBLE;
	goto push_tokc;
    case TOK_CLDOUBLE:
        t = VT_LDOUBLE;
	goto push_tokc;
    case TOK_CLONG:
        t = (LONG_SIZE == 8 ? VT_LLONG : VT_INT) | VT_LONG;
	goto push_tokc;
    case TOK_CULONG:
        t = (LONG_SIZE == 8 ? VT_LLONG : VT_INT) | VT_LONG | VT_UNSIGNED;
	goto push_tokc;
    case TOK___FUNCTION__:
        if (!gnu_ext)
            goto tok_identifier;
        /* fall thru */
    case TOK___FUNC__:
        {
            Section *sec;
            void *ptr;
            int len;
            /* special function name identifier */
            len = strlen(s1->funcname) + 1;
            /* generate char[len] type */
            type.t = VT_BYTE;
            if (s1->warn_write_strings)
                type.t |= VT_CONSTANT;
            mk_pointer(s1, &type);
            type.t |= VT_ARRAY;
            type.ref->c = len;
            sec = rodata_section;
            vpush_ref(s1, &type, sec, sec->data_offset, len);
            if (!NODATA_WANTED) {
                ptr = section_ptr_add(sec, len);
                memcpy(ptr, s1->funcname, len);
            }
            next(s1);
        }
        break;
    case TOK_LSTR:
#ifdef TCC_TARGET_PE
        t = VT_SHORT | VT_UNSIGNED;
#else
        t = VT_INT;
#endif
        goto str_init;
    case TOK_STR:
        /* string parsing */
        t = VT_BYTE;
        if (s1->char_is_unsigned)
            t = VT_BYTE | VT_UNSIGNED;
    str_init:
        if (s1->warn_write_strings)
            t |= VT_CONSTANT;
        type.t = t;
        mk_pointer(s1, &type);
        type.t |= VT_ARRAY;
        memset(&ad, 0, sizeof(AttributeDef));
        ad.section = rodata_section;
        decl_initializer_alloc(s1, &type, &ad, VT_CONST, 2, 0, 0);
        break;
    case '(':
        next(s1);
        /* cast ? */
        if (parse_btype(s1, &type, &ad)) {
            type_decl(s1, &type, &ad, &n, TYPE_ABSTRACT);
            skip(s1, ')');
            /* check ISOC99 compound literal */
            if (s1->tok == '{') {
                    /* data is allocated locally by default */
                if (s1->global_expr)
                    r = VT_CONST;
                else
                    r = VT_LOCAL;
                /* all except arrays are lvalues */
                if (!(type.t & VT_ARRAY))
                    r |= VT_LVAL;
                memset(&ad, 0, sizeof(AttributeDef));
                decl_initializer_alloc(s1, &type, &ad, r, 1, 0, 0);
            } else {
                if (sizeof_caller) {
                    vpush(s1, &type);
                    return;
                }
                unary(s1);
                gen_cast(s1, &type);
            }
        } else if (s1->tok == '{') {
	    int saved_nocode_wanted = s1->nocode_wanted;
            if (s1->const_wanted && !(s1->nocode_wanted & unevalmask))
                expect(s1, "constant");
            if (0 == s1->local_scope)
                tcc_error(s1, "statement expression outside of function");
            /* save all registers */
            save_regs(s1, 0);
            /* statement expression : we do not accept break/continue
               inside as GCC does.  We do retain the nocode_wanted state,
	       as statement expressions can't ever be entered from the
	       outside, so any reactivation of code emission (from labels
	       or loop heads) can be disabled again after the end of it. */
            block(s1, 1);
	    s1->nocode_wanted = saved_nocode_wanted;
            skip(s1, ')');
        } else {
            gexpr(s1);
            skip(s1, ')');
        }
        break;
    case '*':
        next(s1);
        unary(s1);
        indir(s1);
        break;
    case '&':
        next(s1);
        unary(s1);
        /* functions names must be treated as function pointers,
           except for unary '&' and sizeof. Since we consider that
           functions are not lvalues, we only have to handle it
           there and in function calls. */
        /* arrays can also be used although they are not lvalues */
        if ((s1->vtop->type.t & VT_BTYPE) != VT_FUNC &&
            !(s1->vtop->type.t & VT_ARRAY))
            test_lvalue(s1);
        if (s1->vtop->sym)
          s1->vtop->sym->a.addrtaken = 1;
        mk_pointer(s1, &s1->vtop->type);
        gaddrof(s1);
        break;
    case '!':
        next(s1);
        unary(s1);
        gen_test_zero(s1, TOK_EQ);
        break;
    case '~':
        next(s1);
        unary(s1);
        vpushi(s1, -1);
        gen_op(s1, '^');
        break;
    case '+':
        next(s1);
        unary(s1);
        if ((s1->vtop->type.t & VT_BTYPE) == VT_PTR)
            tcc_error(s1, "pointer not accepted for unary plus");
        /* In order to force cast, we add zero, except for floating point
	   where we really need an noop (otherwise -0.0 will be transformed
	   into +0.0).  */
	if (!is_float(s1->vtop->type.t)) {
	    vpushi(s1, 0);
	    gen_op(s1, '+');
	}
        break;
    case TOK_SIZEOF:
    case TOK_ALIGNOF1:
    case TOK_ALIGNOF2:
    case TOK_ALIGNOF3:
        t = s1->tok;
        next(s1);
        s1->in_sizeof++;
        expr_type(s1, &type, unary); /* Perform a in_sizeof = 0; */
        s = NULL;
        if (s1->vtop[1].r & VT_SYM)
            s = s1->vtop[1].sym; /* hack: accessing previous s1->vtop */
        size = type_size(s1, &type, &align);
        if (s && s->a.aligned)
            align = 1 << (s->a.aligned - 1);
        if (t == TOK_SIZEOF) {
            if (!(type.t & VT_VLA)) {
                if (size < 0)
                    tcc_error(s1, "sizeof applied to an incomplete type");
                vpushs(s1, size);
            } else {
                vla_runtime_type_size(s1, &type, &align);
            }
        } else {
            vpushs(s1, align);
        }
        s1->vtop->type.t |= VT_UNSIGNED;
        break;

    case TOK_builtin_expect:
	/* __builtin_expect is a no-op for now */
	parse_builtin_params(s1, 0, "ee");
	vpop(s1);
        break;
    case TOK_builtin_types_compatible_p:
	parse_builtin_params(s1, 0, "tt");
	s1->vtop[-1].type.t &= ~(VT_CONSTANT | VT_VOLATILE);
	s1->vtop[0].type.t &= ~(VT_CONSTANT | VT_VOLATILE);
	n = is_compatible_types(&s1->vtop[-1].type, &s1->vtop[0].type);
	s1->vtop -= 2;
	vpushi(s1, n);
        break;
    case TOK_builtin_choose_expr:
	{
	    int64_t c;
	    next(s1);
	    skip(s1, '(');
	    c = expr_const64(s1);
	    skip(s1, ',');
	    if (!c) {
		s1->nocode_wanted++;
	    }
	    expr_eq(s1);
	    if (!c) {
		vpop(s1);
		s1->nocode_wanted--;
	    }
	    skip(s1, ',');
	    if (c) {
		s1->nocode_wanted++;
	    }
	    expr_eq(s1);
	    if (c) {
		vpop(s1);
		s1->nocode_wanted--;
	    }
	    skip(s1, ')');
	}
        break;
    case TOK_builtin_constant_p:
	parse_builtin_params(s1, 1, "e");
	n = (s1->vtop->r & (VT_VALMASK | VT_LVAL)) == VT_CONST &&
            !((s1->vtop->r & VT_SYM) && s1->vtop->sym->a.addrtaken);
	s1->vtop--;
	vpushi(s1, n);
        break;
    case TOK_builtin_frame_address:
    case TOK_builtin_return_address:
        {
            int tok1 = s1->tok;
            int level;
            next(s1);
            skip(s1, '(');
            if (s1->tok != TOK_CINT) {
                tcc_error(s1, "%s only takes positive integers",
                          tok1 == TOK_builtin_return_address ?
                          "__builtin_return_address" :
                          "__builtin_frame_address");
            }
            level = (uint32_t)s1->tokc.i;
            next(s1);
            skip(s1, ')');
            type.t = VT_VOID;
            mk_pointer(s1, &type);
            vset(s1, &type, VT_LOCAL, 0);       /* local frame */
            while (level--) {
#ifdef TCC_TARGET_RISCV64
                vpushi(s1, 2*PTR_SIZE);
                gen_op(s1, '-');
#endif
                mk_pointer(s1, &s1->vtop->type);
                indir(s1);                    /* -> parent frame */
            }
            if (tok1 == TOK_builtin_return_address) {
                // assume return address is just above frame pointer on stack
#ifdef TCC_TARGET_ARM
                vpushi(s1, 2*PTR_SIZE);
                gen_op(s1, '+');
#elif defined TCC_TARGET_RISCV64
                vpushi(s1, PTR_SIZE);
                gen_op(s1, '-');
#else
                vpushi(s1, PTR_SIZE);
                gen_op(s1, '+');
#endif
                mk_pointer(s1, &s1->vtop->type);
                indir(s1);
            }
        }
        break;
#ifdef TCC_TARGET_RISCV64
    case TOK_builtin_va_start:
        parse_builtin_params(s1, 0, "ee");
        r = s1->vtop->r & VT_VALMASK;
        if (r == VT_LLOCAL)
            r = VT_LOCAL;
        if (r != VT_LOCAL)
            tcc_error(s1, "__builtin_va_start expects a local variable");
        gen_va_start(s1);
	vstore(s1);
        break;
#endif
#ifdef TCC_TARGET_X86_64
#ifdef TCC_TARGET_PE
    case TOK_builtin_va_start:
	parse_builtin_params(s1, 0, "ee");
        r = s1->vtop->r & VT_VALMASK;
        if (r == VT_LLOCAL)
            r = VT_LOCAL;
        if (r != VT_LOCAL)
            tcc_error(s1, "__builtin_va_start expects a local variable");
        s1->vtop->r = r;
	s1->vtop->type = s1->char_pointer_type;
	s1->vtop->c.i += 8;
	vstore(s1);
        break;
#else
    case TOK_builtin_va_arg_types:
	parse_builtin_params(s1, 0, "t");
	vpushi(s1, classify_x86_64_va_arg(s1, &s1->vtop->type));
	vswap(s1);
	vpop(s1);
	break;
#endif
#endif

#ifdef TCC_TARGET_ARM64
    case TOK_builtin_va_start: {
	parse_builtin_params(s1, 0, "ee");
        //xx check types
        gen_va_start(s1);
        vpushi(s1, 0);
        s1->vtop->type.t = VT_VOID;
        break;
    }
    case TOK_builtin_va_arg: {
	parse_builtin_params(s1, 0, "et");
	type = s1->vtop->type;
	vpop(s1);
        //xx check types
        gen_va_arg(s1, &type);
        s1->vtop->type = type;
        break;
    }
    case TOK___arm64_clear_cache: {
	parse_builtin_params(s1, 0, "ee");
        gen_clear_cache(s1);
        vpushi(s1, 0);
        s1->vtop->type.t = VT_VOID;
        break;
    }
#endif

    /* atomic operations */
    case TOK___atomic_store:
    case TOK___atomic_load:
    case TOK___atomic_exchange:
    case TOK___atomic_compare_exchange:
    case TOK___atomic_fetch_add:
    case TOK___atomic_fetch_sub:
    case TOK___atomic_fetch_or:
    case TOK___atomic_fetch_xor:
    case TOK___atomic_fetch_and:
        parse_atomic(s1, s1->tok);
        break;

    /* pre operations */
    case TOK_INC:
    case TOK_DEC:
        t = s1->tok;
        next(s1);
        unary(s1);
        inc(s1, 0, t);
        break;
    case '-':
        next(s1);
        unary(s1);
	if (is_float(s1->vtop->type.t)) {
            gen_opif(s1, TOK_NEG);
	} else {
            vpushi(s1, 0);
            vswap(s1);
            gen_op(s1, '-');
        }
        break;
    case TOK_LAND:
        if (!gnu_ext)
            goto tok_identifier;
        next(s1);
        /* allow to take the address of a label */
        if (s1->tok < TOK_UIDENT)
            expect(s1, "label identifier");
        s = label_find(s1, s1->tok);
        if (!s) {
            s = label_push(s1, &s1->global_label_stack, s1->tok, LABEL_FORWARD);
        } else {
            if (s->r == LABEL_DECLARED)
                s->r = LABEL_FORWARD;
        }
        if (!s->type.t) {
            s->type.t = VT_VOID;
            mk_pointer(s1, &s->type);
            s->type.t |= VT_STATIC;
        }
        vpushsym(s1, &s->type, s);
        next(s1);
        break;

    case TOK_GENERIC:
    {
	CType controlling_type;
	int has_default = 0;
	int has_match = 0;
	int learn = 0;
	TokenString *str = NULL;
	int saved_const_wanted = s1->const_wanted;

	next(s1);
	skip(s1, '(');
	s1->const_wanted = 0;
	expr_type(s1, &controlling_type, expr_eq);
	controlling_type.t &= ~(VT_CONSTANT | VT_VOLATILE | VT_ARRAY);
	if ((controlling_type.t & VT_BTYPE) == VT_FUNC)
	  mk_pointer(s1, &controlling_type);
	s1->const_wanted = saved_const_wanted;
	for (;;) {
	    learn = 0;
	    skip(s1, ',');
	    if (s1->tok == TOK_DEFAULT) {
		if (has_default)
		    tcc_error(s1, "too many 'default'");
		has_default = 1;
		if (!has_match)
		    learn = 1;
		next(s1);
	    } else {
	        AttributeDef ad_tmp;
		int itmp;
	        CType cur_type;

                s1->in_generic++;
		parse_btype(s1, &cur_type, &ad_tmp);
                s1->in_generic--;

		type_decl(s1, &cur_type, &ad_tmp, &itmp, TYPE_ABSTRACT);
		if (compare_types(&controlling_type, &cur_type, 0)) {
		    if (has_match) {
		      tcc_error(s1, "type match twice");
		    }
		    has_match = 1;
		    learn = 1;
		}
	    }
	    skip(s1, ':');
	    if (learn) {
		if (str)
		    tok_str_free(s1, str);
		skip_or_save_block(s1, &str);
	    } else {
		skip_or_save_block(s1, NULL);
	    }
	    if (s1->tok == ')')
		break;
	}
	if (!str) {
	    char buf[60];
	    type_to_str(s1, buf, sizeof buf, &controlling_type, NULL);
	    tcc_error(s1, "type '%s' does not match any association", buf);
	}
	begin_macro(s1, str, 1);
	next(s1);
	expr_eq(s1);
	if (s1->tok != TOK_EOF)
	    expect(s1, ",");
	end_macro(s1);
        next(s1);
	break;
    }
    // special qnan , snan and infinity values
    case TOK___NAN__:
        n = 0x7fc00000;
special_math_val:
	vpushi(s1, n);
	s1->vtop->type.t = VT_FLOAT;
        next(s1);
        break;
    case TOK___SNAN__:
	n = 0x7f800001;
	goto special_math_val;
    case TOK___INF__:
	n = 0x7f800000;
	goto special_math_val;

    default:
    tok_identifier:
        t = s1->tok;
        next(s1);
        if (t < TOK_UIDENT)
            expect(s1, "identifier");
        s = sym_find(s1, t);
        if (!s || IS_ASM_SYM(s)) {
            const char *name = get_tok_str(s1, t, NULL);
            if (s1->tok != '(')
                tcc_error(s1, "'%s' undeclared", name);
            /* for simple function calls, we tolerate undeclared
               external reference to int() function */
            if (s1->warn_implicit_function_declaration
#ifdef TCC_TARGET_PE
                /* people must be warned about using undeclared WINAPI functions
                   (which usually start with uppercase letter) */
                || (name[0] >= 'A' && name[0] <= 'Z')
#endif
            )
                tcc_warning(s1, "implicit declaration of function '%s'", name);
            s = external_global_sym(s1, t, &s1->func_old_type);
        }

        r = s->r;
        /* A symbol that has a register is a local register variable,
           which starts out as VT_LOCAL value.  */
        if ((r & VT_VALMASK) < VT_CONST)
            r = (r & ~VT_VALMASK) | VT_LOCAL;

        vset(s1, &s->type, r, s->c);
        /* Point to s as backpointer (even without r&VT_SYM).
	   Will be used by at least the x86 inline asm parser for
	   regvars.  */
	s1->vtop->sym = s;

        if (r & VT_SYM) {
            s1->vtop->c.i = 0;
        } else if (r == VT_CONST && IS_ENUM_VAL(s->type.t)) {
            s1->vtop->c.i = s->enum_val;
        }
        break;
    }
    
    /* post operations */
    while (1) {
        if (s1->tok == TOK_INC || s1->tok == TOK_DEC) {
            inc(s1, 1, s1->tok);
            next(s1);
        } else if (s1->tok == '.' || s1->tok == TOK_ARROW || s1->tok == TOK_CDOUBLE) {
            int qualifiers, cumofs = 0;
            /* field */ 
            if (s1->tok == TOK_ARROW) 
                indir(s1);
            qualifiers = s1->vtop->type.t & (VT_CONSTANT | VT_VOLATILE);
            test_lvalue(s1);
            gaddrof(s1);
            /* expect pointer on structure */
            if ((s1->vtop->type.t & VT_BTYPE) != VT_STRUCT)
                expect(s1, "struct or union");
            if (s1->tok == TOK_CDOUBLE)
                expect(s1, "field name");
            next(s1);
            if (s1->tok == TOK_CINT || s1->tok == TOK_CUINT)
                expect(s1, "field name");
	    s = find_field(&s1->vtop->type, s1->tok, &cumofs);
            if (!s)
                tcc_error(s1, "field not found: %s",  get_tok_str(s1, s1->tok & ~SYM_FIELD, &s1->tokc));
            /* add field offset to pointer */
            s1->vtop->type = s1->char_pointer_type; /* change type to 'char *' */
            vpushi(s1, cumofs + s->c);
            gen_op(s1, '+');
            /* change type to field type, and set to lvalue */
            s1->vtop->type = s->type;
            s1->vtop->type.t |= qualifiers;
            /* an array is never an lvalue */
            if (!(s1->vtop->type.t & VT_ARRAY)) {
                s1->vtop->r |= VT_LVAL;
#ifdef CONFIG_TCC_BCHECK
                /* if bound checking, the referenced pointer must be checked */
                if (s1->do_bounds_check)
                    s1->vtop->r |= VT_MUSTBOUND;
#endif
            }
            next(s1);
        } else if (s1->tok == '[') {
            next(s1);
            gexpr(s1);
            gen_op(s1, '+');
            indir(s1);
            skip(s1, ']');
        } else if (s1->tok == '(') {
            SValue ret;
            Sym *sa;
            int nb_args, ret_nregs, ret_align, regsize, variadic;

            /* function call  */
            if ((s1->vtop->type.t & VT_BTYPE) != VT_FUNC) {
                /* pointer test (no array accepted) */
                if ((s1->vtop->type.t & (VT_BTYPE | VT_ARRAY)) == VT_PTR) {
                    s1->vtop->type = *pointed_type(&s1->vtop->type);
                    if ((s1->vtop->type.t & VT_BTYPE) != VT_FUNC)
                        goto error_func;
                } else {
                error_func:
                    expect(s1, "function pointer");
                }
            } else {
                s1->vtop->r &= ~VT_LVAL; /* no lvalue */
            }
            /* get return type */
            s = s1->vtop->type.ref;
            next(s1);
            sa = s->next; /* first parameter */
            nb_args = regsize = 0;
            ret.r2 = VT_CONST;
            /* compute first implicit argument if a structure is returned */
            if ((s->type.t & VT_BTYPE) == VT_STRUCT) {
                variadic = (s->f.func_type == FUNC_ELLIPSIS);
                ret_nregs = gfunc_sret(s1, &s->type, variadic, &ret.type,
                                       &ret_align, &regsize);
                if (ret_nregs <= 0) {
                    /* get some space for the returned structure */
                    size = type_size(s1, &s->type, &align);
#ifdef TCC_TARGET_ARM64
                /* On arm64, a small struct is return in registers.
                   It is much easier to write it to memory if we know
                   that we are allowed to write some extra bytes, so
                   round the allocated space up to a power of 2: */
                if (size < 16)
                    while (size & (size - 1))
                        size = (size | (size - 1)) + 1;
#endif
                    s1->loc = (s1->loc - size) & -align;
                    ret.type = s->type;
                    ret.r = VT_LOCAL | VT_LVAL;
                    /* pass it as 'int' to avoid structure arg passing
                       problems */
                    vseti(s1, VT_LOCAL, s1->loc);
#ifdef CONFIG_TCC_BCHECK
                    if (s1->do_bounds_check)
                        --s1->loc;
#endif
                    ret.c = s1->vtop->c;
                    if (ret_nregs < 0)
                      s1->vtop--;
                    else
                      nb_args++;
                }
            } else {
                ret_nregs = 1;
                ret.type = s->type;
            }

            if (ret_nregs > 0) {
                /* return in register */
                ret.c.i = 0;
                PUT_R_RET(&ret, ret.type.t);
            }
            if (s1->tok != ')') {
                for(;;) {
                    expr_eq(s1);
                    gfunc_param_typed(s1, s, sa);
                    nb_args++;
                    if (sa)
                        sa = sa->next;
                    if (s1->tok == ')')
                        break;
                    skip(s1, ',');
                }
            }
            if (sa)
                tcc_error(s1, "too few arguments to function");
            skip(s1, ')');
            gfunc_call(s1, nb_args);

            if (ret_nregs < 0) {
                vsetc(s1, &ret.type, ret.r, &ret.c);
#ifdef TCC_TARGET_RISCV64
                arch_transfer_ret_regs(s1, 1);
#endif
            } else {
                /* return value */
                for (r = ret.r + ret_nregs + !ret_nregs; r-- > ret.r;) {
                    vsetc(s1, &ret.type, r, &ret.c);
                    s1->vtop->r2 = ret.r2; /* Loop only happens when r2 is VT_CONST */
                }

                /* handle packed struct return */
                if (((s->type.t & VT_BTYPE) == VT_STRUCT) && ret_nregs) {
                    int addr, offset;

                    size = type_size(s1, &s->type, &align);
                    /* We're writing whole regs often, make sure there's enough
                       space.  Assume register size is power of 2.  */
                    if (regsize > align)
                      align = regsize;
                    s1->loc = (s1->loc - size) & -align;
                    addr = s1->loc;
                    offset = 0;
                    for (;;) {
                        vset(s1, &ret.type, VT_LOCAL | VT_LVAL, addr + offset);
                        vswap(s1);
                        vstore(s1);
                        s1->vtop--;
                        if (--ret_nregs == 0)
                          break;
                        offset += regsize;
                    }
                    vset(s1, &s->type, VT_LOCAL | VT_LVAL, addr);
                }

                /* Promote char/short return values. This is matters only
                   for calling function that were not compiled by TCC and
                   only on some architectures.  For those where it doesn't
                   matter we expect things to be already promoted to int,
                   but not larger.  */
                t = s->type.t & VT_BTYPE;
                if (t == VT_BYTE || t == VT_SHORT || t == VT_BOOL) {
#ifdef PROMOTE_RET
                    s1->vtop->r |= BFVAL(VT_MUSTCAST, 1);
#else
                    s1->vtop->type.t = VT_INT;
#endif
                }
            }
            if (s->f.func_noreturn) {
                if (s1->debug_modes)
	            tcc_tcov_block_end (s1, tcov_data.line);
                CODE_OFF(s1);
	    }
        } else {
            break;
        }
    }
}

#ifndef precedence_parser /* original top-down parser */

static void expr_prod(void)
{
    int t;

    unary(s1);
    while ((t = s1->tok) == '*' || t == '/' || t == '%') {
        next(s1);
        unary(s1);
        gen_op(t);
    }
}

static void expr_sum(void)
{
    int t;

    expr_prod();
    while ((t = s1->tok) == '+' || t == '-') {
        next(s1);
        expr_prod();
        gen_op(t);
    }
}

static void expr_shift(void)
{
    int t;

    expr_sum();
    while ((t = s1->tok) == TOK_SHL || t == TOK_SAR) {
        next(s1);
        expr_sum();
        gen_op(t);
    }
}

static void expr_cmp(void)
{
    int t;

    expr_shift();
    while (((t = s1->tok) >= TOK_ULE && t <= TOK_GT) ||
           t == TOK_ULT || t == TOK_UGE) {
        next(s1);
        expr_shift();
        gen_op(t);
    }
}

static void expr_cmpeq(void)
{
    int t;

    expr_cmp();
    while ((t = tok) == TOK_EQ || t == TOK_NE) {
        next(s1);
        expr_cmp();
        gen_op(t);
    }
}

static void expr_and(void)
{
    expr_cmpeq();
    while (s1->tok == '&') {
        next(s1);
        expr_cmpeq();
        gen_op(s1, '&');
    }
}

static void expr_xor(void)
{
    expr_and();
    while (s1->tok == '^') {
        next(s1);
        expr_and();
        gen_op(s1, '^');
    }
}

static void expr_or(void)
{
    expr_xor();
    while (s1->tok == '|') {
        next(s1);
        expr_xor();
        gen_op(s1, '|');
    }
}

static void expr_landor(TCCState *s1, int op);

static void expr_land(TCCState *s1)
{
    expr_or();
    if (s1->tok == TOK_LAND)
        expr_landor(s1, tok);
}

static void expr_lor(TCCState *s1)
{
    expr_land(s1);
    if (s1->tok == TOK_LOR)
        expr_landor(s1, tok);
}

# define expr_landor_next(s1, op) op == TOK_LAND ? expr_or(s1) : expr_land(s1)
#else /* defined precedence_parser */
# define expr_landor_next(s1, op) unary(s1), expr_infix(s1, precedence(op) + 1)
# define expr_lor(s1) unary(s1), expr_infix(s1, 1)

static int precedence(int tok)
{
    switch (tok) {
        case TOK_LOR: return 1;
        case TOK_LAND: return 2;
	case '|': return 3;
	case '^': return 4;
	case '&': return 5;
	case TOK_EQ: case TOK_NE: return 6;
 relat: case TOK_ULT: case TOK_UGE: return 7;
	case TOK_SHL: case TOK_SAR: return 8;
	case '+': case '-': return 9;
	case '*': case '/': case '%': return 10;
	default:
	    if (tok >= TOK_ULE && tok <= TOK_GT)
	        goto relat;
	    return 0;
    }
}
static unsigned char prec[256];
static void init_prec(void)
{
    int i;
    for (i = 0; i < 256; i++)
	prec[i] = precedence(i);
}
#define precedence(i) ((unsigned)i < 256 ? prec[i] : 0)

static void expr_landor(TCCState *s1, int op);

static void expr_infix(TCCState *s1, int p)
{
    int t = s1->tok, p2;
    while ((p2 = precedence(t)) >= p) {
        if (t == TOK_LOR || t == TOK_LAND) {
            expr_landor(s1, t);
        } else {
            next(s1);
            unary(s1);
            if (precedence(s1->tok) > p2)
              expr_infix(s1, p2 + 1);
            gen_op(s1, t);
        }
        t = s1->tok;
    }
}
#endif

/* Assuming s1->vtop is a value used in a conditional context
   (i.e. compared with zero) return 0 if it's false, 1 if
   true and -1 if it can't be statically determined.  */
static int condition_3way(TCCState *s1)
{
    int c = -1;
    if ((s1->vtop->r & (VT_VALMASK | VT_LVAL)) == VT_CONST &&
	(!(s1->vtop->r & VT_SYM) || !s1->vtop->sym->a.weak)) {
	vdup(s1);
        gen_cast_s(s1, VT_BOOL);
	c = s1->vtop->c.i;
	vpop(s1);
    }
    return c;
}

static void expr_landor(TCCState *s1, int op)
{
    int t = 0, cc = 1, f = 0, i = op == TOK_LAND, c;
    for(;;) {
        c = f ? i : condition_3way(s1);
        if (c < 0)
            save_regs(s1, 1), cc = 0;
        else if (c != i)
            s1->nocode_wanted++, f = 1;
        if (s1->tok != op)
            break;
        if (c < 0)
            t = gvtst(s1, i, t);
        else
            vpop(s1);
        next(s1);
        expr_landor_next(s1, op);
    }
    if (cc || f) {
        vpop(s1);
        vpushi(s1, i ^ f);
        gsym(s1, t);
        s1->nocode_wanted -= f;
    } else {
        gvtst_set(s1, i, t);
    }
}

static int is_cond_bool(SValue *sv)
{
    if ((sv->r & (VT_VALMASK | VT_LVAL | VT_SYM)) == VT_CONST
        && (sv->type.t & VT_BTYPE) == VT_INT)
        return (unsigned)sv->c.i < 2;
    if (sv->r == VT_CMP)
        return 1;
    return 0;
}

static void expr_cond(TCCState *s1)
{
    int tt, u, r1, r2, rc, t1, t2, islv, c, g;
    SValue sv;
    CType type;
    int ncw_prev;

    expr_lor(s1);
    if (s1->tok == '?') {
        next(s1);
	c = condition_3way(s1);
        g = (s1->tok == ':' && gnu_ext);
        tt = 0;
        if (!g) {
            if (c < 0) {
                save_regs(s1, 1);
                tt = gvtst(s1, 1, 0);
            } else {
                vpop(s1);
            }
        } else if (c < 0) {
            /* needed to avoid having different registers saved in
               each branch */
            save_regs(s1, 1);
            gv_dup(s1);
            tt = gvtst(s1, 0, 0);
        }

        ncw_prev = s1->nocode_wanted;
        if (c == 0)
          s1->nocode_wanted++;
        if (!g)
          gexpr(s1);

        if ((s1->vtop->type.t & VT_BTYPE) == VT_FUNC)
          mk_pointer(s1, &s1->vtop->type);
        sv = *s1->vtop; /* save value to handle it later */
        s1->vtop--; /* no vpop so that FP stack is not flushed */

        if (g) {
            u = tt;
        } else if (c < 0) {
            u = gjmp(s1, 0);
            gsym(s1, tt);
        } else
          u = 0;

        s1->nocode_wanted = ncw_prev;
        if (c == 1)
          s1->nocode_wanted++;
        skip(s1, ':');
        expr_cond(s1);

        if (c < 0 && is_cond_bool(s1->vtop) && is_cond_bool(&sv)) {
            /* optimize "if (f ? a > b : c || d) ..." for example, where normally
               "a < b" and "c || d" would be forced to "(int)0/1" first, whereas
               this code jumps directly to the if's then/else branches. */
            t1 = gvtst(s1, 0, 0);
            t2 = gjmp(s1, 0);
            gsym(s1, u);
            vpushv(s1, &sv);
            /* combine jump targets of 2nd op with VT_CMP of 1st op */
            gvtst_set(s1, 0, t1);
            gvtst_set(s1, 1, t2);
            s1->nocode_wanted = ncw_prev;
            //  tcc_warning(s1, "two conditions expr_cond");
            return;
        }

        if ((s1->vtop->type.t & VT_BTYPE) == VT_FUNC)
          mk_pointer(s1, &s1->vtop->type);

        /* cast operands to correct type according to ISOC rules */
        if (!combine_types(s1, &type, &sv, s1->vtop, '?'))
          type_incompatibility_error(s1, &sv.type, &s1->vtop->type,
            "type mismatch in conditional expression (have '%s' and '%s')");
        /* keep structs lvalue by transforming `(expr ? a : b)` to `*(expr ? &a : &b)` so
           that `(expr ? a : b).mem` does not error  with "lvalue expected" */
        islv = (s1->vtop->r & VT_LVAL) && (sv.r & VT_LVAL) && VT_STRUCT == (type.t & VT_BTYPE);

        /* now we convert second operand */
        if (c != 1) {
            gen_cast(s1, &type);
            if (islv) {
                mk_pointer(s1, &s1->vtop->type);
                gaddrof(s1);
            } else if (VT_STRUCT == (s1->vtop->type.t & VT_BTYPE))
              gaddrof(s1);
        }

        rc = RC_TYPE(type.t);
        /* for long longs, we use fixed registers to avoid having
           to handle a complicated move */
        if (USING_TWO_WORDS(type.t))
          rc = RC_RET(type.t);

        tt = r2 = 0;
        if (c < 0) {
            r2 = gv(s1, rc);
            tt = gjmp(s1, 0);
        }
        gsym(s1, u);
        s1->nocode_wanted = ncw_prev;

        /* this is horrible, but we must also convert first
           operand */
        if (c != 0) {
            *s1->vtop = sv;
            gen_cast(s1, &type);
            if (islv) {
                mk_pointer(s1, &s1->vtop->type);
                gaddrof(s1);
            } else if (VT_STRUCT == (s1->vtop->type.t & VT_BTYPE))
              gaddrof(s1);
        }

        if (c < 0) {
            r1 = gv(s1, rc);
            move_reg(s1, r2, r1, islv ? VT_PTR : type.t);
            s1->vtop->r = r2;
            gsym(s1, tt);
        }

        if (islv)
          indir(s1);
    }
}

static void expr_eq(TCCState *s1)
{
    int t;
    
    expr_cond(s1);
    if ((t = s1->tok) == '=' || TOK_ASSIGN(t)) {
        test_lvalue(s1);
        next(s1);
        if (t == '=') {
            expr_eq(s1);
        } else {
            vdup(s1);
            expr_eq(s1);
            gen_op(s1, TOK_ASSIGN_OP(t));
        }
        vstore(s1);
    }
}

ST_FUNC void gexpr(TCCState *s1)
{
    while (1) {
        expr_eq(s1);
        if (s1->tok != ',')
            break;
        vpop(s1);
        next(s1);
    }
}

/* parse a constant expression and return value in s1->vtop.  */
static void expr_const1(TCCState *s1)
{
    s1->const_wanted++;
    s1->nocode_wanted += unevalmask + 1;
    expr_cond(s1);
    s1->nocode_wanted -= unevalmask + 1;
    s1->const_wanted--;
}

/* parse an integer constant and return its value. */
static inline int64_t expr_const64(TCCState *s1)
{
    int64_t c;
    expr_const1(s1);
    if ((s1->vtop->r & (VT_VALMASK | VT_LVAL | VT_SYM)) != VT_CONST)
        expect(s1, "constant expression");
    c = s1->vtop->c.i;
    vpop(s1);
    return c;
}

/* parse an integer constant and return its value.
   Complain if it doesn't fit 32bit (signed or unsigned).  */
ST_FUNC int expr_const(TCCState *s1)
{
    int c;
    int64_t wc = expr_const64(s1);
    c = wc;
    if (c != wc && (unsigned)c != wc)
        tcc_error(s1, "constant exceeds 32 bit");
    return c;
}

/* ------------------------------------------------------------------------- */
/* return from function */

#ifndef TCC_TARGET_ARM64
static void gfunc_return(TCCState *s1, CType *func_type)
{
    if ((func_type->t & VT_BTYPE) == VT_STRUCT) {
        CType type, ret_type;
        int ret_align, ret_nregs, regsize;
        ret_nregs = gfunc_sret(s1, func_type, s1->func_var, &ret_type,
                               &ret_align, &regsize);
        if (ret_nregs < 0) {
#ifdef TCC_TARGET_RISCV64
            arch_transfer_ret_regs(s1, 0);
#endif
        } else if (0 == ret_nregs) {
            /* if returning structure, must copy it to implicit
               first pointer arg location */
            type = *func_type;
            mk_pointer(s1, &type);
            vset(s1, &type, VT_LOCAL | VT_LVAL, s1->func_vc);
            indir(s1);
            vswap(s1);
            /* copy structure value to pointer */
            vstore(s1);
        } else {
            /* returning structure packed into registers */
            int size, addr, align, rc;
            size = type_size(s1, func_type,&align);
            if ((s1->vtop->r != (VT_LOCAL | VT_LVAL) ||
                 (s1->vtop->c.i & (ret_align-1)))
                && (align & (ret_align-1))) {
                s1->loc = (s1->loc - size) & -ret_align;
                addr = s1->loc;
                type = *func_type;
                vset(s1, &type, VT_LOCAL | VT_LVAL, addr);
                vswap(s1);
                vstore(s1);
                vpop(s1);
                vset(s1, &ret_type, VT_LOCAL | VT_LVAL, addr);
            }
            s1->vtop->type = ret_type;
            rc = RC_RET(ret_type.t);
            if (ret_nregs == 1)
                gv(s1, rc);
            else {
                for (;;) {
                    vdup(s1);
                    gv(s1, rc);
                    vpop(s1);
                    if (--ret_nregs == 0)
                      break;
                    /* We assume that when a structure is returned in multiple
                       registers, their classes are consecutive values of the
                       suite s(n) = 2^n */
                    rc <<= 1;
                    s1->vtop->c.i += regsize;
                }
            }
        }
    } else {
        gv(s1, RC_RET(func_type->t));
    }
    s1->vtop--; /* NOT vpop(s1) because on x86 it would flush the fp stack */
}
#endif

static void check_func_return(TCCState *s1)
{
    if ((s1->func_vt.t & VT_BTYPE) == VT_VOID)
        return;
    if (!strcmp (s1->funcname, "main")
        && (s1->func_vt.t & VT_BTYPE) == VT_INT) {
        /* main returns 0 by default */
        vpushi(s1, 0);
        gen_assign_cast(s1, &s1->func_vt);
        gfunc_return(s1, &s1->func_vt);
    } else {
        tcc_warning(s1, "function might return no value: '%s'", s1->funcname);
    }
}

/* ------------------------------------------------------------------------- */
/* switch/case */

static int case_cmpi(const void *pa, const void *pb)
{
    int64_t a = (*(struct case_t**) pa)->v1;
    int64_t b = (*(struct case_t**) pb)->v1;
    return a < b ? -1 : a > b;
}

static int case_cmpu(const void *pa, const void *pb)
{
    uint64_t a = (uint64_t)(*(struct case_t**) pa)->v1;
    uint64_t b = (uint64_t)(*(struct case_t**) pb)->v1;
    return a < b ? -1 : a > b;
}

static void gtst_addr(TCCState *s1, int t, int a)
{
    gsym_addr(s1, gvtst(s1, 0, t), a);
}

static void gcase(TCCState *s1, struct case_t **base, int len, int *bsym)
{
    struct case_t *p;
    int e;
    int ll = (s1->vtop->type.t & VT_BTYPE) == VT_LLONG;
    while (len > 8) {
        /* binary search */
        p = base[len/2];
        vdup(s1);
	if (ll)
	    vpushll(s1, p->v2);
	else
	    vpushi(s1, p->v2);
        gen_op(s1, TOK_LE);
        e = gvtst(s1, 1, 0);
        vdup(s1);
	if (ll)
	    vpushll(s1, p->v1);
	else
	    vpushi(s1, p->v1);
        gen_op(s1, TOK_GE);
        gtst_addr(s1, 0, p->sym); /* v1 <= x <= v2 */
        /* x < v1 */
        gcase(s1, base, len/2, bsym);
        /* x > v2 */
        gsym(s1, e);
        e = len/2 + 1;
        base += e; len -= e;
    }
    /* linear scan */
    while (len--) {
        p = *base++;
        vdup(s1);
	if (ll)
	    vpushll(s1, p->v2);
	else
	    vpushi(s1, p->v2);
        if (p->v1 == p->v2) {
            gen_op(s1, TOK_EQ);
            gtst_addr(s1, 0, p->sym);
        } else {
            gen_op(s1, TOK_LE);
            e = gvtst(s1, 1, 0);
            vdup(s1);
	    if (ll)
	        vpushll(s1, p->v1);
	    else
	        vpushi(s1, p->v1);
            gen_op(s1, TOK_GE);
            gtst_addr(s1, 0, p->sym);
            gsym(s1, e);
        }
    }
    *bsym = gjmp(s1, *bsym);
}

/* ------------------------------------------------------------------------- */
/* __attribute__((cleanup(fn))) */

static void try_call_scope_cleanup(TCCState *s1, Sym *stop)
{
    Sym *cls = s1->cur_scope->cl.s;

    for (; cls != stop; cls = cls->ncl) {
	Sym *fs = cls->next;
	Sym *vs = cls->prev_tok;

	vpushsym(s1, &fs->type, fs);
	vset(s1, &vs->type, vs->r, vs->c);
	s1->vtop->sym = vs;
        mk_pointer(s1, &s1->vtop->type);
	gaddrof(s1);
	gfunc_call(s1, 1);
    }
}

static void try_call_cleanup_goto(TCCState *s1, Sym *cleanupstate)
{
    Sym *oc, *cc;
    int ocd, ccd;

    if (!s1->cur_scope->cl.s)
	return;

    /* search NCA of both cleanup chains given parents and initial depth */
    ocd = cleanupstate ? cleanupstate->v & ~SYM_FIELD : 0;
    for (ccd = s1->cur_scope->cl.n, oc = cleanupstate; ocd > ccd; --ocd, oc = oc->ncl)
      ;
    for (cc = s1->cur_scope->cl.s; ccd > ocd; --ccd, cc = cc->ncl)
      ;
    for (; cc != oc; cc = cc->ncl, oc = oc->ncl, --ccd)
      ;

    try_call_scope_cleanup(s1, cc);
}

/* call 'func' for each __attribute__((cleanup(func))) */
static void block_cleanup(TCCState *s1, struct scope *o)
{
    int jmp = 0;
    Sym *g, **pg;
    for (pg = &s1->pending_gotos; (g = *pg) && g->c > o->cl.n;) {
        if (g->prev_tok->r & LABEL_FORWARD) {
            Sym *pcl = g->next;
            if (!jmp)
                jmp = gjmp(s1, 0);
            gsym(s1, pcl->jnext);
            try_call_scope_cleanup(s1, o->cl.s);
            pcl->jnext = gjmp(s1, 0);
            if (!o->cl.n)
                goto remove_pending;
            g->c = o->cl.n;
            pg = &g->prev;
        } else {
    remove_pending:
            *pg = g->prev;
            sym_free(s1, g);
        }
    }
    gsym(s1, jmp);
    try_call_scope_cleanup(s1, o->cl.s);
}

/* ------------------------------------------------------------------------- */
/* VLA */

static void vla_restore(TCCState *s1, int loc)
{
    if (loc)
        gen_vla_sp_restore(s1, loc);
}

static void vla_leave(TCCState *s1, struct scope *o)
{
    struct scope *c = s1->cur_scope, *v = NULL;
    for (; c != o && c; c = c->prev)
      if (c->vla.num)
        v = c;
    if (v)
      vla_restore(s1, v->vla.locorig);
}

/* ------------------------------------------------------------------------- */
/* local scopes */

void new_scope(TCCState *s1, struct scope *o)
{
    /* copy and link previous scope */
    *o = *s1->cur_scope;
    o->prev = s1->cur_scope;
    s1->cur_scope = o;
    s1->cur_scope->vla.num = 0;

    /* record local declaration stack position */
    o->lstk = s1->local_stack;
    o->llstk = s1->local_label_stack;
    ++s1->local_scope;

    if (s1->debug_modes)
        tcc_debug_stabn(s1, N_LBRAC, s1->ind - s1->func_ind);
}

void prev_scope(TCCState *s1, struct scope *o, int is_expr)
{
    vla_leave(s1, o->prev);

    if (o->cl.s != o->prev->cl.s)
        block_cleanup(s1, o->prev);

    /* pop locally defined labels */
    label_pop(s1, &s1->local_label_stack, o->llstk, is_expr);

    /* In the is_expr case (a statement expression is finished here),
       s1->vtop might refer to symbols on the local_stack.  Either via the
       type or via s1->vtop->sym.  We can't pop those nor any that in turn
       might be referred to.  To make it easier we don't roll back
       any symbols in that case; some upper level call to block() will
       do that.  We do have to remove such symbols from the lookup
       tables, though.  sym_pop will do that.  */

    /* pop locally defined symbols */
    pop_local_syms(s1, o->lstk, is_expr);
    s1->cur_scope = o->prev;
    --s1->local_scope;

    if (s1->debug_modes)
        tcc_debug_stabn(s1, N_RBRAC, s1->ind - s1->func_ind);
}

/* leave a scope via break/continue(/goto) */
void leave_scope(TCCState *s1, struct scope *o)
{
    if (!o)
        return;
    try_call_scope_cleanup(s1, o->cl.s);
    vla_leave(s1, o);
}

/* ------------------------------------------------------------------------- */
/* call block from 'for do while' loops */

static void lblock(TCCState *s1, int *bsym, int *csym)
{
    struct scope *lo = s1->loop_scope, *co = s1->cur_scope;
    int *b = co->bsym, *c = co->csym;
    if (csym) {
        co->csym = csym;
        s1->loop_scope = co;
    }
    co->bsym = bsym;
    block(s1, 0);
    co->bsym = b;
    if (csym) {
        co->csym = c;
        s1->loop_scope = lo;
    }
}

static void block(TCCState *s1, int is_expr)
{
    int a, b, c, d, e, t;
    struct scope o;
    Sym *s;

    if (is_expr) {
        /* default return value is (void) */
        vpushi(s1, 0);
        s1->vtop->type.t = VT_VOID;
    }

again:
    t = s1->tok;
    /* If the token carries a value, next(s1) might destroy it. Only with
       invalid code such as f(){"123"4;} */
    if (TOK_HAS_VALUE(t))
        goto expr;
    next(s1);

    if (s1->debug_modes)
        tcc_tcov_check_line (s1, 0), tcc_tcov_block_begin (s1);

    if (t == TOK_IF) {
        skip(s1, '(');
        gexpr(s1);
        skip(s1, ')');
        a = gvtst(s1, 1, 0);
        block(s1, 0);
        if (s1->tok == TOK_ELSE) {
            d = gjmp(s1, 0);
            gsym(s1,a);
            next(s1);
            block(s1, 0);
            gsym(s1,d); /* patch else jmp */
        } else {
            gsym(s1,a);
        }

    } else if (t == TOK_WHILE) {
        d = gind(s1);
        skip(s1, '(');
        gexpr(s1);
        skip(s1, ')');
        a = gvtst(s1, 1, 0);
        b = 0;
        lblock(s1, &a, &b);
        gjmp_addr(s1, d);
        gsym_addr(s1, b, d);
        gsym(s1, a);

    } else if (t == '{') {
        new_scope(s1, &o);

        /* handle local labels declarations */
        while (s1->tok == TOK_LABEL) {
            do {
                next(s1);
                if (s1->tok < TOK_UIDENT)
                    expect(s1, "label identifier");
                label_push(s1, &s1->local_label_stack, s1->tok, LABEL_DECLARED);
                next(s1);
            } while (s1->tok == ',');
            skip(s1, ';');
        }

        while (s1->tok != '}') {
	    decl(s1, VT_LOCAL);
            if (s1->tok != '}') {
                if (is_expr)
                    vpop(s1);
                block(s1, is_expr);
            }
        }

        prev_scope(s1, &o, is_expr);
        if (s1->local_scope)
            next(s1);
        else if (!s1->nocode_wanted)
            check_func_return(s1);

    } else if (t == TOK_RETURN) {
        b = (s1->func_vt.t & VT_BTYPE) != VT_VOID;
        if (s1->tok != ';') {
            gexpr(s1);
            if (b) {
                gen_assign_cast(s1, &s1->func_vt);
            } else {
                if (s1->vtop->type.t != VT_VOID)
                    tcc_warning(s1, "void function returns a value");
                s1->vtop--;
            }
        } else if (b) {
            tcc_warning(s1, "'return' with no value");
            b = 0;
        }
        leave_scope(s1, s1->root_scope);
        if (b)
            gfunc_return(s1, &s1->func_vt);
        skip(s1, ';');
        /* jump unless last stmt in top-level block */
        if (s1->tok != '}' || s1->local_scope != 1)
            s1->rsym = gjmp(s1, s1->rsym);
        if (s1->debug_modes)
	    tcc_tcov_block_end (s1, tcov_data.line);
        CODE_OFF(s1);

    } else if (t == TOK_BREAK) {
        /* compute jump */
        if (!s1->cur_scope->bsym)
            tcc_error(s1, "cannot break");
        if (s1->cur_switch && s1->cur_scope->bsym == s1->cur_switch->bsym)
            leave_scope(s1, s1->cur_switch->scope);
        else
            leave_scope(s1, s1->loop_scope);
        *s1->cur_scope->bsym = gjmp(s1, *s1->cur_scope->bsym);
        skip(s1, ';');

    } else if (t == TOK_CONTINUE) {
        /* compute jump */
        if (!s1->cur_scope->csym)
            tcc_error(s1, "cannot continue");
        leave_scope(s1, s1->loop_scope);
        *s1->cur_scope->csym = gjmp(s1, *s1->cur_scope->csym);
        skip(s1, ';');

    } else if (t == TOK_FOR) {
        new_scope(s1, &o);

        skip(s1, '(');
        if (s1->tok != ';') {
            /* c99 for-loop init decl? */
            if (!decl0(s1, VT_LOCAL, 1, NULL)) {
                /* no, regular for-loop init expr */
                gexpr(s1);
                vpop(s1);
            }
        }
        skip(s1, ';');
        a = b = 0;
        c = d = gind(s1);
        if (s1->tok != ';') {
            gexpr(s1);
            a = gvtst(s1, 1, 0);
        }
        skip(s1, ';');
        if (s1->tok != ')') {
            e = gjmp(s1, 0);
            d = gind(s1);
            gexpr(s1);
            vpop(s1);
            gjmp_addr(s1, c);
            gsym(s1, e);
        }
        skip(s1, ')');
        lblock(s1, &a, &b);
        gjmp_addr(s1, d);
        gsym_addr(s1, b, d);
        gsym(s1, a);
        prev_scope(s1, &o, 0);

    } else if (t == TOK_DO) {
        a = b = 0;
        d = gind(s1);
        lblock(s1, &a, &b);
        gsym(s1, b);
        skip(s1, TOK_WHILE);
        skip(s1, '(');
	gexpr(s1);
        skip(s1, ')');
        skip(s1, ';');
	c = gvtst(s1, 0, 0);
	gsym_addr(s1, c, d);
        gsym(s1, a);

    } else if (t == TOK_SWITCH) {
        struct switch_t *sw;

        sw = tcc_mallocz(s1, sizeof *sw);
        sw->bsym = &a;
        sw->scope = s1->cur_scope;
        sw->prev = s1->cur_switch;
        s1->cur_switch = sw;

        skip(s1, '(');
        gexpr(s1);
        skip(s1, ')');
        sw->sv = *s1->vtop--; /* save switch value */

        a = 0;
        b = gjmp(s1, 0); /* jump to first case */
        lblock(s1, &a, NULL);
        a = gjmp(s1, a); /* add implicit break */
        /* case lookup */
        gsym(s1, b);

        if (sw->sv.type.t & VT_UNSIGNED)
            qsort(sw->p, sw->n, sizeof(void*), case_cmpu);
        else
            qsort(sw->p, sw->n, sizeof(void*), case_cmpi);

        for (b = 1; b < sw->n; b++)
            if (sw->sv.type.t & VT_UNSIGNED
                ? (uint64_t)sw->p[b - 1]->v2 >= (uint64_t)sw->p[b]->v1
                : sw->p[b - 1]->v2 >= sw->p[b]->v1)
                tcc_error(s1, "duplicate case value");

        vpushv(s1, &sw->sv);
        gv(s1, RC_INT);
        d = 0, gcase(s1, sw->p, sw->n, &d);
        vpop(s1);
        if (sw->def_sym)
            gsym_addr(s1, d, sw->def_sym);
        else
            gsym(s1, d);
        /* break label */
        gsym(s1, a);

        dynarray_reset(s1, &sw->p, &sw->n);
        s1->cur_switch = sw->prev;
        tcc_free(s1, sw);

    } else if (t == TOK_CASE) {
        struct case_t *cr = tcc_malloc(s1, sizeof(struct case_t));
        if (!s1->cur_switch)
            expect(s1, "switch");
        cr->v1 = cr->v2 = expr_const64(s1);
        if (gnu_ext && s1->tok == TOK_DOTS) {
            next(s1);
            cr->v2 = expr_const64(s1);
            if ((!(s1->cur_switch->sv.type.t & VT_UNSIGNED) && cr->v2 < cr->v1)
                || (s1->cur_switch->sv.type.t & VT_UNSIGNED && (uint64_t)cr->v2 < (uint64_t)cr->v1))
                tcc_warning(s1, "empty case range");
        }
        tcov_data.ind = 0;
        cr->sym = gind(s1);
        dynarray_add(s1, &s1->cur_switch->p, &s1->cur_switch->n, cr);
        skip(s1, ':');
        is_expr = 0;
        goto block_after_label;

    } else if (t == TOK_DEFAULT) {
        if (!s1->cur_switch)
            expect(s1, "switch");
        if (s1->cur_switch->def_sym)
            tcc_error(s1, "too many 'default'");
        tcov_data.ind = 0;
        s1->cur_switch->def_sym = gind(s1);
        skip(s1, ':');
        is_expr = 0;
        goto block_after_label;

    } else if (t == TOK_GOTO) {
        if (s1->cur_scope->vla.num)
          vla_restore(s1, s1->cur_scope->vla.locorig);
        if (s1->tok == '*' && gnu_ext) {
            /* computed goto */
            next(s1);
            gexpr(s1);
            if ((s1->vtop->type.t & VT_BTYPE) != VT_PTR)
                expect(s1, "pointer");
            ggoto(s1);

        } else if (s1->tok >= TOK_UIDENT) {
	    s = label_find(s1, s1->tok);
	    /* put forward definition if needed */
            if (!s)
              s = label_push(s1, &s1->global_label_stack, s1->tok, LABEL_FORWARD);
            else if (s->r == LABEL_DECLARED)
              s->r = LABEL_FORWARD;

	    if (s->r & LABEL_FORWARD) {
		/* start new goto chain for cleanups, linked via label->next */
		if (s1->cur_scope->cl.s && !s1->nocode_wanted) {
                    sym_push2(s1, &s1->pending_gotos, SYM_FIELD, 0, s1->cur_scope->cl.n);
                    s1->pending_gotos->prev_tok = s;
                    s = sym_push2(s1, &s->next, SYM_FIELD, 0, 0);
                    s1->pending_gotos->next = s;
                }
		s->jnext = gjmp(s1, s->jnext);
	    } else {
		try_call_cleanup_goto(s1, s->cleanupstate);
		gjmp_addr(s1, s->jnext);
	    }
	    next(s1);

        } else {
            expect(s1, "label identifier");
        }
        skip(s1, ';');

    } else if (t == TOK_ASM1 || t == TOK_ASM2 || t == TOK_ASM3) {
        asm_instr(s1);

    } else {
        if (s1->tok == ':' && t >= TOK_UIDENT) {
            /* label case */
	    next(s1);
            s = label_find(s1, t);
            if (s) {
                if (s->r == LABEL_DEFINED)
                    tcc_error(s1, "duplicate label '%s'", get_tok_str(s1, s->v, NULL));
                s->r = LABEL_DEFINED;
		if (s->next) {
		    Sym *pcl; /* pending cleanup goto */
		    for (pcl = s->next; pcl; pcl = pcl->prev)
		      gsym(s1, pcl->jnext);
		    sym_pop(s1, &s->next, NULL, 0);
		} else
		  gsym(s1, s->jnext);
            } else {
                s = label_push(s1, &s1->global_label_stack, t, LABEL_DEFINED);
            }
            s->jnext = gind(s1);
            s->cleanupstate = s1->cur_scope->cl.s;

    block_after_label:
            vla_restore(s1, s1->cur_scope->vla.loc);
            /* we accept this, but it is a mistake */
            if (s1->tok == '}') {
                tcc_warning(s1, "deprecated use of label at end of compound statement");
            } else {
                goto again;
            }

        } else {
            /* expression case */
            if (t != ';') {
                unget_tok(s1, t);
    expr:
                if (is_expr) {
                    vpop(s1);
                    gexpr(s1);
                } else {
                    gexpr(s1);
                    vpop(s1);
                }
                skip(s1, ';');
            }
        }
    }

    if (s1->debug_modes)
        tcc_tcov_check_line (s1, 0), tcc_tcov_block_end (s1, 0);
}

/* This skips over a stream of tokens containing balanced {} and ()
   pairs, stopping at outer ',' ';' and '}' (or matching '}' if we started
   with a '{').  If STR then allocates and stores the skipped tokens
   in *STR.  This doesn't check if () and {} are nested correctly,
   i.e. "({)}" is accepted.  */
static void skip_or_save_block(TCCState *s1, TokenString **str)
{
    int braces = s1->tok == '{';
    int level = 0;
    if (str)
      *str = tok_str_alloc(s1);

    while ((level > 0 || (s1->tok != '}' && s1->tok != ',' && s1->tok != ';' && s1->tok != ')'))) {
	int t;
	if (s1->tok == TOK_EOF) {
	     if (str || level > 0)
	       tcc_error(s1, "unexpected end of file");
	     else
	       break;
	}
	if (str)
	  tok_str_add_tok(s1, *str);
	t = s1->tok;
	next(s1);
	if (t == '{' || t == '(') {
	    level++;
	} else if (t == '}' || t == ')') {
	    level--;
	    if (level == 0 && braces && t == '}')
	      break;
	}
    }
    if (str) {
	tok_str_add(s1, *str, -1);
	tok_str_add(s1, *str, 0);
    }
}

#define EXPR_CONST 1
#define EXPR_ANY   2

static void parse_init_elem(TCCState *s1, int expr_type)
{
    int saved_global_expr;
    switch(expr_type) {
    case EXPR_CONST:
        /* compound literals must be allocated globally in this case */
        saved_global_expr = s1->global_expr;
        s1->global_expr = 1;
        expr_const1(s1);
        s1->global_expr = saved_global_expr;
        /* NOTE: symbols are accepted, as well as lvalue for anon symbols
	   (compound literals).  */
        if (((s1->vtop->r & (VT_VALMASK | VT_LVAL)) != VT_CONST
             && ((s1->vtop->r & (VT_SYM|VT_LVAL)) != (VT_SYM|VT_LVAL)
                 || s1->vtop->sym->v < SYM_FIRST_ANOM))
#ifdef TCC_TARGET_PE
                 || ((s1->vtop->r & VT_SYM) && s1->vtop->sym->a.dllimport)
#endif
           )
            tcc_error(s1, "initializer element is not constant");
        break;
    case EXPR_ANY:
        expr_eq(s1);
        break;
    }
}

#if 1
static void init_assert(TCCState *s1, init_params *p, int offset)
{
    if (p->sec ? !NODATA_WANTED && offset > p->sec->data_offset
               : !s1->nocode_wanted && offset > p->local_offset)
        tcc_internal_error(s1, "initializer overflow");
}
#else
#define init_assert(s1, sec, offset)
#endif

/* put zeros for variable based init */
static void init_putz(TCCState *s1, init_params *p, unsigned long c, int size)
{
    init_assert(s1, p, c + size);
    if (p->sec) {
        /* nothing to do because globals are already set to zero */
    } else {
        vpush_helper_func(s1, TOK_memset);
        vseti(s1, VT_LOCAL, c);
#ifdef TCC_TARGET_ARM
        vpushs(s1, size);
        vpushi(s1, 0);
#else
        vpushi(s1, 0);
        vpushs(s1, size);
#endif
        gfunc_call(s1, 3);
    }
}

#define DIF_FIRST     1
#define DIF_SIZE_ONLY 2
#define DIF_HAVE_ELEM 4
#define DIF_CLEAR     8

/* delete relocations for specified range c ... c + size. Unfortunatly
   in very special cases, relocations may occur unordered */
static void decl_design_delrels(Section *sec, int c, int size)
{
    ElfW_Rel *rel, *rel2, *rel_end;
    if (!sec || !sec->reloc)
        return;
    rel = rel2 = (ElfW_Rel*)sec->reloc->data;
    rel_end = (ElfW_Rel*)(sec->reloc->data + sec->reloc->data_offset);
    while (rel < rel_end) {
        if (rel->r_offset >= c && rel->r_offset < c + size) {
            sec->reloc->data_offset -= sizeof *rel;
        } else {
            if (rel2 != rel)
                memcpy(rel2, rel, sizeof *rel);
            ++rel2;
        }
        ++rel;
    }
}

static void decl_design_flex(TCCState *s1, init_params *p, Sym *ref, int index)
{
    if (ref == p->flex_array_ref) {
        if (index >= ref->c)
            ref->c = index + 1;
    } else if (ref->c < 0)
        tcc_error(s1, "flexible array has zero size in this context");
}

/* t is the array or struct type. c is the array or struct
   address. cur_field is the pointer to the current
   field, for arrays the 'c' member contains the current start
   index.  'flags' is as in decl_initializer.
   'al' contains the already initialized length of the
   current container (starting at c).  This returns the new length of that.  */
static int decl_designator(TCCState *s1, init_params *p, CType *type, unsigned long c,
                           Sym **cur_field, int flags, int al)
{
    Sym *s, *f;
    int index, index_last, align, l, nb_elems, elem_size;
    unsigned long corig = c;

    elem_size = 0;
    nb_elems = 1;

    if (flags & DIF_HAVE_ELEM)
        goto no_designator;

    if (gnu_ext && s1->tok >= TOK_UIDENT) {
        l = s1->tok, next(s1);
        if (s1->tok == ':')
            goto struct_field;
        unget_tok(s1, l);
    }

    /* NOTE: we only support ranges for last designator */
    while (nb_elems == 1 && (s1->tok == '[' || s1->tok == '.')) {
        if (s1->tok == '[') {
            if (!(type->t & VT_ARRAY))
                expect(s1, "array type");
            next(s1);
            index = index_last = expr_const(s1);
            if (s1->tok == TOK_DOTS && gnu_ext) {
                next(s1);
                index_last = expr_const(s1);
            }
            skip(s1, ']');
            s = type->ref;
            decl_design_flex(s1, p, s, index_last);
            if (index < 0 || index_last >= s->c || index_last < index)
	        tcc_error(s1, "index exceeds array bounds or range is empty");
            if (cur_field)
		(*cur_field)->c = index_last;
            type = pointed_type(type);
            elem_size = type_size(s1, type, &align);
            c += index * elem_size;
            nb_elems = index_last - index + 1;
        } else {
            int cumofs;
            next(s1);
            l = s1->tok;
        struct_field:
            next(s1);
            if ((type->t & VT_BTYPE) != VT_STRUCT)
                expect(s1, "struct/union type");
            cumofs = 0;
	    f = find_field(type, l, &cumofs);
            if (!f)
                expect(s1, "field");
            if (cur_field)
                *cur_field = f;
	    type = &f->type;
            c += cumofs + f->c;
        }
        cur_field = NULL;
    }
    if (!cur_field) {
        if (s1->tok == '=') {
            next(s1);
        } else if (!gnu_ext) {
	    expect(s1, "=");
        }
    } else {
    no_designator:
        if (type->t & VT_ARRAY) {
	    index = (*cur_field)->c;
            s = type->ref;
            decl_design_flex(s1, p, s, index);
            if (index >= s->c)
                tcc_error(s1, "too many initializers");
            type = pointed_type(type);
            elem_size = type_size(s1, type, &align);
            c += index * elem_size;
        } else {
            f = *cur_field;
	    while (f && (f->v & SYM_FIRST_ANOM) && (f->type.t & VT_BITFIELD))
	        *cur_field = f = f->next;
            if (!f)
                tcc_error(s1, "too many initializers");
	    type = &f->type;
            c += f->c;
        }
    }

    if (!elem_size) /* for structs */
        elem_size = type_size(s1, type, &align);

    /* Using designators the same element can be initialized more
       than once.  In that case we need to delete possibly already
       existing relocations. */
    if (!(flags & DIF_SIZE_ONLY) && c - corig < al) {
        decl_design_delrels(p->sec, c, elem_size * nb_elems);
        flags &= ~DIF_CLEAR; /* mark stack dirty too */
    }

    decl_initializer(s1, p, type, c, flags & ~DIF_FIRST);

    if (!(flags & DIF_SIZE_ONLY) && nb_elems > 1) {
        Sym aref = {0};
        CType t1;
        int i;
        if (p->sec || (type->t & VT_ARRAY)) {
            /* make init_putv/vstore believe it were a struct */
            aref.c = elem_size;
            t1.t = VT_STRUCT, t1.ref = &aref;
            type = &t1;
        }
        if (p->sec)
            vpush_ref(s1, type, p->sec, c, elem_size);
        else
	    vset(s1, type, VT_LOCAL|VT_LVAL, c);
        for (i = 1; i < nb_elems; i++) {
            vdup(s1);
            init_putv(s1, p, type, c + elem_size * i);
	}
        vpop(s1);
    }

    c += nb_elems * elem_size;
    if (c - corig > al)
      al = c - corig;
    return al;
}

/* store a value or an expression directly in global data or in local array */
static void init_putv(TCCState *s1, init_params *p, CType *type, unsigned long c)
{
    int bt;
    void *ptr;
    CType dtype;
    int size, align;
    Section *sec = p->sec;
    uint64_t val;

    dtype = *type;
    dtype.t &= ~VT_CONSTANT; /* need to do that to avoid false warning */

    size = type_size(s1, type, &align);
    if (type->t & VT_BITFIELD)
        size = (BIT_POS(type->t) + BIT_SIZE(type->t) + 7) / 8;
    init_assert(s1, p, c + size);

    if (sec) {
        /* XXX: not portable */
        /* XXX: generate error if incorrect relocation */
        gen_assign_cast(s1, &dtype);
        bt = type->t & VT_BTYPE;

        if ((s1->vtop->r & VT_SYM)
            && bt != VT_PTR
            && (bt != (PTR_SIZE == 8 ? VT_LLONG : VT_INT)
                || (type->t & VT_BITFIELD))
            && !((s1->vtop->r & VT_CONST) && s1->vtop->sym->v >= SYM_FIRST_ANOM)
            )
            tcc_error(s1, "initializer element is not computable at load time");

        if (NODATA_WANTED) {
            s1->vtop--;
            return;
        }

        ptr = sec->data + c;
        val = s1->vtop->c.i;

        /* XXX: make code faster ? */
	if ((s1->vtop->r & (VT_SYM|VT_CONST)) == (VT_SYM|VT_CONST) &&
	    s1->vtop->sym->v >= SYM_FIRST_ANOM &&
	    /* XXX This rejects compound literals like
	       '(void *){ptr}'.  The problem is that '&sym' is
	       represented the same way, which would be ruled out
	       by the SYM_FIRST_ANOM check above, but also '"string"'
	       in 'char *p = "string"' is represented the same
	       with the type being VT_PTR and the symbol being an
	       anonymous one.  That is, there's no difference in s1->vtop
	       between '(void *){x}' and '&(void *){x}'.  Ignore
	       pointer typed entities here.  Hopefully no real code
	       will ever use compound literals with scalar type.  */
	    (s1->vtop->type.t & VT_BTYPE) != VT_PTR) {
	    /* These come from compound literals, memcpy stuff over.  */
	    Section *ssec;
	    ElfSym *esym;
	    ElfW_Rel *rel;
	    esym = elfsym(s1, s1->vtop->sym);
	    ssec = s1->sections[esym->st_shndx];
	    memmove (ptr, ssec->data + esym->st_value + (int)s1->vtop->c.i, size);
	    if (ssec->reloc) {
		/* We need to copy over all memory contents, and that
		   includes relocations.  Use the fact that relocs are
		   created it order, so look from the end of relocs
		   until we hit one before the copied region.  */
                unsigned long relofs = ssec->reloc->data_offset;
		while (relofs >= sizeof(*rel)) {
                    relofs -= sizeof(*rel);
                    rel = (ElfW_Rel*)(ssec->reloc->data + relofs);
		    if (rel->r_offset >= esym->st_value + size)
		      continue;
		    if (rel->r_offset < esym->st_value)
		      break;
		    put_elf_reloca(symtab_section, sec,
				   c + rel->r_offset - esym->st_value,
				   ELFW(R_TYPE)(rel->r_info),
				   ELFW(R_SYM)(rel->r_info),
#if PTR_SIZE == 8
				   rel->r_addend
#else
				   0
#endif
				  );
		}
	    }
	} else {
            if (type->t & VT_BITFIELD) {
                int bit_pos, bit_size, bits, n;
                unsigned char *p, v, m;
                bit_pos = BIT_POS(s1->vtop->type.t);
                bit_size = BIT_SIZE(s1->vtop->type.t);
                p = (unsigned char*)ptr + (bit_pos >> 3);
                bit_pos &= 7, bits = 0;
                while (bit_size) {
                    n = 8 - bit_pos;
                    if (n > bit_size)
                        n = bit_size;
                    v = val >> bits << bit_pos;
                    m = ((1 << n) - 1) << bit_pos;
                    *p = (*p & ~m) | (v & m);
                    bits += n, bit_size -= n, bit_pos = 0, ++p;
                }
            } else
            switch(bt) {
	    case VT_BOOL:
		*(char *)ptr = val != 0;
                break;
	    case VT_BYTE:
		*(char *)ptr = val;
		break;
	    case VT_SHORT:
                write16le(ptr, val);
		break;
	    case VT_FLOAT:
                write32le(ptr, val);
		break;
	    case VT_DOUBLE:
                write64le(ptr, val);
		break;
	    case VT_LDOUBLE:
#if defined TCC_IS_NATIVE_387
                /* Host and target platform may be different but both have x87.
                   On windows, tcc does not use VT_LDOUBLE, except when it is a
                   cross compiler.  In this case a mingw gcc as host compiler
                   comes here with 10-byte long doubles, while msvc or tcc won't.
                   tcc itself can still translate by asm.
                   In any case we avoid possibly random bytes 11 and 12.
                */
                if (sizeof (long double) >= 10)
                    memcpy(ptr, &s1->vtop->c.ld, 10);
#ifdef __TINYC__
                else if (sizeof (long double) == sizeof (double))
                    __asm__("fldl %1\nfstpt %0\n" : "=m" (*ptr) : "m" (s1->vtop->c.ld));
#endif
                else if (s1->vtop->c.ld == 0.0)
                    ;
                else
#endif
                /* For other platforms it should work natively, but may not work
                   for cross compilers */
                if (sizeof(long double) == LDOUBLE_SIZE)
                    memcpy(ptr, &s1->vtop->c.ld, LDOUBLE_SIZE);
                else if (sizeof(double) == LDOUBLE_SIZE)
                    memcpy(ptr, &s1->vtop->c.ld, LDOUBLE_SIZE);
#ifndef TCC_CROSS_TEST
                else
                    tcc_error(s1, "can't cross compile long double constants");
#endif
		break;

#if PTR_SIZE == 8
            /* intptr_t may need a reloc too, see tcctest.c:relocation_test() */
	    case VT_LLONG:
	    case VT_PTR:
	        if (s1->vtop->r & VT_SYM)
	          greloca(s1, sec, s1->vtop->sym, c, R_DATA_PTR, val);
	        else
	          write64le(ptr, val);
	        break;
            case VT_INT:
                write32le(ptr, val);
                break;
#else
	    case VT_LLONG:
                write64le(ptr, val);
                break;
            case VT_PTR:
            case VT_INT:
	        if (s1->vtop->r & VT_SYM)
	          greloc(s1, sec, s1->vtop->sym, c, R_DATA_PTR);
	        write32le(ptr, val);
	        break;
#endif
	    default:
                //tcc_internal_error(s1, "unexpected type");
                break;
	    }
	}
        s1->vtop--;
    } else {
        vset(s1, &dtype, VT_LOCAL|VT_LVAL, c);
        vswap(s1);
        vstore(s1);
        vpop(s1);
    }
}

/* 't' contains the type and storage info. 'c' is the offset of the
   object in section 'sec'. If 'sec' is NULL, it means stack based
   allocation. 'flags & DIF_FIRST' is true if array '{' must be read (multi
   dimension implicit array init handling). 'flags & DIF_SIZE_ONLY' is true if
   size only evaluation is wanted (only for arrays). */
static void decl_initializer(TCCState *s1, init_params *p, CType *type, unsigned long c, int flags)
{
    int len, n, no_oblock, i;
    int size1, align1;
    Sym *s, *f;
    Sym indexsym;
    CType *t1;

    /* generate line number info */
    if (s1->debug_modes && !p->sec)
        tcc_debug_line(s1), tcc_tcov_check_line (s1, 1);

    if (!(flags & DIF_HAVE_ELEM) && s1->tok != '{' &&
	/* In case of strings we have special handling for arrays, so
	   don't consume them as initializer value (which would commit them
	   to some anonymous symbol).  */
	s1->tok != TOK_LSTR && s1->tok != TOK_STR &&
	!(flags & DIF_SIZE_ONLY)) {
	parse_init_elem(s1, !p->sec ? EXPR_ANY : EXPR_CONST);
        flags |= DIF_HAVE_ELEM;
    }

    if ((flags & DIF_HAVE_ELEM) &&
	!(type->t & VT_ARRAY) &&
	/* Use i_c_parameter_t, to strip toplevel qualifiers.
	   The source type might have VT_CONSTANT set, which is
	   of course assignable to non-const elements.  */
	is_compatible_unqualified_types(type, &s1->vtop->type)) {
        goto init_putv;

    } else if (type->t & VT_ARRAY) {
        no_oblock = 1;
        if (((flags & DIF_FIRST) && s1->tok != TOK_LSTR && s1->tok != TOK_STR) ||
            s1->tok == '{') {
            skip(s1, '{');
            no_oblock = 0;
        }

        s = type->ref;
        n = s->c;
        t1 = pointed_type(type);
        size1 = type_size(s1, t1, &align1);

        /* only parse strings here if correct type (otherwise: handle
           them as ((w)char *) expressions */
        if ((s1->tok == TOK_LSTR && 
#ifdef TCC_TARGET_PE
             (t1->t & VT_BTYPE) == VT_SHORT && (t1->t & VT_UNSIGNED)
#else
             (t1->t & VT_BTYPE) == VT_INT
#endif
            ) || (s1->tok == TOK_STR && (t1->t & VT_BTYPE) == VT_BYTE)) {
	    len = 0;
            cstr_reset(&s1->initstr);
            if (size1 != (s1->tok == TOK_STR ? 1 : sizeof(nwchar_t)))
              tcc_error(s1, "unhandled string literal merging");
            while (s1->tok == TOK_STR || s1->tok == TOK_LSTR) {
                if (s1->initstr.size)
                  s1->initstr.size -= size1;
                if (s1->tok == TOK_STR)
                  len += s1->tokc.str.size;
                else
                  len += s1->tokc.str.size / sizeof(nwchar_t);
                len--;
                cstr_cat(s1, &s1->initstr, s1->tokc.str.data, s1->tokc.str.size);
                next(s1);
            }
            if (s1->tok != ')' && s1->tok != '}' && s1->tok != ',' && s1->tok != ';'
                && s1->tok != TOK_EOF) {
                /* Not a lone literal but part of a bigger expression.  */
                unget_tok(s1, size1 == 1 ? TOK_STR : TOK_LSTR);
                s1->tokc.str.size = s1->initstr.size;
                s1->tokc.str.data = s1->initstr.data;
                goto do_init_array;
            }

            decl_design_flex(s1, p, s, len);
            if (!(flags & DIF_SIZE_ONLY)) {
                int nb = n;
                if (len < nb)
                    nb = len;
                if (len > nb)
                  tcc_warning(s1, "initializer-string for array is too long");
                /* in order to go faster for common case (char
                   string in global variable, we handle it
                   specifically */
                if (p->sec && size1 == 1) {
                    init_assert(s1, p, c + nb);
                    if (!NODATA_WANTED)
                      memcpy(p->sec->data + c, s1->initstr.data, nb);
                } else {
                    for(i=0;i<n;i++) {
                        if (i >= nb) {
                          /* only add trailing zero if enough storage (no
                             warning in this case since it is standard) */
                          if (flags & DIF_CLEAR)
                            break;
                          if (n - i >= 4) {
                            init_putz(s1, p, c + i * size1, (n - i) * size1);
                            break;
                          }
                          s1->ch = 0;
                        } else if (size1 == 1)
                          s1->ch = ((unsigned char *)s1->initstr.data)[i];
                        else
                          s1->ch = ((nwchar_t *)s1->initstr.data)[i];
                        vpushi(s1, s1->ch);
                        init_putv(s1, p, t1, c + i * size1);
                    }
                }
            }
        } else {

          do_init_array:
	    indexsym.c = 0;
	    f = &indexsym;

          do_init_list:
            /* zero memory once in advance */
            if (!(flags & (DIF_CLEAR | DIF_SIZE_ONLY))) {
                init_putz(s1, p, c, n*size1);
                flags |= DIF_CLEAR;
            }

	    len = 0;
	    while (s1->tok != '}' || (flags & DIF_HAVE_ELEM)) {
		len = decl_designator(s1, p, type, c, &f, flags, len);
		flags &= ~DIF_HAVE_ELEM;
		if (type->t & VT_ARRAY) {
		    ++indexsym.c;
		    /* special test for multi dimensional arrays (may not
		       be strictly correct if designators are used at the
		       same time) */
		    if (no_oblock && len >= n*size1)
		        break;
		} else {
		    if (s->type.t == VT_UNION)
		        f = NULL;
		    else
		        f = f->next;
		    if (no_oblock && f == NULL)
		        break;
		}

		if (s1->tok == '}')
		    break;
		skip(s1, ',');
	    }
        }
        if (!no_oblock)
            skip(s1, '}');
    } else if ((type->t & VT_BTYPE) == VT_STRUCT) {
        no_oblock = 1;
        if ((flags & DIF_FIRST) || s1->tok == '{') {
            skip(s1, '{');
            no_oblock = 0;
        }
        s = type->ref;
        f = s->next;
        n = s->c;
        size1 = 1;
	goto do_init_list;
    } else if (s1->tok == '{') {
        if (flags & DIF_HAVE_ELEM)
          skip(s1, ';');
        next(s1);
        decl_initializer(s1, p, type, c, flags & ~DIF_HAVE_ELEM);
        skip(s1, '}');
    } else if ((flags & DIF_SIZE_ONLY)) {
	/* If we supported only ISO C we wouldn't have to accept calling
	   this on anything than an array if DIF_SIZE_ONLY (and even then
	   only on the outermost level, so no recursion would be needed),
	   because initializing a flex array member isn't supported.
	   But GNU C supports it, so we need to recurse even into
	   subfields of structs and arrays when DIF_SIZE_ONLY is set.  */
        /* just skip expression */
        skip_or_save_block(s1, NULL);
    } else {
	if (!(flags & DIF_HAVE_ELEM)) {
	    /* This should happen only when we haven't parsed
	       the init element above for fear of committing a
	       string constant to memory too early.  */
	    if (s1->tok != TOK_STR && s1->tok != TOK_LSTR)
	      expect(s1, "string constant");
	    parse_init_elem(s1, !p->sec ? EXPR_ANY : EXPR_CONST);
	}
    init_putv:
        if (!p->sec && (flags & DIF_CLEAR) /* container was already zero'd */
            && (s1->vtop->r & (VT_VALMASK | VT_LVAL | VT_SYM)) == VT_CONST
            && s1->vtop->c.i == 0
            && btype_size(type->t & VT_BTYPE) /* not for fp constants */
            )
            vpop(s1);
        else
            init_putv(s1, p, type, c);
    }
}

/* parse an initializer for type 't' if 'has_init' is non zero, and
   allocate space in local or global data space ('r' is either
   VT_LOCAL or VT_CONST). If 'v' is non zero, then an associated
   variable 'v' of scope 'scope' is declared before initializers
   are parsed. If 'v' is zero, then a reference to the new object
   is put in the value stack. If 'has_init' is 2, a special parsing
   is done to handle string constants. */
static void decl_initializer_alloc(TCCState *s1, CType *type, AttributeDef *ad, int r, 
                                   int has_init, int v, int scope)
{
    int size, align, addr;
    TokenString *init_str = NULL;

    Section *sec;
    Sym *flexible_array;
    Sym *sym;
    int saved_nocode_wanted = s1->nocode_wanted;
#ifdef CONFIG_TCC_BCHECK
    int bcheck = s1->do_bounds_check && !NODATA_WANTED;
#endif
    init_params p = {0};

    /* Always allocate static or global variables */
    if (v && (r & VT_VALMASK) == VT_CONST)
        s1->nocode_wanted |= 0x80000000;

    flexible_array = NULL;
    size = type_size(s1, type, &align);

    /* exactly one flexible array may be initialized, either the
       toplevel array or the last member of the toplevel struct */

    if (size < 0) {
        /* If the base type itself was an array type of unspecified size
           (like in 'typedef int arr[]; arr x = {1};') then we will
           overwrite the unknown size by the real one for this decl.
           We need to unshare the ref symbol holding that size. */
        type->ref = sym_push(s1, SYM_FIELD, &type->ref->type, 0, type->ref->c);
        p.flex_array_ref = type->ref;

    } else if (has_init && (type->t & VT_BTYPE) == VT_STRUCT) {
        Sym *field = type->ref->next;
        if (field) {
            while (field->next)
                field = field->next;
            if (field->type.t & VT_ARRAY && field->type.ref->c < 0) {
                flexible_array = field;
                p.flex_array_ref = field->type.ref;
                size = -1;
            }
        }
    }

    if (size < 0) {
        /* If unknown size, do a dry-run 1st pass */
        if (!has_init) 
            tcc_error(s1, "unknown type size");
        if (has_init == 2) {
            /* only get strings */
            init_str = tok_str_alloc(s1);
            while (s1->tok == TOK_STR || s1->tok == TOK_LSTR) {
                tok_str_add_tok(s1, init_str);
                next(s1);
            }
            tok_str_add(s1, init_str, -1);
            tok_str_add(s1, init_str, 0);
        } else
            skip_or_save_block(s1, &init_str);
        unget_tok(s1, 0);

        /* compute size */
        begin_macro(s1, init_str, 1);
        next(s1);
        decl_initializer(s1, &p, type, 0, DIF_FIRST | DIF_SIZE_ONLY);
        /* prepare second initializer parsing */
        s1->macro_ptr = init_str->str;
        next(s1);

        /* if still unknown size, error */
        size = type_size(s1, type, &align);
        if (size < 0) 
            tcc_error(s1, "unknown type size");

        /* If there's a flex member and it was used in the initializer
           adjust size.  */
        if (flexible_array && flexible_array->type.ref->c > 0)
            size += flexible_array->type.ref->c
                    * pointed_size(s1, &flexible_array->type);
    }

    /* take into account specified alignment if bigger */
    if (ad->a.aligned) {
	int speca = 1 << (ad->a.aligned - 1);
        if (speca > align)
            align = speca;
    } else if (ad->a.packed) {
        align = 1;
    }

    if (!v && NODATA_WANTED)
        size = 0, align = 1;

    if ((r & VT_VALMASK) == VT_LOCAL) {
        sec = NULL;
#ifdef CONFIG_TCC_BCHECK
        if (bcheck && v) {
            /* add padding between stack variables for bound checking */
            s1->loc -= align;
        }
#endif
        s1->loc = (s1->loc - size) & -align;
        addr = s1->loc;
        p.local_offset = addr + size;
#ifdef CONFIG_TCC_BCHECK
        if (bcheck && v) {
            /* add padding between stack variables for bound checking */
            s1->loc -= align;
        }
#endif
        if (v) {
            /* local variable */
#ifdef CONFIG_TCC_ASM
	    if (ad->asm_label) {
		int reg = asm_parse_regvar(s1, ad->asm_label);
		if (reg >= 0)
		    r = (r & ~VT_VALMASK) | reg;
	    }
#endif
            sym = sym_push(s1, v, type, r, addr);
	    if (ad->cleanup_func) {
		Sym *cls = sym_push2(s1, &s1->all_cleanups,
                    SYM_FIELD | ++s1->cur_scope->cl.n, 0, 0);
		cls->prev_tok = sym;
		cls->next = ad->cleanup_func;
		cls->ncl = s1->cur_scope->cl.s;
		s1->cur_scope->cl.s = cls;
	    }

            sym->a = ad->a;
        } else {
            /* push local reference */
            vset(s1, type, r, addr);
        }
    } else {
	sym = NULL;
        if (v && scope == VT_CONST) {
            /* see if the symbol was already defined */
            sym = sym_find(s1, v);
            if (sym) {
                if (p.flex_array_ref && (sym->type.t & type->t & VT_ARRAY)
                    && sym->type.ref->c > type->ref->c) {
                    /* flex array was already declared with explicit size
                            extern int arr[10];
                            int arr[] = { 1,2,3 }; */
                    type->ref->c = sym->type.ref->c;
                    size = type_size(s1, type, &align);
                }
                patch_storage(s1, sym, ad, type);
                /* we accept several definitions of the same global variable. */
                if (!has_init && sym->c && elfsym(s1, sym)->st_shndx != SHN_UNDEF)
                    goto no_alloc;
            }
        }

        /* allocate symbol in corresponding section */
        sec = ad->section;
        if (!sec) {
            CType *tp = type;
            while ((tp->t & (VT_BTYPE|VT_ARRAY)) == (VT_PTR|VT_ARRAY))
                tp = &tp->ref->type;
            if (tp->t & VT_CONSTANT) {
		sec = rodata_section;
            } else if (has_init) {
		sec = data_section;
                /*if (s1->g_debug & 4)
                    tcc_warning(s1, "rw data: %s", get_tok_str(v, 0));*/
            } else if (s1->nocommon)
                sec = bss_section;
        }

        if (sec) {
	    addr = section_add(sec, size, align);
#ifdef CONFIG_TCC_BCHECK
            /* add padding if bound check */
            if (bcheck)
                section_add(sec, 1, 1);
#endif
        } else {
            addr = align; /* SHN_COMMON is special, symbol value is align */
	    sec = common_section;
        }

        if (v) {
            if (!sym) {
                sym = sym_push(s1, v, type, r | VT_SYM, 0);
                patch_storage(s1, sym, ad, NULL);
            }
            /* update symbol definition */
	    put_extern_sym(s1, sym, sec, addr, size);
        } else {
            /* push global reference */
            vpush_ref(s1, type, sec, addr, size);
            sym = s1->vtop->sym;
	    s1->vtop->r |= r;
        }

#ifdef CONFIG_TCC_BCHECK
        /* handles bounds now because the symbol must be defined
           before for the relocation */
        if (bcheck) {
            addr_t *bounds_ptr;

            greloca(s1, lbounds_section, sym, lbounds_section->data_offset, R_DATA_PTR, 0);
            /* then add global bound info */
            bounds_ptr = section_ptr_add(lbounds_section, 2 * sizeof(addr_t));
            bounds_ptr[0] = 0; /* relocated */
            bounds_ptr[1] = size;
        }
#endif
    }

    if (type->t & VT_VLA) {
        int a;

        if (NODATA_WANTED)
            goto no_alloc;

        /* save before-VLA stack pointer if needed */
        if (s1->cur_scope->vla.num == 0) {
            if (s1->cur_scope->prev && s1->cur_scope->prev->vla.num) {
                s1->cur_scope->vla.locorig = s1->cur_scope->prev->vla.loc;
            } else {
                gen_vla_sp_save(s1, s1->loc -= PTR_SIZE);
                s1->cur_scope->vla.locorig = s1->loc;
            }
        }

        vla_runtime_type_size(s1, type, &a);
        gen_vla_alloc(s1, type, a);
#if defined TCC_TARGET_PE && defined TCC_TARGET_X86_64
        /* on _WIN64, because of the function args scratch area, the
           result of alloca differs from RSP and is returned in RAX.  */
        gen_vla_result(s1, addr), addr = (s1->loc -= PTR_SIZE);
#endif
        gen_vla_sp_save(s1, addr);
        s1->cur_scope->vla.loc = addr;
        s1->cur_scope->vla.num++;
    } else if (has_init) {
        p.sec = sec;
        decl_initializer(s1, &p, type, addr, DIF_FIRST);
        /* patch flexible array member size back to -1, */
        /* for possible subsequent similar declarations */
        if (flexible_array)
            flexible_array->type.ref->c = -1;
    }

 no_alloc:
    /* restore parse state if needed */
    if (init_str) {
        end_macro(s1);
        next(s1);
    }

    s1->nocode_wanted = saved_nocode_wanted;
}

/* parse a function defined by symbol 'sym' and generate its code in
   'cur_text_section' */
static void gen_function(TCCState *s1, Sym *sym)
{
    struct scope f = { 0 };
    s1->cur_scope = s1->root_scope = &f;
    s1->nocode_wanted = 0;
    s1->ind = cur_text_section->data_offset;
    if (sym->a.aligned) {
	size_t newoff = section_add(cur_text_section, 0,
				    1 << (sym->a.aligned - 1));
	gen_fill_nops(s1, newoff - s1->ind);
    }
    /* NOTE: we patch the symbol size later */
    put_extern_sym(s1, sym, cur_text_section, s1->ind, 0);
    if (sym->type.ref->f.func_ctor)
        add_array (s1, ".init_array", sym->c);
    if (sym->type.ref->f.func_dtor)
        add_array (s1, ".fini_array", sym->c);

    s1->funcname = get_tok_str(s1, sym->v, NULL);
    s1->func_ind = s1->ind;
    s1->func_vt = sym->type.ref->type;
    s1->func_var = sym->type.ref->f.func_type == FUNC_ELLIPSIS;

    /* put debug symbol */
    tcc_debug_funcstart(s1, sym);
    /* push a dummy symbol to enable local sym storage */
    sym_push2(s1 ,&s1->local_stack, SYM_FIELD, 0, 0);
    s1->local_scope = 1; /* for function parameters */
    gfunc_prolog(s1, sym);
    s1->local_scope = 0;
    s1->rsym = 0;
    clear_temp_local_var_list(s1);
    block(s1, 0);
    gsym(s1, s1->rsym);
    s1->nocode_wanted = 0;
    /* reset local stack */
    pop_local_syms(s1, NULL, 0);
    gfunc_epilog(s1);
    cur_text_section->data_offset = s1->ind;
    s1->local_scope = 0;
    label_pop(s1, &s1->global_label_stack, NULL, 0);
    sym_pop(s1, &s1->all_cleanups, NULL, 0);
    /* patch symbol size */
    elfsym(s1, sym)->st_size = s1->ind - s1->func_ind;
    /* end of function */
    tcc_debug_funcend(s1, s1->ind - s1->func_ind);
    /* It's better to crash than to generate wrong code */
    cur_text_section = NULL;
    s1->funcname = ""; /* for safety */
    s1->func_vt.t = VT_VOID; /* for safety */
    s1->func_var = 0; /* for safety */
    s1->ind = 0; /* for safety */
    s1->nocode_wanted = 0x80000000;
    check_vstack(s1);
    /* do this after funcend debug info */
    next(s1);
}

static void gen_inline_functions(TCCState *s1)
{
    Sym *sym;
    int inline_generated, i;
    struct InlineFunc *fn;

    tcc_open_bf(s1, ":inline:", 0);
    /* iterate while inline function are referenced */
    do {
        inline_generated = 0;
        for (i = 0; i < s1->nb_inline_fns; ++i) {
            fn = s1->inline_fns[i];
            sym = fn->sym;
            if (sym && (sym->c || !(sym->type.t & VT_INLINE))) {
                /* the function was used or forced (and then not internal):
                   generate its code and convert it to a normal function */
                fn->sym = NULL;
                tcc_debug_putfile(s1, fn->filename);
                begin_macro(s1, fn->func_str, 1);
                next(s1);
                cur_text_section = text_section;
                gen_function(s1, sym);
                end_macro(s1);

                inline_generated = 1;
            }
        }
    } while (inline_generated);
    tcc_close(s1);
}

static void free_inline_functions(TCCState *s1)
{
    int i;
    /* free tokens of unused inline functions */
    for (i = 0; i < s1->nb_inline_fns; ++i) {
        struct InlineFunc *fn = s1->inline_fns[i];
        if (fn->sym)
            tok_str_free(s1, fn->func_str);
    }
    dynarray_reset(s1, &s1->inline_fns, &s1->nb_inline_fns);
}

/* 'l' is VT_LOCAL or VT_CONST to define default storage type, or VT_CMP
   if parsing old style parameter decl list (and FUNC_SYM is set then) */
static int decl0(TCCState *s1, int l, int is_for_loop_init, Sym *func_sym)
{
    int v, has_init, r, oldint;
    CType type, btype;
    Sym *sym;
    AttributeDef ad, adbase;

    while (1) {
	if (s1->tok == TOK_STATIC_ASSERT) {
	    CString error_str;
	    int c;

	    next(s1);
	    skip(s1, '(');
	    c = expr_const(s1);

	    if (s1->tok == ')') {
		if (!c)
		    tcc_error(s1, "_Static_assert fail");
		next(s1);
		goto static_assert_out;
	    }

	    skip(s1, ',');
	    parse_mult_str(s1, &error_str, "string constant");
	    if (c == 0)
		tcc_error(s1, "%s", (char *)error_str.data);
	    cstr_free(s1, &error_str);
	    skip(s1, ')');
	  static_assert_out:
            skip(s1, ';');
	    continue;
	}

        oldint = 0;
        if (!parse_btype(s1, &btype, &adbase)) {
            if (is_for_loop_init)
                return 0;
            /* skip redundant ';' if not in old parameter decl scope */
            if (s1->tok == ';' && l != VT_CMP) {
                next(s1);
                continue;
            }
            if (l != VT_CONST)
                break;
            if (s1->tok == TOK_ASM1 || s1->tok == TOK_ASM2 || s1->tok == TOK_ASM3) {
                /* global asm block */
                asm_global_instr(s1);
                continue;
            }
            if (s1->tok >= TOK_UIDENT) {
               /* special test for old K&R protos without explicit int
                  type. Only accepted when defining global data */
                btype.t = VT_INT;
                oldint = 1;
            } else {
                if (s1->tok != TOK_EOF)
                    expect(s1, "declaration");
                break;
            }
        }

        if (s1->tok == ';') {
	    if ((btype.t & VT_BTYPE) == VT_STRUCT) {
		v = btype.ref->v;
		if (!(v & SYM_FIELD) && (v & ~SYM_STRUCT) >= SYM_FIRST_ANOM)
        	    tcc_warning(s1, "unnamed struct/union that defines no instances");
                next(s1);
                continue;
	    }
            if (IS_ENUM(btype.t)) {
                next(s1);
                continue;
            }
        }

        while (1) { /* iterate thru each declaration */
            type = btype;
	    ad = adbase;
            type_decl(s1, &type, &ad, &v, TYPE_DIRECT);
#if 0
            {
                char buf[500];
                type_to_str(buf, sizeof(buf), &type, get_tok_str(v, NULL));
                printf("type = '%s'\n", buf);
            }
#endif
            if ((type.t & VT_BTYPE) == VT_FUNC) {
                if ((type.t & VT_STATIC) && (l == VT_LOCAL))
                    tcc_error(s1, "function without file scope cannot be static");
                /* if old style function prototype, we accept a
                   declaration list */
                sym = type.ref;
                if (sym->f.func_type == FUNC_OLD && l == VT_CONST)
                    decl0(s1, VT_CMP, 0, sym);
#ifdef TCC_TARGET_MACHO
                if (sym->f.func_alwinl
                    && ((type.t & (VT_EXTERN | VT_INLINE))
                        == (VT_EXTERN | VT_INLINE))) {
                    /* always_inline functions must be handled as if they
                       don't generate multiple global defs, even if extern
                       inline, i.e. GNU inline semantics for those.  Rewrite
                       them into static inline.  */
                    type.t &= ~VT_EXTERN;
                    type.t |= VT_STATIC;
                }
#endif
                /* always compile 'extern inline' */
                if (type.t & VT_EXTERN)
                    type.t &= ~VT_INLINE;

            } else if (oldint) {
                tcc_warning(s1, "type defaults to int");
            }

            if (gnu_ext && (s1->tok == TOK_ASM1 || s1->tok == TOK_ASM2 || s1->tok == TOK_ASM3)) {
                ad.asm_label = asm_label_instr(s1);
                /* parse one last attribute list, after asm label */
                parse_attribute(s1, &ad);
            #if 0
                /* gcc does not allow __asm__("label") with function definition,
                   but why not ... */
                if (s1->tok == '{')
                    expect(s1, ";");
            #endif
            }

#ifdef TCC_TARGET_PE
            if (ad.a.dllimport || ad.a.dllexport) {
                if (type.t & VT_STATIC)
                    tcc_error(s1, "cannot have dll linkage with static");
                if (type.t & VT_TYPEDEF) {
                    tcc_warning(s1, "'%s' attribute ignored for typedef",
                        ad.a.dllimport ? (ad.a.dllimport = 0, "dllimport") :
                        (ad.a.dllexport = 0, "dllexport"));
                } else if (ad.a.dllimport) {
                    if ((type.t & VT_BTYPE) == VT_FUNC)
                        ad.a.dllimport = 0;
                    else
                        type.t |= VT_EXTERN;
                }
            }
#endif
            if (s1->tok == '{') {
                if (l != VT_CONST)
                    tcc_error(s1, "cannot use local functions");
                if ((type.t & VT_BTYPE) != VT_FUNC)
                    expect(s1, "function definition");

                /* reject abstract declarators in function definition
                   make old style params without decl have int type */
                sym = type.ref;
                while ((sym = sym->next) != NULL) {
                    if (!(sym->v & ~SYM_FIELD))
                        expect(s1, "identifier");
                    if (sym->type.t == VT_VOID)
                        sym->type = s1->int_type;
                }

                /* apply post-declaraton attributes */
                merge_funcattr(s1, &type.ref->f, &ad.f);

                /* put function symbol */
                type.t &= ~VT_EXTERN;
                sym = external_sym(s1, v, &type, 0, &ad);

                /* static inline functions are just recorded as a kind
                   of macro. Their code will be emitted at the end of
                   the compilation unit only if they are used */
                if (sym->type.t & VT_INLINE) {
                    struct InlineFunc *fn;
                    fn = tcc_malloc(s1, sizeof *fn + strlen(s1->file->filename));
                    strcpy(fn->filename, s1->file->filename);
                    fn->sym = sym;
		    skip_or_save_block(s1, &fn->func_str);
                    dynarray_add(s1, &s1->inline_fns,
				 &s1->nb_inline_fns, fn);
                } else {
                    /* compute text section */
                    cur_text_section = ad.section;
                    if (!cur_text_section)
                        cur_text_section = text_section;
                    gen_function(s1, sym);
                }
                break;
            } else {
		if (l == VT_CMP) {
		    /* find parameter in function parameter list */
		    for (sym = func_sym->next; sym; sym = sym->next)
			if ((sym->v & ~SYM_FIELD) == v)
			    goto found;
		    tcc_error(s1, "declaration for parameter '%s' but no such parameter",
			      get_tok_str(s1, v, NULL));
found:
		    if (type.t & VT_STORAGE) /* 'register' is okay */
		        tcc_error(s1, "storage class specified for '%s'",
				  get_tok_str(s1, v, NULL));
		    if (sym->type.t != VT_VOID)
		        tcc_error(s1, "redefinition of parameter '%s'",
				  get_tok_str(s1, v, NULL));
		    convert_parameter_type(s1, &type);
		    sym->type = type;
		} else if (type.t & VT_TYPEDEF) {
                    /* save typedefed type  */
                    /* XXX: test storage specifiers ? */
                    sym = sym_find(s1, v);
                    if (sym && sym->sym_scope == s1->local_scope) {
                        if (!is_compatible_types(&sym->type, &type)
                            || !(sym->type.t & VT_TYPEDEF))
                            tcc_error(s1, "incompatible redefinition of '%s'",
                                get_tok_str(s1, v, NULL));
                        sym->type = type;
                    } else {
                        sym = sym_push(s1, v, &type, 0, 0);
                    }
                    sym->a = ad.a;
                    sym->f = ad.f;
                    if (s1->debug_modes)
                        tcc_debug_typedef (s1, sym);
		} else if ((type.t & VT_BTYPE) == VT_VOID
			   && !(type.t & VT_EXTERN)) {
		    tcc_error(s1, "declaration of void object");
                } else {
                    r = 0;
                    if ((type.t & VT_BTYPE) == VT_FUNC) {
                        /* external function definition */
                        /* specific case for func_call attribute */
                        type.ref->f = ad.f;
                    } else if (!(type.t & VT_ARRAY)) {
                        /* not lvalue if array */
                        r |= VT_LVAL;
                    }
                    has_init = (s1->tok == '=');
                    if (has_init && (type.t & VT_VLA))
                        tcc_error(s1, "variable length array cannot be initialized");
                    if (((type.t & VT_EXTERN) && (!has_init || l != VT_CONST))
		        || (type.t & VT_BTYPE) == VT_FUNC
                        /* as with GCC, uninitialized global arrays with no size
                           are considered extern: */
                        || ((type.t & VT_ARRAY) && !has_init
                            && l == VT_CONST && type.ref->c < 0)
                        ) {
                        /* external variable or function */
                        type.t |= VT_EXTERN;
                        sym = external_sym(s1, v, &type, r, &ad);
                        if (ad.alias_target) {
                            /* Aliases need to be emitted when their target
                               symbol is emitted, even if perhaps unreferenced.
                               We only support the case where the base is
                               already defined, otherwise we would need
                               deferring to emit the aliases until the end of
                               the compile unit.  */
                            Sym *alias_target = sym_find(s1, ad.alias_target);
                            ElfSym *esym = elfsym(s1, alias_target);
                            if (!esym)
                                tcc_error(s1, "unsupported forward __alias__ attribute");
                            put_extern_sym2(s1, sym, esym->st_shndx,
                                            esym->st_value, esym->st_size, 1);
                        }
                    } else {
                        if (type.t & VT_STATIC)
                            r |= VT_CONST;
                        else
                            r |= l;
                        if (has_init)
                            next(s1);
                        else if (l == VT_CONST)
                            /* uninitialized global variables may be overridden */
                            type.t |= VT_EXTERN;
                        decl_initializer_alloc(s1, &type, &ad, r, has_init, v, l);
                    }
                }
                if (s1->tok != ',') {
                    if (is_for_loop_init)
                        return 1;
                    skip(s1, ';');
                    break;
                }
                next(s1);
            }
        }
    }
    return 0;
}

static void decl(TCCState *s1, int l)
{
    decl0(s1, l, 0, NULL);
}

/* ------------------------------------------------------------------------- */
#undef gjmp_addr
#undef gjmp
/* ------------------------------------------------------------------------- */
