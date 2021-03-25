/*************************************************************/
/*
 *  RISCV64 dummy assembler for TCC
 *
 */

#ifdef TARGET_DEFS_ONLY

#define CONFIG_TCC_ASM
#define NB_ASM_REGS 32

ST_FUNC void g(TCCState *s1, int c);
ST_FUNC void gen_le16(TCCState *s1, int c);
ST_FUNC void gen_le32(TCCState *s1, int c);

/*************************************************************/
#else
/*************************************************************/
#define USING_GLOBALS
#include "tcc.h"

static void asm_error(TCCState *s1)
{
    tcc_error(s1, "RISCV64 asm not implemented.");
}

/* XXX: make it faster ? */
ST_FUNC void g(TCCState *s1, int c)
{
    int ind1;
    if (s1->nocode_wanted)
        return;
    ind1 = s1->ind + 1;
    if (ind1 > cur_text_section->data_allocated)
        section_realloc(cur_text_section, ind1);
    cur_text_section->data[s1->ind] = c;
    s1->ind = ind1;
}

ST_FUNC void gen_le16 (TCCState *s1, int i)
{
    g(s1, i);
    g(s1, i>>8);
}

ST_FUNC void gen_le32 (TCCState *s1, int i)
{
    gen_le16(s1, i);
    gen_le16(s1, i>>16);
}

ST_FUNC void gen_expr32(TCCState *s1, ExprValue *pe)
{
    gen_le32(s1, pe->v);
}

ST_FUNC void asm_opcode(TCCState *s1, int opcode)
{
    asm_error(s1);
}

ST_FUNC void subst_asm_operand(TCCState *s1, CString *add_str, SValue *sv, int modifier)
{
    asm_error(s1);
}

/* generate prolog and epilog code for asm statement */
ST_FUNC void asm_gen_code(TCCState *s1, ASMOperand *operands, int nb_operands,
                         int nb_outputs, int is_output,
                         uint8_t *clobber_regs,
                         int out_reg)
{
}

ST_FUNC void asm_compute_constraints(TCCState *s1, ASMOperand *operands,
                                    int nb_operands, int nb_outputs,
                                    const uint8_t *clobber_regs,
                                    int *pout_reg)
{
}

ST_FUNC void asm_clobber(TCCState *s1, uint8_t *clobber_regs, const char *str)
{
    asm_error(s1);
}

ST_FUNC int asm_parse_regvar (TCCState *s1, int t)
{
    asm_error(s1);
    return -1;
}

/*************************************************************/
#endif /* ndef TARGET_DEFS_ONLY */
