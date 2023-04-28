#include "rz_analysis.h"
#include "rz_il/rz_il_opcodes.h"
#include <rz-gadget.h>

static const RzCmdDescArg cmd_rop_args[] = {
    {
        .name = "Flag or Address of basic block to translate to SMT-LIBv2",
        .type = RZ_CMD_ARG_TYPE_FLAG,
    },
    {0},
};

static const RzCmdDescHelp rop_usage = {
    .summary = "Rizin Extra Plugin to automate building ROP chain",
    .args = cmd_rop_args,
};

RZ_IPI RzCmdStatus rz_cmd_rop_handler(RzCore *core, int argc,
                                      const char **argv) {
  if (argc < 2) {
    return RZ_CMD_STATUS_WRONG_ARGS;
  }
  if (!rop_smtlib(core, argv[1])) {
    return RZ_CMD_STATUS_ERROR;
  }
  return RZ_CMD_STATUS_OK;
}

static bool rz_cmd_rop_init(RzCore *core) {
  RzCmd *rcmd = core->rcmd;
  RzCmdDesc *root_cd = rz_cmd_get_root(rcmd);
  if (!root_cd) {
    rz_warn_if_reached();
    return false;
  }

  RzCmdDesc *rop = rz_cmd_desc_argv_new(rcmd, root_cd, "rop",
                                        rz_cmd_rop_handler, &rop_usage);
  if (!rop) {
    rz_warn_if_reached();
    return false;
  }

  return true;
}

RzCorePlugin rz_core_plugin_rop = {
    .name = "rz-gadget",
    .desc = "Rizin Extra Plugin to automate building ROP chain",
    .license = "LGPL3",
    .version = "0.1.0",
    .author = "ez2take",
    .init = rz_cmd_rop_init,
};

#ifndef CORELIB
RZ_API RzLibStruct rizin_plugin = {
    .type = RZ_LIB_TYPE_CORE,
    .data = &rz_core_plugin_rop,
    .version = RZ_VERSION,
};
#endif

bool rop_smtlib(RzCore *core, const char *expr) {
  printf("[rop_smtlib]\n");
  ut64 addr = rz_num_math(core->num, expr);
  if (core->num->nc.errors) {
    RZ_LOG_ERROR("unknown address '%s'\n", core->num->nc.calc_buf);
    return false;
  }
  //  RzAnalysisBlock *block = rz_analysis_get_block_at(core->analysis, addr);
  RzAnalysisOp *op = rz_core_analysis_op(core, addr, RZ_ANALYSIS_OP_MASK_ALL);
  if (!op) {
    RZ_LOG_ERROR("no op at 0x%llx\n", addr);
    return false;
  }
  //  printf("basic block at 0x%llx ... %s\n", addr, block->op_bytes);
  RzStrBuf *buf = rz_strbuf_new("");
  rz_il_op_effect_stringify(op->il_op, buf);
  printf("op at 0x%llx ... \n\tmnemonic:%s\n\tRzIL:\n%s\n", addr, op->mnemonic,
         rz_strbuf_get(buf));
  return true;
}
