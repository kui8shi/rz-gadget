#include "rz_analysis.h"
#include "rz_il/rz_il_opcodes.h"
#include <rz-gadget.h>
#include <rz_core.h>

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

#include <json.h>

const char *json_parse_error_to_str(enum json_parse_error_e e) {
  switch (e) {

    /* no error occurred (huzzah!). */
  case json_parse_error_none:
    return "json_parse_error_none";

    /* expected either a comma or a closing '}' or ']' to close an object or. */
    /* array! */
  case json_parse_error_expected_comma_or_closing_bracket:
    return "json_parse_error_expected_comma_or_closing_bracket";

    /* colon separating name/value pair was missing! */
  case json_parse_error_expected_colon:
    return "json_parse_error_expected_colon";

    /* expected string to begin with '"'! */
  case json_parse_error_expected_opening_quote:
    return "json_parse_error_expected_opening_quote";

    /* invalid escaped sequence in string! */
  case json_parse_error_invalid_string_escape_sequence:
    return "json_parse_error_invalid_string_escape_sequence";

    /* invalid number format! */
  case json_parse_error_invalid_number_format:
    return "json_parse_error_invalid_number_format";

    /* invalid value! */
  case json_parse_error_invalid_value:
    return "json_parse_error_invalid_value";

    /* reached end of buffer before object/array was complete! */
  case json_parse_error_premature_end_of_buffer:
    return "json_parse_error_premature_end_of_buffer";

    /* string was malformed! */
  case json_parse_error_invalid_string:
    return "json_parse_error_invalid_string";

    /* a call to malloc, or a user provider allocator, failed. */
  case json_parse_error_allocator_failed:
    return "json_parse_error_allocator_failed";

    /* the JSON input had unexpected trailing characters that weren't part of
       the. JSON value. */
  case json_parse_error_unexpected_trailing_characters:
    return "json_parse_error_unexpected_trailing_characters";

    /* catch-all error for everything else that exploded (real bad chi!). */
  case json_parse_error_unknown:
    return "json_parse_error_unknown";
  };
  return "why you see this";
}
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
  // RzStrBuf *buf = rz_strbuf_new("");
  // rz_il_op_effect_stringify(op->il_op, buf);
  struct pj_t *json_buf = pj_new();
  rz_il_op_effect_json(op->il_op, json_buf);
  char *json_str = pj_string(json_buf);
  struct json_parse_result_s reason = {0};
  struct json_value_s *root =
      json_parse_ex(json_str, strlen(json_str), json_parse_flags_default, NULL,
                    NULL, &reason);
  if (!root) {
    printf("json parse failed : json_str: '%s'\n"
           "err_code:%s\n"
           "err_string:%.10s\n",
           json_str, json_parse_error_to_str(reason.error),
           json_str + reason.error_row_no);
    return false;
  }
  json_str = (char *)json_write_pretty(root, NULL, NULL, NULL);
  printf("op at 0x%llx ... \n\tmnemonic:%s\n\til(json):\n%s\n", addr,
         op->mnemonic, json_str);
  printf("%s\n", pj_string(json_buf));
  return true;
}
