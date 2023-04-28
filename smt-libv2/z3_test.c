#include <stdlib.h>
#include <z3_api.h>

#define LOG_Z3_CALLS

#ifdef LOG_Z3_CALLS
#define LOG_MSG(msg) Z3_append_log(msg)
#else
#define LOG_MSG(msg) ((void)0)
#endif

void exitf(const char *message) {
  fprintf(stderr, "BUG: %s.\n", message);
  exit(1);
}
void error_handler(Z3_context c, Z3_error_code e) {
  printf("Error code: %d\n", e);
  exitf("incorrect use of Z3");
}
Z3_context mk_context_custom(Z3_config cfg, Z3_error_handler err) {
  Z3_context ctx;

  Z3_set_param_value(cfg, "model", "true");
  ctx = Z3_mk_context(cfg);
  Z3_set_error_handler(ctx, err);

  return ctx;
}
Z3_context mk_context() {
  Z3_config cfg;
  Z3_context ctx;
  cfg = Z3_mk_config();
  ctx = mk_context_custom(cfg, error_handler);
  Z3_del_config(cfg);
  return ctx;
}
Z3_solver mk_solver(Z3_context ctx) {
  Z3_solver s = Z3_mk_solver(ctx);
  Z3_solver_inc_ref(ctx, s);
  return s;
}

void del_solver(Z3_context ctx, Z3_solver s) { Z3_solver_dec_ref(ctx, s); }

Z3_ast mk_var(Z3_context ctx, const char *name, Z3_sort ty) {
  Z3_symbol s = Z3_mk_string_symbol(ctx, name);
  return Z3_mk_const(ctx, s, ty);
}
Z3_ast mk_unary_app(Z3_context ctx, Z3_func_decl f, Z3_ast x) {
  Z3_ast args[1] = {x};
  return Z3_mk_app(ctx, f, 1, args);
}

Z3_ast mk_binary_app(Z3_context ctx, Z3_func_decl f, Z3_ast x, Z3_ast y) {
  Z3_ast args[2] = {x, y};
  return Z3_mk_app(ctx, f, 2, args);
}

void check(Z3_context ctx, Z3_solver s, Z3_lbool expected_result) {
  Z3_model m = 0;
  printf("%s\n", Z3_solver_to_string(ctx, s));
  Z3_lbool result = Z3_solver_check(ctx, s);
  switch (result) {
  case Z3_L_FALSE:
    printf("unsat\n");
    break;
  case Z3_L_UNDEF:
    printf("unknown\n");
    m = Z3_solver_get_model(ctx, s);
    if (m)
      Z3_model_inc_ref(ctx, m);
    printf("potential model:\n%s\n", Z3_model_to_string(ctx, m));
    break;
  case Z3_L_TRUE:
    m = Z3_solver_get_model(ctx, s);
    if (m)
      Z3_model_inc_ref(ctx, m);
    printf("sat\n%s\n", Z3_model_to_string(ctx, m));
    break;
  }
  if (result != expected_result) {
    exitf("unexpected result");
  }
  if (m)
    Z3_model_dec_ref(ctx, m);
}

/**
   \brief Prove that the constraints already asserted into the logical
   context implies the given formula.  The result of the proof is
   displayed.

   Z3 is a satisfiability checker. So, one can prove \c f by showing
   that <tt>(not f)</tt> is unsatisfiable.

   The context \c ctx is not modified by this function.
*/
void prove(Z3_context ctx, Z3_solver s, Z3_ast f, bool is_valid) {
  Z3_model m = 0;
  Z3_ast not_f;

  /* save the current state of the context */
  Z3_solver_push(ctx, s);

  not_f = Z3_mk_not(ctx, f);
  Z3_solver_assert(ctx, s, not_f);

  printf("%s\n", Z3_solver_to_string(ctx, s));

  switch (Z3_solver_check(ctx, s)) {
  case Z3_L_FALSE:
    /* proved */
    printf("valid\n");
    if (!is_valid) {
      exitf("unexpected result");
    }
    break;
  case Z3_L_UNDEF:
    /* Z3 failed to prove/disprove f. */
    printf("unknown\n");
    m = Z3_solver_get_model(ctx, s);
    if (m != 0) {
      Z3_model_inc_ref(ctx, m);
      /* m should be viewed as a potential counterexample. */
      printf("potential counterexample:\n%s\n", Z3_model_to_string(ctx, m));
    }
    if (is_valid) {
      exitf("unexpected result");
    }
    break;
  case Z3_L_TRUE:
    /* disproved */
    printf("invalid\n");
    m = Z3_solver_get_model(ctx, s);
    if (m) {
      Z3_model_inc_ref(ctx, m);
      /* the model returned by Z3 is a counterexample */
      printf("counterexample:\n%s\n", Z3_model_to_string(ctx, m));
    }
    if (is_valid) {
      exitf("unexpected result");
    }
    break;
  }
  if (m)
    Z3_model_dec_ref(ctx, m);

  /* restore scope */
  Z3_solver_pop(ctx, s, 1);
}
void bitvector_example1() {
  Z3_context ctx = mk_context();
  Z3_solver s = mk_solver(ctx);
  Z3_sort bv_sort;
  Z3_ast x, zero, ten, x_minus_ten, c1, c2, thm;

  printf("\nbitvector_example1\n");
  LOG_MSG("bitvector_example1");

  bv_sort = Z3_mk_bv_sort(ctx, 32);

  x = mk_var(ctx, "x", bv_sort);
  zero = Z3_mk_numeral(ctx, "0", bv_sort);
  ten = Z3_mk_numeral(ctx, "10", bv_sort);
  x_minus_ten = Z3_mk_bvsub(ctx, x, ten);
  /* bvsle is signed less than or equal to */
  c1 = Z3_mk_bvsle(ctx, x, ten);
  c2 = Z3_mk_bvsle(ctx, x_minus_ten, zero);
  thm = Z3_mk_iff(ctx, c1, c2);
  printf("disprove: x - 10 <= 0 IFF x <= 10 for (32-bit) machine integers\n");
  prove(ctx, s, thm, false);

  del_solver(ctx, s);
  Z3_del_context(ctx);
}
int main(int argc, char **argv) {
  bitvector_example1();
  return 0;
}
