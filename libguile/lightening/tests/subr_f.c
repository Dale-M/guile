#include "test.h"

static void
run_test(jit_state_t *j, uint8_t *arena_base, size_t arena_size)
{
  jit_begin(j, arena_base, arena_size);
  jit_load_args_2(j, jit_operand_fpr (JIT_OPERAND_ABI_FLOAT, JIT_F0),
                  jit_operand_fpr (JIT_OPERAND_ABI_FLOAT, JIT_F1));

  jit_subr_f(j, JIT_F0, JIT_F0, JIT_F1);
  jit_retr_f(j, JIT_F0);

  size_t size = 0;
  void* ret = jit_end(j, &size);

  float (*f)(float, float) = ret;
  ASSERT(f(42.f, 69.f) == -27.f);
  ASSERT(f(42.0f, 69.5f) == -27.5f);
}

int
main (int argc, char *argv[])
{
  return main_helper(argc, argv, run_test);
}