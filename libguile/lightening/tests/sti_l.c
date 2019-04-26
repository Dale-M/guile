#include "test.h"

static uint64_t data[] = { 0x1212121212121212, 0, 0x3434343434343434 };

static void
run_test(jit_state_t *j, uint8_t *arena_base, size_t arena_size)
{
#if __WORDSIZE > 32
  jit_begin(j, arena_base, arena_size);
  jit_load_args_1(j, jit_operand_gpr (JIT_OPERAND_ABI_INT64, JIT_R1));

  jit_sti_l(j, &data[1], JIT_R1);
  jit_ret(j);

  void (*f)(int64_t) = jit_end(j, NULL);

  ASSERT(data[0] == 0x1212121212121212);
  ASSERT(data[1] == 0x00);
  ASSERT(data[2] == 0x3434343434343434);
  f(-1);
  ASSERT(data[0] == 0x1212121212121212);
  ASSERT(data[1] == 0xffffffffffffffff);
  ASSERT(data[2] == 0x3434343434343434);
#endif
}

int
main (int argc, char *argv[])
{
  return main_helper(argc, argv, run_test);
}