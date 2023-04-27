inc_dir = include_directories('include')

library('rz-svd',
  ['rz-smt.c'],
  dependencies: [rz_core_dep, z3_dep],
  install: true,
  install_dir: plugins_dir,
  include_directories : inc_dir,
)

executable('z3_test',
  ['z3_test.c'],
  dependencies: [z3_dep],
)
