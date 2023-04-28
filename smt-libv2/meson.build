inc_dir = include_directories('include')

library('rz-svd',
  ['rz-smt.c', 'il_to_z3.c'],
  dependencies: [rz_core_dep, z3_dep],
  install: true,
  install_dir: plugins_dir,
  include_directories : inc_dir,
)
