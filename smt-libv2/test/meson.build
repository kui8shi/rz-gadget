project('test_smt', 'c',
  version: '0.1.0',
  license: 'LGPL3',
  meson_version: '>=0.55.0',
  default_options: [
    'buildtype=debug',
    'b_vscrt=from_buildtype',
  ]
)

rz_core_dep = dependency('rz_core')
z3_dep=dependency('z3')

inc_dir = include_directories('../include')

executable('test_rz-gadget',
  ['z3_test.c','il_test.c'],
  dependencies: [rz_core_dep, z3_dep],
  include_directories : inc_dir,
)
