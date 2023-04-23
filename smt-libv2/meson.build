project('rz-smt', 'c',
  version: '0.1.0',
  license: 'LGPL3',
  meson_version: '>=0.55.0',
  default_options: [
    'buildtype=debugoptimized',
    'b_vscrt=from_buildtype',
  ]
)

rz_core_dep = dependency('rz_core')
#plugins_dir = get_option('prefix') / rz_core_dep.get_variable(pkgconfig: 'plugindir', cmake: 'rz_core_PLUGINDIR')
plugins_dir='~/.local/lib/rizin/plugins/'
message('Plugins install directory: ' + plugins_dir)
inc_dir = include_directories('include')

py3_exe = import('python').find_installation()

library('rz-svd',
  ['rz-smt.c'],
  dependencies: [rz_core_dep],
  install: true,
  install_dir: plugins_dir,
  include_directories : inc_dir,
)
