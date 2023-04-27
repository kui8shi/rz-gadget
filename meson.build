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
z3_dep=dependency('z3')
#z3_dep = cmake.FetchContent_Declare(z3
#        GIT_REPOSITORY https://github.com/Z3Prover/z3
#        GIT_TAG        z3-4.12.1
#)
#FetchContent_MakeAvailable(z3)
#plugins_dir = get_option('prefix') / rz_core_dep.get_variable(pkgconfig: 'plugindir', cmake: 'rz_core_PLUGINDIR')
plugins_dir='~/.local/lib/rizin/plugins/'
message('Plugins install directory: ' + plugins_dir)
subdir('smt-libv2')

