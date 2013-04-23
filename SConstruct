Help("""
'scons debug' for making the debug build
'scons release' for making the release build
""")

########  FLAGS  ########

flags = {
   'cpp': ["-Wall", "-Wextra", "-pedantic", "-Werror", "-std=c++1y"],
   'debug': ["-O0", "-g3", "-D_GLIBCXX_DEBUG"],
   'release': ["-O2", "-g", "-DNDEBUG"],
}

Export('flags')

######  END FLAGS  ######

##### ENVIRONMENTS  #####

base_env = Environment(CXX="g++", CPPFLAGS=flags['cpp'])

dbg_hisp_env = base_env.Clone()
dbg_hisp_env.MergeFlags([flags['debug']])

rel_hisp_env = base_env.Clone()
rel_hisp_env.MergeFlags([flags['release']])

Export('base_env')

#### END ENVIRONMENTS ###

dbg_exports = {
   'env': dbg_hisp_env,
   'prefix': 'dbg',
}

rel_exports = {
   'env': rel_hisp_env,
   'prefix': '',
}

SConscript("runtime/SConscript", exports=dbg_exports, variant_dir='.dbgbuild')
SConscript("runtime/SConscript", exports=rel_exports, variant_dir='.relbuild')

Alias('dbg', '#lib/libdbghisp.a')
Alias('release', '#lib/libhisp.a')

Default('dbg')

