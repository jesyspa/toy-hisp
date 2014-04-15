from SCons.Script.SConscript import SConsEnvironment
import glob, os

vars = Variables(None, ARGUMENTS)
vars.Add(BoolVariable('release', 'Set to build for release', False))
vars.Add('problem', 'Problem to solve', 'real')

########  FLAGS  ########

flags = {
   'cpp': ["-Wall", "-Wextra", "-Werror", "-std=c++1y", "-stdlib=libc++"],
   'link': ["-lc++abi"],
   'debug': ["-O0", "-g3"],
   'release': ["-O2", "-g", "-DNDEBUG"]
}

######  END FLAGS  ######

##### ENVIRONMENTS  #####

env = Environment(variables=vars, CXX="clang++", CPPFLAGS=flags['cpp'], LINKFLAGS=flags['link'], ENV=os.environ)

prefix = 'release' if env['release'] else 'debug'

env.MergeFlags(flags[prefix])
env['prefix'] = prefix

Export('env')

#### END ENVIRONMENTS ###

env.SConscript("runtime/SConscript", variant_dir='.' + prefix + '_build', duplicate=0)

