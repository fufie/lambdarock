#    -*- Python -*-

import os, sys

def get_msvc_dir(toolkit):
    return 'd:\\program files\\Microsoft Visual Studio 9.0\\'

def get_winsdk_dir():
    return 'd:\\program files\\Microsoft SDKs\\Windows\\v6.1\\'

# very windows so far
env = Environment(MSVS_USE_MFC_DIRS = True,
                  LIB_TOOLSET = 'vc90',
                  MSVS_VERSION = '9.0')

msvcdir = get_msvc_dir('vc90')
sdkdir = get_winsdk_dir()

env['ENV']['INCLUDE'] = msvcdir + 'VC\\include;' + sdkdir + 'include;' 
env['ENV']['LIB'] = msvcdir + 'VC\\lib;' + sdkdir + 'lib;' 
env['ENV']['PATH'] = msvcdir + 'Common7\\IDE;' + msvcdir + 'VC\\bin;' + msvcdir + 'Common7\\Tools;' + sdkdir + 'bin;'            

env.Append(CPPDEFINES=[ 'WIN32', '__WIN32__', 'DEBUG', '_DEBUG', 'EBUG', 'WIN_MAKEDLL'])

exports = {'env': env}

dirs = ['libs/zterm']

for d in dirs:
    env.SConscript(os.path.join(d,'SConscript'), exports = exports, duplicate=0)
