import os
from pathlib import Path
import logging

log = logging.getLogger(__name__)

def withPath(fname, thunk):
  if Path(fname).exists():
    f = open(fname, "r")
    return thunk(f)
  else:
    log.error(f"Path {fname} was required to run macro")
    return "FILENOTFOUND "+fname

def readAndStrip(f):
  return f.readline().strip()

def readTwiceAndStrip(f):
  f.readline()
  return readAndStrip(f)

def define_env(env):
  "Hook function"

  @env.macro
  def project_version():
    return withPath("version.txt", lambda f: readAndStrip(f))

  @env.macro
  def statement_coverage():
    return withPath("coverage.txt", lambda f: readAndStrip(f))

  @env.macro
  def branch_coverage():
    return withPath("coverage.txt", lambda f: readTwiceAndStrip(f))
