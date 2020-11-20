#!/usr/bin/env python3
import os
import sys
import json
import subprocess
import shutil
import datetime

os.environ["DEBIAN_FRONTEND"] = "noninteractive"

DISTRIBUTION=sys.argv[1]
NOW=datetime.datetime.utcnow().strftime("%Y%m%dT%H%M%SZ")

SCRIPT_DIR=os.path.dirname(os.path.abspath(__file__))
METAPACKAGES_DIR=os.path.abspath(os.path.join(SCRIPT_DIR, "..", "metapackages"))
BUILDINFO_DIR=os.path.abspath(os.path.join(SCRIPT_DIR, "..", DISTRIBUTION))
BUILD_DIR=os.path.abspath(os.path.join(BUILDINFO_DIR, "build"))

class cd:
    """
    Context manager for changing the current working directory

    Credits: https://stackoverflow.com/a/13197763
    """
    def __init__(self, new_path):
        self.new_path = os.path.expanduser(new_path)

    def __enter__(self):
        self.old_path = os.getcwd()
        os.chdir(self.new_path)

    def __exit__(self, etype, value, traceback):
        os.chdir(self.old_path)

def run(*args, **kwargs):
  """Same as subprocess.run(), but forces 'check' to True"""
  kwargs["check"] = True
  print("+", " ".join(args[0]))
  return subprocess.run(*args, **kwargs)

def get_buildinfo():
  """"Parse and return 'buildinfo.json'"""
  path = os.path.join(SCRIPT_DIR, "..", DISTRIBUTION, "buildinfo.json")
  return json.load(open(path, "rb"))

def apt_install(pkgs):
  """Install a package using system's 'apt'"""
  return run(["apt-get", "install", "-y"] + list(pkgs))

def cabal_update():
  return run(["cabal", "update"])

def cabal_get(name, unpack_to, version=None):
  """Fetch a package from hackage and unpack it"""
  pkg = name
  if version is not None:
    pkg += "=="
    pkg += version

  return run(["cabal", "get", pkg, "-d", unpack_to])

def local_get(name, unpack_to, dir):
  os.makedirs(unpack_to)
  shutil.copytree(
    os.path.abspath(os.path.join(BUILDINFO_DIR, dir)),
    os.path.join(unpack_to, os.path.basename(dir))
  )

def build_and_install(name, cabal_debian_options=()):
  # Create debian/
  cmd = ["cabal-debian"]
  cmd += ["--native"]
  cmd += ["--dep-map", "pthread:libpthread-stubs0-dev"]
  cmd += ["--revision", "-" + NOW]
  cmd += list(cabal_debian_options)
  run(cmd)

  # Install dependencies
  run([
    "mk-build-deps", "--install",
    "--tool", "apt -o Debug::pkgProblemResolver=yes -y"
  ])

  # Build package
  run(["dpkg-buildpackage"])

  with cd(".."):
    apt_install("./" + p for p in os.listdir(".") if p.endswith(".deb"))

def main():
  # Install helpers
  buildinfo = get_buildinfo()
  apt_install(buildinfo["build-dependencies"])

  # Build packages
  cabal_update()
  for pkg in buildinfo.get("packages", []):
    unpack_dir = os.path.join(BUILD_DIR, pkg["name"])

    src = pkg["src"]
    src_type = src["type"]
    del src["type"]

    if src_type == "hackage":
      cabal_get(pkg["name"], unpack_dir, **src)
    elif src_type == "local":
      local_get(pkg["name"], unpack_dir, **src)
    else:
      raise Exception("Unrecognized src type: {}".format(src_type))

    [pkg_dir] = os.listdir(unpack_dir)
    pkg_dir = os.path.join(unpack_dir, pkg_dir)
    with cd(pkg_dir):
      build_and_install(pkg, pkg.get("cabal_debian_options", []))
    shutil.rmtree(pkg_dir)

  # Build metapackages
  with cd(METAPACKAGES_DIR):
    for nm in os.listdir("."):
      run(["equivs-build", nm])

    for deb in [p for p in os.listdir(".") if p.endswith(".deb")]:
      shutil.move(deb, BUILD_DIR)

  # Build package index
  with cd(BUILD_DIR):
    run(["bash", "-c", "dpkg-scanpackages . > Packages"])


if __name__ == '__main__':
  shutil.rmtree(BUILD_DIR, ignore_errors=True)
  os.makedirs(BUILD_DIR)
  main()
