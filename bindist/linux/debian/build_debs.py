#!/usr/bin/env python3
import os
import checksumdir
import shutil
import subprocess
import sys
import tempfile
import glob
import json
import getpass

HACKDIR = "/homedoesnotexistatbuildtime/"

OLD_STYLE_CABAL = [
    "time-compat",
    "type-equality",
    "binary-orphans"
]

HERE = os.path.abspath(os.path.dirname(__file__))
ROOT = os.path.abspath(os.path.join(HERE, "../../.."))
CACHE_DIR = os.path.expanduser(os.path.join(HERE, "src_cache"))
DEB_DIR = os.path.expanduser(os.path.join(HERE, "debs"))
PLAN_JSON = os.path.join(ROOT, "dist-newstyle/cache/plan.json")

os.makedirs(CACHE_DIR, exist_ok=True)
os.makedirs(DEB_DIR, exist_ok=True)

HACKAGE_BASE_URL = "https://hackage.haskell.org/package"
HACKAGE_ARCHIVE_URL = HACKAGE_BASE_URL + "/{pkg_name}-{pkg_version}/{pkg_name}-{pkg_version}.tar.gz"
HACKAGE_CABAL_URL = HACKAGE_BASE_URL + "/{pkg_name}-{pkg_version}/{pkg_name}.cabal"




def install_debs(target_dir, package):
    debs = list(glob.glob(os.path.join(target_dir, "*.deb")))
    subprocess.run(["sudo", "dpkg", "-i"] + debs, check=True)
    return debs


def build_deb(tmpdir, package, src_dir, force=False):
    target_dir = os.path.join(DEB_DIR, os.path.basename(src_dir))

    if force:
        shutil.rmtree(target_dir, ignore_errors=True)
    elif os.path.exists(target_dir):
        return False, install_debs(target_dir, package)

    tmp_target = os.path.join(tmpdir, os.path.basename(src_dir))
    shutil.copytree(src_dir, tmp_target)
    os.chdir(tmp_target)

    cabal_debian_cmd = [
        "cabal-debian",
        "--native",
        "--disable-tests",
        "--dep-map", "pthread:libpthread-stubs0-dev"
    ]

    # clash-prelude work-around
    if package["pkg-name"] in {"clash-prelude", "clash-lib", "clash-ghc"}:
        # TODO: really, this could work :-/
        cabal_debian_cmd.append("--disable-haddock")

    # Build debian/
    subprocess.run(cabal_debian_cmd, check=True)

    if package["pkg-name"] in OLD_STYLE_CABAL:
        user = getpass.getuser()
        subprocess.run("sudo mkdir -p {}".format(HACKDIR).split(), check=True)
        subprocess.run("sudo chown {}:{} {}".format(user, user, HACKDIR).split(), check=True)

    try:
        subprocess.run("dpkg-buildpackage", check=True)
    finally:
        subprocess.run("sudo rm -rf {}".format(HACKDIR).split(), check=True)

    shutil.rmtree(tmp_target)
    shutil.move(tmpdir, target_dir)
    os.mkdir(tmpdir)
    return True, install_debs(target_dir, package)

def build_deps():
    # If we built a package, all packages following it need to be rebuilt.
    force_build = False
    for package, src_dir in prepare_all_deps():
        with tempfile.TemporaryDirectory() as tmpdir:
            (force_build_1, debs) = build_deb(tmpdir, package, src_dir, force_build)
            force_build |= force_build_1
            yield from debs

def find_clash_ghc_lib(plan):
    for package in plan:
        if package["pkg-name"] == "clash-ghc" and package["component-name"] == "lib":
            return package
    raise ValueError("Couldn't find clash-ghc library")


def is_preinstalled(package):
    return package["type"] == "pre-existing"


def get_dep_ids(plan_by_id, package):
    """
    Gather all package ids that need to be built, given a top-level package
    (usually clash-ghc). Given list is in order, but will probably contain
    duplicates.
    """
    package_id = package["id"]
    if not is_preinstalled(package):
        components = package.get("components", {})
        for dep_id in components.get("lib", {}).get("depends", ()):
            yield from get_dep_ids(plan_by_id, plan_by_id[dep_id])
        for dep_id in package.get("depends", ()):
            yield from get_dep_ids(plan_by_id, plan_by_id[dep_id])
        yield package_id


def filter_deps(plan_by_id, dep_ids):
    """
    Filter duplicate dep ids by pkg-name/pkg-version. Makes sure to keep the
    order of deps.
    """
    seen = set()
    for dep_id in dep_ids:
        dep = plan_by_id[dep_id]
        key = (dep["pkg-name"], dep["pkg-version"])
        if key not in seen:
            yield dep_id
        seen.add(key)


def prepare_local_dep(package):
    """Copy locally availabe source code to cache directory."""
    pkg_name = package["pkg-name"]
    pkg_version = package["pkg-version"]
    dirhash = checksumdir.dirhash(package["pkg-src"]["path"])
    target_dirname = "{}-{}-{}".format(pkg_name, pkg_version, dirhash)
    target = os.path.join(CACHE_DIR, target_dirname)
    if os.path.exists(target): shutil.rmtree(target)
    shutil.copytree(package["pkg-src"]["path"], target)
    return target


def preprare_hackage_dep(package):
    """Fetch package from hackage and copy it to cache directory."""
    pkg_name = package["pkg-name"]
    pkg_version = package["pkg-version"]
    target_dirname = package["id"]
    target = os.path.join(CACHE_DIR, target_dirname)
    archive_name = "{pkg_name}-{pkg_version}.tar.gz".format(**locals())
    hackage_archive_url = HACKAGE_ARCHIVE_URL.format(**locals())
    hackage_cabal_url = HACKAGE_CABAL_URL.format(**locals())

    if os.path.exists(target):
        return target

    with tempfile.TemporaryDirectory() as tmpdir:
        archive_dir = os.path.join(tmpdir, "{pkg_name}-{pkg_version}".format(**locals()))
        subprocess.run(["wget", hackage_archive_url], cwd=tmpdir, check=True)
        subprocess.run(["tar", "xzf", archive_name], cwd=tmpdir, check=True)
        os.remove(os.path.join(archive_dir, "{pkg_name}.cabal".format(**locals())))
        subprocess.run(["wget", hackage_cabal_url], cwd=archive_dir, check=True)
        shutil.move(archive_dir, target)

    return target


def prepare_git_dep(package):
    """Clone git repository and copy it to cache directory"""
    # TODO: Account for 'subdir' directive
    tag = package["pkg-src"]["source-repo"]["tag"]
    pkg_name = package["pkg-name"]
    target_dirname = "{}-{}".format(pkg_name, tag)

    # Check for existing cache
    target_dir = os.path.join(CACHE_DIR, target_dirname)
    if os.path.exists(target_dir):
        return target_dir

    location = package["pkg-src"]["source-repo"]["location"]
    with tempfile.TemporaryDirectory() as tmpdir:
        tmp_target = os.path.join(tmpdir, target_dirname)
        subprocess.run(["git", "clone", location, tmp_target], check=True)
        subprocess.run(["git", "checkout", tag], check=True, cwd=tmp_target)
        shutil.move(tmp_target, target_dir)

    return target_dir


def prepare_dep(package):
    """Fetch a given package and copy it to cache directory"""
    src = package.get("pkg-src", {
        "type": "repo-tar",
        "repo": {
            "type": "secure-repo",
            "uri": "http://hackage.haskell.org/"
        }
    })

    if src["type"] == "repo-tar":
        return preprare_hackage_dep(package)
    elif src["type"] == "source-repo" and src["source-repo"]["type"] == "git":
        return prepare_git_dep(package)
    elif src["type"] == "local":
        return prepare_local_dep(package)
    else:
        raise ValueError(
            "Unrecognized type {!r} in {!r}".format(src["type"], package["id"])
        )


def prepare_all_deps():
    """Fetch source code for clash-ghc and its dependencies"""
    os.makedirs(CACHE_DIR, exist_ok=True)

    plan = json.load(open(PLAN_JSON))["install-plan"]
    plan_by_id = {package["id"]: package for package in plan}

    clash_ghc = find_clash_ghc_lib(plan)
    dep_ids = get_dep_ids(plan_by_id, clash_ghc)
    filtered_dep_ids = filter_deps(plan_by_id, dep_ids)

    for dep_id in filtered_dep_ids:
        src_dir = prepare_dep(plan_by_id[dep_id])
        yield (plan_by_id[dep_id], src_dir)

def main():
    """
    Generate Debian packages for clash-ghc and its dependencies. List files
    separated by spaces in '$cwd/debs'. 
    """
    with open("debs.txt", "w") as debsf:
        for dep in build_deps():
            debsf.write(dep)
            debsf.write(" ")
            debsf.flush()

if __name__ == '__main__':
    main()

