/*
 * Copyright (c) 2017 Matthew Justin Bauer
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 *
 * This file is based on @lethalman's nix-user-chroot. This file has
 * diverged from it though.
 *
 * Usage: nix-user-chroot <nixpath> <command>
 */

#include <sched.h>
#include <unistd.h>
#include <stdlib.h>
#include <sys/wait.h>
#include <signal.h>
#include <fcntl.h>
#include <stdio.h>
#include <string.h>
#include <limits.h>
#include <errno.h>
#include <sys/mount.h>
#include <sys/types.h>
#include <dirent.h>
#include <sys/stat.h>
#include <stdint.h>
#include <list>
#include <string>

using namespace std;

#define err_exit(format, ...) { fprintf(stderr, format ": %s\n", ##__VA_ARGS__, strerror(errno)); exit(EXIT_FAILURE); }
static int child_proc(const char *rootdir, const char *nixdir, const char *pwd, uint8_t clear_env, list<struct DirMapping> dirMappings, list<struct SetEnv> envMappings, const char *executable, char * const new_argv[]);

volatile uint8_t child_died = 0;
int child_pid = 0;

static void usage(const char *pname) {
    fprintf(stderr, "Usage: %s -n <nixpath> -- <command>\n", pname);
    fprintf(stderr, "\t-c\tclear all env vars\n");
    fprintf(stderr, "\t-m <src>:<src>\tmap src on the host to dest in the sandbox\n");
    fprintf(stderr, "\t-d\tdelete all default dir mappings, may break things\n");
    fprintf(stderr, "\t-p <var>\tpreserve the value of a variable across the -c clear\n");
    fprintf(stderr, "\t-w <var>\tset the working directory for the chrooted command\n");
    fprintf(stderr, "\t-e\tadd an /escape-hatch to the sandbox, and run (outside the sandbox) any strings written to it\n");

    exit(EXIT_FAILURE);
}

static void update_map(const char *mapping, const char *map_file) {
    int fd;

    fd = open(map_file, O_WRONLY);
    if (fd < 0) {
        err_exit("map open");
    }

    int map_len = strlen(mapping);
    if (write(fd, mapping, map_len) != map_len) {
        err_exit("map write");
    }

    close(fd);
}

static void add_path(string src, string dest, string rootdir) {
  string path_buf2;

  struct stat statbuf;
  if (stat(src.c_str(), &statbuf) < 0) {
    fprintf(stderr, "Cannot stat %s: %s\n", src.c_str(), strerror(errno));
    return;
  }

  path_buf2 = rootdir + "/" + dest;

  if (S_ISDIR(statbuf.st_mode)) {
    mkdir(path_buf2.c_str(), statbuf.st_mode & ~S_IFMT);
    if (mount(src.c_str(), path_buf2.c_str(), "none", MS_BIND | MS_REC, NULL) < 0) {
      fprintf(stderr, "Cannot bind mount %s to %s: %s\n", src.c_str(), path_buf2.c_str(), strerror(errno));
    }
  } else if (S_ISREG(statbuf.st_mode)) {
    printf("bind-mounting file %s not supported", src.c_str());
  }
}

struct DirMapping {
  string src;
  string dest;
};

struct SetEnv {
  string key;
  string value;
};

struct DirMapping parseMapping(string input) {
  auto pos = input.find(":");
  string src = input.substr(0, pos);
  string dest = input.substr(pos + 1);
  return (struct DirMapping){ src, dest };
}

static void handle_child_death(int signo, siginfo_t *info, void *context) {
  if ( (child_pid == 0) || (info->si_pid == child_pid) ) {
    child_died = 1;
  }
}

int main(int argc, char *argv[]) {
  uint8_t clear_env = 0;
  uint8_t enable_escape_hatch = 0;
  char *nixdir = NULL;
  char *pwd = NULL;
  list<struct DirMapping> dirMappings;
  list<struct SetEnv> envMappings;
  const char *t;

#define x(y) dirMappings.push_back({ "/" y, y })
  x("dev");
  x("proc");
  x("sys");
  x("run");
  x("tmp");
  x("var");
  x("etc");
  x("usr");
  x("home");
  x("root");
#undef x

  int opt;
  while ((opt = getopt(argc, argv, "cew:n:m:dp:")) != -1) {
    switch (opt) {
    case 'c':
      clear_env = 1;
      break;
    case 'e':
      enable_escape_hatch = 1;
      break;
    case 'n':
      // determine absolute directory for nix dir
      nixdir = realpath(optarg, NULL);
      if (!nixdir) {
        err_exit("realpath(%s)", optarg);
      }
      break;
    case 'm':
      dirMappings.push_back(parseMapping(optarg));
      break;
    case 'd':
      dirMappings.clear();
      break;
    case 'w':
      pwd = realpath(optarg, NULL);
      if (!pwd) {
        err_exit("realpath(%s)", optarg)
      }
    case 'p':
      t = getenv(optarg);
      if (t) {
        envMappings.push_back({ optarg, t });
      }
      break;
    }
  }

  if (!nixdir) {
    fprintf(stderr, "-n <nixdir> is required\n");
    exit(EXIT_FAILURE);
  }

  if (!pwd) {
    pwd = ((char*)std::string( "/" ).c_str());
  }

  if (argc <= optind) {
    usage(argv[0]);
  }

  const char *tmpdir = getenv("TMPDIR");
  if (!tmpdir) {
    tmpdir = "/tmp";
  }

  char template_[PATH_MAX];
  int needed = snprintf(template_, PATH_MAX, "%s/nixXXXXXX", tmpdir);
  if (needed < 0) {
    err_exit("TMPDIR too long: '%s'", tmpdir);
  }

  char *rootdir = mkdtemp(template_);
  if (!rootdir) {
    err_exit("mkdtemp(%s)", template_);
  }

  int unrace[2];

  if (pipe(unrace)) {
    err_exit("pipe()");
  }

  struct sigaction handle_child;
  handle_child.sa_sigaction = handle_child_death;
  handle_child.sa_flags = SA_SIGINFO;

  struct sigaction old_handler;
  if (sigaction(SIGCHLD, &handle_child, &old_handler)) {
    err_exit("sigaction()");
  }

  int child;
  child = child_pid = fork();
  if (child < 0) {
    err_exit("fork()");
  } else if (child == 0) {
    sigaction(SIGCHLD, &old_handler, NULL);
    close(unrace[1]);
    char buf[10];
    read(unrace[0], buf, 10);
    close(unrace[0]);
    return child_proc(rootdir, nixdir, pwd, clear_env, dirMappings, envMappings, argv[optind], argv + optind);
  } else {
    close(unrace[0]);
    char fifopath[PATH_MAX];
    if (enable_escape_hatch) {
      snprintf(fifopath, PATH_MAX, "%s/escape-hatch", rootdir);
      mkfifo(fifopath, 0600);
    }
    close(unrace[1]);
    if (enable_escape_hatch) {
      char buffer[1024];
      while (!child_died) {
        int fd = open(fifopath, O_RDONLY);
        if (fd < 0) {
          if (errno == EINTR) continue;
          fprintf(stderr, "error opening escape-hatch: %s\n", strerror(errno));
          continue;
        }
        int size = read(fd, buffer, 1024);
        buffer[size] = 0;
        system(buffer);
        close(fd);
      }
    }
    int status;
    int ret = waitpid(child, &status, 0);
    return WEXITSTATUS(status);
  }
}

static int child_proc(const char *rootdir, const char *nixdir, const char *pwd, uint8_t clear_env, list<struct DirMapping> dirMappings, list<struct SetEnv> envMappings, const char *executable, char * const new_argv[]) {
  // get uid, gid before going to new namespace
  uid_t uid = getuid();
  gid_t gid = getgid();

  // "unshare" into new namespace
  if (unshare(CLONE_NEWNS | CLONE_NEWUSER) < 0) {
    if (errno == EPERM) {
      fputs("Run the following to enable unprivileged namespace use:\nsudo bash -c \"sysctl -w kernel.unprivileged_userns_clone=1 ; echo kernel.unprivileged_userns_clone=1 > /etc/sysctl.d/nix-user-chroot.conf\"\n\n", stderr);
      exit(EXIT_FAILURE);
    } else {
      err_exit("unshare()");
    }
  }

  // add necessary system stuff to rootdir namespace
  for (list<struct DirMapping>::iterator it = dirMappings.begin();
      it != dirMappings.end(); ++it) {
    struct DirMapping m = *it;
    add_path(m.src, m.dest, rootdir);
  }

  // make sure nixdir exists
  struct stat statbuf2;
  if (stat(nixdir, &statbuf2) < 0) {
    err_exit("stat(%s)", nixdir);
  }

  char path_buf[PATH_MAX];
  // mount /nix to new namespace
  snprintf(path_buf, sizeof(path_buf), "%s/nix", rootdir);
  mkdir(path_buf, statbuf2.st_mode & ~S_IFMT);
  if (mount(nixdir, path_buf, "none", MS_BIND | MS_REC, NULL) < 0) {
    err_exit("mount(%s, %s)", nixdir, path_buf);
  }

  // fixes issue #1 where writing to /proc/self/gid_map fails
  // see user_namespaces(7) for more documentation
  int fd_setgroups = open("/proc/self/setgroups", O_WRONLY);
  if (fd_setgroups > 0) {
    write(fd_setgroups, "deny", 4);
    close(fd_setgroups);
  }

  // map the original uid/gid in the new ns
  char map_buf[1024];
  snprintf(map_buf, sizeof(map_buf), "%d %d 1", uid, uid);
  update_map(map_buf, "/proc/self/uid_map");
  snprintf(map_buf, sizeof(map_buf), "%d %d 1", gid, gid);
  update_map(map_buf, "/proc/self/gid_map");

  // chroot to rootdir
  if (chroot(rootdir) < 0) {
    err_exit("chroot(%s)", rootdir);
  }

  chdir(pwd);

  if (clear_env) clearenv();
  setenv("PATH", ENV_PATH, 1);

  for (list<struct SetEnv>::iterator it = envMappings.begin();
       it != envMappings.end(); ++it) {
    struct SetEnv e = *it;
    setenv(e.key.c_str(), e.value.c_str(), 1);
  }

  // execute the command
  execvp(executable, new_argv);
  err_exit("execvp(%s)", executable);
}
