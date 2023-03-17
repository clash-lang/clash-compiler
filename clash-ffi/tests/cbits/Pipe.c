#include <stdarg.h>
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>

#include "Pipe.h"

static int pipefd[2];
static FILE* f = NULL;

int init_pipe(void)
{
  if (pipe(pipefd) == -1)
    {
      perror("ERROR: Cannot create pipe.\n");
      exit(EXIT_FAILURE);
    }

  f = fdopen(pipefd[PIPE_WRITE_END], "w");

  return pipefd[PIPE_READ_END];
}

void close_pipe(void)
{
  close(pipefd[PIPE_WRITE_END]);
}

bool pipe_closed()
{
  return f == NULL;
}

void send(char *restrict format, ...)
{
  va_list ap;

  va_start(ap, format);
  vfprintf(f, format, ap);
  va_end(ap);
}

void commit_value()
{
  fprintf(f, "\n");
  fflush(f);
}
