#ifndef PIPE_H
#define PIPE_H

#include <stdbool.h>

#define PIPE_READ_END 0
#define PIPE_WRITE_END 1

/* Creates a POSIX pipe that is used for exchanging data between
 * Haskell and C. The interface serves as an alternative to the
 * Haskell-FFI. The pipe is created at the C level. The function
 * returns the file descriptor of the created pipe.
 */
int init_pipe(void);

/* Closes the pipe created with 'initPipe'.
 */
void close_pipe(void);

/* Checks whether the pipe already got initialized.
 */
bool pipe_closed(void);

/* Sends some values to the pipe formatted according to the given
   format string. It's bascially is just an 'fprintf' wrapper.
 */
void send(char *restrict, ...);

/* Finalizes a value printed to the pipe via sending a newline
 * character.
 */
void commit_value(void);

#endif
