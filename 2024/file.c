#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef enum {
  FILE_OK,
  FILE_NOT_EXIST,
  FILE_TOO_LARGE,
  FILE_READ_ERROR
} FileStatus;

char *read_file(char const *filename, FileStatus *err, size_t *f_size) {
  char *buffer = NULL;
  FILE *f = fopen(filename, "rb");

  if (f) {
    fseek(f, 0, SEEK_END);
    size_t length = ftell(f);
    fseek(f, 0, SEEK_SET);

    if (length > 1073741824) {
      *err = FILE_TOO_LARGE;
      return NULL;
    }

    *f_size = length;
    buffer = malloc(length + 1);
    buffer[length] = '\0';

    if (length) {
      size_t read_length = fread(buffer, 1, length, f);

      if (length != read_length) {
        free(buffer);
        *err = FILE_READ_ERROR;
        return NULL;
      }
    }

    fclose(f);

  } else {
    *err = FILE_NOT_EXIST;
    return NULL;
  }

  *err = FILE_OK;
  return buffer;
}
