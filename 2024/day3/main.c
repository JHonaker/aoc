#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

typedef int (*callback)(char const *, char const *, char const *, void *);

char *instructions = {"mul"};

typedef enum {
  TYPE_DO,
  TYPE_DONT,
  TYPE_MUL,
} InstructionType;
typedef struct {
  InstructionType tag;
  int left;
  int right;
} Instruction;

/*
 * Parsing functions build instructions using out parameters.
 * They return the number of bytes read from the input on a successful parse,
 * and 0 otherwise.
 */

int parse_char(char const *data, char c);
int parse_seq(char const *data, char const *target);
int parse_number(char const *data, size_t max_digits, int *num);
int parse_mul(char const *data, Instruction *mul);

int parse_char(char const *data, char c) { return *data == c; }
int parse_number(char const *data, size_t max_digits, int *num) {
  int digits;
  if (sscanf(data, "%d%n", num, &digits)) {
    if (digits <= max_digits)
      return digits;
    else
      return 0;
  }

  return 0;
}

int parse_seq(char const *data, char const *target) {
  size_t bytes_read = 0;
  while (*target) {
    if (*data++ != *target++)
      return 0;

    bytes_read++;
  }
  return bytes_read;
}

int parse_mul(char const *data, Instruction *mul) {
  char const *parse_head = data;
  int mul_result = parse_seq(data, "mul");
  parse_head += mul_result;

  if (!mul_result)
    return 0;

  if (!parse_char(parse_head, '('))
    return 0;

  parse_head++;

  int left = 0;
  int left_result = parse_number(parse_head, 3, &left);
  parse_head += left_result;

  if (!left_result)
    return 0;

  if (!parse_char(parse_head, ','))
    return 0;

  parse_head++;

  int right = 0;
  int right_result = parse_number(parse_head, 3, &right);
  parse_head += right_result;

  if (!parse_char(parse_head, ')'))
    return 0;
  parse_head++;

  mul->tag = TYPE_MUL;
  mul->left = left;
  mul->right = right;

  return parse_head - data;
}

int parse_do(char const *data, Instruction *inst) {
  size_t bytes_parsed = 0;
  if ((bytes_parsed = parse_seq(data, "do()"))) {
    inst->tag = TYPE_DO;
    return bytes_parsed;
  }
  return 0;
}

int parse_dont(char const *data, Instruction *inst) {
  size_t bytes_parsed = 0;
  if ((bytes_parsed = parse_seq(data, "don't()"))) {
    inst->tag = TYPE_DONT;
    return bytes_parsed;
  }
  return 0;
}

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

int main(int argc, char **argv) {
  FileStatus err;
  size_t data_size;

  char *data = read_file(argv[1], &err, &data_size);
  char *parse_head = data;

  Instruction instructions[10000] = {0};
  size_t i = 0;
  size_t bytes_parsed = 0;
  while (*parse_head) {
    if ((bytes_parsed = parse_mul(parse_head, &instructions[i]))) {
      parse_head += bytes_parsed;
      i++;
    } else if ((bytes_parsed = parse_do(parse_head, &instructions[i]))) {
      parse_head += bytes_parsed;
      i++;
    } else if ((bytes_parsed = parse_dont(parse_head, &instructions[i]))) {
      parse_head += bytes_parsed;
      i++;
    } else {
      parse_head++;
    }
  }

  int part_1_result = 0;
  for (size_t j = 0; j < i; j++) {
    Instruction inst = instructions[j];
    if (inst.tag == TYPE_MUL)
      part_1_result += inst.left * inst.right;
  }

  printf("Part 1 total: %d\n", part_1_result);

  bool mul_enabled = true;
  int part_2_result = 0;

  for (size_t j = 0; j < i; j++) {
    Instruction inst = instructions[j];
    switch (inst.tag) {
    case TYPE_DO:
      mul_enabled = true;
      break;
    case TYPE_DONT:
      mul_enabled = false;
      break;
    case TYPE_MUL:
      part_2_result += mul_enabled ? inst.left * inst.right : 0;
      break;
    }
  }

  printf("Part 2 total: %d\n", part_2_result);

  free(data);
}
