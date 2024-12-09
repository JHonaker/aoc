#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <tgmath.h>

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

typedef struct testcase {
  size_t value;
  size_t operand_count;
  size_t *operands;
} testcase;

typedef enum { op_add, op_mul, op_count } operator;

int tc_is_possible(testcase tc) {
  int possible = 0;
  size_t noperators = tc.operand_count - 1;

  for (size_t i = 0; i < (1ULL << noperators); ++i) {
    size_t result = tc.operands[0];
    for (size_t d = 0; d < noperators; ++d) {
      if ((i >> d) & 1ULL) {
        result *= tc.operands[d + 1];
      } else {
        result += tc.operands[d + 1];
      }
    }
    if (result == tc.value) {
      ++possible;
      break;
    }
  }
  return possible;
}

size_t nth_digit(size_t n, size_t base, size_t digit) {
  return (n / (size_t)pow(base, digit)) % base;
}

int tc_is_possible_with_concat(testcase tc) {
  int possible = 0;
  size_t noperators = tc.operand_count - 1;

  char buf[100] = {0};

  for (size_t i = 0; i < pow(3ULL, noperators); ++i) {
    size_t result = tc.operands[0];
    for (size_t d = 0; d < noperators; ++d) {
      size_t digit = nth_digit(i, 3, d);
      switch (digit) {
      case 0:
        result *= tc.operands[d + 1];
        break;
      case 1:
        result += tc.operands[d + 1];
        break;
      case 2: {
        int printed = sprintf(buf, "%zu", result);
        sprintf(buf + printed, "%zu", tc.operands[d + 1]);
        size_t concat;
        sscanf(buf, "%zu", &concat);

        result = concat;

        break;
      }
      }
    }

    if (result == tc.value) {
      ++possible;
      break;
    }
  }
  return possible;
}

testcase tc_parse(char *data) {
  char *first_space = data;
  char *next_space = strchr(data, ' ') + 1;
  char *last_space = strrchr(data, ' ') + 1;

  size_t test_value;
  sscanf(data, "%zu:", &test_value);

  size_t noperands = 0;
  size_t buffer[100];
  while (first_space < last_space) {
    first_space = next_space;
    next_space = strchr(first_space, ' ') + 1;

    sscanf(first_space, "%zu", &buffer[noperands++]);
  }

  size_t *ops = calloc(noperands, sizeof(ops[0]));

  for (size_t i = 0; i < noperands; ++i) {
    ops[i] = buffer[i];
  }

  return (testcase){
      .value = test_value,
      .operand_count = noperands,
      .operands = ops,
  };
}

testcase *parse_file(char *data, size_t *n) {
  testcase *buffer = calloc(1000, sizeof(testcase));

  char *line = strtok(data, "\n");
  *n = 0;
  while (line) {
    buffer[(*n)++] = tc_parse(line);
    line = strtok(NULL, "\n");
  }

  buffer = realloc(buffer, (*n) * sizeof(testcase));
  return buffer;
}

int main(int argc, char **argv) {
  FileStatus ferr;
  size_t fsize;
  char *raw = read_file(argv[1], &ferr, &fsize);

  size_t n = 0;
  testcase *tcs = parse_file(raw, &n);
  printf("Read %zu cases.\n", n);

  // for (size_t i = 0; i < 27 * 3; ++i) {
  //   printf("%zu ", i);
  //   for (size_t d = 0; d < 4; ++d) {
  //     printf("%zu ", nth_digit(i, 3, d));
  //   }
  //   printf("\n");
  // }

  size_t sum1 = 0;
  size_t sum2 = 0;
  size_t counts = 0;
  for (size_t i = 0; i < n; ++i) {
    int p1 = tc_is_possible(tcs[i]);
    int p2 = tc_is_possible_with_concat(tcs[i]);
    sum1 += p1 ? tcs[i].value : 0;
    sum2 += p2 ? tcs[i].value : 0;
  }
  // 2437272016585 162987117690649
  // 18446744073709551615
  // 22437948932
  printf("Sum of possible (1): %zu\n", sum1);
  printf("Sum of possible (2): %zu\n", sum2);
}
