#define __STDC_WANT_LIB_EXT2__ 1

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>

size_t search_spot_in_direction(size_t N, size_t P, char (*data)[N][P],
                                size_t row, size_t col, int drow, int dcol,
                                char const *str) {
  if (row < 0 || row >= N || col < 0 || col >= P)
    return 0;

  size_t len = strlen(str);
  for (size_t i = 0; i < len; i++) {
    char const target = *(str + i);
    int lcol = col + i * dcol;
    int lrow = row + i * drow;
    int row_in_bounds = 0 <= lrow && lrow < N;
    int col_in_bounds = 0 <= lcol && lcol < P;
    if (!(col_in_bounds && row_in_bounds))
      return 0;
    if ((*data)[lrow][lcol] != target)
      return 0;
  }
  return 1;
}

size_t search_spot(size_t N, size_t P, char (*data)[N][P], size_t row,
                   size_t col) {
  size_t count = 0;
  for (int drow = -1; drow <= 1; drow++) {
    for (int dcol = -1; dcol <= 1; dcol++) {
      count +=
          search_spot_in_direction(N, P, data, row, col, drow, dcol, "XMAS");
    }
  }
  return count;
}

size_t search_spot_for_x(size_t N, size_t P, char (*data)[N][P], size_t row,
                         size_t col) {
  size_t down_leg =
      search_spot_in_direction(N, P, data, row - 1, col - 1, 1, 1, "MAS") ||
      search_spot_in_direction(N, P, data, row - 1, col - 1, 1, 1, "SAM");

  size_t up_leg =
      search_spot_in_direction(N, P, data, row + 1, col - 1, -1, 1, "MAS") ||
      search_spot_in_direction(N, P, data, row + 1, col - 1, -1, 1, "SAM");

  return down_leg && up_leg;
}

int main(int argc, char **argv) {
  FILE *f = fopen(argv[1], "rb");

  char *line = NULL;
  size_t len = 0;
  size_t nread = 0;

  ssize_t line_bytes = getline(&line, &len, f);
  fseek(f, 0, SEEK_END);
  size_t total_bytes = ftell(f);
  fseek(f, 0, SEEK_SET);

  size_t N = total_bytes / line_bytes;
  size_t P = line_bytes - 1;

  char(*data)[N][P] = malloc(sizeof(*data));

  size_t i = 0;
  for (size_t i = 0; (nread = fread((*data)[i], 1, P, f)); i++) {
    fseek(f, 1, SEEK_CUR); // Skip the newline
  }

  size_t xmas_count = 0;
  for (size_t i = 0; i < N; i++) {
    for (size_t j = 0; j < P; j++) {
      xmas_count += search_spot(N, P, data, i, j);
    }
  }

  printf("XMAS count: %zu\n", xmas_count);

  size_t X_mas_count = 0;
  for (size_t i = 1; i < N - 1; i++) {
    for (size_t j = 1; j < P - 1; j++) {
      X_mas_count += search_spot_for_x(N, P, data, i, j);
    }
  }

  printf("X-MAS count: %zu\n", X_mas_count);

  free(data);

  return 0;
}
