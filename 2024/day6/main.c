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

typedef enum {
  d_up = 1,
  d_right = 1 << 1,
  d_down = 1 << 2,
  d_left = 1 << 3
} direction;

typedef struct guard {
  int row;
  int col;
  direction facing;
} guard;

typedef struct map {
  size_t rows;
  size_t cols;
  char data[]; // Not null-terminated strings, an array of char!
} map;

char mapget(map *m, size_t i, size_t j);
void mapset(map *m, size_t i, size_t j, direction d);
void parse_map(char *str, map **m, guard *g);
void empty_map(map **m, size_t nrows, size_t ncols);
int on_map(map *m, guard *g);
void step_guard(map *m, guard *g, map *history);
size_t visited_count(map *history);

void mapset(map *m, size_t i, size_t j, direction d) {
  m->data[i * m->cols + j] |= d;
}

char mapget(map *m, size_t i, size_t j) {
  if (i < 0 || m->rows <= i || j < 0 || m->cols <= j)
    return 0;
  return m->data[i * m->cols + j];
}

int on_map(map *m, guard *g) {
  int gr = g->row;
  int gc = g->col;
  size_t mr = m->rows;
  size_t mc = m->cols;

  return (0 <= gr && gr < mr) && (0 <= gc && gc < mc);
}

void parse_map(char *str, map **m, guard *g) {

  int found_guard = 0;

  size_t total_len = strlen(str);
  char *line = strtok(str, "\n");
  size_t ncols = strlen(line);

  size_t nrows = total_len / (ncols + 1);

  empty_map(m, nrows, ncols);

  for (size_t i = 0; line; i++) {

    for (size_t k = 0; k < ncols; k++) {
      (*m)->data[i * ncols + k] = line[k];
      if (!found_guard) {
        if (line[k] == '^') {
          found_guard = 1;
          g->row = i;
          g->col = k;
          g->facing = d_up;
        }
      }
    }

    line = strtok(NULL, "\n");
  }
}

void empty_map(map **m, size_t nrows, size_t ncols) {
  *m = calloc(sizeof(map) + nrows * ncols, 1);
  (*m)->rows = nrows;
  (*m)->cols = ncols;
}

void print_map(map *m, guard *g, map *h) {
  for (size_t i = 0; i < m->rows; ++i) {
    for (size_t j = 0; j < m->cols; ++j) {
      if (g && g->row == i && g->col == j) {
        printf("X");
      } else {
        char cm = mapget(m, i, j);
        char ch = mapget(h, i, j);
        if (ch)
          printf("%d", ch);
        else
          printf("%c", cm);
      }
    }
    printf("\n");
  }
  printf("\n");
}

void turn_guard(guard *g) {
  g->facing = (g->facing == d_left) ? d_up : g->facing << 1;
}

void next_step(guard *g, int *next_r, int *next_c) {
  *next_r = g->row;
  *next_c = g->col;

  switch (g->facing) {
  case d_up:
    *next_r -= 1;
    break;
  case d_right:
    *next_c += 1;
    break;
  case d_down:
    *next_r += 1;
    break;
  case d_left:
    *next_c -= 1;
    break;
  }
}

void step_guard(map *m, guard *g, map *history) {

  if (!on_map(m, g))
    return;

  int next_r, next_c;
  next_step(g, &next_r, &next_c);

  char next_cell = mapget(m, next_r, next_c);
  if (next_cell == '#') {
    turn_guard(g);
    return;
  }

  mapset(history, g->row, g->col,
         g->facing); // Set the visit *after* the guard leaves.

  g->row = next_r;
  g->col = next_c;
}

int spot_loops(map *m, guard *g, map *h) {
  return mapget(h, g->row, g->col) & g->facing;
}

int simulate_loop(map *m, guard *g, map *history) {
  while (on_map(m, g)) {
    step_guard(m, g, history);
    if (spot_loops(m, g, history))
      return 1;
  }

  return 0;
}

int try_adding_blocker(map *m, guard g, size_t i, size_t j) {
  map *copym = malloc(sizeof(map) + m->rows * m->cols);
  copym->rows = m->rows;
  copym->cols = m->cols;

  for (size_t i = 0; i < m->rows * m->cols; ++i) {
    copym->data[i] = m->data[i];
  }

  map *h;
  empty_map(&h, m->rows, m->cols);

  copym->data[i * m->rows + j] = '#';

  int looped = simulate_loop(copym, &g, h);

  free(copym);
  free(h);

  return looped;
}

size_t visited_count(map *history) {
  size_t count = 0;
  for (size_t i = 0; i < history->rows; ++i) {
    for (size_t j = 0; j < history->cols; ++j) {
      if (mapget(history, i, j))
        ++count;
    }
  }
  return count;
}

int main(int argc, char **argv) {
  FileStatus err;
  size_t fsize;
  char *raw = read_file(argv[1], &err, &fsize);

  map *m;
  guard g = {0};
  parse_map(raw, &m, &g);

  guard initial_g = (guard){.row = g.row, .col = g.col, .facing = g.facing};

  map *history;
  empty_map(&history, m->rows, m->cols);

  map *block_points;
  empty_map(&block_points, m->rows, m->cols);

  map *lines;
  empty_map(&lines, m->rows, m->cols);

  while (on_map(m, &g)) {
    step_guard(m, &g, history);
  }

  size_t count = visited_count(history);
  printf("Visited %zu\n", count);

  size_t block_point_count = 0;
  print_map(m, NULL, history);

  for (size_t i = 0; i < m->rows; ++i) {
    for (size_t j = 0; j < m->cols; ++j) {
      if (!mapget(history, i, j) || (initial_g.row == i && initial_g.col == j))
        continue;
      if (try_adding_blocker(m, initial_g, i, j)) {
        printf("%zu, %zu\n", i, j);
        ++block_point_count;
      }
    }
  }
  printf("Block points %zu\n", block_point_count);
}
