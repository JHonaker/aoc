#include "../file.c"
#include <stdlib.h>

static char const zero_code = 0x30;

typedef struct map {
  size_t rows;
  size_t cols;
  int data[]; // Not null-terminated strings, an array of int!
} map;

typedef struct loc {
  int x;
  int y;
} loc;

void parse_map(char *str, map **m);

int mapget(map *m, size_t i, size_t j);
void mapset(map *m, int i, int j, int value);
void empty_map(map **m, size_t nrows, size_t ncols);
int on_map(map *m, int x, int y);

void mapset(map *m, int i, int j, int value) {
  if (0 <= i && i < m->rows && 0 <= j && j < m->cols)
    m->data[i * m->cols + j] = value;
}

int mapget(map *m, size_t i, size_t j) {
  if (i < 0 || m->rows <= i || j < 0 || m->cols <= j)
    return 0;
  return m->data[i * m->cols + j];
}

int on_map(map *m, int x, int y) {
  size_t mr = m->rows;
  size_t mc = m->cols;

  return (0 <= y && y < mr) && (0 <= x && x < mc);
}

void parse_map(char *str, map **m) {

  size_t total_len = strlen(str);
  char *line = strtok(str, "\n");
  size_t ncols = strlen(line);

  size_t nrows = total_len / (ncols + 1);

  empty_map(m, nrows, ncols);

  for (size_t i = 0; line; i++) {

    for (size_t k = 0; k < ncols; k++) {
      /* Start Parsing logic */

      mapset(*m, i, k, line[k] - zero_code);

      /* End parsing logic */
    }

    line = strtok(NULL, "\n");
  }
}

void empty_map(map **m, size_t nrows, size_t ncols) {
  *m = malloc(sizeof(map) + nrows * ncols * sizeof(int));
  (*m)->rows = nrows;
  (*m)->cols = ncols;

  for (size_t i = 0; i < nrows; ++i) {
    for (size_t j = 0; j < nrows; ++j) {
      mapset(*m, i, j, 0);
    }
  }
}

void print_map(map *m) {
  for (size_t i = 0; i < m->rows; ++i) {
    for (size_t j = 0; j < m->cols; ++j) {
      int cm = mapget(m, i, j);
      printf("%04d\t", cm);
    }
    printf("\n");
  }
  printf("\n");
}

typedef enum { d_up, d_right, d_down, d_left, d_count } direction;

loc neighbor_offset(direction d) {
  int dx = 0;
  int dy = 0;

  switch (d) {
  case d_up:
    dy = -1;
    break;
  case d_right:
    dx = 1;
    break;
  case d_down:
    dy = 1;
    break;
  case d_left:
    dx = -1;
    break;
  default:
    exit(EXIT_FAILURE);
  }

  return (loc){.x = dx, .y = dy};
}

loc ladd(loc here, loc offset) {
  return (loc){.x = here.x + offset.x, .y = here.y + offset.y};
}

void propagate(map *heights, map *trails, loc here, _Bool distinct) {
  int here_trails = mapget(trails, here.x, here.y);
  int trail_count = 1;
  if (distinct) {
    trail_count += here_trails;
  }
  mapset(trails, here.x, here.y, trail_count);

  int height = mapget(heights, here.x, here.y);

  for (direction d = 0; d < d_count; ++d) {
    loc neighbor = ladd(here, neighbor_offset(d));

    if (!on_map(heights, neighbor.x, neighbor.y))
      continue;

    int neighbor_height = mapget(heights, neighbor.x, neighbor.y);

    if (height - neighbor_height == 1) {
      propagate(heights, trails, neighbor, distinct);
    }
  }
}

void add_trails(map *trails, map *buf) {
  for (int i = 0; i < trails->rows; ++i) {
    for (int j = 0; j < trails->rows; ++j) {
      int t = mapget(trails, i, j);
      int b = mapget(buf, i, j);
      mapset(trails, i, j, t + b);
    }
  }
}

int main(int argc, char **argv) {
  FileStatus ferr;
  size_t fsize;
  char *raw = read_file(argv[1], &ferr, &fsize);

  int part2 = *argv[2] - zero_code;

  map *m = {0};
  parse_map(raw, &m);

  map *buf;
  map *trails = {0};
  empty_map(&trails, m->rows, m->cols);

  for (int i = 0; i < m->rows; ++i) {
    for (int j = 0; j < m->rows; ++j) {
      if (mapget(m, i, j) == 9) {
        empty_map(&buf, m->rows, m->cols);
        propagate(m, buf, (loc){.x = i, .y = j}, part2);
        add_trails(trails, buf);
        free(buf);
      }
    }
  }

  int sum = 0;
  for (int i = 0; i < m->rows; ++i) {
    for (int j = 0; j < m->rows; ++j) {
      if (mapget(m, i, j) == 0) {
        sum += mapget(trails, i, j);
      }
    }
  }

  printf("Sum of trailheads: %d\n", sum);
}
