#include <assert.h>
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

typedef struct map {
  size_t rows;
  size_t cols;
  char data[]; // Not null-terminated strings, an array of char!
} map;

typedef struct loc {
  int x;
  int y;
} loc;

typedef struct antenna {
  loc loc;
  char frequency;
} antenna;

void parse_map(char *str, map **m, antenna *ants, size_t *nants);

char mapget(map *m, size_t i, size_t j);
void mapset(map *m, int i, int j, char value);
void empty_map(map **m, size_t nrows, size_t ncols);
int on_map(map *m, size_t x, size_t y);

void pair_antinodes(antenna a, antenna b, loc antinodes[2]) {
  antenna left = (a.loc.x < b.loc.x) ? a : b;
  antenna right = (a.loc.x < b.loc.x) ? b : a;

  int dx = right.loc.x - left.loc.x;
  int dy = right.loc.y - left.loc.y;

  if (dx == 0) {
    int min_y = (a.loc.y < b.loc.y) ? a.loc.y : b.loc.y;
    int max_y = (a.loc.y < b.loc.y) ? b.loc.y : a.loc.y;
    antinodes[0] = (loc){.x = left.loc.x, .y = min_y - abs(dy)};
    antinodes[1] = (loc){.x = left.loc.x, .y = max_y + abs(dy)};
  } else {

    antinodes[0] = (loc){.x = left.loc.x - dx, .y = left.loc.y - dy};
    antinodes[1] = (loc){.x = right.loc.x + dx, .y = right.loc.y + dy};
  }
}

void mark_antinodes(map *m, size_t nants, antenna ants[static nants]) {
  for (size_t i = 0; i < nants; ++i) {
    for (size_t j = 0; j < i; ++j) {
      antenna a = ants[i];
      antenna b = ants[j];

      if (a.frequency != b.frequency)
        continue;

      loc antinodes[2] = {0};
      pair_antinodes(a, b, antinodes);

      mapset(m, antinodes[0].x, antinodes[0].y, '#');
      mapset(m, antinodes[1].x, antinodes[1].y, '#');
    }
  }
}

void compute_all_in_line(map *m, antenna a, antenna b, loc antinodes[],
                         size_t *nantinodes) {
  antenna left = (a.loc.x < b.loc.x) ? a : b;
  antenna right = (a.loc.x < b.loc.x) ? b : a;

  int dx = right.loc.x - left.loc.x;
  int dy = right.loc.y - left.loc.y;

  antinodes[0] = left.loc;

  int x = left.loc.x;
  int y = left.loc.y;
  *nantinodes = 1;

  while (on_map(m, x, y)) {
    x -= dx;
    y -= dy;
    antinodes[(*nantinodes)++] = (loc){.x = x, .y = y};
  }
  x = left.loc.x;
  y = left.loc.y;

  while (on_map(m, x, y)) {
    x += dx;
    y += dy;
    antinodes[(*nantinodes)++] = (loc){.x = x, .y = y};
  }
}

void mark_antinodes_in_line(map *m, size_t nants, antenna ants[static nants]) {
  for (size_t i = 0; i < nants; ++i) {
    for (size_t j = 0; j < i; ++j) {
      antenna a = ants[i];
      antenna b = ants[j];

      if (a.frequency != b.frequency)
        continue;

      loc antinodes[1000] = {0};
      size_t nantinodes = 0;
      compute_all_in_line(m, a, b, antinodes, &nantinodes);

      for (size_t k = 0; k < nantinodes; ++k) {
        mapset(m, antinodes[k].x, antinodes[k].y, '#');
      }
    }
  }
}

void mapset(map *m, int i, int j, char value) {
  if (0 <= i && i < m->rows && 0 <= j && j < m->cols)
    m->data[i * m->cols + j] = value;
}

char mapget(map *m, size_t i, size_t j) {
  if (i < 0 || m->rows <= i || j < 0 || m->cols <= j)
    return 0;
  return m->data[i * m->cols + j];
}

int on_map(map *m, size_t x, size_t y) {
  size_t mr = m->rows;
  size_t mc = m->cols;

  return (0 <= y && y < mr) && (0 <= x && x < mc);
}

void parse_map(char *str, map **m, antenna *ants, size_t *nants) {

  int found_guard = 0;

  size_t total_len = strlen(str);
  char *line = strtok(str, "\n");
  size_t ncols = strlen(line);

  size_t nrows = total_len / (ncols + 1);

  empty_map(m, nrows, ncols);

  *nants = 0;
  for (size_t i = 0; line; i++) {

    for (size_t k = 0; k < ncols; k++) {
      /* Start Parsing logic */

      if (line[k] != '.') {
        ants[(*nants)++] =
            (antenna){.loc = {.x = (int)k, .y = (int)i}, .frequency = line[k]};
      }

      /* End parsing logic */
    }

    line = strtok(NULL, "\n");
  }
}

void empty_map(map **m, size_t nrows, size_t ncols) {
  *m = malloc(sizeof(map) + nrows * ncols);
  (*m)->rows = nrows;
  (*m)->cols = ncols;

  for (size_t i = 0; i < nrows; ++i) {
    for (size_t j = 0; j < nrows; ++j) {
      mapset(*m, i, j, '.');
    }
  }
}

void print_map(map *m) {
  for (size_t i = 0; i < m->rows; ++i) {
    for (size_t j = 0; j < m->cols; ++j) {
      char cm = mapget(m, i, j);
      if (cm)
        printf("%c", cm);
    }
    printf("\n");
  }
  printf("\n");
}

int main(int argc, char **argv) {
  FileStatus ferr;
  size_t fsize;
  char *raw = read_file(argv[1], &ferr, &fsize);

  size_t nants;
  antenna ants[1000] = {0};

  map *m;

  parse_map(raw, &m, ants, &nants);

  mark_antinodes(m, nants, ants);

  size_t count = 0;
  for (size_t i = 0; i < m->rows; ++i) {

    for (size_t j = 0; j < m->cols; ++j) {
      // printf("%zu %zu\n", i, j);
      if (mapget(m, i, j) == '#')
        count++;
    }
  }

  printf("Count antinodes: %zu\n", count);

  mark_antinodes_in_line(m, nants, ants);

  count = 0;
  for (size_t i = 0; i < m->rows; ++i) {

    for (size_t j = 0; j < m->cols; ++j) {
      // printf("%zu %zu\n", i, j);
      if (mapget(m, i, j) == '#')
        count++;
    }
  }
  printf("Count antinodes: %zu\n", count);
}
