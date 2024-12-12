#include "../file.c"

#define STB_DS_IMPLEMENTATION
#include "../stb_ds.h"

#define REGION_UNSET -1

typedef struct map {
  size_t rows;
  size_t cols;
  int data[]; // Not null-terminated strings, an array of int!
} map;

typedef struct loc {
  int x;
  int y;
} loc;

map *parse_map(char *str);

int mapget(map *m, size_t i, size_t j);
int mapgetl(map *m, loc l);
void mapset(map *m, int i, int j, int value);
void mapsetl(map *m, loc l, int value);
map *empty_map(size_t nrows, size_t ncols);
void clear_map(map *m, int value);
int on_map(map *m, int x, int y);
int on_mapl(map *m, loc l);

void mapset(map *m, int i, int j, int value) {
  if (0 <= i && i < m->rows && 0 <= j && j < m->cols)
    m->data[i * m->cols + j] = value;
}

void mapsetl(map *m, loc l, int value) { mapset(m, l.x, l.y, value); }

int mapget(map *m, size_t i, size_t j) {
  if (i < 0 || m->rows <= i || j < 0 || m->cols <= j)
    return 0;
  return m->data[i * m->cols + j];
}

int mapgetl(map *m, loc l) { return mapget(m, l.x, l.y); }

int on_map(map *m, int x, int y) {
  size_t mr = m->rows;
  size_t mc = m->cols;

  return (0 <= y && y < mr) && (0 <= x && x < mc);
}

int on_mapl(map *m, loc l) { return on_map(m, l.x, l.y); }

map *parse_map(char *str) {

  size_t total_len = strlen(str);
  char *line = strtok(str, "\n");
  size_t ncols = strlen(line);

  size_t nrows = total_len / (ncols + 1);

  map *m = empty_map(nrows, ncols);

  for (size_t i = 0; line; i++) {

    for (size_t k = 0; k < ncols; k++) {
      /* Start Parsing logic */

      mapset(m, i, k, line[k]);

      /* End parsing logic */
    }

    line = strtok(NULL, "\n");
  }

  return m;
}

void clear_map(map *m, int value) {
  for (size_t i = 0; i < m->rows; ++i) {
    for (size_t j = 0; j < m->cols; ++j) {
      mapset(m, i, j, value);
    }
  }
}

map *empty_map(size_t nrows, size_t ncols) {
  map *newmap = malloc(sizeof(map) + nrows * ncols * sizeof(int));
  newmap->rows = nrows;
  newmap->cols = ncols;

  clear_map(newmap, 0);

  return newmap;
}

void print_map(map *m) {
  for (size_t i = 0; i < m->rows; ++i) {
    for (size_t j = 0; j < m->cols; ++j) {
      int cm = mapget(m, i, j);
      printf("%02d\t", cm);
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

int has_region(map *paint, loc l) { return mapgetl(paint, l) != REGION_UNSET; }

#define FILLED 1
#define NO_FILL 0

int flood_fill(map *label, map *paint, loc l, int v) {
  if (has_region(paint, l))
    return NO_FILL;

  int region_type = mapgetl(label, l);
  mapsetl(paint, l, v);
  for (direction d = 0; d < d_count; ++d) {
    loc o = neighbor_offset(d);
    loc neighbor = ladd(l, o);

    int should_fill = on_mapl(label, neighbor) &&
                      !has_region(paint, neighbor) &&
                      mapgetl(label, neighbor) == region_type;

    if (should_fill)
      flood_fill(label, paint, neighbor, v);
  }

  return FILLED;
}

void flood_map(map *label, map *regions) {
  int region_id = 0;
  for (int i = 0; i < label->rows; ++i) {
    for (int j = 0; j < label->cols; ++j) {
      int filled = flood_fill(label, regions, (loc){.x = i, .y = j}, region_id);
      if (filled)
        ++region_id;
    }
  }
}

int perimeter(map *regions, loc l) {
  int perim_count = 0;
  int id = mapgetl(regions, l);
  for (direction d = 0; d < d_count; ++d) {
    loc o = neighbor_offset(d);
    loc neighbor = ladd(l, o);
    if (!on_mapl(regions, neighbor))
      ++perim_count;
    else if (mapgetl(regions, neighbor) != id)
      ++perim_count;
  }

  return perim_count;
}

typedef struct {
  int key;
  int area;
  int perimeter;
} region_stat;

region_stat *part_1(map *regions, int *cost) {
  region_stat *count = NULL;
  region_stat default_zero = (region_stat){.key = 0, .area = 0, .perimeter = 0};
  hmdefaults(count, default_zero);

  for (size_t i = 0; i < regions->rows; ++i) {
    for (size_t j = 0; j < regions->cols; ++j) {
      loc l = (loc){.x = i, .y = j};
      int id = mapgetl(regions, l);
      int p = perimeter(regions, l);

      region_stat old = hmgets(count, id);
      region_stat updated = (region_stat){
          .key = id, .area = old.area + 1, .perimeter = old.perimeter + p};
      hmputs(count, updated);
    }
  }

  *cost = 0;
  for (int i = 0; i < hmlen(count); ++i) {
    *cost += count[i].area * count[i].perimeter;
  }

  return count;
}

int is_other(map *regions, loc here, loc there) {
  return mapgetl(regions, here) != mapgetl(regions, there);
}

int is_same(map *regions, loc here, loc there) {
  return on_mapl(regions, there) &&
         mapgetl(regions, here) == mapgetl(regions, there);
}

int neighbor_count(map *regions, loc here) {
  size_t count = 0;
  for (direction d = 0; d < d_count; ++d) {
    if (is_same(regions, here, ladd(here, neighbor_offset(d)))) {
      ++count;
    }
  }
  return count;
}

int is_I_shape(map *regions, loc here) {
  loc up = ladd(here, neighbor_offset(d_up));
  loc down = ladd(here, neighbor_offset(d_down));
  loc left = ladd(here, neighbor_offset(d_left));
  loc right = ladd(here, neighbor_offset(d_right));
  int is_vert_I = is_same(regions, here, up) && is_same(regions, here, down) &&
                  is_same(regions, up, down);
  int is_horiz_I = is_same(regions, here, left) &&
                   is_same(regions, here, right) &&
                   is_same(regions, left, right);
  return is_vert_I || is_horiz_I;
}

int count_corners_L_shape(map *regions, loc here) {

  loc up = ladd(here, neighbor_offset(d_up));
  loc down = ladd(here, neighbor_offset(d_down));
  loc left = ladd(here, neighbor_offset(d_left));
  loc right = ladd(here, neighbor_offset(d_right));

  int count = 0;

  if (is_same(regions, here, up) && is_same(regions, here, right))
    count = !is_same(regions, here, ladd(here, (loc){.x = 1, .y = -1}));
  else if (is_same(regions, here, down) && is_same(regions, here, right))
    count = !is_same(regions, here, ladd(here, (loc){.x = 1, .y = 1}));
  else if (is_same(regions, here, down) && is_same(regions, here, left))
    count = !is_same(regions, here, ladd(here, (loc){.x = -1, .y = 1}));
  else if (is_same(regions, here, up) && is_same(regions, here, left))
    count = !is_same(regions, here, ladd(here, (loc){.x = -1, .y = -1}));

  return 1 + count;
}

int count_corners_T_shape(map *regions, loc here) {
  loc up = ladd(here, neighbor_offset(d_up));
  loc down = ladd(here, neighbor_offset(d_down));
  loc left = ladd(here, neighbor_offset(d_left));
  loc right = ladd(here, neighbor_offset(d_right));

  int horiz = is_same(regions, here, left) && is_same(regions, here, right);

  int count = 0;

  if (horiz) {
    int upward = is_same(regions, here, up);
    if (upward) {
      count += !is_same(regions, here, ladd(here, (loc){.x = -1, .y = -1}));
      count += !is_same(regions, here, ladd(here, (loc){.x = 1, .y = -1}));
    } else {
      count += !is_same(regions, here, ladd(here, (loc){.x = -1, .y = 1}));
      count += !is_same(regions, here, ladd(here, (loc){.x = 1, .y = 1}));
    }
  } else {
    int leftward = is_same(regions, here, left);
    if (leftward) {
      count += !is_same(regions, here, ladd(here, (loc){.x = -1, .y = 1}));
      count += !is_same(regions, here, ladd(here, (loc){.x = -1, .y = -1}));
    } else {
      count += !is_same(regions, here, ladd(here, (loc){.x = 1, .y = 1}));
      count += !is_same(regions, here, ladd(here, (loc){.x = 1, .y = -1}));
    }
  }

  return count;
}

int count_corners_X_shape(map *regions, loc here) {
  int count = 0;

  count += !is_same(regions, here, ladd(here, (loc){.x = 1, .y = 1}));
  count += !is_same(regions, here, ladd(here, (loc){.x = 1, .y = -1}));
  count += !is_same(regions, here, ladd(here, (loc){.x = -1, .y = 1}));
  count += !is_same(regions, here, ladd(here, (loc){.x = -1, .y = -1}));

  return count;
}

int part_2(map *regions, region_stat *rs) {
  struct hv {
    int key;
    int value;
  };

  struct hv *corners = NULL;

  hmdefault(corners, 0);

  for (size_t i = 0; i < regions->rows; ++i) {
    for (size_t j = 0; j < regions->cols; ++j) {
      loc l = (loc){.x = i, .y = j};
      int id = mapgetl(regions, l);
      int nneighbors = neighbor_count(regions, l);
      int l_corners = 0;
      switch (nneighbors) {
      case 0:
        l_corners = 4;
        break;
      case 1:
        l_corners = 2;
        break;
      case 2:
        if (!is_I_shape(regions, l))
          l_corners = count_corners_L_shape(regions, l);
        else
          l_corners = 0;

        break;
      case 3:
        l_corners = count_corners_T_shape(regions, l);
        break;
      case 4:
        l_corners = count_corners_X_shape(regions, l);
        break;
      }
      int c = hmget(corners, id);
      hmput(corners, id, c + l_corners);
    }
  }

  size_t cost = 0;
  for (int i = 0; i < hmlen(corners); ++i) {
    int region = corners[i].key;
    int nsides = corners[i].value;
    int area = hmgets(rs, region).area;

    cost += nsides * area;
  }

  return cost;
}

int main(int argc, char **argv) {
  FileStatus ferr;
  size_t fsize;
  char *raw = read_file(argv[1], &ferr, &fsize);

  map *m = parse_map(raw);
  map *regions = empty_map(m->rows, m->cols);
  clear_map(regions, REGION_UNSET);

  flood_map(m, regions);

  int part1_cost = 0;
  region_stat *rs = part_1(regions, &part1_cost);

  int part2_cost = part_2(regions, rs);

  printf("(P1) Total cost: %d\n", part1_cost);
  printf("(P2) Total cost: %d\n", part2_cost);
}
