#include "../file.c"
#include <tgmath.h>

#define STB_DS_IMPLEMENTATION
#include "../stb_ds.h"

typedef struct machine {
  ptrdiff_t ax, ay;
  ptrdiff_t bx, by;
  ptrdiff_t px, py;
} machine;

typedef struct presses {
  ptrdiff_t a;
  ptrdiff_t b;
} presses;

int parse_machine(char **head, machine *m) {
  int consumed_bytes = 0;
  int read_items =
      sscanf(*head,
             "Button A: X+%zu, Y+%zu\nButton B: X+%zu, Y+%zu\nPrize: X=%zu, "
             "Y=%zu\n\n%n",
             &m->ax, &m->ay, &m->bx, &m->by, &m->px, &m->py, &consumed_bytes);
  if (read_items < 6)
    return 0;

  *head += consumed_bytes;
  return 1;
}

machine *parse_file(char *str, size_t *count) {
  char *parse_head = str;

  int len = strlen(str);

  machine *ms = NULL;
  machine m;

  *count = 0;
  while (parse_head - str < len && (parse_machine(&parse_head, &m))) {
    ++*count;
    arrput(ms, m);
  }

  return ms;
}

presses solve(machine m) {
  double det = m.ax * m.by - m.bx * m.ay;

  if (det == 0)
    return (presses){.a = -1, .b = -1};

  double inva = m.by / det;
  double invb = -m.bx / det;
  double invc = -m.ay / det;
  double invd = m.ax / det;

  double a_presses = inva * m.px + invb * m.py;
  double b_presses = invc * m.px + invd * m.py;

  return (presses){.a = round(a_presses), .b = round(b_presses)};
}

int is_solution(machine m, presses p) {
  ptrdiff_t x = m.ax * p.a + m.bx * p.b;
  ptrdiff_t y = m.ay * p.a + m.by * p.b;
  return x == m.px && y == m.py;
}

int part1(size_t n, machine ms[static n]) {
  int tokens = 0;
  for (size_t i = 0; i < n; ++i) {
    presses p = solve(ms[i]);
    if (is_solution(ms[i], p)) {
      tokens += 3 * p.a;
      tokens += p.b;
    }
  }

  return tokens;
}

size_t part2(size_t n, machine ms[static n]) {
  size_t tokens = 0;
  for (size_t i = 0; i < n; ++i) {
    machine m = ms[i];
    m.px += 10000000000000;
    m.py += 10000000000000;
    presses p = solve(m);
    if (is_solution(m, p)) {
      tokens += 3 * p.a;
      tokens += p.b;
    }
  }

  return tokens;
}

int main(int argc, char **argv) {
  FileStatus ferr;
  size_t fsize;
  char *raw = read_file(argv[1], &ferr, &fsize);

  size_t nmachines = 4;
  machine *ms = parse_file(raw, &nmachines);
  // machine ms[2] = {m1, m2};

  int cost1 = part1(nmachines, ms);
  size_t cost2 = part2(nmachines, ms);

  printf("Part 1 tokens: %d\n", cost1);
  printf("Part 2 tokens: %zu\n", cost2);
}
