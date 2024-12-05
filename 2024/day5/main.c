#define __STDC_WANT_LIB_EXT2__ 1

#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define STB_DS_IMPLEMENTATION
#include "../stb_ds.h"

#define hmhaskey(hash, key) (hmgeti((hash), (key)) >= 0)

typedef struct rule {
  int left;
  int right;
} rule;

typedef struct {
  int key;
  int *before;
  int *after;
} ruleset;

int in_arr(size_t n, int arr[n], int value);

rule *parse_rules(FILE *f, size_t *n);
int **parse_manuals(FILE *f, size_t *n);

ruleset *create_ruleset(size_t nrules, rule rules[static nrules]);
int is_valid_update(ruleset *rules, size_t npages, int manual[static npages]);
int get_middle_page(size_t npages, int manual[static npages]);

rule *parse_rules(FILE *f, size_t *n) {
  rule *rules = NULL;

  int read_bytes = 0;
  char *line = NULL;
  size_t line_size = 0;

  int left = 0;
  int right = 0;

  while ((read_bytes = getline(&line, &line_size, f))) {
    int res = sscanf(line, "%d|%d\n", &left, &right);
    if (res == EOF)
      break;

    rule r = (rule){.left = left, .right = right};
    arrput(rules, r);
    ++*n;
  }

  return rules;
}

int **parse_manuals(FILE *f, size_t *n) {
  int **mans = NULL;

  int read_bytes = 0;
  char *line = NULL;
  size_t line_size = 0;
  while ((read_bytes = getline(&line, &line_size, f)) != -1) {
    int *man = NULL;

    char *token = strtok(line, ",\n");
    while (token) {
      arrput(man, atoi(token));
      token = strtok(NULL, ",\n");
    }

    arrput(mans, man);
    ++*n;
  }

  free(line);

  return mans;
}

int intcmp(void const *l, void const *r) {
  int const *x = l;
  int const *y = r;

  return *x - *y;
}

int in_arr(size_t n, int arr[n], int value) {
  int *idx = bsearch(&value, arr, n, sizeof(int), intcmp);

  return !!idx;
}

ruleset *create_ruleset(size_t nrules, rule rules[static nrules]) {
  ruleset *hash = NULL;

  for (size_t i = 0; i < nrules; i++) {
    rule r = rules[i];

    if (!hmhaskey(hash, r.left)) {
      ruleset rs = (ruleset){.key = r.left};
      hmputs(hash, rs);
    }

    if (!hmhaskey(hash, r.right)) {
      ruleset rs = (ruleset){.key = r.right};
      hmputs(hash, rs);
    }

    // Add the right value to the left value's after set
    ruleset lrule = hmgets(hash, r.left);
    arrput(lrule.after, r.right);
    hmputs(hash, lrule);

    // Add the left value to the right value's before set
    ruleset rrule = hmgets(hash, r.right);
    arrput(rrule.before, r.left);
    hmputs(hash, rrule);
  }

  for (size_t i = 0; i < hmlenu(hash); i++) {
    ruleset r = hash[i];
    qsort(r.before, arrlenu(r.before), sizeof(r.before[0]), intcmp);
    qsort(r.after, arrlenu(r.after), sizeof(r.after[0]), intcmp);
  }

  return hash;
}

int is_valid_update(ruleset *rules, size_t npages, int manual[static npages]) {
  for (size_t i = 0; i < npages; i++) {
    ruleset rule = hmgets(rules, manual[i]);
    for (size_t k = 0; k < i; k++) {
      if (in_arr(arrlen(rule.after), rule.after, manual[k]))
        return 0;
    }

    for (size_t k = i + 1; k < npages; k++) {
      if (in_arr(arrlen(rule.before), rule.before, manual[k]))
        return 0;
    }
  }

  return 1;
}

int get_middle_page(size_t npages, int manual[static npages]) {
  return manual[npages / 2];
}

static ruleset *cmp_ruleset = NULL;

int ruleset_compar(void const *_left, void const *_right) {
  int left = *(int *)_left;
  int right = *(int *)_right;

  ruleset left_rs = hmgets(cmp_ruleset, left);
  ruleset right_rs = hmgets(cmp_ruleset, right);

  if (in_arr(arrlen(right_rs.before), right_rs.before, left))
    return -1;
  if (in_arr(arrlen(right_rs.after), right_rs.after, left))
    return 1;

  return intcmp(_left, _right);
}

void correct_update(ruleset *rules, size_t npages, int manual[static npages]) {
  cmp_ruleset = rules;
  qsort(manual, npages, sizeof(int), ruleset_compar);
}

int main(int argc, char **argv) {
  FILE *f = fopen(argv[1], "rb");

  size_t nrules = 0;
  size_t nmanuals = 0;
  rule *rules = parse_rules(f, &nrules);
  int **manuals = parse_manuals(f, &nmanuals);

  ruleset *rset = create_ruleset(nrules, rules);

  fclose(f);

  // Part 1
  size_t middle_sum = 0;
  for (size_t i = 0; i < nmanuals; i++) {
    if (is_valid_update(rset, arrlen(manuals[i]), manuals[i]))
      middle_sum += get_middle_page(arrlen(manuals[i]), manuals[i]);
  }

  printf("Middle sum: %zu\n", middle_sum);

  // Part 2
  middle_sum = 0;
  for (size_t i = 0; i < nmanuals; i++) {
    if (!is_valid_update(rset, arrlen(manuals[i]), manuals[i])) {
      correct_update(rset, arrlen(manuals[i]), manuals[i]);
      middle_sum += get_middle_page(arrlen(manuals[i]), manuals[i]);
    }
  }

  printf("Fixed middle sum: %zu\n", middle_sum);
}
