#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <tgmath.h>

#define STB_DS_IMPLEMENTATION
#include "../stb_ds.h"

typedef struct {
  size_t key;
  size_t value;
} stonecnt;

size_t *apply_rules_part1(size_t stones[]) {
  ptrdiff_t nstones = arrlen(stones);

  for (ptrdiff_t i = nstones - 1; i >= 0; --i) {
    size_t stone = stones[i];
    char digits[100] = {0};
    snprintf(digits, 100, "%zu", stone);
    size_t ndigits = strlen(digits);
    if (stone == 0) {
      stones[i] = 1;
    } else if (ndigits % 2 == 0) {
      char left[ndigits / 2 + 1];
      char right[ndigits / 2 + 1];
      left[ndigits / 2] = '\0';
      right[ndigits / 2] = '\0';
      for (size_t j = 0; j < ndigits / 2; ++j) {
        left[j] = digits[j];
        right[j] = digits[j + ndigits / 2];
      }
      stones[i] = atol(left);
      arrput(stones, atol(right));
    } else {
      stones[i] *= 2024;
    }
  }

  return stones;
}

stonecnt *apply_rules_part2(stonecnt stones[]) {
  ptrdiff_t nuniq = hmlen(stones);

  stonecnt *next_gen = NULL;
  hmdefault(next_gen, 0);

  for (ptrdiff_t i = nuniq - 1; i >= 0; --i) {
    size_t stone = stones[i].key;
    size_t current_count = stones[i].value;
    char digits[100] = {0};
    snprintf(digits, 100, "%zu", stone);
    size_t ndigits = strlen(digits);
    if (stone == 0) {
      size_t c = hmget(next_gen, 1);
      hmput(next_gen, 1, c + current_count);
    } else if (ndigits % 2 == 0) {
      char left[ndigits / 2 + 1];
      char right[ndigits / 2 + 1];
      left[ndigits / 2] = '\0';
      right[ndigits / 2] = '\0';
      for (size_t j = 0; j < ndigits / 2; ++j) {
        left[j] = digits[j];
        right[j] = digits[j + ndigits / 2];
      }

      size_t lv = 0;
      sscanf(left, "%zu", &lv);
      size_t lc = hmget(next_gen, lv);
      hmput(next_gen, lv, lc + current_count);

      size_t rv = 0;
      sscanf(right, "%zu", &rv);
      size_t rc = hmget(next_gen, rv);
      hmput(next_gen, rv, rc + current_count);

    } else {
      size_t new_stone = stone * 2024;
      size_t c = hmget(next_gen, new_stone);
      hmput(next_gen, new_stone, c + current_count);
    }
  }

  hmfree(stones);

  return next_gen;
}

void print_stones(size_t stones[]) {
  for (int j = 0; j < arrlen(stones); ++j) {
    printf("%zu ", stones[j]);
  }
  printf("\n");
  printf("Stone counts: %td\n", arrlen(stones));
}

void print_stonemap(stonecnt stones[]) {
  size_t count = 0;
  for (int i = 0; i < hmlen(stones); ++i) {
    printf("%zu: %zu\t", stones[i].key, stones[i].value);
    if (i != 0 && i % 10 == 0)
      printf("\n");
    count += stones[i].value;
  }
  printf("\n");
  printf("Stone counts: %td\n", count);
}

int main(int argc, char **argv) {
  int blinks = atoi(argv[1]);

  size_t *stones = NULL;
  stonecnt *stonemap = NULL;
  hmdefault(stonemap, 0);

  for (int i = 2; i < argc; ++i) {
    size_t v = atol(argv[i]);
    arrput(stones, v);
    size_t c = hmget(stonemap, v);
    hmput(stonemap, v, c + 1);
  }

  printf("Initial arrangement:\n");
  print_stonemap(stonemap);

  for (int i = 0; i < blinks; ++i) {
    printf("After %d\n", i + 1);
    // stones = apply_rules_part1(stones);
    // print_stones(stones);
    stonemap = apply_rules_part2(stonemap);
    print_stonemap(stonemap);
  }
}
