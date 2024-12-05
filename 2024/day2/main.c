#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct level {
  size_t length;
  size_t capacity;
  int data[];
};

struct level *parse_single_level(char *buffer) {
  if (!buffer)
    exit(EXIT_FAILURE);

  struct level *numbers = malloc(sizeof(struct level) + 15 * sizeof(int));

  numbers->capacity = 15;

  int i = 0;
  char *token = strtok(buffer, " ");
  while (token && i < numbers->capacity) {
    numbers->data[i] = atoi(token);
    i++;
    token = strtok(NULL, " ");
  }

  numbers->length = i;

  return numbers;
}

struct level **parse_levels(FILE *f, size_t *n_levels) {
  size_t nread = 0;
  char *buffer = NULL;

  size_t level_count_max = 1 << 5;
  size_t level_count = 0;

  struct level **levels = malloc(level_count_max * sizeof(struct level *));
  while ((nread = getline(&buffer, &nread, f)) != -1) {

    levels[level_count] = parse_single_level(buffer);

    level_count++;

    if (level_count >= level_count_max) {
      level_count_max *= 2;
      levels = realloc(levels, level_count_max * sizeof(int *));
    }
  }

  free(buffer);

  *n_levels = level_count;

  return levels;
}

bool safe_jump(bool increasing, int cur, int next) {
  int diff = next - cur;
  int absdiff = abs(diff);

  if (absdiff < 1 || 3 < absdiff)
    return false;
  if (increasing && diff <= 0)
    return false;
  if (!increasing && diff >= 0)
    return false;

  return true;
}

bool is_safe_ex(bool increasing, size_t low, size_t high,
                int buffer[high - low]) {
  if (low > high || high - low <= 1)
    return true;

  for (size_t i = low; i < high - 1; i++) {
    if (!safe_jump(increasing, buffer[i], buffer[i + 1]))
      return false;
  }

  return true;
}

bool is_safe_report(bool increasing, struct level *l) {
  return is_safe_ex(increasing, 0, l->length, l->data);
}

bool safe_with_removal(bool increasing, struct level *l) {
  if (is_safe_ex(increasing, 0, l->length, l->data))
    return true;

  for (size_t i = 0; i < l->length; i++) {
    // Dropping item i
    bool left_safe = is_safe_ex(increasing, 0, i, l->data);
    bool right_safe = is_safe_ex(increasing, i + 1, l->length, l->data);
    bool jump_safe =
        (i == 0 || i == (l->length - 1))
            ? true
            : safe_jump(increasing, l->data[i - 1], l->data[i + 1]);
    if (left_safe && right_safe && jump_safe)
      return true;
  }

  return false;
}

int main(int argc, char **argv) {
  FILE *f = fopen(argv[1], "rb");

  size_t num_levels = 0;
  struct level **levels = parse_levels(f, &num_levels);

  size_t num_safe = 0;
  size_t num_safe_with_tol = 0;
  for (size_t j = 0; j < num_levels; j++) {
    for (size_t i = 0; i < levels[j]->length; i++) {
      printf("%d\t", levels[j]->data[i]);
    }
    if (is_safe_report(true, levels[j]) || is_safe_report(false, levels[j])) {
      num_safe++;
      printf("SAFE\t");
    } else {
      printf("UNSAFE\t");
    }

    if (safe_with_removal(true, levels[j]) ||
        safe_with_removal(false, levels[j])) {
      num_safe_with_tol++;
      printf("SAFE\n");
    } else {
      printf("UNSAFE\n");
    }
  }

  printf("Safe count: %zu\n", num_safe);
  printf("Safe count: %zu\n", num_safe_with_tol);

  for (size_t i = 0; i < num_levels; i++) {
    free(levels[i]);
  }
  free(levels);
}
