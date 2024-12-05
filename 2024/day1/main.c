#include <stdio.h>
#include <stdlib.h>

int comp(void const *l, void const *r) {
  int left = *(int *)l;
  int right = *(int *)r;
  return left - right;
}

int compute_distance(size_t n, int left[static n], int right[static n]) {
  int d = 0;
  for (size_t i = 0; i < n; i++) {
    d += abs(left[i] - right[i]);
  }
  return d;
}

int compute_similarity(size_t n, int left[static n], int right[static n]) {
  int *counts = calloc(n, sizeof(int));

  size_t left_p = 0;
  size_t right_p = 0;

  int r_value = right[right_p++];
  int l_value = left[left_p++];
  while (left_p < n && right_p < n) {
    while (left_p < n && l_value < r_value) {
      l_value = left[left_p++];
      printf("l_value: %d\n", l_value);
    }
    while (right_p < n && r_value < l_value) {
      r_value = right[right_p++];
      printf("r_value: %d\n", r_value);
    }
    while (right_p < n && r_value == l_value) {
      counts[left_p - 1] += 1;
      printf("right_p: %zu", right_p);
      printf("COUNT r_value: %d (%d)\n", r_value, counts[left_p - 1]);
      r_value = right[right_p++];
    }

    if (right_p == n && r_value == l_value) {
      counts[left_p - 1] += 1;
    }

    while (left_p < n && l_value == left[left_p]) {
      counts[left_p] = counts[left_p - 1];
      printf("left[left_p] == l_value: %d\n", l_value);
      l_value = left[left_p++];
    }
  }

  int similarity = 0;
  for (size_t i = 0; i < n; i++) {
    similarity += left[i] * counts[i];
  }

  return similarity;
}

int main(int argc, char **argv) {
  char const *filename = argv[1];

  FILE *f = fopen(filename, "rb");

  char *buffer = NULL;
  size_t len = 0;
  ssize_t nread = 0;

  size_t list_cap = 100;
  int *left = malloc(sizeof(*left) * list_cap);
  int *right = malloc(sizeof(*right) * list_cap);
  size_t i = 0;
  while ((nread = getline(&buffer, &len, f)) != -1) {
    sscanf(buffer, "%u   %u", &left[i], &right[i]);
    i++;

    if (i >= list_cap) {
      list_cap *= 2;
      left = reallocarray(left, list_cap, sizeof(left[0]));
      right = reallocarray(right, list_cap, sizeof(right[0]));
    }
  }

  qsort(left, i, sizeof(left[0]), comp);
  qsort(right, i, sizeof(right[0]), comp);

  printf("%d\n", compute_distance(i, left, right));
  printf("%d\n", compute_similarity(i, left, right));

  fclose(f);

  free(buffer);
  free(left);
  free(right);
}
