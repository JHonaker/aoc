#include "../file.c"
#include <assert.h>

void compact_filesystem(size_t n, int buf[static n]);
size_t compute_checksum(size_t n, int buf[static n]);

size_t find_next_space(size_t max, int buf[static max], size_t start) {
  for (size_t i = start; i < max; ++i) {
    if (buf[i] == -1)
      return i;
  }
  return 0;
}

size_t find_earlier_nonspace(size_t max, int buf[static max], size_t end) {
  for (size_t i = end; 0 < i; --i) {
    if (buf[i] != -1)
      return i;
  }

  return 0;
}

size_t find_file_end(size_t max, int buf[static max], size_t idx) {
  size_t id = buf[idx];
  while (0 < idx && idx < max - 1) {
    if (buf[idx + 1] != id)
      return idx;
    ++idx;
  }

  return idx;
}

size_t find_file_start(size_t max, int buf[static max], size_t idx) {
  size_t id = buf[idx];
  while (0 < idx && idx < max) {
    if (buf[idx - 1] != id)
      return idx;
    --idx;
  }

  return idx;
}

size_t get_file_size(size_t max, int buf[static max], size_t file_start) {
  size_t i = 1;
  while (file_start + i < max) {
    if (buf[file_start + i] != buf[file_start])
      return i;
    ++i;
  }

  return i;
}

void compact_filesystem(size_t n, int buf[static n]) {
  size_t startp = find_next_space(n, buf, 0);
  size_t endp = find_earlier_nonspace(n, buf, n - 1);

  do {
    buf[startp] = buf[endp];
    buf[endp] = -1;

    ++startp;
    --endp;

    startp = find_next_space(n, buf, startp);
    endp = find_earlier_nonspace(n, buf, endp);
  } while (startp && endp && startp < endp);
}

int find_compact_file_location(size_t n, int buf[static n], size_t size,
                               size_t file_location) {
  size_t startp = find_next_space(n, buf, 0);
  size_t space_size = get_file_size(n, buf, startp);

  while (startp < file_location) {
    if (space_size >= size)
      return startp;

    startp = find_next_space(n, buf, startp + space_size);
    space_size = get_file_size(n, buf, startp);
  }

  return -1;
}

void move_file(int buf[], size_t dst, size_t src, size_t fsize) {
  for (size_t i = 0; i < fsize; ++i) {
    buf[dst + i] = buf[src + i];
    buf[src + i] = -1;
  }
}

void compact_continguous(size_t n, int buf[static n]) {
  size_t endp = find_earlier_nonspace(n, buf, n - 1);
  endp = find_file_start(n, buf, endp);

  while (endp) {
    if (buf[endp] != -1) {
      size_t fsize = get_file_size(n, buf, endp);
      int loc = find_compact_file_location(n, buf, fsize, endp);

      if (loc != -1) {
        move_file(buf, loc, endp, fsize);
      }
    }

    endp = find_file_start(n, buf, endp - 1);
  }
}

size_t compute_checksum(size_t n, int buf[static n]) {

  size_t checksum = 0;
  for (size_t i = 0; i < n; ++i) {
    if (buf[i] != -1)
      checksum += buf[i] * i;
  }
  return checksum;
}

void print_filesystem(size_t n, int buf[static n]) {
  printf("\n--------\n");
  for (size_t i = 0; i < n; ++i) {
    if (buf[i] == -1)
      printf(".");
    else if (!(i % 20) && i != 0)
      printf("%d\n", buf[i]);
    else
      printf("%d", buf[i]);
  }

  printf("\n--------\n");
}

int main(int argc, char **argv) {
  FileStatus ferr;
  size_t fsize;
  char *raw = read_file(argv[1], &ferr, &fsize);

  size_t len = strlen(raw);
  int *buf = malloc(sizeof(size_t) * len * 10);

  size_t file_id = 0;
  int file_next = 1;

  int filesize = 0;
  size_t bufp = 0;
  for (size_t i = 0; i < len; ++i) {

    switch (raw[i]) {
    case '0':
      filesize = 0;
      break;
    case '1':
      filesize = 1;
      break;
    case '2':
      filesize = 2;
      break;
    case '3':
      filesize = 3;
      break;
    case '4':
      filesize = 4;
      break;
    case '5':
      filesize = 5;
      break;
    case '6':
      filesize = 6;
      break;
    case '7':
      filesize = 7;
      break;
    case '8':
      filesize = 8;
      break;
    case '9':
      filesize = 9;
      break;
    }

    for (size_t p = 0; p < filesize; ++p) {
      if (file_next)
        buf[bufp++] = file_id;
      else
        buf[bufp++] = -1;
    }

    if (file_next)
      ++file_id;

    file_next = !file_next;
  }

  buf = realloc(buf, sizeof(int) * bufp);
  int *copy = malloc(sizeof(int) * bufp);
  memcpy(copy, buf, sizeof(int) * bufp);

  compact_filesystem(bufp, buf);

  size_t checksum = compute_checksum(bufp, buf);

  printf("Checksum: %zu\n", checksum);

  compact_continguous(bufp, copy);

  checksum = compute_checksum(bufp, copy);
  printf("Checksum: %zu\n", checksum);
}
