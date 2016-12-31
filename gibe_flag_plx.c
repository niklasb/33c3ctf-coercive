#include <stdio.h>
#include <string.h>
int main(int argc, char** argv) {
  if (argc != 2) {
    printf("Usage: %s flag_filename\n", argv[0]);
    return 1;
  }
  if (strcmp(argv[1], "/flag")) {
    printf("Not the flag file!\n");
    return 1;
  }
  FILE* f = fopen(argv[1], "r");
  if (!f) {
    printf("Failed opening flag file\n");
    return 1;
  }
  char buf[1024] = {0};
  fread(buf, 1, 1023, f);
  printf("%s\n", buf);
}
