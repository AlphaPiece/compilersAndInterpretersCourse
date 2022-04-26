#include "minicio.h"

int arr[300];

int sum() {
  int s;
  int i;
  s = 0;
  for (i = 0; i < 300; i = i + 1) {
    s = s + arr[i];
  }
  return s;
}

int fillArray(int limit, int N) {
    int i, y;
    y = (N * N) - (2 * N - N);
    for (i = 0; i < limit; i = i + 1) {
      while ((i * N + y) > 300) {
        N = N - 1;
        y = y - N;
      }
      while ((i * N + y) < 0) {
        N = N + N;
        y = y + y;
      }
      if ((i * N + y) > 0 && (i * N + y) < 300) {
          arr[i * N + y] = arr[i * N + y] + (N * y) + 2 * (N * y);
      } else {
        putint(-100);
        putnewline();
      }
    }
    return 1;
}

int main() {
    int N;
    N = getint();
    if (fillArray(300, N) == -1) {
      return -1;
    }
    if (sum() > 10000) {
      putint(1);
      putnewline();
    } else {
      putint(-1);
      putnewline();
    }
    return 0;
}
