#include <stdio.h>
#include <stdlib.h>

float interpolated(float x0, float y0, float x1, float y1, float x)
{
    float t;

    if (x <= x0) { return y0; }
    if (x >= x1) { return y1; }
    return y0 + (x - x0) * (y1 - y0) / (x1 - x0);
}


float interpolator (float x) {
    int     i;
    float   x0, y0, x1, y1;
    int n = 2;
    float points1[] = {1.0,2.0};
    float points2[] = {3.0,4.0};
    
    
    for (i = 0; i < n ; i++) {
      x0 = points1[i];
      y0 = points2[i];
      x1 = points1[i + 1];
      y1 = points2[i + 1];
      if (x > x0 && x <= x1){ // find the point interval
        return interpolated (x0,y0,x1,y1,x);
      }
    }
  }

int main(void) {
  float res;
  res = interpolator(1.5);
  printf("%f\n" , res);
  return 0;
}  
  


