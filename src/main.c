#include <stdio.h>
#include <stdlib.h>

extern float *run_model(float, int , float, float, float, float, float, float,
                        float, float, float, float, float, float, int, int,
                        float);

int main() {
  int i, length, surface, ntstep;
  float month, lat, min_temp, max_temp, pressure, h2o, no, no_ini, voc,
        voc_ini, co, co_ini, cycle, dt;
  float *result;

  month = 0;
  length = 7;
  lat = 54.0;
  min_temp = 263.0;
  max_temp = 293.0;
  pressure = 1000.0;
  h2o = 0.3;
  no = 6e5;
  no_ini = 2;
  voc = 1e6;
  voc_ini = 25;
  co = 1e6;
  co_ini = 100;
  cycle = 0.0;
  surface = 0;
  ntstep = 336;
  dt = 0.020833;

  result = run_model(
    month,
    length,
    lat,
    min_temp,
    max_temp,
    pressure,
    h2o,
    no,
    no_ini,
    voc,
    voc_ini,
    co,
    co_ini,
    cycle,
    surface,
    ntstep,
    dt
  );

  fprintf(stdout, "ch3o2 = [");
  for (i = 0; i < ntstep; i++) {
    fprintf(stdout, "%e", result[(5 * ntstep) + i]);
    if (i < (ntstep - 1)) {
      fprintf(stdout, ", ");
    };
  };
  fprintf(stdout, "]\n");

  free(result);

  return 0;
};
