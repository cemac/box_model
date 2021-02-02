#include <math.h>
#include <stdlib.h>
#include <f2c.h>

#define MIN(a,b) (((a)<(b))?(a):(b))
#define MAX(a,b) (((a)>(b))?(a):(b))
#define PI 3.14159265358979323846

/* global variables ... : */
float *JJ, *KR, *CONST_SPEC, *DERIVS_VARS;

/* function to calculate solar zenith angle: */
float solzenith(float utc, float lat, float lon) {
  /* variables: */
  float daynum, eta, delta, et, eqt, dtr, time, omega, sinh, solel, sza;
  /* calculate the number of days since the 1.1. of the specific year: */
  daynum = floor(utc) + 1.;
  /*
   * calculate the relative sun-earth distance for the given day resulting
   * from the elliptic orbit of the earth:
   */
  eta = 2. * PI * daynum / 365.;
  /*
   * calculate the solar declination for the given day the declination varies
   * due to the fact, that the earth rotation axis is not perpendicular to the
   * ecliptic plane:
   */
  delta = 0.006918 - 0.399912 * cos(eta) - 0.006758 * cos (2. * eta) - 0.002697
          * cos(3. * eta) + 0.070257 * sin(eta) + 0.000907 * sin(2. * eta) +
          0.001480 * sin(3. * eta);
  /*
   * equation of time, used to compensate for the earth's elliptical orbit
   * around the sun and its axial tilt when calculating solar time eqt is the
   * correction in hours:
   */
  et = 2. * PI * daynum / 366.;
  eqt = 0.0072 * cos(et) - 0.0528 * cos(2. * et) - 0.0012 * cos(3. * et) - 0.1229
        * sin(et) - 0.1565 * sin(2. * et) - 0.0041 * sin(3. * et);
  /*
   * calculate the solar zenith angle ... degrees to radian conversion factor:
   */
  dtr = PI / 180.;
  /* time in hours: */
  time = (utc + 1. - daynum) * 24.;
  omega = (360. / 24.) * (time + lon / 15. + eqt - 12.) * dtr;
  sinh = sin(delta) * sin(lat * dtr) + cos(delta) * cos(lat * dtr)
         * cos(omega);
  solel = asin(sinh);
  sza = PI / 2. - solel;
  /* return the solar zenith angle: */
  return sza;
};

/* function to calculate emission rates: */
int rates(float *zenith, float *pressure, float *temp) {
  float h2o, m, ki, k0, fc, kra, f;
  float *kmt0;
  h2o = CONST_SPEC[0];
  m = *pressure * 100 * 1e-6 / (8.314 * *temp ) * 6.022e23;
  kmt0 = calloc(10, sizeof(float));
  if (*zenith < (89. * PI / 180.)) {
    JJ[0] = 0.;
    JJ[1] = 6.073e-5 * cos(pow(*zenith, 1.743)) * exp(-0.474 / cos(*zenith));
    if (JJ[1] < 0.) {
      JJ[1] = 0.;
    };
    JJ[1] = JJ[1] * 2.2e-10 * h2o * m /
            (2.2e-10 * h2o * m + 2.14e-11 * exp(110 / *temp) * 0.78 * m +
             3.2e-11 * exp (70. / *temp) * 0.21 * m);
    JJ[2] = 1.165e-2 * cos(pow(*zenith, 0.244)) * exp(-0.267 / cos(*zenith));
    JJ[3] = 6.853e-5 * cos(pow(*zenith, 0.477)) * exp(-0.353 / cos(*zenith));
    JJ[4] = 4.642e-5 * cos(pow(*zenith, 0.762)) * exp(-0.353 / cos(*zenith));
    JJ[5] = 2.485e-2 * cos(pow(*zenith, 0.168)) * exp(-0.108 / cos(*zenith));
    JJ[6] = 1.747e-1 * cos(pow(*zenith, 0.155)) * exp(-0.125 / cos(*zenith));
  } else {
    JJ[0] = 0.;
    JJ[1] = 0.;
    JJ[2] = 0.;
    JJ[3] = 0.;
    JJ[4] = 0.;
    JJ[5] = 0.;
    JJ[6] = 0.;
  };
  KR[0] = 0.;
  KR[1] = 0.;
  KR[2] = 0.;
  KR[3] = 1.4e-12 * exp(-1310. / *temp);
  kmt0[5] = 1.;
  KR[4] = 1.3e-13 * kmt0[5];
  KR[5] = 9.65e-20 * (pow(*temp, 2.58)) * exp(-1082. / *temp);
  KR[6] = 3.6e-12 * exp(270. / *temp);
  KR[7] = 2.03e-16 * (pow((*temp / 300.), 4.57)) * exp(693. / *temp);
  KR[8] = 1.82e-13 * exp(416. / *temp);
  KR[9] = 3.8e-13 * exp(780. / *temp);
  kmt0[6] = 1.;
  KR[10] = 2.2e-13 * kmt0[6] * exp(600. / *temp) + 1.9e-33 * m * kmt0[6] *
           exp(980. / *temp);
  ki = 4.1e-11;
  k0 = 3.3e-30 * pow(*temp / 300., -3.) * m;
  fc = 0.4;
  kra = k0 / ki;
  f = pow(10., log10(fc) / (1. + pow(log10(kra), 2.)));
  kmt0[8] = k0 * ki * f / (k0 + ki);
  KR[11] = kmt0[8];
  KR[12] = 4.8e-11 * exp(250. / *temp);
  KR[13] = 1.82e-13 * exp(416. / *temp);
  KR[14] = 1.4e-13 * exp(-2470. / *temp);
  KR[15] = 1.8e-11 * exp(110. / *temp);
  k0 = 3.6e-30 * pow(*temp / 300., -4.1) * m;
  ki = 1.9e-12 * pow(*temp / 300., 0.2);
  fc = 0.35;
  kra = k0 / ki;
  f = pow(10., log10(fc) / (1. + pow(log10(kra), 2.)));
  kmt0[3] = k0 * ki * f / (k0 + ki);
  KR[16] = kmt0[3];
  k0 = 1e-3 * pow(*temp / 300., -3.5) * exp(-11000. / *temp) * m;
  ki = 9.7e14 * pow(*temp / 300., 0.1) * exp(-11080. / *temp);
  fc = 0.35;
  kra = k0 / ki;
  f = pow(10., log10(fc) / (1. + pow(log10(kra), 2.)));
  kmt0[4] = k0 * ki * f / (k0 + ki);
  KR[17] = kmt0[4];
  KR[19] = 4e-4;
  KR[20] = 4.5e-14 * exp(-1260. / *temp);
  KR[21] = 3e-11 * pow(*temp / 300., -1.);
  KR[22] = 4.6e-13 * exp(-1155. / *temp);
  KR[23] = 5.5e-15 * exp(-1880. / *temp);
  free(kmt0);
  return 0;
};

/* function to calculate derivatives for lsode: */
int derivs(int *neq, float *t, float *y, float *ydot) {
  int i;
  float o3, no, no2, oh, ho2, ch3o2, ch2o, no3, n2o5, c3h6, co, nspec,
        pressure, temp, co_rate, ch4, c3h6_rate, no_rate, dep_rate, m;
  o3 = y[0];
  no = y[1];
  no2 = y[2];
  oh = y[3];
  ho2 = y[4];
  ch3o2 = y[5];
  ch2o = y[6];
  no3 = y[10];
  n2o5 = y[11];
  c3h6 = y[12];
  co = y[13];
  nspec = DERIVS_VARS[0];
  pressure = DERIVS_VARS[1];
  temp = DERIVS_VARS[2];
  co_rate = CONST_SPEC[1];
  ch4 = CONST_SPEC[2];
  c3h6_rate = CONST_SPEC[3];
  no_rate = CONST_SPEC[4];
  dep_rate = CONST_SPEC[5];
  m = pressure * 1e2 * 1e-6 / (8.314 * temp) * 6.022e23;
  ydot[0] = -JJ[1] * o3 * m + JJ[2] * no2 * m - KR[7] * ho2 * m * o3 * m +
            JJ[6] * no3 * m - KR[14] * no2 * m * o3 * m - KR[3] * o3 * no * m *
            m - KR[23] * o3 * m * c3h6 * m - dep_rate * o3 * m;
  ydot[1] = no_rate + JJ[2] * no2 * m - KR[3] * no * m * o3 * m - KR[6] *
            ho2 * m * no * m - KR[8] * ch3o2 * m * no * m + JJ[5] * no3 * m -
            KR[15] * no * m * no3 * m + KR[20] * no3 * m * no2 * m;
  ydot[2] = -JJ[2] * no2 * m + KR[3] * no * m * o3 * m + KR[6] * ho2 * m *
            no * m + KR[8] * ch3o2 * m * no * m - KR[11] * oh * m * no2 * m +
            JJ[6] * no3 * m - KR[14] * no2 * m * o3 * m + 2. * KR[15] * no *
            m * no3 * m - KR[16] * no2 * m * no3 * m + KR[17] * n2o5 * m +
            KR[18];
  ydot[3] = 2. * JJ[1] * o3 * m - KR[4] * co * m * oh * m - KR[5] * ch4 * m *
            oh * m + KR[6] * ho2 * m * no * m + KR[7] * ho2 * m * o3 * m -
            KR[11] * oh * m * no2 * m - KR[12] * oh * m * ho2 * m - KR[21] *
            oh * m * c3h6 * m;
  ydot[4] = KR[4] * co * m * oh * m - KR[6] * ho2 * m * no * m - KR[7] * ho2 *
            m * o3 * m + KR[8] * ch3o2 * m * no * m - KR[9] * ch3o2 * m *
            ho2 * m - 2. * KR[10] * ho2 * m * ho2 * m - KR[12] * oh * m * ho2 *
            m + 2. * JJ[4] * ch2o;
  ydot[5] = KR[5] * oh * m * ch4 * m + KR[21] * oh * m * c3h6 * m + KR[22] *
            no3 * m * c3h6 * m + KR[23] * o3 * m * c3h6 * m - KR[8] * ch3o2 *
            m * no * m - KR[9] * ch3o2 * m * ho2 * m - 2. * KR[13] * ch3o2 *
            m * ch3o2 * m;
  ydot[6] = KR[8] * ch3o2 * m * no * m - JJ[3] * ch2o * m - JJ[4] * ch2o * m;
  ydot[7] = KR[9] * ch3o2 * m * ho2 * m;
  ydot[8] = KR[10] * ho2 * m * ho2 * m;
  ydot[9] = KR[11] * oh * m * no2 * m + 2. * KR[19] * n2o5 * m;
  ydot[10] = KR[14] * no2 * m * o3 * m - JJ[5] * no3 * m - JJ[6] * no3 * m -
             KR[15] * no * m * no3 * m - KR[16] * no2 * m * no3 * m + KR[17] *
             n2o5 * m - KR[20] * no2 * m * no3 * m - KR[22] * no3 * m * c3h6 *
             m;
  ydot[11] = KR[16] * no2 * m * no3 * m - KR[17] * n2o5 * m - KR[19] * n2o5 *
             m;
  ydot[12] = c3h6_rate + KR[21] * oh * m * c3h6 * m - KR[22] * no3 * m *
             c3h6 * m - KR[23] * o3 * m * c3h6 * m;
  ydot[13] = co_rate - KR[4] * co * m * oh * m;
  for (i = 0; i < nspec; i++) {
    ydot[i] = ydot[i] / m;
  };
  return 0;
};

/* dummy function to calculate jacobian for lsode: */
int jac(int *neq, float *t, float *y, int *ml, int *mu, float *pd,
        int *nrowpd) {
  return 0;
};

/* wrapper function slsode: */
void lsode(float *Y, float X, float H, S_fp derivs, int *istate, int neq) {
  int itol, itask, iopt, lrw, liw, mf;
  float tout, rtol, atol;
  int *iwork;
  float *rwork;
  extern int slsode_(S_fp, int *, float *, float *, float *, int *, float *,
                     float *, int *, int *, int *, float *, int *, int *,
                     int *, U_fp, int *);
  tout = X + H;
  itol = 1;
  rtol = 1e-4;
  atol = 1e-15;
  itask = 1;
  iopt = 0;
  rwork = calloc(22 + (9 * neq) + (neq * neq), sizeof(float));
  lrw = 22 + (9 * neq) + (neq * neq);
  iwork = calloc(20 + neq, sizeof(int));
  liw = 20 + neq;
  mf = 22;
  slsode_(
    derivs, // function for rhs
    &neq,   // number of odes (in)
    Y,      // y vector, length = neq (inout)
    &X,     // position of x (inout)
    &tout,  // next position of x (in)
    &itol,  // 1 if atol is scalar, 2 if atol is array (in)
    &rtol,  // relative tolerance (in)
    &atol,  // absolute tolerance (in)
    &itask, // slsode task, 1 = normal computation (in)
    istate, // state of calculation (inout)
    &iopt,  // optional values are being used (in)
    rwork,  // real work array
    &lrw,   // length of rwork
    iwork,  // integer work array
    &liw,   // length of iwork
    jac,    // jac function
    &mf     // method flag
  );
  free(rwork);
  free(iwork);
};

/* function to run the model: */
float *run_model(float month, int length, float lat, float min_temp,
                 float max_temp, float pressure, float h2o, float no,
                 float no_ini, float voc, float voc_ini, float co,
                 float co_ini, float cycle, int surface, int ntstep,
                 float dt) {

  /* variables: */
  int i, j, nspec, day, nzsteps, sec20hr, cyc_size, status, keep_offset;
  float lon, dtr, tmean, nmol_air, mxzen, mnzen, cyc_sum, day_scl, depmax,
        depmin, time, emis_fact, zenith, temp;
  float *zeniths, *cyc, *y, *keep;
  /* scale values for output: */
  float scl[16] = {1e-9,  1e-9,  1e-9,  1.,
                   1e-12, 1e-12, 1e-9,  1e-9,
                   1e-9,  1e-9,  1e-12, 1e-12,
                   1e-9,  1e-9,  1e-9,  1e-9};

  /* scale input values: */
  h2o = h2o * 1e-2;
  no_ini = no_ini * 1e-9;
  voc_ini = voc_ini * 1e-9;
  co_ini = co_ini * 1e-9;

  /* number of species: */
  nspec = 14;
  /* longitude: */
  lon = 0.;
  /* roughly middle day of month: */
  day = (month * 30) + 15;
  /* temerature range: */
  dtr = max_temp - min_temp;

  /* number of molecules per cm^3: */
  tmean = (max_temp + min_temp) / 2.;
  nmol_air = (pressure * 100 * 1e-6) / (1.3807e-23 * tmean);
  /* update ppc scale value: */
  scl[3] = 1. / nmol_air;

  /* calculate range of solar zenith angles: */
  nzsteps = length * 24 * 12;
  zeniths = calloc(nzsteps, sizeof(float));
  mxzen = 0.;
  mnzen = 0.;
  for (i = 0; i < nzsteps; i++) {
    zeniths[i] = solzenith(day + (i / (24. * 12.)), lat, lon);
    if (i == 0) {
      mxzen = zeniths[i];
      mnzen = zeniths[i];
    } else {
      mxzen = MAX(mxzen, zeniths[i]);
      mnzen = MIN(mnzen, zeniths[i]);
    };
  };
  free(zeniths);

  /* daily cycle. 20hr sin^2 cycle to describe bidaily rush hour: */
  sec20hr = 20 * 60 * 60;
  cyc_size = 24 * 60 * 60;
  cyc = calloc(cyc_size, sizeof(float));
  cyc_sum = 0.;
  for (i = 0; i < cyc_size; i++) {
    if (i < 7199 || i > 7199 + sec20hr - 1) {
      cyc[i] = 0.;
    } else {
      cyc[i] = cycle * 2. * pow(sin(i / i * 2. * PI), 2.);
    };
    cyc_sum += cyc[i];
  };
  /* normalize. needs to total 86400 so that emission rate averages 1/s: */
  day_scl = (86400. - cyc_sum) / 86400.;
  for (i = 0; i < cyc_size; i++) {
    cyc[i] += day_scl;
  };

  /*
   * constant species:
   *   [0] = H2O
   *   [1] = COemit_rate
   *   [2] = CH4
   *   [3] = C3H6emit_rate
   *   [4] = NOemit_rate
   *   [5] = dep_rate
   */
  CONST_SPEC = calloc(6, sizeof(float));
  CONST_SPEC[0] = h2o;
  CONST_SPEC[1] = 0.;
  CONST_SPEC[2] = 1770e-9;
  CONST_SPEC[3] = 0.;
  CONST_SPEC[4] = 0.;
  CONST_SPEC[5] = 0.;

  /*
   * put in deposition rate as a function of the surface type assuming a
   * boundary layer height of 1km depmax & depmin are noon and midnight
   * depositions as a fraction of boundary layer ozone lost per second:
   */
  if (surface == 0) {
    depmax = 1.00e-2 / 1000.;
    depmin = 0.30e-2 / 1000.;
  } else if (surface == 1) {
    depmax = 0.26e-2 / 1000.;
    depmin = 0.26e-2 / 1000.;
  } else {
    depmax = 0.05e-2 / 1000.;
    depmin = 0.05e-2 / 1000.;
  };

  /* initialise concentrations: */
  y = calloc(nspec, sizeof(float));
  for (i = 0; i < nspec; i++) {
    y[i] = 0;
  };
  y[0] = 30e-9;
  y[1] = no_ini;
  y[2] = 10e-12;
  y[12] = voc_ini;
  y[13] = co_ini;

  /* allocate global arrays ... : */
  JJ = calloc(7, sizeof(float));
  KR = calloc(24, sizeof(float));
  DERIVS_VARS = calloc(3, sizeof(float));
  /*
   * variables for derivs function:
   *   [0] = nspec
   *   [1] = pressure
   *   [2] = temp
   */
  DERIVS_VARS[0] = (float) nspec;
  DERIVS_VARS[1] = pressure;

  /* init lsode status flag: */
  status = 1;

  /* calculate output values: */
  keep = calloc((nspec + 2) * ntstep, sizeof(float));
  for (i = 0; i < ntstep; i++) {
    time = i * dt;
    emis_fact = cyc[((int) (dt * 86400) % 86400)];
    /* NOemit_rate: */
    CONST_SPEC[4] = no * emis_fact;
    /* COemit_rate: */
    CONST_SPEC[1] = co * emis_fact;
    /* C3H6emit_rate: */
    CONST_SPEC[3] = voc * emis_fact;
    zenith = solzenith(day + time, lat, lon);
    temp = min_temp + dtr * (1. - (zenith - mnzen) / (mxzen - mnzen));
    DERIVS_VARS[2] = temp;
    /* dep_rate: */
    CONST_SPEC[5] = depmin + (depmax - depmin) *
                    (1. - (zenith - mnzen) / (mxzen - mnzen));
    /* calculate rates: */
    rates(&zenith, &pressure, &temp);
    /* check status: */
    if (i > 1 && status != 2) {
      for (j = 0; j < nspec; j++) {
        y[j] = 0.;
      };
    } else {
      status = 1;
    };
    /* solve with lsode: */
    lsode(y, time, dt * 60. * 60. * 24., derivs, &status, nspec);
    /* store results: */
    for (j = 0; j < nspec; j++) {
      keep_offset = (j * ntstep) + i;
      /* anything below 0 ... : */
      if (y[j] < 0) {
        keep[keep_offset] = 1e-15 / scl[j];
      } else {
        /* scale and store: */
        keep[keep_offset] = y[j] / scl[j];
      };
    };
    /* calculate NOx and NOy values ... NOx: */
    keep_offset = (nspec * ntstep) + i;
    keep[keep_offset] = (y[1] + y[2]) / scl[nspec];
    /* NOy: */
    keep_offset = ((nspec + 1) * ntstep) + i;
    keep[keep_offset] = (y[1] + y[2] + y[9] + y[10] + 2 * y[11]) /
                        scl[nspec + 1];
  };

  /* free memory: */
  free(cyc);
  free(y);
  free(JJ);
  free(KR);
  free(CONST_SPEC);
  free(DERIVS_VARS);

  /* return results: */
  return keep;
};
