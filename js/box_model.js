"use strict";

/* box model object will be stored in this global variable: */
var box_model = undefined;

/* box model object definition: */
var BoxModel = function(args) {

  /** variables: **/

  /* get instance and memory from arguments: */
  const instance = args['instance'];
  const memory = args['memory'];
  /* number of species is fixed in the model code: */
  const n_spec = 16;

  /* object variables: */
  var _self = this;
  this.instance = instance;
  this.memory = memory;

  /* model variables ... month, 0 -> 11 (int): */
  this.month = 0;
  /* length in days, 1 -> 31 (int) */
  this.length = 7;
  /* latitude -90 -> 90 (float): */
  this.lat = 54.0;
  /* min and max temperature in K, 150 -> 373 (float): */
  this.min_temp = 263.0;
  this.max_temp = 293.0;
  /* pressure in mb, 10 -> 1000 (float): */
  this.pressure = 1000.0;
  /* water vapour in %, 0 -> 100 (float): */
  this.h2o = 0.3;
  /* NO emission rate in mol/cc/s, 0 -> 3e7, 100 steps (float): */
  this.no = 6e5;
  /* NO initial concentration in ppb (float): */
  this.no_ini = 2;
  /* VOC emission rate in mol/cc/s, 0 -> 3e6, 100 steps (float): */
  this.voc = 1e6;
  /* NO initial concentration in ppb (float): */
  this.voc_ini = 25;
  /* CO emission rate in mol/cc/s, 0 -> 6e7, 100 steps (float): */
  this.co = 1e6;
  /* CO initial concentration in ppb (float): */
  this.co_ini = 100;
  /* Traffic signal, 0 -> 1 (float): */
  this.cycle = 0.0;
  /*
   * Surface (int):
   *   0 = vegetation
   *   1 = urban
   *   2 = water
   */
  this.surface = 0;

  /* number of species: */
  this.n_spec = n_spec;
  /* number of time steps in model: */
  this.max_tstep = 336;
  this.n_tstep = undefined;
  /* time step increment size in days: */
  this.sz_tstep = undefined;
  /* time step values: */
  this.tsteps = undefined;
  /* model result: */
  this.result_ptr = undefined;
  this.result = undefined;

  /* species details / results: */
  this.species = {
    'O3': {
      'index': 0,
      'units': 'ppb',
      'values': []
    },
    'NO': {
      'index': 1,
      'units': 'ppb',
      'values': []
    },
    'NO2': {
      'index': 2,
      'units': 'ppb',
      'values': []
    },
    'OH': {
      'index': 3,
      'units': 'per cm³',
      'values': []
    },
    'HO2': {
      'index': 4,
      'units': 'ppt',
      'values': []
    },
    'CH3O2': {
      'index': 5,
      'units': 'ppt',
      'values': []
    },
    'CH2O': {
      'index': 6,
      'units': 'ppb',
      'values': []
    },
    'CH3OOH': {
      'index': 7,
      'units': 'ppb',
      'values': []
    },
    'H2O2': {
      'index': 8,
      'units': 'ppb',
      'values': []
    },
    'HNO3': {
      'index': 9,
      'units': 'ppb',
      'values': []
    },
    'NO3': {
      'index': 10,
      'units': 'ppt',
      'values': []
    },
    'N2O5': {
      'index': 11,
      'units': 'ppt',
      'values': []
    },
    'C3H6': {
      'index': 12,
      'units': 'ppb',
      'values': []
    },
    'CO': {
      'index': 13,
      'units': 'ppb',
      'values': []
    },
    'NOx': {
      'index': 14,
      'units': 'ppb',
      'values': []
    },
    'NOy': {
      'index': 15,
      'units': 'ppb',
      'values': []
    }
  };

  /** functions: **/

  /* calculate time step values: */
  this._calculate_tstep = function(obj) {
    /*
     * time step size is in days, and changes depending upon number of days
     * with a minimum resolution of 1 hour and maximum of 5mins
     */
    var sz_tstep = obj.length / obj.max_tstep;
    if (sz_tstep > 1 / 24) { sz_tstep = 1 / 24; };
    if (sz_tstep < 1 / (24 * 12)) { sz_tstep = 1 / (24 * 12); };
    var n_tstep = Math.round(obj.length / sz_tstep);
    obj.n_tstep = n_tstep;
    obj.sz_tstep = sz_tstep;
    /* time step values: */
    var tsteps = [];
    for (var i = 0; i < n_tstep; i++) {
      tsteps[i] = i * sz_tstep;
    };
    obj.tsteps = tsteps;
  };
  this.calculate_tstep = function() {
    _self._calculate_tstep(_self);
  };

  /* store results function: */
  this._store_results = function(obj) {
    /* loop through species: */
    for (var spec in obj.species) {
      /* get species index: */
      var spec_index = obj.species[spec]['index'];
      /* get species values: */
      var spec_values = obj.result.slice(
        spec_index * obj.n_tstep,
        (spec_index * obj.n_tstep) + obj.n_tstep
      );
      obj.species[spec]['values'] = spec_values;
    };
  };
  this.store_results = function() {
    _self._store_results(_self);
  };

  /* run model function: */
  this._run_model = async function(obj) {
    /* get instance and memory: */
    const instance = obj.instance;
    const memory = obj.memory;
    /* if model result is defined: */
    if (obj.result_ptr != undefined) {
      /* free the memory: */
      instance.exports.free(obj.result_ptr);
    };
    /* calculate time step values: */
    obj.calculate_tstep();
    /* run the model ... : */
    obj.result_ptr = instance.exports.run_model(
      obj.month,
      obj.length,
      obj.lat,
      obj.min_temp,
      obj.max_temp,
      obj.pressure,
      obj.h2o,
      obj.no,
      obj.no_ini,
      obj.voc,
      obj.voc_ini,
      obj.co,
      obj.co_ini,
      obj.cycle,
      obj.surface,
      obj.n_tstep,
      obj.sz_tstep
    );
    /*
     * length of output array
     * length is number of species * number of time steps:
     */
    const result_len = obj.n_spec * obj.n_tstep;
    /* create array for result: */
    obj.result = new Float32Array(memory.buffer, obj.result_ptr,
                                  result_len);
    /* store model results: */
    obj.store_results();
    /* return: */
    return true;
  };
  this.run_model = async function() {
    var result = _self._run_model(_self);
    return result;
  };

};

/* function to init wasm object: */
async function __load_box_model(wasm_file) {
  /* set up memory: */
  const memory = new WebAssembly.Memory({
    /* units are web assembly pages, where 1 page = 64 KiB: */
    'initial': 2,
    'maximum': 2048
  });
  /* get wasm file: */
  const response = await fetch(wasm_file);
  /* check response: */
  if (response.ok != true) {
    /* if no response, return here: */
    return;
  };
  const bytes = await response.arrayBuffer();
  /* instantiate instance: */
  const { instance } = await WebAssembly.instantiate(bytes, {
    'env': { memory },
    /* these wasi libc functions need to be defined ... : */
    'wasi_snapshot_preview1': {
       'clock_time_get': function () {},
       'environ_get': function () {},
       'environ_sizes_get': function () {},
       'fd_close': function () {},
       'fd_fdstat_get': function () {},
       'fd_fdstat_set_flags': function () {},
       'fd_filestat_get': function () {},
       'fd_filestat_set_size': function () {},
       'fd_prestat_dir_name': function () {},
       'fd_prestat_get': function () {},
       'fd_readdir': function () {},
       'fd_read': function () {},
       'fd_renumber': function () {},
       'fd_seek': function () {},
       'fd_write': function () {},
       'path_create_directory': function () {},
       'path_filestat_get': function () {},
       'path_filestat_set_times': function () {},
       'path_link': function () {},
       'path_open': function () {},
       'path_readlink': function () {},
       'path_remove_directory': function () {},
       'path_rename': function () {},
       'path_symlink': function () {},
       'path_unlink_file': function () {},
       'proc_exit': function () {}
    }
  });
  /* return instance and memory: */
  return {
    'instance': instance,
    'memory': memory
  };
};

/* function to load wasm and init model: */
async function init_box_model(wasm_file) {
  return __load_box_model(wasm_file).then(
    function(response) {
      /* if response received: */
      if (response != undefined) {
        /* create box model object from response: */
        box_model = new BoxModel(response);
      };
      return box_model;
    }
  );
};
