"use strict";

/* plotting variables: */
var plot_vars = {
  'plots_div': document.getElementById('plots'),
  'x_title': 'Time (days)',
  'species': {},
  'day_range': document.getElementById('day_range'),
  'day_value': document.getElementById('day_value'),
  'month_range': document.getElementById('month_range'),
  'month_value': document.getElementById('month_value'),
  'lat_range': document.getElementById('lat_range'),
  'lat_value': document.getElementById('lat_value'),
  'surface_range': document.getElementById('surface_range'),
  'surface_value': document.getElementById('surface_value'),
  'temp_range': document.getElementById('temp_range'),
  'temp_value': document.getElementById('temp_value'),
  'press_range': document.getElementById('press_range'),
  'press_value': document.getElementById('press_value'),
  'no_ini_range': document.getElementById('no_ini_range'),
  'no_ini_value': document.getElementById('no_ini_value'),
  'no_range': document.getElementById('no_range'),
  'no_value': document.getElementById('no_value'),
  'voc_ini_range': document.getElementById('voc_ini_range'),
  'voc_ini_value': document.getElementById('voc_ini_value'),
  'voc_range': document.getElementById('voc_range'),
  'voc_value': document.getElementById('voc_value'),
  'co_ini_range': document.getElementById('co_ini_range'),
  'co_ini_value': document.getElementById('co_ini_value'),
  'co_range': document.getElementById('co_range'),
  'co_value': document.getElementById('co_value'),
  'cycle_range': document.getElementById('cycle_range'),
  'cycle_value': document.getElementById('cycle_value'),
  'h2o_range': document.getElementById('h2o_range'),
  'h2o_value': document.getElementById('h2o_value'),
  'run_model': document.getElementById('run_model'),
  'save_csv': document.getElementById('save_csv')
};

/* function to hide / display plots: */
function show_save(value) {
  if (value == true) {
    plot_vars['run_model'].style.display = 'none';
    plot_vars['save_csv'].style.display = 'flex';
  } else {
    plot_vars['save_csv'].style.display = 'none';
    plot_vars['run_model'].style.display = 'flex';
  };
};

/* function to add plot_controls: */
function add_controls() {

  /* number of days: */
  var day_div = plot_vars['day_range'];
  var day_value = plot_vars['day_value'];
  /* set initial value label: */
  day_value.innerHTML = 'Length: <label>' + box_model.length +
                        ' days</label>';
  /* if no slider: */
  if (day_div.noUiSlider == undefined) {
    /* create slider: */
    noUiSlider.create(day_div, {
      'start': box_model.length,
      'range': { 'min': 1, 'max': 31 },
      'step': 1,
      'tooltips': false
    });
    /* on slide: */
    day_div.noUiSlider.on('slide', function() {
      var slider_value = day_div.noUiSlider.get();
      day_value.innerHTML = 'Length: <label>' + parseInt(slider_value) +
                            ' days</label>';
    });
    /* on change: */
    day_div.noUiSlider.on('change', function() {
      var slider_value = day_div.noUiSlider.get();
      box_model.length = parseInt(slider_value);
      show_save(false);
    });
  };

  /* month of year: */
  var month_div = plot_vars['month_range'];
  var month_value = plot_vars['month_value'];
  var month_labels = [
    'January', 'February', 'March', 'April', 'May', 'June', 'July', 'August',
    'September', 'October', 'November', 'December'
  ];
  /* set initial value label: */
  month_value.innerHTML = 'Month: <label>' + month_labels[box_model.month] +
                          '</label>';
  /* if no slider: */
  if (month_div.noUiSlider == undefined) {
    /* pips filter function: */
    function month_pips(value, type) {
      if (value == Math.floor(value)) {
        return 0;
      } else {
        return -1;
      };
    };
    /* create slider: */
    noUiSlider.create(month_div, {
      'start': box_model.month,
      'range': { 'min': 0, 'max': 11 },
      'step': 1,
      'tooltips': false,
      'pips': { 'mode': 'steps', 'filter': month_pips }
    });
    /* on slide: */
    month_div.noUiSlider.on('slide', function() {
      var slider_value = month_div.noUiSlider.get();
      month_value.innerHTML = 'Month: <label>' +
                              month_labels[parseInt(slider_value)] +
                              '</label>';
    });
    /* on change: */
    month_div.noUiSlider.on('change', function() {
      var slider_value = month_div.noUiSlider.get();
      box_model.month = parseInt(slider_value);
      show_save(false);
    });
  };

  /* latitude: */
  var lat_div = plot_vars['lat_range'];
  var lat_value = plot_vars['lat_value'];
  /* set initial value label: */
  lat_value.innerHTML = 'Latitude: <label>' + box_model.lat.toFixed(1) +
                        '</label>';
  /* if no slider: */
  if (lat_div.noUiSlider == undefined) {
    /* create slider: */
    noUiSlider.create(lat_div, {
      'start': box_model.lat.toFixed(1),
      'range': { 'min': -90, 'max': 90 },
      'step': 0.5,
      'tooltips': false
    });
    /* on slide: */
    lat_div.noUiSlider.on('slide', function() {
      var slider_value = lat_div.noUiSlider.get();
      lat_value.innerHTML = 'Latitude: <label>' +
                            parseFloat(slider_value).toFixed(1) +
                            '</label>';
    });
    /* on change: */
    lat_div.noUiSlider.on('change', function() {
      var slider_value = lat_div.noUiSlider.get();
      box_model.lat = parseFloat(slider_value);
      show_save(false);
    });
  };

  /* surface type: */
  var surface_div = plot_vars['surface_range'];
  var surface_value = plot_vars['surface_value'];
  var surface_labels = [
    'vegetation', 'urban', 'water'
  ];
  /* set initial value label: */
  surface_value.innerHTML = 'Surface: <label>' + surface_labels[box_model.surface] +
                          '</label>';
  /* if no slider: */
  if (surface_div.noUiSlider == undefined) {
    /* pips filter function: */
    function surface_pips(value, type) {
      if (value == Math.floor(value)) {
        return 0;
      } else {
        return -1;
      };
    };
    /* create slider: */
    noUiSlider.create(surface_div, {
      'start': box_model.surface,
      'range': { 'min': 0, 'max': 2 },
      'step': 1,
      'tooltips': false,
      'pips': { 'mode': 'steps', 'filter': surface_pips }
    });
    /* on slide: */
    surface_div.noUiSlider.on('slide', function() {
      var slider_value = surface_div.noUiSlider.get();
      surface_value.innerHTML = 'Surface: <label>' +
                              surface_labels[parseInt(slider_value)] +
                              '</label>';
    });
    /* on change: */
    surface_div.noUiSlider.on('change', function() {
      var slider_value = surface_div.noUiSlider.get();
      box_model.surface = parseInt(slider_value);
      show_save(false);
    });
  };

  /* temp type: */
  var temp_div = plot_vars['temp_range'];
  var temp_value = plot_vars['temp_value'];
  /* set initial value label: */
  temp_value.innerHTML = 'Temperature: <label>' + box_model.min_temp +
                         'K - ' + box_model.max_temp + 'K</label>';
  /* if no slider: */
  if (temp_div.noUiSlider == undefined) {
    /* create slider: */
    noUiSlider.create(temp_div, {
      'start': [box_model.min_temp, box_model.max_temp],
      'range': { 'min': 160, 'max': 360 },
      'connect': true,
      'step': 1,
      'margin': 10,
      'tooltips': false
    });
    /* on slide: */
    temp_div.noUiSlider.on('slide', function() {
      var slider_value = temp_div.noUiSlider.get();
      temp_value.innerHTML = 'Temperature: <label>' +
                             parseInt(slider_value[0]) +
                             'K - ' + parseInt(slider_value[1]) +
                             'K</label>';
    });
    /* on change: */
    temp_div.noUiSlider.on('change', function() {
      var slider_value = temp_div.noUiSlider.get();
      box_model.min_temp = parseInt(slider_value[0]);
      box_model.max_temp = parseInt(slider_value[1]);
      show_save(false);
    });
  };

  /* pressure: */
  var press_div = plot_vars['press_range'];
  var press_value = plot_vars['press_value'];
  /* set initial value label: */
  press_value.innerHTML = 'Pressure: <label>' + box_model.pressure +
                          'mb</label>';
  /* if no slider: */
  if (press_div.noUiSlider == undefined) {
    /* create slider: */
    noUiSlider.create(press_div, {
      'start': parseInt(box_model.pressure),
      'range': { 'min': 10, 'max': 1000 },
      'step': 1,
      'tooltips': false
    });
    /* on slide: */
    press_div.noUiSlider.on('slide', function() {
      var slider_value = press_div.noUiSlider.get();
      press_value.innerHTML = 'Pressure: <label>' + parseInt(slider_value) +
                              'mb</label>';
    });
    /* on change: */
    press_div.noUiSlider.on('change', function() {
      var slider_value = press_div.noUiSlider.get();
      box_model.pressure = parseInt(slider_value);
      show_save(false);
    });
  };

  /* initial no: */
  var no_ini_div = plot_vars['no_ini_range'];
  var no_ini_value = plot_vars['no_ini_value'];
  /* set initial value label: */
  no_ini_value.innerHTML = 'Initial NO: <label>' + box_model.no_ini +
                           ' ppb</label>';
  /* if no slider: */
  if (no_ini_div.noUiSlider == undefined) {
    /* create slider: */
    noUiSlider.create(no_ini_div, {
      'start': box_model.no_ini,
      'range': { 'min': 0, 'max': 100 },
      'step': 1,
      'tooltips': false
    });
    /* on slide: */
    no_ini_div.noUiSlider.on('slide', function() {
      var slider_value = no_ini_div.noUiSlider.get();
      no_ini_value.innerHTML = 'Initial NO: <label>' +
                               parseInt(slider_value) +
                               ' ppb</label>';
    });
    /* on change: */
    no_ini_div.noUiSlider.on('change', function() {
      var slider_value = no_ini_div.noUiSlider.get();
      box_model.no_ini = parseInt(slider_value);
      show_save(false);
    });
  };

  /* no emission rate: */
  var no_div = plot_vars['no_range'];
  var no_value = plot_vars['no_value'];
  /* set initial value label: */
  no_value.innerHTML = 'NO emission rate: <label>' +
                       (box_model.no / 1e5).toFixed(2) +
                       'E+05 mol/cm³/s</label>';
  /* if no slider: */
  if (no_div.noUiSlider == undefined) {
    /* create slider: */
    noUiSlider.create(no_div, {
      'start': box_model.no,
      'range': { 'min': 0, 'max': 3e7 },
      'step': 3e5,
      'tooltips': false
    });
    /* on slide: */
    no_div.noUiSlider.on('slide', function() {
      var slider_value = no_div.noUiSlider.get();
      no_value.innerHTML = 'NO emission rate: <label>' +
                           (parseFloat(slider_value) / 1e5).toFixed(2) +
                           'E+05 mol/cm³/s</label>';
    });
    /* on change: */
    no_div.noUiSlider.on('change', function() {
      var slider_value = no_div.noUiSlider.get();
      box_model.no = parseFloat(slider_value);
      show_save(false);
    });
  };

  /* initial voc: */
  var voc_ini_div = plot_vars['voc_ini_range'];
  var voc_ini_value = plot_vars['voc_ini_value'];
  /* set initial value label: */
  voc_ini_value.innerHTML = 'Initial VOC: <label>' + box_model.voc_ini +
                            ' ppb</label>';
  /* if no slider: */
  if (voc_ini_div.noUiSlider == undefined) {
    /* create slider: */
    noUiSlider.create(voc_ini_div, {
      'start': box_model.voc_ini,
      'range': { 'min': 0, 'max': 100 },
      'step': 1,
      'tooltips': false
    });
    /* on slide: */
    voc_ini_div.noUiSlider.on('slide', function() {
      var slider_value = voc_ini_div.noUiSlider.get();
      voc_ini_value.innerHTML = 'Initial VOC: <label>' +
                                parseInt(slider_value) +
                                ' ppb</label>';
    });
    /* on change: */
    voc_ini_div.noUiSlider.on('change', function() {
      var slider_value = voc_ini_div.noUiSlider.get();
      box_model.voc_ini = parseInt(slider_value);
      show_save(false);
    });
  };

  /* voc emission rate: */
  var voc_div = plot_vars['voc_range'];
  var voc_value = plot_vars['voc_value'];
  /* set initial value label: */
  voc_value.innerHTML = 'VOC emission rate: <label>' +
                        (box_model.voc / 1e6).toFixed(2) +
                        'E+06 mol/cm³/s</label>';
  /* if voc slider: */
  if (voc_div.noUiSlider == undefined) {
    /* create slider: */
    noUiSlider.create(voc_div, {
      'start': box_model.voc,
      'range': { 'min': 0, 'max': 3e6 },
      'step': 3e4,
      'tooltips': false
    });
    /* on slide: */
    voc_div.noUiSlider.on('slide', function() {
      var slider_value = voc_div.noUiSlider.get();
      voc_value.innerHTML = 'VOC emission rate: <label>' +
                            (parseFloat(slider_value) / 1e6).toFixed(2) +
                            'E+06 mol/cm³/s</label>';
    });
    /* on change: */
    voc_div.noUiSlider.on('change', function() {
      var slider_value = voc_div.noUiSlider.get();
      box_model.voc = parseFloat(slider_value);
      show_save(false);
    });
  };

  /* initial co: */
  var co_ini_div = plot_vars['co_ini_range'];
  var co_ini_value = plot_vars['co_ini_value'];
  /* set initial value label: */
  co_ini_value.innerHTML = 'Initial CO: <label>' + box_model.co_ini +
                           ' ppb</label>';
  /* if no slider: */
  if (co_ini_div.noUiSlider == undefined) {
    /* create slider: */
    noUiSlider.create(co_ini_div, {
      'start': box_model.co_ini,
      'range': { 'min': 0, 'max': 100 },
      'step': 1,
      'tooltips': false
    });
    /* on slide: */
    co_ini_div.noUiSlider.on('slide', function() {
      var slider_value = co_ini_div.noUiSlider.get();
      co_ini_value.innerHTML = 'Initial CO: <label>' +
                               parseInt(slider_value) +
                               ' ppb</label>';
    });
    /* on change: */
    co_ini_div.noUiSlider.on('change', function() {
      var slider_value = co_ini_div.noUiSlider.get();
      box_model.co_ini = parseInt(slider_value);
      show_save(false);
    });
  };

  /* co emission rate: */
  var co_div = plot_vars['co_range'];
  var co_value = plot_vars['co_value'];
  /* set initial value label: */
  co_value.innerHTML = 'CO emission rate: <label>' +
                       (box_model.co / 1e6).toFixed(2) +
                       'E+06 mol/cm³/s</label>';
  /* if co slider: */
  if (co_div.noUiSlider == undefined) {
    /* create slider: */
    noUiSlider.create(co_div, {
      'start': box_model.co,
      'range': { 'min': 0, 'max': 6e7 },
      'step': 6e5,
      'tooltips': false
    });
    /* on slide: */
    co_div.noUiSlider.on('slide', function() {
      var slider_value = co_div.noUiSlider.get();
      co_value.innerHTML = 'CO emission rate: <label>' +
                           (parseFloat(slider_value) / 1e6).toFixed(2) +
                           'E+06 mol/cm³/s</label>';
    });
    /* on change: */
    co_div.noUiSlider.on('change', function() {
      var slider_value = co_div.noUiSlider.get();
      box_model.co = parseFloat(slider_value);
      show_save(false);
    });
  };

  /* traffic signal: */
  var cycle_div = plot_vars['cycle_range'];
  var cycle_value = plot_vars['cycle_value'];
  /* set initial value label: */
  cycle_value.innerHTML = 'Traffic signal: <label>' +
                          box_model.cycle.toFixed(2) +
                          '</label>';
  /* if cycle slider: */
  if (cycle_div.noUiSlider == undefined) {
    /* create slider: */
    noUiSlider.create(cycle_div, {
      'start': box_model.cycle,
      'range': { 'min': 0, 'max': 1 },
      'step': 0.01,
      'tooltips': false
    });
    /* on slide: */
    cycle_div.noUiSlider.on('slide', function() {
      var slider_value = cycle_div.noUiSlider.get();
      cycle_value.innerHTML = 'Traffic signal: <label>' +
                              parseFloat(slider_value).toFixed(2) +
                              '</label>';
    });
    /* on change: */
    cycle_div.noUiSlider.on('change', function() {
      var slider_value = cycle_div.noUiSlider.get();
      box_model.cycle = parseFloat(slider_value);
      show_save(false);
    });
  };

  /* water vapour: */
  var h2o_div = plot_vars['h2o_range'];
  var h2o_value = plot_vars['h2o_value'];
  /* set initial value label: */
  h2o_value.innerHTML = 'Water vapour: <label>' +
                        box_model.h2o.toFixed(1) +
                        ' %</label>';
  /* if h2o slider: */
  if (h2o_div.noUiSlider == undefined) {
    /* create slider: */
    noUiSlider.create(h2o_div, {
      'start': box_model.h2o,
      'range': { 'min': 0, 'max': 100 },
      'step': 0.1,
      'tooltips': false
    });
    /* on slide: */
    h2o_div.noUiSlider.on('slide', function() {
      var slider_value = h2o_div.noUiSlider.get();
      h2o_value.innerHTML = 'Water vapour: <label>' +
                            parseFloat(slider_value).toFixed(1) +
                            ' %</label>';
    });
    /* on change: */
    h2o_div.noUiSlider.on('change', function() {
      var slider_value = h2o_div.noUiSlider.get();
      box_model.h2o = parseFloat(slider_value);
      show_save(false);
    });
  };

  /* hide the plots: */
  show_save(false);

};

/* function to plot results: */
function plot_species() {
  /* show the plots: */
  show_save(true);
  /* get time step values: */
  var x_values = box_model.tsteps;
  /* scatter plot config: */
  var scatter_conf = {
    'showLink': false,
    'linkText': '',
    'displaylogo': false,
    'modeBarButtonsToRemove': [
      'autoScale2d',
      'lasso2d',
      'toggleSpikelines',
      'select2d'
    ],
    'responsive': true
  };
  /* loop through species: */
  for (var species in box_model.species) {
    /* model data for this species: */
    var species_data = box_model.species[species];
    /* y values: */
    var y_values = species_data['values'];
    /* y units: */
    var y_units = species_data['units'];
    /* hover data: */
    var hover_text = [];
    for (var i = 0; i < x_values.length; i++) {
      hover_text[i] = 'Time: ' + x_values[i].toFixed(2) + ' days<br>' +
                      species + ': ' + y_values[i].toFixed(2) +
                      ' ' + y_units;
    };
    /* plot data for this species: */
    if (plot_vars['species'][species] == undefined) {
      plot_vars['species'][species] = {};
    };
    var species_plot = plot_vars['species'][species];
    /* div for this species plot: */
    if (species_plot['plot_div'] == undefined) {
      species_plot['plot_div'] = document.createElement('div');
      species_plot['plot_div'].classList.add('species_plot');
      plot_vars['plots_div'].appendChild(species_plot['plot_div']);
    }
    var plot_div = species_plot['plot_div'];
    /* scatter plot: */
    var scatter_plot = {
      'name': species,
      'type': 'scatter',
      'mode': 'lines',
      'x': x_values,
      'y': y_values,
      'hoverinfo': 'text',
      'text': hover_text
    };
    var scatter_data = [scatter_plot];
    /* plot update, if updating: */
    var scatter_update = {
      'x': [x_values],
      'y': species_data['values']
    };
    /* scatter plot layout: */
    var scatter_layout = {
      'title': {
        'text': species,
        'y': 0.88
      },
      'xaxis': {
        'title': plot_vars['x_title'],
        'zeroline': false
      },
      'yaxis': {
        'title': y_units,
        'zeroline': false
      },
      'margin': {
        'b': 40,
        't': 90,
        'l': 50,
        'r': 10
      },
      'hovermode': 'closest'
    };
    /* plot for this species: */
    if (species_plot['plot'] == undefined) {
      species_plot['plot'] = Plotly.newPlot(plot_div, scatter_data,
                             scatter_layout, scatter_conf);
    } else {
      Plotly.update(plot_div, scatter_update, scatter_layout)
    };
  /* end loop through species: */
  };
};

/* function to run model: */
function run_model() {
  /* run the model: */
  box_model.run_model().then(
    /* then plot: */
    function(result) {
      plot_species();
    }
  );
};

/* function to save data as csv: */
function save_csv() {
  /* start csv content: */
  var csv_data = 'data:text/csv;charset=utf-8,';
  /* model data ... month: */
  var month_labels = [
    'January', 'February', 'March', 'April', 'May', 'June', 'July', 'August',
    'September', 'October', 'November', 'December'
  ];
  csv_data += 'Month,' + month_labels[box_model.month] + '\r\n';
  /* number of days: */
  csv_data += 'Length (days),' + box_model.length + '\r\n';
  /* latitude: */
  csv_data += 'Latitude,' + box_model.lat.toFixed(1) + '\r\n';
  /* temp: */
  csv_data += 'Min Temp (K),' + box_model.min_temp.toFixed(1) + '\r\n';
  csv_data += 'Max Temp (K),' + box_model.max_temp.toFixed(1) + '\r\n';
  /* ch4 concentration ... this is a constant: */
  csv_data += 'CH4 conc (ppb),1770.0\r\n';
  /* h2o: */
  csv_data += 'H2O (%),' + box_model.h2o.toFixed(1) + '\r\n';
  /* no: */
  csv_data += 'NO (mol/cc/s),' + box_model.no + '\r\n';
  csv_data += 'Init NO (ppb),' + box_model.no_ini + '\r\n';
  /* voc: */
  csv_data += 'VOC (mol/cc/s),' + box_model.voc + '\r\n';
  csv_data += 'Init VOC (ppb),' + box_model.voc_ini + '\r\n';
  /* co: */
  csv_data += 'CO (mol/cc/s),' + box_model.co + '\r\n';
  csv_data += 'Init CO (ppb),' + box_model.co_ini + '\r\n';
  /* cycle: */
  csv_data += 'Bi-daily cycle,' + box_model.cycle + '\r\n';
  /* surface type: */
  var surface_labels = [
    'Vegetation', 'Urban', 'Water'
  ];
  csv_data += 'Surface Type,' + surface_labels[box_model.surface] + '\r\n';
  /* spacer line: */
  csv_data += '\r\n';
  /* header line: */
  var header_row = 'Time (days),';
  for (var species in box_model.species) {
    header_row += species + ' (' + box_model.species[species]['units'] + '),'
  };
  csv_data += header_row.slice(0, -1) + '\r\n';
  /* loop through time steps: */
  for (var i = 0; i < box_model.tsteps.length; i++) {
    var row_data = box_model.tsteps[i] + ',';
    /* loop through species: */
    for (var species in box_model.species) {
      row_data += box_model.species[species]['values'][i] + ','; 
    };
    csv_data += row_data.slice(0, -1) + '\r\n';
  };
  /* encode csv data: */
  var encoded_uri = encodeURI(csv_data);
  /* name for csv file: */
  var csv_name = 'box_model.csv';
  /* create a temporary link element: */
  var csv_link = document.createElement("a");
  csv_link.setAttribute("href", encoded_uri);
  csv_link.setAttribute("download", csv_name);
  csv_link.style.visibility = 'hidden';
  /* add link to document, click to init download, then remove: */
  document.body.appendChild(csv_link);
  csv_link.click();
  document.body.removeChild(csv_link);
};

/* on page load: */
window.addEventListener('load', function() {
  /* init the box model: */
  init_box_model('./wasm/box_model.wasm').then(
    function(result) {
      /* if load was successful: */
      if (result != undefined) {
        /* add controls: */
        add_controls();
      };
    }
  );
});
