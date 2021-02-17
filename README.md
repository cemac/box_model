<div align="center">
<a href="https://www.cemac.leeds.ac.uk/">
  <img src="https://github.com/cemac/cemac_generic/blob/master/Images/cemac.png"></a>
  <br>
</div>

## Box Model

![GitHub License](https://img.shields.io/github/license/cemac/box_model.svg)
[![GitHub Top Language](https://img.shields.io/github/languages/top/cemac/box_model.svg)](https://github.com/cemac/box_model)
[![GitHub Last Commit](https://img.shields.io/github/last-commit/cemac/box_model.svg)](https://github.com/cemac/box_model/commits/master)

Box model adapted from IDL code:

[https://cemac.github.io/box_model/](https://cemac.github.io/box_model/)

The model code was translated from existing IDL code to C and is compiled to web assembly.

The `solzenith` function in the code was translated from an IDL function of the same name, which was created by:

> first implementation Jan, 17 1997 by Dominik Brunner
> adapted from the JNO2 program by Wiel Wauben, KNMI

The model uses the LSODE solver from [ODEPACK](https://computing.llnl.gov/casc/odepack/) to replicate the IDL [LSODE](https://www.l3harrisgeospatial.com/docs/lsode.html) function.

See the [src](src/) directory for more information on comiling the model code.
