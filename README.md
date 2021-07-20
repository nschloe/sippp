## Introduction

LIBPARABOL is a software library that allows for quick solving of equation
systems that arise when discretizing parabolic differential equations of the
kind
```math
u_t - L_{\eps} u  = f\\
u(0)        = u_0\\
u_{\Gamma}  = gamma
```
with $`L_{\eps} = \sqrt(\eps) \Delta u - c(x) u`$ in one space dimension.
The library contains routines which handle the generation
of meshes (in particular layer adapted meshes) when treating singularly perturbed
problems as well as a wide range of A-stable Runge-Kutta methods. The
equation systems aring from the application of fully implicit Runge-Kutta
methods can be solved by a preconditioned GMRES method. Moreover, there
exist a number of user interfaces which neatly plot the approximated solution
in various formats or generate LaTeX style "convergence tables".


## Installation
In each of the subdirectories you will find Makefiles that will help you with
the compilation of the library. All you need to do is specify your favourite
Fortran95 compiler in "make.inc" which is to be found at the top level directory
Then type make, lean back, and watch the engine at work..


## Usage
The user needs to provide the parameters of the differential equation as well 
as everything that relates to the operator L_eps. Which routines are exactly
required can be seen from INTERMOD.f; the interfaces for the Fortran routines
are contained in this module.
The folder MAIN contains examples of provided operators and PDE parameters.

To see the library at work, go to the MAIN folder and link one of the files
in the folder "operators" to the file "operator.f" in the MAIN-directory. Do
the same for one of the parameter files. Then edit "main.f" to your liking
(a few usage examples are contained), type make, and then execute main.out.

The whole range of the routines which may be of interest to to are contained
in PUT.f.

Have fun!


This software was developed by Nico Schlömer as part of his diploma thesis in 2007.
