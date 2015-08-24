/*
 *
 * Copyright (C) 1997-2001 Stephan Seidl
 * This file is part of the FMC Project of the author.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; see the file COPYING. If not, write to the
 * Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 * Stephan Seidl                        E-mail     : seidl@zhr.tu-dresden.de
 * Technische Universitaet Dresden      Letters to : TU Dresden, ZHR,
 * Zentrum fuer Hochleistungsrechnen                 01062 Dresden, Germany
 * Willersbau A113, Zellescher Weg 12
 * Dresden
 *
 *
 * @(#)fmc_h_machdefs.h 1.5 - 07/23/00
 *
 */


#ifndef FMC_INCLUDE_HAVE_H_MACHDEFS_H
#define FMC_INCLUDE_HAVE_H_MACHDEFS_H


/* Try to recognize the machine environment here.
   Don't change this file without updating
   `i_fpesetup.c' and `t_prhoststat.c'. */


/* Make single predefines uniform.
   Derive some specials. */
#if (defined (BSD) || defined (__BSD))
#ifndef __bsd__
#define __bsd__
#endif
#endif
#if (defined (BSD__) || defined (__BSD__))
#ifndef __bsd__
#define __bsd__
#endif
#endif

#if (defined (CNX_MACH_HP700) || defined (__CNX_MACH_HP700))
#ifndef __convex__
#define __convex__
#endif
#endif
#if (defined (CNX_MACH_HP700__) || defined (__CNX_MACH_HP700__))
#ifndef __convex__
#define __convex__
#endif
#endif
#if (defined (CNX_MACH_SPP1) || defined (__CNX_MACH_SPP1))
#ifndef __convex__
#define __convex__
#endif
#endif
#if (defined (CNX_MACH_SPP1__) || defined (__CNX_MACH_SPP1__))
#ifndef __convex__
#define __convex__
#endif
#endif
#if (defined (CNX_MACH_SPP1000) || defined (__CNX_MACH_SPP1000))
#ifndef __convex__
#define __convex__
#endif
#endif
#if (defined (CNX_MACH_SPP1000__) || defined (__CNX_MACH_SPP1000__))
#ifndef __convex__
#define __convex__
#endif
#endif
#if (defined (CNX_MACH_SPP1200) || defined (__CNX_MACH_SPP1200))
#ifndef __convex__
#define __convex__
#endif
#endif
#if (defined (CNX_MACH_SPP1200__) || defined (__CNX_MACH_SPP1200__))
#ifndef __convex__
#define __convex__
#endif
#endif
#if (defined (CNX_MACH_SPP1600) || defined (__CNX_MACH_SPP1600))
#ifndef __convex__
#define __convex__
#endif
#endif
#if (defined (CNX_MACH_SPP1600__) || defined (__CNX_MACH_SPP1600__))
#ifndef __convex__
#define __convex__
#endif
#endif
#if (defined (CNX_MACH_SPP2) || defined (__CNX_MACH_SPP2))
#ifndef __convex__
#define __convex__
#endif
#endif
#if (defined (CNX_MACH_SPP2__) || defined (__CNX_MACH_SPP2__))
#ifndef __convex__
#define __convex__
#endif
#endif
#if (defined (CNX_MACH_SPP2000) || defined (__CNX_MACH_SPP2000))
#ifndef __convex__
#define __convex__
#endif
#endif
#if (defined (CNX_MACH_SPP2000__) || defined (__CNX_MACH_SPP2000__))
#ifndef __convex__
#define __convex__
#endif
#endif
#if (defined (CONVEX_EXT) || defined (__CONVEX_EXT))
#ifndef __convex__
#define __convex__
#endif
#endif
#if (defined (CONVEX_EXT__) || defined (__CONVEX_EXT__))
#ifndef __convex__
#define __convex__
#endif
#endif
#if (defined (CRAY) || defined (__CRAY))
#ifndef __cray__
#define __cray__
#endif
#endif
#if (defined (CRAY__) || defined (__CRAY__))
#ifndef __cray__
#define __cray__
#endif
#endif
#if (defined (CRAY1) || defined (__CRAY1))
#ifndef __cray1__
#define __cray1__
#endif
#ifndef __cray__
#define __cray__
#endif
#endif
#if (defined (CRAY1__) || defined (__CRAY1__))
#ifndef __cray1__
#define __cray1__
#endif
#ifndef __cray__
#define __cray__
#endif
#endif
#if (defined (CRAY2) || defined (__CRAY2))
#ifndef __cray2__
#define __cray2__
#endif
#ifndef __cray__
#define __cray__
#endif
#endif
#if (defined (CRAY2__) || defined (__CRAY2__))
#ifndef __cray2__
#define __cray2__
#endif
#ifndef __cray__
#define __cray__
#endif
#endif
#if (defined (CRAYMPP) || defined (__CRAYMPP))
#ifndef __craympp__
#define __craympp__
#endif
#ifndef __cray__
#define __cray__
#endif
#endif
#if (defined (CRAYMPP__) || defined (__CRAYMPP__))
#ifndef __craympp__
#define __craympp__
#endif
#ifndef __cray__
#define __cray__
#endif
#endif
#if (defined (CRAYT3D) || defined (__CRAYT3D))
#ifndef __crayt3d__
#define __crayt3d__
#endif
#ifndef __craympp__
#define __craympp__
#endif
#ifndef __cray__
#define __cray__
#endif
#endif
#if (defined (CRAYT3D__) || defined (__CRAYT3D__))
#ifndef __crayt3d__
#define __crayt3d__
#endif
#ifndef __craympp__
#define __craympp__
#endif
#ifndef __cray__
#define __cray__
#endif
#endif
#if (defined (CRAYT3E) || defined (__CRAYT3E))
#ifndef __crayt3e__
#define __crayt3e__
#endif
#ifndef __craympp__
#define __craympp__
#endif
#ifndef __cray__
#define __cray__
#endif
#endif
#if (defined (CRAYT3E__) || defined (__CRAYT3E__))
#ifndef __crayt3e__
#define __crayt3e__
#endif
#ifndef __craympp__
#define __craympp__
#endif
#ifndef __cray__
#define __cray__
#endif
#endif
#if (defined (ConvexSPP) || defined (__ConvexSPP))
#ifndef __convex__
#define __convex__
#endif
#endif
#if (defined (ConvexSPP__) || defined (__ConvexSPP__))
#ifndef __convex__
#define __convex__
#endif
#endif
#if (defined (GNUC) || defined (__GNUC))
#ifndef __gnuc__
#define __gnuc__
#endif
#endif
#if (defined (GNUC__) || defined (__GNUC__))
#ifndef __gnuc__
#define __gnuc__
#endif
#endif
#if (defined (HP_CXD_SPP) || defined (__HP_CXD_SPP))
#ifndef __convex__
#define __convex__
#endif
#endif
#if (defined (HP_CXD_SPP__) || defined (__HP_CXD_SPP__))
#ifndef __convex__
#define __convex__
#endif
#endif
#if (defined (SR8000) || defined (__SR8000))
#ifndef __sr8000__
#define __sr8000__
#endif
#endif
#if (defined (SR8000__) || defined (__SR8000__))
#ifndef __sr8000__
#define __sr8000__
#endif
#endif
#if (defined (SX) || defined (__SX))
#ifndef __sx__
#define __sx__
#endif
#endif
#if (defined (SX__) || defined (__SX__))
#ifndef __sx__
#define __sx__
#endif
#endif
#if (defined (SYSTYPE_SVR4) || defined (__SYSTYPE_SVR4))
#ifndef __svr4__
#define __svr4__
#endif
#endif
#if (defined (SYSTYPE_SVR4__) || defined (__SYSTYPE_SVR4__))
#ifndef __svr4__
#define __svr4__
#endif
#endif
#if (defined (SunOS4) || defined (__SunOS4))
#ifndef __bsd__
#define __bsd__
#endif
#endif
#if (defined (SunOS4__) || defined (__SunOS4__))
#ifndef __bsd__
#define __bsd__
#endif
#endif
#if (defined (WIN2K) || defined (__WIN2K))
#ifndef __win2k__
#define __win2k__
#endif
#ifndef __windows__
#define __windows__
#endif
#endif
#if (defined (WIN2K__) || defined (__WIN2K__))
#ifndef __win2k__
#define __win2k__
#endif
#ifndef __windows__
#define __windows__
#endif
#endif
#if (defined (WIN32) || defined (__WIN32))
#ifndef __windows__
#define __windows__
#endif
#endif
#if (defined (WIN32__) || defined (__WIN32__))
#ifndef __windows__
#define __windows__
#endif
#endif
#if (defined (WIN95) || defined (__WIN95))
#ifndef __win95__
#define __win95__
#endif
#ifndef __windows__
#define __windows__
#endif
#endif
#if (defined (WIN95__) || defined (__WIN95__))
#ifndef __win95__
#define __win95__
#endif
#ifndef __windows__
#define __windows__
#endif
#endif
#if (defined (WIN98) || defined (__WIN98))
#ifndef __win98__
#define __win98__
#endif
#ifndef __windows__
#define __windows__
#endif
#endif
#if (defined (WIN98__) || defined (__WIN98__))
#ifndef __win98__
#define __win98__
#endif
#ifndef __windows__
#define __windows__
#endif
#endif
#if (defined (WINNT) || defined (__WINNT))
#ifndef __winnt__
#define __winnt__
#endif
#ifndef __windows__
#define __windows__
#endif
#endif
#if (defined (WINNT__) || defined (__WINNT__))
#ifndef __winnt__
#define __winnt__
#endif
#ifndef __windows__
#define __windows__
#endif
#endif

#if (defined (_AIX) || defined (___AIX))
#ifndef __aix__
#define __aix__
#endif
#endif
#if (defined (_AIX__) || defined (___AIX__))
#ifndef __aix__
#define __aix__
#endif
#endif
#if (defined (_AIX32) || defined (___AIX32))
#ifndef __aix__
#define __aix__
#endif
#endif
#if (defined (_AIX32__) || defined (___AIX32__))
#ifndef __aix__
#define __aix__
#endif
#endif
#if (defined (_AIX41) || defined (___AIX41))
#ifndef __aix__
#define __aix__
#endif
#endif
#if (defined (_AIX41__) || defined (___AIX41__))
#ifndef __aix__
#define __aix__
#endif
#endif
#if (defined (_ALPHA_) || defined (___ALPHA_))
#ifndef __alpha__
#define __alpha__
#endif
#endif
#if (defined (_ALPHA___) || defined (___ALPHA___))
#ifndef __alpha__
#define __alpha__
#endif
#endif
#if (defined (_BSD) || defined (___BSD))
#ifndef __bsd__
#define __bsd__
#endif
#endif
#if (defined (_BSD__) || defined (___BSD__))
#ifndef __bsd__
#define __bsd__
#endif
#endif
#if (defined (_CRAY) || defined (___CRAY))
#ifndef __cray__
#define __cray__
#endif
#endif
#if (defined (_CRAY__) || defined (___CRAY__))
#ifndef __cray__
#define __cray__
#endif
#endif
#if (defined (_CRAY1) || defined (___CRAY1))
#ifndef __cray1__
#define __cray1__
#endif
#ifndef __cray__
#define __cray__
#endif
#endif
#if (defined (_CRAY1__) || defined (___CRAY1__))
#ifndef __cray1__
#define __cray1__
#endif
#ifndef __cray__
#define __cray__
#endif
#endif
#if (defined (_CRAY2) || defined (___CRAY2))
#ifndef __cray2__
#define __cray2__
#endif
#ifndef __cray__
#define __cray__
#endif
#endif
#if (defined (_CRAY2__) || defined (___CRAY2__))
#ifndef __cray2__
#define __cray2__
#endif
#ifndef __cray__
#define __cray__
#endif
#endif
#if (defined (_CRAYMPP) || defined (___CRAYMPP))
#ifndef __craympp__
#define __craympp__
#endif
#ifndef __cray__
#define __cray__
#endif
#endif
#if (defined (_CRAYMPP__) || defined (___CRAYMPP__))
#ifndef __craympp__
#define __craympp__
#endif
#ifndef __cray__
#define __cray__

#endif
#endif
#if (defined (_CRAYT3D) || defined (___CRAYT3D))
#ifndef __crayt3d__
#define __crayt3d__
#endif
#ifndef __craympp__
#define __craympp__
#endif
#ifndef __cray__
#define __cray__
#endif
#endif
#if (defined (_CRAYT3D__) || defined (___CRAYT3D__))
#ifndef __crayt3d__
#define __crayt3d__
#endif
#ifndef __craympp__
#define __craympp__
#endif
#ifndef __cray__
#define __cray__
#endif
#endif
#if (defined (_CRAYT3E) || defined (___CRAYT3E))
#ifndef __crayt3e__
#define __crayt3e__
#endif
#ifndef __craympp__
#define __craympp__
#endif
#ifndef __cray__
#define __cray__
#endif
#endif
#if (defined (_CRAYT3E__) || defined (___CRAYT3E__))
#ifndef __crayt3e__
#define __crayt3e__
#endif
#ifndef __craympp__
#define __craympp__
#endif
#ifndef __cray__
#define __cray__
#endif
#endif
#if (defined (_IBMR2) || defined (___IBMR2))
#ifndef __ibm__
#define __ibm__
#endif
#endif
#if (defined (_IBMR2__) || defined (___IBMR2__))
#ifndef __ibm__
#define __ibm__
#endif
#endif
#if (defined (_M_ALPHA) || defined (___M_ALPHA))
#ifndef __alpha__
#define __alpha__
#endif
#endif
#if (defined (_M_ALPHA__) || defined (___M_ALPHA__))
#ifndef __alpha__
#define __alpha__
#endif
#endif
#if (defined (_M_IX86) || defined (___M_IX86))
#ifndef __ix86__
#define __ix86__
#endif
#ifndef __i386__
#define __i386__
#endif
#endif
#if (defined (_M_IX86__) || defined (___M_IX86__))
#ifndef __ix86__
#define __ix86__
#endif
#ifndef __i386__
#define __i386__
#endif
#endif
#if (defined (_MSC_VER) || defined (___MSC_VER))
#ifndef __microsoft__
#define __microsoft__
#endif
#endif
#if (defined (_MSC_VER__) || defined (___MSC_VER__))
#ifndef __microsoft__
#define __microsoft__
#endif
#endif
#if (defined (_POWER) || defined (___POWER))
#ifndef __power__
#define __power__
#endif
#endif
#if (defined (_POWER__) || defined (___POWER__))
#ifndef __power__
#define __power__
#endif
#endif
#if (defined (_SR8000) || defined (___SR8000))
#ifndef __sr8000__
#define __sr8000__
#endif
#endif
#if (defined (_SR8000__) || defined (___SR8000__))
#ifndef __sr8000__
#define __sr8000__
#endif
#endif
#if (defined (_SX) || defined (___SX))
#ifndef __sx__
#define __sx__
#endif
#endif
#if (defined (_SX__) || defined (___SX__))
#ifndef __sx__
#define __sx__
#endif
#endif
#if (defined (_UNICOS) || defined (___UNICOS))
#ifndef __unicos__
#define __unicos__
#endif
#endif
#if (defined (_UNICOS__) || defined (___UNICOS__))
#ifndef __unicos__
#define __unicos__
#endif

#endif
#if (defined (_WIN2K) || defined (___WIN2K))
#ifndef __win2k__
#define __win2k__
#endif
#ifndef __windows__
#define __windows__
#endif
#endif
#if (defined (_WIN2K__) || defined (___WIN2K__))
#ifndef __win2k__
#define __win2k__
#endif
#ifndef __windows__
#define __windows__
#endif
#endif
#if (defined (_WIN2K_) || defined (___WIN2K_))
#ifndef __win2k__
#define __win2k__
#endif
#ifndef __windows__
#define __windows__
#endif
#endif
#if (defined (_WIN2K___) || defined (___WIN2K___))
#ifndef __win2k__
#define __win2k__
#endif
#ifndef __windows__
#define __windows__
#endif
#endif
#if (defined (_WIN32) || defined (___WIN32))
#ifndef __windows__
#define __windows__
#endif
#endif
#if (defined (_WIN32__) || defined (___WIN32__))
#ifndef __windows__
#define __windows__
#endif
#endif
#if (defined (_WIN95) || defined (___WIN95))
#ifndef __win95__
#define __win95__
#endif
#ifndef __windows__
#define __windows__
#endif
#endif
#if (defined (_WIN95__) || defined (___WIN95__))
#ifndef __win95__
#define __win95__
#endif
#ifndef __windows__
#define __windows__
#endif
#endif
#if (defined (_WIN95_) || defined (___WIN95_))
#ifndef __win95__
#define __win95__
#endif
#ifndef __windows__
#define __windows__
#endif
#endif
#if (defined (_WIN95___) || defined (___WIN95___))
#ifndef __win95__
#define __win95__
#endif
#ifndef __windows__
#define __windows__
#endif
#endif
#if (defined (_WIN98) || defined (___WIN98))
#ifndef __win98__
#define __win98__
#endif

#ifndef __windows__
#define __windows__
#endif
#endif
#if (defined (_WIN98__) || defined (___WIN98__))
#ifndef __win98__
#define __win98__
#endif
#ifndef __windows__
#define __windows__
#endif
#endif
#if (defined (_WIN98_) || defined (___WIN98_))
#ifndef __win98__
#define __win98__
#endif
#ifndef __windows__
#define __windows__
#endif
#endif
#if (defined (_WIN98___) || defined (___WIN98___))
#ifndef __win98__
#define __win98__
#endif
#ifndef __windows__
#define __windows__
#endif

#endif
#if (defined (_WINNT) || defined (___WINNT))
#ifndef __winnt__
#define __winnt__
#endif
#ifndef __windows__
#define __windows__
#endif
#endif
#if (defined (_WINNT__) || defined (___WINNT__))
#ifndef __winnt__
#define __winnt__
#endif
#ifndef __windows__
#define __windows__
#endif
#endif
#if (defined (_WINNT_) || defined (___WINNT_))
#ifndef __winnt__
#define __winnt__
#endif
#ifndef __windows__
#define __windows__
#endif
#endif
#if (defined (_WINNT___) || defined (___WINNT___))
#ifndef __winnt__
#define __winnt__
#endif
#ifndef __windows__
#define __windows__
#endif
#endif
#if (defined (_X86_) || defined (___X86_))
#ifndef __ix86__
#define __ix86__
#endif
#ifndef __i386__
#define __i386__
#endif
#endif
#if (defined (_X86___) || defined (___X86___))
#ifndef __ix86__
#define __ix86__
#endif
#ifndef __i386__
#define __i386__
#endif
#endif
#if (defined (alpha) || defined (__alpha) || defined (alpha__))
#ifndef __alpha__
#define __alpha__
#endif
#endif
#if (defined (bsd) || defined (__bsd) || defined (bsd__))
#ifndef __bsd__
#define __bsd__
#endif
#endif
#if (defined (convex) || defined (__convex) || defined (convex__))
#ifndef __convex__
#define __convex__
#endif
#endif
#if (defined (convexc) || defined (__convexc) || defined (convexc__))
#ifndef __convexc__
#define __convexc__
#endif
#endif
#if (defined (cray) || defined (__cray) || defined (cray__))
#ifndef __cray__
#define __cray__
#endif
#endif
#if (defined (cray1) || defined (__cray1) || defined (cray1__))
#ifndef __cray1__
#define __cray1__
#endif
#ifndef __cray__
#define __cray__
#endif
#endif
#if (defined (cray2) || defined (__cray2) || defined (cray2__))
#ifndef __cray2__
#define __cray2__
#endif
#ifndef __cray__
#define __cray__
#endif
#endif
#if (defined (craympp) || defined (__craympp) || defined (craympp__))
#ifndef __craympp__
#define __craympp__
#endif
#ifndef __cray__
#define __cray__
#endif
#endif
#if (defined (crayt3d) || defined (__crayt3d) || defined (crayt3d__))
#ifndef __crayt3d__
#define __crayt3d__
#endif
#ifndef __craympp__
#define __craympp__
#endif
#ifndef __cray__
#define __cray__
#endif
#endif
#if (defined (crayt3e) || defined (__crayt3e) || defined (crayt3e__))
#ifndef __crayt3e__
#define __crayt3e__
#endif
#ifndef __craympp__
#define __craympp__
#endif
#ifndef __cray__
#define __cray__
#endif
#endif
#if (defined (gnuc) || defined (__gnuc) || defined (gnuc__))
#ifndef __gnuc__
#define __gnuc__
#endif
#endif
#if (defined (hiuxmpp) || defined (__hiuxmpp) || defined (hiuxmpp__))
#ifndef __hiuxmpp__
#define __hiuxmpp__
#endif
#endif
#if (defined (host_mips) || defined (__host_mips) || defined (host_mips__))
#ifndef __host_mips__
#define __host_mips__
#endif
#endif
#if (defined (hp9000s800) || defined (__hp9000s800) || defined (hp9000s800__))
#ifndef __hp9000s800__
#define __hp9000s800__
#endif
#endif
#if (defined (hp9k8) || defined (__hp9k8) || defined (hp9k8__))
#ifndef __hp9k8__
#define __hp9k8__
#endif
#endif
#if (defined (hppa) || defined (__hppa) || defined (hppa__))
#ifndef __hppa__
#define __hppa__
#endif
#endif
#if (defined (hpux) || defined (__hpux) || defined (hpux__))
#ifndef __hpux__
#define __hpux__
#endif
#endif
#if (defined (i386) || defined (__i386) || defined (i386__))
#ifndef __i386__
#define __i386__
#endif
#endif
#if (defined (i486) || defined (__i486) || defined (i486__))
#ifndef __i486__
#define __i486__
#endif
#endif
#if (defined (i586) || defined (__i586) || defined (i586__))
#ifndef __i586__
#define __i586__
#endif
#endif
#if (defined (i686) || defined (__i686) || defined (i686__))
#ifndef __i686__
#define __i686__
#endif
#endif
#if (defined (linux) || defined (__linux) || defined (linux__))
#ifndef __linux__
#define __linux__
#endif
#endif
#if (defined (mips) || defined (__mips) || defined (mips__))
#ifndef __mips__
#define __mips__
#endif
#endif
#if (defined (mips12k) || defined (__mips12k) || defined (mips12k__))
#ifndef __mips12k__
#define __mips12k__
#endif
#ifndef __mips__
#define __mips__
#endif
#endif
#if (defined (mips10k) || defined (__mips10k) || defined (mips10k__))
#ifndef __mips10k__
#define __mips10k__
#endif
#ifndef __mips__
#define __mips__
#endif
#endif
#if (defined (mips4k) || defined (__mips4k) || defined (mips4k__))
#ifndef __mips4k__
#define __mips4k__
#endif
#ifndef __mips__
#define __mips__
#endif
#endif
#if (defined (mips5k) || defined (__mips5k) || defined (mips5k__))
#ifndef __mips5k__

#define __mips5k__
#endif
#ifndef __mips__
#define __mips__
#endif
#endif
#if (defined (mips64) || defined (__mips64) || defined (mips64__))
#ifndef __mips64__
#define __mips64__
#endif
#ifndef __mips__
#define __mips__
#endif
#endif
#if (defined (mips8k) || defined (__mips8k) || defined (mips8k__))
#ifndef __mips8k__
#define __mips8k__
#endif
#ifndef __mips__
#define __mips__
#endif
#endif
#if (defined (osf) || defined (__osf) || defined (osf__))
#ifndef __osf__
#define __osf__
#elif
#undef  WINVER
#define WINVER (0x0500)
#endif
#elif (defined (__win2k__))
/* The image may not run on `win95', `win98' or `winnt'. */
#ifndef _WIN32_WINNT
#define _WIN32_WINNT (0x0500)
#endif
#if ((_WIN32_WINNT) >= 0x0500)
/* O.k. */
#else
#undef  _WIN32_WINNT
#define _WIN32_WINNT (0x0500)
#endif
#ifndef WINVER
#define WINVER (0x0500)
#endif
#if ((WINVER) >= 0x0500)
/* O.k. */
#else
#undef  WINVER
#define WINVER (0x0500)
#endif
#elif (defined (__winnt__) && defined (__win98__))
/* The image may not run on `win95'. */
#ifndef _WIN32_WINDOWS
#define _WIN32_WINDOWS (0x0410)
#endif
#if ((_WIN32_WINDOWS) >= 0x0410)
/* O.k. */
#else
#undef  _WIN32_WINDOWS
#define _WIN32_WINDOWS (0x0410)
#endif
#ifndef WINVER
#define WINVER (0x0400)
#endif
#if ((WINVER) >= 0x0400)
/* O.k. */
#else
#undef  WINVER
#define WINVER (0x0400)
#endif
#elif (defined (__winnt__))
/* W*nd*ws NT 4.0 or higher.
   The image may not run on `win98'.
   Not implemented in W*nd*ws 95. */
#ifndef _WIN32_WINNT
#define _WIN32_WINNT (0x0400)
#endif
#if ((_WIN32_WINNT) >= 0x0400)
/* O.k. */
#else
#undef  _WIN32_WINNT
#define _WIN32_WINNT (0x0400)
#endif
#ifndef WINVER
#define WINVER (0x0400)
#endif
#if ((WINVER) >= 0x0400)
/* O.k. */
#else
#undef  WINVER
#define WINVER (0x0400)
#endif
#else
/* W*nd*ws NT 4.0 or higher, W*nd*ws 95 or higher. */
#ifndef WINVER
#define WINVER (0x0400)
#endif
#if ((WINVER) >= 0x0400)
/* O.k. */
#else
#undef  WINVER
#define WINVER (0x0400)
#endif
#endif
#endif


/* Derive some more specials. */
#if (defined (__ix86__) && defined (__microsoft__) && defined (__windows__))
#if (defined (_M_IX86))
#if (_M_IX86 >= 300)
#ifndef __i386__
#define __i386__
#endif
#endif
#if (_M_IX86 >= 400)
#ifndef __i486__
#define __i486__
#endif
#endif
#if (_M_IX86 >= 500)
#ifndef __i586__
#define __i586__
#endif
#endif
#if (_M_IX86 >= 600)
#ifndef __i686__
#define __i686__
#endif
#endif
#endif
#endif


/* Make the `ix86' consistent. */
#undef  __intel__
#ifdef  __i686__
#ifndef __intel__
#define __intel__ (686)
#endif
#ifndef __i586__
#define __i586__
#endif
#endif
#ifdef  __i586__
#ifndef __intel__
#define __intel__ (586)
#endif
#ifndef __i486__
#define __i486__
#endif
#endif
#ifdef  __i486__
#ifndef __intel__
#define __intel__ (486)
#endif
#ifndef __i386__
#define __i386__
#endif
#endif
#ifdef  __i386__
#ifndef __intel__
#define __intel__ (386)
#endif
#endif


/* Ensure that the initial state is correct. */
#ifndef FMC_MACHINE_UNKNOWN
#define FMC_MACHINE_UNKNOWN
#endif
#ifdef  FMC_MACHINE_UNRECOGNIZABLE
#undef  FMC_MACHINE_UNKNOWN
#endif
#ifdef  FMC_MACHINE_ALPHA_DEC_OSF
#undef  FMC_MACHINE_ALPHA_DEC_OSF
#endif
#ifdef  FMC_MACHINE_ALPHA_MICROSOFT_WINDOWS
#undef  FMC_MACHINE_ALPHA_MICROSOFT_WINDOWS
#endif
#ifdef  FMC_MACHINE_ALPHA_WORLD_LINUX
#undef  FMC_MACHINE_ALPHA_WORLD_LINUX
#endif
#ifdef  FMC_MACHINE_CRAY_CRAY_UNICOS
#undef  FMC_MACHINE_CRAY_CRAY_UNICOS
#endif
#ifdef  FMC_MACHINE_CRAYMPP_CRAY_UNICOS
#undef  FMC_MACHINE_CRAYMPP_CRAY_UNICOS
#endif
#ifdef  FMC_MACHINE_FUJITSU_FUJITSU_UXP
#undef  FMC_MACHINE_FUJITSU_FUJITSU_UXP
#endif
#ifdef  FMC_MACHINE_HIPPC_HITACHI_HIUXMPP
#undef  FMC_MACHINE_HIPPC_HITACHI_HIUXMPP
#endif
#ifdef  FMC_MACHINE_HPPA_HP_HPUX
#undef  FMC_MACHINE_HPPA_HP_HPUX
#endif
#ifdef  FMC_MACHINE_I386_MICROSOFT_WINDOWS
#undef  FMC_MACHINE_I386_MICROSOFT_WINDOWS
#endif
#ifdef  FMC_MACHINE_I386_WORLD_LINUX
#undef  FMC_MACHINE_I386_WORLD_LINUX
#endif
#ifdef  FMC_MACHINE_MIPS_SGI_IRIX
#undef  FMC_MACHINE_MIPS_SGI_IRIX
#endif
#ifdef  FMC_MACHINE_POWER_IBM_AIX
#undef  FMC_MACHINE_POWER_IBM_AIX
#endif
#ifdef  FMC_MACHINE_SPARC_SUN_SOLARIS
#undef  FMC_MACHINE_SPARC_SUN_SOLARIS
#endif
#ifdef  FMC_MACHINE_SPARC_SUN_SUNOS
#undef  FMC_MACHINE_SPARC_SUN_SUNOS
#endif
#ifdef  FMC_MACHINE_SX_NEC_SUPERUX
#undef  FMC_MACHINE_SX_NEC_SUPERUX
#endif


/* Try to recognize known machines. */
#ifdef  FMC_MACHINE_UNKNOWN
#if (defined (__alpha__) && defined (__osf__) && defined (__unix__))
#undef  FMC_MACHINE_UNKNOWN
#define FMC_MACHINE_ALPHA_DEC_OSF
#endif
#endif
#ifdef  FMC_MACHINE_UNKNOWN
#if (defined (__alpha__) && defined (__microsoft__) && defined (__windows__))
#undef  FMC_MACHINE_UNKNOWN
#define FMC_MACHINE_ALPHA_MICROSOFT_WINDOWS
#endif
#endif
#ifdef  FMC_MACHINE_UNKNOWN
#if (defined (__alpha__) && defined (__linux__))
#undef  FMC_MACHINE_UNKNOWN
#define FMC_MACHINE_ALPHA_WORLD_LINUX
#endif
#endif
#ifdef  FMC_MACHINE_UNKNOWN
#if (defined (__cray__) && defined (__unicos__))
#ifdef  FMC_MACHINE_UNKNOWN
#ifdef  __craympp__
#undef  FMC_MACHINE_UNKNOWN
#define FMC_MACHINE_CRAYMPP_CRAY_UNICOS
#endif
#endif
#ifdef  FMC_MACHINE_UNKNOWN
#undef  FMC_MACHINE_UNKNOWN
#define FMC_MACHINE_CRAY_CRAY_UNICOS
#endif
#endif
#endif
#ifdef  FMC_MACHINE_UNKNOWN
#if (defined (__uxp__) && defined (__uxppx__) && defined (__uxpv__))
#undef  FMC_MACHINE_UNKNOWN
#define FMC_MACHINE_FUJITSU_FUJITSU_UXP
#endif
#endif
#ifdef  FMC_MACHINE_UNKNOWN
#if (defined (__sr8000__) && defined (__hiuxmpp__) && defined (__unix__))
#undef  FMC_MACHINE_UNKNOWN
#define FMC_MACHINE_HIPPC_HITACHI_HIUXMPP
#endif
#endif
#ifdef  FMC_MACHINE_UNKNOWN
#if (defined (__hppa__) && defined (__hpux__))
#undef  FMC_MACHINE_UNKNOWN
#define FMC_MACHINE_HPPA_HP_HPUX
#endif
#endif
#ifdef  FMC_MACHINE_UNKNOWN
#if (defined (__i386__) && defined (__microsoft__) && defined (__windows__))
#undef  FMC_MACHINE_UNKNOWN
#define FMC_MACHINE_I386_MICROSOFT_WINDOWS
#endif
#endif
#ifdef  FMC_MACHINE_UNKNOWN
#if (defined (__i386__) && defined (__linux__))
#undef  FMC_MACHINE_UNKNOWN
#define FMC_MACHINE_I386_WORLD_LINUX
#endif
#endif
#ifdef  FMC_MACHINE_UNKNOWN
#if (defined (__mips__) && defined (__sgi__) && defined (__unix__))
#undef  FMC_MACHINE_UNKNOWN
#define FMC_MACHINE_MIPS_SGI_IRIX
#endif
#endif
#ifdef  FMC_MACHINE_UNKNOWN
#if (defined (__ibm__) && defined (__aix__))
#undef  FMC_MACHINE_UNKNOWN
#define FMC_MACHINE_POWER_IBM_AIX
#endif
#endif
#ifdef  FMC_MACHINE_UNKNOWN
#if (defined (__sparc__) && defined (__sun__) && defined (__unix__))
#ifdef  FMC_MACHINE_UNKNOWN
#ifdef  __svr4__
#undef  FMC_MACHINE_UNKNOWN
#define FMC_MACHINE_SPARC_SUN_SOLARIS
#endif
#endif
#ifdef  FMC_MACHINE_UNKNOWN
#ifdef  __bsd__
#undef  FMC_MACHINE_UNKNOWN
#define FMC_MACHINE_SPARC_SUN_SUNOS
#endif
#endif
#ifdef  FMC_MACHINE_UNKNOWN
#undef  FMC_MACHINE_UNKNOWN
#define FMC_MACHINE_SPARC_SUN_SOLARIS
#endif
#endif
#endif
#ifdef  FMC_MACHINE_UNKNOWN
#if (defined (__sx__) && defined (__unix__))
#undef  FMC_MACHINE_UNKNOWN
#define FMC_MACHINE_SX_NEC_SUPERUX
#endif
#endif

