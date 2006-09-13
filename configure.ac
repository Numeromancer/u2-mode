#  Autoconf script for U2-Mode                       -*- Autoconf -*-
# To rebuild the `configure' script from this, execute the command
#     autoconf
# in the directory containing this script.
#
#  Copyright (C) 2003, 2004, 2005, 2006 Free Software Foundation, Inc.
#
#  This file is part of GNU Emacs.
#
#  GNU Emacs is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2, or (at your option)
#  any later version.
#
#  GNU Emacs is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with GNU Emacs; see the file COPYING.  If not, write to the
#  Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
#  Boston, MA 02110-1301, USA.

# Shamelessly copied and (slightly) revised from Tramp, as is much of 
# the stuff in U2-mode
### Code:

AC_PREREQ([2.50])

# Initialize configure script.  This is the only point U2-mode revision
# needs to be set.
AC_INIT([U2-mode], [0.98], [numeromancer@users.sourceforge.net])
AM_INIT_AUTOMAKE([U2-mode],[0.98])
AC_MSG_NOTICE([$PACKAGE_STRING])

# The last version of U2-mode that was merged into the Emacs CVS
# repository.  This will usually be the Tramp pre-version minus one.
U2_MODE_EMACS_MERGE_VERSION=0.98
AC_SUBST(U2_MODE_EMACS_MERGE_VERSION)

# Check suitable make version.
AC_CHECK_PROGS([MAKE], [gmake make], [no])
if test "${MAKE}" = no; then
   AC_MSG_ERROR([make not found])
fi

AC_MSG_CHECKING([for reasonable ${MAKE} version])
if test "$(${MAKE} -s -f test.makefile 123)" = 123; then
   AC_MSG_RESULT([ok])
else
   AC_MSG_RESULT([nok])
   AC_MSG_ERROR([${MAKE} is not up-to-date, you need at least GNU make 3.76])
fi

AC_PROG_MAKE_SET

# Find the Emacs program & flavor to be used.  Check version.
AC_EMACS_INFO

AM_PATH_LISPDIR

# Check suitable makeinfo version.
AC_CHECK_PROG([MAKEINFO], [makeinfo], [makeinfo], [no])
if test "${MAKEINFO}" = no; then
   AC_MSG_ERROR([makeinfo not found])
fi

AC_MSG_CHECKING([for reasonable makeinfo version])
${MAKEINFO} texi/test.texi -o /dev/null 2>/dev/null >/dev/null
if test $? -eq 0; then
   AC_MSG_RESULT([ok])
else
   AC_MSG_RESULT([nok])
   AC_MSG_ERROR([makeinfo is not up-to-date, you need at least Texinfo 4.6])
fi

# Check suitable texi2dvi version.  Here we produce a warning only,
# because this shouldn't prevent Tramp exploitation.
AC_CHECK_PROG([TEXI2DVI], [texi2dvi], [texi2dvi], [no])
if test "${TEXI2DVI}" = no; then
   AC_MSG_WARN([texi2dvi not found])
else
   AC_MSG_CHECKING([for reasonable texi2dvi version])
   ${TEXI2DVI} -b -q -c -o /dev/null texi/test.texi 2>/dev/null >/dev/null
   if test $? -eq 0; then
      AC_MSG_RESULT([ok])
   else
      AC_MSG_RESULT([nok])
      AC_MSG_WARN([texi2dvi doesn't work, check \$TEXINPUTS])
      TEXI2DVI="no"
   fi
   test -e test.log && rm -f test.log
fi

# Check existence of install-info.  Again, we produce a warning only,
# because this shouldn't prevent Tramp exploitation.
AC_CHECK_PROG([INSTALL_INFO], [install-info], [install-info])
if test -z "${INSTALL_INFO}"; then
   AC_PATH_PROG([INSTALL_INFO], [install-info], [no],
		[$PATH:/usr/sbin:/sbin:/usr/local/bin])
fi
if test "${INSTALL_INFO}" = no; then
   AC_MSG_WARN([install-info not found])
fi

# Checks whether Tramp installation chapter is needed.
# Necessary for maintainers only.
AC_EMACS_INSTALL

# Check target installation directories.
AC_PATH_LISPDIR
AC_PATH_INFODIR

AC_SUBST(prefix)
AC_SUBST(prefix_default)
AC_SUBST(datadir)
AC_SUBST(datadir_default)
AC_SUBST(lispdir)
AC_SUBST(lispdir_default)
AC_SUBST(infodir)
AC_SUBST(infodir_default)

# Check whether ftp-upload is available.  Necessary for maintainers only;
# therefore there is no error or warning.
AC_CHECK_PROG([FTP_UPLOAD], [ftp-upload], [yes], [no])

# Define files to be cleaned up.  Applicable in all directories.
U2_MODE_CLEAN_FILES=".\\\\\#* \\\\\#* .*~ *~"
U2_MODE_CLEAN_FILES="$U2_MODE_CLEAN_FILES *.aux *.cp *.cps *.diff *.dvi *.elc"
U2_MODE_CLEAN_FILES="$U2_MODE_CLEAN_FILES *.fn *.fns *.html *.info *.ky *.log"
U2_MODE_CLEAN_FILES="$U2_MODE_CLEAN_FILES *.pg *.tmp *.toc *.tp *.vr *.vrs"
AC_SUBST([U2_MODE_CLEAN_FILES])
# *.elc files must be removed always.  Otherwise, there could be problems
# if they are created with the wrong Emacs flavor.
rm -f *.elc

# Produce the Makefiles.
U2_MODE_CONFIG_FILES="Makefile"
U2_MODE_CONFIG_FILES="$U2_MODE_CONFIG_FILES u2ver.el"
U2_MODE_CONFIG_FILES="$U2_MODE_CONFIG_FILES u2_modever.texi"
AC_SUBST(U2_MODE_CONFIG_FILES)
AC_CONFIG_FILES([$U2_MODE_CONFIG_FILES])

AC_OUTPUT
