define(GUILE_XOSD_CONFIGURE_COPYRIGHT, [[
Copyright (C) 2016 Alex Kost <alezost@gmail.com>

This file is part of Guile-XOSD.

Guile-XOSD is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Guile-XOSD is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Guile-XOSD.  If not, see <http://www.gnu.org/licenses/>.
]])

AC_INIT([Guile-XOSD], [0.1], [alezost@gmail.com], [guile-xosd],
        [https://github.com/alezost/guile-xosd])

AC_COPYRIGHT(GUILE_XOSD_CONFIGURE_COPYRIGHT)

AC_CONFIG_AUX_DIR([build-aux])

AM_INIT_AUTOMAKE([foreign subdir-objects])

AC_PROG_CC
AC_PROG_LIBTOOL

dnl Check for libxosd.  Unfortunately, libxosd does not provide
dnl pkgconfig file, but it has 'xosd-config' script that can be used to
dnl define cflags and libs.

dnl PKG_CHECK_MODULES([LIBXOSD], [libxosd >= 2.2])
AC_PATH_PROG([XOSD_CONFIG], [xosd-config])
if test "x$XOSD_CONFIG" = "x"; then
  AC_MSG_ERROR(['xosd-config' script not found; please install libxosd.])
else
  LIBXOSD_CFLAGS="`$XOSD_CONFIG --cflags`"
  LIBXOSD_LIBS="`$XOSD_CONFIG --libs`"
  AC_SUBST([LIBXOSD_CFLAGS])
  AC_SUBST([LIBXOSD_LIBS])
fi

AC_CONFIG_FILES([Makefile
                 src/Makefile])

AC_OUTPUT