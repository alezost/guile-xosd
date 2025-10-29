define(GUILE_XOSD_CONFIGURE_COPYRIGHT, [[
Copyright (C) 2016-2025 Alex Kost <alezost@gmail.com>

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

AC_INIT([Guile-XOSD], [0.2.2], [alezost@gmail.com], [guile-xosd],
        [https://github.com/alezost/guile-xosd])

AC_COPYRIGHT(GUILE_XOSD_CONFIGURE_COPYRIGHT)

AC_CONFIG_AUX_DIR([build-aux])

AM_INIT_AUTOMAKE([foreign subdir-objects])

dnl ----------------------------------------------------------------
dnl Checking for Guile

GUILE_PKG([3.0 2.2 2.0])
GUILE_PROGS([2.0.2])

AC_ARG_WITH([guile-site-dir],
  [AS_HELP_STRING([--with-guile-site-dir],
    [installation directory for Guile (*.scm) files])],
  [guilemoduledir="$withval"],
  [guilemoduledir="$datarootdir/guile/site/$GUILE_EFFECTIVE_VERSION"])
AC_SUBST([guilemoduledir])
AC_MSG_CHECKING([for the Guile module directory])
AC_MSG_RESULT([$guilemoduledir])

AC_ARG_WITH([guile-site-ccache-dir],
  [AS_HELP_STRING([--with-guile-site-ccache-dir],
    [installation directory for Guile compiled (*.go) files])],
  [guileccachedir="$withval"],
  [guileccachedir="$libdir/guile/$GUILE_EFFECTIVE_VERSION/site-ccache"])
AC_SUBST([guileccachedir])
AC_MSG_CHECKING([for the Guile ccache directory])
AC_MSG_RESULT([$guileccachedir])

AC_CONFIG_FILES([Makefile
                 doc/Makefile
                 modules/Makefile
                 modules/xosd/config.scm
                 pre-inst-env.el])
AC_CONFIG_FILES([pre-inst-env], [chmod +x pre-inst-env])
AC_CONFIG_FILES([examples/message.scm], [chmod +x examples/message.scm])
AC_CONFIG_FILES([examples/clock.scm], [chmod +x examples/clock.scm])
AC_CONFIG_FILES([examples/sound.scm], [chmod +x examples/sound.scm])

AC_OUTPUT
