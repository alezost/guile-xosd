# Makefile.am --- GNU Automake file to build libguile-xosd

# Copyright (C) 2016 Alex Kost <alezost@gmail.com>

# This file is part of Guile-XOSD.

# Guile-XOSD is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# Guile-XOSD is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Guile-XOSD.  If not, see <http://www.gnu.org/licenses/>.

lib_LTLIBRARIES = libguile-xosd.la

libguile_xosd_la_SOURCES = libguile-xosd.c
libguile_xosd_la_CFLAGS = $(GUILE_CFLAGS) $(LIBXOSD_CFLAGS)
libguile_xosd_la_LIBADD = $(GUILE_LIBS) $(LIBXOSD_LIBS)

C_FILES = $(libguile_xosd_la_SOURCES)
X_FILES = $(C_FILES:%.c=%.x)

SNARF_OPTS = $(libguile_xosd_la_CFLAGS)

$(X_FILES): %.x: %.c
	guile-snarf $(SNARF_OPTS) $< > $@

BUILT_SOURCES = $(X_FILES)
CLEANFILES = $(X_FILES)

# Makefile.am ends here
