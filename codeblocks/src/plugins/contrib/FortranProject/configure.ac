AC_PREREQ(2.50)
AC_INIT(FortranProject plugin, 1.8, darmar.lt@gmail.com)

AM_INIT_AUTOMAKE([subdir-objects foreign])

AC_PROG_LIBTOOL

#LT_PREREQ([2.2])
#LT_INIT([dlopen])

#AM_CONFIG_HEADER([config.h])
AC_CONFIG_FILES([Makefile
		images/Makefile
		images/16x16/Makefile
		images/20x20/Makefile
		images/24x24/Makefile
		images/28x28/Makefile
		images/32x32/Makefile
		images/40x40/Makefile
		images/48x48/Makefile
		images/56x56/Makefile
		images/64x64/Makefile
		images/svg/Makefile
		images/fortranproject/Makefile
		images/fortranproject/16x16/Makefile
		images/fortranproject/20x20/Makefile
		images/fortranproject/24x24/Makefile
		images/fortranproject/28x28/Makefile
		images/fortranproject/32x32/Makefile
		images/fortranproject/40x40/Makefile
		images/fortranproject/48x48/Makefile
		images/fortranproject/56x56/Makefile
		images/fortranproject/64x64/Makefile
		images/fortranproject/svg/Makefile])
AC_CONFIG_MACRO_DIR([m4])

# Checks for programs.
#AC_PROG_CC
AC_PROG_CXX

# Checks for c++11 supporting compiler
m4_include([ax_cxx_compile_stdcxx.m4])
AX_CXX_COMPILE_STDCXX(11, noext)

# Checks for libraries.

AM_OPTIONS_WXCONFIG
AM_PATH_WXCONFIG(2.8.0, wxWin=1)
	if test "$wxWin" != 1; then
		AC_MSG_ERROR([
			wxWidgets must be installed on your system.

			Please check that wx-config is in path, the directory
			where wxWidgets libraries are installed (returned by
			'wx-config --libs' or 'wx-config --static --libs' command)
			is in LD_LIBRARY_PATH or equivalent variable and
			wxWindows version is 2.8.0 or above.
			])
	fi

PKG_CHECK_MODULES([CODEBLOCKS], [codeblocks])

AC_SUBST([CODEBLOCKS_CFLAGS])
AC_SUBST([CODEBLOCKS_LIBS])

#cb_plugin_datadir="`$PKG_CONFIG codeblocks --variable plugindir`"
cb_plugin_datadir="`$PKG_CONFIG codeblocks --variable sharedir`/codeblocks/"
cb_plugin_libdir="`$PKG_CONFIG codeblocks --variable libdir`/codeblocks/plugins/"

AC_SUBST([cb_plugin_datadir])
AC_SUBST([cb_plugin_libdir])

CPPFLAGS="$CPPFLAGS $CODEBLOCKS_CFLAGS -ansi "
CXXFLAGS="$CXXFLAGS -std=c++11"
LIBS="$LIBS $CODEBLOCKS_LIBS"


# Checks for header files.
# Checks for typedefs, structures, and compiler characteristics.
# Checks for library functions.

AC_SUBST(CPPFLAGS)
AC_SUBST(CXXFLAGS)
AC_SUBST(CFLAGS)
AC_SUBST(LIBS)

AC_OUTPUT
