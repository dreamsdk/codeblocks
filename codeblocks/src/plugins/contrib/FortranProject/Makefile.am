SUBDIRS = images

ACLOCAL_AMFLAGS = --install -I m4

AM_CPPFLAGS = $(WX_CXXFLAGS) \
		-I$(top_srcdir)/src/include \
		-I$(top_srcdir)/src/sdk/wxscintilla/include

cb_plugin_lib_LTLIBRARIES = libFortranProject.la

libFortranProject_la_LDFLAGS = -shared -no-undefined -avoid-version

libFortranProject_la_LIBADD = $(WX_LIBS) 

libFortranProject_la_SOURCES = adddirparserthread.cpp \
					autoinsert.cpp \
					bindto.cpp \
					bindtonewtype.cpp \
					bufferparserthread.cpp \
					calledbydict.cpp \
					calltree.cpp \
					calltreeview.cpp \
					ccsmartfilter.cpp \
					changecase.cpp \
					constrhighlighter.cpp \
					docblock.cpp \
					farrays.cpp \
					fconstruct.cpp \
					finfowindow.cpp \
					formatindent.cpp \
					formatindentdlg.cpp \
					fortranfileext.cpp \
					fortranproject.cpp \
					fpimagelist.cpp \
					fpoptionsdlg.cpp \
					fpoptionsprjdlg.cpp \
					includedb.cpp \
					indentestimator.cpp \
					jumptracker.cpp \
					keywordsparserf.cpp \
					lineaddress.cpp \
					makefiledlg.cpp \
					makefilegen.cpp \
					moduletokenf.cpp \
					nativeparserf.cpp \
					parserf.cpp \
					parserthreadf.cpp \
					preprocfunction.cpp \
					projectdependencies.cpp \
					tab2space.cpp \
					textcutter.cpp \
					tokenf.cpp \
					tokenizerf.cpp \
					tokenizerpp.cpp \
					usetokenf.cpp \
					workspacebrowserbuilder.cpp \
					workspacebrowserf.cpp \
					workspaceparserthread.cpp

noinst_HEADERS = adddirparserthread.h \
					autoinsert.h \
					bindto.h \
					bindtonewtype.h \
					bufferparserthread.h \
					calledbydict.h \
					calltree.h \
					calltreeview.h \
					ccsmartfilter.h \
					changecase.h \
					constrhighlighter.h \
					docblock.h \
					farrays.h \
					fconstruct.h \
					finfowindow.h \
					formatindent.h \
					formatindentdlg.h \
					fortranfileext.h \
					fortranproject.h \
					fpimagelist.h \
					fpoptionsdlg.h \
					fpoptionsprjdlg.h \
					includedb.h \
					indentestimator.h \
					jumptracker.h \
					keywordsparserf.h \
					lineaddress.h \
					makefiledlg.h \
					makefilegen.h \
					moduletokenf.h \
					nativeparserf.h \
					parserf.h \
					parserthreadf.h \
					preprocfunction.h \
					projectdependencies.h \
					tab2space.h \
					textcutter.h \
					tokenf.h \
					tokenizerf.h \
					tokenizerpp.h \
					usetokenf.h \
					workspacebrowserbuilder.h \
					workspacebrowserf.h \
					workspaceparserthread.h

EXTRA_DIST = $(srcdir)/*.cbp \
		wxsmith/Bindto.wxs \
		wxsmith/BindNewType.wxs \
		wxsmith/FormatIndentDlg.wxs \
		wxsmith/MakefileDlg.wxs \
		manifest.xml \
		resources/calltreeview.xrc \
		resources/changecase.xrc \
		resources/fortranprojecttoolbar.xrc \
		resources/project_settings_fortran.xrc \
		resources/settings.xrc \
		resources/tab2space.xrc \
		resources/workspacebrowserf.xrc \
		update* \
		images/16x16/*.png \
		images/20x20/*.png \
		images/24x24/*.png \
		images/28x28/*.png \
		images/32x32/*.png \
		images/40x40/*.png \
		images/48x48/*.png \
		images/56x56/*.png \
		images/64x64/*.png \
		images/svg/*.svg \
		images/fortranproject/*.py \
		images/fortranproject/*.f90 \
		images/fortranproject/16x16/*.png \
		images/fortranproject/20x20/*.png \
		images/fortranproject/24x24/*.png \
		images/fortranproject/28x28/*.png \
		images/fortranproject/32x32/*.png \
		images/fortranproject/40x40/*.png \
		images/fortranproject/48x48/*.png \
		images/fortranproject/56x56/*.png \
		images/fortranproject/64x64/*.png \
		images/fortranproject/svg/*.svg

cb_plugin_data_DATA = FortranProject.zip

CLEANFILES = $(cb_plugin_data_DATA)

uninstall-hook:
	-rmdir ${cb_plugin_datadir}images/fortranproject

FortranProject.zip: manifest.xml
	PWD=`pwd` cd $(srcdir) && zip -j9 $(PWD)/FortranProject.zip manifest.xml resources/*.xrc > /dev/null
	PWD=`pwd` cd $(srcdir) && zip -R9 $(PWD)/FortranProject.zip images/16x16/*.png images/20x20/*.png images/24x24/*.png images/28x28/*.png images/32x32/*.png images/40x40/*.png images/48x48/*.png images/56x56/*.png images/64x64/*.png images/svg/*.svg images/fortranproject/*.f90 images/fortranproject/16x16/*.png images/fortranproject/20x20/*.png images/fortranproject/24x24/*.png images/fortranproject/28x28/*.png images/fortranproject/32x32/*.png images/fortranproject/40x40/*.png images/fortranproject/48x48/*.png images/fortranproject/56x56/*.png images/fortranproject/64x64/*.png images/fortranproject/svg/*.svg > /dev/null
