include Make.config

RESULT = smithy
SOURCES = CamlExt.ml \
	  Colors.ml \
	  DialSlider.ml \
	  ItemStrings.ml \
	  OrthoDrawer.ml \
	  Resources.ml \
	  MapTypes.ml \
	  MapFormat.ml \
	  GenerateDialog.ml \
	  DrawModeSettings.ml \
	  Toolbar.ml \
	  GeomEdit.ml \
	  FileDialogs.ml \
	  MapDialogs.ml \
	  DrawModeWindows.ml \
	  Preferences.ml \
	  DrawModeEvent.ml \
	  DrawMode.ml \
	  Smithy.ml
LIBS = lablgtk
INCDIRS = +lablgtk2
OCAMLFLAGS = -w s -warn-error A
OCAMLBCFLAGS = -g
OCAMLBLDFLAGS = -g

include $(OCAMLMAKEFILE)

run : byte-code
	./$(RESULT) $(shell cat args)
