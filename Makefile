OCAMLMAKEFILE = /usr/share/ocamlmakefile/OCamlMakefile
RESULT = smithy
SOURCES = CamlExt.ml \
	  Colors.ml \
	  DialSlider.ml \
	  ItemStrings.ml \
	  OrthoDrawer.ml \
	  Resources.ml \
	  MapTypes.ml \
	  GenerateDialog.ml \
	  DrawModeSettings.ml \
	  Preferences.ml \
	  MapFormat.ml \
	  GeomEdit.ml \
	  FileDialogs.ml \
	  MapDialogs.ml \
	  DrawModeWindows.ml \
	  DrawModeEvent.ml \
	  DrawMode.ml \
	  Smithy.ml
LIBS = lablgtk
INCDIRS = +lablgtk2
OCAMLFLAGS = -g -w sy
OCAMLLDFLAGS = -g

include $(OCAMLMAKEFILE)

run : byte-code
	./$(RESULT) $(shell cat args)
