OCAMLMAKEFILE = /usr/share/ocamlmakefile/OCamlMakefile
RESULT = smithy
SOURCES = CamlExt.ml \
	  Resources.ml \
	  ItemStrings.ml \
	  Colors.ml \
	  MapTypes.ml \
	  MapFormat.ml \
	  OrthoDrawer.ml \
	  DialSlider.ml \
	  GenerateDialog.ml \
	  GeomEdit.ml \
	  FileDialogs.ml \
	  DrawModeWindows.ml \
	  MapDialogs.ml \
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
