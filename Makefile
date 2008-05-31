OCAMLMAKEFILE = /usr/share/ocamlmakefile/OCamlMakefile
RESULT = smithy
SOURCES = CamlExt.ml \
	  Resources.ml \
	  Colors.ml \
	  OrthoDrawer.ml \
	  MapTypes.ml \
	  MapFormat.ml \
	  GeomEdit.ml \
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
